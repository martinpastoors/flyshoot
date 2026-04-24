# ==========================================================================================================
# Batch Render FLYSHOOT Tripreports — multiple vessels
# Updated: March 2026
#
# Sits in the same R/ folder as 00_setup.R – 03_main_workflow.R.
# Loads elog once from config$raw_data_path via load_flyshoot_data().
#
# Output: reports/YYYY/WWW/FLYSHOOT_VESSEL_YYYYWWW_Ntrips.docx
# ==========================================================================================================

library(rmarkdown)
library(tidyverse)
library(lubridate)
library(arrow)
library(jsonlite)
library(glue)
library(callr)    # isolates each render in a fresh R process

# ==========================================================================================================
# CONFIGURATION
# ==========================================================================================================

vessels         <- c("SL9")
# vessels         <- c("SCH135", "SCH65")
lang            <- "nl"     # "nl" or "en"
by_week         <- TRUE     # TRUE = one report per ISO week (all trips that week)
                            # FALSE = one report per individual trip
startdate       <- dmy("11/03/2026")
enddate         <- dmy("16/03/2026")

# Auto-compute last week instead:
# enddate   <- Sys.Date()
# startdate <- enddate - days(10)

open_reports    <- FALSE
quiet_mode      <- TRUE  # set TRUE for production

# ==========================================================================================================
# PROJECT ROOT + CONFIG  (p() must be defined before anything else uses it)
# ==========================================================================================================

project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop(
  "FLYSHOOT_PROJECT is not set.\n",
  "  Add FLYSHOOT_PROJECT=C:/GIT/flyshoot to .Renviron and restart R."
)
p <- function(...) file.path(project_root, ...)

if (!file.exists(p("config.json"))) stop("config.json not found at: ", p("config.json"))
config <- jsonlite::read_json(p("config.json"))
source(p("R", "harmonized", "02_storage_functions.R"))

# Rmd location — output dir is set per-vessel inside render_vessel_report()
rmd_file <- p("R", "harmonized", "FLYSHOOT_tripreport_harmonized.Rmd")

# Hard stop if Rmd is outdated — prevents wrong reports being generated
rmd_lines <- readLines(rmd_file)
if (any(grepl("^params:", rmd_lines)) || !any(grepl("flyshoot.vessel", rmd_lines))) {
  stop(
    "\n\n  ✗ Rmd file is OUTDATED at:\n  ", rmd_file,
    "\n\n  Expected: no 'params:' in YAML and 'flyshoot.vessel' in setup chunk.",
    "\n  Copy the latest downloaded version to that path and re-run.\n"
  )
}
cat("  ✓ Rmd version OK\n")

# ==========================================================================================================
# LOAD ELOG ONCE — shared across all vessels
# ==========================================================================================================

cat(strrep("=", 50), "\n")
cat("BATCH RENDER FLYSHOOT TRIPREPORTS\n")
cat(strrep("=", 50), "\n")
cat(glue("Date range: {format(startdate,'%d/%m/%Y')} – {format(enddate,'%d/%m/%Y')}\n"))
cat(glue("Vessels:    {paste(vessels, collapse=', ')}\n\n"))

cat("Loading elog...\n")

elog <- tryCatch(
  load_flyshoot_data("elog"),
  error = function(e) stop(glue("Could not load elog: {e$message}"))
)

required_cols <- c("vessel", "trip_id", "date")
missing_cols  <- setdiff(required_cols, names(elog))
if (length(missing_cols) > 0) {
  stop(glue("Elog missing columns: {paste(missing_cols, collapse=', ')}"))
}

cat(glue("Elog: {nrow(elog)} rows | vessels: {paste(sort(unique(elog$vessel)), collapse=', ')}\n\n"))

# ==========================================================================================================
# RENDER ONE VESSEL
# ==========================================================================================================

render_vessel_report <- function(vessel_name, start_date, end_date) {

  cat(glue("── {vessel_name} "))

  # When by_week=TRUE, expand date range to full ISO week
  if (by_week) {
    mid_date   <- start_date + as.integer((end_date - start_date) / 2)
    wk         <- lubridate::isoweek(mid_date)
    yr         <- lubridate::isoyear(mid_date)
    # Monday of that ISO week
    jan4       <- as.Date(paste0(yr, "-01-04"))
    wk1_mon    <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
    start_date <- wk1_mon + (wk - 1L) * 7L
    end_date   <- start_date + 6L
  }

  trip_info <- elog %>%
    dplyr::filter(vessel == vessel_name,
                  date >= start_date,
                  date <= end_date) %>%
    dplyr::summarise(
      trips     = paste(sort(unique(trip_id)), collapse = "_"),
      ntrips    = dplyr::n_distinct(trip_id),
      year      = lubridate::year(min(date, na.rm = TRUE)),
      week      = lubridate::isoweek(min(date, na.rm = TRUE)),
      startdate = min(date, na.rm = TRUE),
      enddate   = max(date, na.rm = TRUE)
    )

  if (trip_info$ntrips == 0) {
    cat("⊘ geen data — overgeslagen\n")
    return(tibble(vessel = vessel_name, status = "skipped",
                  reason = "No trips found", path = NA_character_,
                  trips = NA_character_, elapsed = NA_real_, size_kb = NA_real_))
  }

  cat(glue("({trip_info$ntrips} trip(s), week {trip_info$week})\n"))

  output_filename <- glue(
    "{vessel_name}_{trip_info$year}W{sprintf('%02d', trip_info$week)}_",
    "{trip_info$ntrips}trip{ifelse(trip_info$ntrips > 1, 's', '')}.docx"
  )

  # Output goes into <tripdata>/<vessel>/ — vessel-based, no year/week subfolders
  output_dir <- file.path(config$tripdata, vessel_name)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_path <- file.path(output_dir, output_filename)

  # Check file is not open in Word; if not locked, delete it so render overwrites cleanly
  if (file.exists(output_path)) {
    is_locked <- tryCatch(
      { con <- file(output_path, open = "ab"); close(con); FALSE },
      error = function(e) TRUE
    )
    if (is_locked) {
      cat(glue("   ✗ File open in Word — close it and re-run: {output_filename}\n\n"))
      return(tibble(vessel = vessel_name, status = "error", path = NA_character_,
                    trips = trip_info$trips, elapsed = NA_real_, size_kb = NA_real_,
                    reason = "Output file is open in Word"))
    }
    # File exists but not locked — delete it so rmarkdown::render() overwrites cleanly
    file.remove(output_path)
  }

  t0 <- Sys.time()

  options(
    flyshoot.vessel      = vessel_name,
    flyshoot.startdate   = start_date,
    flyshoot.enddate     = end_date,
    flyshoot.lang        = lang,
    flyshoot.max_species = 12
  )

  render_error <- NULL
  tryCatch(
    rmarkdown::render(input = rmd_file, output_file = output_path,
                      quiet = quiet_mode),
    error = function(e) {
      render_error <<- e$message
      if (!file.exists(output_path))
        cat(paste0("   ✗ Render failed: ", e$message, "\n"))
      else
        cat(paste0("   ⚠ Render warning (file created): ", e$message, "\n"))
    }
  )

  elapsed   <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  file_size <- if (file.exists(output_path))
    round(file.info(output_path)$size / 1024, 1) else NA_real_
  status    <- if (file.exists(output_path)) "success" else "error"

  cat(glue("   {ifelse(status=='success','✓','✗')} {output_filename}",
           " ({elapsed}s, {file_size} KB)\n"))

  if (status == "success" && open_reports && .Platform$OS.type == "windows")
    shell.exec(output_path)

  tibble(vessel = vessel_name, status = status, path = output_path,
         trips = trip_info$trips, elapsed = elapsed, size_kb = file_size,
         reason = render_error)
}

# ==========================================================================================================
# PROCESS ALL VESSELS
# ==========================================================================================================

cat(strrep("-", 50), "\n")

t_total <- Sys.time()
results <- {
  res_list <- list()
  for (v in vessels) {
    cat(paste0("\n", strrep("=",50), "\nProcessing: ", v, "\n", strrep("=",50), "\n"))
    res_list[[v]] <- tryCatch(
      render_vessel_report(v, startdate, enddate),
      error = function(e) {
        cat(paste0("\n✗ OUTER ERROR for ", v, ": ", e$message, "\n"))
        tibble(vessel = v, status = "error", path = NA_character_,
               trips = NA_character_, elapsed = NA_real_,
               size_kb = NA_real_, reason = e$message)
      }
    )
  }
  dplyr::bind_rows(res_list)
}
elapsed_total <- round(as.numeric(difftime(Sys.time(), t_total, units = "secs")), 1)

# ==========================================================================================================
# SUMMARY
# ==========================================================================================================

cat("\n", strrep("=", 50), "\n")
cat("SAMENVATTING\n")
cat(strrep("=", 50), "\n")
cat(glue("Verwerkt:      {nrow(results)} vessel(s)\n"))
cat(glue("  ✓ Succesvol: {sum(results$status=='success')}\n"))
cat(glue("  ⊘ Geen data: {sum(results$status=='skipped')}\n"))
cat(glue("  ✗ Fouten:    {sum(results$status=='error')}\n"))
cat(glue("Totale tijd:   {elapsed_total}s\n\n"))

results %>%
  dplyr::mutate(file = dplyr::if_else(is.na(path), "-", basename(path))) %>%
  dplyr::select(vessel, status, trips, file,
                dplyr::any_of(c("elapsed", "size_kb", "reason"))) %>%
  knitr::kable(format = "simple") %>%
  print()

if (any(results$status == "error")) {
  cat("\nFouten:\n")
  results %>%
    dplyr::filter(status == "error") %>%
    purrr::pwalk(~cat(glue("  {..1}: {..7}\n")))
}

invisible(results)
