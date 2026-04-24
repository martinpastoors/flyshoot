# ==========================================================================================================
# Render FLYSHOOT Tripreport — single vessel
# Updated: March 2026
#
# Output: <tripdata>/<vessel>/FLYSHOOT_<VESSEL>_<YYYY>W<WW>_<N>trip(s).docx
#   where <tripdata> comes from config$tripdata (set in 00_setup.R)
# ==========================================================================================================

library(rmarkdown)
library(tidyverse)
library(lubridate)
library(arrow)
library(jsonlite)
library(glue)

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

# ==========================================================================================================
# CONFIGURATION
# ==========================================================================================================

setvessel <- "SCH135"
lang      <- "nl"   # "nl" or "en"
startdate <- dmy("01/12/2025")
enddate   <- dmy("04/12/2025")

rmd_file <- p("R", "harmonized", "FLYSHOOT_tripreport_harmonized.Rmd")

# Output root: <tripdata>/<vessel>/  — tripdata path comes from config
# e.g. C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/SCH135/
output_base_dir <- file.path(config$tripdata, setvessel)

# ==========================================================================================================
# RESOLVE TRIP DETAILS FROM ELOG (for filename)
# ==========================================================================================================

cat("Resolving trip details from elog...\n")

elog <- tryCatch(
  load_flyshoot_data("elog", vessel_ids = setvessel),
  error = function(e) stop(glue("Could not load elog: {e$message}"))
)

required_cols <- c("vessel", "trip_id", "date")
missing_cols  <- setdiff(required_cols, names(elog))
if (length(missing_cols) > 0) {
  stop(glue("Elog is missing expected columns: {paste(missing_cols, collapse=', ')}"))
}

trip_info <- elog %>%
  dplyr::filter(date >= startdate, date <= enddate) %>%
  dplyr::summarise(
    trips     = paste(sort(unique(trip_id)), collapse = "_"),
    ntrips    = dplyr::n_distinct(trip_id),
    year      = lubridate::year(min(date, na.rm = TRUE)),
    week      = lubridate::isoweek(min(date, na.rm = TRUE)),
    startdate = min(date, na.rm = TRUE),
    enddate   = max(date, na.rm = TRUE)
  )

if (trip_info$ntrips == 0) {
  stop(glue("No trips found for vessel {setvessel} between ",
            "{format(startdate,'%d/%m/%Y')} and {format(enddate,'%d/%m/%Y')}"))
}

cat(glue("\nVessel:  {setvessel}\n"))
cat(glue("Period:  {format(trip_info$startdate,'%d/%m/%Y')} – {format(trip_info$enddate,'%d/%m/%Y')}\n"))
cat(glue("Year:    {trip_info$year}, ISO week: {trip_info$week}\n"))
cat(glue("Trip(s): {trip_info$trips}\n\n"))

# ==========================================================================================================
# DYNAMIC FILENAME + DIRECTORY
# ==========================================================================================================

output_filename <- glue(
  "{setvessel}_{trip_info$year}W{sprintf('%02d', trip_info$week)}_",
  "{trip_info$ntrips}trip{ifelse(trip_info$ntrips > 1, 's', '')}.docx"
)

# Output goes directly into the vessel folder — no year/week subfolders
output_dir <- output_base_dir

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(glue("Created: {output_dir}\n"))
}

output_path <- file.path(output_dir, output_filename)
cat(glue("Output:  {output_filename}\n\n"))

# ==========================================================================================================
# CHECK OUTPUT FILE IS NOT OPEN IN WORD
# ==========================================================================================================

if (file.exists(output_path)) {
  is_locked <- tryCatch(
    { con <- file(output_path, open = "ab"); close(con); FALSE },
    error = function(e) TRUE
  )
  if (is_locked) stop(
    glue("Output file is open in Word — please close it first:\n  {output_path}")
  )
}

# ==========================================================================================================
# RENDER
# ==========================================================================================================

cat(glue("Rendering {setvessel} ({trip_info$trips})...\n-----------------------------------\n"))

start_time <- Sys.time()

tryCatch({

  options(
    flyshoot.vessel      = setvessel,
    flyshoot.startdate   = startdate,
    flyshoot.enddate     = enddate,
    flyshoot.lang        = lang,
    flyshoot.max_species = 12
  )

  rmarkdown::render(
    input       = rmd_file,
    output_file = output_path,
  )

  elapsed   <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
  file_size <- round(file.info(output_path)$size / 1024, 1)

  cat(glue("-----------------------------------\n✓ Done in {elapsed}s ({file_size} KB)\n"))
  cat(glue("  {output_path}\n"))

  if (.Platform$OS.type == "windows") shell.exec(output_path)

}, error = function(e) {
  cat(glue("-----------------------------------\n✗ ERROR: {e$message}\n"))
})
