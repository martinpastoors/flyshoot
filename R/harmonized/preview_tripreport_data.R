# ============================================================================
# preview_tripreport_data.R
#
# Shows a summary of all datasets that would be used for a tripreport,
# without actually rendering the report. Run this to diagnose empty figures.
# ============================================================================

library(tidyverse)
library(lubridate)
library(arrow)
library(jsonlite)
library(glue)

# ── Parameters — adjust these ─────────────────────────────────────────────────
setvessel   <- "SCH99"
startdate   <- Sys.Date() - days(14)
enddate     <- Sys.Date()
lang        <- "nl"

# ── Setup ─────────────────────────────────────────────────────────────────────
project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop("Set FLYSHOOT_PROJECT in .Renviron")
p <- function(...) file.path(project_root, ...)
config <- jsonlite::read_json(p("config.json"))
withr::with_dir(project_root, source(p("R", "harmonized", "02_storage_functions.R")))

cat(strrep("=", 60), "\n")
cat(glue("DATA PREVIEW: {setvessel}\n"))
cat(glue("Period: {format(startdate,'%d/%m/%Y')} – {format(enddate,'%d/%m/%Y')}\n"))
cat(strrep("=", 60), "\n\n")

# ── Helper ────────────────────────────────────────────────────────────────────
preview <- function(label, df, id_cols = NULL) {
  cat(strrep("-", 60), "\n")
  cat(glue("{label}: {nrow(df)} rows × {ncol(df)} cols\n"))
  if (nrow(df) == 0) {
    cat("  ⚠ EMPTY\n\n")
    return(invisible(df))
  }
  if (!is.null(id_cols)) {
    for (col in id_cols) {
      if (col %in% names(df)) {
        vals <- paste(sort(unique(df[[col]])), collapse = ", ")
        cat(glue("  {col}: {vals}\n"))
      }
    }
  }
  cat(glue("  date range: {format(min(df$date,na.rm=TRUE))} – {format(max(df$date,na.rm=TRUE))}\n"))
  cat("  head(5):\n")
  print(head(df, 5), width = 120)
  cat("\n")
  invisible(df)
}

# ── Load raw datasets ─────────────────────────────────────────────────────────
cat("Loading raw parquet data...\n\n")

elog_raw      <- tryCatch(load_flyshoot_data("elog"),          error = function(e) { cat("  ✗ elog:",      e$message, "\n"); tibble() })
elog_trek_raw <- tryCatch(load_flyshoot_data("elog_trek"),     error = function(e) { cat("  ✗ elog_trek:", e$message, "\n"); tibble() })
haul_raw      <- tryCatch(load_flyshoot_data("haul"),          error = function(e) { cat("  ✗ haul:",      e$message, "\n"); tibble() })
kisten_raw    <- tryCatch(load_flyshoot_data("kisten"),        error = function(e) { cat("  ✗ kisten:",    e$message, "\n"); tibble() })
trip_raw      <- tryCatch(load_flyshoot_data("trip"),          error = function(e) { cat("  ✗ trip:",      e$message, "\n"); tibble() })
vm_raw        <- tryCatch(load_flyshoot_data("vessel_movement"), error = function(e) { cat("  ✗ vm:",       e$message, "\n"); tibble() })

# ── Resolve settrip ───────────────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("STEP 1: Resolve trip IDs\n")
cat(strrep("=", 60), "\n\n")

settrip <- NULL

if (nrow(elog_raw) > 0) {
  settrip <- elog_raw %>%
    dplyr::filter(vessel %in% setvessel, date >= startdate, date <= enddate) %>%
    dplyr::pull(trip_id) %>% unique() %>% sort()
  cat(glue("From elog:      {paste(settrip, collapse=', ')}\n"))
}

if (is.null(settrip) || length(settrip) == 0) {
  if (nrow(haul_raw) > 0) {
    settrip <- haul_raw %>%
      dplyr::filter(vessel %in% setvessel, date >= startdate, date <= enddate) %>%
      dplyr::pull(trip_id) %>% unique() %>% sort()
    cat(glue("From haul:      {paste(settrip, collapse=', ')}\n"))
  }
}

if (is.null(settrip) || length(settrip) == 0) {
  cat("⚠ No trip IDs found in date range!\n\n")
} else {
  cat(glue("\nsettrip = {paste(settrip, collapse=', ')}\n\n"))
}

# ── Filter each dataset ───────────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("STEP 2: Filter datasets to vessel + trip\n")
cat(strrep("=", 60), "\n\n")

e <- if (nrow(elog_raw) > 0)
  dplyr::filter(elog_raw, vessel %in% setvessel, trip_id %in% settrip) else tibble()

et_by_trip <- if (nrow(elog_trek_raw) > 0)
  dplyr::filter(elog_trek_raw, vessel %in% setvessel, trip_id %in% settrip) else tibble()

et_by_date <- if (nrow(elog_trek_raw) > 0)
  dplyr::filter(elog_trek_raw, vessel %in% setvessel, date >= startdate, date <= enddate) else tibble()

h <- if (nrow(haul_raw) > 0)
  dplyr::filter(haul_raw, vessel %in% setvessel, trip_id %in% settrip) else tibble()

m <- if (nrow(kisten_raw) > 0)
  dplyr::filter(kisten_raw, vessel %in% setvessel, trip_id %in% settrip) else tibble()

tt <- if (nrow(trip_raw) > 0)
  dplyr::filter(trip_raw, vessel %in% setvessel, trip_id %in% settrip) else tibble()

vm <- if (nrow(vm_raw) > 0)
  dplyr::filter(vm_raw, vessel %in% setvessel, trip_id %in% settrip) else tibble()

# ── Preview each ─────────────────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("STEP 3: Dataset previews\n")
cat(strrep("=", 60), "\n\n")

preview("elog (e)",        e,           id_cols = c("trip_id", "species_code"))
preview("elog_trek by trip_id", et_by_trip,  id_cols = c("trip_id", "species_code"))

if (nrow(et_by_trip) == 0 && nrow(et_by_date) > 0) {
  cat("  ℹ elog_trek has rows by DATE but not by trip_id — trip_id MISMATCH\n")
  cat(glue("  trip_ids in elog_trek for date range: {paste(unique(et_by_date$trip_id), collapse=', ')}\n"))
  cat(glue("  trip_ids in elog for date range:      {paste(settrip, collapse=', ')}\n\n"))
  preview("elog_trek by date (fallback)", et_by_date, id_cols = c("trip_id", "species_code"))
}

preview("haul (h)",        h,   id_cols = c("trip_id"))
preview("kisten (m)",      m,   id_cols = c("trip_id"))
preview("trip (tt)",       tt,  id_cols = c("trip_id"))
preview("vessel_movement", vm,  id_cols = c("trip_id", "event_type"))

# ── Position check for maps ───────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("STEP 4: Position data check (needed for Figure 1)\n")
cat(strrep("=", 60), "\n\n")

check_positions <- function(label, df) {
  if (nrow(df) == 0) { cat(glue("  {label}: EMPTY\n")); return() }
  has_lon <- "shoot_lon" %in% names(df)
  has_lat <- "shoot_lat" %in% names(df)
  if (!has_lon || !has_lat) { cat(glue("  {label}: no shoot_lon/shoot_lat columns\n")); return() }
  n_pos <- sum(!is.na(df$shoot_lon) & !is.na(df$shoot_lat) &
                 df$shoot_lon != 0 & df$shoot_lat != 0)
  cat(glue("  {label}: {n_pos} / {nrow(df)} rows have valid positions\n"))
  if (n_pos > 0) {
    cat(glue("    lon range: {round(min(df$shoot_lon,na.rm=TRUE),3)} – {round(max(df$shoot_lon,na.rm=TRUE),3)}\n"))
    cat(glue("    lat range: {round(min(df$shoot_lat,na.rm=TRUE),3)} – {round(max(df$shoot_lat,na.rm=TRUE),3)}\n"))
  }
}

check_positions("elog (e)",             e)
check_positions("elog_trek by trip_id", et_by_trip)
check_positions("elog_trek by date",    et_by_date)
check_positions("haul (h)",             h)

if (nrow(vm) > 0) {
  n_vm <- sum(!is.na(vm$lat) & !is.na(vm$lon))
  cat(glue("  vessel_movement: {n_vm} / {nrow(vm)} rows have valid lat/lon\n"))
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("PREVIEW COMPLETE\n")
cat(strrep("=", 60), "\n")





