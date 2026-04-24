# ============================================================================
# debug_tripreport.R
# Run this script to set up the exact same environment as the Rmd,
# then step through chunks manually to find errors.
#
# Usage:
#   1. Source this file (or run line by line)
#   2. Then open FLYSHOOT_tripreport_harmonized.Rmd and run chunks manually
# ============================================================================

library(tidyverse)
library(lubridate)
library(arrow)
library(jsonlite)
library(glue)
library(sf)
library(ggplot2)

# ── 1. Set the parameters that the Rmd expects ────────────────────────────────
setvessel   <- "SCH135"
startdate   <- Sys.Date() - days(7)
enddate     <- Sys.Date()
lang        <- "nl"
max_species <- 12

cat("Parameters set:\n")
cat("  setvessel:  ", setvessel, "\n")
cat("  startdate:  ", format(startdate, "%d/%m/%Y"), "\n")
cat("  enddate:    ", format(enddate,   "%d/%m/%Y"), "\n")
cat("  lang:       ", lang, "\n")
cat("  max_species:", max_species, "\n\n")

# Also set via options() so the Rmd getOption() calls work
options(
  flyshoot.vessel      = setvessel,
  flyshoot.startdate   = startdate,
  flyshoot.enddate     = enddate,
  flyshoot.lang        = lang,
  flyshoot.max_species = max_species
)

# ── 2. Load project config ────────────────────────────────────────────────────
project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop("Set FLYSHOOT_PROJECT in .Renviron")
p <- function(...) file.path(project_root, ...)

config <- jsonlite::read_json(p("config.json"))
cat("Config loaded from:", p("config.json"), "\n")

# ── 3. Source storage functions ───────────────────────────────────────────────
withr::with_dir(project_root, {
  source(p("R", "harmonized", "02_storage_functions.R"))
})
cat("Storage functions loaded\n\n")

# ── 4. Now open the Rmd and run chunks manually ───────────────────────────────
cat("Ready. Now open FLYSHOOT_tripreport_harmonized.Rmd in RStudio\n")
cat("and run the 'setup' chunk first (Ctrl+Shift+Enter on that chunk).\n")
cat("Then step through each subsequent chunk to find the error.\n\n")
cat("Key variables to check after each chunk:\n")
cat("  after setup:     ls()  —  check setvessel, e, et, h, m exist\n")
cat("  after setup:     cat(setvessel)  —  should be SCH135\n")
cat("  in fig_tripmap:  cat(names(et))  —  check shoot_lon/shoot_lat exist\n")
cat("  in fig_tripmap:  cat(names(h))   —  check haul_id exists\n")

# ── 5. Diagnose the et zero-rows problem ─────────────────────────────────────
cat("\n=== Diagnosing elog_trek filter ===\n")

# Load raw data
elog_trek_raw <- load_flyshoot_data("elog_trek")
elog_raw      <- load_flyshoot_data("elog")

cat("elog_trek rows:", nrow(elog_trek_raw), "\n")
cat("elog_trek vessels:", paste(sort(unique(elog_trek_raw$vessel)), collapse=", "), "\n")

et_vessel <- elog_trek_raw %>% dplyr::filter(vessel %in% setvessel)
cat("\nelog_trek rows for", setvessel, ":", nrow(et_vessel), "\n")

if (nrow(et_vessel) > 0) {
  cat("Date range in elog_trek for", setvessel, ":\n")
  cat("  min date:", format(min(et_vessel$date, na.rm=TRUE)), "\n")
  cat("  max date:", format(max(et_vessel$date, na.rm=TRUE)), "\n")
  cat("  trip_ids:", paste(sort(unique(et_vessel$trip_id)), collapse=", "), "\n")
}

cat("\nLooking for trips between", format(startdate), "and", format(enddate), "\n")
et_date <- et_vessel %>% dplyr::filter(date >= startdate, date <= enddate)
cat("Rows in date range:", nrow(et_date), "\n")
if (nrow(et_date) > 0) {
  cat("trip_ids in date range:", paste(sort(unique(et_date$trip_id)), collapse=", "), "\n")
}

# Check what settrip would be
elog_for_trips <- elog_raw %>%
  dplyr::filter(vessel %in% setvessel, date >= startdate, date <= enddate)
cat("\nelog rows for", setvessel, "in date range:", nrow(elog_for_trips), "\n")
if (nrow(elog_for_trips) > 0) {
  cat("trip_ids from elog:", paste(sort(unique(elog_for_trips$trip_id)), collapse=", "), "\n")
}
