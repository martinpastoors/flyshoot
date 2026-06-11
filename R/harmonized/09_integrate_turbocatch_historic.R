# =============================================================================
# 09_integrate_turbocatch_historic.R
# Parse all TurboCatch ERS XML files and write to SEPARATE turbocatch parquets
# (same table names as main pipeline: haul, elog, elog_trek, trip).
#
# Output:
#   C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\turbocatch\
#     haul.parquet       <- per_haul regime (2026+): one row per haul
#     elog_trek.parquet  <- per_haul regime (2026+): one row per species per haul
#     elog.parquet       <- daily regime (2019-2025): one row per species per day
#     trip.parquet       <- one row per trip
#
# Inspect these independently before merging into the main pipeline.
# When ready to merge, call integrate_turbocatch_to_pipeline().
#
# Usage:
#   source("parse_ers.R")                          # in R/turbocatch/
#   source("09_integrate_turbocatch_historic.R")   # in R/turbocatch/
#
#   process_turbocatch_to_parquet(dry_run = TRUE)  # check counts first
#   process_turbocatch_to_parquet()                # write parquets
#   integrate_turbocatch_to_pipeline()             # merge when satisfied
# =============================================================================

library(arrow)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)
library(here)

# Load pipeline functions if not already available
if (!exists("load_flyshoot_data")) {
  source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
  source(file.path(here(), "R/harmonized", "02_storage_functions.R"))
}
if (!exists("parse_ers_folder")) {
  source(file.path(here(), "R/turbocatch", "parse_ers.R"))
}

# Default output folder for turbocatch parquets
TURBOCATCH_PARQUET_DIR <-
  "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch"

# =============================================================================
# process_turbocatch_to_parquet()
# Parse XML files for both vessels and write to separate turbocatch parquets.
# =============================================================================

#' Parse TurboCatch ERS XML files and write to separate parquet files.
#'
#' @param vessel_folders Named character vector (name = vessel ID, value = path).
#'                       Defaults to CC545762 and CC622598 under config$tripdata.
#' @param output_dir     Where to write parquets (default: TURBOCATCH_PARQUET_DIR).
#' @param dry_run        If TRUE, parse and report but do NOT write files.
process_turbocatch_to_parquet <- function(
    vessel_folders = NULL,
    output_dir     = TURBOCATCH_PARQUET_DIR,
    dry_run        = FALSE
) {

  # Default vessel folders
  if (is.null(vessel_folders)) {
    base <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata"
    vessel_folders <- c(
      CC545762 = file.path(base, "CC545762"),
      CC622598 = file.path(base, "CC622598")
    )
  }

  # Validate folders
  missing <- vessel_folders[!dir.exists(vessel_folders)]
  if (length(missing) > 0) {
    message("  These vessel folders do not exist and will be skipped:")
    for (nm in names(missing)) message(glue("    {nm}: {missing[nm]}"))
    vessel_folders <- vessel_folders[dir.exists(vessel_folders)]
  }
  if (length(vessel_folders) == 0) stop("No valid vessel folders found.")

  message(strrep("=", 60))
  message("TURBOCATCH: PARSING ERS XML FILES")
  message(strrep("=", 60))
  if (dry_run) message("  *** DRY RUN — no files will be written ***")
  message(glue("  Vessels : {paste(names(vessel_folders), collapse = ', ')}"))
  message(glue("  Output  : {output_dir}"))

  # ── Step 1: Parse all XML files ───────────────────────────────────────────
  message("\n── Step 1: Parsing ─────────────────────────────────────────")

  all_haul      <- list()
  all_elog_trek <- list()
  all_elog      <- list()
  all_trip      <- list()

  for (vessel_id in names(vessel_folders)) {
    folder    <- vessel_folders[[vessel_id]]
    xml_files <- list.files(folder, pattern = "\\.xml$",
                            full.names = TRUE, recursive = TRUE)
    message(glue("\n  {vessel_id}: {length(xml_files)} XML files"))

    if (length(xml_files) == 0) { message("    No XML files — skipping"); next }

    ers <- parse_ers_folder(folder, recursive = TRUE)
    if (is.null(ers)) next

    # Route by FAR regime (detected per trip in parse_ers_folder):
    #   daily    (2019-2025) avg n_shots > 3 -> elog
    #   per_haul (2026+)     avg n_shots <= 3 -> haul + elog_trek
    haul_data      <- ers_to_haul(ers)
    elog_trek_data <- ers_to_elog_trek(ers)
    elog_data      <- ers_to_elog(ers)
    trip_data      <- ers_to_trip(ers)

    if (!is.null(ers$trips) && "far_regime" %in% names(ers$trips)) {
      regimes <- table(ers$trips$far_regime)
      message(glue("    Regime  : {paste(names(regimes), regimes, sep='=', collapse=', ')}"))
    }
    message(glue("    Trips   : {nrow(trip_data)}"))
    message(glue("    Hauls   : {nrow(haul_data)} rows (per_haul)"))
    message(glue("    elog_trek: {nrow(elog_trek_data)} rows (per_haul)"))
    message(glue("    elog    : {nrow(elog_data)} rows (daily)"))

    if (nrow(haul_data)      > 0) all_haul[[vessel_id]]      <- haul_data
    if (nrow(elog_trek_data) > 0) all_elog_trek[[vessel_id]] <- elog_trek_data
    if (nrow(elog_data)      > 0) all_elog[[vessel_id]]      <- elog_data
    if (nrow(trip_data)      > 0) all_trip[[vessel_id]]      <- trip_data
  }

  new_haul      <- bind_rows(all_haul)
  new_elog_trek <- bind_rows(all_elog_trek)
  new_elog      <- bind_rows(all_elog)
  new_trip      <- bind_rows(all_trip)

  message(glue("\n  TOTAL — haul: {nrow(new_haul)}, elog_trek: {nrow(new_elog_trek)},",
               " elog: {nrow(new_elog)}, trip: {nrow(new_trip)}"))

  if (dry_run) {
    message("\n  *** Dry run complete. Returning data invisibly. ***")
    return(invisible(list(haul = new_haul, elog_trek = new_elog_trek,
                          elog = new_elog, trip = new_trip)))
  }

  # ── Step 2: Write parquets ────────────────────────────────────────────────
  message(glue("\n── Step 2: Writing parquets to {output_dir}"))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(glue("  Created: {output_dir}"))
  }

  write_tc <- function(df, name) {
    if (nrow(df) == 0) { message(glue("  {name}: no data")); return(invisible(NULL)) }
    df_sorted <- switch(name,
      haul      = arrange(df, vessel, trip_id, haul_id),
      elog_trek = arrange(df, vessel, trip_id, haul_id, species_code),
      elog      = arrange(df, vessel, trip_id, date, species_code),
      trip      = arrange(df, vessel, trip_id),
      df
    )
    path <- file.path(output_dir, paste0(name, ".parquet"))
    write_parquet(df_sorted, path)
    message(glue("  ✓ {name}.parquet  ({nrow(df_sorted)} rows, ",
                 "{round(file.size(path)/1024, 1)} KB)"))
    invisible(path)
  }

  write_tc(new_haul,      "haul")
  write_tc(new_elog_trek, "elog_trek")
  write_tc(new_elog,      "elog")
  write_tc(new_trip,      "trip")

  message(strrep("=", 60))
  message("DONE. Inspect parquets before merging into main pipeline.")
  message(glue("  Location: {output_dir}"))
  message("  Call integrate_turbocatch_to_pipeline() when ready to merge.")
  message(strrep("=", 60))

  invisible(list(haul = new_haul, elog_trek = new_elog_trek,
                 elog = new_elog, trip = new_trip))
}

# =============================================================================
# integrate_turbocatch_to_pipeline()
# Merge the turbocatch parquets into the main flyshoot pipeline.
# Run process_turbocatch_to_parquet() and inspect first.
# =============================================================================

#' Merge turbocatch parquets into the main flyshoot pipeline parquets.
#'
#' @param tc_dir  Turbocatch parquet folder (default: TURBOCATCH_PARQUET_DIR)
integrate_turbocatch_to_pipeline <- function(tc_dir = TURBOCATCH_PARQUET_DIR) {

  message(strrep("=", 60))
  message("MERGING TURBOCATCH INTO MAIN PIPELINE")
  message(strrep("=", 60))

  load_tc <- function(name) {
    path <- file.path(tc_dir, paste0(name, ".parquet"))
    if (!file.exists(path)) {
      message(glue("  {name}.parquet not found — skipping"))
      return(tibble())
    }
    df <- read_parquet(path)
    message(glue("  Loaded {name}: {nrow(df)} rows"))
    df
  }

  tc_haul      <- load_tc("haul")
  tc_elog_trek <- load_tc("elog_trek")
  tc_elog      <- load_tc("elog")
  tc_trip      <- load_tc("trip")

  tc_vessels <- unique(c(tc_haul$vessel, tc_elog$vessel,
                         tc_elog_trek$vessel, tc_trip$vessel))
  message(glue("  Vessels: {paste(tc_vessels, collapse = ', ')}"))

  merge_table <- function(new_data, type) {
    if (nrow(new_data) == 0) {
      message(glue("  {type}: nothing to merge"))
      return(invisible(NULL))
    }
    existing <- tryCatch(load_flyshoot_data(type), error = function(e) tibble())

    # Remove prior turbocatch rows for these vessels
    if (nrow(existing) > 0 && "data_source" %in% names(existing)) {
      n_before <- nrow(existing)
      existing <- existing %>%
        filter(!(vessel %in% tc_vessels & data_source == "turbocatch"))
      removed <- n_before - nrow(existing)
      if (removed > 0)
        message(glue("  {type}: removed {removed} prior turbocatch rows"))
    }

    combined <- reconcile_types(existing, new_data) %>% bind_rows(new_data)
    quietly(save_flyshoot_data(combined, type))
    message(glue("  ✓ {type}: {nrow(existing)} existing + {nrow(new_data)} turbocatch",
                 " = {nrow(combined)} total"))
  }

  message("\n── Merging ─────────────────────────────────────────────────")
  merge_table(tc_haul,      "haul")
  merge_table(tc_elog_trek, "elog_trek")
  merge_table(tc_elog,      "elog")
  merge_table(tc_trip,      "trip")

  message(strrep("=", 60))
  message("MERGE COMPLETE")
  message(strrep("=", 60))
  invisible(NULL)
}
