# =============================================================================
# 09_integrate_turbocatch_historic.R
# One-time (and repeatable) script to parse all TurboCatch ERS XML files
# from the historic vessel folders and merge them into the main flyshoot
# parquet files (haul, elog_trek, trip).
#
# Run order:
#   source("parse_ers.R")
#   source("04_turbocatch_adapter.R")
#   source("09_integrate_turbocatch_historic.R")
#   integrate_turbocatch_historic()
#
# This script is safe to re-run: it removes existing turbocatch records for
# the affected vessels before appending, so you never get duplicates.
# =============================================================================

library(arrow)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)
library(here)

# Source pipeline functions if not already loaded
if (!exists("load_flyshoot_data")) {
  source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
  source(file.path(here(), "R/harmonized", "02_storage_functions.R"))
}
if (!exists("parse_ers_folder")) {
  source(file.path(here(), "R/turbocatch", "parse_ers.R"))
}
if (!exists("ers_to_haul")) {
  source(file.path(here(), "R/turbocatch", "04_turbocatch_adapter.R"))
}

# ── TurboCatch → elog_trek mapper ─────────────────────────────────────────────
# FAR catch maps to elog_trek (haul-level catch by species), NOT kisten.
# The elog_trek schema: vessel, trip_id, haul_id, date, weighing_time,
#   species_code, presentation, size_category, weight_kg, box_count,
#   conversion_factor, shoot_lat, shoot_lon, ices_rect, gear_type,
#   mesh_size_mm, data_source

ers_to_elog_trek <- function(ers_data) {
  if (is.null(ers_data$far) || nrow(ers_data$far) == 0) return(tibble())

  # Re-apply haul_id numbering (same logic as ers_to_catch)
  far_msgs <- ers_data$far %>%
    filter(!is_correction) %>%
    distinct(trip_id, source_file, haul_date, haul_time, lat, lon) %>%
    arrange(trip_id, haul_date, haul_time) %>%
    group_by(trip_id) %>%
    mutate(haul_id = row_number()) %>%
    ungroup()

  ers_data$far %>%
    filter(!is_correction) %>%
    left_join(far_msgs, by = c("trip_id", "source_file",
                                "haul_date", "haul_time", "lat", "lon")) %>%
    transmute(
      vessel            = vessel_id,
      trip_id           = trip_id,
      trip_nr           = trip_id,             # no separate trip_nr for ERS
      haul_id           = haul_id,
      date              = as.Date(haul_date),
      weighing_time     = as.POSIXct(paste(haul_date, haul_time),
                                      format = "%Y-%m-%d %H:%M", tz = "UTC"),
      species_code      = species,
      presentation      = presentation,
      size_category     = "legal",             # ERS does not distinguish undersized
      weight_kg         = weight_kg,
      box_count         = n_boxes,
      conversion_factor = conv_factor,
      shoot_lat         = lat,
      shoot_lon         = lon,
      ices_rect         = ices_rect,
      gear_type         = gear_type,
      mesh_size_mm      = as.integer(mesh_mm),
      data_source       = "turbocatch"
    ) %>%
    filter(!is.na(species_code), !is.na(weight_kg))
}

# ── Main integration function ─────────────────────────────────────────────────

#' Parse all TurboCatch XML files for one or more vessel folders and merge
#' into the main flyshoot parquet files.
#'
#' @param vessel_folders  Named character vector: names = vessel IDs (e.g. "CC545762"),
#'                        values = full path to the folder containing XML files.
#'                        Defaults to the two known vessels under config$tripdata.
#' @param dry_run         If TRUE, parse and convert but do NOT write to parquet.
#'                        Useful for checking counts before committing.
#' @param remove_existing If TRUE (default), remove any existing turbocatch rows
#'                        for these vessels from parquet before appending.
#'                        Set FALSE only if you are certain there are no existing
#'                        turbocatch records (e.g. very first run).
integrate_turbocatch_historic <- function(
    vessel_folders  = NULL,
    dry_run         = FALSE,
    remove_existing = TRUE
) {

  # Default vessel folders from config
  if (is.null(vessel_folders)) {
    vessel_folders <- c(
      CC545762 = file.path(config$tripdata, "CC545762"),
      CC622598 = file.path(config$tripdata, "CC622598")
    )
  }

  # Validate
  missing <- vessel_folders[!dir.exists(vessel_folders)]
  if (length(missing) > 0) {
    message("⚠ These vessel folders do not exist and will be skipped:")
    for (nm in names(missing)) message(glue("    {nm}: {missing[nm]}"))
    vessel_folders <- vessel_folders[dir.exists(vessel_folders)]
  }
  if (length(vessel_folders) == 0) stop("No valid vessel folders found.")

  message(strrep("=", 60))
  message("TURBOCATCH HISTORIC INTEGRATION")
  message(strrep("=", 60))
  if (dry_run) message("  *** DRY RUN — no data will be written ***")
  message(glue("  Vessels: {paste(names(vessel_folders), collapse=', ')}"))

  # ── Step 1: Parse all XML files ───────────────────────────────────────────
  message("\n── Step 1: Parsing ERS XML files ──────────────────────────")

  all_haul      <- list()
  all_elog_trek <- list()
  all_trip      <- list()

  for (vessel_id in names(vessel_folders)) {
    folder <- vessel_folders[[vessel_id]]

    xml_files <- list.files(folder, pattern = "\\.xml$",
                            full.names = TRUE, recursive = TRUE)
    message(glue("\n  {vessel_id}: {length(xml_files)} XML files in {folder}"))

    if (length(xml_files) == 0) {
      message("    No XML files found — skipping")
      next
    }

    ers <- parse_ers_folder(folder, recursive = TRUE)
    if (is.null(ers)) next

    # Convert to pipeline schema
    haul_data      <- ers_to_haul(ers)
    elog_trek_data <- ers_to_elog_trek(ers)
    trip_data      <- ers_to_trip(ers)

    message(glue("    Trips:     {nrow(trip_data)}"))
    message(glue("    Hauls:     {nrow(haul_data)}"))
    message(glue("    Catch rows:{nrow(elog_trek_data)}"))

    if (nrow(haul_data)      > 0) all_haul[[vessel_id]]      <- haul_data
    if (nrow(elog_trek_data) > 0) all_elog_trek[[vessel_id]] <- elog_trek_data
    if (nrow(trip_data)      > 0) all_trip[[vessel_id]]      <- trip_data
  }

  if (length(all_haul) == 0 && length(all_elog_trek) == 0) {
    message("\nNo data parsed — nothing to integrate.")
    return(invisible(NULL))
  }

  new_haul      <- bind_rows(all_haul)
  new_elog_trek <- bind_rows(all_elog_trek)
  new_trip      <- bind_rows(all_trip)

  message(glue("\n  Total new rows — haul: {nrow(new_haul)}, ",
               "elog_trek: {nrow(new_elog_trek)}, trip: {nrow(new_trip)}"))

  if (dry_run) {
    message("\n  *** Dry run complete — inspect the tables below ***")
    message("  Return value: list($haul, $elog_trek, $trip)")
    return(invisible(list(haul = new_haul,
                          elog_trek = new_elog_trek,
                          trip = new_trip)))
  }

  # ── Step 2: Load existing parquet data ───────────────────────────────────
  message("\n── Step 2: Loading existing parquet data ───────────────────")

  tc_vessels <- unique(c(new_haul$vessel, new_elog_trek$vessel, new_trip$vessel))

  haul_existing      <- tryCatch(load_flyshoot_data("haul"),
                                  error = function(e) tibble())
  elog_trek_existing <- tryCatch(load_flyshoot_data("elog_trek"),
                                  error = function(e) tibble())
  trip_existing      <- tryCatch(load_flyshoot_data("trip"),
                                  error = function(e) tibble())

  message(glue("  Existing haul rows:      {nrow(haul_existing)}"))
  message(glue("  Existing elog_trek rows: {nrow(elog_trek_existing)}"))
  message(glue("  Existing trip rows:      {nrow(trip_existing)}"))

  # ── Step 3: Remove existing turbocatch rows for these vessels ─────────────
  if (remove_existing) {
    message("\n── Step 3: Removing existing TurboCatch rows ───────────────")

    remove_tc_rows <- function(existing, vessels) {
      if (nrow(existing) == 0) return(existing)
      has_source <- "data_source" %in% names(existing)
      if (has_source) {
        n_before <- nrow(existing)
        cleaned  <- existing %>%
          filter(!(vessel %in% vessels & data_source == "turbocatch"))
        message(glue("    Removed {n_before - nrow(cleaned)} existing turbocatch rows"))
        cleaned
      } else {
        # No data_source column yet — remove by vessel only if first integration
        message("    No data_source column — keeping all existing rows")
        existing
      }
    }

    haul_existing      <- remove_tc_rows(haul_existing,      tc_vessels)
    elog_trek_existing <- remove_tc_rows(elog_trek_existing,  tc_vessels)
    trip_existing      <- remove_tc_rows(trip_existing,       tc_vessels)
  }

  # ── Step 4: Merge and save ────────────────────────────────────────────────
  message("\n── Step 4: Merging and saving ───────────────────────────────")

  save_merged <- function(existing, new_data, type) {
    if (nrow(new_data) == 0) {
      message(glue("  {type}: no new data"))
      return(invisible(NULL))
    }
    combined <- reconcile_types(existing, new_data) %>%
      bind_rows(new_data)

    # Sort appropriately per type
    combined <- switch(type,
      haul      = arrange(combined, vessel, trip_id, haul_id),
      elog_trek = arrange(combined, vessel, trip_id, haul_id,
                          species_code, across(any_of("weighing_time"))),
      trip      = arrange(combined, vessel, trip_id),
      combined
    )

    date_col <- if (type == "trip") "departure_date" else "date"
    dr <- if (date_col %in% names(combined)) {
      c(as.character(min(combined[[date_col]], na.rm = TRUE)),
        as.character(max(combined[[date_col]], na.rm = TRUE)))
    } else NULL

    quietly(save_flyshoot_data(combined, type, date_range = dr))
    message(glue("  ✓ {type}: {nrow(existing)} existing + {nrow(new_data)} new",
                 " = {nrow(combined)} total rows saved"))
  }

  save_merged(haul_existing,      new_haul,      "haul")
  save_merged(elog_trek_existing,  new_elog_trek,  "elog_trek")
  save_merged(trip_existing,       new_trip,       "trip")

  message(glue("\n", strrep("=", 60)))
  message("INTEGRATION COMPLETE")
  message(strrep("=", 60))

  invisible(list(haul = new_haul, elog_trek = new_elog_trek, trip = new_trip))
}

# =============================================================================
# USAGE
# =============================================================================
# source(file.path(here(), "R/turbocatch/parse_ers.R"))
# source(file.path(here(), "R/turbocatch/04_turbocatch_adapter.R"))
# source(file.path(here(), "R/turbocatch/09_integrate_turbocatch_historic.R"))
#
# # Dry run first — check counts without writing anything
# result <- integrate_turbocatch_historic(dry_run = TRUE)
# View(result$haul)
# View(result$elog_trek)
# View(result$trip)
#
# # When happy, run for real
# integrate_turbocatch_historic()
#
# # Or specify custom paths:
# integrate_turbocatch_historic(
#   vessel_folders = c(
#     CC545762 = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC545762",
#     CC622598 = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC622598"
#   )
# )
# =============================================================================
