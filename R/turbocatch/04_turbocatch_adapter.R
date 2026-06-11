# =============================================================================
# 04_turbocatch_adapter.R
# Bridge between parse_ers.R (TurboCatch/French ERS XML) and the
# flyshoot parquet pipeline (01_flyshoot_functions.R / 02_storage_functions.R)
#
# What this file does:
#   1. Reads a folder of anonymized ERS XML files for one vessel/trip
#   2. Converts the parsed output into the same standardized column layout
#      used by the PEFA/kisten pipeline (haul, catch, trip tables)
#   3. Saves to parquet via the existing save_flyshoot_data() functions
#   4. Registers the new source type "turbocatch" in detect_data_source_type()
#
# Dependencies:
#   source("parse_ers.R")              # ERS XML parser
#   source("01_flyshoot_functions.R")  # column schemas + storage helpers
#   source("02_storage_functions.R")   # save_flyshoot_data()
#
# Usage:
#   source("04_turbocatch_adapter.R")
#   process_turbocatch_trip("path/to/xml/folder/VesselA/trip_20260102")
#   # or batch-process a whole vessel folder:
#   process_turbocatch_vessel("path/to/xml/folder/VesselA")
# =============================================================================

library(dplyr)
library(purrr)
library(lubridate)
library(glue)

# ── column mapping: ERS → pipeline standard ──────────────────────────────────
#
# Haul (one row per FAR haul)
#   vessel            ← vessel_id (from LOG XR)
#   trip_id           ← trip_id   (from ELOG TN)
#   haul_id           ← sequential within trip
#   date              ← haul_date
#   shoot_lat         ← lat  (FAR position = shooting position for seine)
#   shoot_lon         ← lon
#   shoot_time        ← haul_date + haul_time (POSIXct)
#   haul_lat          ← NA  (ERS does not record hauling position separately)
#   haul_lon          ← NA
#   haul_time         ← NA
#   fishing_time_hours← duration_min / 60
#   gear_type         ← gear_type (SSC = Scottish seine / flyshoot)
#   mesh_size_mm      ← mesh_mm
#   water_depth       ← NA (not in ERS)
#   ices_rect         ← ices_rect
#   fao_area          ← fao_area
#   eez               ← eez
#   n_shots           ← n_shots
#   data_source       ← "turbocatch"
#
# Catch (one row per species per haul — from FAR only)
#   vessel            ← vessel_id
#   trip_id           ← trip_id
#   haul_id           ← sequential within trip (same numbering as haul table)
#   date              ← haul_date
#   species_code      ← species  (FAO 3-alpha, already the right format)
#   weight_kg         ← weight_kg
#   presentation      ← presentation
#   freshness         ← freshness
#   conversion_factor ← conv_factor
#   n_boxes           ← n_boxes
#   ices_rect         ← ices_rect
#   gear_type         ← gear_type (SSC etc.)
#   measure_method    ← measure  (WGH / EST)
#   data_source       ← "turbocatch"
#
# Trip (one row per trip — from DEP + RTP + FAR)
#   vessel            ← vessel_id
#   trip_id           ← trip_id
#   departure_date    ← dep_date
#   departure_port    ← dep_port
#   arrival_date      ← rtp_date
#   arrival_port      ← rtp_port
#   gear_type         ← from DEP GEA
#   far_kg_total      ← sum of FAR weights
#   n_hauls           ← distinct FAR messages
#   fishing_areas     ← ICES rects fished
#   data_source       ← "turbocatch"

# ── haul converter ────────────────────────────────────────────────────────────

#' Convert ERS FAR data to pipeline haul format
#' @param ers_data  output of parse_ers_folder()
#' @return tibble in pipeline haul schema
ers_to_haul <- function(ers_data) {
  if (is.null(ers_data$far) || nrow(ers_data$far) == 0) {
    message("  No FAR records found — haul table will be empty")
    return(tibble())
  }

  far <- ers_data$far %>% filter(!is_correction)

  # Each unique FAR message = one haul.
  # The natural unique key is trip_id + source_file (one XML file = one FAR message).
  # Assign haul_id by ordering source_file (which encodes date in its name) within trip.
  far_msgs <- far %>%
    distinct(trip_id, source_file, haul_date, haul_time, lat, lon) %>%
    arrange(trip_id, haul_date, haul_time) %>%
    group_by(trip_id) %>%
    mutate(haul_id = row_number()) %>%
    ungroup()

  far <- far %>%
    left_join(far_msgs, by = c("trip_id", "source_file", "haul_date", "haul_time",
                                "lat", "lon"))

  # Build one row per haul (collapse species rows)
  haul_rows <- far %>%
    group_by(trip_id, vessel_id, haul_id, haul_date, haul_time, lat, lon,
             gear_type, mesh_mm, n_shots, duration_min, ices_rect, fao_area, eez) %>%
    slice(1) %>%
    ungroup()

  haul_rows %>%
    mutate(
      shoot_time = as.POSIXct(
        paste(haul_date, haul_time), format = "%Y-%m-%d %H:%M", tz = "UTC"
      ),
      fishing_time_hours = duration_min / 60,
      data_source = "turbocatch"
    ) %>%
    transmute(
      vessel             = vessel_id,
      trip_id            = trip_id,
      haul_id            = haul_id,
      date               = as.Date(haul_date),
      shoot_lat          = lat,
      shoot_lon          = lon,
      shoot_time         = shoot_time,
      haul_lat           = NA_real_,
      haul_lon           = NA_real_,
      haul_time          = NA_POSIXct_,
      fishing_time_hours = fishing_time_hours,
      gear_type          = gear_type,
      mesh_size_mm       = mesh_mm,
      water_depth        = NA_real_,
      n_shots            = n_shots,
      ices_rect          = ices_rect,
      fao_area           = fao_area,
      eez                = eez,
      data_source        = "turbocatch"
    )
}

# ── catch converter ───────────────────────────────────────────────────────────

#' Convert ERS FAR data to pipeline catch format (haul-level, one row per species per haul)
#' @param ers_data  output of parse_ers_folder()
#' @return tibble in pipeline catch schema
ers_to_catch <- function(ers_data) {
  if (is.null(ers_data$far) || nrow(ers_data$far) == 0) {
    message("  No FAR records found — catch table will be empty")
    return(tibble())
  }

  # Re-apply same haul_id logic as ers_to_haul(): one XML file = one haul.
  far_msgs <- ers_data$far %>%
    filter(!is_correction) %>%
    distinct(trip_id, source_file, haul_date, haul_time, lat, lon) %>%
    arrange(trip_id, haul_date, haul_time) %>%
    group_by(trip_id) %>%
    mutate(haul_id = row_number()) %>%
    ungroup()

  far_num <- ers_data$far %>%
    filter(!is_correction) %>%
    left_join(far_msgs, by = c("trip_id", "source_file", "haul_date", "haul_time",
                                "lat", "lon"))

  catch_data <- far_num %>%
    transmute(
      vessel            = vessel_id,
      trip_id           = trip_id,
      haul_id           = haul_id,
      date              = as.Date(haul_date),
      species_code      = species,
      weight_kg         = weight_kg,
      presentation      = presentation,
      freshness         = freshness,
      conversion_factor = conv_factor,
      n_boxes           = n_boxes,
      ices_rect         = ices_rect,
      gear_type         = gear_type,
      measure_method    = measure,
      data_source       = "turbocatch"
    )

  message(glue("  FAR catch: {nrow(catch_data)} species rows across ",
               "{n_distinct(catch_data$haul_id)} haul(s)"))
  catch_data
}

# ── trip converter ────────────────────────────────────────────────────────────

#' Convert ERS trip summary to pipeline trip format
#' @param ers_data  output of parse_ers_folder()
#' @return tibble in pipeline trip schema
ers_to_trip <- function(ers_data) {
  if (is.null(ers_data$trips) || nrow(ers_data$trips) == 0) {
    message("  No complete trips found")
    return(tibble())
  }

  ers_data$trips %>%
    transmute(
      vessel           = vessel_id,
      trip_id          = trip_id,
      departure_date   = as.Date(dep_date),
      departure_port   = dep_port,
      arrival_date     = as.Date(rtp_date),
      arrival_port     = rtp_port,
      gears_declared   = gears_declared,
      far_kg_total     = far_kg_total,
      n_hauls          = n_hauls,
      n_shots_total    = n_shots_total,
      fishing_areas    = fishing_areas,
      data_source      = "turbocatch"
    ) %>%
    mutate(
      gear_type = stringr::str_extract(gears_declared, "^[A-Z]+")
    )
}

# ── main processing functions ─────────────────────────────────────────────────

#' Process a folder of TurboCatch XML files for one trip and save to parquet
#'
#' @param xml_folder   Folder containing the anonymized XML files for one trip
#' @param save         If TRUE, saves to parquet via save_flyshoot_data()
#' @param date_range   Optional c("YYYY-MM-DD","YYYY-MM-DD") for parquet partitioning
#' @return Named list: $haul, $catch, $trip
process_turbocatch_trip <- function(xml_folder,
                                    save       = TRUE,
                                    date_range = NULL) {
  message(glue("\n── TurboCatch: {basename(xml_folder)} ──"))

  ers <- parse_ers_folder(xml_folder)
  if (is.null(ers)) return(invisible(NULL))

  haul_data  <- ers_to_haul(ers)
  catch_data <- ers_to_catch(ers)
  trip_data  <- ers_to_trip(ers)

  # Derive date_range from haul dates if not supplied
  if (is.null(date_range) && nrow(haul_data) > 0) {
    date_range <- c(
      as.character(min(haul_data$date, na.rm = TRUE)),
      as.character(max(haul_data$date, na.rm = TRUE))
    )
  } else if (is.null(date_range) && nrow(trip_data) > 0) {
    date_range <- c(
      as.character(min(trip_data$departure_date, na.rm = TRUE)),
      as.character(max(trip_data$arrival_date,   na.rm = TRUE))
    )
  }

  message(glue("  Trips: {nrow(trip_data)}  |  Hauls: {nrow(haul_data)}  |",
               "  Catch rows: {nrow(catch_data)}"))

  if (save) {
    if (nrow(haul_data)  > 0) save_flyshoot_data(haul_data,  "haul",  date_range)
    if (nrow(catch_data) > 0) save_flyshoot_data(catch_data, "catch", date_range)
    if (nrow(trip_data)  > 0) save_flyshoot_data(trip_data,  "trip",  date_range)
    message("  ✓ Saved to parquet")
  }

  invisible(list(haul = haul_data, catch = catch_data, trip = trip_data))
}

#' Batch-process all trip sub-folders inside a vessel folder
#'
#' Expects structure:  vessel_folder/
#'                         trip_20260102/   <- one folder per trip
#'                         trip_20260115/
#'                         ...
#' If all XML files are in one flat folder, set flat = TRUE.
#'
#' @param vessel_folder  Top-level folder for one vessel
#' @param flat           If TRUE, treat vessel_folder as a single trip folder
#' @param save           Save to parquet? Default TRUE
process_turbocatch_vessel <- function(vessel_folder,
                                       flat = FALSE,
                                       save = TRUE) {
  if (flat) {
    process_turbocatch_trip(vessel_folder, save = save)
    return(invisible(NULL))
  }

  trip_folders <- list.dirs(vessel_folder, recursive = FALSE, full.names = TRUE)
  if (length(trip_folders) == 0) {
    message("No sub-folders found — treating as flat folder")
    process_turbocatch_trip(vessel_folder, save = save)
    return(invisible(NULL))
  }

  message(glue("Processing {length(trip_folders)} trip folder(s) for ",
               "{basename(vessel_folder)}"))
  walk(trip_folders, process_turbocatch_trip, save = save)
}

# ── source-type detection patch ───────────────────────────────────────────────
# Add this block to detect_data_source_type() in 01_flyshoot_functions.R,
# or call it here to extend the function at runtime.
#
# A "turbocatch" trip is identified by the presence of .xml files whose names
# match the OOE* pattern used by TurboCatch exports.

.patch_detect_data_source <- function() {
  original_fn <- get("detect_data_source_type", envir = .GlobalEnv)

  patched_fn <- function(trip_files) {
    # Check for TurboCatch ERS XML files first
    xml_files <- trip_files %>%
      filter(tolower(tools::file_ext(file)) == "xml")

    if (nrow(xml_files) > 0) {
      message("  Detected data source: turbocatch (ERS XML)")
      return("turbocatch")
    }
    # Fall through to original detection logic
    original_fn(trip_files)
  }

  assign("detect_data_source_type", patched_fn, envir = .GlobalEnv)
  message("detect_data_source_type() patched to recognise 'turbocatch' source")
}

# Call this after sourcing 01_flyshoot_functions.R to activate the patch:
# .patch_detect_data_source()

# ── workflow integration note ─────────────────────────────────────────────────
# In 03_main_workflow.R, add this branch inside the data_source switch block:
#
#   } else if (data_source == "turbocatch") {
#     xml_folder   <- trip_files %>% filter(source == "turbocatch") %>%
#                      pull(file) %>% dirname() %>% unique()
#     tc_result    <- process_turbocatch_trip(xml_folder, save = FALSE)
#     trip_hauls   <- tc_result$haul
#     trip_catches <- tc_result$catch
#     trip_info    <- tc_result$trip
#   }

# =============================================================================
# EXAMPLE
# =============================================================================
# source("parse_ers.R")
# source("01_flyshoot_functions.R")
# source("02_storage_functions.R")
# source("04_turbocatch_adapter.R")
#
# # Process one trip folder
# result <- process_turbocatch_trip(
#   xml_folder = "C:/ERS_exports/anonymized/VESSEL01/trip_20260102",
#   save       = TRUE
# )
#
# View(result$trip)
# View(result$haul)
# View(result$catch)
#
# # Batch process all trips for one vessel
# process_turbocatch_vessel("C:/ERS_exports/anonymized/VESSEL01")
#
# # Load and compare with PEFA data
# all_catches <- load_flyshoot_data("catch",
#                                    date_from = "2026-01-01",
#                                    date_to   = "2026-12-31")
# all_catches %>% count(data_source)
# =============================================================================
