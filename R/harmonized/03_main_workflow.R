# ==============================================================================
# FLYSHOOT Main Workflow
# ==============================================================================
# Main processing script for flyshoot trip data
# Processes all data types and saves to Parquet format
#
# Aligned with Poseidat system conventions
# ==============================================================================

message(paste0("03_main_workflow.R loaded — saved: ",
               format(file.mtime(file.path(here::here(), "R/harmonized", "03_main_workflow.R")),
                      "%Y-%m-%d %H:%M:%S")))

# Setup ----
options(max.print = 999999)
options(dplyr.summarise.inform = FALSE)

# quietly(), ices_rect_centroid(), lookup_port_coord(),
# safe_remove_trips(), reconcile_types() are defined in 01_flyshoot_functions.R

rm(list=ls())

# Verbosity — set TRUE to see detailed function-level messages (schema mapping,
# column listings, haul assignment details etc.). FALSE gives a clean summary.
verbose <- FALSE
options(flyshoot.verbose = verbose)

move_files     <- FALSE # set FALSE to skip
render_reports <- TRUE   # set FALSE to skip

# Load required libraries
library(tidyverse)
library(arrow)
library(readxl)
library(lubridate)
library(sf)
library(glue)
library(jsonlite)
library(here)

# Load configuration
if (!file.exists("config.json")) {
  stop("Configuration not found. Please run 00_setup.R first.")
}

config <- read_json("config.json")

# Load harbour places for port coordinate lookup during vm synthesis
places_file <- file.path(here(), "R", "harmonized", "flyshoot_places.csv")
places_all  <- if (file.exists(places_file)) {
  readr::read_csv(places_file, col_types = "cddcc", show_col_types = FALSE)
} else {
  message("  ⚠ flyshoot_places.csv not found — port coordinates will be NA in vessel_movement")
  tibble::tibble(name = character(), lon = numeric(), lat = numeric(),
                 type = character(), harbour_code = character())
}

# Source functions
source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
source(file.path(here(), "R/harmonized", "02_storage_functions.R"))
# kisten_haul_assignment.R removed — functions now in 01_flyshoot_functions.R

if (verbose) {
if (verbose) message("\n")
if (verbose) message("=" |> rep(70) |> paste0(collapse = ""), "\n")
if (verbose) message("FLYSHOOT DATA PROCESSING WORKFLOW\n")
if (verbose) message("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
}

# Load existing data ----
if (verbose) message("Loading existing data from Parquet files...\n")

haul_existing <- tryCatch(
  quietly(load_flyshoot_data("haul")),
  error = function(e) {
    message("  No existing haul data found")
    tibble()
  }
)

trip_existing <- tryCatch(
  quietly(load_flyshoot_data("trip")),
  error = function(e) {
    message("  No existing trip data found")
    tibble()
  }
)

kisten_existing <- tryCatch(
  quietly(load_flyshoot_data("kisten")),
  error = function(e) {
    message("  No existing kisten data found")
    tibble()
  }
)

elog_existing <- tryCatch(
  quietly(load_flyshoot_data("elog")),
  error = function(e) {
    message("  No existing elog data found")
    tibble()
  }
)

elog_trek_existing <- tryCatch(
  quietly(load_flyshoot_data("elog_trek")),
  error = function(e) {
    message("  No existing elog_trek data found")
    tibble()
  }
)

# Load spatial datasets ----
if (verbose) message("Loading spatial datasets...\n")

spatial_datasets <- list()

# Load FAO areas
fao_path <- file.path(config$spatial_data, "fao_sf.RData")
if (file.exists(fao_path)) {
  load(fao_path)
  spatial_datasets$fao_sf_area <- fao_sf %>% 
    filter(F_LEVEL == "MAJOR") %>%
    select(fao_area = F_AREA)
  spatial_datasets$fao_sf_division <- fao_sf %>%
    filter(F_LEVEL == "DIVISION") %>%
    select(fao_division = F_DIVISION)
  if (verbose) message("  ✓ FAO areas loaded")
} else {
  if (verbose) message("  ⚠ FAO areas not found")
}

# Load ICES rectangles
rect_path <- file.path(config$spatial_data, "rect_sf.RData")
if (file.exists(rect_path)) {
  load(rect_path)
  spatial_datasets$rect_sf <- rect_sf %>%
    select(ices_rect = ICESNAME)
  if (verbose) message("  ✓ ICES rectangles loaded")
} else {
  if (verbose) message("  ⚠ ICES rectangles not found")
}

# Load EEZ
eez_path <- file.path(config$spatial_data, "eez_sf.RData")
if (file.exists(eez_path)) {
  load(eez_path)
  spatial_datasets$eez_sf <- eez_sf %>%
    select(economic_zone = ISO_TER1)
  if (verbose) message("  ✓ EEZ loaded")
} else {
  message("  ⚠ EEZ not found")
}

if (verbose) message("\n")

# Scan for files to process ----
if (verbose) message("Scanning for new trip data files...\n")

file_inventory <- get_file_inventory(
  input_dir = config$tripdata_input,
  file_patterns = config$file_patterns
)

if (nrow(file_inventory) == 0) {
  message("\nExiting.\n")
  # message("=" |> rep(70) |> paste0(collapse = ""), "\n")
  stop("No files to process", call. = FALSE)
}

# Display file inventory
# message("\nFiles to process:")
file_inventory %>%
  select(vessel, trip, source, filename) %>%
  print(n = Inf)

if (verbose) message("\n")

# Group files by vessel and trip ----
trip_groups <- file_inventory %>%
  group_by(vessel, trip) %>%
  summarise(
    n_files = n(),
    sources = list(source),
    files = list(file),
    .groups = "drop"
  )

if (verbose) message(glue("Found {nrow(trip_groups)} trips to process\n\n"))

# Process each trip ----
# message("=" |> rep(70) |> paste0(collapse = ""), "\n")
# message("PROCESSING TRIPS\n")
# message("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
trip_groups %>%
  select(vessel, trip, n_files, sources) %>%
  rowwise() %>%
  mutate(sources = paste(sources, collapse = ", ")) %>%
  ungroup() %>%
  print(n = Inf)



all_new_hauls           <- list()
all_new_trips           <- list()
all_new_kisten          <- list()
all_new_elogs           <- list()
all_new_elog_treks      <- list()
all_haul_assign_quality <- list()   # haul assignment quality summaries

i <- 4  # counter for trips
for (i in seq_len(nrow(trip_groups))) {
  
  trip_row  <- trip_groups[i, ]
  vessel_id <- trip_row$vessel
  trip_id   <- trip_row$trip
  sources   <- trip_row$sources[[1]]
  files     <- trip_row$files[[1]]
  
  if (verbose) message(glue("Processing {vessel_id} - Trip {trip_id} ({length(sources)} files)"))
  
  # Create trip files dataframe
  trip_files <- tibble(
    vessel = vessel_id,
    trip = trip_id,
    source = sources,
    file = files
  )
  
  # Detect data source type
  data_source <- detect_data_source_type(trip_files)
  if (verbose) message(glue("  Data source type: {data_source}"))
  
  # Process based on data source type
  trip_hauls      <- NULL
  trip_catches    <- NULL
  trip_info       <- NULL
  trip_elog       <- NULL
  trip_elog_trek  <- NULL
  
  tryCatch({
    
    if (data_source == "treklijst_full") {

      treklijst_file <- trip_files %>% filter(source == "treklijst") %>% pull(file)
      quietly(trip_hauls <- get_haul_from_treklijst(treklijst_file))
      trip_tz <- if (!is.null(trip_hauls) && "timezone" %in% names(trip_hauls))
        first(na.omit(trip_hauls$timezone)) else "Europe/Amsterdam"
      if ("kisten" %in% sources) {
        kisten_file  <- trip_files %>% filter(source == "kisten") %>% pull(file)
        quietly(
          trip_catches <- get_catch_from_kisten(kisten_file, local_tz = trip_tz,
                                                treklijst_path = treklijst_file,
                                                haul_data      = trip_hauls)
        )
        trip_elog_trek <- kisten_to_elog_trek(trip_catches, haul = trip_hauls)
      }
      quietly(trip_info <- get_trip_info(treklijst_file, haul_data = trip_hauls))

    } else if (data_source == "pefa_trek") {

      pefa_file   <- trip_files %>% filter(source == "pefa_trek") %>% pull(file)
      elog_lookup <- if (nrow(elog_existing) > 0)
        dplyr::select(elog_existing, vessel, date, trip_id) %>% dplyr::distinct()
      else NULL
      quietly({
        trip_hauls     <- get_haul_from_pefa_trek(pefa_file, elog_data = elog_lookup)
        trip_elog_trek <- get_catch_from_pefa_trek(pefa_file, elog_data = elog_lookup)
        trip_info      <- get_trip_info(pefa_file)
      })
      if ("kisten" %in% sources) {
        kisten_file <- trip_files %>% filter(source == "kisten") %>% pull(file)
        quietly(trip_catches <- get_catch_from_kisten(kisten_file))
      }

    } else if (data_source == "treklijst_simplified") {

      treklijst_file <- trip_files %>% filter(source == "treklijst") %>% pull(file)
      kisten_file    <- trip_files %>% filter(source == "kisten")    %>% pull(file)
      quietly(trip_hauls <- get_haul_from_treklijst(treklijst_file))
      local_tz <- if (!is.null(trip_hauls) && "timezone" %in% names(trip_hauls))
        first(trip_hauls$timezone) else "Europe/Amsterdam"
      quietly({
        trip_catches <- get_catch_from_kisten(kisten_file, local_tz = local_tz,
                                              treklijst_path = treklijst_file,
                                              haul_data      = trip_hauls)
        trip_info    <- get_trip_info(treklijst_file)
      })
      trip_elog_trek <- kisten_to_elog_trek(trip_catches, haul = trip_hauls)

    } else if (data_source == "kisten_pefa") {

      pefa_file   <- trip_files %>% filter(source == "pefa")   %>% pull(file)
      kisten_file <- trip_files %>% filter(source == "kisten") %>% pull(file)
      trek_file   <- if ("treklijst" %in% trip_files$source)
        trip_files %>% filter(source == "treklijst") %>% pull(file) else NULL
      quietly({
        trip_hauls   <- get_haul_from_kisten_pefa(pefa_file, kisten_file)
        trip_catches   <- get_catch_from_kisten(kisten_file, treklijst_path = trek_file)
        trip_elog_trek <- kisten_to_elog_trek(trip_catches, haul = trip_hauls)
        trip_info    <- get_trip_info(pefa_file)
      })
      if ("elog" %in% sources) {
        elog_file <- trip_files %>% filter(source == "elog") %>% pull(file)
        quietly({
          elog_result <- get_data_from_elog(elog_file)
          trip_info   <- elog_result$trip
        })
      }

    } else if (data_source == "elog_only") {

      elog_file <- trip_files %>% filter(source == "elog") %>% pull(file)
      quietly({
        elog_result <- get_data_from_elog(elog_file)
        trip_elog   <- elog_result$catch
        trip_info   <- elog_result$trip
      })
      message("  ⚠ No haul position data (elog only)")

    } else if (data_source == "elog_by_haul") {

      elog_file <- trip_files %>% filter(source == "elog") %>% pull(file)
      quietly({
        elog_result <- get_data_from_elog(elog_file)
        trip_info   <- elog_result$trip
        trip_elog   <- elog_result$catch
      })
      message("  ℹ elog_by_haul reader not yet implemented — using elog summary only")

    } else if (data_source == "kisten_only") {

      kisten_file <- trip_files %>% filter(source == "kisten") %>% pull(file)
      quietly({
        trip_catches <- get_catch_from_kisten(kisten_file)
        trip_info    <- get_trip_info(kisten_file)
      })
      message("  ⚠ No position data (kisten only)")

    } else {
      message(glue("  ⚠ Unsupported data source: {data_source}"))
    }

    # ------------------------------------------------------------------
    # Read elog independently for canonical trip_id and elog parquet entry
    # ------------------------------------------------------------------
    elog_source <- dplyr::case_when(
      "elog"  %in% sources & is.null(trip_elog) ~ "elog",
      "pefa"  %in% sources & is.null(trip_elog) ~ "pefa",
      TRUE                                       ~ NA_character_
    )
    if (!is.na(elog_source)) {
      tryCatch({
        elog_file <- trip_files %>% filter(source == elog_source) %>% pull(file)
        quietly({
          elog_result <- get_data_from_elog(elog_file)
          trip_elog   <- elog_result$catch
          if (is.null(trip_info) || nrow(trip_info) == 0)
            trip_info <- elog_result$trip
        })
      }, error = function(e) {
        message(glue("  ✗ Could not read elog: {e$message}"))
      })
    }

    # Add spatial attributes and validate
    if (!is.null(trip_hauls) && nrow(trip_hauls) > 0) {
      if (config$add_spatial_data && length(spatial_datasets) > 0)
        quietly(trip_hauls <- add_spatial_attributes(trip_hauls, spatial_datasets))
      if (config$validate_data)
        quietly(validation <- validate_haul_data(trip_hauls))
    }

    # Standardise species codes in catches
    if (!is.null(trip_catches) && nrow(trip_catches) > 0) {
      trip_catches <- trip_catches %>%
        mutate(species_code = standardize_species_codes(species_code))
    }
    if (!is.null(trip_elog_trek) && nrow(trip_elog_trek) > 0) {
      # elog_trek present — no additional action needed here
    }

  }, error = function(e) {
    message(glue("  ✗ Error processing trip: {e$message}\n"))
  })

  # ------------------------------------------------------------------
  # Unify trip_id across all datasets for this trip.
  # Rule: if an elog was processed, its trip_id (= trip_nr_elog from the
  # e-logbook system) is the canonical identifier. Overwrite trip_id in
  # all other datasets (haul, kisten, elog_trek, trip_info) so that every
  # parquet table uses the same key and joins work correctly.
  # trip_nr is preserved as the raw treklijst reis number where present.
  # ------------------------------------------------------------------
  canonical_trip_id <- NULL

  if (!is.null(trip_elog) && nrow(trip_elog) > 0 &&
      "trip_id" %in% names(trip_elog)) {
    canonical_trip_id <- first(na.omit(trip_elog$trip_id))
  }

  if (!is.null(canonical_trip_id)) {
    if (verbose) message(glue("  ✓ Canonical trip_id from elog: {canonical_trip_id}"))

    restamp <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      # Preserve the old treklijst-derived id as trip_nr if no trip_nr yet
      if ("trip_nr" %in% names(df)) {
        df <- df %>%
          mutate(trip_nr = if_else(is.na(trip_nr), trip_id, trip_nr))
      } else {
        df <- df %>% mutate(trip_nr = trip_id)
      }
      df %>% mutate(trip_id = canonical_trip_id)
    }

    trip_hauls     <- restamp(trip_hauls)
    trip_catches   <- restamp(trip_catches)
    trip_elog_trek <- restamp(trip_elog_trek)
    trip_info      <- restamp(trip_info)
    # trip_elog itself already has the correct trip_id — just ensure trip_nr
    if (!is.null(trip_elog) && !"trip_nr" %in% names(trip_elog)) {
      trip_elog <- trip_elog %>% mutate(trip_nr = trip_id)
    }
  }

  # Store processed data OUTSIDE tryCatch so partial results are never lost.
  # Even if spatial attributes or validation fails, collected data is saved.
  if (!is.null(trip_hauls) && nrow(trip_hauls) > 0) {
    all_new_hauls[[length(all_new_hauls) + 1]] <- trip_hauls
  }
  if (!is.null(trip_info) && nrow(trip_info) > 0) {
    all_new_trips[[length(all_new_trips) + 1]] <- trip_info
  }
  if (!is.null(trip_catches) && nrow(trip_catches) > 0) {
    all_new_kisten[[length(all_new_kisten) + 1]] <- trip_catches

    # Collect the haul assignment quality summary if the attribute was set
    qa <- attr(trip_catches, "haul_assignment_summary")
    if (!is.null(qa)) {
      qa <- qa %>%
        dplyr::mutate(vessel = vessel_id, trip_id = trip_id) %>%
        dplyr::select(vessel, trip_id, dplyr::everything())
      all_haul_assign_quality[[length(all_haul_assign_quality) + 1]] <- qa
    }
  }
  if (!is.null(trip_elog) && nrow(trip_elog) > 0) {
    all_new_elogs[[length(all_new_elogs) + 1]] <- trip_elog
  }
  if (!is.null(trip_elog_trek) && nrow(trip_elog_trek) > 0) {
    all_new_elog_treks[[length(all_new_elog_treks) + 1]] <- trip_elog_trek
  }
  
}

# Combine all new data ----
# Strategy: for each data type, REMOVE all existing records that belong to
# trips being reprocessed, then APPEND the new records. This ensures updated
# data always replaces old data rather than being silently deduplicated away.
if (verbose) {
if (verbose) message("\n")
if (verbose) message("=" |> rep(70) |> paste0(collapse = ""), "\n")
if (verbose) message("SAVING PROCESSED DATA\n")
if (verbose) message("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
}

# Build a lookup of all vessel+trip combinations that were processed this run.
# Priority: use canonical trip_id from the newly collected data (elog-derived
# where available), falling back to the filename-derived trip from file_inventory.
# This ensures safe_remove_trips() and report rendering use the correct ids.
canonical_ids_from_data <- dplyr::bind_rows(
  if (length(all_new_elogs)      > 0) dplyr::bind_rows(all_new_elogs)      %>% dplyr::distinct(vessel, trip_id) else tibble::tibble(),
  if (length(all_new_hauls)      > 0) dplyr::bind_rows(all_new_hauls)      %>% dplyr::distinct(vessel, trip_id) else tibble::tibble(),
  if (length(all_new_kisten)     > 0) dplyr::bind_rows(all_new_kisten)     %>% dplyr::distinct(vessel, trip_id) else tibble::tibble(),
  if (length(all_new_elog_treks) > 0) dplyr::bind_rows(all_new_elog_treks) %>% dplyr::distinct(vessel, trip_id) else tibble::tibble(),
  if (length(all_new_trips)      > 0) dplyr::bind_rows(all_new_trips)      %>% dplyr::distinct(vessel, trip_id) else tibble::tibble()
) %>% dplyr::distinct(vessel, trip_id)

# processed_trips: canonical ids only — used for report rendering and as the
# authoritative record of what was processed this run.
processed_trips <- if (nrow(canonical_ids_from_data) > 0) {
  canonical_ids_from_data
} else {
  file_inventory %>% dplyr::distinct(vessel, trip) %>% dplyr::rename(trip_id = trip)
}

# cleanup_trips: canonical + filename-derived ids — used only by safe_remove_trips()
# to also purge any stale records previously written under the old date-range id
# (e.g. "2026030920260312") when the canonical id is now "2026030900012".
filename_ids <- file_inventory %>%
  dplyr::distinct(vessel, trip) %>%
  dplyr::rename(trip_id = trip)

cleanup_trips <- dplyr::bind_rows(processed_trips, filename_ids) %>%
  dplyr::distinct(vessel, trip_id)

# Helper: safely remove processed trips from an existing dataset.
# Returns list(cleaned = tibble, n_removed = integer).
# Handles the case where existing is empty or lacks the join columns
# (e.g. first run, no parquet file yet).
# Synthesise vessel_movement entirely from haul data
# departure_port/arrival_port/dates come from the haul worksheet itself
all_new_vm <- list()
for (i in seq_along(all_new_hauls)) {
  h_i <- all_new_hauls[[i]]
  if (nrow(h_i) == 0) next

  trip_nr_val <- if ("trip_nr" %in% names(h_i)) dplyr::first(na.omit(h_i$trip_nr)) else NA_character_

  # Haul events from shoot positions
  vm_hauls <- h_i %>%
    dplyr::filter(!is.na(shoot_lat), !is.na(shoot_lon)) %>%
    dplyr::transmute(
      vessel, trip_id,
      trip_nr    = trip_nr_val,
      date       = as.Date(dplyr::coalesce(shoot_time, as.POSIXct(date))),
      event_type = "haul", haul_id = haul_id,
      port = NA_character_, lat = shoot_lat, lon = shoot_lon,
      distance = NA_real_, source = "treklijst")

  # Departure and arrival derived from haul columns
  dep_port  <- dplyr::first(na.omit(h_i$departure_port))
  dep_date  <- dplyr::first(na.omit(h_i$departure_date))
  arr_port  <- dplyr::first(na.omit(h_i$arrival_port))
  arr_date  <- dplyr::first(na.omit(h_i$arrival_date))
  vessel_id <- dplyr::first(h_i$vessel)
  trip_id_v <- dplyr::first(h_i$trip_id)

  vm_ports <- dplyr::bind_rows(
    if (!is.na(dep_date))
      tibble::tibble(vessel = vessel_id, trip_id = trip_id_v,
                     trip_nr = trip_nr_val,
                     date = as.Date(dep_date), event_type = "departure",
                     haul_id = NA_integer_, port = dep_port,
                     lat = lookup_port_coord(dep_port, "lat", places_all),
                     lon = lookup_port_coord(dep_port, "lon", places_all),
                     distance = NA_real_, source = "treklijst")
    else tibble::tibble(),
    if (!is.na(arr_date))
      tibble::tibble(vessel = vessel_id, trip_id = trip_id_v,
                     trip_nr = trip_nr_val,
                     date = as.Date(arr_date), event_type = "arrival",
                     haul_id = NA_integer_, port = arr_port,
                     lat = lookup_port_coord(arr_port, "lat", places_all),
                     lon = lookup_port_coord(arr_port, "lon", places_all),
                     distance = NA_real_, source = "treklijst")
    else tibble::tibble()
  )

  # Sort by date, with event_type order as tiebreaker within the same date:
  # departure first, hauls in haul_id order, arrival last.
  vm_i <- dplyr::bind_rows(vm_ports, vm_hauls) %>%
    dplyr::mutate(
      .sort_event = dplyr::case_when(
        event_type == "departure" ~ 0L,
        event_type == "haul"      ~ 1L,
        event_type == "arrival"   ~ 2L,
        TRUE                      ~ 1L
      ),
      .sort_haul = dplyr::if_else(is.na(haul_id), 0L, haul_id)
    ) %>%
    dplyr::arrange(date, .sort_event, .sort_haul) %>%
    dplyr::select(-.sort_event, -.sort_haul)
  if (nrow(vm_i) > 0) all_new_vm[[length(all_new_vm) + 1]] <- vm_i
}
# Synthesise vessel_movement from elog data when no haul data was produced
# (e.g. elog-only trips). Uses ICES rectangle centroids as approximate positions.
# ICES rect naming convention: "29F0" = col "F0" (lon ~0-1E), row "29" (lat ~50-50.5N).
# Centroid formula: lon = (col_index * 1) - 44 + 0.5, lat = (row * 0.5) + 35.75 + 0.25
# Build a lookup of all vm vessel/trip keys already covered by haul-based synthesis
vm_keys_existing <- if (length(all_new_vm) > 0) {
  dplyr::bind_rows(all_new_vm) %>%
    dplyr::distinct(vessel, trip_id) %>%
    dplyr::mutate(.key = paste(vessel, trip_id))
} else {
  tibble::tibble(.key = character())
}

for (i in seq_along(all_new_elogs)) {
  e_i <- all_new_elogs[[i]]

  vessel_id_e <- dplyr::first(na.omit(e_i$vessel))
  trip_id_e   <- dplyr::first(na.omit(e_i$trip_id))

  # Only synthesise vm from elog if there is no haul-based vm for this vessel/trip
  already_has_vm <- paste(vessel_id_e, trip_id_e) %in% vm_keys_existing$.key
  if (already_has_vm) next
  if (!"ices_rect" %in% names(e_i)) next

  trip_nr_e <- if ("trip_nr" %in% names(e_i)) dplyr::first(na.omit(e_i$trip_nr)) else NA_character_

  # One row per unique rect + date combination
  rect_pos <- e_i %>%
    dplyr::filter(!is.na(ices_rect)) %>%
    dplyr::group_by(date, ices_rect) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date)

  if (nrow(rect_pos) == 0) next

  centroids <- ices_rect_centroid(rect_pos$ices_rect)
  rect_pos  <- dplyr::bind_cols(rect_pos, centroids)

  vm_elog <- rect_pos %>%
    dplyr::filter(!is.na(lon), !is.na(lat)) %>%
    dplyr::transmute(
      vessel     = vessel_id_e,
      trip_id    = trip_id_e,
      trip_nr    = trip_nr_e,
      date       = as.Date(date),
      event_type = "haul",
      haul_id    = NA_integer_,
      port       = NA_character_,
      lat        = lat,
      lon        = lon,
      distance   = NA_real_,
      source     = "elog_rect"
    )

  # Get departure/arrival from the trip record (all_new_trips has these from elog$trip)
  trip_meta <- if (length(all_new_trips) > 0) {
    dplyr::bind_rows(all_new_trips) %>%
      dplyr::filter(vessel == vessel_id_e, trip_id == trip_id_e) %>%
      dplyr::slice(1)
  } else { tibble::tibble() }

  dep_port_e <- if (nrow(trip_meta) > 0 && "departure_port" %in% names(trip_meta)) trip_meta$departure_port[1] else NA_character_
  dep_date_e <- if (nrow(trip_meta) > 0 && "departure_date" %in% names(trip_meta)) as.Date(trip_meta$departure_date[1]) else as.Date(NA)
  arr_port_e <- if (nrow(trip_meta) > 0 && "arrival_port"   %in% names(trip_meta)) trip_meta$arrival_port[1]   else NA_character_
  arr_date_e <- if (nrow(trip_meta) > 0 && "arrival_date"   %in% names(trip_meta)) as.Date(trip_meta$arrival_date[1])   else as.Date(NA)

  vm_ports_e <- dplyr::bind_rows(
    if (!is.na(dep_date_e))
      tibble::tibble(vessel = vessel_id_e, trip_id = trip_id_e, trip_nr = trip_nr_e,
                     date = dep_date_e, event_type = "departure", haul_id = NA_integer_,
                     port = dep_port_e, lat = lookup_port_coord(dep_port_e, "lat", places_all),
                     lon  = lookup_port_coord(dep_port_e, "lon", places_all),
                     distance = NA_real_, source = "elog_rect")
    else tibble::tibble(),
    if (!is.na(arr_date_e))
      tibble::tibble(vessel = vessel_id_e, trip_id = trip_id_e, trip_nr = trip_nr_e,
                     date = arr_date_e, event_type = "arrival", haul_id = NA_integer_,
                     port = arr_port_e, lat = lookup_port_coord(arr_port_e, "lat", places_all),
                     lon  = lookup_port_coord(arr_port_e, "lon", places_all),
                     distance = NA_real_, source = "elog_rect")
    else tibble::tibble()
  )

  vm_e_i <- dplyr::bind_rows(vm_ports_e, vm_elog) %>%
    dplyr::mutate(
      .sort_event = dplyr::case_when(
        event_type == "departure" ~ 0L,
        event_type == "haul"      ~ 1L,
        event_type == "arrival"   ~ 2L, TRUE ~ 1L),
      .sort_haul = dplyr::if_else(is.na(haul_id), 0L, haul_id)
    ) %>%
    dplyr::arrange(date, .sort_event, .sort_haul) %>%
    dplyr::select(-.sort_event, -.sort_haul)

  if (nrow(vm_e_i) > 0) {
    all_new_vm[[length(all_new_vm) + 1]] <- vm_e_i
    message(glue("  VM from elog rects: {nrow(vm_e_i)} events for {vessel_id_e} / {trip_id_e}"))
  }
}

if (length(all_new_vm) > 0) {
  vm_existing <- tryCatch(quietly(load_flyshoot_data("vessel_movement")), error = function(e) tibble::tibble())
  new_vm <- dplyr::bind_rows(all_new_vm)
  r_vm   <- safe_remove_trips(vm_existing, cleanup_trips, new_data = new_vm)
  combined_vm <- reconcile_types(r_vm$cleaned, new_vm) %>%
    dplyr::bind_rows(new_vm) %>% dplyr::arrange(vessel, trip_id, date)
quietly(  save_flyshoot_data(combined_vm, "vessel_movement",
    date_range = c(min(combined_vm$date, na.rm=TRUE), max(combined_vm$date, na.rm=TRUE))))
  if (verbose) message(glue("  Vessel movement: {nrow(new_vm)} synthetic events added"))
}

if (length(all_new_hauls) > 0) {
  new_hauls <- bind_rows(all_new_hauls)
  r         <- safe_remove_trips(haul_existing, cleanup_trips)
  combined_hauls <- reconcile_types(r$cleaned, new_hauls) %>%
    bind_rows(new_hauls) %>%
    arrange(vessel, trip_id, haul_id)
quietly(  save_flyshoot_data(combined_hauls, "haul",
    date_range = c(min(combined_hauls$date, na.rm = TRUE),
                   max(combined_hauls$date, na.rm = TRUE))))
  if (verbose) message(glue("  Hauls: removed {r$n_removed} old records, ",
               "added {nrow(new_hauls)} new (total: {nrow(combined_hauls)})"))
}

if (length(all_new_trips) > 0) {
  new_trips <- bind_rows(all_new_trips)
  r         <- safe_remove_trips(trip_existing, cleanup_trips)
  # Ensure trip_nr exists in old records (added by restamp() for new data only)
  existing_trips <- r$cleaned
  if (nrow(existing_trips) > 0 && !"trip_nr" %in% names(existing_trips)) {
    existing_trips <- existing_trips %>% mutate(trip_nr = NA_character_)
  }
  combined_trips <- reconcile_types(existing_trips, new_trips) %>%
    bind_rows(new_trips) %>%
    arrange(vessel, trip_id)
quietly(  save_flyshoot_data(combined_trips, "trip",
    date_range = c(min(combined_trips$departure_date, na.rm = TRUE),
                   max(combined_trips$arrival_date, na.rm = TRUE))))
  if (verbose) message(glue("  Trips: removed {r$n_removed} old records, ",
               "added {nrow(new_trips)} new (total: {nrow(combined_trips)})"))
}

if (length(all_new_kisten) > 0) {
  new_kisten <- bind_rows(all_new_kisten)
  r          <- safe_remove_trips(kisten_existing, cleanup_trips)
  combined_kisten <- reconcile_types(r$cleaned, new_kisten) %>%
    bind_rows(new_kisten) %>%
    arrange(vessel, trip_id, haul_id, species_code)
quietly(  save_flyshoot_data(combined_kisten, "kisten",
    date_range = c(min(combined_kisten$date, na.rm = TRUE),
                   max(combined_kisten$date, na.rm = TRUE))))
  if (verbose) message(glue("  Kisten: removed {r$n_removed} old records, ",
               "added {nrow(new_kisten)} new (total: {nrow(combined_kisten)})"))
}

if (length(all_new_elogs) > 0) {
  new_elogs <- bind_rows(all_new_elogs)
  r         <- safe_remove_trips(elog_existing, cleanup_trips)
  
  # Schema compatibility check: only warn if new data has columns not in existing
  if (nrow(r$cleaned) > 0) {
    new_only <- setdiff(names(new_elogs), names(r$cleaned))
    if (length(new_only) > 0) {
      warning(glue(
        "Elog schema mismatch: new data has columns not in existing parquet:\n",
        "  {paste(new_only, collapse=', ')}\n",
        "  Run migrate_elog_schema.R to align the existing data."
      ))
    }
  }
  
  combined_elogs <- reconcile_types(r$cleaned, new_elogs) %>%
    bind_rows(new_elogs) %>%
    arrange(vessel, trip_id, date, species_code)
quietly(  save_flyshoot_data(combined_elogs, "elog",
    date_range = c(min(combined_elogs$date, na.rm = TRUE),
                   max(combined_elogs$date, na.rm = TRUE))))
  if (verbose) message(glue("  Elog: removed {r$n_removed} old records, ",
               "added {nrow(new_elogs)} new (total: {nrow(combined_elogs)})"))
}

if (length(all_new_elog_treks) > 0) {
  new_elog_treks <- bind_rows(all_new_elog_treks)
  r              <- safe_remove_trips(elog_trek_existing, cleanup_trips)
  combined_elog_treks <- reconcile_types(r$cleaned, new_elog_treks) %>%
    bind_rows(new_elog_treks) %>%
    arrange(vessel, trip_id, haul_id, species_code)
quietly(  save_flyshoot_data(combined_elog_treks, "elog_trek",
    date_range = c(min(combined_elog_treks$date, na.rm = TRUE),
                   max(combined_elog_treks$date, na.rm = TRUE))))
  if (verbose) message(glue("  Elog_trek: removed {r$n_removed} old records, ",
               "added {nrow(new_elog_treks)} new (total: {nrow(combined_elog_treks)})"))
}

# ==============================================================================
# MOVE FILES
# ==============================================================================


if (move_files && nrow(file_inventory) > 0) {

  message("
")
  message("Moving processed files to vessel-specific folders...
")

  for (j in seq_len(nrow(file_inventory))) {
    file_path <- file_inventory$file[j]
    vessel_j  <- file_inventory$vessel[j]
    dest_dir  <- file.path(config$tripdata, vessel_j)

    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
      message(glue("  Created folder: {dest_dir}"))
    }

    dest_path <- file.path(dest_dir, basename(file_path))

    tryCatch({
      file.copy(file_path, dest_path, overwrite = TRUE)
      file.remove(file_path)
      message(glue("  ✓ Moved to {vessel_j}/: {basename(file_path)}"))
    }, error = function(e) {
      message(glue("  ✗ Failed: {basename(file_path)} — {e$message}"))
    })
  }

}



# ==============================================================================
# Create summary report ----
# ==============================================================================

if (verbose) {
if (verbose) message("\n")
if (verbose) message("=" |> rep(70) |> paste0(collapse = ""), "\n")
if (verbose) message("PROCESSING SUMMARY\n")
if (verbose) message("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
}

if (verbose) message(glue("Files processed: {nrow(file_inventory)}"))
if (verbose) message(glue("Trips processed: {nrow(trip_groups)}"))

if (length(all_new_hauls) > 0) {
  if (verbose) message(glue("New hauls: {nrow(bind_rows(all_new_hauls))}"))
}
if (length(all_new_trips) > 0) {
  if (verbose) message(glue("New trips: {nrow(bind_rows(all_new_trips))}"))
}
if (length(all_new_kisten) > 0) {
  if (verbose) message(glue("New catch records (kisten): {nrow(bind_rows(all_new_kisten))}"))
}
if (length(all_new_elogs) > 0) {
  if (verbose) message(glue("New elog records: {nrow(bind_rows(all_new_elogs))}"))
}
if (length(all_new_elog_treks) > 0) {
  if (verbose) message(glue("New elog_trek records: {nrow(bind_rows(all_new_elog_treks))}"))
}

if (verbose) message("\n")

# Show added data statistics per vessel and data type
get_added_data_stats(
  hauls      = all_new_hauls,
  kisten     = all_new_kisten,
  elogs      = all_new_elogs,
  elog_treks = all_new_elog_treks,
  trips      = all_new_trips,
  inventory  = file_inventory
)

if (verbose) message("\n")

# ==============================================================================
# HAUL ASSIGNMENT QUALITY REPORT (consolidated across all trips this run)
# ==============================================================================
if (length(all_haul_assign_quality) > 0) {

  qa_all <- dplyr::bind_rows(all_haul_assign_quality)

  sep70 <- strrep("=", 70)
  sep60 <- strrep("-", 60)
  message(sep70)
  message("HAUL ASSIGNMENT QUALITY — CONSOLIDATED REPORT")
  message(sep70)

  # Guard: quality_flag column may be absent if summary was built without it
  if (!"quality_flag" %in% names(qa_all)) {
    if (verbose) message("  ℹ quality_flag column not present — will show as unknown")
    qa_all <- qa_all %>% dplyr::mutate(quality_flag = "unknown")
  }

  # Overall counts
  n_total  <- nrow(qa_all)
  qa_counts <- qa_all %>%
    dplyr::count(quality_flag, name = "n") %>%
    dplyr::mutate(pct = round(100 * n / n_total)) %>%
    dplyr::arrange(dplyr::desc(n))

  message(glue("  Total hauls assessed: {n_total}"))
  message(glue("  Quality breakdown:"))
  qa_counts %>%
    dplyr::mutate(.line = glue("    {formatC(quality_flag, width = -12)} : {n}  ({pct}%)")) %>%
    dplyr::pull(.line) %>%
    { for (l in .) message(l) }

  # Per-vessel / per-trip breakdown showing non-ok hauls
  message(sep60)
  message("  Per-trip detail (non-ok hauls only):")
  message(sep60)

  problems <- qa_all %>%
    dplyr::filter(quality_flag != "ok") %>%
    dplyr::select(dplyr::any_of(c("vessel", "trip_id", "haul_id",
                                   "trek_catch_kg", "weighed_kg", "weight_ratio",
                                   "anchor_offset_min", "quality_flag"))) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of("trek_catch_kg"),     ~round(as.double(.), 1)),
      dplyr::across(dplyr::any_of("weighed_kg"),        ~round(as.double(.), 1)),
      dplyr::across(dplyr::any_of("weight_ratio"),      ~round(as.double(.), 2)),
      dplyr::across(dplyr::any_of("anchor_offset_min"), ~round(as.double(.), 0))
    ) %>%
    dplyr::arrange(vessel, trip_id, haul_id)

  if (nrow(problems) == 0) {
    message("  ✓ No assignment problems found across all processed trips.")
  } else {
    message(glue("  ⚠ {nrow(problems)} haul(s) with potential assignment issues:"))
    print(problems, n = 200)

    # Guidance per flag type
    message(sep60)
    flag_types <- unique(problems$quality_flag)
    if ("empty" %in% flag_types)
      message("  empty       : No kisten session assigned. ",
              "Could be genuine zero-catch, or a session that was not ",
              "detected (gap threshold too large).")
    if ("offset" %in% flag_types)
      message(glue("  offset      : Session start deviates > {KISTEN_FLAG_MIN} min from ",
                   "expected anchor. Check whether the kisten clock matches the ",
                   "treklijst timezone."))
    if ("weight_low" %in% flag_types)
      message("  weight_low  : Marelec weighed < 50% of treklijst catch. ",
              "Session may have landed on the wrong haul, or a kisten file ",
              "was processed separately.")
    if ("weight_high" %in% flag_types)
      message("  weight_high : Marelec weighed > 150% of treklijst catch. ",
              "Multiple sessions may have been merged, or sessions from a ",
              "different haul were assigned here.")
  }

  message(sep70)
  message("\n")
}

# Show storage statistics
get_storage_stats()

# ==============================================================================
# AUTO-RENDER TRIPREPORTS FOR NEWLY PROCESSED TRIPS
# ==============================================================================
# After parquet files are written, render a tripreport for each vessel/trip
# that was successfully processed in this run.
#
# The date range for each report is derived from the actual data just saved,
# not from a manual date specification — so no extra configuration is needed.
#
# Set render_reports <- FALSE to skip report generation entirely.
# ==============================================================================


if (render_reports && nrow(processed_trips) > 0) {

  rmd_file <- file.path(here(), "R", "harmonized", "FLYSHOOT_tripreport_harmonized.Rmd")

  if (!file.exists(rmd_file)) {
    message("⚠ Tripreport Rmd not found — skipping report generation:")
    message(glue("    {rmd_file}"))

  } else {

    # message(strrep("=", 50), "\n")
    # message("AUTO-RENDERING TRIPREPORTS\n")
    # message(strrep("=", 50), "\n")
    message(strrep("=", 50))
    message("AUTO-RENDERING TRIPREPORTS\n")
    message(strrep("=", 50))
    
    # (storage functions already loaded at top of script)

    # Build per-vessel date ranges from the data just written to parquet.
    # We prefer elog (most complete date coverage), then haul, then kisten.
    # This reuses the parquet we just wrote so there is no re-reading of
    # raw files and the date range always matches exactly what was processed.
    derive_trip_dates <- function(vessel_id, trip_id_chr) {
      # First try: use in-memory data from this run (fastest, most accurate)
      in_memory <- dplyr::bind_rows(
        if (length(all_new_elogs)      > 0) dplyr::bind_rows(all_new_elogs)      %>%
          dplyr::filter(vessel == vessel_id, trip_id == trip_id_chr) else tibble::tibble(),
        if (length(all_new_elog_treks) > 0) dplyr::bind_rows(all_new_elog_treks) %>%
          dplyr::filter(vessel == vessel_id, trip_id == trip_id_chr) else tibble::tibble(),
        if (length(all_new_hauls)      > 0) dplyr::bind_rows(all_new_hauls)      %>%
          dplyr::filter(vessel == vessel_id, trip_id == trip_id_chr) else tibble::tibble(),
        if (length(all_new_kisten)     > 0) dplyr::bind_rows(all_new_kisten)     %>%
          dplyr::filter(vessel == vessel_id, trip_id == trip_id_chr) else tibble::tibble()
      )
      if (nrow(in_memory) > 0 && "date" %in% names(in_memory)) {
        return(list(
          startdate = min(as.Date(in_memory$date), na.rm = TRUE),
          enddate   = max(as.Date(in_memory$date), na.rm = TRUE)
        ))
      }

      # Fallback: read from parquet (handles re-render of previously processed trips)
      for (type in c("elog", "elog_trek", "haul", "kisten")) {
        dat <- tryCatch(
          quietly(load_flyshoot_data(type, vessel_ids = vessel_id)),
          error = function(e) tibble::tibble()
        )
        if (nrow(dat) == 0 || !"date" %in% names(dat) || !"trip_id" %in% names(dat)) next
        filtered <- dplyr::filter(dat, vessel == vessel_id, trip_id == trip_id_chr)
        if (nrow(filtered) > 0) {
          return(list(
            startdate = min(as.Date(filtered$date), na.rm = TRUE),
            enddate   = max(as.Date(filtered$date), na.rm = TRUE)
          ))
        }
      }
      NULL   # no date range found — report will be skipped
    }

    # Render one report; returns a one-row tibble with status info
    render_one_report <- function(vessel_id, trip_id_chr, lang = "nl") {

      dates <- derive_trip_dates(vessel_id, trip_id_chr)

      if (is.null(dates)) {
        message(glue("  ⊘ {vessel_id} / {trip_id_chr} — no date range found, skipping"))
        return(tibble(vessel = vessel_id, trip_id = trip_id_chr,
                      status = "skipped", reason = "no date range", path = NA_character_,
                      elapsed = NA_real_, size_kb = NA_real_))
      }

      startdate <- dates$startdate
      enddate   <- dates$enddate

      # Build output filename matching batch_render_tripreports.R convention:
      # {vessel}_{yr}W{wk}_{n}trip(s).docx
      yr   <- lubridate::year(startdate)
      wk   <- lubridate::isoweek(startdate)
      output_filename <- glue(
        "{vessel_id}_{yr}W{sprintf('%02d', wk)}_1trip.docx"
      )

      output_dir  <- file.path(config$tripdata, vessel_id)
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      output_path <- file.path(output_dir, output_filename)

      # Abort gracefully if Word has the file locked; otherwise delete for clean overwrite
      if (file.exists(output_path)) {
        locked <- tryCatch(
          { con <- file(output_path, open = "ab"); close(con); FALSE },
          error = function(e) TRUE
        )
        if (locked) {
          message(glue("  ✗ {vessel_id} / {trip_id_chr} — file open in Word: {output_filename}"))
          return(tibble(vessel = vessel_id, trip_id = trip_id_chr,
                        status = "error", reason = "file locked by Word",
                        path = NA_character_, elapsed = NA_real_, size_kb = NA_real_))
        }
        file.remove(output_path)
      }

      # message(glue("  Rendering: {output_filename}"))
      # message(glue("    {format(startdate, '%d/%m/%Y')} – {format(enddate, '%d/%m/%Y')}"))
      message(glue("  Rendering: {output_filename} | {format(startdate, '%d/%m/%Y')}-{format(enddate, '%d/%m/%Y')}"))

      t0 <- Sys.time()

      options(
        flyshoot.vessel      = vessel_id,
        flyshoot.startdate   = startdate,
        flyshoot.enddate     = enddate,
        flyshoot.lang        = lang,
        flyshoot.max_species = 12
      )

      tryCatch(
        rmarkdown::render(input = rmd_file, output_file = output_path, quiet = TRUE),
        error = function(e) {
          if (!file.exists(output_path)) stop(e$message)
          message(paste0("  ⚠ Render warning (file still created): ", e$message))
        }
      )

      elapsed   <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
      file_size <- if (file.exists(output_path))
        round(file.info(output_path)$size / 1024, 1) else NA_real_
      status    <- if (file.exists(output_path)) "success" else "error"

      message(glue("  {ifelse(status=='success','✓','✗')} {output_filename}",
                   " ({elapsed}s, {file_size} KB)"))

      result <- tibble(vessel = vessel_id, trip_id = trip_id_chr, status = status,
                       reason = NA_character_, path = output_path,
                       elapsed = elapsed, size_kb = file_size)

      result
    }

    # Group trips by vessel + ISO week so that multiple trips in the same
    # week produce a single combined report rather than duplicate filenames.
    render_groups <- processed_trips %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dates = list(derive_trip_dates(vessel, trip_id))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!sapply(dates, is.null)) %>%
      dplyr::mutate(
        startdate = as.Date(sapply(dates, `[[`, "startdate"), origin = "1970-01-01"),
        enddate   = as.Date(sapply(dates, `[[`, "enddate"),   origin = "1970-01-01"),
        yr        = lubridate::year(startdate),
        wk        = lubridate::isoweek(startdate)
      ) %>%
      dplyr::group_by(vessel, yr, wk) %>%
      dplyr::summarise(
        trip_ids  = paste(sort(unique(trip_id)), collapse = "+"),
        startdate = min(startdate),
        enddate   = max(enddate),
        n_trips   = dplyr::n(),
        .groups   = "drop"
      )

    # Render one report per vessel+week group
    report_results <- vector("list", nrow(render_groups))
    for (i in seq_len(nrow(render_groups))) {
      grp <- render_groups[i, ]

      vessel_id  <- grp$vessel
      startdate  <- grp$startdate
      enddate    <- grp$enddate
      n_trips    <- grp$n_trips
      trip_label <- grp$trip_ids

      trip_word       <- if (n_trips == 1) "1trip" else glue("{n_trips}trips")
      output_filename <- glue("{vessel_id}_{grp$yr}W{sprintf('%02d', grp$wk)}_{trip_word}.docx")
      output_dir      <- file.path(config$tripdata, vessel_id)
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      output_path     <- file.path(output_dir, output_filename)

      if (file.exists(output_path)) {
        locked <- tryCatch(
          { con <- file(output_path, open = "ab"); close(con); FALSE },
          error = function(e) TRUE
        )
        if (locked) {
          message(glue("  \u2717 {vessel_id} / {trip_label} \u2014 file open in Word: {output_filename}"))
          report_results[[i]] <- tibble::tibble(
            vessel = vessel_id, trip_id = trip_label,
            status = "error", reason = "file locked by Word",
            path = NA_character_, elapsed = NA_real_, size_kb = NA_real_)
          next
        }
        file.remove(output_path)
      }

      message(glue("  Rendering: {output_filename} | {format(startdate, '%d/%m/%Y')}-{format(enddate, '%d/%m/%Y')}"))
      t0 <- Sys.time()

      options(
        flyshoot.vessel      = vessel_id,
        flyshoot.startdate   = startdate,
        flyshoot.enddate     = enddate,
        flyshoot.lang        = "nl",
        flyshoot.max_species = 12
      )

      result <- tryCatch(
        {
          rmarkdown::render(input = rmd_file, output_file = output_path, quiet = TRUE)
          elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
          size_kb <- round(file.size(output_path) / 1024, 1)
          message(glue("  \u2713 {output_filename} ({elapsed}s, {size_kb} KB)"))
          tibble::tibble(vessel = vessel_id, trip_id = trip_label,
                         status = "success", reason = NA_character_,
                         path = output_path, elapsed = elapsed, size_kb = size_kb)
        },
        error = function(e) {
          if (!file.exists(output_path)) {
            message(glue("  \u2717 {output_filename}: {e$message}"))
            tibble::tibble(vessel = vessel_id, trip_id = trip_label,
                           status = "error", reason = e$message,
                           path = NA_character_, elapsed = NA_real_, size_kb = NA_real_)
          } else {
            elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
            size_kb <- round(file.size(output_path) / 1024, 1)
            message(glue("  \u26a0 {output_filename} rendered with warnings ({elapsed}s)"))
            tibble::tibble(vessel = vessel_id, trip_id = trip_label,
                           status = "success", reason = e$message,
                           path = output_path, elapsed = elapsed, size_kb = size_kb)
          }
        }
      )
      report_results[[i]] <- result
    }
    report_results <- dplyr::bind_rows(report_results)

    # Summary
    # message("\n", strrep("=", 50))
    # message("TRIPREPORT SUMMARY")
    # message(strrep("=", 50))
    # 
    # message(glue("  ✓ Rendered:  {sum(report_results$status == 'success')}"))
    # message(glue("  ⊘ Skipped:   {sum(report_results$status == 'skipped')}"))
    # message(glue("  ✗ Errors:    {sum(report_results$status == 'error')}"))

    if (any(report_results$status == "success")) {
      message("\nReports written to:")
      success_paths <- report_results %>%
        dplyr::filter(status == "success") %>%
        dplyr::pull(path)
      for (p in success_paths) message(glue("  {p}"))
    }

    if (any(report_results$status == "error")) {
      message("\nErrors:")
      report_results %>%
        dplyr::filter(status == "error") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(.msg = glue("  {vessel}/{trip_id}: {reason}")) %>%
        dplyr::ungroup() %>%
        dplyr::pull(.msg) %>%
        { for (msg in .) message(msg) }
    }

    message("\n")
  }
}
