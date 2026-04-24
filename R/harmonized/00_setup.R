# ==============================================================================
# FLYSHOOT Data Management System - Setup
# ==============================================================================
# This script sets up the environment, installs required packages, and 
# configures directory structure
#
# Aligned with Poseidat naming conventions and structure
# ==============================================================================

# Required packages ----
required_packages <- c(
  "tidyverse",      # Data manipulation
  "arrow",          # Parquet file handling
  "readxl",         # Excel file reading
  "writexl",        # Excel file writing
  "lubridate",      # Date/time handling
  "sf",             # Spatial features
  "tidygeocoder",   # Geocoding
  "sqldf",          # SQL operations
  "zoo",            # Time series
  "glue",           # String interpolation
  "fs",             # File system operations
  "here",           # Find root directory
  "jsonlite"        # JSON handling
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("Installing required packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(readxl)
  library(writexl)
  library(lubridate)
  library(sf)
  library(tidygeocoder)
  library(sqldf)
  library(zoo)
  library(glue)
  library(fs)
  library(here)
  library(jsonlite)
})

cat("✓ All required packages loaded\n\n")

# auto haul-ID assignment for kisten files
source(file.path(here(), "R/harmonized", "kisten_haul_assignment.R")) 

# Configure OneDrive path ----
# IMPORTANT: Set your OneDrive paths here
configure_onedrive_paths <- function() {
  
  # Set your OneDrive paths
  # Update these to match your actual file structure
  paths <- list(
    onedrive_data  = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data",
    onedrive_rdata = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
    tripdata       = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
    spatial_data   = "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
  )
  
  # Check if paths exist
  for (name in names(paths)) {
    if (!dir.exists(paths[[name]])) {
      cat("⚠ Warning: Path does not exist:", paths[[name]], "\n")
      cat("  Please update this path in 00_setup.R\n\n")
    } else {
      cat("✓", name, ":", paths[[name]], "\n")
    }
  }
  
  cat("\n")
  return(paths)
}

onedrive_paths <- configure_onedrive_paths()

# Create directory structure ----
create_directory_structure <- function(base_path) {
  
  # Subdirectories for flyshoot data
  dirs <- c(
    file.path(base_path, "raw/haul"),
    file.path(base_path, "raw/trip"),
    file.path(base_path, "raw/kisten"),
    file.path(base_path, "raw/elog"),
    file.path(base_path, "raw/elog_trek"),
    file.path(base_path, "raw/vessel_movement"),
    
    file.path(base_path, "processed/trip_summaries"),
    file.path(base_path, "processed/catch_summaries"),

    file.path(base_path, "cache"),
    file.path(base_path, "outputs/reports"),
    file.path(base_path, "outputs/figures"),
    file.path(base_path, "archive")
  )
  
  for (dir in dirs) {
    dir_create(dir)
  }
  
  cat("✓ Directory structure created:\n")
  cat(paste0("  ", dirs, collapse = "\n"), "\n\n")
}

create_directory_structure(onedrive_paths$onedrive_data)

# Configuration ----
config <- list(
  # Base paths
  onedrive_data = onedrive_paths$onedrive_data,
  onedrive_rdata = onedrive_paths$onedrive_rdata,
  tripdata = onedrive_paths$tripdata,
  spatial_data = onedrive_paths$spatial_data,
  
  # Data paths (using parquet format)
  raw_data_path = file.path(onedrive_paths$onedrive_data, "raw"),
  processed_data_path = file.path(onedrive_paths$onedrive_data, "processed"),
  outputs_path = file.path(onedrive_paths$onedrive_data, "outputs"),
  cache_path = file.path(onedrive_paths$onedrive_data, "cache"),
  archive_path = file.path(onedrive_paths$onedrive_data, "archive"),
  
  # Input directories (legacy format locations)
  tripdata_input = file.path(onedrive_paths$tripdata, "_te verwerken"),
  tripdata_processed = file.path(onedrive_paths$tripdata, "_verwerkt"),
  
  # File patterns for detection
  file_patterns = list(
    treklijst = "treklijst",
    kisten = "kisten",
    pefa = "elog pefa",
    pefa_trek = "elog_pefa_per_trek",
    mcatch = "elog mcatch"
  ),
  
  # Date format
  date_format = "%Y-%m-%d",
  
  # Processing options
  add_spatial_data = TRUE,
  validate_data = TRUE
)

# Save configuration ----
# Config is saved locally in your git folder for easy access
config_path <- "config.json"
write_json(config, config_path, pretty = TRUE, auto_unbox = TRUE)
cat("✓ Configuration saved to", config_path, "\n")
cat("  Data will be stored in:", config$onedrive_data, "\n\n")

# ==============================================================================
# CONVERT RDATA -> PARQUET ----
# Replaces the old bare migrate_legacy_data() with full schema alignment,
# type coercion, legal/undersized pivot, and vessel movement log.
# Run once when setting up the system for the first time.
# ==============================================================================

convert_rdata_to_parquet <- function() {
  
  rdata_path <- config$onedrive_rdata
  
  # Require flyshoot functions for parse_marelec_species()
  functions_file <- file.path(here::here(), "R/harmonized3/01_flyshoot_functions.R")
  if (!file.exists(functions_file)) {
    stop("01_flyshoot_functions.R not found. Please source it first.")
  }
  source(functions_file)
  
  # ── Helpers ──────────────────────────────────────────────────────────────────
  
  write_clean_parquet <- function(data, dtype) {
    out_dir  <- file.path(config$raw_data_path, dtype)
    dir_create(out_dir)
    old <- list.files(out_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(old) > 0) { file.remove(old) }
    out_path <- file.path(out_dir, glue("{dtype}.parquet"))
    write_parquet(
      data %>% mutate(save_date = Sys.Date(), save_timestamp = Sys.time()),
      out_path
    )
    cat(glue("    ✓ {dtype}.parquet  ({nrow(data)} rows)\n"))
    invisible(out_path)
  }
  
  drop_all_na_cols <- function(df) {
    df[, colSums(!is.na(df)) > 0, drop = FALSE]
  }
  
  # Pivot wide legal+undersized catch to long, adding size_category column.
  # All original rows -> size_category = "legal"
  # Rows where weight_undersized_kg > 0 -> additional rows with size_category = "undersized"
  # Gracefully handles datasets where the undersized columns are absent (all-NA and dropped).
  pivot_catch_long <- function(df) {
    has_undersized <- all(c("weight_undersized_kg", "boxes_undersized") %in% names(df))
    
    id_cols <- setdiff(names(df),
                       c("weight_kg", "box_count",
                         "weight_undersized_kg", "boxes_undersized"))
    
    legal <- df %>%
      select(all_of(id_cols), weight_kg, box_count) %>%
      mutate(size_category = "legal")
    
    if (has_undersized) {
      undersized <- df %>%
        filter(!is.na(weight_undersized_kg) & weight_undersized_kg > 0) %>%
        select(all_of(id_cols),
               weight_kg = weight_undersized_kg,
               box_count = boxes_undersized) %>%
        mutate(size_category = "undersized", box_count = as.integer(box_count))
      bind_rows(legal, undersized)
    } else {
      legal
    } %>%
      select(all_of(id_cols), size_category, weight_kg, box_count)
  }
  
  # ── HAUL ─────────────────────────────────────────────────────────────────────
  cat("\n  Converting haul...\n")
  haul_env <- new.env()
  load(file.path(rdata_path, "haul.RData"), envir = haul_env)
  
  haul_env$haul %>%
    select(-any_of(c("year", "quarter", "month", "week", "yday",
                     "source", "file", "NA",
                     "rect_calc", "division_calc", "economiczone_calc",
                     "captain"))) %>%
    rename(
      trip_nr             = trip,         haul_id             = haul,
      shoot_time          = shoottime,    haul_time           = haultime,
      shoot_end_time      = shoottime2,   next_haul_time      = nexthaultime,
      shoot_lon           = lon,          shoot_lat           = lat,
      wind_direction      = winddirection,wind_force_bft      = windforce,
      water_depth         = waterdepth,   catch_height_cm     = catchheight,
      box_type            = boxtype,      marketable_catch_kg = landingweight,
      total_catch_kg      = catchweight,  bycatch_pct         = bycatchperc,
      gear_type           = gear,         mesh_size_mm        = meshsize,
      vertical_opening_m  = vertopening,  cable_length_m      = cablelength,
      cable_thickness_mm  = cablethickness, groundrope_length_m = lengthgroundrope,
      escape_panel        = escapepanel,  fishing_time_hours  = duration,
      fao_area            = area,         fao_subarea         = subarea,
      fao_division        = division,     ices_rect           = rect,
      economic_zone       = economiczone, departure_date      = departuredate,
      departure_port      = departureport,arrival_date        = arrivaldate,
      arrival_port        = arrivalport
    ) %>%
    mutate(
      trip_id            = as.character(trip_nr),
      date               = as.Date(date),
      shoot_time         = as.POSIXct(shoot_time,     tz = "UTC"),
      haul_time          = as.POSIXct(haul_time,      tz = "UTC"),
      shoot_end_time     = as.POSIXct(shoot_end_time, tz = "UTC"),
      next_haul_time     = as.POSIXct(next_haul_time, tz = "UTC"),
      departure_date     = as.Date(departure_date),
      arrival_date       = as.Date(arrival_date),
      haul_id            = as.integer(haul_id),
      mesh_size_mm       = as.integer(suppressWarnings(as.numeric(mesh_size_mm))),
      water_depth        = as.integer(suppressWarnings(as.numeric(water_depth))),
      wind_force_bft     = as.integer(suppressWarnings(as.numeric(wind_force_bft))),
      vertical_opening_m = as.integer(suppressWarnings(as.numeric(vertical_opening_m))),
      cable_length_m     = as.integer(suppressWarnings(as.numeric(cable_length_m))),
      cable_thickness_mm = as.integer(suppressWarnings(as.numeric(cable_thickness_mm))),
      groundrope_length_m = as.numeric(suppressWarnings(as.numeric(groundrope_length_m))),
      fishing_time_hours = as.numeric(suppressWarnings(as.numeric(fishing_time_hours))),
      total_catch_kg     = as.numeric(suppressWarnings(as.numeric(total_catch_kg))),
      marketable_catch_kg = as.numeric(suppressWarnings(as.numeric(marketable_catch_kg))),
      shoot_lat          = as.numeric(shoot_lat),
      shoot_lon          = as.numeric(shoot_lon),
      haul_lat           = as.numeric(shoot_lat),
      haul_lon           = as.numeric(shoot_lon)
    ) %>%
    select(-any_of(c("shoot_time_hhmm", "shoot_end_time_hhmm", "haul_time_hhmm",
                     "shoot_lat_ddmm", "shoot_ns", "shoot_lon_dddmm", "shoot_ew",
                     "catch_height_cm", "bycatch_pct", "box_type", "photo_box"))) %>%
    select(vessel, trip_id, trip_nr, haul_id, date,
           shoot_lat, shoot_lon, shoot_time, shoot_end_time,
           haul_lat, haul_lon, haul_time, next_haul_time, fishing_time_hours,
           gear_type, mesh_size_mm, vertical_opening_m,
           cable_length_m, cable_thickness_mm, groundrope_length_m, escape_panel,
           water_depth, wind_direction, wind_force_bft,
           total_catch_kg, marketable_catch_kg,
           fao_area, fao_subarea, fao_division, ices_rect, economic_zone,
           skipper, departure_date, departure_port, arrival_date, arrival_port,
           everything()) %>%
    arrange(vessel, trip_id, haul_id) %>%
    write_clean_parquet("haul")
  cat("\n")
  
  # ── KISTEN ───────────────────────────────────────────────────────────────────
  cat("  Converting kisten...\n")
  kisten_env <- new.env()
  load(file.path(rdata_path, "kisten.RData"), envir = kisten_env)
  kisten_raw <- kisten_env$kisten
  
  species_parsed <- parse_marelec_species(kisten_raw$soorten)
  
  kisten_raw %>%
    mutate(
      haul_id         = as.integer(if_else(is.na(haul), haul2, haul)),
      trip_id         = as.character(trip),
      trip_nr         = as.character(trip),
      lot_nr          = as.integer(lotnummer),
      date            = as.Date(if_else(!is.na(datum), as.character(datum),
                                        as.character(as.Date(datetime)))),
      weighing_time   = as.POSIXct(datetime, tz = "UTC"),
      weight_kg       = suppressWarnings(
        as.numeric(trimws(gsub("kg", "", as.character(gewicht), ignore.case = TRUE)))
      ),
      size_class      = suppressWarnings(
        as.integer(trimws(str_remove(as.character(maat),
                                     regex("klasse", ignore_case = TRUE))))
      ),
      box_count       = 1L,
      species_code    = if_else(!is.na(species_parsed$species_code),
                                species_parsed$species_code,
                                toupper(as.character(species))),
      species_name_nl = species_parsed$species_name_nl,
      species_name_en = species_parsed$species_name_en,
      presentation    = species_parsed$presentation
    ) %>%
    select(vessel, trip_id, trip_nr, haul_id, lot_nr,
           weighing_time, date,
           species_code, species_name_nl, species_name_en, presentation,
           size_class, weight_kg, box_count) %>%
    arrange(vessel, trip_id, haul_id, lot_nr) %>%
    write_clean_parquet("kisten")
  cat("\n")
  
  # ── ELOG ─────────────────────────────────────────────────────────────────────
  cat("  Converting elog...\n")
  elog_env <- new.env()
  load(file.path(rdata_path, "elog.RData"), envir = elog_env)
  
  elog_base <- elog_env$elog %>%
    select(-any_of(c("year", "quarter", "month", "week", "yday",
                     "source", "file", "activitydayofyearbst", "remarks",
                     "version", "fishingoperations", "liveweight",
                     "catchdate", "haulid"))) %>%
    mutate(
      shoot_lon = if_else(!is.na(lon2), lon2, lon),
      shoot_lat = if_else(!is.na(lat2), lat2, lat)
    ) %>%
    select(-lon, -lat, -lon2, -lat2) %>%
    rename(
      trip_nr              = trip,        species_code         = species,
      conversion_factor    = conversionfactor, weight_kg       = weight,
      economic_zone        = economiczone,ices_rect            = rect,
      fao_division         = faozone,     gear_type            = geartype,
      mesh_size_mm         = meshsize,    departure_date       = departuredate,
      departure_port       = departureport,arrival_date        = arrivaldate,
      arrival_port         = arrivalport, landing_date         = landingdate,
      landing_port         = landingport, auction_date         = auctiondate,
      auction_port         = auctionport, skipper              = captain,
      vessel_nr            = vesselnumber,trip_nr_elog         = tripidentifier,
      box_count            = boxes,       weight_undersized_kg = weightundersized,
      boxes_undersized     = boxesundersized, trip_status      = tripstatus,
      discard_reason       = discardreason, size_class         = size,
      loss_grams           = lossgrams
    ) %>%
    mutate(
      trip_id              = as.character(trip_nr),
      date                 = as.Date(date),
      departure_date       = as.Date(departure_date),
      arrival_date         = as.Date(arrival_date),
      landing_date         = as.Date(landing_date),
      auction_date         = as.Date(auction_date),
      mesh_size_mm         = as.integer(mesh_size_mm),
      box_count            = as.integer(box_count),
      weight_kg            = as.numeric(weight_kg),
      weight_undersized_kg = as.numeric(weight_undersized_kg),
      boxes_undersized     = as.integer(boxes_undersized),
      species_code         = toupper(trimws(species_code)),
      shoot_lon            = as.numeric(shoot_lon),
      shoot_lat            = as.numeric(shoot_lat)
    ) %>%
    drop_all_na_cols()
  
  pivot_catch_long(elog_base) %>%
    distinct(vessel, trip_id, date, species_code, presentation,
             size_category, .keep_all = TRUE) %>%
    select(vessel, trip_id, trip_nr, date,
           species_code, any_of(c("presentation", "preservation", "freshness")),
           size_category, weight_kg, box_count,
           any_of(c("conversion_factor", "loss_grams", "size_class")),
           any_of(c("shoot_lat", "shoot_lon")),
           any_of(c("fao_division", "ices_rect", "economic_zone")),
           any_of(c("gear_type", "mesh_size_mm")),
           any_of(c("skipper", "vessel_nr", "trip_status", "discard_reason")),
           any_of(c("departure_date", "departure_port",
                    "arrival_date",   "arrival_port",
                    "landing_date",   "landing_port",
                    "auction_date",   "auction_port")),
           everything()) %>%
    arrange(vessel, trip_id, date, species_code, size_category) %>%
    write_clean_parquet("elog")
  cat("\n")
  
  # ── ELOG_TREK ─────────────────────────────────────────────────────────────────
  cat("  Converting elog_trek...\n")
  elog_trek_env <- new.env()
  load(file.path(rdata_path, "elog_trek.RData"), envir = elog_trek_env)
  
  elog_trek_base <- elog_trek_env$elog_trek %>%
    select(-any_of(c("year", "quarter", "month", "week", "yday",
                     "liveweight"))) %>%   # catchdate kept — it's the box weighing timestamp
    rename(weighing_time = catchdate) %>%  # rename to clarify it's Marelec box timestamp
    rename(
      trip_nr              = trip,        haul_id              = haul,
      species_code         = species,     conversion_factor    = conversionfactor,
      weight_kg            = weight,      box_count            = boxes,
      weight_undersized_kg = weightundersized, boxes_undersized = boxesundersized,
      economic_zone        = economiczone,ices_rect            = rect,
      fao_division         = faozone,     shoot_lon            = lon,
      shoot_lat            = lat,         gear_type            = geartype,
      mesh_size_mm         = meshsize,    vessel_nr            = vesselnumber,
      trip_nr_elog         = tripidentifier, departure_date    = departuredate,
      departure_port       = departureport, arrival_date       = arrivaldate,
      arrival_port         = arrivalport, auction_date         = auctiondate,
      auction_port         = auctionport, skipper              = captain,
      trip_status          = tripstatus,  size_class           = size,
      loss_grams           = lossgrams
    ) %>%
    mutate(
      trip_id              = as.character(trip_nr),
      date                 = as.Date(date),
      haul_id              = as.integer(haul_id),
      departure_date       = as.Date(departure_date),
      arrival_date         = as.Date(arrival_date),
      auction_date         = as.Date(auction_date),
      mesh_size_mm         = as.integer(mesh_size_mm),
      box_count            = as.integer(box_count),
      weight_kg            = as.numeric(weight_kg),
      weight_undersized_kg = as.numeric(weight_undersized_kg),
      boxes_undersized     = as.integer(boxes_undersized),
      species_code         = toupper(trimws(species_code)),
      shoot_lon            = as.numeric(shoot_lon),
      shoot_lat            = as.numeric(shoot_lat)
    ) %>%
    drop_all_na_cols()
  
  pivot_catch_long(elog_trek_base) %>%
    # weighing_time (Marelec box timestamp) is part of the unique key —
    # multiple boxes of the same species can be weighed per haul
    distinct(vessel, trip_id, haul_id, species_code, presentation,
             size_category, across(any_of("weighing_time")), .keep_all = TRUE) %>%
    select(vessel, trip_id, trip_nr, haul_id, date,
           any_of("weighing_time"),
           species_code, any_of(c("presentation", "preservation", "freshness")),
           size_category, weight_kg, box_count,
           any_of(c("conversion_factor", "loss_grams", "size_class")),
           any_of(c("shoot_lat", "shoot_lon")),
           any_of(c("fao_division", "ices_rect", "economic_zone")),
           any_of(c("gear_type", "mesh_size_mm")),
           any_of(c("skipper", "vessel_nr", "trip_status")),
           any_of(c("departure_date", "departure_port",
                    "arrival_date",   "arrival_port",
                    "auction_date",   "auction_port")),
           everything()) %>%
    arrange(vessel, trip_id, haul_id, species_code, size_category, 
            across(any_of("weighing_time"))) %>%
    write_clean_parquet("elog_trek")
  cat("\n")
  
  # ── TRIP + VESSEL_MOVEMENT ────────────────────────────────────────────────────
  cat("  Converting trip -> trip summary + vessel_movement...\n")
  trip_env <- new.env()
  load(file.path(rdata_path, "trip.RData"), envir = trip_env)
  
  departure_actions <- c("departure", "vertrek", "sailed", "depart")
  arrival_actions   <- c("arrival", "aankomst", "landed", "land", "landing")
  haul_actions      <- c("haul", "trek", "fishing", "vissen")
  
  trip_prep <- trip_env$trip %>%
    rename(trip_nr = trip) %>%
    mutate(
      trip_id      = as.character(trip_nr),
      date         = as.Date(date),
      lat          = as.numeric(lat),
      lon          = as.numeric(lon),
      haul         = as.integer(haul),
      action_lower = tolower(trimws(action))
    )
  
  # Trip summary
  port_fallback <- trip_prep %>%
    group_by(vessel, trip_id) %>%
    summarise(first_port = first(na.omit(port)),
              last_port  = last(na.omit(port)), .groups = "drop")
  
  trip_prep %>%
    group_by(vessel, trip_id, trip_nr) %>%
    summarise(
      departure_date = { d <- min(date[action_lower %in% departure_actions], na.rm=TRUE)
                         if (is.infinite(d)) min(date, na.rm=TRUE) else d },
      departure_port = first(na.omit(port[action_lower %in% departure_actions])),
      arrival_date   = { d <- max(date[action_lower %in% arrival_actions], na.rm=TRUE)
                         if (is.infinite(d)) max(date, na.rm=TRUE) else d },
      arrival_port   = last(na.omit(port[action_lower %in% arrival_actions])),
      landing_port   = last(na.omit(port[action_lower %in% arrival_actions])),
      n_hauls        = sum(action_lower %in% haul_actions | !is.na(haul), na.rm=TRUE),
      total_distance = sum(distance, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    left_join(port_fallback, by = c("vessel", "trip_id")) %>%
    mutate(
      departure_port = if_else(is.na(departure_port), first_port, departure_port),
      arrival_port   = if_else(is.na(arrival_port),   last_port,  arrival_port),
      landing_port   = if_else(is.na(landing_port),   last_port,  landing_port)
    ) %>%
    select(-first_port, -last_port) %>%
    arrange(vessel, trip_id) %>%
    write_clean_parquet("trip")
  
  # Vessel movement log
  trip_prep %>%
    mutate(
      event_type = case_when(
        action_lower %in% departure_actions            ~ "departure",
        action_lower %in% arrival_actions              ~ "arrival",
        action_lower %in% haul_actions | !is.na(haul) ~ "haul",
        TRUE                                            ~ "other"
      )
    ) %>%
    filter(event_type != "other") %>%
    select(vessel, trip_id, trip_nr, date, event_type,
           haul_id = haul, port, lat, lon, distance, source) %>%
    mutate(haul_id = as.integer(haul_id)) %>%
    arrange(vessel, trip_id, date) %>%
    write_clean_parquet("vessel_movement")
  
  cat("\n✓ Conversion complete\n")
  cat("  Note: elog/elog_trek have size_category = 'legal' or 'undersized'\n")
  cat("  Filter size_category == 'legal' for official landings weight\n\n")
}

# Prompt for conversion ----
cat(strrep("=", 70), "\n")
cat("RDATA -> PARQUET CONVERSION\n")
cat(strrep("=", 70), "\n\n")
cat("Convert existing RData files to Parquet with full Poseidat schema?\n")
cat("This converts: haul, kisten, elog, elog_trek, trip -> trip + vessel_movement\n")
cat("Location:", config$onedrive_rdata, "\n\n")
cat("This should only be run ONCE when setting up the system.\n")
cat("Existing parquet files in the raw/ folders will be overwritten.\n\n")

response <- readline(prompt = "Convert now? (y/n): ")

if (tolower(response) == "y") {
  convert_rdata_to_parquet()
} else {
  cat("Skipping conversion. Run convert_rdata_to_parquet() manually when ready.\n\n")
}

# Print setup summary
cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
cat("FLYSHOOT DATA MANAGEMENT SYSTEM - SETUP COMPLETE\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
cat("Next steps:\n")
cat("1. Review and update paths in config.json if needed\n")
cat("2. Source 01_flyshoot_functions.R\n")
cat("3. Source 02_storage_functions.R\n")
cat("4. Source 03_main_workflow.R\n")
cat("5. Process your trip data!\n\n")
cat("Key features:\n")
cat("  ✓ Parquet format for efficient storage\n")
cat("  ✓ Unified processing for all data sources\n")
cat("  ✓ Automatic data type detection\n")
cat("  ✓ Consistent naming with Poseidat system\n\n")
cat("=" |> rep(70) |> paste0(collapse = ""), "\n")

