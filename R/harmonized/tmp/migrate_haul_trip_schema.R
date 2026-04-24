# ==============================================================================
# RECOVERY + SCHEMA MIGRATION: haul and trip parquet files
# ==============================================================================
# The consolidate script over-deduplicated haul data because haul_id was NA
# in the old format (column was named 'haul', not 'haul_id').
#
# This script:
#   1. Restores haul and trip from their backup folders
#   2. Applies column renames to align old names with current Poseidat standard
#   3. Deduplicates using the correct (old) key columns
#   4. Writes clean fixed-name parquet files
#
# Run this BEFORE running the main workflow with the new storage functions.
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(arrow)
library(glue)
library(fs)
library(jsonlite)
library(lubridate)

config <- read_json("config.json")

message("\n", strrep("=", 70))
message("HAUL + TRIP SCHEMA MIGRATION AND RECOVERY")
message(strrep("=", 70), "\n")

# ==============================================================================
# HELPER: find the most recent backup folder for a given data type
# ==============================================================================
find_backup <- function(dtype) {
  parent <- config$raw_data_path
  pattern <- glue("^{dtype}_backup_")
  candidates <- list.dirs(parent, full.names = TRUE, recursive = FALSE)
  candidates <- candidates[grepl(pattern, basename(candidates))]
  if (length(candidates) == 0) return(NULL)
  # Most recent by name (date suffix YYYYMMDD sorts correctly)
  sort(candidates, decreasing = TRUE)[1]
}

# ==============================================================================
# HAUL migration
# ==============================================================================
message("── HAUL ─────────────────────────────────────────────────────────────\n")

haul_path   <- glue("{config$raw_data_path}/haul")
haul_backup <- find_backup("haul")

if (is.null(haul_backup)) {
  stop("No haul backup folder found. Cannot recover.")
}

message(glue("  Restoring from backup: {basename(haul_backup)}"))
backup_files <- list.files(haul_backup, pattern = "\\.parquet$", full.names = TRUE)
message(glue("  Found {length(backup_files)} backup file(s)"))

haul_raw <- open_dataset(haul_backup) %>% collect()
message(glue("  Loaded {nrow(haul_raw)} rows from backup"))
message(glue("  Columns: {paste(names(haul_raw), collapse=', ')}"))

# Column rename mapping: old name -> new Poseidat name
haul_rename <- c(
  "trip"              = "trip_nr",
  "haul"              = "haul_id",
  "shoottime"         = "shoot_time",
  "haultime"          = "haul_time",
  "shoottime2"        = "shoot_end_time",
  "nexthaultime"      = "next_haul_time",
  "lon"               = "shoot_lon",
  "lat"               = "shoot_lat",
  "winddirection"     = "wind_direction",
  "windforce"         = "wind_force_bft",
  "waterdepth"        = "water_depth",
  "catchheight"       = "catch_height_cm",
  "boxtype"           = "box_type",
  "landingweight"     = "marketable_catch_kg",
  "catchweight"       = "total_catch_kg",
  "bycatchperc"       = "bycatch_pct",
  "gear"              = "gear_type",
  "meshsize"          = "mesh_size_mm",
  "vertopening"       = "vertical_opening_m",
  "cablelength"       = "cable_length_m",
  "cablethickness"    = "cable_thickness_mm",
  "lengthgroundrope"  = "groundrope_length_m",
  "escapepanel"       = "escape_panel",
  "duration"          = "fishing_time_hours",
  "area"              = "fao_area",
  "subarea"           = "fao_subarea",
  "division"          = "fao_division",
  "rect"              = "ices_rect",
  "economiczone"      = "economic_zone",
  "captain"           = "skipper",
  "departuredate"     = "departure_date",
  "departureport"     = "departure_port",
  "arrivaldate"       = "arrival_date",
  "arrivalport"       = "arrival_port",
  # Calculated spatial (if present from old pipeline)
  "rect_calc"         = "ices_rect_calc",
  "division_calc"     = "fao_division_calc",
  "economiczone_calc" = "economic_zone_calc"
)

haul_migrated <- haul_raw
for (old in names(haul_rename)) {
  new <- haul_rename[[old]]
  if (old %in% names(haul_migrated) && !new %in% names(haul_migrated)) {
    haul_migrated <- rename(haul_migrated, !!new := !!old)
    message(glue("  Renamed: {old} -> {new}"))
  } else if (old %in% names(haul_migrated) && new %in% names(haul_migrated)) {
    haul_migrated <- select(haul_migrated, -any_of(old))
    message(glue("  Dropped duplicate: {old} (kept {new})"))
  }
}

# Drop columns with no useful content (e.g. column named "NA")
haul_migrated <- haul_migrated %>%
  select(-any_of(c("NA", "source", "file", "year", "quarter", "month",
                   "week", "yday", "migration_date", "migration_timestamp",
                   "source_format", "save_date", "save_timestamp")))

# Coerce key date/time columns
if ("shoot_time" %in% names(haul_migrated))
  haul_migrated$shoot_time <- as.POSIXct(haul_migrated$shoot_time, tz = "UTC")
if ("haul_time" %in% names(haul_migrated))
  haul_migrated$haul_time  <- as.POSIXct(haul_migrated$haul_time,  tz = "UTC")
if ("departure_date" %in% names(haul_migrated))
  haul_migrated$departure_date <- as.Date(haul_migrated$departure_date)
if ("arrival_date" %in% names(haul_migrated))
  haul_migrated$arrival_date   <- as.Date(haul_migrated$arrival_date)
if ("date" %in% names(haul_migrated))
  haul_migrated$date <- as.Date(haul_migrated$date)

# Ensure haul_id is integer (it existed in old data as haul, now renamed)
if ("haul_id" %in% names(haul_migrated))
  haul_migrated$haul_id <- as.integer(haul_migrated$haul_id)

# Deduplicate on correct keys — now haul_id is populated
haul_keys_available <- intersect(c("vessel", "trip_id", "haul_id"),
                                 names(haul_migrated))
# Also try old-format trip column if trip_id still missing
if (!"trip_id" %in% haul_keys_available && "trip_nr" %in% names(haul_migrated)) {
  # trip_nr is the renamed 'trip' — use it as the dedup key
  haul_keys_available <- intersect(c("vessel", "trip_nr", "haul_id"),
                                   names(haul_migrated))
}

message(glue("\n  Deduplicating on: {paste(haul_keys_available, collapse=' + ')}"))

haul_clean <- haul_migrated %>%
  filter(!is.na(haul_id)) %>%   # drop rows where haul_id is genuinely NA
  slice(n():1) %>%
  distinct(across(all_of(haul_keys_available)), .keep_all = TRUE) %>%
  arrange(vessel, across(any_of(c("trip_id", "trip_nr"))), haul_id)

n_na_haul <- nrow(haul_migrated) - nrow(filter(haul_migrated, !is.na(haul_id)))
message(glue("  Dropped {n_na_haul} rows with NA haul_id"))
message(glue("  After dedup: {nrow(haul_clean)} rows"))

# Write
target <- file.path(haul_path, "haul.parquet")
old_files <- list.files(haul_path, pattern = "\\.parquet$", full.names = TRUE)
if (length(old_files) > 0) file.remove(old_files)
write_parquet(haul_clean %>%
                mutate(save_date = Sys.Date(), save_timestamp = Sys.time()),
              target)
message(glue("  ✓ Written: haul.parquet ({nrow(haul_clean)} rows)"))

# ==============================================================================
# TRIP migration
# ==============================================================================
message("\n── TRIP ─────────────────────────────────────────────────────────────\n")

trip_path   <- glue("{config$raw_data_path}/trip")
trip_backup <- find_backup("trip")

if (is.null(trip_backup)) {
  message("  ⚠ No trip backup found — skipping (trip may already be correct)")
} else {
  message(glue("  Restoring from backup: {basename(trip_backup)}"))
  trip_raw <- open_dataset(trip_backup) %>% collect()
  message(glue("  Loaded {nrow(trip_raw)} rows from backup"))

  trip_rename <- c(
    "trip"          = "trip_nr",
    "captain"       = "skipper",
    "departuredate" = "departure_date",
    "departureport" = "departure_port",
    "arrivaldate"   = "arrival_date",
    "arrivalport"   = "arrival_port",
    "landingdate"   = "auction_date",
    "landingport"   = "auction_port"
  )

  trip_migrated <- trip_raw
  for (old in names(trip_rename)) {
    new <- trip_rename[[old]]
    if (old %in% names(trip_migrated) && !new %in% names(trip_migrated)) {
      trip_migrated <- rename(trip_migrated, !!new := !!old)
      message(glue("  Renamed: {old} -> {new}"))
    } else if (old %in% names(trip_migrated) && new %in% names(trip_migrated)) {
      trip_migrated <- select(trip_migrated, -any_of(old))
      message(glue("  Dropped duplicate: {old} (kept {new})"))
    }
  }

  trip_migrated <- trip_migrated %>%
    select(-any_of(c("source", "file", "year", "quarter", "month", "week",
                     "yday", "migration_date", "migration_timestamp",
                     "source_format", "save_date", "save_timestamp")))

  if ("departure_date" %in% names(trip_migrated))
    trip_migrated$departure_date <- as.Date(trip_migrated$departure_date)
  if ("arrival_date" %in% names(trip_migrated))
    trip_migrated$arrival_date <- as.Date(trip_migrated$arrival_date)

  trip_keys <- intersect(c("vessel", "trip_id"), names(trip_migrated))
  if (length(trip_keys) < 2 && "trip_nr" %in% names(trip_migrated))
    trip_keys <- intersect(c("vessel", "trip_nr"), names(trip_migrated))

  trip_clean <- trip_migrated %>%
    slice(n():1) %>%
    distinct(across(all_of(trip_keys)), .keep_all = TRUE) %>%
    arrange(vessel, across(any_of(c("trip_id", "trip_nr"))))

  message(glue("  After dedup: {nrow(trip_clean)} rows"))

  target <- file.path(trip_path, "trip.parquet")
  old_files <- list.files(trip_path, pattern = "\\.parquet$", full.names = TRUE)
  if (length(old_files) > 0) file.remove(old_files)
  write_parquet(trip_clean %>%
                  mutate(save_date = Sys.Date(), save_timestamp = Sys.time()),
                target)
  message(glue("  ✓ Written: trip.parquet ({nrow(trip_clean)} rows)"))
}

message(glue("\n", strrep("=", 70)))
message("DONE — verify with load_flyshoot_data('haul') and load_flyshoot_data('trip')")
message(strrep("=", 70), "\n")
