# ==============================================================================
# ONE-TIME MIGRATION: Rename elog_existing columns to Poseidat standard names
# ==============================================================================
# The existing elog parquet was saved in an old format with non-standard column
# names. This script renames them to match the current elog_schema so that
# new data can be combined without column explosion.
#
# Run ONCE before reprocessing elog data with the current workflow.
# The original parquet files are backed up before overwriting.
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(arrow)
library(glue)
library(fs)
library(lubridate)
library(jsonlite)

config    <- read_json("config.json")
source(file.path(here::here(), "R/harmonized3/01_flyshoot_functions.R"))
source(file.path(here::here(), "R/harmonized3/02_storage_functions.R"))

message("\n", strrep("=", 70))
message("ELOG SCHEMA MIGRATION")
message(strrep("=", 70), "\n")

# ------------------------------------------------------------------------------
# 1. Load existing elog data
# ------------------------------------------------------------------------------
elog_existing <- tryCatch(
  load_flyshoot_data("elog"),
  error = function(e) stop("Could not load elog data: ", e$message)
)

message(glue("Loaded {nrow(elog_existing)} existing elog records"))
message(glue("Columns: {paste(names(elog_existing), collapse = ', ')}\n"))

# ------------------------------------------------------------------------------
# 2. Define old -> new column name mapping
#    Left  = old name (as stored in existing parquet)
#    Right = new Poseidat name (as produced by current elog_schema)
# ------------------------------------------------------------------------------
col_rename <- c(
  # Core identifiers
  "catchdate"           = "date",
  "trip"                = "trip_nr",
  "tripidentifier"      = "trip_nr",       # keep whichever exists
  "haulid"              = "haul_id",
  
  # Species / catch
  "species"             = "species_code",
  "weight"              = "weight_kg",
  "boxes"               = "box_count",
  "weightundersized"    = "weight_undersized_kg",
  "boxesundersized"     = "boxes_undersized",
  "conversionfactor"    = "conversion_factor",
  "lossgrams"           = "loss_grams",
  "size"                = "size_class",
  "discardreason"       = "discard_reason",
  
  # Spatial
  "rect"                = "ices_rect",
  "faozone"             = "fao_zone",
  "economiczone"        = "economic_zone",
  "lon"                 = "shoot_lon",
  "lat"                 = "shoot_lat",
  "lon2"                = "shoot_lon",      # fallback if lon absent
  "lat2"                = "shoot_lat",      # fallback if lat absent
  
  # Gear
  "geartype"            = "gear_type",
  "meshsize"            = "mesh_size_mm",
  
  # Trip admin
  "departuredate"       = "departure_date",
  "departureport"       = "departure_port",
  "arrivaldate"         = "arrival_date",
  "arrivalport"         = "arrival_port",
  "landingdate"         = "auction_date",
  "landingport"         = "auction_port",
  "auctiondate"         = "auction_date",
  "auctionport"         = "auction_port",
  "tripstatus"          = "trip_status",
  "captain"             = "skipper",
  "vesselnumber"        = "vessel_nr"
)

# ------------------------------------------------------------------------------
# 3. Apply renaming — only rename columns that actually exist and whose
#    target name doesn't already exist (avoid clobbering)
# ------------------------------------------------------------------------------
elog_migrated <- elog_existing

for (old_name in names(col_rename)) {
  new_name <- col_rename[[old_name]]
  if (old_name %in% names(elog_migrated) && !new_name %in% names(elog_migrated)) {
    elog_migrated <- dplyr::rename(elog_migrated, !!new_name := !!old_name)
    message(glue("  Renamed: {old_name} -> {new_name}"))
  } else if (old_name %in% names(elog_migrated) && new_name %in% names(elog_migrated)) {
    # Both exist — drop the old one, keep the new one
    elog_migrated <- dplyr::select(elog_migrated, -dplyr::all_of(old_name))
    message(glue("  Dropped duplicate: {old_name} (kept {new_name})"))
  }
}

# Ensure date column is Date type (old format may have stored as dttm)
if ("date" %in% names(elog_migrated) && inherits(elog_migrated$date, "POSIXct")) {
  elog_migrated <- elog_migrated %>%
    mutate(date = as.Date(date))
  message("  Coerced date from POSIXct to Date")
}

# ------------------------------------------------------------------------------
# 4. Show before/after column comparison
# ------------------------------------------------------------------------------
old_cols  <- names(elog_existing)
new_cols  <- names(elog_migrated)
added     <- setdiff(new_cols, old_cols)
removed   <- setdiff(old_cols, new_cols)
unchanged <- intersect(old_cols, new_cols)

message(glue("\nColumn changes:"))
message(glue("  Renamed/added: {paste(added,   collapse = ', ')}"))
message(glue("  Removed:       {paste(removed,  collapse = ', ')}"))
message(glue("  Unchanged:     {length(unchanged)} columns"))

# Check that the key Poseidat columns are now present
required <- c("vessel", "trip_id", "date", "species_code", 
              "weight_kg", "gear_type")
missing  <- setdiff(required, names(elog_migrated))
if (length(missing) > 0) {
  warning(glue("Still missing required columns after migration: ",
               "{paste(missing, collapse = ', ')}"))
} else {
  message("\n✓ All required Poseidat columns present")
}

# ------------------------------------------------------------------------------
# 5. Back up existing parquet files, then save migrated data
# ------------------------------------------------------------------------------
elog_path   <- glue("{config$raw_data_path}/elog")
backup_path <- glue("{config$raw_data_path}/elog_backup_{format(Sys.Date(), '%Y%m%d')}")

message(glue("\nBacking up to: {backup_path}"))
dir_copy(elog_path, backup_path)
message("✓ Backup complete")

old_files <- list.files(elog_path, pattern = "\\.parquet$", full.names = TRUE)
file.remove(old_files)
message(glue("  Removed {length(old_files)} old parquet file(s)"))

save_flyshoot_data(
  elog_migrated,
  "elog",
  date_range = c(min(elog_migrated$date, na.rm = TRUE),
                 max(elog_migrated$date, na.rm = TRUE))
)

message(glue("\n✓ Migration complete — {nrow(elog_migrated)} records saved"))
message(glue("  Backup retained at: {backup_path}"))
message("  Delete the backup manually once satisfied with the results.\n")
