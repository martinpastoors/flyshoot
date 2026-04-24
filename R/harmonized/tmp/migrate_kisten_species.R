# ==============================================================================
# ONE-TIME MIGRATION: Reprocess species fields in kisten_existing
# ==============================================================================
# Applies parse_marelec_species() to the old species_code column, replacing
# it with the new structured columns:
#   species_code    - FAO 3-letter code
#   species_name_nl - Dutch name (uppercased)
#   species_name_en - English name
#   presentation    - processing/presentation code (DICHT, GUT, etc.) or NA
#
# Also adds:
#   datetime        - combined date + time if a time column is present
#   size_class      - integer (from "KLASSE N" if present)
#
# Run ONCE after updating 01_flyshoot_functions.R.
# The original parquet files are backed up before overwriting.
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(arrow)
library(glue)
library(fs)
library(lubridate)
library(jsonlite)

# Load config and functions
config <- read_json("config.json")
source(file.path(here::here(), "R/harmonized3/01_flyshoot_functions.R"))
source(file.path(here::here(), "R/harmonized3/02_storage_functions.R"))

message("\n", strrep("=", 70))
message("KISTEN SPECIES MIGRATION")
message(strrep("=", 70), "\n")

# ------------------------------------------------------------------------------
# 1. Load existing kisten data
# ------------------------------------------------------------------------------
kisten_existing <- tryCatch(
  load_flyshoot_data("kisten"),
  error = function(e) {
    stop("Could not load kisten data: ", e$message)
  }
)

message(glue("Loaded {nrow(kisten_existing)} existing kisten records"))
message(glue("Columns: {paste(names(kisten_existing), collapse = ', ')}\n"))

if (nrow(kisten_existing) == 0) {
  stop("No kisten data found to migrate.")
}

# ------------------------------------------------------------------------------
# 2. Preview what the old species_code column looks like
# ------------------------------------------------------------------------------
old_codes <- sort(unique(kisten_existing$soorten))
message(glue("Found {length(old_codes)} unique species_code values in existing data:"))
print(old_codes)
cat("\n")

# ------------------------------------------------------------------------------
# 3. Back up existing parquet files
# ------------------------------------------------------------------------------
kisten_path   <- glue("{config$raw_data_path}/kisten")
backup_path   <- glue("{config$raw_data_path}/kisten_backup_{format(Sys.Date(), '%Y%m%d')}")

message(glue("Backing up existing parquet files to: {backup_path}"))
dir_copy(kisten_path, backup_path)
message("✓ Backup complete\n")

# ------------------------------------------------------------------------------
# 4. Apply species parser to old species_code column
# ------------------------------------------------------------------------------

# The old column may be named 'species_code' (already somewhat processed)
# or it may still contain the raw Marelec strings. Either way,
# parse_marelec_species() handles all formats.
source_col <- if ("species_code" %in% names(kisten_existing)) {
  "species_code"
} else if ("species_raw" %in% names(kisten_existing)) {
  "species_raw"
} else if ("soorten" %in% names(kisten_existing)) {
  "soorten"
} else {
  stop("Cannot find a species column in kisten_existing")
}

message(glue("Parsing species from column: '{source_col}'"))

species_parsed <- parse_marelec_species(kisten_existing[[source_col]])

# Show mapping for review before committing
mapping_review <- kisten_existing %>%
  select(all_of(source_col)) %>%
  bind_cols(species_parsed) %>%
  distinct() %>%
  arrange(species_name_nl)

message(glue("\nSpecies mapping preview ({nrow(mapping_review)} unique values):"))
print(mapping_review, n = Inf)
cat("\n")

# Pause for user to review - comment out if running non-interactively
# readline("Press Enter to continue with migration, or Ctrl+C to abort...")

# ------------------------------------------------------------------------------
# 5. Build migrated dataset
# ------------------------------------------------------------------------------
kisten_migrated <- kisten_existing %>%
  
  # Drop old parsed/partial columns if they exist, replace with new ones
  select(-any_of(c("species_code", "species_name_nl", "species_name_en",
                   "presentation", "species_raw"))) %>%
  
  bind_cols(species_parsed) %>%
  
  # Fix size_class if it's still a string ("KLASSE 1") rather than integer
  mutate(
    size_class = if ("size_class" %in% names(.)) {
      sc <- size_class
      if (is.character(sc)) {
        suppressWarnings(
          as.integer(trimws(str_remove(sc, regex("klasse", ignore_case = TRUE))))
        )
      } else {
        as.integer(sc)
      }
    } else {
      NA_integer_
    }
  ) %>%
  
  # Build datetime if date + time columns are both present but datetime is absent
  {
    d <- .
    if (!"datetime" %in% names(d) &&
        "date" %in% names(d) &&
        "time_hhmm" %in% names(d)) {
      message("  Building datetime from date + time_hhmm")
      d <- d %>%
        mutate(
          datetime = as.POSIXct(
            paste(date, time_hhmm),
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          )
        )
    }
    d
  } %>%
  
  # Canonical column order
  select(
    any_of(c("vessel", "trip_id", "haul_id", "datetime", "date",
             "species_code", "species_name_nl", "species_name_en", "presentation",
             "size_class", "weight_kg", "box_count", "lot_nr")),
    everything()
  )

# ------------------------------------------------------------------------------
# 6. Summarise changes
# ------------------------------------------------------------------------------
message("\nMigration summary:")
message(glue("  Rows:    {nrow(kisten_existing)} -> {nrow(kisten_migrated)}"))
message(glue("  Columns: {ncol(kisten_existing)} -> {ncol(kisten_migrated)}"))

new_codes <- sort(unique(kisten_migrated$species_code))
message(glue("\n  {length(new_codes)} unique FAO species codes after migration:"))
print(new_codes)

if ("presentation" %in% names(kisten_migrated)) {
  pres <- sort(unique(na.omit(kisten_migrated$presentation)))
  message(glue("\n  Presentation codes found: {paste(pres, collapse = ', ')}"))
}

# Flag any fallback codes (uppercase first word, not a known 3-letter FAO code)
# These warrant manual review
unknown <- kisten_migrated %>%
  filter(!species_code %in% unique(MARELEC_SPECIES_LOOKUP$fao_code)) %>%
  distinct(species_code, species_name_nl) %>%
  arrange(species_name_nl)

if (nrow(unknown) > 0) {
  message(glue("\n  ⚠ {nrow(unknown)} species_code(s) not in lookup table (check these):"))
  print(unknown)
} else {
  message("\n  ✓ All species resolved to known FAO codes")
}

# ------------------------------------------------------------------------------
# 7. Save migrated data (overwrites existing parquet files)
# ------------------------------------------------------------------------------
message(glue("\nSaving migrated data to: {kisten_path}"))

# Remove old parquet files
old_files <- list.files(kisten_path, pattern = "\\.parquet$", full.names = TRUE)
file.remove(old_files)
message(glue("  Removed {length(old_files)} old parquet file(s)"))

save_flyshoot_data(
  kisten_migrated,
  "kisten",
  date_range = c(min(kisten_migrated$date, na.rm = TRUE),
                 max(kisten_migrated$date, na.rm = TRUE))
)

message(glue("\n✓ Migration complete. Backup retained at: {backup_path}"))
message("  Delete the backup manually once you are satisfied with the results.\n")
