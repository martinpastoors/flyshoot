# ==============================================================================
# ONE-TIME MIGRATION: Consolidate parquet files to new fixed-name structure
# ==============================================================================
# The old save_flyshoot_data() created a new date-suffixed file on every run
# (e.g. haul_2017-08-20_2026-03-12.parquet), causing:
#   1. Multiple files accumulating in each folder
#   2. open_dataset() reading all of them, producing duplicate records
#
# This script:
#   1. Reads all existing parquet files per data type (via open_dataset)
#   2. Deduplicates using the natural key for each type
#   3. Backs up the old files
#   4. Writes a single clean fixed-name file (e.g. haul.parquet)
#
# Run ONCE before switching to the new 02_storage_functions.R.
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(arrow)
library(glue)
library(fs)
library(jsonlite)

config <- read_json("config.json")

message("\n", strrep("=", 70))
message("PARQUET CONSOLIDATION MIGRATION")
message(strrep("=", 70), "\n")

# ------------------------------------------------------------------------------
# Deduplication keys per data type
# ------------------------------------------------------------------------------
dedup_keys <- list(
  haul      = c("vessel", "trip_id", "haul_id"),
  trip      = c("vessel", "trip_id"),
  kisten    = c("vessel", "trip_id", "haul_id", "lot_nr"),
  elog      = c("vessel", "trip_id", "date", "species_code", "presentation"),
  elog_trek = c("vessel", "trip_id", "haul_id", "species_code", "presentation")
)

# ------------------------------------------------------------------------------
# Sort order per data type (for deterministic output)
# ------------------------------------------------------------------------------
sort_cols <- list(
  haul      = c("vessel", "trip_id", "haul_id"),
  trip      = c("vessel", "trip_id"),
  kisten    = c("vessel", "trip_id", "haul_id", "lot_nr"),
  elog      = c("vessel", "trip_id", "date", "species_code"),
  elog_trek = c("vessel", "trip_id", "haul_id", "species_code")
)

# ------------------------------------------------------------------------------
# Process each data type
# ------------------------------------------------------------------------------
for (dtype in names(dedup_keys)) {
  
  data_path <- glue("{config$raw_data_path}/{dtype}")
  
  if (!dir.exists(data_path)) {
    message(glue("  {dtype}: directory not found, skipping"))
    next
  }
  
  parquet_files <- list.files(data_path, pattern = "\\.parquet$",
                              full.names = TRUE, recursive = FALSE)
  
  if (length(parquet_files) == 0) {
    message(glue("  {dtype}: no parquet files found, skipping"))
    next
  }
  
  target_file <- file.path(data_path, glue("{dtype}.parquet"))
  
  message(glue("\n{dtype}/"))
  message(glue("  Found {length(parquet_files)} parquet file(s):"))
  for (f in parquet_files) message(glue("    {basename(f)}"))
  
  # ------------------------------------------------------------------
  # 1. Read all files
  # ------------------------------------------------------------------
  data_raw <- tryCatch(
    open_dataset(data_path) %>% collect(),
    error = function(e) {
      message(glue("  ✗ Could not read {dtype}: {e$message}"))
      NULL
    }
  )
  
  if (is.null(data_raw) || nrow(data_raw) == 0) {
    message(glue("  ⚠ No data loaded for {dtype}, skipping"))
    next
  }
  
  message(glue("  Loaded {nrow(data_raw)} total rows (before dedup)"))
  
  # ------------------------------------------------------------------
  # 2. Deduplicate — keep last occurrence (most recently saved = most current)
  #    Reverse row order first so that later saves win over earlier ones
  # ------------------------------------------------------------------
  keys <- dedup_keys[[dtype]]
  
  # Only deduplicate on keys that actually exist in the data
  available_keys <- keys[keys %in% names(data_raw)]
  
  if (length(available_keys) < length(keys)) {
    missing_keys <- setdiff(keys, names(data_raw))
    message(glue("  ⚠ Missing key columns: {paste(missing_keys, collapse=', ')} — deduplicating on available keys only"))
  }
  
  if (length(available_keys) > 0) {
    # Reverse so slice(1) keeps the last-saved version
    data_clean <- data_raw %>%
      slice(n():1) %>%
      distinct(across(all_of(available_keys)), .keep_all = TRUE) %>%
      arrange(across(all_of(sort_cols[[dtype]][sort_cols[[dtype]] %in% names(.)])))
  } else {
    message(glue("  ⚠ No key columns available — skipping deduplication"))
    data_clean <- data_raw
  }
  
  n_removed <- nrow(data_raw) - nrow(data_clean)
  message(glue("  Deduplicated: {nrow(data_raw)} -> {nrow(data_clean)} rows ",
               "({n_removed} duplicates removed)"))
  
  # ------------------------------------------------------------------
  # 3. Back up old files (move to dated backup folder)
  # ------------------------------------------------------------------
  backup_dir <- glue("{config$raw_data_path}/{dtype}_backup_{format(Sys.Date(), '%Y%m%d')}")
  
  # Only back up files that are NOT already the target fixed-name file
  files_to_backup <- parquet_files[parquet_files != target_file]
  
  if (length(files_to_backup) > 0) {
    dir_create(backup_dir)
    for (f in files_to_backup) {
      file_copy(f, file.path(backup_dir, basename(f)), overwrite = TRUE)
      file_delete(f)
    }
    message(glue("  Backed up {length(files_to_backup)} old file(s) to {basename(backup_dir)}/"))
  }
  
  # ------------------------------------------------------------------
  # 4. Write clean fixed-name file
  # ------------------------------------------------------------------
  data_with_meta <- data_clean %>%
    mutate(
      save_date      = Sys.Date(),
      save_timestamp = Sys.time(),
      .after         = last_col()
    )
  
  # Remove old save_date/save_timestamp columns if they already exist
  # (the mutate above adds them at the end; if they existed in the middle
  # the data would have duplicates)
  if (sum(names(data_with_meta) == "save_date") > 1) {
    data_with_meta <- data_with_meta %>%
      select(-any_of(c("save_date", "save_timestamp"))) %>%
      mutate(save_date = Sys.Date(), save_timestamp = Sys.time())
  }
  
  write_parquet(data_with_meta, target_file)
  message(glue("  ✓ Written: {basename(target_file)} ({nrow(data_clean)} rows)"))
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
message(glue("\n", strrep("=", 70)))
message("MIGRATION COMPLETE")
message(strrep("=", 70))
message("\nNext steps:")
message("  1. Verify the new .parquet files look correct")
message("  2. Delete backup folders manually once satisfied")
message("  3. Switch to the new 02_storage_functions.R")
message("  4. Run the main workflow — it will now maintain single fixed-name files\n")
