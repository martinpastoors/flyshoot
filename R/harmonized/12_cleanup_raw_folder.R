# =============================================================================
# 12_cleanup_raw_folder.R
# One-time cleanup of the raw parquet folder.
#
# What it does:
#   1. Moves active parquets into their correct subfolders if not already there
#   2. Moves any existing date-stamped backups into backup/ subfolders
#   3. Deletes *_legacy_migrated.parquet files (already in active parquets)
#   4. Prints a before/after summary
#
# Run ONCE after updating 02_storage_functions.R.
# Safe to re-run — uses file.exists() checks throughout.
# =============================================================================

library(fs)
library(dplyr)
library(glue)
library(lubridate)

RAW_DIR <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw"

ACTIVE_TABLES <- c("haul", "elog", "elog_trek", "kisten", "trip", "vessel_movement", "harbours")

cleanup_raw_folder <- function(raw_dir = RAW_DIR, dry_run = FALSE) {

  message(strrep("=", 60))
  message("RAW FOLDER CLEANUP")
  message(strrep("=", 60))
  message(glue("  Folder : {raw_dir}"))
  if (dry_run) message("  *** DRY RUN — no files will be moved or deleted ***")

  # ── Inventory: all parquet files currently in raw/ (flat + subfolders) ─────
  all_files <- list.files(raw_dir, pattern = "\\.parquet$",
                          full.names = TRUE, recursive = TRUE)

  message(glue("\n  Found {length(all_files)} parquet files total"))

  moved   <- 0
  deleted <- 0
  backed  <- 0

  for (f in all_files) {
    fname    <- basename(f)
    fdir     <- dirname(f)
    rel_dir  <- fs::path_rel(fdir, raw_dir)   # e.g. "." or "elog" or "elog/backup"

    # ── 1. Delete legacy migrated files ──────────────────────────────────────
    if (grepl("_legacy_migrated", fname)) {
      message(glue("  DELETE (legacy): {fs::path_rel(f, raw_dir)}"))
      if (!dry_run) file.remove(f)
      deleted <- deleted + 1
      next
    }

    # ── 2. Identify which active table this file belongs to ───────────────────
    matched_table <- NA_character_
    for (tbl in ACTIVE_TABLES[order(-nchar(ACTIVE_TABLES))]) {  # longest first to avoid partial matches
      # Active file: exactly "<table>.parquet"
      if (fname == paste0(tbl, ".parquet")) {
        matched_table <- tbl
        break
      }
      # Date-stamped backup: "<table>_YYYY..." or "<table>_2026..."
      if (grepl(paste0("^", tbl, "[_-]"), fname)) {
        matched_table <- tbl
        break
      }
    }

    if (is.na(matched_table)) {
      message(glue("  SKIP (unrecognised): {fs::path_rel(f, raw_dir)}"))
      next
    }

    target_dir    <- file.path(raw_dir, matched_table)
    backup_dir    <- file.path(target_dir, "backup")
    is_active     <- (fname == paste0(matched_table, ".parquet"))
    already_right <- (normalizePath(fdir) == normalizePath(target_dir))
    in_backup     <- grepl("backup", rel_dir)

    # ── 3. Active parquet not yet in its subfolder ────────────────────────────
    if (is_active && !already_right && !in_backup) {
      dest <- file.path(target_dir, fname)
      message(glue("  MOVE (active): {fname} -> {matched_table}/{fname}"))
      if (!dry_run) {
        dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
        file.copy(f, dest, overwrite = TRUE)
        file.remove(f)
      }
      moved <- moved + 1
      next
    }

    # ── 4. Date-stamped backup not yet in backup/ subfolder ───────────────────
    if (!is_active && !in_backup) {
      # Extract timestamp from filename for the new backup name
      ts_match <- regmatches(fname, regexpr("[0-9]{4}[-_][0-9]{2}", fname))
      ts       <- if (length(ts_match) > 0)
                    gsub("[-_]", "", ts_match)
                  else
                    format(file.mtime(f), "%Y%m%d_%H%M%S")
      new_name <- glue("{matched_table}_{ts}.parquet")
      dest     <- file.path(backup_dir, new_name)
      message(glue("  MOVE (backup): {fname} -> {matched_table}/backup/{new_name}"))
      if (!dry_run) {
        dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
        file.copy(f, dest, overwrite = FALSE)
        file.remove(f)
      }
      backed <- backed + 1
      next
    }

    message(glue("  OK: {fs::path_rel(f, raw_dir)}"))
  }

  # ── Summary ────────────────────────────────────────────────────────────────
  message(glue("\n{strrep('=', 60)}"))
  message("CLEANUP SUMMARY")
  message(strrep("=", 60))
  message(glue("  Moved to subfolders : {moved}"))
  message(glue("  Moved to backup/    : {backed}"))
  message(glue("  Deleted (legacy)    : {deleted}"))

  # ── Final structure ────────────────────────────────────────────────────────
  message("\nResulting structure:")
  remaining <- list.files(raw_dir, pattern = "\\.parquet$",
                          full.names = FALSE, recursive = TRUE)
  for (f in sort(remaining)) message(glue("  {f}"))

  message(strrep("=", 60))
  invisible(NULL)
}

# =============================================================================
# USAGE
# =============================================================================
# source("12_cleanup_raw_folder.R")
#
# # Dry run first — see what would happen
# cleanup_raw_folder(dry_run = TRUE)
#
# # Run for real
# cleanup_raw_folder()
# =============================================================================
