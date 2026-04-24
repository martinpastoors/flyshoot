# ============================================================================
# fix_elog_trek_trip_ids.R
#
# PURPOSE: Fix trip_id mismatch between elog and elog_trek parquet files.
#
# PROBLEM: When elog_trek files don't contain a 'trip' column, the pipeline
# derives trip_id from the filename (e.g. "20260316_20260319" → "2026031620260319").
# This doesn't match the trip_id in elog (e.g. "2026031600015").
#
# SOLUTION: After loading both parquets, re-assign trip_id in elog_trek by
# joining to elog on vessel + date + haul_id (where available), or
# vessel + date range overlap.
#
# RUN THIS ONCE after conversion, then re-save elog_trek parquet.
# ============================================================================

library(tidyverse)
library(arrow)
library(glue)
library(lubridate)

project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop("Set FLYSHOOT_PROJECT in .Renviron")
p <- function(...) file.path(project_root, ...)

config   <- jsonlite::read_json(p("config.json"))
source(p("R", "harmonized", "02_storage_functions.R"))

cat(strrep("=", 60), "\n")
cat("FIX ELOG_TREK TRIP_IDs\n")
cat(strrep("=", 60), "\n\n")

# ── Load both datasets ────────────────────────────────────────────────────────
cat("Loading elog...\n")
elog      <- load_flyshoot_data("elog")
cat(glue("  {nrow(elog)} rows | vessels: {paste(sort(unique(elog$vessel)), collapse=', ')}\n\n"))

cat("Loading elog_trek...\n")
elog_trek <- load_flyshoot_data("elog_trek")
cat(glue("  {nrow(elog_trek)} rows | vessels: {paste(sort(unique(elog_trek$vessel)), collapse=', ')}\n\n"))

# ── Check for mismatched trip_ids ─────────────────────────────────────────────
cat("Checking trip_id alignment...\n")

# Get trip_id lookup from elog: vessel + date → trip_id
# elog has authoritative trip_ids because it comes from the e-logbook system
elog_trip_lookup <- elog %>%
  dplyr::select(vessel, date, trip_id) %>%
  dplyr::distinct() %>%
  dplyr::rename(trip_id_elog = trip_id)

# Find elog_trek trip_ids that DON'T exist in elog
et_trips   <- elog_trek %>% dplyr::distinct(vessel, trip_id)
elog_trips <- elog      %>% dplyr::distinct(vessel, trip_id)

mismatched <- et_trips %>%
  dplyr::anti_join(elog_trips, by = c("vessel", "trip_id"))

cat(glue("  elog_trek trips:          {nrow(et_trips)}\n"))
cat(glue("  Mismatched (not in elog): {nrow(mismatched)}\n\n"))

if (nrow(mismatched) == 0) {
  cat("✓ All elog_trek trip_ids match elog — no fix needed.\n")
  stop("Nothing to fix.", call. = FALSE)
}

cat("Mismatched trip_ids in elog_trek:\n")
print(mismatched)
cat("\n")

# ── Re-assign trip_id in elog_trek via trip date-range join ──────────────────
# Use trip-level date ranges from elog (not individual dates) to avoid
# many-to-many at trip boundaries where two trips share the same date.
cat("Building trip date ranges from elog...\n")

elog_trip_ranges <- elog %>%
  dplyr::group_by(vessel, trip_id) %>%
  dplyr::summarise(
    trip_start = min(date, na.rm = TRUE),
    trip_end   = max(date, na.rm = TRUE),
    .groups = "drop"
  )

cat(glue("  {nrow(elog_trip_ranges)} trips in elog\n\n"))

cat("Re-assigning trip_ids via vessel + date-range join to elog...\n")

# For each elog_trek row, find which elog trip its date falls within.
# Use a non-equi join via data.table or a tidy approach:
elog_trek_fixed <- elog_trek %>%
  dplyr::mutate(trip_id_old = trip_id) %>%
  # Join trip ranges: keep rows where et date falls within [trip_start, trip_end]
  dplyr::left_join(elog_trip_ranges, by = "vessel",
                   relationship = "many-to-many") %>%
  dplyr::filter(is.na(trip_start) | (date >= trip_start & date <= trip_end)) %>%
  # Where multiple trips match (boundary dates), pick the one with most rows in elog
  dplyr::left_join(
    elog %>% dplyr::count(vessel, trip_id, name = "elog_n"),
    by = c("vessel", "trip_id")
  ) %>%
  dplyr::arrange(vessel, trip_id_old, date, dplyr::desc(elog_n)) %>%
  dplyr::group_by(vessel, trip_id_old, date,
                  dplyr::across(dplyr::any_of(
                    c("haul_id","species_code","size_category","weight_kg")
                  ))) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    trip_id = dplyr::if_else(!is.na(trip_start), trip_id, trip_id_old)
  ) %>%
  dplyr::select(-trip_start, -trip_end, -elog_n)

# Report what changed
changed <- elog_trek_fixed %>%
  dplyr::filter(trip_id != trip_id_old) %>%
  dplyr::distinct(vessel, trip_id_old, trip_id)

cat(glue("  Records updated: {sum(elog_trek_fixed$trip_id != elog_trek_fixed$trip_id_old)}\n"))
cat(glue("  Trip_id mappings applied:\n"))
print(changed)
cat("\n")

# Check for any remaining unmatched (date not in elog)
still_missing <- elog_trek_fixed %>%
  dplyr::distinct(vessel, trip_id) %>%
  dplyr::anti_join(elog_trips, by = c("vessel", "trip_id"))

if (nrow(still_missing) > 0) {
  cat("⚠ Some elog_trek trip_ids still unresolved (no matching date in elog):\n")
  print(still_missing)
  cat("\nThese rows will keep their original trip_id.\n\n")
} else {
  cat("✓ All trip_ids successfully resolved.\n\n")
}

# Remove the helper column
elog_trek_fixed <- dplyr::select(elog_trek_fixed, -trip_id_old)

# ── Save fixed parquet ────────────────────────────────────────────────────────
cat("Saving fixed elog_trek parquet...\n")

save_flyshoot_data(
  elog_trek_fixed, "elog_trek",
  date_range = c(min(elog_trek_fixed$date, na.rm = TRUE),
                 max(elog_trek_fixed$date, na.rm = TRUE))
)

cat(glue("✓ Saved {nrow(elog_trek_fixed)} rows to elog_trek parquet\n\n"))

# ── Verify ────────────────────────────────────────────────────────────────────
cat("Verifying fix...\n")
et_check   <- load_flyshoot_data("elog_trek") %>% dplyr::distinct(vessel, trip_id)
remaining  <- et_check %>% dplyr::anti_join(elog_trips, by = c("vessel", "trip_id"))
cat(glue("  Remaining mismatches: {nrow(remaining)}\n"))
if (nrow(remaining) == 0) {
  cat("✓ All elog_trek trip_ids now match elog.\n")
} else {
  cat("⚠ Some mismatches remain (likely elog_trek-only vessels without elog data):\n")
  print(remaining)
}

cat("\nDone.\n")
