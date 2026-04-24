# ==============================================================================
# repair_parquet_types.R
# ==============================================================================
# One-time fix: coerce shoot_lon / shoot_lat (and other numeric columns) from
# character to double in the elog and elog_trek parquet files.
#
# Run this ONCE if you have parquet files converted from legacy RData where
# coordinate columns were stored as character.
#
# After running this script, re-run 03_main_workflow.R to rebuild the reports.
# ==============================================================================

library(arrow)
library(dplyr)
library(glue)
library(jsonlite)
library(here)

if (!file.exists("config.json")) stop("config.json not found — run from project root")
config <- read_json("config.json")
source(file.path(here(), "R", "harmonized", "02_storage_functions.R"))

# Columns that must be numeric but may have been stored as character
NUMERIC_COLS <- c("shoot_lon", "shoot_lat", "haul_lon", "haul_lat",
                  "weight_kg", "weight_undersized_kg", "conversion_factor",
                  "fishing_time_hours", "total_catch_kg", "marketable_catch_kg")

INTEGER_COLS <- c("haul_id", "mesh_size_mm", "box_count", "boxes_undersized",
                  "water_depth", "wind_force_bft", "vertical_opening_m",
                  "cable_length_m", "cable_thickness_mm")

repair_parquet <- function(data_type) {
  cat(glue("\n── Repairing {data_type} ──────────────────────────────\n"))

  dat <- tryCatch(
    load_flyshoot_data(data_type),
    error = function(e) { cat(glue("  ⚠ Could not load {data_type}: {e$message}\n")); NULL }
  )
  if (is.null(dat) || nrow(dat) == 0) { cat("  ⊘ Empty or missing — skipped\n"); return(invisible()) }

  # Report current types of problem columns
  present_num <- intersect(NUMERIC_COLS, names(dat))
  present_int <- intersect(INTEGER_COLS, names(dat))

  bad_num <- dat %>% dplyr::select(all_of(present_num)) %>%
    dplyr::select(where(~!is.numeric(.))) %>% names()
  bad_int <- dat %>% dplyr::select(all_of(present_int)) %>%
    dplyr::select(where(~!is.integer(.))) %>% names()

  # Report types of all key spatial columns regardless
  spatial_cols <- intersect(c("shoot_lon", "shoot_lat"), names(dat))
  if (length(spatial_cols) > 0) {
    types <- dat %>% dplyr::select(all_of(spatial_cols)) %>%
      purrr::map_chr(~class(.x)[1])
    cat(glue("  shoot_lon/lat types: {paste(names(types), types, sep='=', collapse=', ')}\n"))
  }

  if (length(bad_num) == 0 && length(bad_int) == 0) {
    cat("  ✓ All numeric/integer columns already correct — no repair needed\n")
    return(invisible())
  }

  if (length(bad_num) > 0)
    cat(glue("  Fixing numeric: {paste(bad_num, collapse=', ')}\n"))
  if (length(bad_int) > 0)
    cat(glue("  Fixing integer: {paste(bad_int, collapse=', ')}\n"))

  dat <- dat %>%
    mutate(
      across(all_of(bad_num), ~suppressWarnings(as.numeric(.))),
      across(all_of(bad_int), ~suppressWarnings(as.integer(as.numeric(.))))
    )

  # Overwrite parquet
  save_flyshoot_data(dat, data_type)
  cat(glue("  ✓ {data_type} repaired and saved ({nrow(dat)} rows)\n"))
}

cat("=" |> rep(60) |> paste0(collapse=""), "\n")
cat("PARQUET TYPE REPAIR\n")
cat("=" |> rep(60) |> paste0(collapse=""), "\n")

for (dtype in c("elog", "elog_trek", "haul", "kisten", "trip")) {
  repair_parquet(dtype)
}

cat("\n", "=" |> rep(60) |> paste0(collapse=""), "\n")
cat("REPAIR COMPLETE\n")
cat("Re-run 03_main_workflow.R or batch_render_tripreports.R\n")
