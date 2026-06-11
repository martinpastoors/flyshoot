# =============================================================================
# 05_turbocatch_to_parquet.R
# Write TurboCatch ERS data to temporary parquet files for inspection.
# Does NOT touch the main flyshoot parquet pipeline.
#
# Output (one parquet per table, written to output_dir):
#   turbocatch_haul.parquet   — one row per haul
#   turbocatch_catch.parquet  — one row per species per haul (FAR only)
#   turbocatch_trip.parquet   — one row per trip
#
# Usage:
#   source("parse_ers.R")
#   source("04_turbocatch_adapter.R")
#   source("05_turbocatch_to_parquet.R")
#
#   write_turbocatch_parquet(
#     xml_folder = "C:/ERS_exports/anonymized",   # folder with XML files
#     output_dir = "C:/ERS_exports/turbocatch_test"
#   )
# =============================================================================

library(arrow)
library(dplyr)
library(glue)

#' Parse TurboCatch XML files and write to temporary parquet files
#'
#' @param xml_folder  Folder containing anonymized ERS XML files.
#'                    Can be a single flat folder (all XMLs together) or a
#'                    parent folder with one sub-folder per vessel/trip.
#' @param output_dir  Where to write the parquet files (created if needed).
#' @param recursive   Search sub-folders for XML files? Default TRUE.
#' @param overwrite   Overwrite existing parquet files? Default TRUE.
write_turbocatch_parquet <- function(xml_folder,
                                     output_dir = file.path(xml_folder,
                                                            "turbocatch_parquet"),
                                     recursive  = TRUE,
                                     overwrite  = TRUE) {

  # --- parse all XML files
  message("── Step 1: Parsing ERS XML files ──────────────────────────────")
  ers <- parse_ers_folder(xml_folder, recursive = recursive)
  if (is.null(ers)) stop("No ERS data parsed — check xml_folder path.")

  # --- convert to pipeline format
  message("\n── Step 2: Converting to pipeline schema ───────────────────────")
  haul_data  <- ers_to_haul(ers)
  catch_data <- ers_to_catch(ers)
  trip_data  <- ers_to_trip(ers)

  message(glue("  Trips : {nrow(trip_data)}"))
  message(glue("  Hauls : {nrow(haul_data)}"))
  message(glue("  Catch : {nrow(catch_data)} species rows"))

  # --- write parquet
  message("\n── Step 3: Writing parquet files ───────────────────────────────")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(glue("  Created output folder: {output_dir}"))
  }

  write_table <- function(df, name) {
    path <- file.path(output_dir, paste0("turbocatch_", name, ".parquet"))
    if (file.exists(path) && !overwrite) {
      message(glue("  SKIP (exists): {basename(path)}"))
      return(invisible(NULL))
    }
    write_parquet(df, path)
    size_kb <- round(file.size(path) / 1024, 1)
    message(glue("  ✓ {basename(path)}  ({nrow(df)} rows, {size_kb} KB)"))
    invisible(path)
  }

  write_table(trip_data,  "trip")
  write_table(haul_data,  "haul")
  write_table(catch_data, "catch")

  # --- quick validation readback
  message("\n── Step 4: Validation readback ─────────────────────────────────")
  for (name in c("trip", "haul", "catch")) {
    path <- file.path(output_dir, paste0("turbocatch_", name, ".parquet"))
    df   <- read_parquet(path)
    message(glue("  {name}: {nrow(df)} rows  x  {ncol(df)} cols  |  ",
                 "columns: {paste(names(df), collapse = ', ')}"))
  }

  message(glue("\n── Done. Files written to: {output_dir}"))
  invisible(output_dir)
}


#' Read the temporary turbocatch parquet files back into R
#'
#' @param output_dir  Folder where write_turbocatch_parquet() wrote its files
#' @return Named list: $trip, $haul, $catch
read_turbocatch_parquet <- function(output_dir) {
  tables <- c("trip", "haul", "catch")
  out <- setNames(
    lapply(tables, function(name) {
      path <- file.path(output_dir, paste0("turbocatch_", name, ".parquet"))
      if (!file.exists(path)) {
        warning(glue("File not found: {path}"))
        return(NULL)
      }
      read_parquet(path)
    }),
    tables
  )
  message(glue("Read {sum(!sapply(out, is.null))} table(s) from {output_dir}"))
  out
}

# =============================================================================
# EXAMPLE
# =============================================================================
# source("parse_ers.R")
# source("04_turbocatch_adapter.R")
# source("05_turbocatch_to_parquet.R")
#
# # Write
# write_turbocatch_parquet(
#   xml_folder = "C:/ERS_exports/anonymized",
#   output_dir = "C:/ERS_exports/turbocatch_test"
# )
#
# # Read back and inspect
# tc <- read_turbocatch_parquet("C:/ERS_exports/turbocatch_test")
#
# View(tc$trip)
# View(tc$haul)
# View(tc$catch)
#
# # Spot checks
# tc$catch %>%
#   group_by(trip_id, species_code) %>%
#   summarise(total_kg = sum(weight_kg, na.rm = TRUE), .groups = "drop") %>%
#   arrange(trip_id, desc(total_kg))
#
# tc$haul %>%
#   group_by(trip_id) %>%
#   summarise(n_hauls = n(), total_kg = sum(total_catch_kg, na.rm = TRUE))
# =============================================================================
