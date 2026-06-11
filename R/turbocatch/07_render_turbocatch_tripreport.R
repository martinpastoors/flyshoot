# =============================================================================
# 07_render_turbocatch_tripreport.R
# Render a tripreport for a TurboCatch trip WITHOUT touching the main pipeline.
#
# Strategy: load the turbocatch parquet files, reshape them to match the
# column names the Rmd expects, and inject them as options so the Rmd's
# data-loading block picks them up instead of calling load_flyshoot_data().
#
# Usage:
#   source("07_render_turbocatch_tripreport.R")
#   render_turbocatch_tripreport(
#     parquet_dir = "C:/.../parquet/CC545762",
#     trip_id     = "20260001",          # ELOG TN value from diagnostics
#     output_dir  = "C:/.../reports"
#   )
# =============================================================================

library(arrow)
library(dplyr)
library(lubridate)
library(glue)
library(rmarkdown)
library(here)

#' Render a tripreport for one TurboCatch trip
#'
#' @param parquet_dir  Folder written by write_turbocatch_parquet()
#' @param trip_id      The ELOG TN value (trip identifier) to render, e.g. "20260001"
#'                     Run turbocatch_diagnostics() first to see available trip_ids.
#'                     If NULL, renders the most recent trip.
#' @param output_dir   Where to write the .docx report (default: parquet_dir/reports)
#' @param rmd_file     Path to the tripreport Rmd template
#' @param lang         Report language: "nl" or "en" (default "nl")
render_turbocatch_tripreport <- function(
    parquet_dir,
    trip_id    = NULL,
    output_dir = file.path(parquet_dir, "..", "reports"),
    rmd_file   = file.path(here::here(), "R", "harmonized",
                           "FLYSHOOT_tripreport_harmonized.Rmd"),
    lang       = "nl"
) {

  # ── 1. Load turbocatch parquet tables ──────────────────────────────────────
  message("── Loading turbocatch parquet files ──────────────────────────")

  tc_trip  <- read_parquet(file.path(parquet_dir, "turbocatch_trip.parquet"))
  tc_haul  <- read_parquet(file.path(parquet_dir, "turbocatch_haul.parquet"))
  tc_catch <- read_parquet(file.path(parquet_dir, "turbocatch_catch.parquet"))

  # ── 2. Select trip ─────────────────────────────────────────────────────────
  if (is.null(trip_id)) {
    trip_id <- tc_trip %>%
      arrange(desc(departure_date)) %>%
      slice(1) %>%
      pull(trip_id)
    message(glue("  No trip_id supplied — using most recent: {trip_id}"))
  }

  trip_meta <- tc_trip %>% filter(trip_id == !!trip_id)
  if (nrow(trip_meta) == 0) {
    available <- paste(tc_trip$trip_id, collapse = ", ")
    stop(glue("trip_id '{trip_id}' not found. Available: {available}"))
  }

  vessel_id <- trip_meta$vessel
  startdate <- as.Date(trip_meta$departure_date)
  enddate   <- as.Date(trip_meta$arrival_date)

  message(glue("  Vessel: {vessel_id}  |  Trip: {trip_id}  |  ",
               "{startdate} – {enddate}"))

  # ── 3. Reshape to pipeline schema ─────────────────────────────────────────
  # Haul: turbocatch uses `vessel_id`; Rmd expects `vessel`
  haul_tc <- tc_haul %>%
    filter(trip_id == !!trip_id) %>%
    rename(vessel = vessel) %>%          # already named `vessel` by adapter
    mutate(
      # Rmd may use `shoot_time` as POSIXct — ensure it is
      shoot_time = as.POSIXct(shoot_time, tz = "UTC"),
      # Rmd expects total_catch_kg on the haul row (sum from FAR)
      total_catch_kg = NA_real_           # not in ERS haul table; patched below
    )

  # Derive total_catch_kg per haul from catch table
  haul_totals <- tc_catch %>%
    filter(trip_id == !!trip_id) %>%
    group_by(trip_id, haul_id) %>%
    summarise(total_catch_kg = sum(weight_kg, na.rm = TRUE), .groups = "drop")

  haul_tc <- haul_tc %>%
    select(-total_catch_kg) %>%
    left_join(haul_totals, by = c("trip_id", "haul_id"))

  # Catch: rename `species_code` → `species_code` (already correct)
  # The Rmd uses `species_code` and `weight_kg` — both present
  catch_tc <- tc_catch %>%
    filter(trip_id == !!trip_id) %>%
    mutate(
      # Rmd kisten table may expect `weighing_time` — add from haul date
      weighing_time = as.POSIXct(date, tz = "UTC")
    )

  # ── 4. Inject into global options so Rmd can find them ────────────────────
  # The Rmd reads vessel/date from options, then loads data via load_flyshoot_data().
  # We override that by pre-assigning the data frames to .GlobalEnv so any
  # internal reference picks up the turbocatch data.
  message("  Injecting turbocatch data into render environment")

  options(
    flyshoot.vessel    = vessel_id,
    flyshoot.startdate = startdate,
    flyshoot.enddate   = enddate,
    flyshoot.lang      = lang,
    # Signal to Rmd that data is pre-loaded (checked in setup chunk if present)
    flyshoot.datasource = "turbocatch"
  )

  # Pre-assign to global so Rmd picks them up if it references these names
  assign("haul_preloaded",  haul_tc,  envir = .GlobalEnv)
  assign("catch_preloaded", catch_tc, envir = .GlobalEnv)
  assign("trip_preloaded",  trip_meta, envir = .GlobalEnv)

  # ── 5. Build output filename ───────────────────────────────────────────────
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  yr  <- year(startdate)
  wk  <- isoweek(startdate)
  output_filename <- glue("{vessel_id}_{yr}W{sprintf('%02d', wk)}_turbocatch.docx")
  output_path     <- file.path(output_dir, output_filename)

  message(glue("── Rendering: {output_filename}"))

  if (!file.exists(rmd_file)) {
    stop(glue(
      "Rmd template not found:\n  {rmd_file}\n",
      "Supply the correct path via the rmd_file argument."
    ))
  }

  # ── 6. Render ──────────────────────────────────────────────────────────────
  tryCatch(
    rmarkdown::render(
      input       = rmd_file,
      output_file = output_path,
      quiet       = FALSE
    ),
    error = function(e) {
      message(glue("  ✗ Render failed: {e$message}"))
      message("\n  Tip: if the Rmd cannot find 'haul' or 'catch', add this near the")
      message("  top of its setup chunk (after load_flyshoot_data calls):\n")
      message("    if (exists('haul_preloaded')  && nrow(haul_preloaded)  > 0) haul  <- haul_preloaded")
      message("    if (exists('catch_preloaded') && nrow(catch_preloaded) > 0) kisten <- catch_preloaded")
      message("    if (exists('trip_preloaded')  && nrow(trip_preloaded)  > 0) trip   <- trip_preloaded")
      stop(e)
    }
  )

  message(glue("  ✓ Report written to: {output_path}"))
  invisible(output_path)
}

# =============================================================================
# EXAMPLE
# =============================================================================
# source("parse_ers.R")
# source("04_turbocatch_adapter.R")
# source("07_render_turbocatch_tripreport.R")
#
# # See what trips are available
# turbocatch_diagnostics("C:/.../parquet/CC545762")
#
# # Render the most recent trip
# render_turbocatch_tripreport(
#   parquet_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch ExportMessagesERS_EVLinden/parquet/CC545762",
#   trip_id     = "20260001",
#   output_dir  = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch ExportMessagesERS_EVLinden/reports"
# )
# =============================================================================
