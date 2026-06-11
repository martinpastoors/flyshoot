# =============================================================================
# 06_turbocatch_diagnostics.R
# Quick diagnostics for the temporary turbocatch parquet files
#
# Usage:
#   source("06_turbocatch_diagnostics.R")
#   turbocatch_diagnostics("C:/ERS_exports/turbocatch_test")
# =============================================================================

library(arrow)
library(dplyr)
library(glue)

turbocatch_diagnostics <- function(parquet_dir) {

  message("\n", strrep("=", 60))
  message("TURBOCATCH PARQUET DIAGNOSTICS")
  message(glue("Folder: {parquet_dir}"))
  message(strrep("=", 60))

  # ── helper: load one table ────────────────────────────────────────
  load_table <- function(name) {
    path <- file.path(parquet_dir, paste0("turbocatch_", name, ".parquet"))
    if (!file.exists(path)) {
      message(glue("\n  ✗ turbocatch_{name}.parquet NOT FOUND"))
      return(NULL)
    }
    df <- read_parquet(path)
    size_kb <- round(file.size(path) / 1024, 1)
    message(glue("\n  ✓ turbocatch_{name}.parquet  ",
                 "({nrow(df)} rows x {ncol(df)} cols, {size_kb} KB)"))
    df
  }

  trip  <- load_table("trip")
  haul  <- load_table("haul")
  catch <- load_table("catch")

  # ── TRIP ─────────────────────────────────────────────────────────
  if (!is.null(trip) && nrow(trip) > 0) {
    message("\n", strrep("-", 60))
    message("TRIPS")
    message(strrep("-", 60))
    message("Columns: ", paste(names(trip), collapse = ", "))
    message(glue("\nVessels : {paste(unique(trip$vessel), collapse = ', ')}"))
    message(glue("N trips : {nrow(trip)}"))
    message(glue("Date range: {min(trip$departure_date, na.rm=TRUE)}",
                 " to {max(trip$arrival_date, na.rm=TRUE)}"))
    message("\nTrip overview:")
    trip %>%
      select(any_of(c("vessel", "trip_id", "departure_date", "departure_port",
                       "arrival_date", "arrival_port", "n_hauls",
                       "n_shots_total", "far_kg_total", "fishing_areas",
                       "gears_declared"))) %>%
      as.data.frame() %>%
      print()
  }

  # ── HAUL ─────────────────────────────────────────────────────────
  if (!is.null(haul) && nrow(haul) > 0) {
    message("\n", strrep("-", 60))
    message("HAULS")
    message(strrep("-", 60))
    message("Columns: ", paste(names(haul), collapse = ", "))

    message("\nHauls per trip:")
    haul %>%
      group_by(vessel, trip_id) %>%
      summarise(
        n_hauls            = n(),
        date_from          = min(date, na.rm = TRUE),
        date_to            = max(date, na.rm = TRUE),
        n_shots_total      = sum(n_shots, na.rm = TRUE),
        avg_duration_min   = round(mean(fishing_time_hours * 60, na.rm = TRUE), 0),
        ices_rects         = paste(sort(unique(na.omit(ices_rect))), collapse = ", "),
        .groups = "drop"
      ) %>%
      as.data.frame() %>%
      print()

    message("\nPositions available:")
    message(glue("  shoot_lat/lon non-NA: ",
                 "{sum(!is.na(haul$shoot_lat))} / {nrow(haul)}"))

    message("\nGear types:")
    haul %>% count(gear_type, mesh_size_mm) %>%
      as.data.frame() %>% print()
  }

  # ── CATCH ─────────────────────────────────────────────────────────
  if (!is.null(catch) && nrow(catch) > 0) {
    message("\n", strrep("-", 60))
    message("CATCH")
    message(strrep("-", 60))
    message("Columns: ", paste(names(catch), collapse = ", "))

    message("\nSpecies totals across all trips (kg, FAR):")
    catch %>%
      group_by(species_code) %>%
      summarise(
        total_kg   = sum(weight_kg, na.rm = TRUE),
        n_hauls    = n_distinct(paste(trip_id, haul_id)),
        n_trips    = n_distinct(trip_id),
        pct_weighed = round(100 * mean(measure_method == "WGH", na.rm = TRUE), 0),
        .groups = "drop"
      ) %>%
      arrange(desc(total_kg)) %>%
      as.data.frame() %>%
      print()

    message("\nCatch per trip:")
    catch %>%
      group_by(vessel, trip_id) %>%
      summarise(
        total_kg   = sum(weight_kg, na.rm = TRUE),
        n_species  = n_distinct(species_code),
        n_hauls    = n_distinct(haul_id),
        .groups = "drop"
      ) %>%
      as.data.frame() %>%
      print()

    message("\nMissing values:")
    catch %>%
      summarise(across(everything(),
                       ~ sum(is.na(.)))) %>%
      select(where(~ . > 0)) %>%
      as.data.frame() %>%
      print()
  }

  message("\n", strrep("=", 60))
  message("END DIAGNOSTICS")
  message(strrep("=", 60), "\n")

  invisible(list(trip = trip, haul = haul, catch = catch))
}

# =============================================================================
# EXAMPLE
# =============================================================================
# source("06_turbocatch_diagnostics.R")
#
# turbocatch_diagnostics(
#   "C:/ERS_exports/turbocatch_test"
# )
# =============================================================================
