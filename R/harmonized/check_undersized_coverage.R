# check_undersized_coverage.R
# ============================================================================
# Compares undersized catch coverage between elog and elog_trek parquets.
# Shows which vessel/trip combinations have undersized in elog but not elog_trek
# ============================================================================

library(tidyverse)
library(glue)
library(here)
library(jsonlite)

config <- read_json("config.json")
source(file.path(here(), "R/harmonized/02_storage_functions.R"))

elog      <- load_flyshoot_data("elog")
elog_trek <- load_flyshoot_data("elog_trek")

cat("=" |> rep(70) |> paste0(collapse=""), "\n")
cat("UNDERSIZED CATCH COVERAGE: elog vs elog_trek\n")
cat("=" |> rep(70) |> paste0(collapse=""), "\n\n")

# ── Elog: trips with undersized catch ────────────────────────────────────────
elog_undersized <- elog %>%
  dplyr::filter(size_category == "BMS" ) %>%
  dplyr::group_by(vessel, trip_id, date) %>%
  dplyr::summarise(
    n_elog_undersized  = dplyr::n(),
    kg_elog_undersized = round(sum(weight_kg, na.rm = TRUE), 1),
    species_elog       = paste(sort(unique(species_code)), collapse = ", "),
    .groups = "drop"
  )

cat(glue("Trips with undersized catch in elog: {nrow(elog_undersized)}\n\n"))
print(elog_undersized)

# ── Elog_trek: check same trips ───────────────────────────────────────────────
cat("\n", "-" |> rep(70) |> paste0(collapse=""), "\n", sep="")
cat("Elog_trek coverage for those trips:\n\n")

et_coverage <- elog_trek %>%
  dplyr::semi_join(elog_undersized, by = c("vessel", "trip_id")) %>%
  dplyr::filter(is.na(size_category)) %>% 
  dplyr::group_by(vessel, trip_id) %>%
  dplyr::summarise(
    has_elog_trek        = TRUE,
    n_trek_total         = dplyr::n(),
    n_trek_undersized    = sum(size_category == "undersized", na.rm = TRUE),
    kg_trek_undersized   = round(sum(
      dplyr::if_else(!is.na(weight_undersized_kg), weight_undersized_kg, 0),
      na.rm = TRUE), 1),
    species_trek         = paste(sort(unique(species_code)), collapse = ", "),
    .groups = "drop"
  )

# Full comparison
comparison <- elog_undersized %>%
  dplyr::left_join(et_coverage, by = c("vessel", "trip_id")) %>%
  dplyr::mutate(
    has_elog_trek     = dplyr::if_else(is.na(has_elog_trek), FALSE, has_elog_trek),
    trek_undersized   = dplyr::if_else(is.na(n_trek_undersized), 0L, n_trek_undersized),
    status = dplyr::case_when(
      !has_elog_trek                          ~ "NO elog_trek data for this trip",
      has_elog_trek & trek_undersized == 0    ~ "elog_trek present but NO undersized rows",
      has_elog_trek & trek_undersized  > 0    ~ "OK — undersized in both"
    )
  ) %>%
  dplyr::select(vessel, trip_id, kg_elog_undersized, has_elog_trek,
                trek_undersized, status)

print(comparison, n = 99)

cat("\n", "-" |> rep(70) |> paste0(collapse=""), "\n", sep="")
cat("Summary:\n")
comparison %>%
  dplyr::count(status) %>%
  dplyr::rename(n_trips = n) %>%
  print()
