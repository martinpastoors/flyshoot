# =============================================================================
# check_far_frequency.R
# Quick diagnostic: how many FAR messages per day per trip?
# This tells us whether FAR = one per haul or one per day (accumulated report)
# =============================================================================

library(arrow)
library(dplyr)
library(lubridate)

check_far_frequency <- function(parquet_dir) {

  haul <- read_parquet(file.path(parquet_dir, "turbocatch_haul.parquet"))

  cat("\n=== FAR messages per day (sample — first 30 trip-days) ===\n")
  haul %>%
    group_by(vessel, trip_id, date) %>%
    summarise(
      n_far_messages = n(),
      total_n_shots  = sum(n_shots, na.rm = TRUE),
      avg_n_shots    = round(mean(n_shots, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(vessel, trip_id, date) %>%
    print(n = 30)

  cat("\n=== Distribution of n_shots per FAR message ===\n")
  haul %>%
    count(n_shots) %>%
    arrange(n_shots) %>%
    print(n = Inf)

  cat("\n=== FAR messages per day — summary across all trips ===\n")
  haul %>%
    group_by(vessel, trip_id, date) %>%
    summarise(n_far = n(), .groups = "drop") %>%
    summarise(
      min_far_per_day    = min(n_far),
      median_far_per_day = median(n_far),
      max_far_per_day    = max(n_far),
      pct_one_per_day    = round(100 * mean(n_far == 1), 1),
      pct_multi_per_day  = round(100 * mean(n_far > 1), 1)
    ) %>%
    print()

  cat("\n=== n_shots distribution by year ===\n")
  haul %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(
      n_far_messages    = n(),
      total_shots       = sum(n_shots, na.rm = TRUE),
      avg_shots_per_far = round(mean(n_shots, na.rm = TRUE), 1),
      max_shots_per_far = max(n_shots, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print(n = Inf)
}

# =============================================================================
# USAGE
# =============================================================================
# source("check_far_frequency.R")
# check_far_frequency("C:/.../parquet/CC545762")
