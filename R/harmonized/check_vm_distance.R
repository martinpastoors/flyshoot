# ============================================================================
# check_vm_distance.R
# Quick diagnostic: what is in vessel_movement, and can we get distance/trip?
# Run this interactively in the same R session as the tripreport, or standalone.
# ============================================================================

library(dplyr)
library(arrow)
library(glue)

# ── 1. Load config ────────────────────────────────────────────────────────────
project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (nchar(project_root) == 0) stop("Set FLYSHOOT_PROJECT env var first")

config <- jsonlite::read_json(file.path(project_root, "config.json"))
raw_path <- config$raw_data_path

vm_path <- file.path(raw_path, "vessel_movement", "vessel_movement.parquet")
if (!file.exists(vm_path)) stop("vessel_movement.parquet not found at: ", vm_path)

vm_all <- arrow::read_parquet(vm_path)

cat("\n=== vessel_movement columns ===\n")
print(names(vm_all))

cat("\n=== event_type breakdown ===\n")
print(table(vm_all$event_type, useNA = "ifany"))

cat("\n=== distance column? ===\n")
if ("distance" %in% names(vm_all)) {
  cat("YES — summary:\n")
  print(summary(vm_all$distance))
  cat(glue("  NA count: {sum(is.na(vm_all$distance))} / {nrow(vm_all)}\n\n"))
} else {
  cat("NO — 'distance' column does not exist in vessel_movement\n\n")
}

cat("\n=== vessels available ===\n")
print(sort(unique(vm_all$vessel)))

cat("\n=== trips per vessel (first 20) ===\n")
print(vm_all %>% distinct(vessel, trip_id) %>% arrange(vessel, trip_id) %>% head(20))

# ── 2. Compute distance from coordinates (haversine) if distance col missing ──
haversine_km <- function(lon1, lat1, lon2, lat2) {
  R   <- 6371
  phi <- (lat2 - lat1) * pi / 180
  lam <- (lon2 - lon1) * pi / 180
  a   <- sin(phi/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(lam/2)^2
  2 * R * asin(sqrt(a))
}

cat("\n=== Distance per trip (computed from coordinates) ===\n")
dist_by_trip <- vm_all %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  arrange(vessel, trip_id, date) %>%
  group_by(vessel, trip_id) %>%
  mutate(
    dist_km = haversine_km(
      dplyr::lag(lon, default = first(lon)),
      dplyr::lag(lat, default = first(lat)),
      lon, lat
    ),
    dist_nm = dist_km / 1.852
  ) %>%
  summarise(
    n_events        = n(),
    total_dist_km   = round(sum(dist_km, na.rm = TRUE), 1),
    total_dist_nm   = round(sum(dist_nm, na.rm = TRUE), 1),
    has_dist_col    = "distance" %in% names(vm_all),
    dist_col_total  = if ("distance" %in% names(vm_all))
                        round(sum(distance, na.rm = TRUE), 1) else NA_real_,
    .groups = "drop"
  )

print(dist_by_trip, n = 30)

cat("\n=== Recommendation ===\n")
if ("distance" %in% names(vm_all) && !all(is.na(vm_all$distance))) {
  cat("Use existing 'distance' column — it has values.\n")
  cat("Check units: typical North Sea trip = 200-600 nm\n")
} else {
  cat("'distance' column missing or all NA.\n")
  cat("Recommendation: compute from lat/lon using haversine (see dist_by_trip above).\n")
  cat("Add to Rmd: replace the distance join with the haversine approach.\n")
}
