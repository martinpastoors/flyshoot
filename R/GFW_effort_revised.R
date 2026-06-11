# ============================================================
# GFW Effort Analysis — gfwr v3.0
# Goals:
#   1. Detailed GFW vs CV days-at-sea comparison by vessel/year/division
#   2. Fishing effort by year/country/vessel/gear/division/EEZ
# ============================================================

library(tidyverse)
library(lubridate)
library(gfwr)
library(sf)

key <- gfw_auth()

spatialdir   <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir  <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

source("R/FLYSHOOT utils.r")

# ---- Target ICES divisions ----
target_divisions <- c("27.4.A", "27.4.B", "27.4.C", "27.7.D", "27.7.E")

# ============================================================
# SPATIAL LAYERS
# ============================================================

fao_sf <- loadRData(file.path(spatialdir, "fao_sf.RData"))

fao_sf_division <-
  fao_sf %>%
  filter(F_LEVEL == "DIVISION") %>%
  dplyr::select(F_DIVISION) %>%
  rename(division = F_DIVISION) %>%
  mutate(division = toupper(division)) %>%
  filter(division %in% target_divisions) %>%
  group_by(division) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

# EEZ layer — load your EEZ shapefile here.
# It must have at minimum a column called `eez` (character, e.g. "FRA", "GBR", "IRL").
eez_sf <- loadRData(file.path(spatialdir, "eez_sf.RData")) %>% rename(eez=ISO_SOV1)


# ============================================================
# VESSEL LISTS
# ============================================================

vessels <-
  readxl::read_excel(file.path(flyshootdir, "flyshoot vessels update EvL.xlsx")) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi))

vessels_cv <-
  readxl::read_excel(file.path(flyshootdir, "flyshoot vessels CV.xlsx")) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi))

# ============================================================
# SECTION 1 — RESOLVE MMSI → GFW VESSEL IDs
# ============================================================
# We call the API once per MMSI and collect all selfReportedInfo rows
# (covering the full 2012-2025 transmission history) plus one registry row
# (gear, GT, length).
#
# purrr::map is used here because each iteration is an independent API call
# that cannot be vectorised — the benefit is safe error isolation via tryCatch
# and automatic list-to-dataframe binding, which outweighs the readability cost.

vessel_info <- map_dfr(vessels$mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)

    sri <- res$selfReportedInfo %>%
      arrange(desc(transmissionDateTo)) %>%
      slice(1)

    ri <- res$registryInfo %>%
      arrange(desc(latestVesselInfo)) %>%
      slice(1) %>%
      dplyr::select(geartypes, tonnageGt, lengthM)

    bind_cols(sri, ri) %>%
      mutate(mmsi_queried = m)

  }, error = function(e) { message("Failed MMSI: ", m, " — ", e$message); NULL })
})

# All vesselIds per MMSI — needed for full temporal coverage
vessel_ids_all <- map_dfr(vessels$mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)
    res$selfReportedInfo %>%
      dplyr::select(vesselId, ssvid, shipname, transmissionDateFrom, transmissionDateTo) %>%
      mutate(mmsi_queried = m)
  }, error = function(e) { message("Failed: ", m); NULL })
})

all_vessel_ids <- unique(vessel_ids_all$vesselId)

# ============================================================
# SECTION 2 — PULL FISHING EVENTS (2012–2025, all vessels)
# ============================================================
# One API call per vessel × year because the Events API is capped at 366 days.
# The inner map_dfr is kept because it cleanly collapses the yearly list per
# vessel; replacing it with a for-loop + bind_rows would be more verbose with
# no readability gain here.

all_events <- vector("list", length(all_vessel_ids))
names(all_events) <- all_vessel_ids

for (vid in all_vessel_ids) {
  message("Processing vessel: ", vid)

  events_v <- map_dfr(2012:2025, function(y) {
    Sys.sleep(0.5)
    tryCatch({
      result <- gfw_event(
        event_type = "FISHING",
        vessels    = vid,
        start_date = paste0(y, "-01-01"),
        end_date   = paste0(y, "-12-31"),
        key        = key
      )
      if (!is.null(result) && nrow(result) > 0) result else NULL
    }, error = function(e) {
      message("Failed: ", vid, " / ", y, " — ", e$message)
      NULL
    })
  })

  all_events[[vid]] <- events_v
  message("  → ", nrow(events_v), " events collected")
}

events_df <- bind_rows(all_events)

save(vessel_info, events_df, vessel_ids_all, all_vessel_ids,
     file = file.path(flyshootdir, "gfw_fishing_events_raw.RData"))

# load(file = file.path(flyshootdir, "gfw_fishing_events_raw.RData"))

# ============================================================
# SECTION 3 — ENRICH EVENTS WITH DIVISION AND EEZ
# ============================================================

# Convert events to sf using the midpoint coordinate of each event.
# GFW events provide `lat` and `lon` for the event centroid.
events_sf <-
  events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Assign ICES division via spatial join
events_with_division <-
  events_sf %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

# Assign EEZ (only if eez_sf has been loaded above)
if (!is.null(eez_sf)) {
  events_with_division <-
    events_with_division %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_join(eez_sf %>% dplyr::select(eez), join = st_within) %>%
    st_drop_geometry()
} else {
  events_with_division <- events_with_division %>%
    mutate(eez = NA_character_)
  message("EEZ layer not loaded — eez column set to NA. Load eez_sf to populate.")
}

# ============================================================
# SECTION 4 — MASTER EFFORT TABLE
# ============================================================
# A day at sea is defined as a unique calendar date on which a vessel
# had at least one fishing event. We count distinct dates rather than
# summing event durations to match the logbook definition.

# Build a vessel metadata table with one row per vesselId (= one MMSI period).
#
# The key design principle: a physical vessel (e.g. Z19) may have had several
# MMSIs over its life — GFW assigns a new vesselId for each MMSI period. We must
# NOT collapse to one row per MMSI because that discards historical events.
# Instead we keep ALL vesselIds and attach the stable physical-vessel identifiers
# (vessel_code, vessel_name, vessel_flag, gear, gt) from the fleet lists, which
# are keyed on MMSI (ssvid).
#
# The many-to-many problem is avoided at the JOIN stage below by joining events
# to metadata on `id` = vesselId (which GFW stores on every event row), not on
# ssvid. Since vesselId is unique per MMSI period, this join is strictly
# many-to-one: many events per vesselId, one metadata row per vesselId.
# After joining we group by vessel_code (stable physical vessel) so that all
# historical MMSIs for the same vessel aggregate correctly.

vessel_meta <-
  vessel_ids_all %>%
  dplyr::select(vesselId, ssvid, shipname, transmissionDateFrom, transmissionDateTo) %>%
  # Remove exact duplicate rows originating from vessel_ids_all itself
  # (confirmed: duplicates are fully identical rows, not content conflicts)
  distinct() %>%
  left_join(
    vessel_info %>%
      dplyr::select(vesselId, vessel_flag = flag, geartypes, tonnageGt),
    by = "vesselId"
  ) %>%
  left_join(
    vessels %>% dplyr::select(mmsi, vessel_name = vesselname, vessel_code = vessel, gear, gt),
    by = c("ssvid" = "mmsi")
  ) %>%
  left_join(
    vessels_cv %>% dplyr::select(mmsi, vessel_code_cv = vessel),
    by = c("ssvid" = "mmsi")
  ) %>%
  mutate(
    vessel_code = coalesce(vessel_code, vessel_code_cv),
    gear_final  = coalesce(geartypes, gear),
    gt_final    = coalesce(tonnageGt, gt)
  ) %>%
  dplyr::select(vesselId, ssvid, vessel_name, vessel_code, vessel_flag,
                gear = gear_final, gt = gt_final,
                transmissionDateFrom, transmissionDateTo)

# Verify: each vesselId should now appear exactly once.
# If this still warns, remaining duplicates have differing content and need manual review via:
#   vessel_meta %>%
#     filter(vesselId %in% (vessel_meta %>% filter(duplicated(vesselId)) %>% pull(vesselId))) %>%
#     arrange(vesselId)
vessel_meta_dupes <- vessel_meta %>% filter(duplicated(vesselId))
if (nrow(vessel_meta_dupes) > 0) {
  message("WARNING: ", nrow(vessel_meta_dupes),
          " duplicate vesselId(s) remain — rows differ in content, manual review needed")
} else {
  message("vessel_meta OK — all vesselIds unique (", nrow(vessel_meta), " rows)")
}

# Days-at-sea: distinct fishing dates per vessel x year x division x EEZ.
#
# Join on vesselId — present on both events_with_division and vessel_meta.
# This is strictly many-to-one so no duplication is introduced.
# Grouping is by vessel_code (stable physical vessel identity) so that all
# MMSI periods for the same vessel are summed together correctly.
gfw_days <-
  events_with_division %>%
  mutate(
    date    = as.Date(start),
    year    = year(date),
    quarter = quarter(date)
  ) %>%
  # Drop GFW-reported vessel columns that also exist in vessel_meta to avoid
  # .x/.y suffixes after the join — fleet list values take precedence
  dplyr::select(-vessel_name, -vessel_flag) %>%
  left_join(vessel_meta, by = "vesselId", relationship = "many-to-one") %>%
  distinct(vessel_code, vessel_name, vessel_ssvid, vessel_flag, gear, gt,
           year, quarter, division, eez, date) %>%
  group_by(year, quarter, vessel_code, vessel_name, vessel_flag, gear, gt,
           division, eez) %>%
  summarise(days_at_sea = n(), .groups = "drop")

# ============================================================
# SECTION 5 — GOAL 2: EFFORT SUMMARY TABLE
# ============================================================
# Effort by year / country / vessel / gear / division / EEZ

effort_summary <-
  gfw_days %>%
  group_by(year, vessel_flag, vessel_code, vessel_name, gear, division, eez) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  arrange(year, vessel_flag, vessel_code, division)

print(effort_summary, n = 30)

# Aggregated by year / country / gear / division (fleet-level, no vessel)
effort_fleet <-
  gfw_days %>%
  group_by(year, vessel_flag, gear, division, eez) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  arrange(year, vessel_flag, gear, division)

print(effort_fleet, n = 30)

# ============================================================
# SECTION 6 — GOAL 1: GFW vs CV COMPARISON
# ============================================================
# Step 6a — GFW side: all CV vessels, all divisions, all years
# We match vessels via the vessels_cv lookup (mmsi → vessel code)

gfw_cv_days <-
  events_with_division %>%
  mutate(
    date = as.Date(start),
    year = year(date)
  ) %>%
  filter(vessel_ssvid %in% vessels_cv$mmsi) %>%
  left_join(
    vessels_cv %>% dplyr::select(mmsi, vessel),   # vessel = short vessel code e.g. Z19
    by = c("vessel_ssvid" = "mmsi")
  ) %>%
  filter(!is.na(vessel)) %>%
  distinct(vessel, year, division, date) %>%       # one row per fishing day
  group_by(year, vessel, division) %>%
  summarise(days_at_sea = n(), .groups = "drop") %>%
  mutate(source = "GFW")

# Step 6b — CV (logbook) side
# Assumes elog_existing has columns: vessel, date, fao_division
# Adjust column names below if they differ in your data.
#
# load(file.path(flyshootdir, "elog_existing.RData"))   # <-- uncomment if needed

cv_days <-
  elog_existing %>%
  rename_with(tolower) %>%                              # defensive: lowercase all names
  mutate(
    division = toupper(fao_division),
    date     = as.Date(date),
    year     = year(date)
  ) %>%
  filter(division %in% target_divisions) %>%
  distinct(vessel, year, division, date) %>%
  group_by(year, vessel, division) %>%
  summarise(days_at_sea = n(), .groups = "drop") %>%
  mutate(source = "CV")

# Step 6c — Bind and create comparison table
comparison_long <-
  bind_rows(gfw_cv_days, cv_days)

# Wide format: one row per vessel × year × division, columns for each source
comparison_wide <-
  comparison_long %>%
  pivot_wider(
    names_from  = source,
    values_from = days_at_sea,
    values_fill = 0
  ) %>%
  mutate(
    diff_abs = GFW - CV,
    diff_pct = if_else(CV > 0, round((GFW - CV) / CV * 100, 1), NA_real_),
    ratio    = if_else(CV > 0, round(GFW / CV, 2), NA_real_)
  ) %>%
  arrange(vessel, year, division)

print(comparison_wide, n = 40)

# Summary statistics across all vessel-year-division combinations
comparison_stats <-
  comparison_wide %>%
  filter(CV > 0, GFW > 0) %>%
  summarise(
    n               = n(),
    mean_diff_abs   = round(mean(diff_abs), 1),
    median_diff_abs = round(median(diff_abs), 1),
    mean_ratio      = round(mean(ratio), 2),
    pct_GFW_higher  = round(mean(diff_abs > 0) * 100, 1),
    pct_CV_higher   = round(mean(diff_abs < 0) * 100, 1)
  )

print(comparison_stats)

# ============================================================
# SECTION 7 — PLOTS
# ============================================================

# ---- Plot 1: GFW fishing days by year and flag (absolute) ----
gfw_days %>%
  group_by(year, vessel_flag) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  ggplot(aes(x = year, y = days_at_sea, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Days at sea", fill = "Flag",
       title = "GFW apparent fishing days at sea by year and country")

# ---- Plot 2: GFW days by year and flag, faceted by division ----
gfw_days %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  ggplot(aes(x = year, y = days_at_sea, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Days at sea", fill = "Flag",
       title = "GFW apparent fishing days at sea by year, country and division") +
  facet_wrap(~ division)

# ---- Plot 3: GFW days by year and flag (%) ----
gfw_days %>%
  group_by(year, vessel_flag) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = days_at_sea / sum(days_at_sea) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Days at sea", fill = "Flag",
       title = "GFW fishing days at sea by year and country (%)")

# ---- Plot 4: GFW days by year, flag and division (%) ----
gfw_days %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  group_by(year, division) %>%
  mutate(pct = days_at_sea / sum(days_at_sea) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Days at sea", fill = "Flag",
       title = "GFW fishing days at sea by year, country and division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: GFW days by year and gear type, faceted by division ----
gfw_days %>%
  filter(!is.na(division)) %>%
  group_by(year, gear, division) %>%
  summarise(days_at_sea = sum(days_at_sea), .groups = "drop") %>%
  ggplot(aes(x = year, y = days_at_sea, fill = gear)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Days at sea", fill = "Gear type",
       title = "GFW fishing days at sea by year, gear type and division") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 6: GFW vs CV days at sea by vessel, year and division ----
comparison_long %>%
  ggplot(aes(x = year, y = days_at_sea, colour = source, shape = source)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Days at sea", colour = "Source", shape = "Source",
       title = "GFW vs CV (logbook) days at sea by vessel, year and division") +
  facet_grid(division ~ vessel)

# ---- Plot 7: Absolute difference (GFW − CV) by vessel and division ----
comparison_wide %>%
  filter(CV > 0 | GFW > 0) %>%
  ggplot(aes(x = year, y = diff_abs, fill = diff_abs > 0)) +
  theme_bw() +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d6604d")) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "GFW − CV (days)", 
       title = "Difference in days at sea: GFW minus CV logbook") +
  facet_grid(division ~ vessel)

# ---- Plot 8: Scatter GFW vs CV with 1:1 line ----
comparison_wide %>%
  filter(CV > 0, GFW > 0) %>%
  ggplot(aes(x = CV, y = GFW, colour = vessel, label = year)) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(size = 2.5, nudge_y = 0.5, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "CV logbook days at sea", y = "GFW days at sea",
       colour = "Vessel",
       title = "GFW vs CV days at sea (1:1 line = perfect agreement)") +
  facet_wrap(~ division)

# ============================================================
# SECTION 8 — COVERAGE DIAGNOSTICS: GFW vs CV
# ============================================================
# Goal: identify gaps — years/divisions present in one source but not the other,
# and years where both are present but numbers diverge strongly.

# ---- 8a: Coverage status per vessel × year × division ----
# Classify each combination into one of four states:
#   "Both"      — present in GFW and CV (the ideal case)
#   "GFW only"  — GFW has data, CV does not (possible AIS coverage without logbook)
#   "CV only"   — CV has data, GFW does not (vessel fished but AIS not detected)
#   "Neither"   — neither source has data for this vessel/year/division

# Build the full grid of all vessel × year × division combinations that appear
# in at least one source, then classify
all_years     <- sort(unique(c(gfw_cv_days$year,  cv_days$year)))
all_vessels   <- sort(unique(c(gfw_cv_days$vessel, cv_days$vessel)))
all_divisions <- target_divisions

coverage_grid <-
  expand.grid(
    vessel   = all_vessels,
    year     = all_years,
    division = all_divisions,
    stringsAsFactors = FALSE
  ) %>%
  as_tibble() %>%
  left_join(
    gfw_cv_days %>% dplyr::select(vessel, year, division, days_gfw = days_at_sea),
    by = c("vessel", "year", "division")
  ) %>%
  left_join(
    cv_days %>% dplyr::select(vessel, year, division, days_cv = days_at_sea),
    by = c("vessel", "year", "division")
  ) %>%
  mutate(
    coverage = case_when(
      !is.na(days_gfw) & !is.na(days_cv) ~ "Both",
      !is.na(days_gfw) &  is.na(days_cv) ~ "GFW only",
       is.na(days_gfw) & !is.na(days_cv) ~ "CV only",
      TRUE                                ~ "Neither"
    ),
    coverage = factor(coverage, levels = c("Both", "GFW only", "CV only", "Neither"))
  )

# ---- 8b: Coverage summary table ----
# How many vessel × year × division cells fall into each category, by division
coverage_summary <-
  coverage_grid %>%
  filter(coverage != "Neither") %>%        # only rows where at least one source has data
  count(division, coverage) %>%
  pivot_wider(names_from = coverage, values_from = n, values_fill = 0) %>%
  arrange(division)

print(coverage_summary)

# ---- 8c: Vessels with gaps — which vessels have CV-only or GFW-only years ----
coverage_gaps <-
  coverage_grid %>%
  filter(coverage %in% c("GFW only", "CV only")) %>%
  count(vessel, division, coverage) %>%
  pivot_wider(names_from = coverage, values_from = n, values_fill = 0) %>%
  arrange(vessel, division)

print(coverage_gaps, n = 40)

# ---- 8d: Plot — coverage heatmap by vessel, year and division ----
# Each cell shows the coverage status; "Neither" cells are left blank to reduce noise
coverage_colours <- c(
  "Both"     = "#2166ac",   # blue   — ideal
  "GFW only" = "#f4a582",   # orange — GFW without logbook
  "CV only"  = "#d6604d",   # red    — logbook without GFW
  "Neither"  = "grey92"     # light grey — no data
)

coverage_grid %>%
  ggplot(aes(x = year, y = vessel, fill = coverage)) +
  theme_bw() +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = coverage_colours) +
  scale_x_continuous(breaks = all_years) +
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 7),
    axis.text.y  = element_text(size = 7),
    strip.text   = element_text(size = 8),
    legend.position = "bottom"
  ) +
  labs(
    x = "", y = "", fill = "Coverage",
    title = "Data coverage: GFW vs CV logbook by vessel, year and division"
  ) +
  facet_wrap(~ division, ncol = 3)

# ---- 8e: Divergence diagnostic — where both sources agree they fished,
#          but the day counts differ by more than 30% ----
divergence <-
  comparison_wide %>%
  filter(GFW > 0, CV > 0) %>%
  mutate(
    ratio      = GFW / CV,
    divergence = case_when(
      ratio > 1.3  ~ "GFW > 30% higher",
      ratio < 0.77 ~ "CV > 30% higher",
      TRUE         ~ "Within 30%"
    ),
    divergence = factor(divergence,
                        levels = c("GFW > 30% higher", "Within 30%", "CV > 30% higher"))
  )

# Summary: how often does each vessel diverge?
divergence %>%
  count(vessel, divergence) %>%
  pivot_wider(names_from = divergence, values_from = n, values_fill = 0) %>%
  arrange(vessel) %>%
  print()

# Plot: divergence heatmap — same structure as coverage but showing agreement level
divergence_colours <- c(
  "GFW > 30% higher" = "#2166ac",
  "Within 30%"       = "#a8d8a8",
  "CV > 30% higher"  = "#d6604d"
)

divergence %>%
  ggplot(aes(x = year, y = vessel, fill = divergence)) +
  theme_bw() +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = divergence_colours) +
  scale_x_continuous(breaks = all_years) +
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 7),
    axis.text.y  = element_text(size = 7),
    strip.text   = element_text(size = 8),
    legend.position = "bottom"
  ) +
  labs(
    x = "", y = "", fill = "Agreement",
    title = "GFW vs CV agreement where both sources have data (>30% threshold)",
    subtitle = "Blue = GFW higher, green = within 30%, red = CV higher"
  ) +
  facet_wrap(~ division, ncol = 3)

# ============================================================
# SECTION 9 — WITHIN-YEAR TEMPORAL DIAGNOSTIC
# ============================================================
# For vessels/divisions where both sources show data but counts diverge,
# break down to monthly resolution to reveal timing mismatches.

# ---- 9a: Monthly day counts from GFW for CV vessels ----
gfw_monthly <-
  events_with_division %>%
  mutate(
    date  = as.Date(start),
    year  = year(date),
    month = month(date)
  ) %>%
  filter(vessel_ssvid %in% vessels_cv$mmsi) %>%
  left_join(
    vessels_cv %>% dplyr::select(mmsi, vessel),
    by = c("vessel_ssvid" = "mmsi")
  ) %>%
  filter(!is.na(vessel), !is.na(division)) %>%
  distinct(vessel, year, month, division, date) %>%
  group_by(vessel, year, month, division) %>%
  summarise(days_at_sea = n(), .groups = "drop") %>%
  mutate(source = "GFW")

# ---- 9b: Monthly day counts from CV logbooks ----
cv_monthly <-
  elog_existing %>%
  rename_with(tolower) %>%
  mutate(
    division = toupper(fao_division),
    date     = as.Date(date),
    year     = year(date),
    month    = month(date)
  ) %>%
  filter(division %in% target_divisions) %>%
  distinct(vessel, year, month, division, date) %>%
  group_by(vessel, year, month, division) %>%
  summarise(days_at_sea = n(), .groups = "drop") %>%
  mutate(source = "CV")

# ---- 9c: Focus plot — monthly breakdown for a specific vessel × division ----
# Edit `focus_vessel` and `focus_division` to investigate any combination.
# Start with the two flagged cases: SL9/27.7.D and SCH65/27.7.D

plot_monthly_comparison <- function(focus_vessel, focus_division) {
  bind_rows(gfw_monthly, cv_monthly) %>%
    filter(vessel == focus_vessel, division == focus_division) %>%
    mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    ggplot(aes(x = month_date, y = days_at_sea, colour = source, shape = source)) +
    theme_bw() +
    geom_point(size = 1.8) +
    geom_line(linewidth = 0.4, alpha = 0.7) +
    scale_colour_manual(values = c("GFW" = "#2166ac", "CV" = "#d6604d")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)) +
    labs(
      x = "", y = "Days at sea", colour = "Source", shape = "Source",
      title = paste("Monthly days at sea —", focus_vessel, "in", focus_division),
      subtitle = "Gaps in one source while the other has data reveal timing mismatches"
    )
}

plot_monthly_comparison("SL9",   "27.7.D")
plot_monthly_comparison("SCH65", "27.7.D")

# ---- 9d: Monthly coverage heatmap — shows which months each source has data ----
# Useful for spotting systematic seasonal gaps (e.g. GFW always missing Q1)

plot_monthly_coverage <- function(focus_vessel, focus_division) {

  all_year_months <-
    expand.grid(
      year  = 2012:2025,
      month = 1:12,
      stringsAsFactors = FALSE
    ) %>%
    as_tibble()

  gfw_present <- gfw_monthly %>%
    filter(vessel == focus_vessel, division == focus_division) %>%
    dplyr::select(year, month) %>%
    mutate(gfw = TRUE)

  cv_present <- cv_monthly %>%
    filter(vessel == focus_vessel, division == focus_division) %>%
    dplyr::select(year, month) %>%
    mutate(cv = TRUE)

  all_year_months %>%
    left_join(gfw_present, by = c("year", "month")) %>%
    left_join(cv_present,  by = c("year", "month")) %>%
    # coalesce NA to FALSE so that missing joins are treated as "no data"
    # rather than propagating NA through the & and ! operators
    mutate(
      gfw = coalesce(gfw, FALSE),
      cv  = coalesce(cv,  FALSE)
    ) %>%
    mutate(
      coverage = case_when(
        gfw & cv   ~ "Both",
        gfw & !cv  ~ "GFW only",
        !gfw & cv  ~ "CV only",
        TRUE       ~ "Neither"
      ),
      coverage = factor(coverage, levels = c("Both", "GFW only", "CV only", "Neither")),
      month_label = month.abb[month]
    ) %>%
    mutate(month_label = factor(month_label, levels = month.abb)) %>%
    ggplot(aes(x = factor(year), y = month_label, fill = coverage)) +
    theme_bw() +
    geom_tile(colour = "white", linewidth = 0.4) +
    scale_fill_manual(values = c(
      "Both"     = "#2166ac",
      "GFW only" = "#f4a582",
      "CV only"  = "#d6604d",
      "Neither"  = "grey92"
    )) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)) +
    labs(
      x = "", y = "", fill = "Coverage",
      title = paste("Monthly coverage heatmap —", focus_vessel, "in", focus_division)
    )
}

plot_monthly_coverage("SL9",   "27.7.D")
plot_monthly_coverage("SCH65", "27.7.D")

# ---- 9e: Annual mismatch summary for the two focus vessels ----
# Side-by-side annual totals with the monthly overlap rate
bind_rows(gfw_monthly, cv_monthly) %>%
  filter(
    vessel   %in% c("SL9", "SCH65"),
    division == "27.7.D"
  ) %>%
  group_by(vessel, year, source) %>%
  summarise(
    days_at_sea   = sum(days_at_sea),
    months_active = n_distinct(month),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = source,
    values_from = c(days_at_sea, months_active),
    values_fill = 0
  ) %>%
  mutate(
    ratio             = if_else(days_at_sea_CV > 0, round(days_at_sea_GFW / days_at_sea_CV, 2), NA_real_),
    month_overlap     = pmin(months_active_GFW, months_active_CV),
    months_gfw_extra  = months_active_GFW - month_overlap,
    months_cv_extra   = months_active_CV  - month_overlap
  ) %>%
  arrange(vessel, year) %>%
  print(n = 40)

# ============================================================
# SECTION 10 — SPATIAL COMPARISON: GFW vs CV BY ICES RECTANGLE
# ============================================================
# ICES statistical rectangles are 1° longitude × 0.5° latitude.
# Naming convention: row letter (A-M, south to north, starting at 36°N)
#   + column number (00-99, starting at 44°W).
# We derive the rectangle from coordinates for GFW, and use the
# rectangle field directly from the CV logbook if available,
# otherwise derive it from CV lat/lon.

# ICES rectangle helper functions — taken directly from mapplots::ices.rect
# (Hans Gerritsen, https://cran.r-project.org/package=mapplots)
#
# ices.rect2: coordinates -> rectangle name
ices_rectangle <- function(lon, lat) {
  x    <- floor(lon + 60) + 1000
  y    <- floor(lat * 2) - 71 + 100
  num1 <- substr(y, 2, 3)
  lett <- LETTERS[as.numeric(substr(x, 2, 3))]
  num2 <- substr(x, 4, 4)
  paste0(num1, lett, num2)
}

# ices.rect: rectangle name -> coordinates (midpoint)
ices_rect_to_coords <- function(rectangle) {
  if (is.factor(rectangle)) rectangle <- as.character(rectangle)
  get_lat <- function(r) {
    sp <- unlist(strsplit(as.character(r), ""))
    if (sp[1] %in% 0:9 & sp[2] %in% 0:9 & sp[3] %in% LETTERS & sp[4] %in% 0:9)
      ry <- as.numeric(substr(r, 1, 2))
    else
      ry <- as.numeric(r) / 10^floor(log10(as.numeric(r)) - 1)
    (ry + 71.5) / 2
  }
  get_lon <- function(r) {
    sp <- unlist(strsplit(as.character(r), ""))
    if (sp[1] %in% 0:9 & sp[2] %in% 0:9 & sp[3] %in% LETTERS & sp[4] %in% 0:9)
      rx <- as.numeric(paste0(match(sp[3], LETTERS), sp[4]))
    else
      rx <- floor(log10(as.numeric(r)) - 1) + 50
    rx - 59.5
  }
  data.frame(
    lon_ctr = unlist(lapply(rectangle, get_lon)),
    lat_ctr = unlist(lapply(rectangle, get_lat))
  )
}

# ---- 10a: GFW rectangles for SCH65 ----
gfw_rect_sch65 <-
  events_with_division %>%
  filter(vessel_ssvid %in% sch65_mmsi) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    year     = year(as.Date(start)),
    ices_rect = ices_rectangle(lon, lat)
  ) %>%
  distinct(year, ices_rect, date = as.Date(start)) %>%
  group_by(year, ices_rect) %>%
  summarise(days_at_sea = n(), .groups = "drop") %>%
  mutate(source = "GFW")

# ---- 10b: CV rectangles for SCH65 ----
# Assumes elog_existing has an `icesrectangle` column (or similar).
# Check your column names and adjust the rename below if needed.
# If CV only has lat/lon rather than a rectangle code, we derive it.

cv_rect_sch65 <-
  elog_existing %>%
  rename_with(tolower) %>%
  filter(vessel == "SCH65") %>%
  mutate(
    date      = as.Date(date),
    year      = year(date),
    ices_rect = toupper(ices_rect)
  ) %>%
  distinct(year, ices_rect, date) %>%
  group_by(year, ices_rect) %>%
  summarise(days_at_sea = n(), .groups = "drop") %>%
  mutate(source = "CV")

# ---- 10c: Combine and compute rectangle centroids for plotting ----
# CV uses standard ICES number-first convention e.g. "31F2":
#   first 2 chars = row number (latitude band, 01 = 36-36.5N)
#   last 1-2 chars = column letter (A = 40W, B = 39W ... )
# Centroid: lat = (row_number - 1) * 0.5 + 36 + 0.25
#            lon = (match(col_letter, LETTERS) - 1) - 40 + 0.5
#
# GFW side: derive centroids directly from event lat/lon (already available),
# avoiding the round-trip through rectangle codes which introduced NAs.

# ices_rect_to_coords is defined above alongside ices_rectangle

# GFW: aggregate by ICES rectangle but carry mean lat/lon for centroid
gfw_rect_sch65 <-
  events_with_division %>%
  filter(vessel_ssvid %in% sch65_mmsi) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    year      = year(as.Date(start)),
    date      = as.Date(start),
    ices_rect = ices_rectangle(lon, lat)
  ) %>%
  filter(!is.na(ices_rect), !grepl("^NA", ices_rect)) %>%
  distinct(year, ices_rect, date, lat, lon) %>%
  group_by(year, ices_rect) %>%
  summarise(
    days_at_sea = n_distinct(date),
    lat_ctr     = mean(lat, na.rm = TRUE),
    lon_ctr     = mean(lon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(source = "GFW")

# CV: derive centroids from rectangle code using mapplots formula
cv_rect_sch65_coords <-
  cv_rect_sch65 %>%
  filter(!is.na(ices_rect), nchar(ices_rect) == 4) %>%
  mutate(
    lat_ctr = ices_rect_to_coords(ices_rect)$lat_ctr,
    lon_ctr = ices_rect_to_coords(ices_rect)$lon_ctr
  )

gfw_rect_sch65_coords <- gfw_rect_sch65

rect_coords <-
  bind_rows(
    gfw_rect_sch65_coords,
    cv_rect_sch65_coords
  )

# ---- 10d: Dot map — size = days at sea, faceted by year and source ----
focus_years <- 2019:2025   # adjust range as needed

rect_coords %>%
  filter(year %in% focus_years) %>%
  ggplot(aes(x = lon_ctr, y = lat_ctr, size = days_at_sea, colour = source)) +
  theme_bw() +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 8) +
  scale_colour_manual(values = c("GFW" = "#2166ac", "CV" = "#d6604d")) +
  coord_quickmap(xlim = c(-5, 6), ylim = c(49, 55)) +
  facet_grid(source ~ year) +
  theme(
    axis.text   = element_text(size = 6),
    strip.text  = element_text(size = 7),
    legend.position = "bottom"
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    size = "Days at sea", colour = "Source",
    title = "SCH65 — fishing effort by ICES rectangle: GFW vs CV",
    subtitle = "Each bubble = one ICES rectangle; size = days at sea"
  )

# ---- 10e: Rectangle-level comparison table for 2022-2024 ----
bind_rows(gfw_rect_sch65, cv_rect_sch65) %>%
  filter(year %in% 2022:2024) %>%
  pivot_wider(names_from = source, values_from = days_at_sea, values_fill = 0) %>%
  mutate(diff = GFW - CV) %>%
  arrange(year, ices_rect) %>%
  print(n = 50)

# ============================================================
# SECTION 11 — VESSEL IDENTITY TIMELINE CHART
# ============================================================
# One row per MMSI, horizontal bars showing each vesselId period
# with vessel name, flag and gear annotated.
# Intended as a talking piece for expert review.

vessel_timeline <-
  vessel_ids_all %>%
  dplyr::select(vesselId, ssvid, shipname, transmissionDateFrom, transmissionDateTo,
                mmsi_queried) %>%
  left_join(
    vessel_info %>% dplyr::select(vesselId, vessel_flag = flag, geartypes),
    by = "vesselId"
  ) %>%
  left_join(
    vessels %>% dplyr::select(mmsi, vessel_code = vessel, vessel_flag_list = flag, gear),
    by = c("ssvid" = "mmsi")
  ) %>%
  left_join(
    vessels_cv %>% dplyr::select(mmsi, vessel_code_cv = vessel),
    by = c("ssvid" = "mmsi")
  ) %>%
  mutate(
    vessel_code  = coalesce(vessel_code, vessel_code_cv),
    vessel_flag  = coalesce(vessel_flag, vessel_flag_list),
    gear         = coalesce(geartypes, gear),
    date_from    = as.Date(substr(transmissionDateFrom, 1, 10)),
    date_to      = as.Date(substr(transmissionDateTo,   1, 10)),
    # Label shown inside/beside each bar
    bar_label    = case_when(
      !is.na(shipname) & !is.na(vessel_flag) ~ paste0(shipname, " (", vessel_flag, ")"),
      !is.na(shipname)                       ~ shipname,
      TRUE                                   ~ vesselId
    ),
    # Y-axis label: vessel code + MMSI
    mmsi_label   = if_else(
      !is.na(vessel_code),
      paste0(vessel_code, "\n", ssvid),
      ssvid
    )
  ) %>%
  # Determine the most recent flag per MMSI (used for sorting)
  # then sort by: most recent flag → vessel_code → ssvid
  group_by(ssvid) %>%
  mutate(recent_flag = vessel_flag[which.max(date_to)]) %>%
  ungroup() %>%
  arrange(recent_flag, vessel_code, ssvid, date_from) %>%
  mutate(mmsi_label = factor(mmsi_label, levels = unique(mmsi_label)))

# Colour by flag — named palette covering all flags present
flag_levels  <- sort(unique(na.omit(vessel_timeline$vessel_flag)))
flag_colours <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(min(length(flag_levels), 8), "Set1"))(length(flag_levels)),
  flag_levels
)

# Split into pages of 20 rows each for readable font sizes
rows_per_page <- 20
all_labels    <- levels(vessel_timeline$mmsi_label)
n_pages       <- ceiling(length(all_labels) / rows_per_page)

plot_timeline_page <- function(page) {

  row_start  <- (page - 1) * rows_per_page + 1
  row_end    <- min(page * rows_per_page, length(all_labels))
  page_labels <- all_labels[row_start:row_end]

  df_page <- vessel_timeline %>%
    filter(mmsi_label %in% page_labels) %>%
    mutate(mmsi_label = factor(mmsi_label, levels = page_labels))

  # Only label bars that are wide enough to hold text without overlapping
  # Threshold: 365 days. Narrow bars get no text — identity readable from y-axis.
  df_labelled <- df_page %>%
    mutate(
      bar_width_days = as.numeric(date_to - date_from),
      label_text     = if_else(bar_width_days >= 365, bar_label, "")
    )

  ggplot(df_labelled,
         aes(xmin = date_from, xmax = date_to,
             ymin = as.numeric(mmsi_label) - 0.4,
             ymax = as.numeric(mmsi_label) + 0.4,
             fill = vessel_flag)) +
    theme_bw() +
    geom_rect(colour = "white", linewidth = 0.4) +
    geom_text(
      aes(x = date_from + (date_to - date_from) / 2,
          y = as.numeric(mmsi_label),
          label = label_text),
      size = 2.8, colour = "white", fontface = "bold",
      hjust = 0.5, vjust = 0.5
    ) +
    scale_fill_manual(values = flag_colours, na.value = "grey70") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 limits = as.Date(c("2012-01-01", "2026-12-31"))) +
    scale_y_continuous(
      breaks = seq_along(page_labels),
      labels = page_labels,
      expand = expansion(add = 0.6)
    ) +
    theme(
      axis.text.y        = element_text(size = 9, family = "mono"),
      axis.text.x        = element_text(size = 9),
      axis.title         = element_blank(),
      panel.grid.major.x = element_line(colour = "grey85"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom"
    ) +
    labs(
      fill     = "Flag",
      title    = paste0("Vessel identity timeline (", page, "/", n_pages, ")"),
      subtitle = "Each bar = one GFW vesselId period  |  rows grouped by vessel code then MMSI"
    )
}

# Print all pages — in RStudio each appears as a separate plot in the Plots pane.
# Each timeline page is followed by a companion table listing all vesselId periods
# on that page, so narrow-bar labels that are suppressed are still readable.

for (p in seq_len(n_pages)) {

  print(plot_timeline_page(p))

  # Companion table for this page
  row_start   <- (p - 1) * rows_per_page + 1
  row_end     <- min(p * rows_per_page, length(all_labels))
  page_labels <- all_labels[row_start:row_end]

  tbl <- vessel_timeline %>%
    filter(mmsi_label %in% page_labels) %>%
    mutate(
      period = paste0(format(date_from, "%Y-%m"), " – ", format(date_to, "%Y-%m"))
    ) %>%
    dplyr::select(
      `Vessel code` = vessel_code,
      MMSI          = ssvid,
      `Vessel name` = shipname,
      Flag          = vessel_flag,
      Gear          = gear,
      Period        = period
    ) %>%
    arrange(`Vessel code`, MMSI, Period)

  # Print as a formatted table using knitr if available, otherwise just print
  if (requireNamespace("knitr", quietly = TRUE)) {
    print(knitr::kable(tbl, format = "simple", caption = paste0("Page ", p, " vessel details")))
  } else {
    print(tbl, n = nrow(tbl))
  }
}

