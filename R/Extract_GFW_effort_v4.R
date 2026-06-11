# ============================================================
# GFW Effort Analysis — gfwr v3.0
# Version 4 — cleaned and consolidated
#
# Goals:
#   1. Extract fishing effort (days at sea) for all vessels
#      active in ICES divisions 27.4.C, 27.7.D, 27.7.E
#   2. Filter to marine-only events (exclude internal waters)
#   3. Classify events as coastal (0-12NM) or offshore (>12NM)
#   4. Summarise by year / quarter / flag / gear / division / zone
#   5. Diagnostics on data completeness
#   6. Visualise effort trends and vessel coverage
#
# Processing steps:
#   STEP 1  — Setup: libraries, paths, spatial layers
#   STEP 2  — Vessel lists: load CSVs → master MMSI list
#   STEP 3  — GFW vessel info: MMSI → vesselId + metadata
#             [save: gfw_vessel_registry.RData]
#   STEP 4  — Fishing events: pull 2012-2025 per vesselId
#             [save: gfw_fishing_events_raw.RData]
#   STEP 5  — Spatial join: events → ICES division
#   STEP 6  — Marine filter: exclude internal waters + EEZ filter
#   STEP 7  — Coastal zone: classify events as coastal/offshore
#   STEP 8  — Vessel metadata: build vessel_meta lookup
#   STEP 9  — Fishing days: aggregate to day-level effort
#             [save: gfw_effort_analysis.RData]
#   STEP 10 — Diagnostics: completeness checks
#   STEP 11 — Vessel coverage chart (PDF)
#   STEP 12 — Effort plots
#
# Saved RData files:
#   gfw_vessel_registry.RData  — MMSI → vesselId lookup + gear/GT
#   gfw_fishing_events_raw.RData — raw fishing events from GFW API
#   gfw_effort_analysis.RData  — vessel_meta, fishing_days, fishing_days_study
#
# To skip slow steps on re-runs, load the relevant RData file and
# jump directly to the step you need. See load() comments in each step.
# ============================================================


# ============================================================
# STEP 1 — Setup
# ============================================================

library(tidyverse)
library(lubridate)
library(gfwr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

key <- gfw_auth()

spatialdir  <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"
gisdir      <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/GIS"

source("R/FLYSHOOT utils.r")

# ---- Spatial layers ----

# Base map for plots
world <- ne_countries(scale = "medium", returnclass = "sf")

# ICES division polygons — all divisions needed for spatial join
target_divisions <- c("27.4.A", "27.4.B", "27.4.C", "27.7.D", "27.7.E")

fao_sf <- loadRData(file.path(spatialdir, "fao_sf.RData"))
fao_sf_division <-
  fao_sf %>%
  filter(F_LEVEL == "DIVISION") %>%
  dplyr::select(division = F_DIVISION) %>%
  mutate(division = toupper(division)) %>%
  filter(division %in% target_divisions) %>%
  group_by(division) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

# Study divisions — subset used for all analysis
study_divisions <- c("27.4.C", "27.7.D", "27.7.E")

# EEZ shapefile — for marine filtering (excludes inland waters implicitly)
eez_sf <- loadRData(file.path(spatialdir, "eez_sf.RData"))

# Internal waters — Marineregions World Internal Waters v4
# Download from: https://www.marineregions.org/downloads.php

# sf_use_s2(FALSE)
# internal_waters <- st_read(
#   file.path(gisdir, "World_Internal_Waters_v4_20231025/eez_internal_waters_v4.shp")
# ) %>%
#   st_make_valid() %>%
#   st_crop(c(xmin = -10, ymin = 48, xmax = 10, ymax = 58))
# sf_use_s2(TRUE)
# save(internal_waters, file=file.path(spatialdir, "internal_waters.RData"))
load(file.path(spatialdir, "internal_waters.RData"))

# Territorial sea — Marineregions World 12NM Zone v4
# Download from: https://www.marineregions.org/downloads.php
# Used to classify events as coastal (0-12NM) vs offshore (>12NM)

# sf_use_s2(FALSE)
# territorial_sea <- st_read(
#   file.path(gisdir, "World_12NM_v4_20231025/eez_12nm_v4.shp")
# ) %>%
#   st_make_valid() %>%
#   st_crop(c(xmin = -10, ymin = 48, xmax = 10, ymax = 58)) %>%
#   st_make_valid()
# sf_use_s2(TRUE)
# save(territorial_sea, file=file.path(spatialdir, "territorial_sea.RData"))
load(file.path(spatialdir, "territorial_sea.RData"))

# EEZ filter — relevant countries in study area
study_eez <- eez_sf %>%
  dplyr::select(eez = ISO_SOV1, geometry) %>%
  filter(eez %in% c("GBR", "NLD", "BEL", "FRA", "DEU", "DNK",
                    "NOR", "IRL", "ESP"))

# ---- Plot settings ----

# Muted Tableau-style colour palette — consistent across all plots
flag_colours <- c(
  "BEL"   = "#4E79A7",   # steel blue
  "NLD"   = "#F28E2B",   # warm orange
  "GBR"   = "#59A14F",   # muted green
  "FRA"   = "#E15759",   # muted red
  "DEU"   = "#76B7B2",   # teal
  "DNK"   = "#EDC948",   # gold
  "NOR"   = "#B07AA1",   # muted purple
  "IRL"   = "#FF9DA7",   # soft pink
  "Other" = "#BAB0AC"    # neutral grey
)

# Threshold for "Other" grouping in plots (% of total days)
threshold_pct <- 1

# Map bounding box
xlim <- c(-10, 8)
ylim <- c(48, 56)


# ============================================================
# STEP 2 — Vessel lists
# ============================================================
# Four CSVs capture different time periods to include vessels
# that may have entered or left the fishery over time.
# Combined into one master list of unique MMSIs.

vessel_csvs <- list.files(
  path       = flyshootdir,
  pattern    = "^fishing_vessels_27\\.4\\.c_27\\.7\\.d_27\\.7\\.e_.*\\.csv$",
  full.names = TRUE
)
message("Found ", length(vessel_csvs), " CSV files")

mmsi_csv_raw <- map_dfr(vessel_csvs, function(f) {
  read_csv(f, col_types = cols(mmsi = col_character())) %>%
    mutate(source_file = basename(f))
})

# One row per MMSI — best available metadata, n_events summed across periods
mmsi_master <- mmsi_csv_raw %>%
  group_by(mmsi) %>%
  summarise(
    vessel_name = first(na.omit(vessel_name)),
    flag        = first(na.omit(flag)),
    vessel_type = first(na.omit(vessel_type)),
    n_events    = sum(n_events, na.rm = TRUE),
    n_periods   = n(),
    .groups     = "drop"
  ) %>%
  arrange(flag, vessel_name)

message("Master vessel list: ", nrow(mmsi_master), " unique MMSIs")
message("Flags: ", paste(sort(unique(na.omit(mmsi_master$flag))), collapse = ", "))

# CV vessel list — for GFW vs logbook comparison
vessels_cv <- readxl::read_excel(
  file.path(flyshootdir, "flyshoot vessels CV.xlsx")) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi))


# ============================================================
# STEP 3 — GFW vessel info: MMSI → vesselId + metadata
# ============================================================
# Single API call per MMSI returns ALL vesselId periods
# (selfReportedInfo) plus registry metadata (gear, GT, length).
#
# To update registry without re-running events extraction:
#   Run Steps 1-3, save, then load gfw_fishing_events_raw.RData
#   and continue from Step 5.
#
# To skip: load(file.path(flyshootdir, "gfw_vessel_registry.RData"))

n_total_mmsi <- nrow(mmsi_master)

# gfw_vessel_registry <- map_dfr(seq_along(mmsi_master$mmsi), function(i) {
#   m <- mmsi_master$mmsi[i]
#   if (i %% 50 == 0) message(sprintf("[%d/%d] querying MMSI %s", i, n_total_mmsi, m))
#   tryCatch({
#     res <- gfw_vessel_info(query = m, search_type = "search", key = key)
# 
#     # Safely extract registry info — handle empty or missing columns
#     ri <- if (!is.null(res$registryInfo) && nrow(res$registryInfo) > 0 &&
#               "latestVesselInfo" %in% names(res$registryInfo)) {
#       res$registryInfo %>%
#         arrange(desc(latestVesselInfo)) %>%
#         slice(1) %>%
#         dplyr::select(any_of(c("geartypes", "tonnageGt", "lengthM")))
#     } else {
#       tibble(geartypes = NA_character_, tonnageGt = NA_real_, lengthM = NA_real_)
#     }
# 
#     res$selfReportedInfo %>%
#       mutate(mmsi_queried = m) %>%
#       bind_cols(ri)
# 
#   }, error = function(e) {
#     message("Failed MMSI: ", m, " — ", e$message)
#     NULL
#   })
# })
# 
# # Check for missing MMSIs
# n_missing <- mmsi_master %>%
#   anti_join(gfw_vessel_registry, by = c("mmsi" = "mmsi_queried")) %>%
#   nrow()
# message(n_missing, " MMSIs absent from gfw_vessel_registry (should be 0)")
# 
# # Derive downstream objects
# gfw_vessel_periods <- gfw_vessel_registry %>%
#   dplyr::select(vesselId, ssvid, shipname,
#                 transmissionDateFrom, transmissionDateTo, mmsi_queried)
# 
# gfw_vessel_ids <- unique(gfw_vessel_periods$vesselId)
# 
# message(n_distinct(gfw_vessel_periods$vesselId), " unique vesselIds for ",
#         n_distinct(gfw_vessel_periods$ssvid), " unique MMSIs")

# Save — skip Step 3 on future runs by loading this file
# save(gfw_vessel_registry, gfw_vessel_periods, gfw_vessel_ids,
#      file = file.path(flyshootdir, "gfw_vessel_registry.RData"))

load(file.path(flyshootdir, "gfw_vessel_registry.RData"))


# ============================================================
# STEP 4 — Fishing events: pull 2012-2025 per vesselId
# ============================================================
# GFW Events API is capped at 366 days per call → loop by year.
# Takes ~30s per vesselId × 14 years → plan for 1+ day runtime.


# Check what's already done
completed_ids <- unique(events_df$vesselId)
remaining_ids <- setdiff(gfw_vessel_ids, completed_ids)

message("Already completed: ", length(completed_ids))
message("Still to run:      ", length(remaining_ids))

# Estimated runtime at ~30s per vesselId × 14 years
est_hours <- length(remaining_ids) * 30 / 3600
message("Estimated time: ", round(est_hours, 1), " hours")

# To RESUME an interrupted run (all_events still in memory):
#   completed_ids <- names(Filter(Negate(is.null), all_events))
#   remaining_ids <- setdiff(gfw_vessel_ids, completed_ids)
#
# To START FRESH:
#   remaining_ids <- gfw_vessel_ids
#
# To SKIP entirely:
#   load(file.path(flyshootdir, "gfw_fishing_events_raw.RData"))

# all_events    <- vector("list", length(gfw_vessel_ids))
# names(all_events) <- gfw_vessel_ids
# remaining_ids <- gfw_vessel_ids   # replace with setdiff() to resume
# n_total       <- length(remaining_ids)
# 
# for (i in seq_along(remaining_ids)) {
#   vid <- remaining_ids[i]
# 
#   vessel_label <- gfw_vessel_periods %>%
#     filter(vesselId == vid) %>%
#     pull(shipname) %>%
#     first() %>%
#     replace_na("Unknown")
# 
#   message(sprintf("[%d/%d] Processing: %s (%s)", i, n_total, vessel_label, vid))
# 
#   events_v <- map_dfr(2012:2025, function(y) {
#     Sys.sleep(0.5)
#     tryCatch({
#       result <- gfw_event(
#         event_type = "FISHING",
#         vessels    = vid,
#         start_date = paste0(y, "-01-01"),
#         end_date   = paste0(y, "-12-31"),
#         key        = key
#       )
#       if (!is.null(result) && nrow(result) > 0) result else NULL
#     }, error = function(e) {
#       message("  Failed: ", vid, " / ", y, " — ", e$message)
#       NULL
#     })
#   })
# 
#   all_events[[vid]] <- events_v
#   message(sprintf("  → %d events collected", nrow(events_v)))
# }
# 
# events_df <- bind_rows(all_events)
# message("Total events collected: ", nrow(events_df))
# 
# # Remove large raw list from memory
# rm(all_events)
# gc()

# Save — skip Steps 3+4 on future runs by loading this file
# save(gfw_vessel_registry, gfw_vessel_periods, gfw_vessel_ids, events_df,
#      file = file.path(flyshootdir, "gfw_fishing_events_raw.RData"))
load(file.path(flyshootdir, "gfw_fishing_events_raw.RData"))


# ============================================================
# STEP 5 — Spatial join: events → ICES division
# ============================================================
# Assigns each event to an ICES division based on lat/lon.
# Events outside target_divisions get division = NA.
# Run diagnostic 10e BEFORE removing events_with_division.

events_sf <-
  events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

events_with_division <-
  events_sf %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

rm(events_sf)
gc()

message(nrow(events_with_division), " events after division join")
message(sum(is.na(events_with_division$division)),
        " events with no division (",
        round(mean(is.na(events_with_division$division)) * 100, 1), "%)")


# ============================================================
# STEP 6 — Marine filter: internal waters + EEZ
# ============================================================
# Two-stage filter:
#   a) Keep only events in study_divisions within a study EEZ
#   b) Remove events in internal waters (Wadden Sea, Zeeland etc.)
#      using the Marineregions World Internal Waters v4 shapefile

# Stage a: filter to study divisions + EEZ
sf_use_s2(FALSE)
events_marine <-
  events_with_division %>%
  filter(division %in% study_divisions, !is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_join(study_eez, join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(eez))
sf_use_s2(TRUE)

message("Events in study divisions         : ",
        nrow(filter(events_with_division, division %in% study_divisions)))
message("Events after EEZ filter           : ", nrow(events_marine))

# Stage b: remove internal waters
sf_use_s2(FALSE)
events_marine_clean <-
  events_marine %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  mutate(internal = lengths(st_intersects(., internal_waters)) > 0) %>%
  st_drop_geometry() %>%
  filter(!internal) %>%
  dplyr::select(-internal)
sf_use_s2(TRUE)

message("Events after internal water filter: ", nrow(events_marine_clean))
message("Events removed (internal waters)  : ",
        nrow(events_marine) - nrow(events_marine_clean))


# ============================================================
# STEP 7 — Coastal zone classification (0-12NM vs >12NM)
# ============================================================
# Uses the Marineregions World 12NM Territorial Sea shapefile
# as proxy for the coastal zone. Events within the 12NM zone
# are classified as "Coastal"; all others as "Offshore".
# Note: 3NM and 6NM zones are legally defined in EU fisheries
# regulations but no authoritative GIS layer is publicly available.
# The 12NM territorial sea is the closest available standard.

sf_use_s2(FALSE)
events_classified <-
  events_marine_clean %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  mutate(zone = if_else(
    lengths(st_intersects(., territorial_sea)) > 0,
    "Coastal (0-12NM)", "Offshore (>12NM)"
  )) %>%
  st_drop_geometry()
sf_use_s2(TRUE)

events_classified %>%
  count(zone) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()


# ============================================================
# STEP 8 — Vessel metadata: build vessel_meta lookup
# ============================================================
#
# Inputs:
#   gfw_vessel_registry          — from STEP 3 (vesselId × gear/GT/length)
#   mmsi_master                  — from STEP 2 (MMSI × vessel_name/flag)
#   vessel_gear_corrections.xlsx — manual fixes for non-fishing gears
#   vessel_gear_missing.xlsx     — manual gear assignments for NA-gear vessels
#
# Output:
#   vessel_meta — one row per vesselId, fully corrected, used in all steps
#
# To skip on re-runs: load(file.path(flyshootdir, "gfw_effort_analysis.RData"))
# ============================================================


# ---- 8a: Build base vessel_meta from registry ----
# One row per vesselId. For each MMSI, propagate the best available
# gear, GT and length across all identity periods.

vessel_meta <- gfw_vessel_registry %>%
  dplyr::select(
    vesselId, ssvid, shipname,
    transmissionDateFrom, transmissionDateTo,
    vessel_flag = flag, geartypes, tonnageGt, lengthM
  ) %>%
  distinct() %>%
  
  # Add vessel_name from our own vessel list (mmsi_master from STEP 2)
  left_join(
    mmsi_master %>% dplyr::select(mmsi, vessel_name, flag),
    by = c("ssvid" = "mmsi")
  ) %>%
  mutate(vessel_flag = coalesce(vessel_flag, flag)) %>%
  
  # Propagate best gear/GT/length across all periods for the same MMSI
  group_by(ssvid) %>%
  mutate(
    best_gear   = first(na.omit(geartypes)),
    best_gt     = median(tonnageGt[tonnageGt > 0], na.rm = TRUE),
    best_length = median(lengthM[lengthM > 0],     na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Keep one row per vesselId — prefer rows with gear and GT
  dplyr::select(
    vesselId, ssvid, vessel_name, vessel_flag,
    gear      = best_gear,
    gt        = best_gt,
    length_m  = best_length,
    transmissionDateFrom, transmissionDateTo
  ) %>%
  group_by(vesselId) %>%
  arrange(desc(!is.na(gear)), desc(!is.na(gt))) %>%
  slice(1) %>%
  ungroup() %>%
  
  # ---- Size classification ----
# Primary: GT. Fallback: length converted to approximate GT class.
mutate(
  size_var = case_when(
    !is.na(gt) & gt > 0             ~ gt,
    !is.na(length_m) & length_m > 0 ~ case_when(
      length_m < 15  ~   50,
      length_m < 20  ~  120,
      length_m < 28  ~  280,
      length_m < 42  ~  550,
      length_m < 70  ~  900,
      TRUE           ~ 1500
    ),
    TRUE ~ NA_real_
  ),
  size_source = case_when(
    !is.na(gt) & gt > 0             ~ "GT",
    !is.na(length_m) & length_m > 0 ~ "length proxy",
    TRUE                             ~ "unknown"
  ),
  size_class = case_when(
    is.na(size_var)    ~ "Unknown",
    size_var <  100    ~ "S1 <100 GT",
    size_var <  300    ~ "S2 100-300 GT",
    size_var <  600    ~ "S3 300-600 GT",
    size_var < 1200    ~ "S4 600-1200 GT",
    size_var >= 1200   ~ "S5 >1200 GT"
  ),
  size_class = factor(size_class, levels = c(
    "S1 <100 GT", "S2 100-300 GT", "S3 300-600 GT",
    "S4 600-1200 GT", "S5 >1200 GT", "Unknown"
  ))
)

message("vessel_meta (raw): ", nrow(vessel_meta), " rows | ",
        n_distinct(vessel_meta$vesselId), " vesselIds | ",
        n_distinct(vessel_meta$ssvid), " unique MMSIs")


# ---- 8b: Apply gear corrections (non-fishing vessel fixes) ----
# Source: vessel_gear_corrections.xlsx — vessels GFW misclassified as
# CARGO, PASSENGER, SEISMIC etc.
# action = "exclude" → remove from analysis
# action = "correct" → overwrite gear and/or size_class

corrections <- readxl::read_xlsx(
  file.path(flyshootdir, "vessel_gear_corrections.xlsx")
) %>%
  mutate(ssvid = as.character(ssvid)) %>%
  filter(!is.na(gear_corrected) | action == "exclude")

vessel_meta <- vessel_meta %>%
  left_join(
    corrections %>% dplyr::select(ssvid, gear_corrected,
                                  size_class_corrected, action),
    by = "ssvid"
  ) %>%
  filter(is.na(action) | action != "exclude") %>%
  mutate(
    gear = case_when(
      action == "correct" & !is.na(gear_corrected) ~ gear_corrected,
      TRUE ~ gear
    ),
    size_class = case_when(
      action == "correct" & !is.na(size_class_corrected) ~
        factor(size_class_corrected, levels = levels(size_class)),
      TRUE ~ size_class
    )
  ) %>%
  dplyr::select(-gear_corrected, -size_class_corrected, -action)

message("After gear corrections — vessels remaining: ",
        n_distinct(vessel_meta$ssvid))


# ---- 8c: Apply missing gear assignments ----
# Source: vessel_gear_missing.xlsx — manually assigned gear for vessels
# with no gear in the GFW registry.

gfw_gear_types <- c(
  "TRAWLERS", "OTHER_SEINES", "DREDGE_FISHING", "PURSE_SEINES",
  "TUNA_PURSE_SEINES", "SET_GILLNETS", "DRIFTNETS", "DRIFTING_LONGLINES",
  "POTS_AND_TRAPS", "POLE_AND_LINE", "SEINERS", "OTHER_PURSE_SEINES",
  "OTHER_FISHING", "FISHING"
)

reference <- tibble(
  valid_gear_types   = gfw_gear_types,
  valid_size_classes = c(
    "S1 <100 GT", "S2 100-300 GT", "S3 300-600 GT",
    "S4 600-1200 GT", "S5 >1200 GT",
    rep(NA_character_, length(gfw_gear_types) - 5)
  )
)

missing_gear_corrections <- readxl::read_xlsx(
  file.path(flyshootdir, "vessel_gear_missing.xlsx"),
  sheet = "missing_gear"
) %>%
  mutate(ssvid = as.character(ssvid)) %>%
  filter(!is.na(gear_corrected) | action == "exclude")

vessel_meta <- vessel_meta %>%
  left_join(
    missing_gear_corrections %>%
      dplyr::select(ssvid, gear_corrected, size_class_corrected, action),
    by = "ssvid"
  ) %>%
  filter(is.na(action) | action != "exclude") %>%
  mutate(
    gear = coalesce(gear_corrected, gear),
    size_class = case_when(
      !is.na(size_class_corrected) ~
        factor(size_class_corrected, levels = levels(size_class)),
      TRUE ~ size_class
    )
  ) %>%
  dplyr::select(-gear_corrected, -size_class_corrected, -action)

message("After missing gear corrections — vessels remaining: ",
        n_distinct(vessel_meta$ssvid))
message("Remaining NA gear: ", sum(is.na(vessel_meta$gear)))


# ---- 8d: Validation checks ----

vessel_meta %>%
  distinct(ssvid, size_class) %>%
  count(size_class) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

vessel_meta %>%
  distinct(ssvid, gear, size_class) %>%
  count(size_class, gear) %>%
  arrange(size_class, desc(n)) %>%
  print(n = 40)

vessel_meta %>%
  distinct(ssvid, vessel_flag, size_class) %>%
  count(vessel_flag, size_class) %>%
  pivot_wider(names_from  = size_class,
              values_from = n,
              values_fill = 0) %>%
  arrange(desc(rowSums(across(where(is.numeric))))) %>%
  print(n = 15)


# ============================================================
# STEP 9 — Fishing days
# ============================================================
# A fishing day = one distinct calendar date with at least one
# fishing event per vessel × division × year × quarter × zone.
# Uses n_distinct(date) to avoid double-counting multiple events
# on the same day.
#
# Two outputs:
#   fishing_days       — all events in target_divisions (broad)
#   fishing_days_study — marine-only, study divisions, with zone
#                        (coastal vs offshore) — use for main analysis

# Helper: suppress "fishing_days" column name warning in rename_with
apply_flag_other <- function(df, flag_col = "vessel_flag",
                             days_col = "fishing_days") {
  df %>%
    group_by(across(all_of(flag_col))) %>%
    mutate(total_flag_days = sum(.data[[days_col]])) %>%
    ungroup() %>%
    mutate(
      total_days = sum(.data[[days_col]]),
      flag_pct   = total_flag_days / total_days * 100,
      across(all_of(flag_col),
             ~ if_else(flag_pct < threshold_pct, "Other", .x))
    ) %>%
    rename_with(~ str_replace(., "fishing_days", days_col),
                any_of(c("fishing_days", "total_flag_days", "total_days")))
}

# --- 9a: fishing_days (all target divisions) ---
fishing_days <-
  events_with_division %>%
  mutate(date = as.Date(start), year = year(date), quarter = quarter(date)) %>%
  dplyr::select(-any_of(c("vessel_name", "vessel_flag"))) %>%
  left_join(vessel_meta, by = "vesselId", relationship = "many-to-one") %>%
  filter(!is.na(ssvid)) %>%                                    # remove NA ssvid
  distinct(ssvid, vessel_name, vessel_flag, gear, gt, size_class,
           division, year, quarter, date) %>%
  group_by(ssvid, vessel_name, vessel_flag, gear, gt, size_class,
           division, year, quarter) %>%
  summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
  mutate(gt_fishing_days = fishing_days * gt)

message("fishing_days: ", nrow(fishing_days), " rows | ",
        n_distinct(fishing_days$ssvid), " vessels | ",
        sum(fishing_days$fishing_days), " total days")

# --- 9b: fishing_days_study (marine only, study divisions, with zone) ---
fishing_days_study <-
  events_classified %>%
  mutate(date = as.Date(start), year = year(date), quarter = quarter(date)) %>%
  dplyr::select(-any_of(c("vessel_name", "vessel_flag"))) %>%
  left_join(vessel_meta, by = "vesselId", relationship = "many-to-one") %>%
  filter(!is.na(ssvid)) %>%                                    # remove NA ssvid
  distinct(ssvid, vessel_name, vessel_flag, gear, gt, size_class,
           division, zone, year, quarter, date) %>%
  group_by(ssvid, vessel_name, vessel_flag, gear, gt, size_class,
           division, zone, year, quarter) %>%
  summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
  mutate(gt_fishing_days = fishing_days * gt)

# Remove large intermediate objects
rm(events_with_division, events_marine, events_marine_clean)
gc()

# Save final analysis objects
save(gfw_vessel_registry, gfw_vessel_periods, gfw_vessel_ids,
     vessel_meta, fishing_days, fishing_days_study,
     file = file.path(flyshootdir, "gfw_effort_analysis.RData"))

# load(file.path(flyshootdir, "gfw_effort_analysis.RData"))


# ============================================================
# STEP 10 — Diagnostics
# ============================================================

message("\n========== DATA COMPLETENESS DIAGNOSTICS ==========\n")

completeness_summary <- function(df, label) {
  message("--- ", label, " (", nrow(df), " rows) ---")
  df %>%
    summarise(
      n_total        = n(),
      n_missing_name = sum(is.na(vessel_name)),
      n_missing_flag = sum(is.na(vessel_flag)),
      n_missing_gear = sum(is.na(gear)),
      n_missing_gt   = sum(is.na(gt)),
      pct_name       = round(n_missing_name / n_total * 100, 1),
      pct_flag       = round(n_missing_flag / n_total * 100, 1),
      pct_gear       = round(n_missing_gear / n_total * 100, 1),
      pct_gt         = round(n_missing_gt   / n_total * 100, 1)
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    print()
}

# ---- 10a: vessel_meta completeness ----
completeness_summary(vessel_meta, "vessel_meta")

# ---- 10b: fishing_days_study completeness ----
completeness_summary(fishing_days_study, "fishing_days_study")

# ---- 10c: fishing days with missing flag — top MMSIs ----
message("\n--- missing flag: top 20 MMSIs by fishing days ---")
fishing_days_study %>%
  filter(is.na(vessel_flag)) %>%
  group_by(ssvid, vessel_name) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  arrange(desc(fishing_days)) %>%
  print(n = 20)

# ---- 10d: missing gear by flag ----
message("\n--- missing gear by flag ---")
fishing_days_study %>%
  mutate(has_gear = !is.na(gear)) %>%
  group_by(vessel_flag, has_gear) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  pivot_wider(names_from = has_gear, values_from = fishing_days,
              names_prefix = "gear_", values_fill = 0) %>%
  rename(any_of(c(with_gear = "gear_TRUE", without_gear = "gear_FALSE"))) %>%
  mutate(
    without_gear = if ("without_gear" %in% names(.)) without_gear else 0L,
    pct_without  = round(without_gear / (with_gear + without_gear) * 100, 1)
  ) %>%
  arrange(desc(without_gear)) %>%
  print(n = 20)

# ---- 10e: NLD vessels with missing gear ----
message("\n--- NLD vessels with missing gear ---")
fishing_days_study %>%
  filter(vessel_flag == "NLD") %>%
  group_by(ssvid, vessel_name) %>%
  summarise(
    total_days        = sum(fishing_days),
    days_missing_gear = sum(fishing_days[is.na(gear)]),
    days_with_gear    = sum(fishing_days[!is.na(gear)]),
    pct_missing       = round(days_missing_gear / total_days * 100, 1),
    gear_types        = paste(sort(unique(na.omit(gear))), collapse = ", "),
    gear_pattern      = if_else(days_with_gear == 0,
                                "ALL missing", "PARTIAL missing"),
    .groups           = "drop"
  ) %>%
  filter(days_missing_gear > 0) %>%
  mutate(
    vessel_code = str_remove(
      str_extract(vessel_name, "^[A-Z]{2,3}[-\\s]?\\d{1,3}"), "[-\\s]"),
    vessel_code       = str_trim(str_to_upper(vessel_code)),
    vessel_name_clean = str_trim(str_remove(vessel_name,
                                "^[A-Z]{2,3}[-\\s]?\\d{1,3}[-\\s]?"))
  ) %>%
  arrange(desc(days_missing_gear)) %>%
  print(n = 30)

# ---- 10f: coverage by year ----
message("\n--- fishing days by year (sanity check) ---")
fishing_days_study %>%
  group_by(year) %>%
  summarise(
    total_days = sum(fishing_days),
    n_vessels  = n_distinct(ssvid),
    n_flags    = n_distinct(vessel_flag, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  print(n = 20)

# ---- 10g: coastal vs offshore split ----
message("\n--- coastal vs offshore effort ---")
fishing_days_study %>%
  group_by(zone) %>%
  summarise(
    fishing_days = sum(fishing_days),
    n_vessels    = n_distinct(ssvid),
    .groups      = "drop"
  ) %>%
  mutate(pct = round(fishing_days / sum(fishing_days) * 100, 1)) %>%
  print()


# ============================================================
# STEP 11 — Vessel coverage chart (PDF)
# ============================================================
# One row per MMSI. Segments coloured by flag per vesselId period.
# Vessel name shown inside bar when segment >= 1.5 years wide.
# Only vessels with at least one fishing day in fishing_days_study.

vessel_coverage <- gfw_vessel_periods %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, gear), by = "vesselId") %>%
  left_join(gfw_vessel_registry %>% dplyr::select(vesselId, vessel_flag = flag),
            by = "vesselId") %>%
  mutate(
    date_from       = as.Date(substr(transmissionDateFrom, 1, 10)),
    date_to         = as.Date(substr(transmissionDateTo,   1, 10)),
    bar_width_years = as.numeric(date_to - date_from) / 365,
    bar_text        = if_else(bar_width_years >= 1.5,
                              paste0(shipname, " (", vessel_flag, ")"), "")
  ) %>%
  group_by(ssvid) %>%
  mutate(
    recent_name = shipname[which.max(date_to)],
    recent_flag = vessel_flag[which.max(date_to)],
    y_label     = paste0(recent_name, "\n", ssvid)
  ) %>%
  ungroup() %>%
  semi_join(fishing_days_study %>% dplyr::select(ssvid), by = "ssvid") %>%
  arrange(recent_flag, recent_name, ssvid, date_from) %>%
  mutate(y_label = factor(y_label, levels = unique(y_label)))

# Build extended flag colour palette covering all flags present
flag_levels_all  <- sort(unique(na.omit(vessel_coverage$vessel_flag)))
flag_colours_all <- setNames(
  colorRampPalette(c(
    "#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#76B7B2",
    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  ))(length(flag_levels_all)),
  flag_levels_all
)
flag_colours_all[names(flag_colours[names(flag_colours) != "Other"])] <-
  flag_colours[names(flag_colours) != "Other"]

rows_per_page <- 30
all_labels    <- levels(vessel_coverage$y_label)
n_pages       <- ceiling(length(all_labels) / rows_per_page)

pdf(
  file   = file.path(flyshootdir, paste0("vessel_coverage_", Sys.Date(), ".pdf")),
  width  = 14,
  height = max(8, rows_per_page * 0.4)
)

for (p in seq_len(n_pages)) {
  row_start   <- (p - 1) * rows_per_page + 1
  row_end     <- min(p * rows_per_page, length(all_labels))
  page_labels <- all_labels[row_start:row_end]

  df_page <- vessel_coverage %>%
    filter(y_label %in% page_labels) %>%
    mutate(y_label = factor(y_label, levels = page_labels))

  print(
    ggplot(df_page,
           aes(xmin = date_from, xmax = date_to,
               ymin = as.numeric(y_label) - 0.4,
               ymax = as.numeric(y_label) + 0.4,
               fill = vessel_flag)) +
      theme_bw() +
      geom_rect(colour = "white", linewidth = 0.4) +
      geom_text(
        aes(x     = date_from + (date_to - date_from) / 2,
            y     = as.numeric(y_label),
            label = bar_text),
        size = 2.5, colour = "white", fontface = "bold",
        hjust = 0.5, vjust = 0.5
      ) +
      scale_fill_manual(values = flag_colours_all, na.value = "grey70") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits      = as.Date(c("2012-01-01", "2026-12-31"))) +
      scale_y_continuous(
        breaks = seq_along(page_labels),
        labels = page_labels,
        expand = expansion(add = 0.6)
      ) +
      theme(
        axis.text.y        = element_text(size = 7, family = "mono"),
        axis.text.x        = element_text(size = 9),
        axis.title         = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "bottom"
      ) +
      labs(
        fill     = "Flag",
        title    = paste0("Vessel fishing activity (", p, "/", n_pages, ")"),
        subtitle = "One row = one MMSI  |  segments show name/flag changes over time"
      )
  )
}

dev.off()
message("Vessel coverage PDF saved")


# ============================================================
# STEP 12 — Effort plots
# ============================================================
# All plots use fishing_days_study (marine only, study divisions).
# Use apply_flag_other() to collapse minor flags into "Other".

# ---- Plot 1: fishing days by year and flag (absolute) ----
fishing_days_study %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country")

# ---- Plot 2: fishing days by year and flag, faceted by division (absolute) ----
fishing_days_study %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days at sea by year, country and ICES division") +
  facet_wrap(~ division)

# ---- Plot 3: fishing days by year and flag (%) ----
fishing_days_study %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country (%)")

# ---- Plot 4: fishing days by year and flag, faceted by division (%) ----
fishing_days_study %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year, division) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year, country and ICES division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: fishing days by year and gear, faceted by division ----
fishing_days_study %>%
  filter(!is.na(division), !is.na(gear)) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  group_by(gear) %>%
  mutate(total_gear_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(
    total_days = sum(fishing_days),
    gear_pct   = total_gear_days / total_days * 100,
    gear       = if_else(gear_pct < threshold_pct, "Other", gear)
  ) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = gear)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Gear type",
       title = "Fishing days at sea by year and gear type") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 6: GT fishing days by year and flag ----
fishing_days_study %>%
  filter(!is.na(gt)) %>%
  group_by(year, vessel_flag) %>%
  summarise(gt_fishing_days = sum(gt_fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other(days_col = "gt_fishing_days") %>%
  group_by(year, vessel_flag) %>%
  summarise(gt_fishing_days = sum(gt_fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = gt_fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "GT fishing days", fill = "Flag",
       title = "GT fishing days at sea by year and country")

# ---- Plot 7: fishing events map — sample coloured by flag ----
events_sample <- events_classified %>%
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag), by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                    ~ "Other",
    vessel_flag %in% names(flag_colours)  ~ vessel_flag,
    TRUE                                  ~ "Other"
  )) %>%
  slice_sample(n = 5000)

ggplot() +
  theme_bw() +
  geom_sf(data = fao_sf_division, fill = NA, colour = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90", colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample,
             aes(x = lon, y = lat, colour = vessel_flag),
             size = 0.5, alpha = 0.5) +
  scale_colour_manual(values = flag_colours, na.value = "grey70") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_line(colour = "grey95"),
    legend.position = "bottom"
  ) +
  labs(colour   = "Flag",
       title    = "Fishing events — marine only (sample of 5000)",
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E")

# ---- Plot 8: fishing events map — coastal vs offshore ----
events_sample_zone <- events_classified %>%
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag), by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                    ~ "Other",
    vessel_flag %in% names(flag_colours)  ~ vessel_flag,
    TRUE                                  ~ "Other"
  )) %>%
  slice_sample(n = 5000)

ggplot() +
  theme_bw() +
  geom_sf(data = territorial_sea, fill = "lightyellow", alpha = 0.7, colour = NA) +
  geom_sf(data = fao_sf_division, fill = NA, colour = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90", colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample_zone,
             aes(x = lon, y = lat, colour = zone),
             size = 0.5, alpha = 0.5) +
  scale_colour_manual(values = c(
    "Coastal (0-12NM)"  = "#E15759",
    "Offshore (>12NM)"  = "#4E79A7"
  )) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_line(colour = "grey95"),
    legend.position = "bottom"
  ) +
  labs(colour   = "Zone",
       title    = "Fishing events by zone (sample of 5000)",
       subtitle = "Yellow = 12NM territorial sea  |  Red = coastal  |  Blue = offshore") +
  facet_wrap(~ zone)

# ---- Plot x: GFW vs CV logbook comparison (specific vessels) ----
# Requires elog_existing to be loaded separately
# t1 <-
#   events_classified %>%
#   mutate(date = as.Date(start), year = year(start)) %>%
#   filter(vessel_ssvid %in% vessels_cv$mmsi) %>%
#   filter(grepl("SCH|SL", vessel_name)) %>%
#   filter(division == "27.7.D") %>%
#   mutate(vessel = case_when(
#     vessel_name %in% c("JOHANNA SL-9", "SL-9 JOHANNA") ~ "SL9",
#     vessel_name == "SCH-135 GALIBIER"                   ~ "SCH135",
#     vessel_name == "SCH-144 VERTROUWEN"                 ~ "SCH144",
#     vessel_name == "SCH-99 ARAVIS"                      ~ "SCH99",
#     vessel_name == "SCH65 SIMPLON"                      ~ "SCH65"
#   )) %>%
#   distinct(year, vessel, division, date) %>%
#   group_by(year, vessel, division) %>%
#   summarise(fishing_days = n(), .groups = "drop") %>%
#   mutate(source = "GFW")
#
# t2 <-
#   elog_existing %>%
#   mutate(division = toupper(fao_division)) %>%
#   distinct(vessel, date, division, .keep_all = TRUE) %>%
#   filter(division == "27.7.D") %>%
#   mutate(year = year(date)) %>%
#   group_by(year, vessel, division) %>%
#   summarise(fishing_days = n(), .groups = "drop") %>%
#   mutate(source = "CV")
#
# bind_rows(t1, t2) %>%
#   ggplot(aes(x = year, y = fishing_days, colour = source, shape = source)) +
#   theme_bw() +
#   geom_point(size = 2) +
#   geom_line(linewidth = 0.5, alpha = 0.7) +
#   scale_colour_manual(values = c("GFW" = "#4E79A7", "CV" = "#E15759")) +
#   scale_x_continuous(breaks = 2012:2025) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(x = "", y = "Fishing days", colour = "Source", shape = "Source",
#        title = "GFW vs CV logbook — fishing days in 27.7.D by vessel") +
#   facet_wrap(~ vessel)
