# ============================================================
# GFW Effort Analysis — gfwr v3.0
# Goals:
#   1. Extract fishing effort (days at sea) for all vessels
#      active in ICES divisions 27.4.C, 27.7.D, 27.7.E
#   2. Summarise by year / quarter / flag / gear / division
#   3. Diagnostics on data completeness
#   4. Visualise effort trends
#
# Processing steps:
#   STEP 1 — Setup: libraries, paths, spatial layers
#   STEP 2 — Vessel lists: load CSVs → master MMSI list
#   STEP 3 — GFW vessel info: MMSI → vesselId + metadata
#   STEP 4 — Fishing events: pull 2012-2025 per vesselId
#   STEP 5 — Enrich events: spatial join → division
#   STEP 6 — Vessel metadata: build vessel_meta lookup
#   STEP 7 — Fishing days: aggregate to day-level effort
#   STEP 8 — Diagnostics: completeness checks
#   STEP 9 — Vessel coverage chart (PDF)
#   STEP 10 — Effort plots
# ============================================================


# ============================================================
# STEP 1 — Setup
# ============================================================

library(tidyverse)
library(lubridate)
library(gfwr)
library(sf)

key <- gfw_auth()

spatialdir  <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

source("R/FLYSHOOT utils.r")

# Target ICES divisions
target_divisions <- c("27.4.A", "27.4.B", "27.4.C", "27.7.D", "27.7.E")

# FAO/ICES division polygons
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

vessels_raw <- map_dfr(vessel_csvs, function(f) {
  read_csv(f, col_types = cols(mmsi = col_character())) %>%
    mutate(source_file = basename(f))
})

# One row per MMSI — best available metadata, n_events summed across periods
vessels <- vessels_raw %>%
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

message("Master vessel list: ", nrow(vessels), " unique MMSIs")
message("Flags: ", paste(sort(unique(na.omit(vessels$flag))), collapse = ", "))

# CV vessel list (for GFW vs logbook comparison in later sections)
vessels_cv <- readxl::read_excel(
  file.path(flyshootdir, "flyshoot vessels CV.xlsx")) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi))


# ============================================================
# STEP 3 — GFW vessel info: MMSI → vesselId + metadata
# ============================================================
# Single API call per MMSI returns ALL vesselId periods
# (selfReportedInfo) plus registry metadata (gear, GT, length).
# This replaces the two separate map_dfr calls from the old code.
#
# NOTE: if vessel_info_all already exists in the saved RData,
# load it and skip this step.

vessel_info_all <- map_dfr(vessels$mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)

    # Most recent registry row — gear, GT, length
    ri <- res$registryInfo %>%
      arrange(desc(latestVesselInfo)) %>%
      slice(1) %>%
      dplyr::select(geartypes, tonnageGt, lengthM)

    # All selfReportedInfo rows — one per vesselId period
    # bind_cols broadcasts the single registry row to all periods
    res$selfReportedInfo %>%
      mutate(mmsi_queried = m) %>%
      bind_cols(ri)

  }, error = function(e) {
    message("Failed MMSI: ", m, " — ", e$message)
    NULL
  })
})

# Derive downstream objects from the single result
vessel_ids_all <- vessel_info_all %>%
  dplyr::select(vesselId, ssvid, shipname,
                transmissionDateFrom, transmissionDateTo, mmsi_queried)

all_vessel_ids <- unique(vessel_ids_all$vesselId)

message(n_distinct(vessel_ids_all$vesselId), " unique vesselIds for ",
        n_distinct(vessel_ids_all$ssvid), " unique MMSIs")


# ============================================================
# STEP 4 — Fishing events: pull 2012-2025 per vesselId
# ============================================================
# GFW Events API is capped at 366 days per call → loop by year.
# Loop includes counter and vessel name for progress tracking.
#
# To resume an interrupted run:
#   completed_ids <- names(Filter(Negate(is.null), all_events))
#   remaining_ids <- setdiff(all_vessel_ids, completed_ids)
# Otherwise start fresh:

all_events    <- vector("list", length(all_vessel_ids))
names(all_events) <- all_vessel_ids
remaining_ids <- all_vessel_ids   # replace with setdiff() to resume
n_total       <- length(remaining_ids)

for (i in seq_along(remaining_ids)) {
  vid <- remaining_ids[i]

  vessel_label <- vessel_ids_all %>%
    filter(vesselId == vid) %>%
    pull(shipname) %>%
    first() %>%
    replace_na("Unknown")

  message(sprintf("[%d/%d] Processing: %s (%s)", i, n_total, vessel_label, vid))

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
      message("  Failed: ", vid, " / ", y, " — ", e$message)
      NULL
    })
  })

  all_events[[vid]] <- events_v
  message(sprintf("  → %d events collected", nrow(events_v)))
}

events_df <- bind_rows(all_events)
message("Total events collected: ", nrow(events_df))

# Save all key objects
save(vessel_info_all, vessel_ids_all, all_vessel_ids, events_df,
     file = file.path(flyshootdir, "gfw_fishing_events_raw.RData"))

# To reload later without re-running:
# load(file.path(flyshootdir, "gfw_fishing_events_raw_20260513.RData"))


# ============================================================
# STEP 5 — Enrich events: spatial join → division
# ============================================================

events_sf <-
  events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

events_with_division <-
  events_sf %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

# Optionally add EEZ (uncomment if eez_sf is available):
# eez_sf <- loadRData(file.path(spatialdir, "eez_sf.RData")) %>%
#   rename(eez = ISO_SOV1)
# events_with_division <-
#   events_with_division %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
#   st_join(eez_sf %>% dplyr::select(eez), join = st_within) %>%
#   st_drop_geometry()

message(nrow(events_with_division), " events total")
message(sum(is.na(events_with_division$division)),
        " events with no division assigned (",
        round(mean(is.na(events_with_division$division)) * 100, 1), "%)")

save(events_sf, events_with_division,
     file = file.path(flyshootdir, "gfw_fishing_events_sf.RData"))
# load(file.path(flyshootdir, "gfw_fishing_events_sf.RData"))

# ============================================================
# STEP 6 — Vessel metadata: build vessel_meta lookup
# ============================================================
# One row per vesselId.
# Gear: use registry geartypes (specific) propagated across all
# periods for the same MMSI. Falls back to NA if unavailable.
# Flag: from vessel_info_all (covers all periods, not just most recent).

vessel_meta <-
  vessel_ids_all %>%
  dplyr::select(vesselId, ssvid, shipname,
                transmissionDateFrom, transmissionDateTo) %>%
  distinct() %>%
  left_join(
    vessel_info_all %>%
      dplyr::select(vesselId, vessel_flag = flag, geartypes, tonnageGt),
    by = "vesselId"
  ) %>%
  left_join(
    vessels %>% dplyr::select(mmsi, vessel_name, flag),
    by = c("ssvid" = "mmsi")
  ) %>%
  mutate(
    vessel_flag = coalesce(vessel_flag, flag)
  ) %>%
  # Propagate best known gear (registry geartypes) across all periods per MMSI
  group_by(ssvid) %>%
  mutate(best_gear = first(na.omit(geartypes))) %>%
  ungroup() %>%
  dplyr::select(vesselId, ssvid, vessel_name, vessel_flag,
                gear = best_gear, gt = tonnageGt,
                transmissionDateFrom, transmissionDateTo) %>%
  # Deduplicate to one row per vesselId, preferring rows with gear/gt
  group_by(vesselId) %>%
  arrange(desc(!is.na(gear)), desc(!is.na(gt))) %>%
  slice(1) %>%
  ungroup()

message("vessel_meta: ", nrow(vessel_meta), " rows, ",
        n_distinct(vessel_meta$vesselId), " unique vesselIds")


# ============================================================
# STEP 7 — Fishing days
# ============================================================
# A fishing day = one distinct calendar date with at least one
# fishing event per vessel × division × year × quarter.
# Uses n_distinct(date) NOT sum(difftime) to avoid double-counting
# multiple events on the same day.

fishing_days <-
  events_with_division %>%
  mutate(
    date    = as.Date(start),
    year    = year(date),
    quarter = quarter(date)
  ) %>%
  dplyr::select(-vessel_name, -vessel_flag) %>%
  left_join(vessel_meta, by = "vesselId", relationship = "many-to-one") %>%
  distinct(ssvid, vessel_name, vessel_flag, gear, gt,
           division, year, quarter, date) %>%
  group_by(ssvid, vessel_name, vessel_flag, gear, gt,
           division, year, quarter) %>%
  summarise(
    fishing_days    = n_distinct(date),
    .groups         = "drop"
  ) %>%
  mutate(
    gt_fishing_days = fishing_days * gt   # effort weighted by vessel size
  )

message("fishing_days: ", nrow(fishing_days), " rows, ",
        n_distinct(fishing_days$ssvid), " unique vessels, ",
        sum(fishing_days$fishing_days), " total fishing days")


# ============================================================
# STEP 8 — Diagnostics
# ============================================================

message("\n========== DATA COMPLETENESS DIAGNOSTICS ==========\n")

# ---- 8a: vessel_meta completeness ----
message("--- vessel_meta (", nrow(vessel_meta), " vesselIds) ---")
vessel_meta %>%
  summarise(
    n_total         = n(),
    n_missing_name  = sum(is.na(vessel_name)),
    n_missing_flag  = sum(is.na(vessel_flag)),
    n_missing_gear  = sum(is.na(gear)),
    n_missing_gt    = sum(is.na(gt)),
    pct_name        = round(n_missing_name  / n_total * 100, 1),
    pct_flag        = round(n_missing_flag  / n_total * 100, 1),
    pct_gear        = round(n_missing_gear  / n_total * 100, 1),
    pct_gt          = round(n_missing_gt    / n_total * 100, 1)
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  print()

# ---- 8b: fishing_days completeness ----
message("\n--- fishing_days (", nrow(fishing_days), " rows) ---")
fishing_days %>%
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

# ---- 8c: fishing days with missing flag — which MMSIs? ----
message("\n--- fishing_days rows with missing flag (top 20 by days) ---")
fishing_days %>%
  filter(is.na(vessel_flag)) %>%
  group_by(ssvid, vessel_name) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  arrange(desc(fishing_days)) %>%
  print(n = 20)

# ---- 8d: fishing days with missing gear — breakdown by flag ----
message("\n--- missing gear by flag ---")
fishing_days %>%
  mutate(has_gear = !is.na(gear)) %>%
  group_by(vessel_flag, has_gear) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  pivot_wider(names_from = has_gear, values_from = fishing_days,
              names_prefix = "gear_", values_fill = 0) %>%
  rename(with_gear = gear_TRUE, without_gear = gear_FALSE) %>%
  mutate(pct_without = round(without_gear / (with_gear + without_gear) * 100, 1)) %>%
  arrange(desc(without_gear)) %>%
  print(n = 20)

# ---- 8e: events with no division — geographic extent ----
message("\n--- events outside target divisions ---")
events_with_division %>%
  filter(is.na(division)) %>%
  summarise(
    n       = n(),
    lat_min = round(min(lat, na.rm = TRUE), 1),
    lat_max = round(max(lat, na.rm = TRUE), 1),
    lon_min = round(min(lon, na.rm = TRUE), 1),
    lon_max = round(max(lon, na.rm = TRUE), 1)
  ) %>%
  print()

# ---- 8f: coverage by year ----
message("\n--- fishing days by year (sanity check) ---")
fishing_days %>%
  group_by(year) %>%
  summarise(
    total_days = sum(fishing_days),
    n_vessels  = n_distinct(ssvid),
    n_flags    = n_distinct(vessel_flag, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  print(n = 20)

# ---- 8g: empty vesselIds (no fishing events detected) ----
empty_vessels <- all_events %>%
  keep(~ is.null(.x) || nrow(.x) == 0) %>%
  names()

message("\n--- empty vesselIds (no fishing events) ---")
message(length(empty_vessels), " of ", length(all_vessel_ids),
        " vesselIds returned no events (",
        round(length(empty_vessels) / length(all_vessel_ids) * 100, 1), "%)")

vessel_ids_all %>%
  filter(vesselId %in% empty_vessels) %>%
  mutate(
    date_from   = as.Date(substr(transmissionDateFrom, 1, 10)),
    date_to     = as.Date(substr(transmissionDateTo,   1, 10)),
    days_active = as.numeric(date_to - date_from)
  ) %>%
  summarise(
    n                  = n(),
    median_days_active = round(median(days_active, na.rm = TRUE)),
    pct_short          = round(mean(days_active < 90, na.rm = TRUE) * 100, 1)
  ) %>%
  print()


# ============================================================
# STEP 9 — Vessel coverage chart (PDF)
# ============================================================
# One row per MMSI, segments coloured by flag per vesselId period.
# Vessel name shown inside bar if segment is wide enough (>= 1.5 yrs).

vessel_coverage <- vessel_ids_all %>%
  left_join(
    vessel_meta %>% dplyr::select(vesselId, gear),
    by = "vesselId"
  ) %>%
  left_join(
    vessel_info_all %>% dplyr::select(vesselId, vessel_flag = flag),
    by = "vesselId"
  ) %>%
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
  # Only vessels with actual fishing events
  semi_join(fishing_days %>% dplyr::select(ssvid), by = "ssvid") %>%
  arrange(recent_flag, recent_name, ssvid, date_from) %>%
  mutate(y_label = factor(y_label, levels = unique(y_label)))

# Build flag colour palette covering all flags present
flag_levels_all  <- sort(unique(na.omit(vessel_coverage$vessel_flag)))
flag_colours_all <- setNames(
  colorRampPalette(c(
    "#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#76B7B2",
    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  ))(length(flag_levels_all)),
  flag_levels_all
)
# Override with named colours for known flags
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

  p_out <-
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

  print(p_out)
}

dev.off()
message("Vessel coverage PDF saved")


# ============================================================
# STEP 10 — Effort plots
# ============================================================

# Helper: apply "Other" grouping to vessel_flag
apply_flag_other <- function(df, flag_col = "vessel_flag") {
  df %>%
    group_by(across(all_of(flag_col))) %>%
    mutate(total_flag_days = sum(fishing_days)) %>%
    ungroup() %>%
    mutate(
      total_days        = sum(fishing_days),
      flag_pct          = total_flag_days / total_days * 100,
      across(all_of(flag_col), ~ if_else(flag_pct < threshold_pct, "Other", .x))
    )
}

# ---- Plot 1: fishing days by year and flag (absolute) ----
fishing_days %>%
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
       title = "Days at sea by year and country")

# ---- Plot 2: fishing days by year and flag, faceted by division (absolute) ----
fishing_days %>%
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
       title = "Days at sea by year, country and ICES division") +
  facet_wrap(~ division)

# ---- Plot 3: fishing days by year and flag (%) ----
fishing_days %>%
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
fishing_days %>%
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
fishing_days %>%
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

# ---- Plot x: GFW vs CV logbook comparison (specific vessels) ----
t1 <-
  events_with_division %>%
  mutate(
    date = as.Date(start),
    year = year(start)
  ) %>%
  filter(!is.na(division)) %>%
  filter(vessel_ssvid %in% vessels_cv$mmsi) %>%
  filter(grepl("SCH|SL", vessel_name)) %>%
  filter(division == "27.7.D") %>%
  mutate(vessel = case_when(
    vessel_name %in% c("JOHANNA SL-9", "SL-9 JOHANNA") ~ "SL9",
    vessel_name == "SCH-135 GALIBIER"                   ~ "SCH135",
    vessel_name == "SCH-144 VERTROUWEN"                 ~ "SCH144",
    vessel_name == "SCH-99 ARAVIS"                      ~ "SCH99",
    vessel_name == "SCH65 SIMPLON"                      ~ "SCH65"
  )) %>%
  distinct(year, vessel, division, date) %>%
  group_by(year, vessel, division) %>%
  summarise(fishing_days = n(), .groups = "drop") %>%
  mutate(source = "GFW")

t2 <-
  elog_existing %>%
  mutate(division = toupper(fao_division)) %>%
  distinct(vessel, date, division, .keep_all = TRUE) %>%
  filter(division == "27.7.D") %>%
  mutate(year = year(date)) %>%
  group_by(year, vessel, division) %>%
  summarise(fishing_days = n(), .groups = "drop") %>%
  mutate(source = "CV")

bind_rows(t1, t2) %>%
  ggplot(aes(x = year, y = fishing_days, colour = source, shape = source)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  scale_colour_manual(values = c("GFW" = "#4E79A7", "CV" = "#E15759")) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", colour = "Source", shape = "Source",
       title = "GFW vs CV logbook — fishing days in 27.7.D by vessel") +
  facet_wrap(~ vessel)
