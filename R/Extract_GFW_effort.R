# ============================================================
# GFW Effort Analysis — gfwr v3.0
# ============================================================

library(tidyverse)
library(lubridate)
library(gfwr)
library(sf)

key <- gfw_auth()

spatialdir <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

source("R/FLYSHOOT utils.r")

fao_sf          <- loadRData(file.path(spatialdir, "fao_sf.RData"))
fao_sf_division <- 
  fao_sf %>% 
  filter(F_LEVEL=="DIVISION") %>% 
  dplyr::select(F_DIVISION) %>% 
  rename(division = F_DIVISION) %>% 
  mutate(division=toupper(division)) %>% 
  filter(division %in% c("27.4.A","27.4.B","27.4.C","27.7.D","27.7.E")) %>% 
  group_by(division) %>% 
  summarise(geometry = st_union(geometry)) %>%  # grouped union per division
  st_make_valid()

# ---- 1. Load vessel lists ----

# ============================================================
# VESSEL LISTS — built from area-extracted CSV files
# ============================================================
# Four CSVs capture different periods to include vessels that
# may have entered or left the fishery over time.
# We combine them into one master list of unique MMSIs.

# csvdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

vessel_csvs <- list.files(
  path       = flyshootdir,
  pattern    = "^fishing_vessels_27\\.4\\.c_27\\.7\\.d_27\\.7\\.e_.*\\.csv$",
  full.names = TRUE
)

message("Found ", length(vessel_csvs), " CSV files:")
message(paste(" -", vessel_csvs, collapse = "\n"))

# Read and combine all CSVs — keep source period for traceability
vessels_raw <- map_dfr(vessel_csvs, function(f) {
  read_csv(f, col_types = cols(mmsi = col_character())) |>
    mutate(source_file = basename(f))
})

# Master unique MMSI list — one row per MMSI, keeping best available
# metadata (vessel_name, flag, vessel_type, n_events summed across periods)
vessels <- vessels_raw |>
  group_by(mmsi) |>
  summarise(
    vessel_name  = first(na.omit(vessel_name)),
    flag         = first(na.omit(flag)),
    vessel_type  = first(na.omit(vessel_type)),
    n_events     = sum(n_events, na.rm = TRUE),
    n_periods    = n(),                           # how many CSVs this MMSI appeared in
    .groups = "drop"
  ) |>
  arrange(flag, vessel_name)

message("\nMaster vessel list: ", nrow(vessels), " unique MMSIs across ",
        length(vessel_csvs), " periods")
message("Flags represented: ", paste(sort(unique(vessels$flag)), collapse = ", "))


# vessels <- readxl::read_excel(file.path(flyshootdir, "flyshoot vessels update EvL.xlsx")) %>%
#   mutate(mmsi = as.character(mmsi)) %>%
#   filter(!is.na(mmsi)) 

vessels_cv <- readxl::read_excel(file.path(flyshootdir, "flyshoot vessels CV.xlsx")) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi))

# Inspect one vessel first
# test <- gfw_vessel_info(query = "244704000", search_type = "search", key = key)
# str(test, max.level = 3)
# names(test)





# ---- 2. Resolve MMSI → GFW vessel IDs ----

# Single API call per MMSI — returns both full vesselId history AND
# the most recent registry metadata (gear, GT, length)

vessel_info_all <- map_dfr(vessels$mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)
    
    # Most recent registry row for metadata
    ri <- res$registryInfo %>%
      arrange(desc(latestVesselInfo)) %>%
      slice(1) %>%
      dplyr::select(geartypes, tonnageGt, lengthM)
    
    # All selfReportedInfo rows — one per vesselId period
    res$selfReportedInfo %>%
      mutate(mmsi_queried = m) %>%
      bind_cols(ri)           # broadcast the single registry row to all periods
    
  }, error = function(e) { message("Failed MMSI: ", m, " — ", e$message); NULL })
})

# Derive the two downstream objects from the single result
vessel_ids_all <- vessel_info_all %>%
  dplyr::select(vesselId, ssvid, shipname,
                transmissionDateFrom, transmissionDateTo, mmsi_queried)

vessel_info <- vessel_info_all %>%
  group_by(mmsi_queried) %>%
  arrange(desc(transmissionDateTo)) %>%
  slice(1) %>%
  ungroup()

all_vessel_ids <- unique(vessel_ids_all$vesselId)


# vessel_info <- map_dfr(vessels$mmsi, function(m) {
#   tryCatch({
#     res <- gfw_vessel_info(query = m, search_type = "search", key = key)
#     
#     # Take the most recent/best-matched selfReportedInfo row
#     sri <- res$selfReportedInfo %>%
#       arrange(desc(transmissionDateTo)) %>%   # most recent first
#       slice(1)                                 # take top row per MMSI
#     
#     # Grab registry info (gear, GT) if available
#     ri <- res$registryInfo %>%
#       arrange(desc(latestVesselInfo)) %>%
#       slice(1) %>%
#       select(geartypes, tonnageGt, lengthM) 
#     
#     bind_cols(sri, ri) %>%
#       mutate(mmsi_queried = m)
#     
#   }, error = function(e) { message("Failed MMSI: ", m, " — ", e$message); NULL })
# })


# # All vesselIds per MMSI (needed for full 2012-2025 coverage)
# vessel_ids_all <- map_dfr(vessels$mmsi, function(m) {
#   tryCatch({
#     res <- gfw_vessel_info(query = m, search_type = "search", key = key)
#     res$selfReportedInfo %>%
#       select(vesselId, ssvid, shipname, transmissionDateFrom, transmissionDateTo) %>%
#       mutate(mmsi_queried = m)
#   }, error = function(e) { message("Failed: ", m); NULL })
# })

# Then use ALL vesselIds when pulling events
# all_vessel_ids <- unique(vessel_ids_all$vesselId)




# ---- 3. Pull fishing events per vessel (full period) ----
# GFW Events API: one call per vessel, full 2012-2025 range
# NOTE: date range limit is 366 days per call, so loop by year

all_events <- list()

n_total <- length(remaining_ids)

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
save(vessel_info, events_df, vessel_ids_all, all_vessel_ids,
     file = file.path(flyshootdir, "gfw_fishing_events_raw_20260513.RData"))



# How many vesselIds in vessel_ids_all are missing from vessel_info?
vessel_ids_all %>%
  anti_join(vessel_info, by = "vesselId") %>%
  nrow()

# Find which MMSIs have incomplete vesselId coverage
missing_mmsi <- vessel_ids_all %>%
  anti_join(vessel_info, by = "vesselId") %>%
  distinct(mmsi_queried) %>%
  pull(mmsi_queried)

message(length(missing_mmsi), " MMSIs need to be re-queried")

vessel_info_missing <- map_dfr(missing_mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)
    
    ri <- res$registryInfo %>%
      arrange(desc(latestVesselInfo)) %>%
      slice(1) %>%
      dplyr::select(geartypes, tonnageGt, lengthM)
    
    res$selfReportedInfo %>%
      mutate(mmsi_queried = m) %>%
      bind_cols(ri)
    
  }, error = function(e) { message("Failed MMSI: ", m, " — ", e$message); NULL })
})

# Combine with existing vessel_info to get the full vessel_info_all
vessel_info_all <- bind_rows(vessel_info, vessel_info_missing) %>%
  distinct(vesselId, .keep_all = TRUE)

message(n_distinct(vessel_info_all$vesselId), " unique vesselIds in vessel_info_all")
message(n_distinct(vessel_ids_all$vesselId),  " unique vesselIds in vessel_ids_all")

# Save
save(vessel_info, events_df, vessel_ids_all, all_vessel_ids,
     file = file.path(flyshootdir, "gfw_fishing_events_raw_20260513.RData"))


load(file = file.path(flyshootdir, "gfw_fishing_events_raw_20260513.RData"))

completed_ids <- names(Filter(Negate(is.null), all_events))
remaining_ids <- setdiff(all_vessel_ids, completed_ids)

# Vessels that returned NULL or zero events
empty_vessels <- all_events %>%
  keep(~ is.null(.x) || nrow(.x) == 0) %>%
  names()
message(length(empty_vessels), " vessel IDs with no events recorded")

# check the empty vessels
vessel_ids_all %>%
  filter(vesselId %in% empty_vessels) %>%
  mutate(
    date_from = as.Date(substr(transmissionDateFrom, 1, 10)),
    date_to   = as.Date(substr(transmissionDateTo,   1, 10)),
    days_active = as.numeric(date_to - date_from)
  ) %>%
  summarise(
    n                 = n(),
    median_days_active = median(days_active, na.rm = TRUE),
    pct_short         = mean(days_active < 90, na.rm = TRUE) * 100
  )

# Where they fishing at all
vessel_ids_all %>%
  filter(vesselId %in% empty_vessels) %>%
  left_join(vessels %>% dplyr::select(mmsi, flag, vessel_type),
            by = c("ssvid" = "mmsi")) %>%
  count(flag, vessel_type, sort = TRUE) %>%
  print(n = 20)

# Overall though, 765 empty vesselIds out of 3,063 is not a concern — the vessels with actual fishing events are the ones that matter for your effort analysis, and you have 2,298 of those.

# Derive events_by_division
events_sf <-
  events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

events_with_division <-
  events_sf %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

# optionally add EEZ
# events_with_division <-
#   events_with_division %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
#   st_join(eez_sf %>% dplyr::select(eez), join = st_within) %>%
#   st_drop_geometry()

# vessel_meta
vessel_meta <-
  vessel_ids_all %>%
  dplyr::select(vesselId, ssvid, shipname, transmissionDateFrom, transmissionDateTo) %>%
  distinct() %>%
  left_join(
    vessel_info %>%
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
  # For each ssvid, take the most specific gear (non-NA geartypes preferred)
  group_by(ssvid) %>%
  mutate(
    best_gear = first(na.omit(geartypes))   # propagate best known gear to all periods
  ) %>%
  ungroup() %>%
  dplyr::select(vesselId, ssvid, vessel_name, vessel_flag,
                gear = best_gear, gt = tonnageGt,
                transmissionDateFrom, transmissionDateTo) %>% 
  
  group_by(vesselId) %>%
  arrange(desc(!is.na(gear)), desc(!is.na(gt))) %>%  # rows with gear/gt first
  slice(1) %>%
  ungroup()


# ---- 4. Derive fishing days ----
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
    fishing_days = n_distinct(date),
    .groups = "drop"
  ) %>% 
  mutate(
    gt_fishing_days = fishing_days * gt
  )

# Sanity check
fishing_days %>%
  group_by(year) %>%
  summarise(
    total_days = sum(fishing_days),
    n_vessels  = n_distinct(ssvid),
    .groups    = "drop"
  ) %>%
  print(n = 20)


glimpse(fishing_days)
fishing_days %>% count(year) %>% print(n = 20)

# flag colours

# Replace scale_fill_manual with this — muted professional palette
# Assign colours to specific flags for consistency across all plots
flag_colours <- c(
  "BEL" = "#4E79A7",   # steel blue
  "NLD" = "#F28E2B",   # warm orange
  "GBR" = "#59A14F",   # muted green
  "FRA" = "#E15759",   # muted red
  "DEU" = "#76B7B2",   # teal
  "DNK" = "#EDC948",   # gold (darker than yellow)
  "NOR" = "#B07AA1",   # muted purple
  "IRL" = "#FF9DA7",   # soft pink
  "Other" = "#BAB0AC"  # neutral grey
)

# Vessel coverage plot

vessel_coverage <- vessel_ids_all %>%
  # Join best gear from vessel_meta
  left_join(
    vessel_meta %>% dplyr::select(vesselId, gear),
    by = "vesselId"
  ) %>%
  # Join flag from vessel_info
  left_join(
    vessel_info_all %>% dplyr::select(vesselId, vessel_flag = flag),
    by = "vesselId"
  ) %>%
  mutate(
    date_from = as.Date(substr(transmissionDateFrom, 1, 10)),
    date_to   = as.Date(substr(transmissionDateTo,   1, 10)),
    # Only show name inside bar if segment is wide enough
    bar_width_years = as.numeric(date_to - date_from) / 365,
    bar_text        = if_else(bar_width_years >= 1.5,
                              paste0(shipname, " (", vessel_flag, ")"), "")
  ) %>%
  # Y-axis: most recent name + MMSI
  group_by(ssvid) %>%
  mutate(
    recent_name  = shipname[which.max(date_to)],
    recent_flag  = vessel_flag[which.max(date_to)],
    y_label      = paste0(recent_name, "\n", ssvid)
  ) %>%
  ungroup() %>%
  # Only keep periods where vessel was actively fishing
  semi_join(
    fishing_days %>% dplyr::select(ssvid),
    by = "ssvid"
  ) %>%
  arrange(recent_flag, recent_name, ssvid, date_from) %>%
  mutate(y_label = factor(y_label, levels = unique(y_label)))

# Updated flag colours using the muted palette
flag_levels  <- sort(unique(na.omit(vessel_coverage$vessel_flag)))
flag_colours_all <- setNames(
  colorRampPalette(c(
    "#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#76B7B2",
    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  ))(length(flag_levels)),
  flag_levels
)

# Override with named colours where we know the flag
known_colours <- c(
  "BEL" = "#4E79A7", "NLD" = "#F28E2B", "GBR" = "#59A14F",
  "FRA" = "#E15759", "DEU" = "#76B7B2", "DNK" = "#EDC948",
  "NOR" = "#B07AA1", "IRL" = "#FF9DA7"
)
flag_colours_all[names(known_colours)] <- known_colours

# PDF output
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
  
  p_out <- ggplot(df_page,
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
    scale_x_date(date_breaks  = "2 years", date_labels = "%Y",
                 limits       = as.Date(c("2012-01-01", "2026-12-31"))) +
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
message("PDF saved")


# ---- Plot 1: fishing days by year and country (stacked bar) ----

fishing_days %>%
  group_by(vessel_flag) %>%
  summarise(total_days = sum(fishing_days), .groups = "drop") %>%
  mutate(pct = round(total_days / sum(total_days) * 100, 2)) %>%
  arrange(desc(pct)) %>%
  print(n = 30)

# Define threshold — flags contributing less than X% of total days go into "Other"
threshold_pct <- 1   # adjust as needed

fishing_days %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  # Calculate each flag's share of total days across all years
  group_by(vessel_flag) %>%
  mutate(total_flag_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(
    total_days  = sum(fishing_days),
    flag_pct    = total_flag_days / total_days * 100,
    vessel_flag = if_else(flag_pct < threshold_pct, "Other", vessel_flag)
  ) %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Days at sea by year and country")




# ---- Plot 2: fishing days by year and country, faceted by ICES division ----
fishing_days %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  # Calculate each flag's share of total days across all years and divisions
  group_by(vessel_flag) %>%
  mutate(total_flag_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(
    total_days  = sum(fishing_days),
    flag_pct    = total_flag_days / total_days * 100,
    vessel_flag = if_else(flag_pct < threshold_pct, "Other", vessel_flag)
  ) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Days at sea by year, country and ICES division") +
  facet_wrap(~ division)


# ---- Plot 3: fishing days by year and country (stacked bar, percentage) ----
fishing_days %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  group_by(vessel_flag) %>%
  mutate(total_flag_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(
    total_days  = sum(fishing_days),
    flag_pct    = total_flag_days / total_days * 100,
    vessel_flag = if_else(flag_pct < threshold_pct, "Other", vessel_flag)
  ) %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country (%)")

# ---- Plot 4: fishing days by year and country, faceted by ICES division (percentage) ----
fishing_days %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  group_by(vessel_flag) %>%
  mutate(total_flag_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(
    total_days  = sum(fishing_days),
    flag_pct    = total_flag_days / total_days * 100,
    vessel_flag = if_else(flag_pct < threshold_pct, "Other", vessel_flag)
  ) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year, division) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year, country and ICES division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: fishing days by year and gear type, faceted by ICES division ----
fishing_days %>%
  filter(!is.na(division), !is.na(gear)) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  # Collapse low-effort gear types into "Other"
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

# ---- Plot x: fishing days by year and vessel ----
t1 <-
  events_with_division %>% 
  mutate(
    date    = as.Date(start),
    year    = year(start)
  ) %>%
  filter(!is.na(division)) %>%
  filter(vessel_ssvid %in% vessels_cv$mmsi) %>% 
  filter(grepl("SCH|SL", vessel_name)) %>% 
  filter(division == "27.7.D") %>% 
  mutate(vessel = case_when(
    vessel_name %in% c("JOHANNA SL-9","SL-9 JOHANNA") ~ "SL9",
    vessel_name =="SCH-135 GALIBIER"                  ~ "SCH135",
    vessel_name == "SCH-144 VERTROUWEN"               ~ "SCH144",
    vessel_name == "SCH-99 ARAVIS"                    ~ "SCH99",
    vessel_name == "SCH65 SIMPLON"                    ~ "SCH65")) %>% 
  distinct(year, vessel, division, date) %>% 
  group_by(year, vessel, division) %>%
  summarise(fishing_days = n(), .groups = "drop") %>% 
  mutate(source="GFW")

unique(t1$vessel)
tmp <- t1 %>% group_by(vessel, year) %>% mutate(n=n()) 
View(tmp)

  # ggplot(aes(x = year, y = fishing_days, fill = vessel_name)) +
  # theme_bw() +
  # theme(legend.position = "none") +
  # geom_bar(stat = "identity") +
  # scale_fill_brewer(palette = "Set1") +
  # scale_x_continuous(breaks = 2012:2025) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  # labs(x = "", y = "Fishing days", fill = "Flag",
  #      title = "Apparent fishing days at sea in 27.7.D by year and vessel") +
  # facet_wrap(~ vessel_name)


t2 <-
  elog_existing %>%
  mutate(division = toupper(fao_division)) %>% 
  distinct(vessel, date, division, .keep_all = TRUE) %>% 
  filter(division == "27.7.D") %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, vessel, division) %>%
  summarise(fishing_days = n(), .groups = "drop") %>% 
  mutate(source="CV")

tmp <- t2 %>% group_by(vessel, year) %>% mutate(n=n()) 
View(tmp)

bind_rows(t1, t2) %>%   
  ggplot(aes(x = year, y = fishing_days, fill = source, colour=source)) +
  theme_bw() +
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Apparent fishing days at sea in 27.7.D by year and vessel") +
  facet_wrap(~ vessel)

















