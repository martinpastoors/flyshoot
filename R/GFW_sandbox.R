# GFW sandbox

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

source(file.path(here::here(), "R/gfw_functions.R"))  # UPDATE THIS PATH
source(file.path(here::here(), "R/gfw_plotting.R"))  # UPDATE THIS PATH
source(file.path(here::here(), "R/gfw_digivloot_matching.R"))  # UPDATE THIS PATH

output_dir <- CONFIG$output_dir

# Load the complete historical dataset
trips  <- readRDS(file.path(output_dir, "gfw_trips_cumulative.rds"))
events <- readRDS(file.path(output_dir, "gfw_events_by_trip_cumulative.rds"))
digivloot_data <- loadRData(file.path(onedrive, "trip.RData")) %>% filter(year(date) == 2026)

plot_trips_dumbbell_compact(
  trips_data = trips,
  events_data = events,
  show_fishing_events=FALSE,
  line_size = 1.5
)

p1 <- plot_trips_comparison_dumbbell(
  gfw_trips = gfw_trips,
  digivloot_data = digivloot_data,
  vessel_mapping = vessel_mapping,
  show_trip_numbers = TRUE,
  line_size = 1.5
)


full_report <- run_full_diagnostic(
  gfw_trips = gfw_trips,
  digivloot_data = digivloot_data,
  vessel_mapping = vessel_mapping,
  time_tolerance_hours = 24  # Current tolerance
)


# Try overview (just trips, no events)
p_overview <- plot_trips_overview(
  trips_data = trips
)
print(p_overview)

# Check data span
range(events$start)
range(trips$port_departure)

summary(trips[, c("port_departure_lon", "port_departure_lat", 
                  "port_arrival_lon", "port_arrival_lat")])




p_overview <- plot_trips_overview(
  trips_data = trips,
  events_data = events
)
print(p_overview)

p_overview2 <- plot_trips_overview(
  trips_data = trips,
  events_data = events_with_trips  # Use this file!
)
print(p_overview2)

p1 <- plot_trips_map(
  trips_data = trips,
  events_data = events_with_trips,  # Use this file!
  facet_by_month = TRUE,
  facet_by_vessel = TRUE
)
print(p1)

# Save
ggsave("output/trips_map_faceted.png", p1, 
       width = 14, height = 10, dpi = 300)


# Example 2: Simple overview without faceting
p2 <- plot_trips_simple(
  trips_data = trips,
  events_data = events,
  region = "north_sea"  # Or "auto", "europe"
)

print(p2)

p3 <- plot_vessel_trips_timeline(
  trips_data = trips,
  vessel_name = "SCH-99 ARAVIS",
  events_data = events_with_trips  # Use this file!
)
print(p3)



tasks <- taskscheduler_ls()
print(tasks)

"GFW_Weekly_Extraction" %in% tasks$TaskName

task_details <- tasks %>%
  filter(TaskName == "GFW_Weekly_Extraction")

print(task_details)

source(file.path(here::here(), "R/gfw_weekly_extraction.R"))






ggsave("output/trips_overview.png", p2, 
       width = 10, height = 8, dpi = 300)


# Example 3: Single vessel timeline
p3 <- plot_vessel_trips_timeline(
  trips_data = trips,
  vessel_name = "SCH-99 ARAVIS",
  events_data = events
)

print(p3)
ggsave("output/aravis_trips_timeline.png", p3, 
       width = 12, height = 8, dpi = 300)


# Example 4: Filter to specific time period
trips_jan <- trips %>%
  filter(port_departure >= "2026-01-01", 
         port_departure < "2026-02-01")

p4 <- plot_trips_map(
  trips_data = trips_jan,
  region = "auto",
  facet_by_month = FALSE,
  facet_by_vessel = TRUE
)

print(p4)


# Example 5: Focus on specific region
p5 <- plot_trips_map(
  trips_data = trips,
  region = c(xmin = 2, xmax = 8, ymin = 51, ymax = 56),  # Custom North Sea area
  facet_by_month = TRUE,
  facet_by_vessel = TRUE
)

print(p5)


# 7/5/2026
# ---- Did GFW extract any events at all for SCH65 in 2023-2024? ----
# Check raw events_df BEFORE the spatial join and division filter

sch65_mmsi <- vessels_cv %>% filter(vessel == "SCH65") %>% pull(mmsi)

# Total events pulled for SCH65 by year
events_df %>%
  filter(vessel_ssvid %in% sch65_mmsi) %>%
  mutate(year = year(as.Date(start))) %>%
  count(year) %>%
  print(n = 20)

# ---- Where did those 2023-2024 events fall spatially? ----
# Check events_with_division — this has the spatial join applied but includes
# ALL divisions (not just your 5 target ones), and NAs for events outside any division

events_with_division %>%
  filter(vessel_ssvid %in% sch65_mmsi) %>%
  mutate(year = year(as.Date(start))) %>%
  filter(year %in% c(2022, 2023, 2024)) %>%   # include 2022 as a "normal" reference year
  count(year, division) %>%
  arrange(year, division) %>%
  print(n = 30)


# ---- If events exist but fall outside target divisions, map them ----
events_with_division %>%
  filter(vessel_ssvid %in% sch65_mmsi) %>%
  mutate(year = year(as.Date(start))) %>%
  filter(year %in% c(2022, 2023, 2024, 2025), !is.na(lat), !is.na(lon)) %>%
  ggplot(aes(x = lon, y = lat, colour = factor(year))) +
  theme_bw() +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_colour_brewer(palette = "Set1") +
  coord_quickmap() +
  labs(x = "", y = "", colour = "Year",
       title = "GFW event locations for SCH65 in 2022-2025") +
  facet_wrap(~year, ncol=2)

events_with_division %>%
  filter(vessel_ssvid %in% sch65_mmsi) %>%
  mutate(year = year(as.Date(start))) %>%
  filter(year %in% 2022:2024) %>%
  count(year, division) %>%
  arrange(year, division) %>%
  print(n = 30)




pdf(file.path(flyshootdir, "vessel_timeline.pdf"), width = 14, height = 10)
for (p in seq_len(n_pages)) print(plot_timeline_page(p))
dev.off()



library(sf)

## ── Netherlands: read GeoPackage directly from defensie.nl ──────────────
# 1. Download zip first (no live WFS, but direct download works)
tmp <- tempfile(fileext = ".zip")
download.file(
  "https://english.defensie.nl/site/binaries/site-content/collections/documents/2025/04/25/zones-under-the-common-fisheries-policy/Zones_CFP_%28Visserijbeleid%29_NL_apr2025.zip",
  tmp
)

# Inspect contents:
unzip(tmp, list = TRUE)

# Extract all files
extracted <- unzip(tmp, exdir = tempdir())

# Pick the 0-3M and 0-6M CFP zone GeoPackages
nl_3nm <- st_read(extracted[grepl("NL_Zone_0_3M.*\\.gpkg$", extracted)])
nl_6nm <- st_read(extracted[grepl("NL_Zone_0_6M.*\\.gpkg$", extracted)])

# Or the boundary lines (the 3M and 6M limits as lines, not polygons):
nl_3nm_line <- st_read(extracted[grepl("NL_03M.*\\.gpkg$", extracted)])
nl_6nm_line <- st_read(extracted[grepl("NL_06M.*\\.gpkg$", extracted)])

library(sf)
library(ggplot2)

# Read the CFP zone polygons
nl_3nm  <- st_read(extracted[grepl("NL_Zone_0_3M.*\\.gpkg$",  extracted)])
nl_6nm  <- st_read(extracted[grepl("NL_Zone_0_6M.*\\.gpkg$",  extracted)])
nl_312m <- st_read(extracted[grepl("NL_Zone_3_12M.*\\.gpkg$", extracted)])
nl_612m <- st_read(extracted[grepl("NL_Zone_6_12M.*\\.gpkg$", extracted)])

# Read the baseline and limit lines for context
nl_baseline <- st_read(extracted[grepl("NL_NormalBaselines.*\\.gpkg$",   extracted)])
nl_3nm_line <- st_read(extracted[grepl("NL_03M.*\\.gpkg$",               extracted)])
nl_6nm_line <- st_read(extracted[grepl("NL_06M.*\\.gpkg$",               extracted)])
nl_12nm_line <- st_read(extracted[grepl("NL_12M.*\\.gpkg$",              extracted)])

ggplot() +
  # Zones as filled polygons (plot wider zones first so narrow ones sit on top)
  geom_sf(data = nl_612m,  aes(fill = "6–12 NM (FR access)"),    colour = NA, alpha = 0.5) +
  geom_sf(data = nl_312m,  aes(fill = "3–12 NM (BE/DE/DK access)"), colour = NA, alpha = 0.5) +
  geom_sf(data = nl_6nm,   aes(fill = "0–6 NM"),                 colour = NA, alpha = 0.5) +
  geom_sf(data = nl_3nm,   aes(fill = "0–3 NM (NL exclusive)"),  colour = NA, alpha = 0.6) +
  # Boundary lines
  geom_sf(data = nl_baseline,  colour = "grey40", linewidth = 0.3, linetype = "dotted") +
  geom_sf(data = nl_3nm_line,  colour = "#e41a1c", linewidth = 0.5) +
  geom_sf(data = nl_6nm_line,  colour = "#ff7f00", linewidth = 0.5) +
  geom_sf(data = nl_12nm_line, colour = "#377eb8", linewidth = 0.5) +
  scale_fill_manual(
    name = "CFP zone",
    values = c(
      "0–3 NM (NL exclusive)"      = "#e41a1c",
      "0–6 NM"                     = "#ff7f00",
      "3–12 NM (BE/DE/DK access)"  = "#377eb8",
      "6–12 NM (FR access)"        = "#984ea3"
    )
  ) +
  labs(
    title    = "Netherlands CFP Fishing Zones",
    subtitle = "Common Fisheries Policy coastal access zones (April 2025)",
    caption  = "Source: Royal Netherlands Navy Hydrographic Service"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

## ── Ireland: ArcGIS REST GeoJSON (works freely) ─────────────────────────
irl_6nm <- st_read(
  "https://atlas.marine.ie/arcgis/rest/services/ReportingUnits/MapServer/16/query?where=1%3D1&outFields=*&f=geojson"
)




# Coverage comparison: GT vs length
gfw_vessel_registry %>%
  summarise(
    n_total         = n(),
    n_with_gt       = sum(!is.na(tonnageGt) & tonnageGt > 0),
    pct_with_gt     = round(n_with_gt     / n_total * 100, 1),
    n_with_length   = sum(!is.na(lengthM)  & lengthM  > 0),
    pct_with_length = round(n_with_length / n_total * 100, 1),
    n_with_both     = sum(!is.na(tonnageGt) & tonnageGt > 0 &
                            !is.na(lengthM)   & lengthM  > 0),
    pct_with_both   = round(n_with_both  / n_total * 100, 1)
  )

# Coverage by flag
gfw_vessel_registry %>%
  group_by(flag) %>%
  summarise(
    n               = n(),
    pct_has_gt      = round(mean(!is.na(tonnageGt) & tonnageGt > 0) * 100, 1),
    pct_has_length  = round(mean(!is.na(lengthM)   & lengthM  > 0) * 100, 1),
    gt_median       = round(median(tonnageGt, na.rm = TRUE)),
    length_median   = round(median(lengthM,   na.rm = TRUE), 1)
  ) %>%
  arrange(desc(n)) %>%
  print(n = 20)

# For vessels that have BOTH: check GT-length consistency
# (useful for calibrating which field to trust more)
gfw_vessel_registry %>%
  filter(!is.na(tonnageGt), tonnageGt > 0,
         !is.na(lengthM),   lengthM   > 0) %>%
  ggplot(aes(x = lengthM, y = tonnageGt)) +
  theme_bw() +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, colour = "steelblue") +
  scale_y_log10() +
  labs(x = "Length (m)", y = "GT (log scale)",
       title = "GT vs length for vessels with both fields",
       subtitle = "Outliers from this relationship are likely registry errors")





# 18/5
# When did each vessel first appear in the GFW data?
vessel_first_appearance <- events_df %>%
  mutate(date = as.Date(start), year = year(date)) %>%
  group_by(vesselId) %>%
  summarise(first_year = min(year), .groups = "drop")

# Join to vessel_meta to get size class and flag
vessel_appearance_profile <- vessel_meta %>%
  distinct(vesselId, ssvid, vessel_flag, size_class, gt, length_m) %>%
  left_join(vessel_first_appearance, by = "vesselId") %>%
  filter(!is.na(first_year))

# Key question: do smaller vessels appear later?
vessel_appearance_profile %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  group_by(size_class, first_year) %>%
  summarise(n_vessels = n_distinct(ssvid), .groups = "drop") %>%
  ggplot(aes(x = first_year, y = n_vessels, fill = size_class)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~ size_class, scales = "free_y") +
  labs(x = "First year in GFW data", y = "Number of vessels",
       fill = "Size class",
       title = "Year of first appearance in GFW data by vessel size class",
       subtitle = "A peak in 2016-2019 for smaller size classes suggests AIS mandate effect")

# Summarise: median first appearance year by size class
vessel_appearance_profile %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  group_by(size_class) %>%
  summarise(
    n_vessels        = n_distinct(ssvid),
    median_first_yr  = median(first_year, na.rm = TRUE),
    pct_before_2016  = round(mean(first_year < 2016) * 100, 1),
    pct_2016_to_2019 = round(mean(first_year >= 2016 & 
                                    first_year <= 2019) * 100, 1),
    pct_after_2019   = round(mean(first_year > 2019) * 100, 1),
    .groups          = "drop"
  ) %>%
  print()

# Also check by flag — NLD and GBR may show different patterns
# if they had earlier national mandates
vessel_appearance_profile %>%
  filter(vessel_flag %in% c("NLD", "GBR", "BEL", "FRA", "DEU"),
         !is.na(size_class), size_class != "Unknown") %>%
  group_by(vessel_flag, size_class, first_year) %>%
  summarise(n_vessels = n_distinct(ssvid), .groups = "drop") %>%
  ggplot(aes(x = first_year, y = n_vessels, fill = size_class)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_grid(vessel_flag ~ size_class, scales = "free_y") +
  labs(x = "First year in GFW data", y = "Number of vessels",
       fill = "Size class",
       title = "First appearance year by flag and size class",
       subtitle = "National AIS mandate timing differences should be visible here")



# Derive first appearance from registry transmission dates
# (doesn't require events_df — safe to run before Step 4)
vessel_first_appearance <- gfw_vessel_periods %>%
  mutate(first_year = year(as.Date(substr(transmissionDateFrom, 1, 10)))) %>%
  group_by(vesselId) %>%
  summarise(first_year = min(first_year, na.rm = TRUE), .groups = "drop")


# Join to vessel_meta to get size class and flag
vessel_appearance_profile <- vessel_meta %>%
  distinct(vesselId, ssvid, vessel_flag, size_class, gt, length_m) %>%
  left_join(vessel_first_appearance, by = "vesselId") %>%
  filter(!is.na(first_year))

vessel_appearance_profile %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  group_by(size_class, first_year) %>%
  summarise(n_vessels = n_distinct(ssvid), .groups = "drop") %>%
  ggplot(aes(x = first_year, y = n_vessels, fill = size_class)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~ size_class, scales = "free_y") +
  labs(x = "First year in GFW data", y = "Number of vessels",
       fill = "Size class",
       title = "Year of first appearance in GFW data by vessel size class",
       subtitle = "A peak in 2016-2019 for smaller size classes suggests AIS mandate effect")

# What is the actual earliest transmissionDateFrom in the registry?
gfw_vessel_periods %>%
  mutate(date_from = as.Date(substr(transmissionDateFrom, 1, 10))) %>%
  summarise(
    min_date   = min(date_from, na.rm = TRUE),
    max_date   = max(date_from, na.rm = TRUE),
    pct_before_2012 = round(mean(date_from < as.Date("2012-01-01")) * 100, 1)
  )

# How many vessels have transmissionDateFrom before 2012?
gfw_vessel_periods %>%
  mutate(date_from = as.Date(substr(transmissionDateFrom, 1, 10)),
         first_year = year(date_from)) %>%
  count(first_year) %>%
  arrange(first_year) %>%
  print(n = 20)







# ============================================================
# Speed test: gfw_event with vs without shapefile restriction
# ============================================================
# Tests on a small sample of vessels whether passing the study
# area polygon to gfw_event() reduces download time.
# Uses vessels already in vessel_meta so results are meaningful.

library(tictoc)

# Sample vessels — pick a mix of sizes and flags
test_vessels <- vessel_meta %>%
  distinct(vesselId, ssvid, vessel_flag, size_class) %>%
  filter(size_class != "Unknown") %>%
  group_by(size_class) %>%
  slice_sample(n = 2) %>%    # 2 per size class = 10 vessels total
  ungroup() %>%
  pull(vesselId)

message("Testing on ", length(test_vessels), " vessels")

# ---- Test A: without shapefile restriction ----
tic("Without shapefile")
events_no_filter <- map_dfr(test_vessels, function(vid) {
  Sys.sleep(0.5)
  tryCatch({
    result <- gfw_event(
      event_type = "FISHING",
      vessels    = vid,
      start_date = "2023-01-01",
      end_date   = "2023-12-31",
      key        = key
    )
    if (!is.null(result) && nrow(result) > 0) {
      result %>% mutate(vesselId = vid)
    } else {
      NULL
    }
  }, error = function(e) {
    message("  Failed: ", vid, " — ", e$message)
    NULL
  })
})
time_no_filter <- toc(log = TRUE, quiet = TRUE)

# ---- Test B: with shapefile restriction ----
tic("With shapefile")
events_with_filter <- map_dfr(test_vessels, function(vid) {
  Sys.sleep(0.5)
  tryCatch({
    result <- gfw_event(
      event_type    = "FISHING",
      vessels       = vid,
      start_date    = "2023-01-01",
      end_date      = "2023-12-31",
      region        = study_area_polygon,
      region_source = "USER_SHAPEFILE",
      key           = key
    )
    if (!is.null(result) && nrow(result) > 0) {
      result %>% mutate(vesselId = vid)
    } else {
      NULL
    }
  }, error = function(e) {
    message("  Failed: ", vid, " — ", e$message)
    NULL
  })
})
time_with_filter <- toc(log = TRUE, quiet = TRUE)

# ---- Compare results ----
message("\n========== SPEED TEST RESULTS ==========")
message("Without shapefile: ",
        round(time_no_filter$toc  - time_no_filter$tic,  1), " seconds | ",
        nrow(events_no_filter),   " events | ",
        n_distinct(events_no_filter$vesselId), " vessels")

message("With shapefile:    ",
        round(time_with_filter$toc - time_with_filter$tic, 1), " seconds | ",
        nrow(events_with_filter),  " events | ",
        n_distinct(events_with_filter$vesselId), " vessels")

message("Events reduction:  ",
        nrow(events_no_filter) - nrow(events_with_filter),
        " fewer events (",
        round((1 - nrow(events_with_filter) /
                 nrow(events_no_filter)) * 100, 1), "% reduction)")

message("Time saved per vessel: ",
        round((time_no_filter$toc  - time_no_filter$tic -
                 time_with_filter$toc + time_with_filter$tic) /
                length(test_vessels), 1), " seconds")

# ---- Extrapolate to full run ----
n_vessels_total <- n_distinct(vessel_meta$vesselId)
years           <- 14   # 2012-2025

time_per_vessel_no_filter   <- (time_no_filter$toc  - time_no_filter$tic)  /
  length(test_vessels)
time_per_vessel_with_filter <- (time_with_filter$toc - time_with_filter$tic) /
  length(test_vessels)

message("\n--- Extrapolated to full run (", n_vessels_total,
        " vessels × ", years, " years) ---")
message("Without shapefile: ",
        round(n_vessels_total * time_per_vessel_no_filter   *
                years / 3600, 1), " hours estimated")
message("With shapefile:    ",
        round(n_vessels_total * time_per_vessel_with_filter *
                years / 3600, 1), " hours estimated")

# ---- Sanity check: do both return the same events? ----
# Events in the study area should match between A and B
sf_use_s2(FALSE)
events_no_filter_study <-
  events_no_filter %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(division))
sf_use_s2(TRUE)

message("\n--- Sanity check: same events returned? ---")
message("Events in study area (no filter, post-clip): ",
        nrow(events_no_filter_study))
message("Events returned with filter:                 ",
        nrow(events_with_filter))
message("Difference (should be ~0):                   ",
        nrow(events_no_filter_study) - nrow(events_with_filter))





# ---- Pre-compute query year range per vesselId ----
# Only query years that overlap the vessel's transmission period.
# This avoids querying 2012-2025 for a vessel only active in 2019-2021.

vessel_year_ranges <- vessel_meta %>%
  mutate(
    year_from = year(as.Date(substr(transmissionDateFrom, 1, 10))),
    year_to   = year(as.Date(substr(transmissionDateTo,   1, 10)))
  ) %>%
  # Clip to analysis window 2012-2025
  mutate(
    year_from = pmax(year_from, 2012),
    year_to   = pmin(year_to,   2025)
  ) %>%
  filter(year_from <= year_to) %>%   # drop any outside window entirely
  group_by(vesselId) %>%
  summarise(
    year_from = min(year_from),
    year_to   = max(year_to),
    .groups   = "drop"
  )

# Check the distribution of query ranges
vessel_year_ranges %>%
  mutate(n_years = year_to - year_from + 1) %>%
  count(n_years) %>%
  mutate(
    pct          = round(n / sum(n) * 100, 1),
    total_calls  = n * n_years
  ) %>%
  arrange(n_years) %>%
  print()

# Total API calls with vs without year range optimisation
message("API calls WITHOUT year range: ",
        length(target_vessel_ids) * 14)
message("API calls WITH year range:    ",
        vessel_year_ranges %>%
          filter(vesselId %in% target_vessel_ids) %>%
          mutate(n_years = year_to - year_from + 1) %>%
          summarise(total = sum(n_years)) %>%
          pull(total))


vessel_year_ranges %>%
  filter(vesselId %in% target_vessel_ids) %>%
  mutate(n_years = year_to - year_from + 1) %>%
  count(n_years) %>%
  mutate(
    pct         = round(n / sum(n) * 100, 1),
    total_calls = n * n_years
  ) %>%
  arrange(n_years) %>%
  janitor::adorn_totals("row") %>%   # from janitor package
  print()

vessel_year_ranges %>%
  filter(vesselId %in% target_vessel_ids) %>%
  mutate(n_years = year_to - year_from + 1) %>%
  summarise(
    n_vessels     = n(),
    min_year_from = min(year_from),
    max_year_to   = max(year_to),
    min_n_years   = min(n_years),
    max_n_years   = max(n_years),
    mean_n_years  = round(mean(n_years), 1)
  ) %>%
  print()

# Any with year_from > year_to after clipping? Should be zero
vessel_year_ranges %>%
  filter(year_from > year_to) %>%
  nrow() %>%
  message("Vessels with invalid year range (should be 0): ", .)


# ---- Pre-run checklist ----

# 1. Confirm remaining_ids is correct
message("Vessels to query: ", length(remaining_ids))
message("Estimated API calls: ",
        vessel_year_ranges %>%
          filter(vesselId %in% remaining_ids) %>%
          mutate(n_years = year_to - year_from + 1) %>%
          summarise(total = sum(n_years)) %>%
          pull(total))

# 2. Confirm checkpoint file path is writable
tryCatch({
  saveRDS("test", file.path(flyshootdir, "write_test.rds"))
  file.remove(file.path(flyshootdir, "write_test.rds"))
  message("Output directory writable: OK")
}, error = function(e) {
  message("WARNING: cannot write to flyshootdir — ", e$message)
})

# 3. Confirm GFW key still valid with a quick test call
tryCatch({
  test <- suppressMessages(gfw_event(
    event_type = "FISHING",
    vessels    = remaining_ids[1],
    start_date = "2023-01-01",
    end_date   = "2023-01-07",
    key        = key
  ))
  message("GFW API key valid: OK")
}, error = function(e) {
  message("WARNING: GFW API test failed — ", e$message)
})

# 4. Prevent R session timeout if running overnight
# In RStudio: Tools → Global Options → R Sessions → 
#   uncheck "Suspend R sessions after N minutes of inactivity"
# Or keep a dummy loop running in a separate console:
# while(TRUE) { Sys.sleep(60); message(Sys.time()) }


# What does GFW know about this MMSI?
vessel_info_test <- gfw_vessel_info(
  query       = "227626720",
  search_type = "search",
  key         = key
)

# How many vesselIds are associated with this MMSI?
vessel_info_test$selfReportedInfo %>%
  dplyr::select(vesselId, ssvid, shipname,
                transmissionDateFrom, transmissionDateTo) %>%
  print()

# What does the registry say?
vessel_info_test$registryInfo %>%
  dplyr::select(any_of(c("vesselId", "ssvid", "shipname", "flag",
                         "geartypes", "tonnageGt", "lengthM"))) %>%
  print()

# Try querying by MMSI directly instead of vesselId
events_test <- suppressMessages(
  gfw_event(
    event_type = "FISHING",
    vessels    = "227626720",   # MMSI instead of vesselId
    start_date = "2018-01-01",
    end_date   = "2018-12-31",
    key        = key
  )
)
message("Events found by MMSI query: ",
        if (is.null(events_test)) 0 else nrow(events_test))



# VesselIds that actually generated events in discovery
vesselIds_with_events <- discovery_events %>%
  distinct(vesselId) %>%
  pull(vesselId)

message("vesselIds with discovery events:    ", length(vesselIds_with_events))
message("vesselIds in vessel_meta (all):     ", n_distinct(vessel_meta$vesselId))
message("vesselIds in vessel_meta (no events): ",
        n_distinct(vessel_meta$vesselId) - 
          length(intersect(vessel_meta$vesselId, vesselIds_with_events)))

# How many target_vessel_ids are NOT in discovery events?
not_in_discovery <- setdiff(target_vessel_ids, vesselIds_with_events)
message("target_vessel_ids not in discovery: ", length(not_in_discovery))

# Option A: restrict to only vesselIds seen in discovery events
target_vessel_ids_strict <- intersect(target_vessel_ids, vesselIds_with_events)

# Option B: also include vesselIds for MMSIs seen in discovery
# (catches name/flag changes for the same physical vessel)
mmsi_with_events <- discovery_events %>%
  distinct(vessel_ssvid) %>%
  pull(vessel_ssvid)

target_vessel_ids_broad <- vessel_meta %>%
  filter(ssvid %in% mmsi_with_events) %>%
  pull(vesselId) %>%
  unique()

message("\nOption A (strict — vesselId in discovery):  ",
        length(target_vessel_ids_strict), " vesselIds | ",
        round(length(target_vessel_ids_strict) * 6.4 * 30 / 3600, 1), " hours est.")
message("Option B (broad — MMSI in discovery):       ",
        length(target_vessel_ids_broad), " vesselIds | ",
        round(length(target_vessel_ids_broad) * 6.4 * 30 / 3600, 1), " hours est.")


# Correct estimate using actual year ranges per vesselId

# Option A
calls_A <- vessel_year_ranges %>%
  filter(vesselId %in% target_vessel_ids_strict) %>%
  mutate(n_years = year_to - year_from + 1) %>%
  summarise(total_calls = sum(n_years)) %>%
  pull(total_calls)

# Option B  
calls_B <- vessel_year_ranges %>%
  filter(vesselId %in% target_vessel_ids_broad) %>%
  mutate(n_years = year_to - year_from + 1) %>%
  summarise(total_calls = sum(n_years)) %>%
  pull(total_calls)

# Already completed
calls_done <- vessel_year_ranges %>%
  filter(vesselId %in% completed_ids) %>%
  mutate(n_years = year_to - year_from + 1) %>%
  summarise(total_calls = sum(n_years)) %>%
  pull(total_calls)

message("Seconds per API call (from speed test): ~30s")
message("")
message("Option A — strict (vesselId in discovery):")
message("  Total API calls:     ", calls_A)
message("  Already done:        ", calls_done)
message("  Remaining calls:     ", calls_A - calls_done)
message("  Estimated runtime:   ",
        round((calls_A - calls_done) * 24 / 3600, 1), " hours")
message("")
message("Option B — broad (MMSI in discovery):")
message("  Total API calls:     ", calls_B)
message("  Already done:        ", calls_done)
message("  Remaining calls:     ", calls_B - calls_done)
message("  Estimated runtime:   ",
        round((calls_B - calls_done) * 24 / 3600, 1), " hours")





library(furrr)
library(future)
library(progressr)

# ---- Derive remaining vessels to process ----

# Vessels that survived vessel_meta cleaning and were in discovery events
target_vessel_ids_strict <- intersect(
  unique(vessel_meta$vesselId),
  unique(discovery_events$vesselId)
)

# Vessels already processed (from existing events_df if loaded)
completed_ids <- if (exists("events_df") && nrow(events_df) > 0) {
  unique(events_df$vesselId)
} else if (file.exists(file.path(flyshootdir, "gfw_s4_events.RData"))) {
  tmp <- new.env()
  load(file.path(flyshootdir, "gfw_s4_events.RData"), envir = tmp)
  unique(tmp$events_df$vesselId)
} else {
  character(0)
}

# Remaining = strict target minus already completed
remaining_ids_to_run <- setdiff(target_vessel_ids_strict, completed_ids)

message("target_vessel_ids_strict: ", length(target_vessel_ids_strict))
message("Already completed:        ", length(completed_ids))
message("Remaining to run:         ", length(remaining_ids_to_run))

# Recalculate API calls and runtime
calls_remaining <- vessel_year_ranges %>%
  filter(vesselId %in% remaining_ids_to_run) %>%
  mutate(n_years = year_to - year_from + 1) %>%
  summarise(total = sum(n_years)) %>%
  pull(total)

message("Remaining API calls:      ", calls_remaining)
message("Estimated runtime (1 worker):  ",
        round(calls_remaining * 24 / 3600, 1), " hours")
message("Estimated runtime (4 workers): ",
        round(calls_remaining * 24 / 3600 / 4, 1), " hours")

# ---- Set up parallel workers ----
n_cores   <- parallel::detectCores()
n_workers <- 4
plan(multisession, workers = n_workers)

message("Available cores:   ", n_cores)
message("Using workers:     ", n_workers)
message("Remaining vessels: ", length(remaining_ids_to_run))
message("Remaining calls:   ", calls_A - calls_done)
message("Estimated runtime: ",
        round((calls_A - calls_done) * 24 / 3600 / n_workers, 1),
        " hours with ", n_workers, " workers")

# vessel_lookup must exist before the parallel loop
vessel_lookup <- vessel_meta %>%
  distinct(vesselId, ssvid, vessel_name, vessel_flag, size_class) %>%
  left_join(vessel_year_ranges, by = "vesselId")

message("vessel_lookup rows: ", nrow(vessel_lookup))

# ---- Define vessel processing function ----
process_vessel <- function(vid, vessel_lookup, key_token) {
  
  library(gfwr)
  library(dplyr)
  library(purrr)
  
  # Authenticate in worker session using token directly
  key <- key_token
  
  # Get year range for this vessel
  vinfo  <- vessel_lookup %>% filter(vesselId == vid)
  y_from <- if (nrow(vinfo) > 0 && !is.na(vinfo$year_from)) vinfo$year_from else 2012
  y_to   <- if (nrow(vinfo) > 0 && !is.na(vinfo$year_to))   vinfo$year_to   else 2025
  vname  <- if (nrow(vinfo) > 0 && !is.na(vinfo$vessel_name)) vinfo$vessel_name else "unknown"
  vflag  <- if (nrow(vinfo) > 0 && !is.na(vinfo$vessel_flag)) vinfo$vessel_flag else "?"
  n_yrs  <- y_to - y_from + 1
  
  events_v <- map_dfr(y_from:y_to, function(y) {
    Sys.sleep(0.5)
    tryCatch({
      result <- suppressMessages(
        gfw_event(
          event_type = "FISHING",
          vessels    = vid,
          start_date = paste0(y, "-01-01"),
          end_date   = paste0(y, "-12-31"),
          key        = key
        )
      )
      if (!is.null(result) && nrow(result) > 0) result else NULL
    }, error = function(e) NULL)
  })
  
  n_events <- if (is.null(events_v) || nrow(events_v) == 0) 0 else nrow(events_v)
  
  # Return result with metadata
  list(
    vesselId = vid,
    vessel_name = vname,
    vessel_flag = vflag,
    year_from   = y_from,
    year_to     = y_to,
    n_years     = n_yrs,
    n_events    = n_events,
    events      = events_v
  )
}

# ---- Get GFW token for passing to workers ----
key_token <- Sys.getenv("GFW_TOKEN")
if (nchar(key_token) == 0) stop("GFW_TOKEN not found in environment")

# ---- Split into chunks for checkpointing ----
remaining_ids_to_run <- setdiff(target_vessel_ids_strict, completed_ids)
n_total       <- length(remaining_ids_to_run)
chunk_size    <- 100
vessel_chunks <- split(remaining_ids_to_run,
                       ceiling(seq_along(remaining_ids_to_run) / chunk_size))
n_chunks      <- length(vessel_chunks)

message(sprintf("Processing %d vessels in %d chunks of ~%d",
                n_total, n_chunks, chunk_size))

# ---- Chunk loop with progress reporting ----
chunk_log <- tibble()   # summary log across all chunks

for (chunk_i in seq_along(vessel_chunks)) {
  
  chunk_ids   <- vessel_chunks[[chunk_i]]
  chunk_start <- Sys.time()
  
  message(sprintf("\n[Chunk %d/%d] %d vessels | %s",
                  chunk_i, n_chunks, length(chunk_ids),
                  format(chunk_start, "%H:%M:%S")))
  
  # Run with progress bar
  with_progress({
    p <- progressor(steps = length(chunk_ids))
    
    chunk_results <- future_map(
      chunk_ids,
      function(vid) {
        p(sprintf("%s", vid))   # advances progress bar
        process_vessel(vid, vessel_lookup, key_token)
      },
      .options = furrr_options(
        seed     = TRUE,
        packages = c("gfwr", "dplyr", "purrr")
      )
    )
  })
  
  # Unpack results
  chunk_events <- bind_rows(
    keep(map(chunk_results, "events"), ~ !is.null(.x) && nrow(.x) > 0)
  )
  
  # Chunk summary
  chunk_summary <- tibble(
    chunk        = chunk_i,
    n_vessels    = length(chunk_ids),
    n_with_events = sum(map_int(chunk_results, "n_events") > 0),
    n_events     = sum(map_int(chunk_results, "n_events")),
    elapsed_min  = round(as.numeric(Sys.time() - chunk_start, units = "mins"), 1)
  )
  chunk_log <- bind_rows(chunk_log, chunk_summary)
  
  message(sprintf("  Vessels with events: %d/%d | Events: %d | Time: %.1f min",
                  chunk_summary$n_with_events,
                  chunk_summary$n_vessels,
                  chunk_summary$n_events,
                  chunk_summary$elapsed_min))
  
  # Append to events_df and save checkpoint
  if (!is.null(chunk_events) && nrow(chunk_events) > 0) {
    events_df <- if (exists("events_df") && nrow(events_df) > 0) {
      bind_rows(events_df, chunk_events)
    } else {
      chunk_events
    }
  }
  
  save(events_df,
       file = file.path(flyshootdir, "gfw_s4_events.RData"))
  
  # Overall progress estimate
  vessels_done  <- chunk_i * chunk_size
  vessels_left  <- n_total - vessels_done
  rate_per_min  <- chunk_summary$n_vessels / chunk_summary$elapsed_min
  eta_hours     <- round(vessels_left / rate_per_min / 60, 1)
  
  message(sprintf("  Overall: %d/%d vessels done | ETA: %.1f hours | Total events: %d",
                  min(vessels_done, n_total), n_total,
                  eta_hours,
                  nrow(events_df)))
  
  # Print running chunk log every 5 chunks
  if (chunk_i %% 5 == 0) {
    message("\n  --- Chunk log so far ---")
    print(chunk_log)
  }
}

# ---- Finalise ----
plan(sequential)

message("\n========== STEP 4 COMPLETE ==========")
message("Total events:    ", nrow(events_df))
message("Total vesselIds: ", n_distinct(events_df$vesselId))
message("Chunk summary:")
print(chunk_log)

save(events_df,
     file = file.path(flyshootdir, "gfw_s4_events.RData"))
message("Final save: gfw_s4_events.RData")



# Verify events_df before proceeding
message("events_df summary:")
message("  Total events:      ", nrow(events_df))
message("  Unique vesselIds:  ", n_distinct(events_df$vesselId))
message("  Unique MMSIs:      ", n_distinct(events_df$vessel_ssvid))
message("  Date range:        ",
        min(as.Date(events_df$start)), " to ",
        max(as.Date(events_df$start)))

# Check year coverage
events_df %>%
  mutate(year = year(as.Date(start))) %>%
  count(year) %>%
  arrange(year) %>%
  print(n = 20)

# Check flag coverage
events_df %>%
  count(vessel_flag, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print(n = 15)














# ---- Step 5 diagnostics ----

# 1. Check fao_sf_division structure
message("fao_sf_division:")
print(fao_sf_division)
names(fao_sf_division)
st_crs(fao_sf_division)

# 2. Check events CRS and a sample of coordinates
message("\nevents_df coordinate sample:")
events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  slice_sample(n = 10) %>%
  select(vesselId, lat, lon) %>%
  print()

# 3. Check coordinate ranges — should be within study area
events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  summarise(
    lat_min = min(lat), lat_max = max(lat),
    lon_min = min(lon), lon_max = max(lon)
  ) %>%
  print()

# 4. Check fao_sf_division bounding box
st_bbox(fao_sf_division)

# 5. Try a small test join on 100 events
events_test <- events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  slice_sample(n = 100) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

st_crs(events_test)
st_crs(fao_sf_division)

# Are the CRS identical?
message("CRS match: ", st_crs(events_test) == st_crs(fao_sf_division))

# 6. Test join on small sample
test_join <- events_test %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

message("Test join — division column present: ",
        "division" %in% names(test_join))
message("Test join — events with division: ",
        sum(!is.na(test_join$division)), " / 100")

# 7. Check what columns fao_sf_division actually has after join
names(test_join)

# 8. Plot a sample to visually check overlap
library(ggplot2)
ggplot() +
  geom_sf(data = fao_sf_division, fill = "lightblue",
          alpha = 0.5, colour = "blue") +
  geom_point(data = events_df %>%
               filter(!is.na(lat), !is.na(lon)) %>%
               slice_sample(n = 1000),
             aes(x = lon, y = lat),
             size = 0.5, alpha = 0.3, colour = "red") +
  labs(title = "Study divisions vs event locations",
       subtitle = "Blue = fao_sf_division | Red = events sample")





# 1. Plot the divisions to see exactly what area they cover
ggplot() +
  geom_sf(data = fao_sf_division, aes(fill = division),
          alpha = 0.4, colour = "blue", linewidth = 0.5) +
  geom_sf(data = world, fill = "grey90", colour = "grey70") +
  coord_sf(xlim = c(-12, 10), ylim = c(46, 58), expand = FALSE) +
  geom_text(data = events_df %>%
              filter(!is.na(lat), !is.na(lon)) %>%
              slice_sample(n = 500),
            aes(x = lon, y = lat), label = ".", colour = "red") +
  labs(title = "fao_sf_division coverage vs event locations",
       subtitle = "Check if polygons cover the full division areas")

# 2. How many events fall within the bounding box of fao_sf_division?
bbox <- st_bbox(fao_sf_division)
events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  filter(lon >= bbox["xmin"], lon <= bbox["xmax"],
         lat >= bbox["ymin"], lat <= bbox["ymax"]) %>%
  nrow() %>%
  message("Events within bounding box: ", .)

# 3. Check the original fao_sf for 27.4.C extent
fao_sf %>%
  filter(F_LEVEL == "DIVISION") %>%
  mutate(division = toupper(F_DIVISION)) %>%
  filter(division %in% study_divisions) %>%
  st_bbox() %>%
  print()

# 4. Check if fao_sf was already cropped before you filtered it
fao_sf %>%
  filter(F_LEVEL == "DIVISION") %>%
  mutate(division = toupper(F_DIVISION)) %>%
  filter(division == "27.4.C") %>%
  st_bbox() %>%
  print()

# 5. Try st_intersects instead of st_within — less strict
events_test <- events_df %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  slice_sample(n = 1000) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

test_within     <- events_test %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()
test_intersects <- events_test %>%
  st_join(fao_sf_division, join = st_intersects) %>%
  st_drop_geometry()

message("st_within    — events with division: ",
        sum(!is.na(test_within$division)), " / 1000")
message("st_intersects — events with division: ",
        sum(!is.na(test_intersects$division)), " / 1000")





# Check current S2 setting
sf_use_s2()

# Try the join with S2 disabled
sf_use_s2(FALSE)

test_s2off <- events_test %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

message("S2 OFF — events with division: ",
        sum(!is.na(test_s2off$division)), " / 1000")

sf_use_s2(TRUE)

# Also try with S2 on for comparison
test_s2on <- events_test %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

message("S2 ON  — events with division: ",
        sum(!is.na(test_s2on$division)), " / 1000")




# Check what columns come back from the join
test_raw <- events_test %>%
  st_join(fao_sf_division, join = st_within)

message("Columns after join: ", paste(names(test_raw), collapse = ", "))
message("Rows after join: ", nrow(test_raw))

# Check if fao_sf_division geometry is valid
message("fao_sf_division valid: ", all(st_is_valid(fao_sf_division)))
st_is_valid(fao_sf_division)

# Check if events_test geometry is valid
message("events_test valid: ", all(st_is_valid(events_test)))

# Try the reverse — join events TO divisions
# instead of divisions TO events
test_reverse <- fao_sf_division %>%
  st_join(events_test, join = st_contains) %>%
  st_drop_geometry()

message("Reverse join — rows: ", nrow(test_reverse))
message("Reverse join columns: ", paste(names(test_reverse), collapse = ", "))

# Try a completely manual point-in-polygon test
# Pick one event that visually falls inside 27.4.C
test_point <- st_point(c(4.0, 52.0)) %>%   # clearly inside 27.4.C
  st_sfc(crs = 4326)

message("Test point (4E, 52N) within 27.4.C: ",
        st_within(test_point,
                  fao_sf_division %>% filter(division == "27.4.C"),
                  sparse = FALSE)[1,1])

message("Test point (4E, 52N) intersects 27.4.C: ",
        st_intersects(test_point,
                      fao_sf_division %>% filter(division == "27.4.C"),
                      sparse = FALSE)[1,1])

# Check if fao_sf_division needs st_make_valid
fao_sf_division_valid <- st_make_valid(fao_sf_division)
message("After st_make_valid, valid: ",
        all(st_is_valid(fao_sf_division_valid)))

sf_use_s2(FALSE)
test_valid <- events_test %>%
  st_join(fao_sf_division_valid, join = st_within) %>%
  st_drop_geometry()
sf_use_s2(TRUE)

message("With st_make_valid — events with division: ",
        sum(!is.na(test_valid$division)), " / 1000")







# Does events_test already have a division column?
message("Columns in events_test: ", paste(names(events_test), collapse = ", "))

# Does events_df have a division column?
message("'division' in events_df: ", "division" %in% names(events_df))

# If yes — what values does it have?
if ("division" %in% names(events_df)) {
  events_df %>%
    count(division, sort = TRUE) %>%
    print()
}





events_df %>%
  count(division, sort = TRUE) %>%
  print()

# Plot a sample to visually check overlap
library(ggplot2)
ggplot() +
  geom_sf(data = fao_sf_division, fill = "lightblue",
          alpha = 0.5, colour = "blue") +
  geom_point(data = events_marine_clean %>%
               filter(!is.na(lat), !is.na(lon)) %>%
               slice_sample(n = 1000),
             aes(x = lon, y = lat),
             size = 0.5, alpha = 0.3, colour = "red") +
  labs(title = "Study divisions vs event locations",
       subtitle = "Blue = fao_sf_division | Red = events sample")


# How many unique vessels have NA gear?
fishing_days_study %>%
  filter(is.na(gear)) %>%
  distinct(ssvid, vessel_name, vessel_flag, size_class) %>%
  count(vessel_flag, size_class) %>%
  arrange(desc(n))

# How many fishing days are affected?
fishing_days_study %>%
  group_by(has_gear = !is.na(gear)) %>%
  summarise(
    fishing_days = sum(fishing_days),
    n_vessels    = n_distinct(ssvid),
    pct_days     = round(fishing_days / sum(fishing_days_study$fishing_days) * 100, 1),
    .groups      = "drop"
  )

# Are the NA gear vessels new — not in the correction files?
na_gear_ssvids <- fishing_days_study %>%
  filter(is.na(gear)) %>%
  distinct(ssvid) %>%
  pull(ssvid)

# Check if they were in the previous correction files
missing_gear_corrections %>%
  filter(ssvid %in% na_gear_ssvids) %>%
  nrow() %>%
  message("NA gear vessels already in correction file: ", .)

message("NA gear vessels NOT in any correction file: ",
        sum(!na_gear_ssvids %in% missing_gear_corrections$ssvid))



















# 6NM boundaries France

library(sf)
library(archive)

# Download the shapefile zip directly from data.gouv.fr
tmp <- tempfile(fileext = ".zip")
download.file(
  "https://www.data.gouv.fr/api/1/datasets/r/0e3a9c3b-ab5c-4c81-8bab-3b9e1818b9fb",
  tmp, mode = "wb"
)

# Inspect contents
archive(tmp)

# Extract to a temp directory
exdir <- tempdir()
archive_extract(tmp, dir = exdir)

# List what was extracted
list.files(exdir, recursive = TRUE, pattern = "\\.shp$")

fr_6nm <- st_read(file.path(exdir, "PECHE-DPMA/2_Lim_6_milles/SHAPE/Limite_6milles_bande_cotiere_peche_WGS84_EPSG4326.shp"))
fr_3nm <- st_read(file.path(exdir, "PECHE-DPMA/1_Lim_3_milles/SHAPE/Limite_3milles_peche_WGS84_EPSG4326.shp"))

france_hr <- world |> filter(grepl("France",         name, ignore.case = TRUE)) |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52))
uk_hr     <- world |> filter(grepl("United Kingdom", name, ignore.case = TRUE)) |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52))
ci <- ne_countries(scale = "medium", returnclass = "sf", type = "map_units") |>
  filter(grepl("Jersey|Guernsey", name, ignore.case = TRUE))

# 12NM territorial sea polygons for France and UK (already in territorial_sea)
ts_france <- territorial_sea |> filter(grepl("France",          TERRITORY1, ignore.case = TRUE)) 
ts_uk     <- territorial_sea |> filter(grepl("United Kingdom",  TERRITORY1, ignore.case = TRUE)) 
ts_jersey   <- territorial_sea |> filter(TERRITORY1 == "Jersey")
ts_guernsey <- territorial_sea |> filter(TERRITORY1 == "Guernsey")

# save to GIS folder


# Check what you have
print(fr_6nm)
st_crs(fr_6nm)  

ggplot() +
  geom_sf(data = fr_3nm, aes(colour = "3 NM limit"), linewidth = 0.6) +
  geom_sf(data = fr_6nm, aes(colour = "6 NM limit"), linewidth = 0.6) +
  scale_colour_manual(
    name = "CFP fishing limits",
    values = c("3 NM limit" = "#e41a1c", "6 NM limit" = "#377eb8")
  ) +
  labs(
    title    = "French CFP Fishing Limits",
    subtitle = "Inner limits of coastal fishing bands (DPMA/SHOM)",
    caption  = "Source: Ministère de l'Agriculture et de l'Alimentation / DPMA - Shom, 2020"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-7, 5), ylim = c(48, 52), expand = FALSE)





library(sf)
library(lwgeom)
library(archive)
library(ggplot2)
library(dplyr)

# ============================================================
# STEP 1 — Download and read French CFP limit lines
# ============================================================
tmp <- tempfile(fileext = ".7z")
download.file(
  "https://www.data.gouv.fr/api/1/datasets/r/0e3a9c3b-ab5c-4c81-8bab-3b9e1818b9fb",
  tmp, mode = "wb"
)
exdir <- tempdir()
archive_extract(tmp, dir = exdir)

fr_3nm <- st_read(file.path(exdir, "PECHE-DPMA/1_Lim_3_milles/SHAPE/Limite_3milles_peche_WGS84_EPSG4326.shp"))
fr_6nm <- st_read(file.path(exdir, "PECHE-DPMA/2_Lim_6_milles/SHAPE/Limite_6milles_bande_cotiere_peche_WGS84_EPSG4326.shp"))

# ============================================================
# STEP 2 — Prepare French 12NM territorial sea as base polygon
# ============================================================
fr_12nm <- territorial_sea |>
  filter(grepl("France", TERRITORY1, ignore.case = TRUE)) |>
  st_make_valid() |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_union() |>
  st_make_valid()

# ============================================================
# STEP 3 — Project everything to LAEA (metres) for splitting
# ============================================================
fr_12nm_proj <- st_transform(fr_12nm, 3035)
fr_3nm_proj  <- st_transform(fr_3nm,  3035)
fr_6nm_proj  <- st_transform(fr_6nm,  3035)

# Clip 3NM line to metropolitan extent before using as blade
fr_3nm_metro <- fr_3nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

# ============================================================
# STEP 4 — Split 12NM polygon by the 3NM line
# ============================================================
fr_3nm_blade <- fr_3nm_metro |>
  st_union() |>
  st_buffer(100) |>
  st_boundary() |>
  st_cast("MULTILINESTRING")

split_3nm <- st_split(fr_12nm_proj, fr_3nm_blade) |>
  st_collection_extract("POLYGON") |>
  st_as_sf()
split_3nm$area <- st_area(split_3nm)

# Classify: inner (0-3NM) touches land, outer (3-12NM) does not
france_land <- world |>
  filter(grepl("France", name, ignore.case = TRUE)) |>
  st_transform(3035) |>
  st_crop(st_bbox(fr_12nm_proj)) |>
  st_make_valid()

touches_land       <- st_intersects(split_3nm, france_land, sparse = FALSE)
split_3nm$is_inner <- apply(touches_land, 1, any)

zone_0_3nm  <- split_3nm |> filter(is_inner)  |> st_union() |> st_as_sf()
zone_3_12nm <- split_3nm |> filter(!is_inner) |> st_union() |> st_as_sf()

# ============================================================
# STEP 5 — Split 3-12NM band by the 6NM line (Normandy only)
# ============================================================
fr_6nm_blade <- fr_6nm_proj |>
  st_union() |>
  st_buffer(100) |>
  st_boundary() |>
  st_cast("MULTILINESTRING")

# Crop outer zone to the extent of the 6NM line
bbox_6nm           <- st_buffer(fr_6nm_proj, 20000) |> st_bbox()
zone_3_12nm_norm   <- zone_3_12nm |> st_crop(bbox_6nm) |> st_make_valid()

split_6nm <- st_split(st_geometry(zone_3_12nm_norm), fr_6nm_blade) |>
  st_collection_extract("POLYGON") |>
  st_as_sf()
split_6nm$area <- st_area(split_6nm)

# Classify: inner (3-6NM) touches 0-3NM zone, outer (6-12NM) does not
touches_inner       <- st_intersects(split_6nm, zone_0_3nm, sparse = FALSE)
split_6nm$is_inner  <- apply(touches_inner, 1, any)

zone_3_6nm  <- split_6nm |> filter(is_inner)  |> st_union() |> st_as_sf()
zone_6_12nm <- split_6nm |> filter(!is_inner) |> st_union() |> st_as_sf()

# ============================================================
# STEP 6 — Back to WGS84
# ============================================================
zone_0_3nm  <- st_transform(zone_0_3nm,  4326)
zone_3_12nm <- st_transform(zone_3_12nm, 4326)
zone_3_6nm  <- st_transform(zone_3_6nm,  4326)
zone_6_12nm <- st_transform(zone_6_12nm, 4326)

# ============================================================
# STEP 7 — Plot
# ============================================================
ggplot() +
  geom_sf(data = france_hr,   fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,          fill = "grey85", colour = "grey50", linewidth = 0.3) +
  # geom_sf(data = zone_3_12nm, aes(fill = "3–12 NM"), colour = NA, alpha = 0.4) +
  # geom_sf(data = zone_6_12nm, aes(fill = "6–12 NM"), colour = NA, alpha = 0.4) +
  # geom_sf(data = zone_3_6nm,  aes(fill = "3–6 NM"),  colour = NA, alpha = 0.4) +
  # geom_sf(data = zone_0_3nm,  aes(fill = "0–3 NM"),  colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,      colour = "#e41a1c", linewidth = 0.6) +
  geom_sf(data = fr_6nm,      colour = "#ff7f00", linewidth = 0.6) +
  geom_sf(data = ts_jersey,   fill = NA, colour = "#4daf4a", linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = ts_guernsey, fill = NA, colour = "#4daf4a", linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = ts_uk,       fill = NA, colour = "#4daf4a", linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = territorial_sea,  fill="blue", colour = "blue", alpha = 0.4, size=0.5) +
  geom_sf(data = internal_waters,  fill="purple", colour = "purple", size = 0.5, alpha=0.4) +
  scale_fill_manual(
    name   = "CFP zone",
    values = c(
      "0–3 NM"  = "#e41a1c",
      "3–6 NM"  = "#ff7f00",
      "3–12 NM" = "#377eb8",
      "6–12 NM" = "#984ea3"
    )
  ) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(
    title    = "French CFP Coastal Fishing Zones",
    subtitle = "VLIZ 12NM territorial sea split by official DPMA/SHOM limit lines",
    caption  = "Source: VLIZ Marine Regions v4; DPMA/SHOM 2020"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
















library(sf)
library(lwgeom)
library(dplyr)

# ============================================================
# STEP 1 — Prepare base layers, all in LAEA (EPSG:3035)
# ============================================================
fr_3nm_proj  <- st_transform(fr_3nm,  3035)
fr_6nm_proj  <- st_transform(fr_6nm,  3035)

# French territorial sea only
ts_france_proj <- territorial_sea |>
  filter(grepl("France", TERRITORY1, ignore.case = TRUE)) |>
  st_make_valid() |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035) |>
  st_union() |>
  st_make_valid()

# French internal waters only
iw_france_proj <- internal_waters |>
  filter(grepl("France", TERRITORY1, ignore.case = TRUE)) |>
  st_make_valid() |>
  st_transform(3035) |>
  st_union() |>
  st_make_valid()

# ============================================================
# STEP 2 — 0-3NM zone
# = territorial sea MINUS (territorial sea eroded by 3NM inward)
# i.e. just the outer 3NM strip, then clip to where line is defined
# ============================================================

nm_to_m <- 1852  # 1 nautical mile in metres

# Erode the territorial sea inward by 3NM to get the "beyond 3NM" part
ts_eroded_3nm <- ts_france_proj |>
  st_buffer(-3 * nm_to_m) |>   # negative buffer = erode inward
  st_make_valid()

# The 0-3NM strip = territorial sea minus the eroded part
zone_0_3nm_raw <- ts_france_proj |>
  st_difference(ts_eroded_3nm) |>
  st_make_valid()

# Remove internal waters to avoid Brittany overextension
zone_0_3nm <- zone_0_3nm_raw |>
  st_difference(iw_france_proj) |>
  st_make_valid()

# Clip to where the official 3NM line is defined (metropolitan France extent)
fr_3nm_metro_proj <- fr_3nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

bbox_3nm <- st_bbox(st_buffer(fr_3nm_metro_proj, 10000))
zone_0_3nm <- zone_0_3nm |> st_crop(bbox_3nm) |> st_make_valid()

# ============================================================
# STEP 3 — 3-6NM zone (Normandy only)
# = 6NM strip minus 3NM strip
# ============================================================
ts_eroded_6nm <- ts_france_proj |>
  st_buffer(-6 * nm_to_m) |>
  st_make_valid()

zone_0_6nm_raw <- ts_france_proj |>
  st_difference(ts_eroded_6nm) |>
  st_make_valid()

zone_3_6nm <- zone_0_6nm_raw |>
  st_difference(
    st_union(zone_0_3nm |> st_transform(3035), iw_france_proj)
  ) |>
  st_make_valid()

# Clip to Normandy (where 6NM line is defined)
fr_6nm_proj_crop <- fr_6nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

bbox_6nm   <- st_bbox(st_buffer(fr_6nm_proj_crop, 10000))
zone_3_6nm <- zone_3_6nm |> st_crop(bbox_6nm) |> st_make_valid()

# ============================================================
# STEP 4 — Back to WGS84
# ============================================================
zone_0_3nm <- st_transform(zone_0_3nm, 4326)
zone_3_6nm <- st_transform(zone_3_6nm, 4326)

# ============================================================
# STEP 5 — Plot
# ============================================================
ggplot() +
  geom_sf(data = france_hr,   fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,          fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ts_france,   fill = NA, colour = "#e41a1c", linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = ts_uk,       fill = NA, colour = "#377eb8", linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = ts_jersey,   fill = NA, colour = "#4daf4a", linewidth = 0.4, linetype = "dashed") +
  geom_sf(data = ts_guernsey, fill = NA, colour = "#4daf4a", linewidth = 0.4, linetype = "dashed") +
  # geom_sf(data = zone_3_6nm,  aes(fill = "3–6 NM"),  colour = NA, alpha = 0.5) +
  geom_sf(data = zone_0_3nm,  aes(fill = "0–3 NM"),  colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,      colour = "#e41a1c", linewidth = 0.6) +
  geom_sf(data = fr_6nm,      colour = "#ff7f00", linewidth = 0.6) +
  scale_fill_manual(
    name   = "CFP zone",
    values = c("0–3 NM" = "#e41a1c", "3–6 NM" = "#ff7f00")
  ) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(
    title    = "French CFP Coastal Fishing Zones",
    subtitle = "Derived from VLIZ territorial sea, internal waters, and DPMA/SHOM limit lines",
    caption  = "Source: VLIZ Marine Regions v4; DPMA/SHOM 2020"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ==================================================================================

library(patchwork)

# Plot each intermediate step
p1 <- ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = st_transform(ts_france_proj, 4326), 
          fill = "lightblue", colour = "blue", alpha = 0.4) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(title = "1. French territorial sea (ts_france_proj)") +
  theme_minimal()

p2 <- ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = st_transform(ts_eroded_3nm, 4326), 
          fill = "orange", colour = "darkorange", alpha = 0.4) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(title = "2. Territorial sea eroded by 3NM (ts_eroded_3nm)") +
  theme_minimal()

p3 <- ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = st_transform(zone_0_3nm_raw, 4326), 
          fill = "#e41a1c", colour = NA, alpha = 0.4) +
  geom_sf(data = fr_3nm, colour = "#e41a1c", linewidth = 0.6) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(title = "3. Raw 0-3NM strip (ts minus eroded)") +
  theme_minimal()

p4 <- ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = st_transform(iw_france_proj, 4326), 
          fill = "purple", colour = "purple", alpha = 0.4) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(title = "4. French internal waters (iw_france_proj)") +
  theme_minimal()

p5 <- ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_0_3nm |> st_transform(3035) |> 
            st_transform(4326),  # before crop
          fill = "#e41a1c", colour = NA, alpha = 0.4) +
  geom_sf(data = fr_3nm, colour = "#e41a1c", linewidth = 0.6) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(title = "5. After removing internal waters") +
  theme_minimal()

p6 <- ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_0_3nm, fill = "#e41a1c", colour = NA, alpha = 0.4) +
  geom_sf(data = fr_3nm, colour = "#e41a1c", linewidth = 0.6) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  labs(title = "6. Final zone_0_3nm (after bbox crop)") +
  theme_minimal()

(p1 + p2) / (p3 + p4) / (p5 + p6)



# ============================================================
# FRENCH CFP 0-3NM COASTAL ZONE — FULL PIPELINE
# ============================================================
library(sf)
library(lwgeom)
library(archive)
library(ggplot2)
library(dplyr)

# ── STEP 1: Download French CFP limit lines ──────────────────
tmp <- tempfile(fileext = ".7z")
download.file(
  "https://www.data.gouv.fr/api/1/datasets/r/0e3a9c3b-ab5c-4c81-8bab-3b9e1818b9fb",
  tmp, mode = "wb"
)
exdir <- tempdir()
archive_extract(tmp, dir = exdir)

fr_3nm <- st_read(file.path(exdir,
                            "PECHE-DPMA/1_Lim_3_milles/SHAPE/Limite_3milles_peche_WGS84_EPSG4326.shp"))
fr_6nm <- st_read(file.path(exdir,
                            "PECHE-DPMA/2_Lim_6_milles/SHAPE/Limite_6milles_bande_cotiere_peche_WGS84_EPSG4326.shp"))

# ── STEP 2: Project limit lines to LAEA ──────────────────────
fr_3nm_proj <- fr_3nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

fr_6nm_proj <- fr_6nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

# ── STEP 3: Prepare supporting layers in LAEA ────────────────
fao_study_proj <- fao_sf_division |>
  filter(division %in% c("27.4.C", "27.7.D", "27.7.E")) |>
  st_transform(3035) |>
  st_make_valid()

france_land_proj <- world |>
  filter(grepl("France", name, ignore.case = TRUE)) |>
  st_transform(3035) |>
  st_make_valid()

ts_uk_proj       <- ts_uk       |> st_transform(3035) |> st_make_valid()
ts_jersey_proj   <- ts_jersey   |> st_transform(3035) |> st_make_valid()
ts_guernsey_proj <- ts_guernsey |> st_transform(3035) |> st_make_valid()

# ── STEP 4: Define split_division() helper ───────────────────
split_division <- function(fao_div, fr_3nm_proj, france_land_proj) {
  bbox_lines <- fao_div |> st_bbox() |> st_as_sfc() |> st_boundary()
  blade <- bind_rows(
    fr_3nm_proj |> st_union() |> st_as_sf(),
    bbox_lines   |> st_as_sf()
  ) |>
    st_union() |>
    st_buffer(100) |>
    st_boundary() |>
    st_cast("MULTILINESTRING")
  
  result <- tryCatch(
    st_split(fao_div |> st_union(), blade) |>
      st_collection_extract("POLYGON") |>
      st_as_sf(),
    error = function(e) NULL
  )
  if (is.null(result)) return(NULL)
  
  touches <- st_intersects(result, france_land_proj, sparse = FALSE)
  result$is_inner <- apply(touches, 1, any)
  result |> filter(is_inner)
}

# ── STEP 5: Define bboxes and extract coastal polygons ───────

# 27.7.E — Brittany/Atlantic
bbox_7e <- st_bbox(c(xmin = -6, xmax = 1.0, ymin = 47.5, ymax = 50.0),
                   crs = 4326) |> st_as_sfc() |> st_transform(3035)
fao_7e <- fao_study_proj |> filter(division == "27.7.E") |>
  st_intersection(bbox_7e) |> st_make_valid()

# 27.7.D — Channel (remove UK/Channel Islands territorial seas)
bbox_7d <- st_bbox(c(xmin = -5, xmax = 2.5, ymin = 47.5, ymax = 51.0),
                   crs = 4326) |> st_as_sfc() |> st_transform(3035)
fao_7d <- fao_study_proj |> filter(division == "27.7.D") |>
  st_intersection(bbox_7d) |>
  st_difference(ts_uk_proj) |>
  st_difference(ts_jersey_proj) |>
  st_difference(ts_guernsey_proj) |>
  st_make_valid()

# 27.4.C — Eastern Channel / Cap Gris-Nez (tight bbox)
bbox_4c <- st_bbox(c(xmin = 0.5, xmax = 2.5, ymin = 50.9, ymax = 51.25),
                   crs = 4326) |> st_as_sfc() |> st_transform(3035)
fao_4c <- fao_study_proj |> filter(division == "27.4.C") |>
  st_intersection(bbox_4c) |> st_make_valid()

# ── STEP 6: Split each division by the 3NM line ──────────────
zone_7e <- split_division(fao_7e, fr_3nm_proj, france_land_proj)
zone_7d <- split_division(fao_7d, fr_3nm_proj, france_land_proj)
zone_4c <- split_division(fao_4c, fr_3nm_proj, france_land_proj)

# ── STEP 7: Combine into final 0-3NM zone ────────────────────
zone_0_3nm <- bind_rows(zone_7e, zone_7d, zone_4c) |>
  st_union() |> st_as_sf() |>
  st_make_valid() |>
  st_transform(4326)

# ── STEP 8: Plot ──────────────────────────────────────────────
ggplot() +
  geom_sf(data = france_hr,   fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,          fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ts_france,   fill = NA, colour = "#e41a1c", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = ts_uk,       fill = NA, colour = "#377eb8", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = ts_jersey,   fill = NA, colour = "#4daf4a", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = ts_guernsey, fill = NA, colour = "#4daf4a", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = zone_0_3nm,  aes(fill = "0–3 NM"), colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,      colour = "#e41a1c", linewidth = 0.7) +
  geom_sf(data = fr_6nm,      colour = "#ff7f00", linewidth = 0.7) +
  scale_fill_manual(name = "CFP zone", values = c("0–3 NM" = "#e41a1c")) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "French CFP 0-3NM Coastal Zone",
    subtitle = "FAO division coastlines split by official DPMA/SHOM 3NM limit line",
    caption  = "Source: VLIZ Marine Regions v4; DPMA/SHOM 2020; ICES FAO divisions"
  )



# ── STEP 9: Generate 3-6NM zone (Normandy only) ──────────────

# ============================================================
# FRENCH CFP 3-6NM ZONE — FULL PIPELINE
# ============================================================

# ── STEP 1: Project lines to LAEA ────────────────────────────
fr_3nm_proj <- fr_3nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

fr_6nm_proj <- fr_6nm |>
  st_crop(c(xmin = -6, xmax = 5, ymin = 47.5, ymax = 52)) |>
  st_transform(3035)

# ── STEP 2: Extract single linestrings ───────────────────────
fr_3nm_line <- fr_3nm_proj |> st_union() |> st_cast("LINESTRING")
fr_6nm_line <- fr_6nm_proj |> st_union() |> st_cast("LINESTRING")

# ── STEP 3: Get endpoints of the 6NM line ────────────────────
coords_6nm   <- st_coordinates(fr_6nm_line)
pt_6nm_start <- st_point(coords_6nm[1, c("X", "Y")])                |> st_sfc(crs = 3035)
pt_6nm_end   <- st_point(coords_6nm[nrow(coords_6nm), c("X", "Y")]) |> st_sfc(crs = 3035)

# ── STEP 4: Find nearest points on 3NM line (local search) ───
search_radius <- 50000  # 50km

nearest_start <- st_nearest_points(
  pt_6nm_start,
  fr_3nm_proj |> st_union() |>
    st_intersection(st_buffer(pt_6nm_start, search_radius))
) |> st_cast("POINT")

nearest_end <- st_nearest_points(
  pt_6nm_end,
  fr_3nm_proj |> st_union() |>
    st_intersection(st_buffer(pt_6nm_end, search_radius))
) |> st_cast("POINT")

pt_3nm_near_start <- nearest_start[2] |> st_sfc(crs = 3035)
pt_3nm_near_end   <- nearest_end[2]   |> st_sfc(crs = 3035)

# ── STEP 5: Extract 3NM segment between snap points ──────────
coords_3nm <- st_coordinates(fr_3nm_line) |> as.data.frame()

idx_start <- which.min(
  (coords_3nm$X - st_coordinates(pt_3nm_near_start)[1])^2 +
    (coords_3nm$Y - st_coordinates(pt_3nm_near_start)[2])^2
)
idx_end <- which.min(
  (coords_3nm$X - st_coordinates(pt_3nm_near_end)[1])^2 +
    (coords_3nm$Y - st_coordinates(pt_3nm_near_end)[2])^2
)

i1 <- min(idx_start, idx_end)
i2 <- max(idx_start, idx_end)

fr_3nm_segment <- st_linestring(
  as.matrix(coords_3nm[i1:i2, c("X", "Y")])
) |> st_sfc(crs = 3035)

# ── STEP 6 (revised): Build connectors using actual point coordinates ─────

# Extract coordinates directly from the named points
xy_6nm_start  <- st_coordinates(pt_6nm_start)
xy_6nm_end    <- st_coordinates(pt_6nm_end)
xy_3nm_start  <- st_coordinates(pt_3nm_near_start)
xy_3nm_end    <- st_coordinates(pt_3nm_near_end)

# Connect pt_6nm_start to pt_3nm_near_start
connector_start <- st_linestring(rbind(
  c(xy_6nm_start[1], xy_6nm_start[2]),
  c(xy_3nm_start[1], xy_3nm_start[2])
)) |> st_sfc(crs = 3035)

# Connect pt_6nm_end to pt_3nm_near_end
connector_end <- st_linestring(rbind(
  c(xy_6nm_end[1], xy_6nm_end[2]),
  c(xy_3nm_end[1], xy_3nm_end[2])
)) |> st_sfc(crs = 3035)

# Check
ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = fr_3nm,    colour = "#e41a1c", linewidth = 0.7) +
  geom_sf(data = fr_6nm,    colour = "#ff7f00", linewidth = 0.7) +
  geom_sf(data = connector_start |> st_transform(4326),
          colour = "blue", linewidth = 1) +
  geom_sf(data = connector_end |> st_transform(4326),
          colour = "blue", linewidth = 1) +
  geom_point(aes(x = xy_6nm_start[1],  y = xy_6nm_start[2]),
             data = data.frame(), colour = "darkblue", size = 3,
             inherit.aes = FALSE) +
  geom_point(aes(x = xy_6nm_end[1],    y = xy_6nm_end[2]),
             data = data.frame(), colour = "darkblue", size = 3,
             inherit.aes = FALSE) +
  geom_point(aes(x = xy_3nm_start[1],  y = xy_3nm_start[2]),
             data = data.frame(), colour = "darkred", size = 3,
             inherit.aes = FALSE) +
  geom_point(aes(x = xy_3nm_end[1],    y = xy_3nm_end[2]),
             data = data.frame(), colour = "darkred", size = 3,
             inherit.aes = FALSE) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.2, 51.5),
           crs = 4326) +
  theme_minimal() +
  labs(title = "Connectors — using direct point coordinates")

# ── STEP 7: Polygonise ───────────────────────────────────────
all_lines <- c(
  st_geometry(fr_6nm_line),
  st_geometry(fr_3nm_segment),
  st_geometry(connector_start),
  st_geometry(connector_end)
) |>
  st_set_crs(3035) |>
  st_union()

zone_3_6nm <- st_polygonize(all_lines) |>
  st_collection_extract("POLYGON") |>
  st_as_sf() |>
  st_make_valid() |>
  st_transform(4326)

cat("Number of polygons:", nrow(zone_3_6nm), "\n")

# ── STEP 8: Plot ──────────────────────────────────────────────
ggplot() +
  geom_sf(data = france_hr,   fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,          fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ts_france,   fill = NA, colour = "#e41a1c", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = ts_uk,       fill = NA, colour = "#377eb8", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = ts_jersey,   fill = NA, colour = "#4daf4a", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = ts_guernsey, fill = NA, colour = "#4daf4a", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = zone_3_6nm,  aes(fill = "3–6 NM"), colour = NA, alpha = 0.5) +
  geom_sf(data = zone_0_3nm,  aes(fill = "0–3 NM"), colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,      colour = "#e41a1c", linewidth = 0.7) +
  geom_sf(data = fr_6nm,      colour = "#ff7f00", linewidth = 0.7) +
  scale_fill_manual(
    name   = "CFP zone",
    values = c("0–3 NM" = "#e41a1c", "3–6 NM" = "#ff7f00")
  ) +
  coord_sf(xlim = c(-6, 5), ylim = c(47.5, 52)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "French CFP Coastal Zones",
    subtitle = "0-3NM and 3-6NM zones from FAO coastlines + DPMA/SHOM limit lines",
    caption  = "Source: VLIZ Marine Regions v4; DPMA/SHOM 2020; ICES FAO divisions"
  )








# Extract the 3NM segment between the two snap points
fr_3nm_segment_wgs <- fr_3nm_segment |> st_transform(4326)

# Get coordinates for labelling (in WGS84 for plotting)
coords_pt_6nm_start <- st_coordinates(pt_6nm_start |> st_transform(4326))
coords_pt_6nm_end   <- st_coordinates(pt_6nm_end   |> st_transform(4326))
coords_pt_3nm_start <- st_coordinates(pt_3nm_near_start |> st_transform(4326))
coords_pt_3nm_end   <- st_coordinates(pt_3nm_near_end   |> st_transform(4326))








ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,     fill = "grey85", colour = "grey50", linewidth = 0.3) +
  # Full 3NM line in light grey for context
  geom_sf(data = fr_3nm,    colour = "grey70", linewidth = 0.7) +
  # 6NM line in orange
  geom_sf(data = fr_6nm,    colour = "#ff7f00", linewidth = 0.9) +
  # Highlighted 3NM segment in green
  geom_sf(data = fr_3nm_segment |> st_transform(4326),
          colour = "darkgreen", linewidth = 1.5) +
  # Connectors in blue (using sfc objects with transform)
  geom_sf(data = connector_start |> st_transform(4326), colour = "blue", linewidth = 1.0) +
  geom_sf(data = connector_end   |> st_transform(4326), colour = "blue", linewidth = 1.0) +
  # 6NM endpoints
  geom_sf(data = pt_6nm_start |> st_transform(4326), colour = "darkblue", size = 3) +
  geom_sf(data = pt_6nm_end   |> st_transform(4326), colour = "darkblue", size = 3) +
  # 3NM snap points
  geom_sf(data = pt_3nm_near_start |> st_transform(4326), colour = "darkred", size = 3) +
  geom_sf(data = pt_3nm_near_end   |> st_transform(4326), colour = "darkred", size = 3) +
  # Labels using sf label
  geom_sf_label(data = pt_6nm_start |> st_transform(4326),
                label = "pt_6nm_start",    nudge_y =  0.12, size = 3) +
  geom_sf_label(data = pt_6nm_end   |> st_transform(4326),
                label = "pt_6nm_end",      nudge_y =  0.12, size = 3) +
  geom_sf_label(data = pt_3nm_near_start |> st_transform(4326),
                label = "pt_3nm_near_start", nudge_y = -0.12, size = 3) +
  geom_sf_label(data = pt_3nm_near_end   |> st_transform(4326),
                label = "pt_3nm_near_end",   nudge_y = -0.12, size = 3) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "3-6NM zone components",
       subtitle = "orange=6NM | green=3NM segment | blue=connectors | grey=rest of 3NM")









# ── Force exact coordinate matching at all junction points ───
# Get exact coordinates from each named point
xy_6nm_start <- st_coordinates(pt_6nm_start)
xy_6nm_end   <- st_coordinates(pt_6nm_end)

# Use coords_3nm[i1] and coords_3nm[i2] as the authoritative 3NM snap coords
xy_3nm_i1 <- c(coords_3nm[i1, "X"], coords_3nm[i1, "Y"])
xy_3nm_i2 <- c(coords_3nm[i2, "X"], coords_3nm[i2, "Y"])

# Rebuild 6NM line with snapped start/end
coords_6nm_snapped <- coords_6nm
coords_6nm_snapped[1, c("X","Y")]              <- c(xy_6nm_start[1], xy_6nm_start[2])
coords_6nm_snapped[nrow(coords_6nm), c("X","Y")] <- c(xy_6nm_end[1],   xy_6nm_end[2])

fr_6nm_line_snapped <- st_linestring(
  as.matrix(coords_6nm_snapped[, c("X","Y")])
)

# Rebuild 3NM segment with exact endpoint coords
fr_3nm_segment_snapped <- st_linestring(
  as.matrix(coords_3nm[i1:i2, c("X","Y")])
)

# Connectors — use EXACT same coords as line endpoints
connector_start_snapped <- st_linestring(rbind(
  c(xy_6nm_start[1], xy_6nm_start[2]),  # matches fr_6nm start
  xy_3nm_i2                              # matches fr_3nm_segment end
))

connector_end_snapped <- st_linestring(rbind(
  c(xy_6nm_end[1], xy_6nm_end[2]),      # matches fr_6nm end
  xy_3nm_i1                              # matches fr_3nm_segment start
))

# Build closed ring manually by concatenating coordinates
# Order: 6NM start -> along 6NM -> 6NM end -> connector -> 3NM snap -> along 3NM -> 3NM snap -> connector -> back to start
ring_coords <- rbind(
  as.matrix(coords_6nm_snapped[, c("X","Y")]),          # 6NM line
  xy_3nm_i2,                                             # connector end point
  as.matrix(coords_3nm[i2:i1, c("X","Y")]),             # 3NM segment reversed
  c(xy_6nm_start[1], xy_6nm_start[2])                   # close ring back to start
)

zone_3_6nm_ring <- st_polygon(list(ring_coords)) |>
  st_sfc(crs = 3035) |>
  st_make_valid() |>
  st_transform(4326)

# Check
ggplot() +
  geom_sf(data = france_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,           fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,              fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_3_6nm_ring, fill = "#ff7f00", colour = NA, alpha = 0.5) +
  geom_sf(data = zone_0_3nm,      fill = "#e41a1c", colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,          colour = "#e41a1c", linewidth = 0.7) +
  geom_sf(data = fr_6nm,          colour = "#ff7f00", linewidth = 0.7) +
  
  # Connectors in blue (using sfc objects with transform)
  geom_sf(data = connector_start |> st_transform(4326), colour = "blue", linewidth = 1.0) +
  geom_sf(data = connector_end   |> st_transform(4326), colour = "blue", linewidth = 1.0) +
  # 6NM endpoints
  geom_sf(data = pt_6nm_start |> st_transform(4326), colour = "darkblue", size = 3) +
  geom_sf(data = pt_6nm_end   |> st_transform(4326), colour = "darkblue", size = 3) +
  # 3NM snap points
  geom_sf(data = pt_3nm_near_start |> st_transform(4326), colour = "darkred", size = 3) +
  geom_sf(data = pt_3nm_near_end   |> st_transform(4326), colour = "darkred", size = 3) +
  
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "3-6NM zone as polygon")














# Check which idx corresponds to which end
cat("idx_start corresponds to pt_3nm_near_start at:",
    coords_3nm[idx_start, "X"], coords_3nm[idx_start, "Y"], "\n")
cat("idx_end corresponds to pt_3nm_near_end at:",
    coords_3nm[idx_end, "X"], coords_3nm[idx_end, "Y"], "\n")
cat("xy_3nm_start:", xy_3nm_start, "\n")
cat("xy_3nm_end:",   xy_3nm_end, "\n")
cat("pt_6nm_start:", xy_6nm_start, "\n")
cat("pt_6nm_end:",   xy_6nm_end, "\n")

# Build ring_coords with section labels
n_6nm    <- nrow(coords_6nm)
n_3nm_seg <- length(idx_end:idx_start)

ring_coords_df <- rbind(
  # Section 1: along 6NM line
  data.frame(
    X       = coords_6nm[, "X"],
    Y       = coords_6nm[, "Y"],
    section = "1_6nm_line",
    point_id = 1:n_6nm
  ),
  # Section 2: connector from 6NM end to 3NM idx_end
  data.frame(
    X       = coords_3nm[idx_end, "X"],
    Y       = coords_3nm[idx_end, "Y"],
    section = "2_connector_end",
    point_id = n_6nm + 1
  ),
  # Section 3: along 3NM from idx_end back to idx_start
  data.frame(
    X       = coords_3nm[idx_end:idx_start, "X"],
    Y       = coords_3nm[idx_end:idx_start, "Y"],
    section = "3_3nm_segment",
    point_id = (n_6nm + 2):(n_6nm + 1 + n_3nm_seg)
  ),
  # Section 4: connector from 3NM idx_start back to 6NM start
  data.frame(
    X       = coords_6nm[1, "X"],
    Y       = coords_6nm[1, "Y"],
    section = "4_connector_start",
    point_id = n_6nm + 2 + n_3nm_seg
  )
)

cat("Section point counts:\n")
print(table(ring_coords_df$section))

# Plot coloured by section
ring_points_sf <- ring_coords_df |>
  st_as_sf(coords = c("X", "Y"), crs = 3035) |>
  st_transform(4326)

ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,     fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = fr_3nm,    colour = "#e41a1c", linewidth = 0.5) +
  geom_sf(data = fr_6nm,    colour = "#ff7f00", linewidth = 0.5) +
  geom_sf(data = ring_points_sf, aes(colour = section), size = 0.8, alpha=0.5) +
  scale_colour_manual(values = c(
    "1_6nm_line"        = "#ff7f00",
    "2_connector_end"   = "blue",
    "3_3nm_segment"     = "#e41a1c",
    "4_connector_start" = "darkblue"
  )) +
  # Label start of each section
  # geom_sf_label(
  #   data = ring_points_sf |>
  #     group_by(section) |> slice(1),
  #   aes(label = section), size = 2.5
  # ) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "Ring sections — coloured by component")



zone_3_6nm_ring <- st_polygon(list(ring_coords)) |>
  st_sfc(crs = 3035) |>
  st_make_valid() |>
  st_transform(4326)

# Check
ggplot() +
  geom_sf(data = france_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,           fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,              fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_3_6nm_ring, fill = "#ff7f00", colour = NA, alpha = 0.5) +
  geom_sf(data = zone_0_3nm,      fill = "#e41a1c", colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,          colour = "#e41a1c", linewidth = 0.7) +
  geom_sf(data = fr_6nm,          colour = "#ff7f00", linewidth = 0.7) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "3-6NM zone — corrected ring sequence")





# Export ring_coords as sf points for inspection
ring_points <- ring_coords |>
  as.data.frame() |>
  setNames(c("X", "Y")) |>
  mutate(point_id = row_number()) |>
  st_as_sf(coords = c("X", "Y"), crs = 3035) |>
  st_transform(4326)

# Plot with point numbers to find the outlier
ggplot() +
  geom_sf(data = france_hr,   fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_3_6nm_ring, fill = "#ff7f00", colour = NA, alpha = 0.3) +
  geom_sf(data = fr_3nm,      colour = "#e41a1c", linewidth = 0.5) +
  geom_sf(data = fr_6nm,      colour = "#ff7f00", linewidth = 0.5) +
  geom_sf(data = ring_points, colour = "blue", size = 1) +
  # Label every 500th point to avoid overplotting, plus first and last
  geom_sf_label(
    data = ring_points |> filter(point_id %in% c(1, seq(500, nrow(ring_points), 500), nrow(ring_points))),
    aes(label = point_id), size = 2.5, colour = "black"
  ) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "Ring coordinate points — numbered every 500",
       subtitle = paste("Total points:", nrow(ring_points)))








ring_coords_df <- rbind(
  # Section 1: along 6NM line from start to end
  data.frame(
    X       = coords_6nm[, "X"],
    Y       = coords_6nm[, "Y"],
    section  = "1_6nm_line",
    point_id = 1:nrow(coords_6nm)
  ),
  # Section 2: connector — FROM 6NM end TO 3NM idx_end
  # (coords_6nm last point -> coords_3nm[idx_end])
  data.frame(
    X        = coords_3nm[idx_end, "X"],
    Y        = coords_3nm[idx_end, "Y"],
    section  = "2_connector_end",
    point_id = nrow(coords_6nm) + 1
  ),
  # Section 3: along 3NM from idx_end back to idx_start
  data.frame(
    X        = coords_3nm[idx_end:idx_start, "X"],
    Y        = coords_3nm[idx_end:idx_start, "Y"],
    section  = "3_3nm_segment",
    point_id = (nrow(coords_6nm) + 2):(nrow(coords_6nm) + 1 + length(idx_end:idx_start))
  ),
  # Section 4: connector — FROM 3NM idx_start BACK TO 6NM start
  # (coords_3nm[idx_start] is already last point of section 3,
  #  so we only need to add the 6NM start point to close the ring)
  data.frame(
    X        = coords_6nm[1, "X"],
    Y        = coords_6nm[1, "Y"],
    section  = "4_connector_start",
    point_id = nrow(coords_6nm) + 2 + length(idx_end:idx_start)
  )
)

cat("Section point counts:\n")
print(table(ring_coords_df$section))

# Check points around the connectors
cat("\nPoints around connector_end (sections 1->2->3):\n")
print(ring_coords_df[
  ring_coords_df$section %in% c("1_6nm_line", "2_connector_end", "3_3nm_segment"),
] |> tail(3) |> rbind(
  ring_coords_df[ring_coords_df$section == "2_connector_end", ],
  ring_coords_df[ring_coords_df$section == "3_3nm_segment", ] |> head(3)
))

# Rebuild ring and polygon
ring_coords_matrix <- as.matrix(ring_coords_df[, c("X", "Y")])

zone_3_6nm_ring <- st_polygon(list(ring_coords_matrix)) |>
  st_sfc(crs = 3035) |>
  st_make_valid() |>
  st_transform(4326)

# Plot coloured by section
ring_points_sf <- ring_coords_df |>
  st_as_sf(coords = c("X", "Y"), crs = 3035) |>
  st_transform(4326)

ggplot() +
  geom_sf(data = france_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,           fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_3_6nm_ring, fill = "#ff7f00", colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,          colour = "#e41a1c", linewidth = 0.5) +
  geom_sf(data = fr_6nm,          colour = "#ff7f00", linewidth = 0.5) +
  geom_sf(data = ring_points_sf,  aes(colour = section), size = 0.8) +
  scale_colour_manual(values = c(
    "1_6nm_line"        = "#ff7f00",
    "2_connector_end"   = "blue",
    "3_3nm_segment"     = "#e41a1c",
    "4_connector_start" = "darkblue"
  )) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "3-6NM zone — corrected connectors")













# Calculate distance between consecutive points
ring_coords_df <- ring_coords_df |>
  mutate(
    X_next = lead(X),
    Y_next = lead(Y),
    dist_to_next = sqrt((X_next - X)^2 + (Y_next - Y)^2)
  )

# Find the largest jumps
ring_coords_df |>
  arrange(desc(dist_to_next)) |>
  select(point_id, section, X, Y, X_next, Y_next, dist_to_next) |>
  head(10)







# The 6NM line points are out of order — sort them by cumulative distance
coords_6nm_df <- as.data.frame(coords_6nm[, c("X","Y")])

# Calculate cumulative distance from first point
coords_6nm_df <- coords_6nm_df |>
  mutate(
    dX   = X - lag(X, default = first(X)),
    dY   = Y - lag(Y, default = first(Y)),
    dist = sqrt(dX^2 + dY^2),
    cumdist = cumsum(dist)
  )

# Plot the raw 6NM coordinates coloured by point order to see the jumps
ggplot() +
  geom_sf(data = france_hr, fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_path(data = coords_6nm_df |>
              st_as_sf(coords = c("X","Y"), crs = 3035) |>
              st_transform(4326) |>
              cbind(st_coordinates(st_transform(
                st_as_sf(coords_6nm_df, coords = c("X","Y"), crs = 3035), 4326))),
            aes(x = X, y = Y, colour = row_number(coords_6nm_df))) +
  scale_colour_viridis_c(name = "point order") +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "6NM line point order — are they sequential?")















# ── Fix: use line_merge to properly order the 6NM segments ───
fr_6nm_merged <- fr_6nm_proj |>
  st_union() |>
  st_line_merge() |>       # merges and orders connected line segments
  st_cast("LINESTRING")

# Check the result
coords_6nm <- st_coordinates(fr_6nm_merged) |> as.data.frame()

# Verify no more large jumps
coords_6nm_check <- coords_6nm |>
  mutate(
    dist_to_next = sqrt((lead(X) - X)^2 + (lead(Y) - Y)^2)
  ) |>
  arrange(desc(dist_to_next))

cat("Largest jumps after line_merge:\n")
print(head(coords_6nm_check[, c("X","Y","dist_to_next")], 5))

# Re-extract endpoints
pt_6nm_start <- st_point(as.numeric(coords_6nm[1, c("X","Y")])) |>
  st_sfc(crs = 3035)
pt_6nm_end   <- st_point(as.numeric(coords_6nm[nrow(coords_6nm), c("X","Y")])) |>
  st_sfc(crs = 3035)

# Re-find nearest points on 3NM line with local search
nearest_start <- st_nearest_points(
  pt_6nm_start,
  fr_3nm_proj |> st_union() |>
    st_intersection(st_buffer(pt_6nm_start, search_radius))
) |> st_cast("POINT")

nearest_end <- st_nearest_points(
  pt_6nm_end,
  fr_3nm_proj |> st_union() |>
    st_intersection(st_buffer(pt_6nm_end, search_radius))
) |> st_cast("POINT")

pt_3nm_near_start <- nearest_start[2] |> st_sfc(crs = 3035)
pt_3nm_near_end   <- nearest_end[2]   |> st_sfc(crs = 3035)

# Re-find indices on 3NM line
idx_start <- which.min(
  (coords_3nm$X - st_coordinates(pt_3nm_near_start)[1])^2 +
    (coords_3nm$Y - st_coordinates(pt_3nm_near_start)[2])^2
)
idx_end <- which.min(
  (coords_3nm$X - st_coordinates(pt_3nm_near_end)[1])^2 +
    (coords_3nm$Y - st_coordinates(pt_3nm_near_end)[2])^2
)

cat("idx_start:", idx_start, "idx_end:", idx_end, "\n")

# Rebuild ring
ring_coords_df <- rbind(
  data.frame(
    X = coords_6nm[, "X"], Y = coords_6nm[, "Y"],
    section = "1_6nm_line", point_id = 1:nrow(coords_6nm)
  ),
  data.frame(
    X = coords_3nm[idx_end, "X"], Y = coords_3nm[idx_end, "Y"],
    section = "2_connector_end", point_id = nrow(coords_6nm) + 1
  ),
  data.frame(
    X = coords_3nm[idx_end:idx_start, "X"],
    Y = coords_3nm[idx_end:idx_start, "Y"],
    section = "3_3nm_segment",
    point_id = (nrow(coords_6nm) + 2):(nrow(coords_6nm) + 1 + length(idx_end:idx_start))
  ),
  data.frame(
    X = coords_6nm[1, "X"], Y = coords_6nm[1, "Y"],
    section = "4_connector_start",
    point_id = nrow(coords_6nm) + 2 + length(idx_end:idx_start)
  )
)

# Check for large jumps
ring_coords_df <- ring_coords_df |>
  mutate(dist_to_next = sqrt((lead(X)-X)^2 + (lead(Y)-Y)^2))

cat("Largest jumps in ring:\n")
print(ring_coords_df |> arrange(desc(dist_to_next)) |>
        select(point_id, section, dist_to_next) |> head(5))

# Build polygon
zone_3_6nm_ring <- st_polygon(list(as.matrix(ring_coords_df[, c("X","Y")]))) |>
  st_sfc(crs = 3035) |>
  st_make_valid() |>
  st_transform(4326)

# Plot
ggplot() +
  geom_sf(data = france_hr,       fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = uk_hr,           fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = ci,              fill = "grey85", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = zone_3_6nm_ring, fill = "#ff7f00", colour = NA, alpha = 0.5) +
  geom_sf(data = zone_0_3nm,      fill = "#e41a1c", colour = NA, alpha = 0.5) +
  geom_sf(data = fr_3nm,          colour = "#e41a1c", linewidth = 0.7) +
  geom_sf(data = fr_6nm,          colour = "#ff7f00", linewidth = 0.7) +
  coord_sf(xlim = c(-1.5, 3.5), ylim = c(49.0, 51.8)) +
  theme_minimal() +
  labs(title = "3-6NM zone — after st_line_merge() fix")



gfw_vessel_registry <- local({
  e <- new.env()
  load(file.path(flyshootdir, "gfw_s2_registry.RData"), envir = e)
  e$gfw_vessel_registry
})

gfw_vessel_registry %>%
  filter(grepl("BONEFAAS", shipname, ignore.case = TRUE)) %>%
  dplyr::select(vesselId, ssvid, imo, shipname, flag,
                transmissionDateFrom, transmissionDateTo,
                tonnageGt, lengthM) %>%
  arrange(transmissionDateFrom) %>%
  print()


sum(is.na(gfw_vessel_registry$imo))
sum(!is.na(gfw_vessel_registry$imo))


# Vessels that changed flag while keeping the same MMSI
flag_changes <- gfw_vessel_registry %>%
  distinct(ssvid, flag, transmissionDateFrom, transmissionDateTo) %>%
  group_by(ssvid) %>%
  filter(n_distinct(flag) > 1) %>%
  arrange(ssvid, transmissionDateFrom) %>%
  summarise(
    n_flags      = n_distinct(flag),
    flags        = paste(unique(flag), collapse = " → "),
    date_from    = min(as.Date(substr(transmissionDateFrom, 1, 10))),
    date_to      = max(as.Date(substr(transmissionDateTo,   1, 10))),
    .groups      = "drop"
  ) %>%
  arrange(desc(n_flags))

message("MMSIs with flag change (same MMSI): ", nrow(flag_changes))
message("MMSIs with 2 flags:                 ",
        sum(flag_changes$n_flags == 2))
message("MMSIs with 3+ flags:                ",
        sum(flag_changes$n_flags >= 3))

# Show the flag change sequences
flag_changes %>%
  print(n = 30)

# For comparison: vessels that changed MMSI across flag change
# (like FRANK BONEFAAS — different ssvid for each flag period)
mmsi_changes <- gfw_vessel_registry %>%
  distinct(vesselId, ssvid, flag, transmissionDateFrom) %>%
  group_by(vesselId) %>%
  filter(n_distinct(ssvid) > 1) %>%
  summarise(
    n_mmsi    = n_distinct(ssvid),
    mmsis     = paste(unique(ssvid), collapse = " → "),
    n_flags   = n_distinct(flag),
    flags     = paste(unique(flag), collapse = " → "),
    .groups   = "drop"
  )

message("\nVesselIds with MMSI change: ", nrow(mmsi_changes))

# Summary: how are flag changes handled in your registry?
message("\n--- How flag changes are represented ---")
message("Same MMSI, flag changed:     ", nrow(flag_changes))
message("Different MMSI, flag changed: ", nrow(mmsi_changes))








# What fields does gfw_vessel_registry have?
names(gfw_vessel_registry)

# If imo exists, check FRANK BONEFAAS
gfw_vessel_registry %>%
  filter(grepl("BONEFAAS", shipname, ignore.case = TRUE)) %>%
  dplyr::select(any_of(c("vesselId", "ssvid", "shipname", "flag",
                         "imo", "callsign", "tonnageGt", "lengthM",
                         "transmissionDateFrom", "transmissionDateTo"))) %>%
  print()

# How many vessels in registry have an IMO number?
if ("imo" %in% names(gfw_vessel_registry)) {
  gfw_vessel_registry %>%
    summarise(
      n_total    = n_distinct(ssvid),
      n_with_imo = n_distinct(ssvid[!is.na(imo) & imo != ""]),
      pct        = round(n_with_imo / n_total * 100, 1)
    ) %>%
    print()
  
  # Vessels sharing an IMO but with different ssvid = flag changers
  imo_links <- gfw_vessel_registry %>%
    filter(!is.na(imo), imo != "") %>%
    group_by(imo) %>%
    filter(n_distinct(ssvid) > 1) %>%
    summarise(
      n_ssvid  = n_distinct(ssvid),
      ssvids   = paste(unique(ssvid),    collapse = " → "),
      n_flags  = n_distinct(flag),
      flags    = paste(unique(flag),     collapse = " → "),
      names    = paste(unique(shipname), collapse = " / "),
      .groups  = "drop"
    ) %>%
    arrange(desc(n_ssvid))
  
  message("Vessels with same IMO but different MMSI: ", nrow(imo_links))
  print(imo_links, n = 20)
}





# IMO coverage by size class and flag
gfw_vessel_registry %>%
  distinct(ssvid, flag, tonnageGt, lengthM, imo) %>%
  mutate(
    has_imo = !is.na(imo) & imo != "" & imo != "0",
    size_class = case_when(
      is.na(tonnageGt)    ~ "Unknown GT",
      tonnageGt < 100     ~ "S1 <100 GT",
      tonnageGt < 300     ~ "S2 100-300 GT",
      tonnageGt < 600     ~ "S3 300-600 GT",
      tonnageGt < 1200    ~ "S4 600-1200 GT",
      tonnageGt >= 1200   ~ "S5 >1200 GT"
    )
  ) %>%
  group_by(size_class) %>%
  summarise(
    n_vessels   = n_distinct(ssvid),
    pct_has_imo = round(mean(has_imo) * 100, 1),
    .groups     = "drop"
  ) %>%
  arrange(size_class) %>%
  print()

# And by flag
gfw_vessel_registry %>%
  distinct(ssvid, flag, imo) %>%
  mutate(has_imo = !is.na(imo) & imo != "" & imo != "0") %>%
  group_by(flag) %>%
  summarise(
    n          = n_distinct(ssvid),
    pct_has_imo = round(mean(has_imo) * 100, 1),
    .groups    = "drop"
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(pct_has_imo)) %>%
  print(n = 20)








# 22/5
# ==============================================================================
# Corrected fishing days — one day per vessel per calendar date,
# spatial zone assigned by majority rule (most GFW events on that date)
# ==============================================================================

# Step 1: for each vessel × date, pick the dominant zone attributes
# (the zone where most fishing events were recorded that day)

events_cv_daily <- events_classified %>%
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  filter(ssvid %in% cv_mmsi) %>%
  mutate(date = as.Date(start),
         year = year(date)) %>%
  filter(year >= year_start, year <= year_end)

# Count events per vessel × date × spatial combination
events_cv_daily_zones <- events_cv_daily %>%
  group_by(ssvid, date, year, division, eez, zone_detail, zone) %>%
  summarise(n_events = n(), .groups = "drop")

# For each vessel × date, keep only the row with the most events
# (majority zone rule — one row per vessel-date)
fishing_days_cv_corrected <- events_cv_daily_zones %>%
  group_by(ssvid, date, year) %>%
  slice_max(n_events, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Now aggregate to fishing days — each row IS one fishing day
  group_by(ssvid, division, eez, zone_detail, zone, year) %>%
  summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
  left_join(
    vessels_cv %>% dplyr::select(vessel, mmsi),
    by = c("ssvid" = "mmsi")
  ) %>%
  left_join(
    vessel_meta %>% distinct(ssvid, vessel_flag, gear, gt, size_class),
    by = "ssvid"
  ) %>%
  filter(!is.na(vessel))

message("Corrected CV fishing days: ",
        sum(fishing_days_cv_corrected$fishing_days), " total")
message("Original CV fishing days:  ",
        sum(fishing_days_cv_full$fishing_days), " total")
message("Difference: ",
        sum(fishing_days_cv_full$fishing_days) -
          sum(fishing_days_cv_corrected$fishing_days),
        " days removed by deduplication")

# Quick check: how many dates were split across multiple zones?
n_split_dates <- events_cv_daily_zones %>%
  group_by(ssvid, date) %>%
  filter(n_distinct(paste(division, eez, zone_detail)) > 1) %>%
  n_groups()

message("Vessel-dates spanning multiple zone combinations: ", n_split_dates)













