# ============================================================
# GFW Effort Analysis — gfwr v3.0
# Version 8
#
# Each slow step is controlled by a boolean flag at the top:
#   recreate_* = TRUE  → re-run the step and overwrite saved data
#   recreate_* = FALSE → load saved data from disk, skip the step
#
# Processing steps:
#   STEP 1  — Setup: libraries, paths, spatial layers, settings
#   STEP 2a — Vessel discovery: query GFW → mmsi_master
#   STEP 2b — Vessel registry: MMSI → vesselId + metadata
#   STEP 3  — Vessel metadata: build vessel_meta + corrections + GT imputation
#   STEP 4  — Fishing events: pull 2013-2025 per vesselId (parallel)
#   STEP 5  — Spatial filter: division join + EEZ + internal waters
#   STEP 6  — Zone classification: 0-12NM vs >12NM + French CFP sub-zones
#   STEP 7  — Fishing days: aggregate to day-level effort (majority-rule dedup)
#   STEP 8  — Diagnostics: completeness + AIS coverage checks
#   STEP 9  — Vessel coverage: PDF chart + Excel export
#   STEP 10 — Effort plots
#
# ============================================================


# ==============================================================================
# Step 10–12 standalone — loads only required objects
# No events_classified, no vessel_meta, no events_classified_fr
# ==============================================================================

library(tidyverse)
library(lubridate)
library(sf)
library(spatstat)      # masked KDE — install.packages("spatstat") if needed
library(rnaturalearth)
library(rnaturalearthdata)
library(ineq)

# ---- Paths (copy from Step 1) ----
spatialdir  <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

# ---- Settings (copy from Step 1) ----
study_years     <- 2013:2025
study_divisions <- c("27.4.C", "27.7.D", "27.7.E")
threshold_pct   <- 1
xlim <- c(-7, 8)
ylim <- c(48, 54)

flag_colours <- c(
  "BEL"   = "#4E79A7",
  "NLD"   = "#F28E2B",
  "GBR"   = "#59A14F",
  "FRA"   = "#E15759",
  "DEU"   = "#76B7B2",
  "Other" = "#BAB0AC"
)

size_colours <- c(
  "S1 <100 GT"     = "#1D9E75",
  "S2 100-300 GT"  = "#BA7517",
  "S3 300-600 GT"  = "#7F77DD",
  "S4 600-1200 GT" = "#D85A30",
  "S5 >1200 GT"    = "#A32D2D"
)

# ---- Helper functions ----
load_object <- function(filepath, object_name) {
  local({
    e <- new.env()
    load(filepath, envir = e)
    e[[object_name]]
  })
}

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

# ---- Load ONLY what is needed ----

# For Step 10 plots 1-8: fishing_days_study only
fishing_days_study <- load_object(
  file.path(flyshootdir, "gfw_s7_effort.RData"), "fishing_days_study")

events_classified_fr <- load_object(
  file.path(flyshootdir, "gfw_s6_zones.RData"), "events_classified_fr")

vessel_meta <- load_object(
  file.path(flyshootdir, "gfw_s3_vessel_meta.RData"), "vessel_meta")

# For spatial layers (Step 10 plots 9-10 and Step 11-12 maps)
world <- ne_countries(scale = "medium", returnclass = "sf")

fao_sf <- load_object(file.path(spatialdir, "fao_sf.RData"), "fao_sf")
fao_sf_division <- fao_sf %>%
  filter(F_LEVEL == "DIVISION") %>%
  dplyr::select(division = F_DIVISION) %>%
  mutate(division = toupper(division)) %>%
  filter(division %in% study_divisions) %>%
  group_by(division) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()
rm(fao_sf)   # free memory immediately after deriving what's needed

message("Objects loaded. fishing_days_study: ",
        nrow(fishing_days_study), " rows | ",
        n_distinct(fishing_days_study$ssvid), " vessels")



# ==============================================================================
# STEP 10 — Effort plots (Plots 1–8, fishing_days_study only)
# Plots 9 and 10 omitted — they require events_classified_fr which is large
# ==============================================================================

# ---- Plot 1: fishing days by year and flag (absolute) ----
fishing_days_study %>%
  filter(year %in% study_years) %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country")

# ---- Plot 2: by year, flag and division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days by year, country and ICES division") +
  facet_wrap(~ division)

# ---- Plot 3: % by year and flag ----
fishing_days_study %>%
  filter(year %in% study_years) %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days by year and country (%)")

# ---- Plot 4: % by year, flag and division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year, division) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days by year, country and ICES division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: by year and gear, faceted by division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(division), !is.na(gear)) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(gear) %>%
  mutate(total_gear_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(gear = if_else(total_gear_days / sum(fishing_days) * 100
                        < threshold_pct, "Other", gear)) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = gear)) +
  theme_bw() + geom_col() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Gear type",
       title = "Fishing days by year and gear type") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 6: GT fishing days by year and flag ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(gt_final)) %>%
  group_by(year, vessel_flag) %>%
  summarise(gt_fishing_days = sum(gt_fishing_days, na.rm = TRUE),
            .groups = "drop") %>%
  apply_flag_other(days_col = "gt_fishing_days") %>%
  group_by(year, vessel_flag) %>%
  summarise(gt_fishing_days = sum(gt_fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = gt_fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "GT fishing days", fill = "Flag",
       title = "GT fishing days by year and country")

# ---- Plot 7: by year and size class ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(size_class),
         size_class != "Unknown") %>%
  group_by(year, size_class) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = size_class)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Size class",
       title = "Fishing days by year and vessel size class")

# ---- Plot 8: size class by division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(size_class),
         size_class != "Unknown", !is.na(division)) %>%
  group_by(year, size_class, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = size_class)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Size class",
       title = "Fishing days by year, size class and ICES division") +
  facet_wrap(~ division)

# ---- Plot 9: fishing events map — flag ----
sample_size <- 20000
events_sample <- events_classified_fr %>%
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
            by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                   ~ "Other",
    vessel_flag %in% names(flag_colours) ~ vessel_flag,
    TRUE                                 ~ "Other"
  )) %>%
  slice_sample(n = sample_size)

ggplot() +
  theme_bw() +
  geom_sf(data = fao_sf_division, fill = NA, colour = "grey60",
          linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90", colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample,
             aes(x = lon, y = lat, colour = vessel_flag),
             size = 0.5, alpha = 0.9) +
  scale_colour_manual(values = flag_colours, na.value = "grey70") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom") +
  labs(colour = "Flag",
       title  = paste0("Fishing events — marine only (sample of ",sample_size,")"),
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E")

# ---- Plot 10: fishing events map — flag and year ----
sample_size <- 10000
events_sample_by_year <- events_classified_fr %>%
  mutate(year = lubridate::year(start)) %>% 
  filter(year %in% study_years) %>% 
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
            by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                   ~ "Other",
    vessel_flag %in% names(flag_colours) ~ vessel_flag,
    TRUE                                 ~ "Other"
  )) %>%
  group_by(year) %>% 
  slice_sample(n = sample_size)

ggplot() +
  theme_bw() +
  geom_sf(data = fao_sf_division, fill = NA,
          colour = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90",
          colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample_by_year,
             aes(x = lon, y = lat, colour = vessel_flag),
             size = 0.5, alpha = 0.9) +
  scale_colour_manual(values = flag_colours, na.value = "grey70") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom") +
  labs(colour = "Flag",
       title  = paste0("Fishing events — marine only (sample of ",sample_size,")"),
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E") +
  facet_wrap(~year)



# ==============================================================================
# STEP 11 — Density maps
# ==============================================================================

land_study <- world %>%
  st_crop(st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                    ymin = ylim[1], ymax = ylim[2]),
                  crs = 4326)) %>%
  st_union()

study_window_sf <- fao_sf_division %>%
  st_union() %>%
  st_difference(land_study) %>%   # punch land out of study area
  st_make_valid()


# ---- Project to UTM 30N (metres) for spatstat ----
crs_proj <- 32630   # UTM zone 30N — appropriate for North Sea / English Channel

study_window_proj <- study_window_sf %>%
  st_transform(crs_proj)

study_owin <- as.owin(study_window_proj)

# ---- Project events to same CRS before KDE ----
sample_size <- 10000   # per year — increase to 10000 if memory allows

events_for_kde <- events_classified_fr %>%
  mutate(year = lubridate::year(start)) %>%
  filter(year %in% study_years,
         !is.na(lon), !is.na(lat)) %>%
  dplyr::select(year, lon, lat) %>%
  group_by(year) %>%
  slice_sample(n = sample_size) %>%
  ungroup()

events_for_kde_proj <- events_for_kde %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs_proj) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# ---- Compute masked KDE per year ----
sigma_m <- 20000   # bandwidth in metres — 20 km; adjust to taste



# ---- Compute KDE — keep output in UTM, do NOT back-project ----
kde_results <- map_dfr(study_years, function(yr) {
  
  pts <- events_for_kde_proj %>% filter(year == yr)
  if (nrow(pts) < 50) return(NULL)
  
  ppp_yr <- tryCatch(
    ppp(x = pts$x, y = pts$y, window = study_owin),
    error = function(e) { message("ppp failed yr ", yr, ": ", e$message); NULL }
  )
  if (is.null(ppp_yr)) return(NULL)
  
  kde <- density(ppp_yr, sigma = sigma_m,
                 dimyx = c(200, 200),
                 edge  = TRUE)
  
  # Keep in UTM — regular grid, no reprojection
  as.data.frame(kde) %>%
    rename(x = x, y = y, density = value) %>%
    filter(!is.na(density), density > 0) %>%
    mutate(
      ndensity = density / max(density),
      year     = yr
    )
})

message("KDE grid rows: ", nrow(kde_results))

# ---- Project spatial layers to UTM for plotting ----
world_proj       <- world        %>% st_transform(crs_proj)
fao_div_proj     <- fao_sf_division %>% st_transform(crs_proj)

# ---- Plot 11: density map in UTM coordinates ----
ggplot() +
  theme_bw() +
  geom_tile(data = kde_results,
            aes(x = x, y = y, fill = ndensity)) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Relative\ndensity",
    limits   = c(0, 1),
    breaks   = c(0, 0.5, 1),
    labels   = c("Low", "Mid", "High"),
    na.value = "transparent"
  ) +
  geom_sf(data = world_proj, fill = "grey85",
          colour = "grey60", linewidth = 0.2) +
  geom_sf(data = fao_div_proj, fill = NA,
          colour = "grey30", linewidth = 0.4) +
  coord_sf(crs  = crs_proj,
           xlim = st_bbox(study_window_proj)[c("xmin", "xmax")],
           ylim = st_bbox(study_window_proj)[c("ymin", "ymax")],
           expand = TRUE) +
  theme(
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid       = element_line(colour = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7),
    strip.background = element_rect(fill = "grey90"),
    strip.text       = element_text(face = "bold", colour = "grey20"),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey50", size = 8)
  ) +
  labs(title    = "Fishing effort density — all fleets combined",
       subtitle = paste0("Masked KDE (sigma = ", sigma_m/1000, " km)  |  ",
                         "Land excluded  |  Normalised per year  |  ",
                         "Sample ", sample_size, " events/year")) +
  facet_wrap(~ year, ncol=5)


# now for 7d only

window_7d_proj <- window_7d_sf %>%
  st_transform(crs_proj)

window_7d_owin <- as.owin(window_7d_proj)

events_7d_proj <- events_7d_pts %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs_proj) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()


# ---- KDE for 7d — keep in UTM ----
bbox_7d <- st_bbox(fao_sf_division %>% filter(division == "27.7.D"))

land_7d <- world %>%
  st_crop(bbox_7d) %>%
  st_union()

window_7d_sf <- fao_sf_division %>%
  filter(division == "27.7.D") %>%
  st_difference(land_7d) %>%
  st_make_valid()

window_7d_proj <- window_7d_sf %>%
  st_transform(crs_proj)

window_7d_owin <- as.owin(window_7d_proj)

events_7d_pts <- events_classified_fr %>%
  mutate(year = lubridate::year(start)) %>%
  filter(year %in% study_years,
         division == "27.7.D",
         !is.na(lon), !is.na(lat)) %>%
  dplyr::select(year, lon, lat)

events_7d_proj <- events_7d_pts %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs_proj) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# ---- KDE for 7d — keep in UTM ----
kde_7d <- map_dfr(study_years, function(yr) {
  
  pts <- events_7d_proj %>% filter(year == yr)
  if (nrow(pts) < 50) return(NULL)
  
  ppp_yr <- tryCatch(
    ppp(x = pts$x, y = pts$y, window = window_7d_owin),
    error = function(e) { message("ppp failed yr ", yr, ": ", e$message); NULL }
  )
  if (is.null(ppp_yr)) return(NULL)
  
  kde <- density(ppp_yr, sigma = 10000,
                 dimyx = c(200, 200),
                 edge  = TRUE)
  
  as.data.frame(kde) %>%
    rename(x = x, y = y, density = value) %>%
    filter(!is.na(density), density > 0) %>%
    mutate(ndensity = density / max(density), year = yr)
})

bbox_7d_proj <- st_bbox(window_7d_proj)

# ---- Plot 12c: 7d density map in UTM ----
ggplot() +
  theme_bw() +
  geom_tile(data = kde_7d,
            aes(x = x, y = y, fill = ndensity)) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Relative\ndensity",
    limits   = c(0, 1),
    na.value = "transparent"
  ) +
  geom_sf(data = world_proj, fill = "grey40",
          colour = "grey60", linewidth = 0.2) +
  geom_sf(data = fao_div_proj %>% filter(division == "27.7.D"),
          fill = NA, colour = "white", linewidth = 0.5) +
  coord_sf(crs   = crs_proj,
           xlim  = bbox_7d_proj[c("xmin", "xmax")],
           ylim  = bbox_7d_proj[c("ymin", "ymax")],
           expand = TRUE) +
  theme(
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.grid       = element_line(colour = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7),
    strip.background = element_rect(fill = "grey90"),
    strip.text       = element_text(face = "bold", colour = "grey20"),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey50", size = 8)
  ) +
  labs(title    = "Fishing effort density — ICES 27.7.D, all fleets combined",
       subtitle = "Masked KDE (sigma = 10 km)  |  Land excluded  |  Normalised per year") +
  facet_wrap(~ year)




# ==============================================================================
# STEP 12 — 7d concentration analysis using events_classified_fr
# ==============================================================================

library(ineq)

# ---- Reuse events_7d_proj (already filtered to 27.7.D and projected) ----
# If not in memory, rebuild it:
# events_7d_pts <- events_classified_fr %>%
#   mutate(year = lubridate::year(start)) %>%
#   filter(year %in% study_years, division == "27.7.D",
#          !is.na(lon), !is.na(lat)) %>%
#   dplyr::select(year, lon, lat)
#
# events_7d_proj <- events_7d_pts %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   st_transform(crs_proj) %>%
#   mutate(x = st_coordinates(.)[,1],
#          y = st_coordinates(.)[,2]) %>%
#   st_drop_geometry()

# ---- Assign each event to a 5km × 5km grid cell (in UTM metres) ----
cell_size <- 5000   # metres — adjust to taste

events_7d_grid <- events_7d_proj %>%
  mutate(
    x_cell  = floor(x / cell_size) * cell_size + cell_size / 2,
    y_cell  = floor(y / cell_size) * cell_size + cell_size / 2,
    cell_id = paste(x_cell, y_cell, sep = "_")
  )

# ---- Effort per cell per year ----
cell_effort_7d <- events_7d_grid %>%
  group_by(year, cell_id, x_cell, y_cell) %>%
  summarise(n_events = n(), .groups = "drop")

# ---- Concentration metrics per year ----
concentration_7d <- cell_effort_7d %>%
  group_by(year) %>%
  summarise(
    n_cells      = n(),
    total_events = sum(n_events),
    gini         = ineq::Gini(n_events),
    top10_pct    = {
      s     <- sort(n_events, decreasing = TRUE)
      n_top <- max(1L, ceiling(0.10 * length(s)))
      sum(s[seq_len(n_top)]) / sum(s) * 100
    },
    n_cells_1pct = sum(n_events / total_events >= 0.01),
    .groups = "drop"
  )

message("\n===== 7d CONCENTRATION METRICS =====")
print(concentration_7d, n = 20)

# ---- Plot 12a: Gini coefficient over time ----
ggplot(concentration_7d, aes(x = year, y = gini)) +
  theme_bw() +
  geom_line(colour = "#E15759", linewidth = 1) +
  geom_point(colour = "#E15759", size = 3) +
  geom_text(aes(label = round(gini, 3)),
            vjust = -0.8, size = 2.8) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title  = element_text(face = "bold")) +
  labs(x        = "",
       y        = "Gini coefficient",
       title    = "Spatial concentration of fishing effort — ICES 27.7.D",
       subtitle = paste0("Gini coefficient across ", cell_size/1000,
                         " km grid cells  |  Higher = more concentrated",
                         "  |  All fleets combined"))

# ---- Plot 12b: % effort in top-10% cells ----
ggplot(concentration_7d, aes(x = year, y = top10_pct)) +
  theme_bw() +
  geom_line(colour = "#4E79A7", linewidth = 1) +
  geom_point(colour = "#4E79A7", size = 3) +
  geom_text(aes(label = paste0(round(top10_pct, 1), "%")),
            vjust = -0.8, size = 2.8) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0.05, 0.15))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title  = element_text(face = "bold")) +
  labs(x        = "",
       y        = "% of total effort",
       title    = paste0("Effort in top-10% most-fished ",
                         cell_size/1000, " km cells — ICES 27.7.D"),
       subtitle = "All fleets combined")

# ---- Plot 12c: both metrics combined on dual axis ----
# Useful for seeing whether Gini and top-10% tell the same story

conc_long <- concentration_7d %>%
  dplyr::select(year, gini, top10_pct) %>%
  pivot_longer(c(gini, top10_pct),
               names_to  = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         gini     = "Gini coefficient",
                         top10_pct = "Top-10% cell share (%)"))

ggplot(conc_long, aes(x = year, y = value,
                      colour = metric, group = metric)) +
  theme_bw() +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Gini coefficient"      = "#E15759",
                                 "Top-10% cell share (%)" = "#4E79A7")) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme(axis.text.x     = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title      = element_text(face = "bold")) +
  labs(x      = "", y = "Value", colour = "",
       title  = paste0("Effort concentration — ICES 27.7.D  |  ",
                       cell_size/1000, " km grid  |  All fleets"),
       subtitle = "Gini coefficient (0–1) and % effort in top-10% most-fished cells") +
  facet_wrap(~ metric, scales = "free_y")

# ---- Plot 12d: grid cell map — mean annual effort, all years ----
# Shows WHERE the persistent hotspots are

cell_effort_mean <- cell_effort_7d %>%
  group_by(cell_id, x_cell, y_cell) %>%
  summarise(mean_events = mean(n_events), .groups = "drop") %>%
  mutate(ndensity = mean_events / max(mean_events))

ggplot() +
  theme_bw() +
  geom_tile(data = cell_effort_mean,
            aes(x = x_cell, y = y_cell, fill = ndensity)) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Relative\neffort",
    limits   = c(0, 1),
    na.value = "transparent"
  ) +
  geom_sf(data = world_proj, fill = "grey85",
          colour = "grey60", linewidth = 0.2) +
  geom_sf(data = fao_div_proj %>% filter(division == "27.7.D"),
          fill = NA, colour = "grey30", linewidth = 0.5) +
  coord_sf(crs   = crs_proj,
           xlim  = bbox_7d_proj[c("xmin", "xmax")],
           ylim  = bbox_7d_proj[c("ymin", "ymax")],
           expand = TRUE) +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white"),
        panel.grid       = element_line(colour = "grey90"),
        legend.position  = "bottom",
        plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(colour = "grey50", size = 8)) +
  labs(title    = paste0("Mean fishing effort hotspots — ICES 27.7.D  |  ",
                         cell_size/1000, " km grid"),
       subtitle = "Mean events per cell averaged across all years  |  All fleets combined")



# ---- Plot 12e: grid cell map — mean annual effort, by year ----
# Shows WHERE the persistent hotspots are

# ---- Build complete grid covering 27.7.D extent ----
# Fill missing cells with zero so background is ocean-coloured, not white

x_range <- seq(
  floor(min(cell_effort_7d$x_cell) / cell_size) * cell_size,
  ceiling(max(cell_effort_7d$x_cell) / cell_size) * cell_size,
  by = cell_size
)
y_range <- seq(
  floor(min(cell_effort_7d$y_cell) / cell_size) * cell_size,
  ceiling(max(cell_effort_7d$y_cell) / cell_size) * cell_size,
  by = cell_size
)

# Full grid for every year
full_grid_7d <- expand_grid(
  year   = study_years,
  x_cell = x_range,
  y_cell = y_range
)

# Clip full grid to 27.7.D window (removes land and out-of-division cells)
full_grid_sf <- full_grid_7d %>%
  st_as_sf(coords = c("x_cell", "y_cell"), crs = crs_proj, remove = FALSE) %>%
  st_filter(window_7d_proj %>% st_buffer(cell_size / 2)) %>%
  st_drop_geometry()

cell_effort_full <- full_grid_sf %>%
  left_join(
    cell_effort_7d %>%
      dplyr::select(year, x_cell, y_cell, n_events),
    by = c("year", "x_cell", "y_cell")
  ) %>%
  replace_na(list(n_events = 0)) %>%
  group_by(year) %>%
  mutate(ndensity = n_events / max(n_events)) %>%
  ungroup()

# ---- Plot 12e: faceted hotspot map with corrected colour scale ----

# ---- Compute a consistent scale cap across all years ----
# Use p99 across all years combined as the upper limit
# so one extreme year doesn't compress the whole scale

scale_cap <- quantile(cell_effort_full$n_events[cell_effort_full$n_events > 0], 
                      0.99, na.rm = TRUE)

# Cap values at scale_cap for display (cells above are all shown as max colour)
cell_effort_plot <- cell_effort_full %>%
  mutate(n_events_capped = pmin(n_events, scale_cap))

# Recompute breakpoints from the full dataset distribution
quants <- quantile(cell_effort_full$n_events[cell_effort_full$n_events > 0],
                   c(0.50, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)

message("Cross-year quantiles (cells with n_events > 0):")
print(quants)

ggplot() +
  theme_bw() +
  geom_tile(data = cell_effort_plot,
            aes(x = x_cell, y = y_cell, fill = n_events_capped)) +
  scale_fill_gradientn(
    colours  = c("white", "#c7e9b4", "#7fcdbb", "#41b6c4",
                 "#1d91c0", "#225ea8", "#0c2c84"),
    values   = scales::rescale(c(0, quants["50%"], quants["75%"], 
                                 quants["90%"], quants["95%"], 
                                 quants["99%"], scale_cap)),
    name     = "Events\nper cell",
    limits   = c(0, scale_cap),
    na.value = "grey92"
  ) +
  geom_sf(data = world_proj, fill = "grey80",
          colour = "grey60", linewidth = 0.2) +
  geom_sf(data = fao_div_proj %>% filter(division == "27.7.D"),
          fill = NA, colour = "grey30", linewidth = 0.5) +
  coord_sf(crs    = crs_proj,
           xlim   = bbox_7d_proj[c("xmin", "xmax")],
           ylim   = bbox_7d_proj[c("ymin", "ymax")],
           expand = TRUE) +
  theme(
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    panel.background = element_rect(fill = "grey92"),
    plot.background  = element_rect(fill = "white"),
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey50", size = 8),
    strip.background = element_rect(fill = "grey90"),
    strip.text       = element_text(face = "bold", colour = "grey20")
  ) +
  labs(title    = paste0("Fishing effort hotspots — ICES 27.7.D  |  ",
                         cell_size/1000, " km grid"),
       subtitle = paste0("Events per cell per year  |  All fleets combined  |  ",
                         "Scale capped at p99 (", round(scale_cap), " events)")) +
  facet_wrap(~ year, ncol=5)


# ==============================================================================
# Plot 12f/12g — Hotspot maps by flag: FRA and NLD
# ==============================================================================

# ---- Helper function to build cell effort for a given flag ----
make_cell_effort <- function(flag_code) {
  
  # Filter events to flag, join vessel_meta for flag info
  events_flag <- events_classified_fr %>%
    mutate(year = lubridate::year(start)) %>%
    filter(year %in% study_years,
           division == "27.7.D",
           !is.na(lon), !is.na(lat)) %>%
    dplyr::select(year, vesselId, lon, lat) %>%
    left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
              by = "vesselId") %>%
    filter(vessel_flag == flag_code)
  
  message(flag_code, ": ", nrow(events_flag), " events in 27.7.D")
  
  if (nrow(events_flag) == 0) return(NULL)
  
  # Project to UTM
  events_proj <- events_flag %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs_proj) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  # Assign to grid cells
  events_grid <- events_proj %>%
    mutate(
      x_cell  = floor(x / cell_size) * cell_size + cell_size / 2,
      y_cell  = floor(y / cell_size) * cell_size + cell_size / 2,
      cell_id = paste(x_cell, y_cell, sep = "_")
    )
  
  # Effort per cell per year
  cell_effort <- events_grid %>%
    group_by(year, cell_id, x_cell, y_cell) %>%
    summarise(n_events = n(), .groups = "drop")
  
  # Join to full grid (zeros for empty cells)
  full_grid_sf %>%
    mutate(x_cell = round(x_cell),
           y_cell = round(y_cell)) %>%
    left_join(
      cell_effort %>%
        mutate(x_cell = round(x_cell),
               y_cell = round(y_cell)) %>%
        dplyr::select(year, x_cell, y_cell, n_events),
      by = c("year", "x_cell", "y_cell")
    ) %>%
    replace_na(list(n_events = 0)) %>%
    mutate(flag = flag_code)
}

# ---- Build for FRA and NLD ----
cell_effort_fra <- make_cell_effort("FRA")
cell_effort_nld <- make_cell_effort("NLD")

# ---- Compute shared scale from both flags combined ----
combined_events <- c(
  cell_effort_fra$n_events[cell_effort_fra$n_events > 0],
  cell_effort_nld$n_events[cell_effort_nld$n_events > 0]
)

shared_scale_cap <- quantile(combined_events, 0.99, na.rm = TRUE)
shared_quants    <- quantile(combined_events, c(0.50, 0.75, 0.90, 0.95, 0.99),
                             na.rm = TRUE)

message("Shared scale cap (p99): ", round(shared_scale_cap))
print(shared_quants)

# ---- Update function to accept shared scale ----
make_hotspot_plot <- function(cell_effort_flag, flag_label,
                              scale_cap = NULL, quants = NULL) {
  
  # Use provided scale or compute from this flag alone
  if (is.null(scale_cap)) {
    scale_cap <- quantile(
      cell_effort_flag$n_events[cell_effort_flag$n_events > 0],
      0.99, na.rm = TRUE)
  }
  if (is.null(quants)) {
    quants <- quantile(
      cell_effort_flag$n_events[cell_effort_flag$n_events > 0],
      c(0.50, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)
  }
  
  cell_effort_flag %>%
    mutate(n_events_capped = pmin(n_events, scale_cap)) %>%
    ggplot() +
    theme_bw() +
    geom_tile(aes(x = x_cell, y = y_cell, fill = n_events_capped)) +
    scale_fill_gradientn(
      colours  = c("white", "#c7e9b4", "#7fcdbb", "#41b6c4",
                   "#1d91c0", "#225ea8", "#0c2c84"),
      values   = scales::rescale(c(0, quants["50%"], quants["75%"],
                                   quants["90%"], quants["95%"],
                                   quants["99%"], scale_cap)),
      name     = "Events\nper cell",
      limits   = c(0, scale_cap),
      na.value = "grey92"
    ) +
    geom_sf(data = world_proj, fill = "grey80",
            colour = "grey60", linewidth = 0.2) +
    geom_sf(data = fao_div_proj %>% filter(division == "27.7.D"),
            fill = NA, colour = "grey30", linewidth = 0.5) +
    coord_sf(crs    = crs_proj,
             xlim   = bbox_7d_proj[c("xmin", "xmax")],
             ylim   = bbox_7d_proj[c("ymin", "ymax")],
             expand = TRUE) +
    theme(
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      panel.background = element_rect(fill = "grey92"),
      plot.background  = element_rect(fill = "white"),
      panel.grid       = element_blank(),
      legend.position  = "bottom",
      legend.key.width = unit(2, "cm"),
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(colour = "grey50", size = 8),
      strip.background = element_rect(fill = "grey90"),
      strip.text       = element_text(face = "bold", colour = "grey20")
    ) +
    labs(title    = paste0("Fishing effort hotspots — ", flag_label,
                           "  |  ICES 27.7.D  |  ", cell_size/1000, " km grid"),
         subtitle = paste0("Events per cell per year  |  ",
                           flag_label, " fleet only  |  ",
                           "Scale capped at p99 (",
                           round(scale_cap), " events)")) +
    facet_wrap(~ year)
}

# ---- Plot 12f: French fleet — shared scale ----
make_hotspot_plot(cell_effort_fra, "FRA",
                  scale_cap = shared_scale_cap,
                  quants    = shared_quants)

# ---- Plot 12g: Dutch fleet — shared scale ----
make_hotspot_plot(cell_effort_nld, "NLD",
                  scale_cap = shared_scale_cap,
                  quants    = shared_quants)

