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

source("../prf/R/my utils.r")
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

# ---- 1. Load vessel list ----
vessels <- readxl::read_excel(file.path(flyshootdir, "flyshoot vessels update EvL.xlsx")) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi)) 

# Inspect one vessel first
# test <- gfw_vessel_info(query = "244704000", search_type = "search", key = key)
# str(test, max.level = 3)
# names(test)

# ---- 2. Resolve MMSI → GFW vessel IDs ----
vessel_info <- map_dfr(vessels$mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)
    
    # Take the most recent/best-matched selfReportedInfo row
    sri <- res$selfReportedInfo %>%
      arrange(desc(transmissionDateTo)) %>%   # most recent first
      slice(1)                                 # take top row per MMSI
    
    # Grab registry info (gear, GT) if available
    ri <- res$registryInfo %>%
      arrange(desc(latestVesselInfo)) %>%
      slice(1) %>%
      select(geartypes, tonnageGt, lengthM) 
    
    bind_cols(sri, ri) %>%
      mutate(mmsi_queried = m)
    
  }, error = function(e) { message("Failed MMSI: ", m, " — ", e$message); NULL })
})


# All vesselIds per MMSI (needed for full 2012-2025 coverage)
vessel_ids_all <- map_dfr(vessels$mmsi, function(m) {
  tryCatch({
    res <- gfw_vessel_info(query = m, search_type = "search", key = key)
    res$selfReportedInfo %>%
      select(vesselId, ssvid, shipname, transmissionDateFrom, transmissionDateTo) %>%
      mutate(mmsi_queried = m)
  }, error = function(e) { message("Failed: ", m); NULL })
})

# Then use ALL vesselIds when pulling events
all_vessel_ids <- unique(vessel_ids_all$vesselId)

# ---- 3. Pull fishing events per vessel (full period) ----
# GFW Events API: one call per vessel, full 2012-2025 range
# NOTE: date range limit is 366 days per call, so loop by year

all_events <- list()
for (vid in all_vessel_ids) {
  message("Processing vessel: ", vid)
  events_v <- map_dfr(2012:2025, function(y) {
    Sys.sleep(0.5)  # INSIDE the function, BEFORE the call
    tryCatch({
      result <- gfw_event(
        event_type = "FISHING",
        vessels    = vid,
        start_date = paste0(y, "-01-01"),
        end_date   = paste0(y, "-12-31"),
        key        = key
      )
      if (!is.null(result) && nrow(result) > 0) result else NULL
    }, error = function(e) { message("Failed: ", vid, " / ", y, " — ", e$message); NULL })
  })
  all_events[[vid]] <- events_v
  message("  → ", nrow(events_v), " events collected")
}

events_df <- bind_rows(all_events)
save(events_df, file = file.path(flyshootdir, "gfw_fishing_events_raw.RData"))
load(file = file.path(flyshootdir, "gfw_fishing_events_raw.RData"))

# ---- 4. Derive fishing days ----
fishing_days <- events_df %>%
  mutate(
    year    = year(start),
    quarter = quarter(start),
    days    = as.numeric(difftime(end, start, units = "days"))
  ) %>%
  left_join(vessels, by = c("vessel_ssvid" = "mmsi")) %>%
  group_by(year, quarter, vesselId, vessel_name, vessel_ssvid, vessel_flag, gear, gt) %>%
  summarise(fishing_days = sum(days, na.rm = TRUE), .groups = "drop")

df_raster <- list()
for (y in 2012:2025) {
  for (a in c("27.4.A","27.4.B","27.4.C","27.7.D","27.7.E")) {
    mysf <- fao_sf_division %>% filter(division == a) %>% st_make_valid()
    df_raster[[paste(y,a)]] <- gfw_ais_fishing_hours(
      spatial_resolution  = "HIGH",
      temporal_resolution = "YEARLY",
      group_by            = "FLAGANDGEARTYPE",   # UPPERCASE now
      start_date          = paste0(y, "-01-01"),
      end_date            = paste0(y, "-12-31"),
      region              = mysf,                # sf object directly
      region_source       = "USER_SHAPEFILE",
      key                 = key
    ) %>% mutate(year = y, division = a)
  }
}
df <- bind_rows(df_raster)
save(df, file = "gfw_effort_flagandgeartype.RData")











library(httr2)
library(geojsonsf)
library(jsonlite)

# Test with one division and one year first
# Strip properties from geojson - geometry only
mysf_clean <- fao_sf_division %>% 
  filter(division == "27.4.C") %>% 
  st_make_valid() %>%
  st_geometry() %>%  # extract geometry only, no properties
  st_as_sf()

geojson_clean <- sf_geojson(mysf_clean)
cat(substr(geojson_clean, 1, 200))  # verify no properties

# Wrap in proper FeatureCollection format
geojson_wrapped <- paste0('{
  "type": "FeatureCollection",
  "features": [{
    "type": "Feature",
    "properties": {},
    "geometry": ', geojson_clean, '
  }]
}')

cat(substr(geojson_wrapped, 1, 300))  # verify structure

resp <- request("https://gateway.api.globalfishingwatch.org/v3/4wings/report") %>%
  req_headers(
    Authorization = paste("Bearer", key),
    `Content-Type` = "application/json"
  ) %>%
  req_url_query(
    `datasets[0]`         = "public-global-fishing-effort:latest",
    `spatial-resolution`  = "LOW",
    `temporal-resolution` = "YEARLY",
    `group-by`            = "FLAGANDGEARTYPE",
    `date-range`          = "2021-01-01,2021-12-31",   # comma-separated!
    `format`              = "CSV"
  ) %>%
  req_body_json(list(
    geojson = fromJSON(geojson_wrapped)
  )) %>%
  req_error(is_error = \(r) FALSE) %>%
  req_perform()

resp %>% resp_status()
resp %>% resp_body_string() %>% substr(1, 1000)


# List ALL files in the zip
zip_contents <- unzip(tmp_zip, list = TRUE)
print(zip_contents)

# Also check what's in tempdir
list.files(tmp_dir, recursive = TRUE)

result <- read_csv(file.path(tmp_dir, "layer-activity-data-0/public-global-fishing-effort-v4.0.csv"))
glimpse(result)



get_fishing_hours <- function(mysf, y, a, key) {
  
  geojson_clean <- mysf %>%
    st_geometry() %>%
    st_as_sf() %>%
    sf_geojson()
  
  geojson_wrapped <- paste0('{
    "type": "FeatureCollection",
    "features": [{
      "type": "Feature",
      "properties": {},
      "geometry": ', geojson_clean, '
    }]
  }')
  
  resp <- request("https://gateway.api.globalfishingwatch.org/v3/4wings/report") %>%
    req_headers(
      Authorization = paste("Bearer", key),
      `Content-Type` = "application/json"
    ) %>%
    req_url_query(
      `datasets[0]`         = "public-global-fishing-effort:latest",
      `spatial-resolution`  = "HIGH",
      `temporal-resolution` = "MONTHLY",       
      `group-by`            = "FLAGANDGEARTYPE",
      `date-range`          = paste0(y, "-01-01,", y, "-12-31"),
      `format`              = "CSV"
    ) %>%
    req_body_json(list(geojson = fromJSON(geojson_wrapped))) %>%
    req_perform()
  
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile()   # unique dir per call to avoid csv collision
  dir.create(tmp_dir)
  writeBin(resp %>% resp_body_raw(), tmp_zip)
  unzip(tmp_zip, exdir = tmp_dir, overwrite = TRUE)
  
  csv_file <- list.files(tmp_dir, pattern = "\\.csv$", 
                         full.names = TRUE, recursive = TRUE)
  
  read_csv(csv_file[1], show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    rename(apparent_fishing_hours = `apparent fishing hours`,
           vessel_ids = `vessel ids`,
           time_range = `time range`) %>%
    mutate(
      year     = y,
      division = a,
      month    = as.integer(time_range),        # time_range is now month number
      quarter  = ceiling(month / 3)
    )
}


df_raster <- list()
for (y in 2012:2025) {
  for (a in c("27.4.A","27.4.B","27.4.C","27.7.D","27.7.E")) {
    message("Processing: ", y, " / ", a)
    mysf <- fao_sf_division %>% filter(division == a) %>% st_make_valid()
    tryCatch({
      df_raster[[paste(y, a)]] <- get_fishing_hours(mysf, y, a, key)
      Sys.sleep(1)
    }, error = function(e) message("Failed: ", y, "/", a, " — ", e$message))
  }
}

df <- bind_rows(df_raster) %>%
  group_by(year, quarter, lat, lon, flag, geartype, division) %>%
  summarise(
    apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    vessel_ids             = max(vessel_ids, na.rm = TRUE),
    .groups = "drop"
  )

save(df, file = file.path(spatialdir, "gfw_effort_flagandgeartype.RData"))

# Days at sea from events pipeline
fishing_days <- events_df %>%
  mutate(
    year    = year(start),
    quarter = quarter(start),
    days    = as.numeric(difftime(end, start, units = "days"))
  ) %>%
  left_join(vessels, by = c("vessel_ssvid" = "mmsi")) %>%
  group_by(year, quarter, vessel_ssvid, vessel_name, vessel_flag, gear, gt) %>%
  summarise(
    fishing_days = sum(days, na.rm = TRUE),
    n_events     = n(),
    .groups = "drop"
  )

glimpse(fishing_days)
fishing_days %>% count(year) %>% print(n = 20)

# ---- Plot 1: fishing days by year and country (stacked bar) ----
fishing_days %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Apparent fishing days at sea by year and country")


# ---- Assign events to ICES divisions ----
events_sf <- events_df %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

events_with_division <- events_sf %>%
  st_join(fao_sf_division, join = st_within) %>%
  st_drop_geometry()

# ---- Plot 2: fishing days by year and country, faceted by ICES division ----
events_with_division %>%
  mutate(
    year    = year(start),
    quarter = quarter(start),
    days    = as.numeric(difftime(end, start, units = "days"))
  ) %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(days, na.rm = TRUE), .groups = "drop") %>%
  
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Apparent fishing days at sea by year, country and ICES division") +
  facet_wrap(~ division)

# ---- Plot 3: fishing days by year and country (stacked bar, percentage) ----
fishing_days %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country (%)")

# ---- Plot 4: fishing days by year and country, faceted by ICES division (percentage) ----
events_with_division %>%
  mutate(
    year = year(start),
    days = as.numeric(difftime(end, start, units = "days"))
  ) %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(days, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, division) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2012:2025) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year, country and ICES division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: fishing days by year and gear type (stacked bar, absolute) ----
events_with_division %>%
  mutate(
    year = year(start),
    days = as.numeric(difftime(end, start, units = "days"))
  ) %>%
  filter(!is.na(division)) %>%
  group_by(year, vessel_type, division) %>%
  summarise(fishing_days = sum(days, na.rm = TRUE), .groups = "drop") %>%
  
  ggplot(aes(x = year, y = fishing_days, fill = vessel_type)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Gear type",
       title = "Fishing days at sea by year and gear type") +
  facet_wrap(~ division, scales = "free_y")
