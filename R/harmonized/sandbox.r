# sandbox.r

library(arrow)
library(dplyr)
library(ggplot2)
library(here)

# ── Load config & functions ──────────────────────────────────────────

source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
source(file.path(here(), "R/harmonized", "02_storage_functions.R"))

config <- jsonlite::read_json("config.json")
flyshoot_root <- config$raw_data_path   # adjust if your key differs
flyshoot_teverwerken <- config$tripdata_input


# ── Load haul data for SCH65, most recent trip ────────────────────────────────
haul <- arrow::read_parquet(file.path(flyshoot_root, "haul", "haul.parquet")) %>%
  filter(vessel == "SCH65") %>%
  arrange(desc(date)) %>%
  mutate(trip_id = as.character(trip_id))

# Show latest trip
latest_trip <- haul %>% slice_head(n = 1) %>% pull(trip_id)
cat("Latest trip:", latest_trip, "\n")

h <- haul %>% filter(trip_id == latest_trip)

# ── Position summary ──────────────────────────────────────────────────────────
cat("\n--- Position summary (", nrow(h), "hauls) ---\n")
h %>%
  summarise(
    n_hauls      = n(),
    n_with_pos   = sum(!is.na(shoot_lon) & !is.na(shoot_lat)),
    lon_range    = paste(round(range(shoot_lon, na.rm=TRUE), 4), collapse=" – "),
    lat_range    = paste(round(range(shoot_lat, na.rm=TRUE), 4), collapse=" – "),
    lon_spread   = round(diff(range(shoot_lon, na.rm=TRUE)), 5),
    lat_spread   = round(diff(range(shoot_lat, na.rm=TRUE)), 5)
  ) %>% print()

# ── Per-haul positions ────────────────────────────────────────────────────────
h %>%
  select(haul_id, date, shoot_lon, shoot_lat) %>%
  print(n = Inf)

# ── Quick map ────────────────────────────────────────────────────────────────
ggplot(h %>% filter(!is.na(shoot_lon)),
       aes(x = shoot_lon, y = shoot_lat, label = haul_id)) +
  geom_point(colour = "steelblue", size = 3) +
  geom_text(vjust = -0.7, size = 3) +
  coord_quickmap() +
  theme_bw() +
  labs(title = paste("SCH65 —", latest_trip, "— haul positions"),
       x = "Longitude", y = "Latitude")




source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))

remove_flyshoot_data(
  vessel_id     = c("SCH144"),
  date_from     = "2026-03-16",
  date_to       = "2026-03-20",
  flyshoot_root = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw",
  dry_run       = FALSE
)

combined_vm %>% filter(vessel == "SCH144") %>% print()

# Basic comparison (elog vs elog_trek vs kisten)
compare_data_sources(
  vessel_id     = "SCH135",
  trip_id       = "2026031600015",
  flyshoot_root = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw"
)

# Just elog vs elog_trek, flag differences > 5%
compare_data_sources("SCH135", "2026031600015",
                     sources   = c("elog", "elog_trek"),
                     threshold = 0.05
)


# =============================================================================
# removing erroneous trip data
# =============================================================================

# Preview first
remove_flyshoot_data("SCH99", trip_ids = "2026285",
                     flyshoot_root = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw")

# Then apply
remove_flyshoot_data("SCH99", trip_ids = "2026285",
                     flyshoot_root = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw", dry_run = FALSE)


# =============================================================================
# checking elog and elog_trek data
# =============================================================================

source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
source(file.path(here(), "R/harmonized", "02_storage_functions.R"))
source(file.path(here(), "R/harmonized", "kisten_haul_assignment.R"))   # auto haul-ID assignment for kisten files

elog <- tryCatch(
  load_flyshoot_data("elog"),
  error = function(e) {
    message("  No existing elog data found")
    tibble()
  }
)

elog_trek <- tryCatch(
  load_flyshoot_data("elog_trek"),
  error = function(e) {
    message("  No existing elog_trek data found")
    tibble()
  }
)

e <-
  elog %>% 
  filter(vessel == "SCH135", lubridate::year(date) ==2026) %>% 
  group_by(trip_id) %>% 
  summarise(
    startdate = min(date, na.rm=TRUE),
    enddate   = max(date, na.rm=TRUE),
    weight_kg = sum(weight_kg, na.rm=TRUE)
  ) %>% 
  mutate(source="elog")

et <-
  elog_trek %>% 
  filter(vessel == "SCH135", lubridate::year(date) ==2026) %>% 
  group_by(trip_id) %>% 
  summarise(
    startdate = min(date, na.rm=TRUE),
    enddate   = max(date, na.rm=TRUE),
    weight_kg = sum(weight_kg, na.rm=TRUE)
  ) %>% 
  mutate(source = "elog_trek")

bind_rows(e, et) %>% 
  ggplot(aes(x=trip_id, y=weight_kg)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_point(aes(colour=source)) +
  expand_limits(y=0)


bind_rows(e, et) %>% 
  ggplot(aes(x=trip_id, y=startdate)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_point(aes(colour=source)) 

bind_rows(e, et) %>% 
  filter(trip_id == "2026031600015") %>% 
  ggplot(aes(x=trip_id, y=enddate)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_point(aes(colour=source)) 

bind_rows(e, et) %>% 
  filter(trip_id == "2026031600015") %>% 
  group_by()
  ggplot(aes(x=trip_id, y=enddate)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_point(aes(colour=source)) 

elog_trek %>% 
  filter(vessel == "SCH135", lubridate::year(date) ==2026) %>% 
  filter(is.na(weighing_time)) %>% 
  group_by(weighing_time, species_code) %>% 
  mutate(n=n()) %>% 
  filter(n>1) %>% 
  View()



diagnose_weighing_time()

# number of fishing days per year (based on elog)
t <-
  elog %>% 
  ungroup() %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year >= 2019, year < 2026) %>%
  distinct(year, vessel, date) %>% 
  group_by(year) %>% 
  summarise(
    nvessels = n_distinct(vessel),
    ndays = n()
  ) %>% 
  mutate(
    days_vessel = ndays / nvessels
  )

t %>% 
  pivot_longer(names_to = "variable", values_to = "data", c(nvessels, ndays, days_vessel)) %>% 
  ggplot(aes(x=year, y=data)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  expand_limits(y=0) +
  facet_wrap(~variable, scales = "free_y", ncol=1)

# =============================================================================
# testing trip 287 SCH99
# =============================================================================

kisten <- arrow::read_parquet("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw/kisten/kisten.parquet")
kisten %>% dplyr::filter(vessel == "SCH99", trip_id == "2026287") %>%
  dplyr::count(haul_id)
t <- kisten %>% mutate(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  dplyr::filter(vessel == "SCH99", year == 2026, month == 4)
# View(t)

source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
config <- jsonlite::read_json("config.json")
flyshoot_root <- config$raw_data_path   # adjust if your key differs
flyshoot_teverwerken <- config$tripdata_input

result <- debug_haul_assignment(
  kisten_file    = file.path(flyshoot_teverwerken, "SCH99 2026_287 kisten-reis-287-trek-35.xlsx"),
  treklijst_file = file.path(flyshoot_teverwerken, "SCH99 2026_287 treklijst wk99week16.xlsx")
)

readLines(file.path(here::here(), "R/harmonized", "01_flyshoot_functions.R"), n=8)
