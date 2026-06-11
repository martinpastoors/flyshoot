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
  vessel_id     = c("SCH99","SCH135","SCH65", "SCH144"),
  date_from     = "2026-04-27",
  date_to       = "2026-04-30",
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


remove_flyshoot_data("SCH144", date_from="2026/04/27", date_to="2026/04/30",
                     flyshoot_root = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw", 
                     dry_run = FALSE)


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


# ==============================================================================
# Run tripreport on specific trip
# ==============================================================================

# Find the most recent SCH99 trip
recent <- haul %>%
  dplyr::filter(vessel == "SCH99") %>%
  dplyr::arrange(dplyr::desc(date)) %>%
  dplyr::slice(1)

vessel_id <- recent$vessel
trip_id   <- recent$trip_id
startdate <- as.Date(min(haul$date[haul$trip_id == trip_id & haul$vessel == vessel_id], na.rm = TRUE))
enddate   <- as.Date(max(haul$date[haul$trip_id == trip_id & haul$vessel == vessel_id], na.rm = TRUE))

# Set options the Rmd reads
options(
  flyshoot.vessel    = vessel_id,
  flyshoot.startdate = startdate,
  flyshoot.enddate   = enddate,
  flyshoot.lang      = "nl",
  flyshoot.max_species = 12
)

# Render
rmarkdown::render(
  input       = file.path(here::here(), "R/harmonized", "FLYSHOOT_tripreport_harmonized.Rmd"),
  output_file = file.path(here::here(), glue::glue("SCH99_{trip_id}_test.docx")),
  quiet       = FALSE   # set TRUE to suppress knit messages
)


# ==============================================================================
# Check content of parquet files
# ==============================================================================

source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))

# Basic check — no RData needed
result <- diagnose_parquet_coverage("SCH99")

# If you have an old RData file to compare against
rdata_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"

result <- diagnose_parquet_coverage(
  vessel_id         = "SCH99",
  rdata_haul_path   = file.path(rdata_dir, "haul.RData"),
  rdata_kisten_path = file.path(rdata_dir, "kisten.RData")
)

# ==============================================================================
# Debugging nspecies
# ==============================================================================

# Source the Rmd setup manually to see the debug output
# First set the options the Rmd expects:
options(
  flyshoot.vessel    = "SCH135",
  flyshoot.startdate = as.Date("2026-04-27"),
  flyshoot.enddate   = as.Date("2026-04-30"),
  flyshoot.lang      = "nl",
  flyshoot.max_species = 12
)

# Then source just the data loading and species section:
flyshoot_root <- Sys.getenv("ONEDRIVE_FLYSHOOT")

# Load the parquet data the same way the Rmd does
haul_raw  <- load_flyshoot_data("haul")
elog_raw  <- load_flyshoot_data("elog")
kisten_raw <- load_flyshoot_data("kisten")
et_raw    <- load_flyshoot_data("elog_trek")

setvessel   <- "SCH135"
startdate   <- as.Date("2026-04-27")
enddate     <- as.Date("2026-04-30")
max_species <- 12

# Replicate the Rmd filter
h <- haul_raw  %>% dplyr::filter(vessel == setvessel, date >= startdate, date <= enddate)
e <- elog_raw  %>% dplyr::filter(vessel == setvessel, date >= startdate, date <= enddate)
m <- kisten_raw %>% dplyr::filter(vessel == setvessel, date >= startdate, date <= enddate)
et <- et_raw   %>% dplyr::filter(vessel == setvessel, date >= startdate, date <= enddate)

cat("nrow(h):", nrow(h), "| nrow(e):", nrow(e), 
    "| nrow(m):", nrow(m), "| nrow(et):", nrow(et), "\n")
cat("species in e:", paste(sort(unique(e$species_code)), collapse=", "), "\n")
cat("trip_ids in e:", paste(sort(unique(e$trip_id)), collapse=", "), "\n")


e %>% 
  dplyr::group_by(species_code) %>%
  dplyr::summarise(weight = sum(weight_kg, na.rm=TRUE), n=n()) %>%
  dplyr::arrange(desc(weight)) %>%
  print(n=30)

cat("weight_kg class:", class(e$weight_kg), "\n")
cat("weight_kg NAs:", sum(is.na(e$weight_kg)), "of", nrow(e), "\n")
cat("weight_kg zeros:", sum(e$weight_kg == 0, na.rm=TRUE), "\n")
summary(e$weight_kg)




# Replicate the asp2 calculation
xmin2 <- 0; xmax2 <- 1.25   # approximate from the map you saw
ymin2 <- 49.75; ymax2 <- 50.875

xd2 <- icosa::arcdist(p1 = c(xmin2, ymin2), p2 = c(xmax2, ymin2))
yd2 <- icosa::arcdist(p1 = c(xmin2, ymin2), p2 = c(xmin2, ymax2))
asp2 <- yd2 / xd2

cat("xd2:", xd2, "yd2:", yd2, "asp2:", asp2, "\n")

# Then the layout calculation
page_h   <- 9.2 * 0.63
text_w   <- 6.3
legend_h <- 0.8
max_species <- 12

for (nc in 2:5) {
  panel_w  <- text_w / nc
  panel_h  <- panel_w * asp2
  max_rows <- max(floor((page_h - legend_h) / panel_h), 1)
  fits     <- min(nc * max_rows, max_species)
  cat(glue::glue("ncol={nc}: panel_h={round(panel_h,2)}, max_rows={max_rows}, fits={fits}\n"))
}


cat("asp2:", asp2, "\n")
cat("bbox: lon", xmin2, "to", xmax2, "| lat", ymin2, "to", ymax2, "\n")
cat("page_h:", page_h, "| legend_h:", legend_h, "\n")




# Backfill parquet files with kisten information
source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))

rdata_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"

preview <- backfill_kisten_from_rdata(
  rdata_path    = file.path(rdata_dir, "kisten.RData"),
  vessel_id     = "SCH99",
  dry_run       = TRUE
)

backfill_kisten_from_rdata(
  rdata_path    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata/kisten.RData",
  vessel_id     = "SCH99",
  dry_run       = FALSE
)

rdata_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"

diagnose_parquet_coverage(
  vessel_id         = "SCH99",
  rdata_kisten_path = file.path(rdata_dir, "kisten.RData")
)

env_old <- new.env()
load("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata/kisten.RData", 
     envir = env_old)
k <- get("kisten", envir = env_old)
cat("datum class:", class(k$datum), "\n")
cat("datum sample (first 10 non-NA):\n")
print(head(k$datum[!is.na(k$datum)], 10))


# check of overlapping trips with different tripnumbers

kisten <- load_flyshoot_data("kisten", vessel_ids = "SCH99")

# Check for same trip content under different IDs
kisten %>%
  dplyr::mutate(
    date_part = as.Date(date),
    id_type   = dplyr::if_else(nchar(trip_id) > 7, "date_based", "numeric")
  ) %>%
  dplyr::group_by(id_type, trip_id) %>%
  dplyr::summarise(
    n         = dplyr::n(),
    date_from = min(date_part, na.rm = TRUE),
    date_to   = max(date_part, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::arrange(date_from) %>%
  print(n = 30)

kisten %>%
  dplyr::mutate(id_type = dplyr::if_else(nchar(trip_id) > 7, "date_based", "numeric")) %>%
  dplyr::group_by(id_type) %>%
  dplyr::summarise(
    n_trips   = dplyr::n_distinct(trip_id),
    date_from = min(date, na.rm = TRUE),
    date_to   = max(date, na.rm = TRUE),
    .groups   = "drop"
  )


haul_hist <- haul %>%
  dplyr::filter(vessel == "SCH99",
                lubridate::year(date) == 2026,
                lubridate::isoweek(date) %in% c(14, 15))

kisten_hist <- kisten %>%
  dplyr::filter(vessel == "SCH99",
                lubridate::year(date) == 2026,
                lubridate::isoweek(date) %in% c(14, 15))

cat("Hauls in wk 14-15:", nrow(haul_hist), "\n")
cat("Kisten in wk 14-15:", nrow(kisten_hist), "\n")

# Check which hauls have no kisten match
haul_hist %>%
  dplyr::left_join(
    kisten_hist %>% dplyr::group_by(trip_id, haul_id) %>%
      dplyr::summarise(landed_kg = sum(weight_kg, na.rm=TRUE), .groups="drop"),
    by = c("trip_id", "haul_id")
  ) %>%
  dplyr::select(trip_id, haul_id, date, total_catch_kg, landed_kg) %>%
  print(n = 30)


# Focus on week 14 only (trip 2026033000015, dates around Apr 1-4)
wk15_hauls <- haul %>%
  dplyr::filter(vessel == "SCH99",
                lubridate::year(date) == 2026,
                lubridate::isoweek(date) == 15) %>%
  dplyr::select(trip_id, haul_id, date, total_catch_kg)

wk15_kisten <- kisten %>%
  dplyr::filter(vessel == "SCH99",
                lubridate::year(date) == 2026,
                lubridate::isoweek(date) == 15) %>%
  dplyr::group_by(trip_id, haul_id) %>%
  dplyr::summarise(
    landed_kg   = sum(weight_kg, na.rm = TRUE),
    n_weighings = dplyr::n(),
    .groups = "drop"
  )

wk15_hauls %>%
  dplyr::left_join(wk1_kisten, by = c("trip_id", "haul_id")) %>%
  dplyr::mutate(
    discard_kg  = pmax(total_catch_kg - dplyr::coalesce(landed_kg, 0), 0),
    discard_pct = dplyr::if_else(total_catch_kg > 0,
                                 round(100 * discard_kg / total_catch_kg), NA_real_),
    kisten_flag = dplyr::case_when(
      is.na(landed_kg)           ~ "no kisten",
      landed_kg == 0             ~ "zero landing",
      landed_kg > total_catch_kg ~ "landed > catch",
      TRUE                       ~ "ok"
    )
  ) %>%
  dplyr::arrange(date, haul_id) %>%
  print(n = 50)




# Verify it's a duplicate; wk14
# haul %>%
#   dplyr::filter(vessel == "SCH99",
#                 date >= dmy("30-3-2026"), date <= dmy("2/4/2026")) %>%
#   dplyr::group_by(trip_id) %>%
#   dplyr::summarise(n = n(), date_from = min(date), date_to = max(date))

remove_flyshoot_data(
  vessel_id     = "SCH99",
  trip_ids      = "2026286",
  flyshoot_root = config$raw_data_path,
  dry_run       = FALSE   # set FALSE to apply
)


# Verify it's a duplicate; wk15
# haul %>%
#   dplyr::filter(vessel == "SCH99",
#                 date >= dmy("6-4-2026"), date <= dmy("9/4/2026")) %>%
#   dplyr::group_by(trip_id) %>%
#   dplyr::summarise(n = n(), date_from = min(date), date_to = max(date))

haul %>%
  dplyr::filter(vessel == "SCH99",
                trip_id %in% c("2026040600016", "2026286")) %>%
  dplyr::group_by(trip_id) %>%
  dplyr::summarise(n = n(), date_from = min(date), date_to = max(date))

remove_flyshoot_data(
  vessel_id     = "SCH99",
  trip_ids      = "2026286",
  flyshoot_root = config$raw_data_path,
  dry_run       = FALSE   # set FALSE to apply
)








# Check what the kisten file header looks like
f <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/_te verwerken/SCH99 2026_291 kisten-reis-1-trek-31.xlsx"

# Read first 10 rows raw to see the structure
readxl::read_excel(f, sheet = 1, col_names = FALSE, 
                   col_types = "text", n_max = 10)

f2 <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/_te verwerken/SCH99 2026_291 treklijst simplified.xlsx"
readxl::read_excel(f2, sheet = 1, n_max = 3)




# After sourcing 03_main_workflow.R, reprocess the trip manually:
treklijst_file <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/_te verwerken/SCH99 2026_291 treklijst simplified.xlsx"
kisten_file    <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/_te verwerken/SCH99 2026_291 kisten-reis-1-trek-31.xlsx"

# Step 1: what does trek_pos look like?
trek_pos <- get_haul_from_treklijst(treklijst_file)
cat("trek_pos: ", nrow(trek_pos), "rows\n")
print(trek_pos %>% dplyr::select(haul_id, date, shoot_lat, shoot_lon))

# Step 2: what does trip_catches look like?
trip_catches <- get_catch_from_kisten(kisten_file,
                                      local_tz  = "Europe/Amsterdam",
                                      haul_data = trek_pos)
cat("\ntrip_catches haul_id distribution:\n")
print(trip_catches %>% dplyr::count(haul_id))
cat("Date of weighings:\n")
print(trip_catches %>%
        dplyr::mutate(d = as.Date(weighing_time, tz="UTC")) %>%
        dplyr::count(d, haul_id))



trip_existing %>% filter(vessel=="SCH65") %>% slice_tail(n=3) %>% dplyr::select(vessel, trip_id, trip_nr, departure_date, departure_port)
trip_existing %>% filter(vessel=="SCH99") %>% slice_tail(n=3) %>% dplyr::select(vessel, trip_id, trip_nr, departure_date, departure_port)

haul <- load_flyshoot_data("haul")
haul %>% 
  mutate(year = year(shoot_time)) %>% 
  group_by(vessel, year) %>% 
  summarise(nobs = sum(!is.na(vessel)))


elog <- load_flyshoot_data("elog")
elog %>% 
  mutate(year = year(date)) %>% 
  group_by(vessel, year) %>% 
  summarise(nobs = sum(!is.na(vessel))) %>% 
  pivot_wider(names_from = year, values_from = nobs)

source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
parquet_coverage()
parquet_coverage(value = "n_days")
parquet_coverage("elog")








source(file.path(here(), "R/harmonized/check_parquet_consistency.R"))

# Basic run
check_parquet_consistency()

# Hard stop on any issue (good for pipeline gating)
check_parquet_consistency(stop_on_error = TRUE)

# Custom directory (e.g. turbocatch subfolder before integration)
check_parquet_consistency(parquet_dir = file.path(
  "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch"
))

# Capture issues programmatically
issues <- check_parquet_consistency()
length(issues)  # 0 = all good

glimpse(haul)
glimpse(elog_trek)
glimpse(elog)
glimpse(kisten)
glimpse(vessel_movement)
haul %>% count(is.na(marketable_catch_kg))


source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
standardise_all_parquets(dry_run = TRUE)

elog %>% 
  select(vessel, trip_id, date, haul) %>% 
  filter(!is.na(haul)) %>% 
  head(20)

elog %>%
  filter(!is.na(haul)) %>%
  mutate(haul_id = as.integer(haul)) %>%
  distinct(vessel, trip_id, haul_id) %>%
  left_join(haul %>% distinct(vessel, trip_id, haul_id),
            by = c("vessel", "trip_id", "haul_id")) %>%
  head(20)


# Clear everything in the environment
rm(list = ls())
gc()

# Re-source functions
source(file.path(here(), "R/harmonized/01_flyshoot_functions.R"))
standardise_all_parquets()


source(file.path(here(), "R/harmonized/check_parquet_consistency.R"))
check_parquet_consistency()








options(
  flyshoot.vessel    = "SCH144",
  flyshoot.startdate = lubridate::dmy("01/06/2026"),
  flyshoot.enddate   = lubridate::dmy("04/06/2026"),
  flyshoot.lang      = "nl",
  flyshoot.max_species = 12
)

rmarkdown::render(
  input       = file.path(here::here(), "R/harmonized/FLYSHOOT_tripreport_harmonized.Rmd"),
  output_file = "SCH144_2026W23_1trip.docx",
  output_dir  = file.path(here::here())
)
