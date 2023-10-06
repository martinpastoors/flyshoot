# GFW
#

options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

# Libraries
require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
require(lubridate, quietly=TRUE)     # data handling
require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
library(sf)

library(captioner)                   # for captions
tab_nums <- captioner::captioner(prefix = "Table " , levels=1, type=c("n"), infix=".")
fig_nums <- captioner::captioner(prefix = "Figure ", levels=1, type=c("n"), infix=".")

library(gfwr)
key <- gfw_auth()

# source("../../prf/R/my utils.r")
# source("../../mptools/R/get_onedrive.r")
source("R/FLYSHOOT utils.r")

spatialdir <- "C:/DATA/RDATA"
load(file.path(spatialdir, "world_mr_df.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))

fao_sf          <- loadRData(file.path(spatialdir, "fao_sf.RData"))
fao_sf_division <- fao_sf %>% filter(F_LEVEL=="DIVISION") %>% dplyr::select(F_DIVISION) %>% rename(division = F_DIVISION)

# flyshoot info
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

# read vessels from excel file
r <- 
  readxl::read_excel(path=file.path(onedrive, "data","flyshoot vessels update EvL.xlsx")) %>% 
  mutate(mmsi = as.character(mmsi))

trawlers <- gfwr::get_vessel_info(
  query = paste0("mmsi=",paste(r$mmsi, collapse=" OR mmsi=")),
  search_type = "advanced",
  dataset = "fishing_vessel",
  key = key
)

# Collapse vessel ids into a commas separated list to pass to Events API
trawler_ids <- paste0(trawlers$id, collapse = ',')

gfw <-
  get_event(event_type='fishing',
            vessel = trawler_ids,
            # start_date = "2012-01-01", end_date = "2023-12-31",
            start_date = "2023-01-01", end_date = as.Date(now()),
            key = key) %>%
  mutate(year = lubridate::year(start),
         month = lubridate::month(start),
         week  = lubridate::week(start),
         date = as.Date(start)) %>%
  unnest_wider(regions, names_repair = "unique") %>%
  unnest_wider(fao, names_sep = "_") %>%
  unnest_wider(eez, names_sep = "_") %>%
  unnest_wider(vessel, names_repair = "unique") %>%
  unnest_wider(event_info, names_repair = "unique") %>%
  unnest_wider(distances, names_repair = "unique") %>%
  dplyr::select(-mpa, -authorizations, -rfmo, -majorFao) %>%
  left_join(r, by=c("ssvid"="mmsi")) %>%
  arrange(vesselname, year, week, end) %>%
  group_by(vesselname, year, week) %>%
  mutate(haul = row_number()) %>%
  
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = FALSE) %>%
  sf::st_join(., fao_sf_division, join = st_within) %>%
  sf::st_drop_geometry()

save(gfw, file = file.path(onedrive, "rdata", "gfw.RData"))
  
