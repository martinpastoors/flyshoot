# CopernicusMarine
#

options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

# Libraries
require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
require(lubridate, quietly=TRUE)     # data handling
require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
library(sf)

# install.packages("CopernicusMarine")
library(CopernicusMarine)

source("R/FLYSHOOT utils.r")

spatialdir <- "C:/DATA/RDATA"
load(file.path(spatialdir, "world_mr_df.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))

fao_sf          <- loadRData(file.path(spatialdir, "fao_sf.RData"))
fao_sf_division <- fao_sf %>% filter(F_LEVEL=="DIVISION") %>% dplyr::select(F_DIVISION) %>% rename(division = F_DIVISION)

# flyshoot info
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

destination <- tempfile("copernicus", fileext = ".nc")


options(CopernicusMarine_uid = "mpastoors1")
options(CopernicusMarine_pwd = "PelagicFishWithPlankton")

copernicus_download_motu(
  destination   = destination,
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
  variable      = "sea_water_velocity",
  output        = "netcdf",
  region        = c(-1, 50, 10, 55),
  timerange     = c("2021-01-01", "2021-01-02"),
  verticalrange = c(0, 2),
  sub_variables = c("uo", "vo")
)

mydata <- stars::read_stars(destination)

plot(mydata["vo"], col = hcl.colors(100), axes = TRUE)

leaflet::leaflet() %>%
  leaflet::setView(lng = 3, lat = 54, zoom = 4) %>%
  leaflet::addProviderTiles("Esri.WorldImagery") %>%
  addCopernicusWMSTiles(
    product     = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
    layer       = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
    variable    = "thetao"
  )

copernicus_products_list() %>% filter(grepl("temperature", tolower(title))) %>% View()

copernicus_product_details(product="SST_GLO_SST_L3S_NRT_OBSERVATIONS_010_010") %>% View()


