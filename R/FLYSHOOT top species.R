# =====================================================================================================
# FLYSHOOT top species
# 
# 11/01/2023 first coding
#
# =====================================================================================================

options(dplyr.summarise.inform = FALSE)

# Reset lists
# rm(list=ls())

# Libraries
require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
# Source all the utils
source("R/FLYSHOOT utils.r")
# source("../mptools/R/get_onedrive.r")

spatialdir <- "C:/DATA/RDATA"
asfis <- 
  loadRData(file.path(spatialdir, "asfis.RData")) %>% 
  rename_all(tolower) 

# ===============================================================================
#  Load and filter the trip data
# ===============================================================================

# set onedrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")


# load datasets
regulated  <- 
  readr::read_rds(file.path(onedrive, "rdata/prices.rds")) %>% 
  ungroup() %>% 
  dplyr::distinct(species, regulated) 

price  <- 
  readr::read_rds(file.path(onedrive, "rdata/prices.rds")) %>% 
  dplyr::select(year, species, avgprice) 

maxyear <- max(price$year, na.rm=TRUE)

top   <- 
  loadRData(file.path(onedrive, "data/elog pefa.RData")) %>% 
  
  filter(tolower(faozone) %in% c("27.7.d","27.4.b","27.4.c")) %>% 
  filter(year %in% (maxyear-5):maxyear) %>% 
  
  mutate(species = toupper(species)) %>% 
  mutate(species = ifelse(species == "JAX","HOM",species)) %>% 
  mutate(species = ifelse(species == "SQU", "SQR", species)) %>% 

  left_join(price) %>% 
  left_join(regulated) %>% 
  left_join(dplyr::select(asfis,
                          species, scientificname, englishname, dutchname),
            by = "species") %>% 
  drop_na(avgprice) %>% 
  mutate(value = weight * avgprice) %>% 
  
  group_by(species, scientificname, englishname, dutchname) %>% 
  summarise(
    weight = sum(weight, na.rm=TRUE),
    value  = sum(value, na.rm=TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(englishspecies = paste(englishname, species)) %>% 
  mutate(
    englishname = ifelse(species == "PIL", "European pilchard", englishname),
    englishspecies = ifelse(species == "PIL", "European pilchard PIL", englishspecies)
  ) %>% 
  arrange(desc(value)) %>% 
  slice_head(n=15)

readr::write_rds(top, file=file.path(onedrive,
                                     "rdata",
                                     "top.rds"))
