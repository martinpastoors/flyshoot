# =======================================================================================
# FLYSHOOT: add tripdata v2.0.r
# 
# Function to read data, check it and add to RData sets
#
# Martin Pastoors
#
# 11/01/2023 First coding
# 20/01/2023 Added elog; added moving files
# 27/01/2023 Changed marelec lots to kisten; removed other marelec exports
# 23/02/2023 Changed pefa export to include position and time (by box)
# 03/08/2023 v1.2 reading in all file types and assessing how to calculate haul information
# 04/08/2023 only based on PEFA export by haul
# 
# TO DO: 
# 
# =========================================================================================

# print settings
options(max.print=999999)
options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

add_data   = FALSE
move_data  = FALSE

onedrive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"
tripdir    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata"
spatialdir = "C:/DATA/RDATA"
  

# Open relevant packages 
library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(readxl)        # excel reader from hadley
library(writexl)       # write excel files
library(zoo)           # manipulating data
library(lubridate)     # date functions      (this is part of tidyverse, but needs to be loaded separately)
library(stringr)       # string manipulation (this is part of tidyverse, but needs to be loaded separately)
library(sf)            # simple features
library(tidygeocoder)  # finding positions for harbours
library(sqldf)         # look up values within a range

# source("../gisland/r/geo_inside.R")
source("r/FLYSHOOT utils.R")
source("r/FLYSHOOT add tripdata functions.r")

# load spatial datasets -------------------------

fao_sf          <- loadRData(file.path(spatialdir, "fao_sf.RData"))
fao_sf_area     <- fao_sf %>% filter(F_LEVEL=="MAJOR") %>% dplyr::select(F_AREA) %>% rename(area = F_AREA)
fao_sf_subarea  <- fao_sf %>% filter(F_LEVEL=="SUBAREA") %>% dplyr::select(F_SUBAREA) %>% rename(subarea = F_SUBAREA)
fao_sf_division <- fao_sf %>% filter(F_LEVEL=="DIVISION") %>% dplyr::select(F_DIVISION) %>% rename(division = F_DIVISION)

rect_sf         <- loadRData(file.path(spatialdir, "rect_sf.RData")) 
rect_sf2        <- rect_sf %>% dplyr::select(rect=ICESNAME) 
raw %>% group_by(catchdate) %>% summarise(n=n()) %>% View()

rect_df <-
  loadRData(file.path(spatialdir, "rect_df.RData")) %>% 
  rename(rect=ICESNAME) %>% 
  group_by(rect) %>% 
  filter(row_number() ==1) %>% 
  dplyr::select(rect, lon=long, lat)

# load fish biology datasets -------------------------

asfis <- loadRData(file.path(spatialdir, "asfis.RData"))

# load fishery datasets -------------------------

load(file.path(onedrive, "haul.RData"))
load(file.path(onedrive, "kisten.RData"))
load(file.path(onedrive, "elog.RData"))
load(file.path(onedrive, "elog_trek.RData"))
load(file.path(onedrive, "trip.RData"))

load(file.path(onedrive, "harbours.RData"))

# elog <- elog %>% filter(paste0(vessel, trip) %notin% c("SCH1352023344", "SCH1352023345","SCH1352023346", "SCH1352023347")) 
# save(elog,         file = file.path(onedrive, "elog.RData"))  

# ----------------------------------------------------------------------------
# inventory of files to be processed
# ----------------------------------------------------------------------------

pefa_trek_list <- list.files(
  path=file.path(tripdir, "/_te verwerken"),
  pattern="elog_pefa_per_trek",
  full.names = TRUE)

my_files <-
  data.frame(stringsAsFactors = FALSE) %>% 
  
  {if(!is_empty(pefa_trek_list)) {
    bind_rows(
      ., 
      data.frame(
        vessel = stringr::word(basename(pefa_trek_list), 1),
        trip   = stringr::word(basename(pefa_trek_list), 2),
        source = "pefa_trek",
        file   = pefa_trek_list
      )
    )} else {.}
  } 

my_trips <-
  my_files %>% 
  distinct(vessel, trip)


# ----------------------------------------------------------------------------
# loop over unique files
# ----------------------------------------------------------------------------
i <-1

for (i in 1:nrow(my_trips)) {
  
  my_vessel = my_trips[i,"vessel"]
  my_trip   = my_trips[i,"trip"]
  my_trip2  = gsub("_","",my_trip)
  
  print(paste(my_vessel, my_trip2))
  
  # HAUL from elog per trek ----------------------------------------------------
  
  if (any(grepl(paste(my_vessel, my_trip), pefa_trek_list))) {

    my_file <- filter(my_files,
                      vessel == my_vessel, trip== my_trip, source=="pefa_trek")$file
    
    raw <- get_raw_from_pefa_trek(my_file)
      
    h  <- get_haul_from_raw(raw)
    t  <- get_trip_from_haul(h, my_vessel, my_trip2) 
    m  <- get_kisten_from_pefa_trek(my_vessel, my_trip2, my_file)
    e  <- get_elog_from_pefa_trek(my_vessel, my_trip2, my_file)
    et <- get_pefa_trek(my_vessel, my_trip2, my_file) 
    
  } else {
    
    stop(paste("no PEFA haul information available for trip"), paste(my_vessel, my_trip))
  
  }
  
  if(add_data) {
    
    haul <- 
      haul %>% 
      filter(paste0(vessel, trip) %notin% paste0(h$vessel, h$trip)) %>% 
      bind_rows(h)
    
    trip <-
      trip %>%
      filter(paste0(vessel, trip) %notin% paste0(t$vessel, t$trip)) %>%
      bind_rows(t)
    
    kisten <-
      kisten %>%
      filter(paste0(vessel, trip) %notin% paste0(m$vessel, m$trip)) %>%
      bind_rows(m)
    
    elog <-
      elog %>%
      filter(paste0(vessel, trip) %notin% paste0(e$vessel, e$trip)) %>%
      bind_rows(e)
    
    elog_trek <-
      elog_trek %>%
      filter(paste0(vessel, trip) %notin% paste0(et$vessel, et$trip)) %>%
      bind_rows(et)
    
    save(haul,         file = file.path(onedrive, "haul.RData"))
    save(trip,         file = file.path(onedrive, "trip.RData"))
    save(kisten,  file = file.path(onedrive, "kisten.RData"))
    save(elog,  file = file.path(onedrive, "elog.RData"))
    save(elog_trek,         file = file.path(onedrive, "elog_trek.RData"))  
    
  }
  
  # KISTEN from elog per trek --------------------------------------------------  
  
  if (any(grepl(paste(my_vessel, my_trip), kisten_list))) {
    
    my_file <- filter(my_files, vessel == my_vessel, trip== my_trip, source=="kisten")$file
    
    m <- get_kisten(my_vessel, my_trip2, my_file, h) 
    
    # add to database
    if(add_data) {

      kisten <-
        kisten %>%
        filter(paste0(vessel, trip) %notin% paste0(m$vessel, m$trip)) %>%
        bind_rows(m)

      save(kisten,  file = file.path(onedrive, "kisten.RData"))

    }

    if (move_data) {
      file.copy(my_file, file.path(tripdir,my_vessel), overwrite = TRUE)
      file.remove(my_file)
    }
  
  } # end of Kisten 

  # PEFA ELOG  ----------------------------------------------------  
  
  if (any(grepl(paste(my_vessel, my_trip), pefa_list))) {
    
    my_file <- filter(my_files, vessel == my_vessel, trip== my_trip, source=="pefa")$file
    
    e <- get_pefa(my_vessel, my_trip2, my_file) 
    
    # add to database
    if(add_data) {
      
      elog <-
        elog %>%
        filter(paste0(vessel, trip) %notin% paste0(e$vessel, e$trip)) %>%
        bind_rows(e)
      
      # marelec_lot <- marelec_lot %>% dplyr::select(-haul2)
      save(elog,  file = file.path(onedrive, "elog.RData"))
      
    }
    
    if (move_data) {
      file.copy(my_file, file.path(tripdir,my_vessel), overwrite = TRUE)        
      file.remove(my_file)        
    } 
    
  } # end of PEFA elog

  
  # PEFA ELOG PER TREK ----------------------------------------------------  
  
  if (any(grepl(paste(my_vessel, my_trip), pefa_trek_list))) {
    
    my_file <- filter(my_files, vessel == my_vessel, trip== my_trip, source=="pefa_trek")$file
    
    et <- get_pefa_trek(my_vessel, my_trip2, my_file) 
    
    # add to database
    if(add_data) {
      
      elog_trek <-
        elog_trek %>%
        filter(paste0(vessel, trip) %notin% paste0(et$vessel, et$trip)) %>%
        bind_rows(et)
      
      # marelec_lot <- marelec_lot %>% dplyr::select(-haul2)
      save(elog_trek,         file = file.path(onedrive, "elog_trek.RData"))  
      
    }
    
    if (move_data) {
      file.copy(my_file, file.path(tripdir,my_vessel), overwrite = TRUE)        
      file.remove(my_file)        
    } 
    
  } # end of PEFA elog per trek
  
} # end of loop


# janitor::compare_df_cols(h, haul)
# haul <- haul %>% janitor::remove_empty(which = "cols")
# skimr::skim(h)





