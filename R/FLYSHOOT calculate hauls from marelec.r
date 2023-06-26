# =======================================================================================
# FLYSHOOT: calculate hauls from marelec.r
# 
#
# Martin Pastoors
#
# 19/06/2023 what hauls can we detect from marelec data
#
# TO DO: 
# 
# =========================================================================================

# print settings
options(max.print=999999)
options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

check_data = TRUE
add_data = TRUE
move_data = TRUE

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

# load spatial datasets -------------------------

load(file.path(spatialdir, "fao_sf.RData"))
load(file.path(spatialdir, "rect_sf.RData"))

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
# load(file.path(onedrive, "marelec_trip.RData"))
# load(file.path(onedrive, "marelec_trek.RData"))
load(file.path(onedrive, "elog.RData"))
load(file.path(onedrive, "trip.RData"))

# trip <-
#   trip %>% 
#   mutate(port = ifelse(port=="Boulogne", "Boulogne sur Mer", port)) %>% 
#   geocode(port, method = 'osm', lat = lat2 , lon = lon2) %>% 
#   mutate(
#     lat = ifelse(!is.na(lat2), lat2, lat),
#     lon = ifelse(!is.na(lon2), lon2, lon)
#   ) %>% 
#   
#   group_by(vessel, trip) %>% 
#   
#   # calculate distance between shoot and haul positions
#   mutate(distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat)))/1852 ) %>% 
#   
#   # add distance within haul if zero
#   mutate(distance = ifelse(distance == 0, 4.0, distance)) %>% 
#   dplyr::select(-lat2, -lon2)
  
  

# ----------------------------------------------------------------------------
# calculate hauls from marelec lot data
# ----------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, "/_te verwerken"),
  pattern="kisten",
  full.names = TRUE)

if(!is_empty(filelist)){

  # i <- 1
  for (i in 1:length(filelist)) {
    
    myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    mystartrow <-
      readxl::read_excel(filelist[i],
                         range="A1:A20",
                         col_names=FALSE, 
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      mutate(rownumber = row_number()) %>% 
      filter(tolower(X) == "lotnummer") %>% 
      dplyr::select(rownumber) %>% 
      as.integer()

    print(paste("kisten", myvessel, mytrip))
    
    m  <-
      readxl::read_excel(filelist[i],
                         skip=mystartrow-1,
                         col_names=TRUE, 
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      data.frame() %>% 
      lowcase() %>% 
      filter(!grepl("totaal", tolower(soorten))) %>%
      filter(!grepl("^einde", tolower(soorten))) %>%
      tidyr::drop_na(soorten) %>% 
      # tidyr::separate(soorten, into=c("soort", "species"), sep="/", remove=FALSE) %>% 
      mutate(maat = gsub("KLASSE ","", maat)) %>% 
      mutate(vessel = myvessel) %>% 
      mutate(trip = mytrip) %>% 
      mutate(datetime = lubridate::dmy_hms(paste(datum, tijd))) %>% 
      arrange(datetime) %>% 
      mutate(lotnummer = row_number()) %>% 
      mutate(gewicht = as.numeric(gewicht)) %>% 
      
      # assign haul; could be done with sqldf instead, but time registration is 
      # currently problematic
      arrange(datetime) %>% 
      mutate(time_diff = as.numeric(datetime - lag(datetime))/60) %>% 
      filter(time_diff > 20) %>% 
      mutate(haul = row_number()) %>% 
      mutate(haultime = datetime - minutes(5)) %>% 
      mutate(shoottime = haultime - lubridate::hm("1:20")) %>% 
      rename(date=datum) %>% 
      dplyr::select(-tijd) 

  } # end of marelec for loop
  
} # end of not empty filelist


# m %>% 
#   ggplot(aes(x=datetime, y=time_diff)) +
#   theme_publication() +
#   geom_point() +
#   scale_y_continuous(limits = c(0,120), breaks=seq(0,120,20))
