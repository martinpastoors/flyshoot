# =======================================================================================
# FLYSHOOT: pefa test
# 
# Function to read data, check it and add to RData sets
#
# Martin Pastoors
#
# 11/01/2023 First coding
# 20/01/2023 Added elog; added moving files
# 27/01/2023 Changed marelec lots to kisten; removed other marelec exports
# 23/02/2023 Changed pefa export to include position and time (by box)

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

selectvessel <- "SCH135"
selecttrip   <- "2023_329"
# ----------------------------------------------------------------------------
# read marelec lot data = kisten
# ----------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, selectvessel),
  pattern="kisten",
  full.names = TRUE)

filelist <- filelist[grepl(selecttrip, filelist)]

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
      mutate(haul = ifelse(time_diff > 20 | is.na(time_diff), 1, 0)) %>% 
      mutate(haul = cumsum(haul)) %>% 
      dplyr::select(-datum, -tijd) %>% 
      mutate(source="marelec") %>% 
      rename(
        dutch_name=soorten,
        sizeclass = maat,
        weight    = gewicht,
        catchdate = datetime
      )
    


  } # end of marelec for loop
  
} # end of not empty filelist


# ----------------------------------------------------------------------------
# read the old pefa elog data
# ----------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, selectvessel),
  pattern="elog pefa",
  full.names = TRUE)

filelist <- filelist[grepl(selecttrip, filelist)]

if(!is_empty(filelist)){
  
  i <- 1
  for (i in 1:length(filelist)) {

    myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog pefa", myvessel, mytrip))
    
    # TO DO: SET HAULID TO START AT 1 (haulid-min(haulid))
    
    eold  <-
      readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      data.frame() %>% 
      lowcase() %>% 
      rename(rect = icesrectangle) %>% 
      # rename(vessel = vesselnumber) %>% 
      # mutate(vessel = gsub(" ","", vessel)) %>% 
      # mutate(vessel = ifelse(vessel=="SL09", "SL9","")) %>% 
      
      rename(lat = latitude) %>% 
      rename(lon = longitude) %>% 

      {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
      
      mutate(across (one_of("boxes", "meshsize", "haul"),
                     as.integer)) %>%
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", 
                       "weight", "lat", "lon", "conversionfactor"),
                     as.numeric)) %>%
      # mutate(across(c("catchdate"),
      #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                     ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
      
      mutate(date   = as.Date(catchdate)) %>% 
      
      {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 
      # dplyr::select(-catchdate) %>% 
    
    mutate(
      year       = lubridate::year(date),
      quarter    = lubridate::quarter(date),
      month      = lubridate::month(date),
      week       = lubridate::week(date),
      yday       = lubridate::yday(date)
    ) %>% 

    mutate(weight = conversionfactor * weight) %>%       
    mutate(vessel = myvessel) %>% 
    mutate(trip = mytrip) %>% 
    mutate(source="pefa_old") %>% 
    left_join(dplyr::select(asfis,
                            species, dutch_name),
              by="species")


  } # end of pefa elog for loop

} # end of not empty filelist

# ----------------------------------------------------------------------------
# read the new pefa elog data
# ----------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, selectvessel),
  pattern="elog new pefa",
  full.names = TRUE)

if(!is_empty(filelist)){
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog pefa", myvessel, mytrip))
    
    # TO DO: SET HAULID TO START AT 1 (haulid-min(haulid))
    enew  <-
      readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      data.frame() %>% 
      lowcase() %>% 
      rename(rect = icesrectangle) %>% 
      # rename(vessel = vesselnumber) %>% 
      # mutate(vessel = gsub(" ","", vessel)) %>% 
      # mutate(vessel = ifelse(vessel=="SL09", "SL9","")) %>% 
      
      rename(lat = latitude) %>% 
      rename(lon = longitude) %>% 
      
      {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
      
      mutate(across (one_of("boxes", "meshsize", "haul"),
                     as.integer)) %>%
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "weight", "lat", "lon", "conversionfactor"),
                     as.numeric)) %>%
      # mutate(across(c("catchdate"),
      #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                     ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
      
      mutate(date   = as.Date(catchdate)) %>% 
      
      {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 
      # dplyr::select(-catchdate) %>% 
      
      mutate(
        year       = lubridate::year(date),
        quarter    = lubridate::quarter(date),
        month      = lubridate::month(date),
        week       = lubridate::week(date),
        yday       = lubridate::yday(date)) %>% 
      
      # left_join(rect_df, by="rect") %>% 
      # mutate(
      #   lat = lat + 0.25,
      #   lon = lon + 0.5
      # ) %>% 
      mutate(vessel = myvessel) %>% 
      mutate(trip = mytrip) %>% 
      mutate(source="pefa_new") %>% 
      left_join(dplyr::select(asfis,
                              species, dutch_name),
                by="species")
    

  } # end of pefa elog for loop
  
} # end of not empty filelist


# Combine
comb <-
  bind_rows(m, eold, enew) %>% 
  mutate(dutch_name = tolower(dutch_name)) %>% 
  mutate(dutch_name = gsub(" poon", "_poon", dutch_name)) %>% 
  mutate(dutch_name = gsub("rode mul", "mul", dutch_name))  %>% 
  mutate(dutch_name = gsub("gewone pijlinktvis", "inktvis", dutch_name))  %>% 
  mutate(dutch_name = ifelse(dutch_name=="dorade", "zeekarper", dutch_name)) %>% 
  mutate(dutch_name = stringr::word(dutch_name, 1, sep=" "))
 
# comb %>% distinct(dutch_name) %>% arrange(dutch_name) %>%  View()

# dutch_name
comb %>% 
  group_by(vessel, trip, source, dutch_name) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  # tidyr::pivot_wider(names_from = source, values_from = weight) %>% 
  reshape2::dcast(vessel+trip+dutch_name ~ source, sum, value.var="weight", margins="dutch_name") %>% 
  mutate(
    pefa_new_dev = scales::percent(pefa_new/marelec-1, accuracy=1),
    pefa_old_dev = scales::percent(pefa_old/marelec-1, accuracy=1)
  ) %>% 
  pander::pandoc.table(., 
                       style        = "simple",
                       split.tables = 200, 
                       split.cells  = c(rep(7,10)),
                       justify      = "right",
                       missing      =" ",
                       big.mark     = ',', 
                       round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )


# rect
comb %>% 
  group_by(vessel, trip, source, rect) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  # tidyr::pivot_wider(names_from = source, values_from = weight) %>% 
  reshape2::dcast(vessel+trip+rect ~ source, sum, value.var="weight", margins="dutch_name") %>% 
  mutate(
    pefa_new_dev = scales::percent(pefa_new/marelec-1, accuracy=1),
    pefa_old_dev = scales::percent(pefa_old/marelec-1, accuracy=1)
  ) %>% 
  pander::pandoc.table(., 
                       style        = "simple",
                       split.tables = 200, 
                       split.cells  = c(rep(7,10)),
                       justify      = "right",
                       missing      =" ",
                       big.mark     = ',', 
                       round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )


# lat*lon
comb %>% 
  mutate(latlon = paste(lat,lon)) %>% 
  group_by(vessel, trip, source, latlon) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  # tidyr::pivot_wider(names_from = source, values_from = weight) %>% 
  reshape2::dcast(vessel+trip+latlon ~ source, sum, value.var="weight", margins="dutch_name") %>% 
  mutate(
    pefa_new_dev = scales::percent(pefa_new/marelec-1, accuracy=1),
    pefa_old_dev = scales::percent(pefa_old/marelec-1, accuracy=1)
  ) %>% 
  pander::pandoc.table(., 
                       style        = "simple",
                       split.tables = 200, 
                       split.cells  = c(rep(7,10)),
                       justify      = "right",
                       missing      =" ",
                       big.mark     = ',', 
                       round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )
