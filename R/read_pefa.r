# ==============================================================================
# Read PEFA files
#
# 02/05/2023 Half way in redoing the elog data; need to check OLRAC, Ecatch33 and Ecatch20
# 08/05/2023 Finalized the elog data
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../prf/r/my utils.R")
source("../mptools/R/get_onedrive.r")

onedrive  <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/data")
onedrive2 <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")
tripdir   <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/tripdata")

# ------------------------------------------------------------------------------
# pefa historic data
# ------------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, "pefa"),
  pattern="pefa export",
  full.names = TRUE)

pefa  <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(filelist)) {
  pefa  <-
    bind_rows(
      pefa,
      readxl::read_excel(filelist[i],
                         col_names=TRUE, 
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
        data.frame() %>% 
        lowcase() %>% 
        {if(any(grepl("latitide",names(.)))) {rename(., lat = latitide)} else{.}} %>% 
        {if(any(grepl("latitude",names(.)))) {rename(., lat = latitude)} else{.}} %>% 
        {if(any(grepl("longitude",names(.)))) {rename(., lon = longitude)} else{.}} %>% 
        {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
        {if(any(grepl("tripidentifier",names(.)))) {rename(., trip = tripidentifier)} else{.}} %>% 
        rename(rect = icesrectangle) %>%
        mutate(across (any_of(c("meshsize", "boxes")), as.integer)) %>%
        mutate(across (any_of(c("catchdate", "departuredate", "arrivaldate", "auctiondate",
                                "weight", "conversionfactor", "lat","lon")), 
                       as.numeric)) %>%
        mutate(across(any_of(c("catchdate", "departuredate", "arrivaldate","auctiondate")), 
                      ~excel_timezone_to_utc(.,"UTC"))) %>% 
        mutate(month   = lubridate::month(catchdate),
               quarter = lubridate::quarter(catchdate),
               week    = lubridate::week(catchdate),
               yday    = lubridate::yday(catchdate),
               year    = lubridate::year(catchdate),
               date    = as.Date(catchdate)) %>%
        mutate(faozone = tolower(faozone)) %>% 
        mutate(file            = basename(filelist[i])) %>%
        rename(vessel          = vesselnumber) %>%
        mutate(vessel = gsub(" |0","",vessel)) %>% 
        mutate(source          = "pefa") %>% 
        relocate(vessel) 
      # relocate(remarks, .after = date)
    )
}

# ------------------------------------------------------------------------------
# pefa 2023 data
# ------------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir),
  pattern="elog pefa",
  recursive=TRUE,
  full.names = TRUE)

if(!is_empty(filelist)){
  
  e <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(filelist)) {
    
    myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog pefa", myvessel, mytrip))
    
    # TO DO: SET HAULID TO START AT 1 (haulid-min(haulid))
    
    e  <-
      bind_rows(
        e,
        readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          rename(rect = icesrectangle) %>% 
          # rename(vessel = vesselnumber) %>% 
          # mutate(vessel = gsub(" ","", vessel)) %>% 
          # mutate(vessel = ifelse(vessel=="SL09", "SL9","")) %>% 
          
          # rename(lat = latitude) %>% 
          # rename(lon = longitude) %>% 
          {if(any(grepl("latitide",names(.)))) {rename(., lat = latitide)} else{.}} %>% 
          {if(any(grepl("latitude",names(.)))) {rename(., lat = latitude)} else{.}} %>% 
          {if(any(grepl("longitude",names(.)))) {rename(., lon = longitude)} else{.}} %>% 
          {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
          
          mutate(across (any_of(c("boxes", "meshsize", "haul")),
                         as.integer)) %>%
          mutate(across (any_of(c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "weight", "lat", "lon", "conversionfactor")),
                         as.numeric)) %>%
          # mutate(across(c("catchdate"),
          #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
          mutate(across (any_of(c("catchdate", "departuredate","arrivaldate", "auctiondate")), 
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
          mutate(source="pefa")
        
      )

  } # end of pefa elog for loop
  
} # end of not empty filelist

janitor::compare_df_cols(pefa, e)

# ------------------------------------------------------------------------------
# pefa combined data
# ------------------------------------------------------------------------------

pefa <-
  bind_rows(
    pefa,
    e
  ) %>% 
  # rename(lat2 = lat, lon2 = lon) %>% 
  dplyr::select(-c(boxes,
                   weightundersized,
                   boxesundersized,
                   auctiondate,
                   auctionport,
                   tripstatus))

save(pefa, file=file.path(onedrive, "elog pefa.RData"))

skimr::skim(pefa)


