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

source("R/FLYSHOOT utils.R")

tripdir   <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/LO/data"

# ------------------------------------------------------------------------------
# pefa data
# ------------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir),
  pattern="pefa",
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

skimr::skim(e)


glimpse(e)

# summarise by presentation
e %>% 
  mutate(weight = as.numeric(ifelse(is.na(weight),0,weight)) + as.numeric(ifelse(is.na(weightundersized), 0, weightundersized))) %>% 
  group_by(vesselnumber, presentation) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  mutate(presentation = factor(presentation, levels=c("WHL","GUT","OTH","ROE","DIS","CLA"))) %>% 
  reshape2::dcast(vesselnumber ~ presentation, value.var="weight", sum, margins = c("vesselnumber","presentation")) %>% 

  pander::pandoc.table(.,
                       style = "simple",
                       split.tables=400, 
                       justify = "right",
                       missing=".",
                       big.mark=".",
                       round=c(0))


# summarise by discard reason
e %>% 
  mutate(weight = as.numeric(ifelse(is.na(weight),0,weight)) + as.numeric(ifelse(is.na(weightundersized), 0, weightundersized))) %>% 
  filter(presentation == "DIS") %>% 
  mutate(discardreason = stringr:: str_sub(discardreason, 1, 3)) %>% 
  group_by(vesselnumber, discardreason) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  reshape2::dcast(vesselnumber ~ discardreason, value.var="weight", sum, margins = c("vesselnumber","discardreason")) %>% 
  
  pander::pandoc.table(.,
                       style = "simple",
                       split.tables=400, 
                       justify = "right",
                       missing=".",
                       big.mark=".",
                       round=c(0))


# summarise by month
e %>% 
  mutate(weight = as.numeric(ifelse(is.na(weight),0,weight)) + as.numeric(ifelse(is.na(weightundersized), 0, weightundersized))) %>% 
  filter(presentation == "DIS") %>% 
  group_by(vesselnumber, month) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  reshape2::dcast(vesselnumber ~ month, value.var="weight", sum, margins = c("vesselnumber","month")) %>% 
  
  pander::pandoc.table(.,
                       style = "simple",
                       split.tables=400, 
                       justify = "right",
                       missing=".",
                       big.mark=".",
                       round=c(0))
