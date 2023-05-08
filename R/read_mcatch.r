# ==============================================================================
# Read M-Catch files
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
# mcatch history
# ------------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, "m-catch"),
  pattern="m-catch export",
  full.names = TRUE)

mcatch  <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(filelist)) {
  mcatch  <-
    bind_rows(
      mcatch,
      readxl::read_excel(filelist[i],
                         sheet="catch details table",
                         col_names=TRUE,
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>%
        data.frame() %>%
        lowcase() %>%
        rename(
          species   = fishspecie,
          weight    = catchweight,
          catchdate = activitydate,
          rect      = icesrectangle,
          economiczone = economicalzone
        ) %>%
        mutate(across (c("meshsize"), as.integer)) %>%
        mutate(across (c("lat","lon", "weight",
                         "catchdate", "departuredate", "arrivaldate","landingdate"), as.numeric)) %>%
        mutate(across(c("catchdate", "departuredate", "arrivaldate","landingdate"), ~excel_timezone_to_utc(.,"UTC"))) %>%
        mutate(month   = lubridate::month(catchdate),
               quarter = lubridate::quarter(catchdate),
               week    = lubridate::week(catchdate),
               yday    = lubridate::yday(catchdate),
               year    = lubridate::year(catchdate),
               date    = as.Date(catchdate)) %>%
        mutate(file            = basename(filelist[i])) %>%
        mutate(vessel          = stringr::word(file, 3)) %>%
        mutate(source          = "mcatch") %>%
        dplyr::select(-c(catchamount,
                         juliandate,
                         juvenile,
                         discard,
                         gearamount,
                         gearlength,
                         gearactivity,
                         faoarea,
                         faosubarea,
                         faodivision,
                         faosubdivision,
                         faounit,
                         zoneactivity,
                         vesselname,
                         vesselcfr,
                         vesselcallsign,
                         vesselgbrrss,
                         vesselhullnumber,
                         vesselflagstate,
                         tripidentifier,
                         tripid,
                         tripstatus,
                         activitydayofyear,
                         activitydayofyearcet,
                         vesselid,
                         entryid)) %>%
        relocate(vessel) %>%
        relocate(remarks, .after = date)
    )
}

# ------------------------------------------------------------------------------
# mcatch Z99 2023 (files by trip)
# ------------------------------------------------------------------------------

filelist <- list.files(
  path=file.path(tripdir, "Z99"),
  pattern="elog mcatch export",
  full.names = TRUE)

if(!is_empty(filelist)){
  
  e <- data.frame(stringsAsFactors = FALSE)
  
  # i <- 1
  for (i in 1:length(filelist)) {
    
    myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog mcatch", myvessel, mytrip))
    
    e  <-
      bind_rows(
        e, 
        
        readxl::read_excel(filelist[i], 
                           # sheet = "landed catch details table",
                           sheet = "catch details table",
                           col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          
          rename(
            catchdate=activitydate,
            rect = icesrectangle,
            vessel = vesselhullnumber,
            weight = catchweight,
            species = fishspecie,
            economiczone = economicalzone,
            # freshness = fishfreshness,
            #presentation = fishpresentation,
            #preservation = fishpreservation
          ) %>% 
          
          mutate(vessel = gsub("-","", vessel)) %>% 
          mutate(vessel = gsub("\\.","", vessel)) %>% 
          # mutate(vessel = ifelse(vessel=="SL09", "SL9", vessel)) %>%  
          
          mutate(across (c("catchdate", "meshsize"),
                         as.integer)) %>%
          mutate(across (c("departuredate","arrivaldate", "landingdate", "catchdate", "weight", "lon","lat"),
                         as.numeric)) %>%
          # mutate(across(c("catchdate"),
          #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
          mutate(across (c("catchdate", "departuredate","arrivaldate", "landingdate"), 
                         ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
          mutate(across (c("faozone"),
                         toupper)) %>%
          
          mutate(date   = as.Date(catchdate, origin="1899-12-30" , tz="Europe/Amsterdam")) %>% 
          # dplyr::select(-catchdate) %>% 
          
          # mutate(landingdate = as.character(landingdate)) %>% 
          
          mutate(
            year       = lubridate::year(date),
            quarter    = lubridate::quarter(date),
            month      = lubridate::month(date),
            week       = lubridate::week(date),
            yday       = lubridate::yday(date)) %>% 
          
          mutate(file            = basename(filelist[i])) %>%
          
          dplyr::select_if(names(.) %in% names(mcatch)) %>%
          
          # dplyr::select(-lat, -lon) %>% 
          # left_join(rect_df, by="rect") %>% 
          # mutate(lon = lon) %>% 
          # mutate(lat = lat) %>% 
          
          mutate(trip = mytrip) %>% 
          mutate(source="mcatch")
        
      )
    
  } # end of mcatch elog for loop
  
} # end of not empty filelist


mcatch <- bind_rows(mcatch, e)

save(mcatch, 
     file=file.path(onedrive, "elog mcatch.RData"))

# load(file=file.path(onedrive, "elog mcatch.RData"))

# skimr::skim(mcatch)
# inspectdf::inspect_num(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(mcatch) %>% inspectdf::show_plot()

# mcatch %>% ggplot(aes(x=lon, y=lat, colour=as.character(year))) + geom_point()
# mcatch %>% filter(weight > 1000) %>% arrange(desc(weight)) %>% View()


