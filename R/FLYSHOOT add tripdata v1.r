# =======================================================================================
# FLYSHOOT: add tripdata v1.r
# 
# Function to read data, check it and add to RData sets
#
# Martin Pastoors
#
# 11/01/2023 First coding
# 20/01/2023 Added elog; added moving files
# 27/01/2023 Changed marelec lots to kisten; removed other marelec exports

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
  
# Define function to read, check and add datasets
# add_tripdata <- function(
#     check_data       = TRUE, 
#     add_data         = TRUE,
#     move_data        = TRUE, 
#     tripdir    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
#     onedrive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
#     spatialdir = "C:/DATA/RDATA") {
  
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
  
  afsis <- loadRData(file.path(spatialdir, "afsis.RData"))

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
  # read the treklijst data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(tripdir, "/_te verwerken"),
    pattern="treklijst",
    full.names = TRUE)
  
  # i <- 1
  if(!is_empty(filelist)){
    
    for (i in 1:length(filelist)) {
    
    # vessel and trip
      myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ")
  
      mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
        gsub("_","",.) %>%
        unlist()     
      # mytrip <-
      #   stringr::word(basename(filelist[i]), 2, sep=" ") %>%
      #   str_extract_all(.,"\\(?[0-9,\\-]+\\)?") %>%
      #   unlist() %>% 
      #   paste(., collapse="")
      
      # number of used rows    
      r <-
        readxl::read_excel(filelist[i],
                   sheet = "Haul",  col_names=TRUE, col_types="text",
                   .name_repair =  ~make.names(., unique = TRUE))  %>% 
        data.frame() %>% 
        lowcase() %>% 
        summarise(
          ndate = max(
            sum(!is.na(date)),
            sum(!is.na(tijdeindehalen)),
            sum(!is.na(tijdbeginuitzetten)),
            sum(!is.na(tijdeindeuitzetten)),
            sum(!is.na(shootlat)),
            sum(!is.na(shootlong)),
            sum(!is.na(waterdepth)))
        ) %>% 
        as.integer()
  
      
      if(r > 0) {
        
        h  <-
          readxl::read_excel(filelist[i],
                             sheet = "Haul",  col_names=TRUE, col_types="text",
                             .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          
          filter(!is.na(vessel), row_number() <= r ) %>% 
          dplyr::select(
            vessel, trip, haul, date, 
            # shoottime     = tijdbeginuitzetten, 
            # haultime      = tijdeindehalen, 
            shoottime    = berekentijdbeginuitzetten,
            haultime     = berekentijdeindehalen,
            shootlat, shootns, shootlong, shootew,  
            winddirection, 
            windforce     = windforcebft,
            waterdepth, 
            catchheight   = catchhoogtevangstincm, 
            boxtype       = boxvolledigofkleinvk, 
            landingweight = marktwaardigeviskg,
            totalcatch    = totalevangstkgberekend, 
            bycatchperc   = percentagebijvangst, 
            skipper,
            dateembarked = dateembarkedutcdate,
            portembarked = portofembarkation,
            datedisembarked = datedisembarkedutcdate,
            portdisembarked = portofdisembarkation,
            gear, meshsize, vertopening, 
            cablelength   = cablelengthm, 
            cablethickness= cablethicknessmm,
            lengthgroundrope= lengthgroundropem,
            escapepanel   = escapepanelyn, 
            timezone
          )  %>%  
          
          # fill up empty cells
          mutate(across (c("date","shootlat","shootns", "shootlong", "shootew",
                           "winddirection","windforce"),
                         ~zoo::na.locf(.))) %>% 
          
          mutate(across (c("vessel", "shootew", "shootns"), 
                         toupper)) %>% 
          mutate(across (c("haul", "meshsize","date", "windforce", "waterdepth",
                           "catchheight", "dateembarked","datedisembarked"), 
                         as.integer)) %>% 
          mutate(across (c("waterdepth","vertopening", "landingweight", "totalcatch",
                           "shoottime", "haultime"), 
                         as.numeric)) %>%
          # mutate(across (c("shoottime","haultime"), 
          #                ~calculate_time_from_string(.))) %>% 
          mutate(across (c("shoottime","haultime"), 
                         ~calculate_time(.))) %>% 
          
          mutate(timezone = case_when(
            timezone == "CET"   ~ "Europe/Amsterdam", 
            timezone == "UTC+1" ~ "Europe/Amsterdam", 
            timezone == "UTC-5" ~ "America/Santiago",
            is.na(timezone)     ~ "UTC",    
            TRUE                ~ timezone) ) %>% 
          
          mutate(date   = as.Date(date, origin="1899-12-30" , tz=unique(timezone))) %>% 
          
          mutate(dateembarked   = as.Date(dateembarked, origin="1899-12-30" , tz=unique(timezone))) %>% 
          mutate(datedisembarked   = as.Date(datedisembarked, origin="1899-12-30" , tz=unique(timezone))) %>% 
          
          mutate( 
            # shoottime = force_timezone_to_utc(t=date+shoottime, timezone=unique(timezone)),
            # haultime  = force_timezone_to_utc(t=date+haultime, timezone=unique(timezone)),
            shoottime = date + shoottime,
            haultime  = date + haultime
          ) %>%
          
          # add haultime if missing
          mutate(
            haultime = ifelse(is.na(haultime) & !is.na(shoottime), 
                              shoottime+lubridate::hm("1:20"), 
                              haultime),
            haultime = lubridate::as_datetime(haultime),
            
            # special case for SCH135 2023320 trip
            haultime = ifelse(trip == "2023320", 
                              haultime-lubridate::hm("1:00"), 
                              haultime),
            haultime = lubridate::as_datetime(haultime)
          ) %>% 
        
          # add next haul time  
          group_by(vessel, trip) %>% 
          mutate(nexthaultime = lead(haultime)) %>% 
          mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
          mutate(nexthaultime = as_datetime(nexthaultime)) %>% 

          mutate(
            year       = lubridate::year(date),
            quarter    = lubridate::quarter(date),
            month      = lubridate::month(date),
            week       = lubridate::week(date),
            yday       = lubridate::yday(date)) %>%
          dplyr::select(-timezone) %>%
          
          # calculate haul duration: haul_time-shoot_time*24 
          mutate(duration   = as.numeric(as.duration(shoottime %--% haultime))/3600 ) %>% 
          
          # calculate positions
          mutate(
            lon = calculate_position_from_strings(shootlat, shootns, shootlong, shootew)$lon,
            lat = calculate_position_from_strings(shootlat, shootns, shootlong, shootew)$lat
          )  %>% 
          
          # wind directions
          mutate(
            winddirection = toupper(winddirection),
            winddirection = gsub("Z","S",winddirection),
            winddirection = gsub("O","E",winddirection)
          ) %>% 
          
          ungroup()
        
        # calculate FAO areas  
        h_fao <- 
          h %>%
          drop_na(lat, lon) %>% 
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = TRUE) %>% 
          sf::st_join(., fao_sf, join = st_within) %>% 
          sf::st_drop_geometry() %>% 
          dplyr::select(vessel, trip, haul, F_LEVEL, F_CODE) %>%
          mutate(F_LEVEL = tolower(F_LEVEL)) %>% 
          mutate(F_LEVEL = ifelse(F_LEVEL=="major", "area", F_LEVEL)) %>% 
          tidyr::pivot_wider(names_from = F_LEVEL, values_from = F_CODE)
        
        # calculate ICES rectangles
        h_rect <- 
          h %>%
          drop_na(lat, lon) %>% 
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = TRUE) %>% 
          sf::st_join(., rect_sf, join = st_within) %>% 
          sf::st_drop_geometry() %>% 
          dplyr::select(vessel, trip, haul, rect=ICESNAME) 
        
        h <- 
          left_join(h, h_fao,  by=c("vessel","trip","haul")) %>% 
          left_join(., h_rect, by=c("vessel","trip","haul"))
        
        if (any(h$year != 2023)) stop ("Error: year not in 2023")
        
        # trip
        tmp <-
          h %>% 
          filter(row_number() == 1) %>% 
          dplyr::select(dateembarked, datedisembarked, portembarked, portdisembarked) %>% 
          t() %>% 
          data.frame() %>%
          setNames("value") %>% 
          rownames_to_column(var="variable") %>% 
          mutate(
            action   = ifelse(grepl("disembarked", variable), "disembarked", "embarked"),
            variable = gsub("disembarked|embarked","", variable) 
          )
        
        t <- 
          bind_cols(
            tmp %>% filter(row_number() <= 2) %>% dplyr::select(action, date=value),
            tmp %>% filter(row_number() > 2)  %>% dplyr::select(port=value)
          ) %>% 
          mutate(date   = as.Date(date, origin="1899-12-30" , tz=unique(timezone))) %>% 
          mutate(port   = ifelse(port=="Boulogne", "Boulogne sur Mer", port)) %>% 
          
          geocode(port, method = 'osm', lat = lat , lon = lon) %>% 
          mutate(haul = ifelse(row_number() == 1, 0, r+1)) %>% 
          bind_rows(dplyr::select(h,
                                  haul, date, lat, lon)) %>% 
          arrange(haul) %>% 
          
          # calculate distance between shoot and haul positions
          mutate(distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat)))/1852 ) %>% 
          
          # add distance within haul if zero
          mutate(distance = ifelse(distance == 0, 4.0, distance)) %>% 
          
          # add vessel and trip
          mutate(
            vessel = myvessel, 
            trip   = mytrip
          )      
  
        # add to haul and trip data
        if(add_data) {
          
          haul <- 
            haul %>% 
            filter(paste0(vessel, trip) %notin% paste0(h$vessel, h$trip)) %>% 
            bind_rows(h)
  
          trip <-
            trip %>% 
            filter(paste0(vessel, trip) %notin% paste0(t$vessel, t$trip)) %>% 
            bind_rows(t)
  
          # haul <- data.frame(stringsAsFactors = FALSE)
          save(haul,         file = file.path(onedrive, "haul.RData"))  
          save(trip,         file = file.path(onedrive, "trip.RData"))  
        }
        
        if (move_data) {
          file.copy(filelist[i], file.path(tripdir, myvessel), overwrite = TRUE)        
          file.remove(filelist[i])        
        } 
        
        
      } # end of nrow(h) > 0
    
    } # end of treklijst for loop
    
  } # end of not empty filelist
  
  # haul <- haul %>% filter(vessel != "SCH135")  
  
  # ----------------------------------------------------------------------------
  # read marelec lot data = kisten
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
      
      # hauls for this trip
      # if(!exists("h")) {
        h <- haul %>% filter(vessel ==myvessel, trip==mytrip)
        if(!exists("h")) stop(paste("Probleem: treklijst niet beschikbaar voor",myvessel, mytrip))
        if(nrow(h)==0)   stop(paste("Probleem: treklijst leeg",myvessel, mytrip))
        # }
      
      # print(paste(myvessel, mytrip))
      
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
        mutate(haul2 = ifelse(time_diff > 20 | is.na(time_diff), 1, 0)) %>% 
        mutate(haul2 = cumsum(haul2)) %>% 
        dplyr::select(-datum, -tijd) 
      
      # add calculated hauls from haul list
      t <-
        sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
                             m.datetime, m.haul2, h.haul from m
              join h on m.vessel   == h.vessel and
                        m.trip     == h.trip and
                        m.datetime >= h.haultime and
                        m.datetime <  h.nexthaultime") %>%
        as_tibble()
      
      if(nrow(t)==0) stop(paste("Probleem bij toewijzing trekken in m van",myvessel, mytrip))
      
      m <- left_join(m, 
                     dplyr::select(t,
                                   lotnummer, haul),
                     by="lotnummer")      
        

      # add to database
      if(add_data) {
        
        kisten <- 
          kisten %>% 
          filter(paste0(vessel, trip) %notin% paste0(m$vessel, m$trip)) %>% 
          bind_rows(m)
        
        # marelec_lot <- marelec_lot %>% dplyr::select(-haul2)
        save(kisten,  file = file.path(onedrive, "kisten.RData"))  
        
      }
      
      if (move_data) {
        file.copy(filelist[i], file.path(tripdir,myvessel), overwrite = TRUE)        
        file.remove(filelist[i])        
      } 
      
    } # end of marelec for loop
    
  } # end of not empty filelist
  
  
  

  # ----------------------------------------------------------------------------
  # read the pefa elog data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(tripdir, "/_te verwerken"),
    pattern="elog pefa",
    full.names = TRUE)
  
  if(!is_empty(filelist)){
    
    # i <- 1
    for (i in 1:length(filelist)) {
  
      myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
        unlist()     
      mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
        gsub("_","",.) %>%
        unlist()     
      
      print(paste("elog pefa", myvessel, mytrip))
      
      e  <-
        readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
        data.frame() %>% 
        lowcase() %>% 
        rename(rect = icesrectangle) %>% 
        rename(vessel = vesselnumber) %>% 
        mutate(vessel = gsub(" ","", vessel)) %>% 
        mutate(vessel = ifelse(vessel=="SL09", "SL9","")) %>% 
        
        mutate(across (c("boxes", "catchdate", "meshsize"),
                       as.integer)) %>%
        mutate(across (c("departuredate","arrivaldate", "catchdate", "weight"),
                       as.numeric)) %>%
        # mutate(across(c("catchdate"),
        #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
        mutate(across (c("departuredate","arrivaldate"), 
                       ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
        
        mutate(date   = as.Date(catchdate, origin="1899-12-30" , tz="Europe/Amsterdam")) %>% 
        dplyr::select(-catchdate) %>% 
      
      mutate(
        year       = lubridate::year(date),
        quarter    = lubridate::quarter(date),
        month      = lubridate::month(date),
        week       = lubridate::week(date),
        yday       = lubridate::yday(date)) %>% 
        
        left_join(rect_df, by="rect") %>% 
        mutate(
          lat = lat + 0.25,
          lon = lon + 0.5
        ) %>% 
        mutate(trip = mytrip) %>% 
        mutate(source="pefa")
  
      # add to database
      if(add_data) {
        
        elog <- 
          elog %>%
          filter(paste0(vessel, trip) %notin% paste0(myvessel, mytrip)) %>%
          bind_rows(e)
        
        save(elog,         file = file.path(onedrive, "elog.RData"))  
        
      }
      
      if (move_data) {
        file.copy(filelist[i], file.path(tripdir,myvessel), overwrite = TRUE)        
        file.remove(filelist[i])        
      } 
      
    } # end of pefa elog for loop

  } # end of not empty filelist
  
  # ----------------------------------------------------------------------------
  # read the m-catch elog data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(tripdir, "_te verwerken"),
    pattern="elog mcatch",
    full.names = TRUE)
  
  if(!is_empty(filelist)){
    
    # i <- 1
    for (i in 1:length(filelist)) {
  
      myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
        unlist()     
      mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
        gsub("_","",.) %>%
        unlist()     
      
      print(paste("elog mcatch", myvessel, mytrip))
      
      e  <-
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
        mutate(across (c("departuredate","arrivaldate", "landingdate"), 
                       ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
        mutate(across (c("faozone"),
                       toupper)) %>%
        
        mutate(date   = as.Date(catchdate, origin="1899-12-30" , tz="Europe/Amsterdam")) %>% 
        dplyr::select(-catchdate) %>% 
        
        mutate(landingdate = as.character(landingdate)) %>% 
        
        mutate(
          year       = lubridate::year(date),
          quarter    = lubridate::quarter(date),
          month      = lubridate::month(date),
          week       = lubridate::week(date),
          yday       = lubridate::yday(date)) %>% 
        
        dplyr::select(-lat, -lon) %>% 
        left_join(rect_df, by="rect") %>% 
        
        dplyr::select_if(names(.) %in% names(elog)) %>%
        
        mutate(trip = mytrip) %>% 
        mutate(source="m-catch")
      
      
      # add to database
      if(add_data) {
        
        elog <- 
          elog %>%
          filter(paste0(vessel, trip) %notin% paste0(myvessel, mytrip)) %>%
          bind_rows(e)
        
        
        save(elog,         file = file.path(onedrive, "elog.RData"))  
        
      }
      
      if (move_data) {
        file.copy(filelist[i], file.path(tripdir,myvessel), overwrite = TRUE)        
        file.remove(filelist[i])        
      } 
  
      
    } # end of mcatch elog for loop
  
  } # end of not empty filelist
  
# } # End of function

# add_tripdata (check_data = TRUE, 
#               add_data = TRUE,
#               move_data = TRUE,
#               tripdir    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
#               onedrive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
#               spatialdir = "C:/DATA/RDATA")
  
