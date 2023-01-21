# =======================================================================================
# FLYSHOOT: add tripdata.r
# 
# Function to read data, check it and add to RData sets
#
# Martin Pastoors
#
# 11/01/2023 First coding
# =========================================================================================

# print settings
options(max.print=999999)
options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

# check_data = TRUE
# add_data = TRUE
# move_data = TRUE
# my_data_drive    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata"
# my_rdata_drive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"
# my_spatial_drive = "C:/DATA/RDATA"
  
# Define function to read, check and add datasets
add_tripdata <- function(
    check_data       = TRUE, 
    add_data         = TRUE,
    move_data        = TRUE, 
    my_data_drive    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
    my_rdata_drive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
    my_spatial_drive = "C:/DATA/RDATA") {
  
  # Open relevant packages 
  library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
  library(readxl)        # excel reader from hadley
  library(writexl)       # write excel files
  library(zoo)           # manipulating data
  library(lubridate)     # date functions      (this is part of tidyverse, but needs to be loaded separately)
  library(stringr)       # string manipulation (this is part of tidyverse, but needs to be loaded separately)
  library(sf)            # simple features

  # source("../gisland/r/geo_inside.R")
  source("../prf/r/my utils.R")

  
  # load spatial datasets -------------------------
  
  load(file.path(my_spatial_drive, "fao_sf.RData"))
  load(file.path(my_spatial_drive, "rect_sf.RData"))
  
  # load fish biology datasets -------------------------
  
  afsis <- loadRData(file.path(my_spatial_drive, "afsis.RData"))

  # load fishery datasets -------------------------
  
  load(file.path(my_rdata_drive, "haul.RData"))
  load(file.path(my_rdata_drive, "marelec_trip.RData"))
  load(file.path(my_rdata_drive, "marelec_lot.RData"))
  load(file.path(my_rdata_drive, "marelec_trek.RData"))
  # load(file.path(my_spatial_drive, "pefa.RData"))

  # ----------------------------------------------------------------------------
  # read the treklijst data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(my_data_drive, "/_te verwerken"),
    pattern="treklijst",
    full.names = TRUE)
  
  # i <- 1
  for (i in 1:length(filelist)) {
    
    h  <-
      readxl::read_excel(filelist[i],
                 sheet = "Haul",  col_names=TRUE, col_types="text",
                 .name_repair =  ~make.names(., unique = TRUE))  %>% 
      data.frame() %>% 
      lowcase() %>% 
      filter(!is.na(vessel), !is.na(tijdeindehalen))
    
    if(nrow(h) > 0) {
      
      h <-
        h %>% 
        dplyr::select(
          vessel, trip, haul, date, 
          shoottime     = tijdbeginuitzetten, 
          haultime      = tijdeindehalen, 
          shootlat, shootns, shootlong, shootew,  
          winddirection, 
          windforce     = windforcebft,
          waterdepth, 
          catchheight   = catchhoogtevangstincm, 
          boxtype       = boxvolledigofkleinvk, 
          landingweight = marktwaardigeviskg,
          totalcatch    = totalevangstkgberekend, 
          bycatchperc   = percentagebijvangst, 
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
        
        mutate(across (c("vessel"), 
                       toupper)) %>% 
        mutate(across (c("haul", "meshsize","date", "windforce", "waterdepth",
                         "catchheight"), 
                       as.integer)) %>% 
        mutate(across (c("waterdepth","vertopening", "landingweight", "totalcatch"), 
                       as.numeric)) %>%
        mutate(across (c("shoottime","haultime"), 
                       ~calculate_time_from_string(.))) %>% 
        
        mutate(timezone = case_when(
          timezone == "CET"   ~ "Europe/Amsterdam", 
          timezone == "UTC+1" ~ "Europe/Amsterdam", 
          timezone == "UTC-5" ~ "America/Santiago",
          is.na(timezone)     ~ "UTC",    
          TRUE                ~ timezone) ) %>% 
        
        mutate(date   = as.Date(date, origin="1899-12-30" , tz=unique(timezone))) %>% 
        
        mutate( 
          shoottime = force_timezone_to_utc(t=date+shoottime, timezone=unique(timezone)),
          haultime  = force_timezone_to_utc(t=date+haultime, timezone=unique(timezone)),
        ) %>%
        
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
      
      # add to haul data
      haul <- 
        haul %>% 
        filter(paste0(vessel, trip) %notin% paste0(h$vessel, h$trip)) %>% 
        bind_rows(h)
      
      # haul <- data.frame(stringsAsFactors = FALSE)
      save(haul,         file = file.path(my_rdata_drive, "haul.RData"))  
      
      
    } # end of nrow(h) > 0
    

  } # end of treklijst for loop

  # haul <- haul %>% filter(vessel != "SCH135")  
  
  # ----------------------------------------------------------------------------
  # read marelec trip data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(my_data_drive, "/_te verwerken"),
    pattern="marelec_trip.xlsx",
    full.names = TRUE)

  
  if(!is_empty(filelist)){
    
    # i <- 1
    for (i in 1:length(filelist)) {
      
      mytrip <- 
        readxl::read_xlsx(
          path = filelist[i],
          range = "A1",
          col_names = FALSE,
          .name_repair = "unique") %>% 
        as.character() %>% 
        stringr::word(., 2, sep=" ") %>%
        str_extract_all(.,"\\(?[0-9]+\\)?") %>%
        unlist() %>% 
        paste0("2023", .)
      
      myvessel <-
        readxl::read_xlsx(
          path = filelist[i],
          range = "A4",
          col_names = FALSE,
          .name_repair = "unique") %>% 
        as.character() %>% 
        stringr::word(., 2, sep=" ") %>%
        gsub("-","",.) %>%
        unlist()     
      
      m  <-
        readxl::read_excel(filelist[i],
                           skip=7,
                           col_names=TRUE, 
                           col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
        data.frame() %>% 
        lowcase() %>% 
        filter(!grepl("Totaal", soorten)) %>%
        filter(!grepl("Einde dag", soorten)) %>%
        tidyr::drop_na(soorten) %>% 
        tidyr::separate(soorten, into=c("soort", "species"), sep="/", remove=FALSE) %>% 
        mutate(maat = gsub("KLASSE ","", maat)) %>% 
        mutate(vessel = myvessel) %>% 
        mutate(trip = mytrip) %>% 
        dplyr::select(vessel, trip, soorten, soort, species, maat, kisten, gewicht=totaalgewicht)
      
      marelec_trip <- 
        marelec_trip %>% 
        filter(paste0(vessel, trip) %notin% paste0(m$vessel, m$trip)) %>% 
        bind_rows(m)
      
      save(marelec_trip, file = file.path(my_rdata_drive, "marelec_trip.RData"))  
      
    } # end of marelec for loop
    
  }  # end of if statemen t

  # ----------------------------------------------------------------------------
  # read marelec lot data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(my_data_drive, "/_te verwerken"),
    pattern="marelec_lots.xlsx",
    full.names = TRUE)
  
  if(!is_empty(filelist)){
  
    # i <- 1
    for (i in 1:length(filelist)) {
      
      myvessel <- stringr::word(basename(filelist[i]), 1, sep=" ") %>%
        unlist()     
      mytrip <- stringr::word(basename(filelist[i]), 2, sep=" ") %>%
        gsub("_","",.) %>%
        unlist()     

      print(paste(myvessel, mytrip))
      
      m  <-
        readxl::read_excel(filelist[i],
                           skip=7,
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
        mutate(lotnummer = as.integer(lotnummer)) %>% 
        mutate(datetime = lubridate::dmy_hms(paste(datum, tijd))) %>% 
        mutate(gewicht = as.numeric(gewicht)) %>% 
        
        arrange(lotnummer) %>% 
        mutate(time_diff = as.numeric(datetime - lag(datetime))/60) %>% 
        mutate(haul = ifelse(time_diff > 15 | is.na(time_diff), 1, 0)) %>% 
        mutate(haul = cumsum(haul)) %>% 
        dplyr::select(-datum, -tijd) 
      
      # marelec_lot <- m    
      marelec_lot <- 
        marelec_lot %>% 
        filter(paste0(vessel, trip) %notin% paste0(m$vessel, m$trip)) %>% 
        bind_rows(m)
      
      # marelec_lot <- marelec_lot %>% dplyr::select(-haul2)
      save(marelec_lot,  file = file.path(my_rdata_drive, "marelec_lot.RData"))  
      
    } # end of marelec for loop
    
  } # end of if statement
  
  
  
  # ----------------------------------------------------------------------------
  # read marelec trek data
  # ----------------------------------------------------------------------------
  
  m2 <- data.frame(stringsAsFactors = FALSE)
  
  dirlist <- list.dirs(
    path=file.path(my_data_drive, "/_te verwerken"),
    full.names = TRUE) 
  dirlist <- dirlist[  grep("marelec",dirlist)]
  
  # print(dirlist)
  
  if(!is_empty(dirlist)){
  
    for (i in 1:length(dirlist)) {
      
      filelist <- list.files(
        path=dirlist[i],
        pattern="totalen",
        full.names = TRUE)
      
      # j <- 1
      for (j in 1:length(filelist)) {
        
        tmp <- readxl::read_xlsx(
          path = filelist[j],
          range = "A1",
          col_names = FALSE,
          .name_repair = "unique"
        ) %>% as.character()
        
        mytrip <-
          stringr::word(tmp, 2, sep=" ") %>%
          str_extract_all(.,"\\(?[0-9]+\\)?") %>%
          unlist() %>% 
          paste0("2023", .)
        
        myhaul <-
          stringr::word(tmp, 4, sep=" ") %>%
          str_extract_all(.,"\\(?[0-9]+\\)?") %>%
          unlist() %>%
          as.integer()  
        myhaul = ifelse(myhaul >= 25, myhaul+1, myhaul)
        
        myvessel <-
          readxl::read_xlsx(
            path = filelist[j],
            range = "A4",
            col_names = FALSE,
            .name_repair = "unique") %>% 
          as.character() %>% 
          stringr::word(., 2, sep=" ") %>%
          gsub("-","",.) %>%
          unlist()     
        
        m2 <-
          bind_rows(
            m2,
            readxl::read_xlsx(
              path = filelist[j],
              range = "A8:C100") %>%
              lowcase() %>%
              filter(!is.na(soorten)) %>%
              filter(!grepl("Totaal", soorten)) %>%
              filter(!grepl("Einde dag", soorten)) %>%
              mutate(
                vessel = myvessel,
                trip = mytrip,
                haul = myhaul)
          )
      } # end of j loop
      
      marelec_trek <- 
        marelec_trek %>% 
        filter(paste0(vessel, trip) %notin% paste0(m2$vessel, m2$trip)) %>% 
        bind_rows(m2)
      
      save(marelec_trek, file = file.path(my_rdata_drive, "marelec_trek.RData"))  
      
    } # end of marelec trek for loop
    
  } # end of if statement
  
  
  # ----------------------------------------------------------------------------
  # read pefa data
  # ----------------------------------------------------------------------------

  # To be done

  # ----------------------------------------------------------------------------
  # add data to datasets after first removing trips that were already included
  # ----------------------------------------------------------------------------

  # haul <- h; marelec <- m; marelec_trek <- m2
  
  # move data files 
  
  if (move_data) {
    
    # TO BE DONE
    
  }
  
  
} # End of function




add_tripdata (check_data = TRUE, 
              add_data = TRUE,
              move_data = TRUE,
              my_data_drive    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
              my_rdata_drive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
              my_spatial_drive = "C:/DATA/RDATA")
  
