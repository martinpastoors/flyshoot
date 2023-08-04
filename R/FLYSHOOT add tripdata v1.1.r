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
add_data   = TRUE
move_data  = TRUE

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
load(file.path(onedrive, "elog_trek.RData"))
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
# inventory of files to be processed
# ----------------------------------------------------------------------------

treklijst_list <- list.files(
  path=file.path(tripdir, "/_te verwerken"),
  pattern="treklijst",
  full.names = TRUE)

kisten_list <- list.files(
  path=file.path(tripdir, "/_te verwerken"),
  pattern="kisten",
  full.names = TRUE)

pefa_list <- list.files(
  path=file.path(tripdir, "/_te verwerken"),
  pattern="elog pefa",
  full.names = TRUE)

pefa_trek_list <- list.files(
  path=file.path(tripdir, "/_te verwerken"),
  pattern="elog_pefa_per_trek",
  full.names = TRUE)

mcatch_list <- list.files(
  path=file.path(tripdir, "_te verwerken"),
  pattern="elog mcatch",
  full.names = TRUE)

my_files <-
  data.frame(stringsAsFactors = FALSE) %>% 
  
  {if(!is_empty(treklijst_list)) {
    bind_rows(
      .,
      data.frame(
        vessel = stringr::word(basename(treklijst_list), 1),
        trip   = stringr::word(basename(treklijst_list), 2),
        source = "treklijst",
        file   = kisten_list
      )      
    )}  else {.}
  } %>% 
  
  {if(!is_empty(kisten_list)) {
    bind_rows(
      .,
      data.frame(
        vessel = stringr::word(basename(kisten_list), 1),
        trip   = stringr::word(basename(kisten_list), 2),
        source = "kisten",
        file   = kisten_list
      )      
    )}  else {.}
  } %>% 
  
  {if(!is_empty(pefa_list)) {
    bind_rows(
      ., 
      data.frame(
        vessel = stringr::word(basename(pefa_list), 1),
        trip   = stringr::word(basename(pefa_list), 2),
        source = "pefa",
        file   = kisten_list
      )
    )} else {.}
  } %>% 
  
  {if(!is_empty(pefa_trek_list)) {
    bind_rows(
      ., 
      data.frame(
        vessel = stringr::word(basename(pefa_trek_list), 1),
        trip   = stringr::word(basename(pefa_trek_list), 2),
        source = "pefa_trek",
        file   = kisten_list
      )
    )} else {.}
  } %>% 
  
  {if(!is_empty(mcatch_list)) {
    bind_rows(
      ., 
      data.frame(
        vessel = stringr::word(basename(mcatch_list), 1),
        trip   = stringr::word(basename(mcatch_list), 2),
        source = "mcatch",
        file   = kisten_list
      )
    )} else {.}
  } 

my_trips <-
  my_files %>% 
  distinct(vessel, trip)

# ----------------------------------------------------------------------------
# Just in case: calculate hauls from marelec lot data
# ----------------------------------------------------------------------------

if(!is_empty(kisten_list)){
  
  hh <- data.frame(stringsAsFactors = FALSE)
  
  i <- 1
  for (i in 1:length(kisten_list)) {
    
    myvessel <- stringr::word(basename(kisten_list[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(kisten_list[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    mystartrow <-
      readxl::read_excel(kisten_list[i],
                         range="A1:A20",
                         col_names=FALSE, 
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      mutate(rownumber = row_number()) %>% 
      filter(tolower(X) == "lotnummer") %>% 
      dplyr::select(rownumber) %>% 
      as.integer()
    
    print(paste("getting hauls from marelec kisten", myvessel, mytrip))
    
    hh  <-
      bind_rows(
        
        hh,
        
        readxl::read_excel(kisten_list[i],
                           skip=mystartrow-1,
                           col_names=TRUE, 
                           col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
          data.frame() %>% 
          lowcase() %>% 
          filter(!grepl("totaal", tolower(soorten))) %>%
          filter(!grepl("^einde", tolower(soorten))) %>%
          tidyr::drop_na(soorten) %>% 
          
          mutate(vessel = myvessel) %>% 
          mutate(trip = mytrip) %>% 
          
          mutate(datetime = lubridate::dmy_hms(paste(datum, tijd))) %>% 
          arrange(datetime) %>% 
          mutate(lotnummer = row_number()) %>% 
          
          arrange(datetime) %>% 
          mutate(time_diff = as.numeric(datetime - lag(datetime))/60) %>% 
          filter(time_diff > 20 | row_number()==1) %>% 
          mutate(haul = row_number()) %>% 
          mutate(haultime = datetime - minutes(5)) %>% 
          mutate(shoottime = haultime - lubridate::hm("1:20")) %>% 
          mutate(shoottime2 = shoottime + (haultime - shoottime)/2) %>% 
          rename(date=datum) %>%
          mutate(date=dmy(date)) %>% 
          
          # add next haul time  
          group_by(vessel, trip) %>% 
          mutate(nexthaultime = lead(haultime)) %>% 
          mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
          mutate(nexthaultime = as_datetime(nexthaultime)) %>% 
          
          dplyr::select(vessel, trip, haul, date, shoottime, shoottime2, haultime, nexthaultime) 
        
      )
    
  } # end of marelec for loop
  
} # end of not empty kisten_list


# ----------------------------------------------------------------------------
# read the treklijst data
# ----------------------------------------------------------------------------

i <- 1
if(!is_empty(treklijst_list)){
  
  for (i in 1:length(treklijst_list)) {
  
  # vessel and trip
    myvessel <- stringr::word(basename(treklijst_list[i]), 1, sep=" ")

    mytrip <- stringr::word(basename(treklijst_list[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    # mytrip <-
    #   stringr::word(basename(treklijst_list[i]), 2, sep=" ") %>%
    #   str_extract_all(.,"\\(?[0-9,\\-]+\\)?") %>%
    #   unlist() %>% 
    #   paste(., collapse="")
    
    # number of used rows    
    r <-
      readxl::read_excel(treklijst_list[i],
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
      
      # check in trip number
      if (c(readxl::read_excel(treklijst_list[i], 
                               sheet = "Haul",  
                               col_names=TRUE, 
                               .name_repair =  ~make.names(., unique = TRUE)) %>% 
            filter(row_number()==1) %>% 
            dplyr::pull(trip)) != mytrip) 
        stop(paste(basename(treklijst_list[i]), ": tripnumber problem in list not equal to tripnumber in filename"))

      # first read the haul data
      t <-
        readxl::read_excel(treklijst_list[i],
                           sheet = "Haul",  col_names=TRUE, col_types="text",
                           .name_repair =  ~make.names(., unique = TRUE))  %>% 
        data.frame() %>% 
        lowcase() %>% 
        filter(!is.na(vessel), row_number() <= r ) %>% 
        mutate(timezone = case_when(
          timezone == "CET"   ~ "Europe/Amsterdam", 
          timezone == "UTC+1" ~ "Europe/Amsterdam", 
          timezone == "UTC-5" ~ "America/Santiago",
          is.na(timezone)     ~ "UTC",    
          TRUE                ~ timezone) ) %>% 
        mutate(date   = as.Date(as.integer(date), origin="1899-12-30", tz=timezone)) 
        
      
      # if times are used, get haul data from this list; otherwise get haul data from hh (marelec)      
      if(sum(!is.na(t$tijdbeginuitzetten)) + sum(!is.na(t$tijdeindeuitzetten)) + sum(!is.na(t$tijdeindehalen))  > 0) {

        # case 1: read all data-time data from haullist
        
        h  <-
          t %>% 
          dplyr::rename(
            shoottime    = berekentijdbeginuitzetten,
            shoottime2   = berekentijdeindeuitzetten,
            haultime     = berekentijdeindehalen
          )  %>%  
          
          mutate(across (c("shoottime", "shoottime2", "haultime"), 
                         as.numeric)) %>%
          
          mutate(across (c("shoottime","shoottime2", "haultime"),
                         ~calculate_time(.))) %>%
          
          mutate( 
            shoottime  = ifelse(!is.na(shoottime), date + shoottime, NA),
            shoottime  = as_datetime(shoottime),
            shoottime2 = ifelse(!is.na(shoottime2), date + shoottime2, NA),
            shoottime2 = as_datetime(shoottime2),
            haultime   = ifelse(!is.na(haultime), date + haultime, NA),
            haultime   = as_datetime(haultime)
          ) %>%
          
          # add haultime if missing
          mutate(
            haultime = ifelse(is.na(haultime) & !is.na(shoottime), 
                              shoottime+lubridate::hm("1:20"), 
                              haultime),
            haultime = lubridate::as_datetime(haultime)
          ) %>% 
          
          # add haultime and shoottime if missing and shoottime2 is available
          mutate(
            test     = ifelse(is.na(haultime) & is.na(shoottime) & !is.na(shoottime2),
                              TRUE, FALSE),
            shoottime = ifelse(test, 
                               shoottime2-lubridate::hm("0:40"), 
                               shoottime),
            shoottime = lubridate::as_datetime(shoottime),
            
            haultime = ifelse(test, 
                              shoottime2+lubridate::hm("0:40"), 
                              haultime),
            haultime = lubridate::as_datetime(haultime)
          ) %>% 
          
          # add next haul time  
          group_by(vessel, trip) %>% 
          mutate(nexthaultime = lead(haultime)) %>% 
          mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
          mutate(nexthaultime = as_datetime(nexthaultime)) 
          
          

      } else {
        
        # start from hh list and add haul information
        h <-
          hh %>% 
          filter(vessel == myvessel, trip == mytrip) %>% 
          left_join(dplyr::select(t,
                                  -haul, -tijdbeginuitzetten, -tijdeindeuitzetten, -tijdeindehalen), 
                    by=c("vessel","trip","date"))
      }
      
      # now finalize the haul data manipulations
      h  <-
        h %>% 
        dplyr::rename(
          windforce     = windforcebft,
          catchheight   = catchhoogtevangstincm, 
          boxtype       = boxvolledigofkleinvk, 
          landingweight = marktwaardigeviskg,
          totalcatch    = totalevangstkgberekend, 
          bycatchperc   = percentagebijvangst, 
          dateembarked = dateembarkedutcdate,
          portembarked = portofembarkation,
          datedisembarked = datedisembarkedutcdate,
          portdisembarked = portofdisembarkation,
          cablelength   = cablelengthm, 
          cablethickness= cablethicknessmm,
          lengthgroundrope= lengthgroundropem,
          escapepanel   = escapepanelyn         
        )  %>%  
        
        # remove shootns or shootew if empty position
        mutate(shootns = ifelse(is.na(shootlat), NA, shootns)) %>% 
        mutate(shootew = ifelse(is.na(shootlong), NA, shootew)) %>% 
        
        # fill up empty cells
        mutate(across (c("date","shootlat","shootns", "shootlong", "shootew"),    ~zoo::na.locf(.))) %>% 
        {if(!all(is.na(.$winddirection))) { mutate(., across (c("winddirection"), ~zoo::na.locf(.)))} else {.}} %>%
        {if(!all(is.na(.$windforce)))     { mutate(., across (c("windforce"),     ~zoo::na.locf(.)))} else {.}} %>%
        
        mutate(across (c("shootew", "shootns"), 
                       toupper)) %>% 
        mutate(vessel = toupper(vessel)) %>% 
        mutate(across (c("haul", "meshsize", "windforce", "waterdepth",
                         "catchheight", "dateembarked","datedisembarked"), 
                       as.integer)) %>% 
        mutate(across (c("waterdepth","vertopening", "landingweight", "totalcatch"), 
                       as.numeric)) %>%
        
        # mutate(date   = as.Date(date, origin="1899-12-30" , tz=unique(timezone))) %>% 
        mutate(dateembarked   = as.Date(dateembarked, origin="1899-12-30" , tz=unique(timezone))) %>% 
        mutate(datedisembarked   = as.Date(datedisembarked, origin="1899-12-30" , tz=unique(timezone))) %>% 
        

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
        
        ungroup() %>% 
        dplyr::select(
          vessel, trip, haul, date, shoottime, shoottime2, haultime, nexthaultime,
          lat, lon, 
          winddirection, windforce, waterdepth,
          catchheight, boxtype, landingweight, totalcatch,
          bycatchperc, skipper, dateembarked, portembarked,
          datedisembarked, portdisembarked, gear, meshsize, vertopening,
          cablelength, cablethickness, lengthgroundrope, escapepanel,
          duration, year, quarter, month, yday
        ) 
      
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
        mutate(haul = ifelse(row_number() == 1, 0, nrow(h)+1)) %>% 
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
        file.copy(treklijst_list[i], file.path(tripdir, myvessel), overwrite = TRUE)        
        file.remove(treklijst_list[i])        
      } 
      
      
    } # end of nrow(h) > 0
  
  } # end of treklijst for loop
  
} # end of not empty treklijst_list

# haul <- haul %>% filter(vessel != "SCH135")  

# ----------------------------------------------------------------------------
# read marelec lot data = kisten
# ----------------------------------------------------------------------------

if(!is_empty(kisten_list)){

  # i <- 1
  for (i in 1:length(kisten_list)) {
    
    myvessel <- stringr::word(basename(kisten_list[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(kisten_list[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    mystartrow <-
      readxl::read_excel(kisten_list[i],
                         range="A1:A20",
                         col_names=FALSE, 
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      mutate(rownumber = row_number()) %>% 
      filter(tolower(X) == "lotnummer") %>% 
      dplyr::select(rownumber) %>% 
      as.integer()

    print(paste("kisten", myvessel, mytrip))
    
    h <- haul %>% filter(vessel ==myvessel, trip==mytrip)
    if(!exists("h") & !exists("hh")) stop(paste("Probleem: treklijst niet beschikbaar voor",myvessel, mytrip))
    if(nrow(h)==0 & nrow(hh)==0)     stop(paste("Probleem: treklijst leeg",myvessel, mytrip))

    m  <-
      readxl::read_excel(kisten_list[i],
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
    # t <-
    #   sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
    #                        m.datetime, m.haul2, h.haul from m
    #         join h on m.vessel   == h.vessel and
    #                   m.trip     == h.trip and
    #                   m.datetime >= h.haultime") %>%
    #   as_tibble()
    
    if(nrow(h) >0) {
      
      t <-
        sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
                           m.datetime, m.haul2, h.haul from m
            join h on m.vessel   == h.vessel and
                      m.trip     == h.trip and
                      m.datetime >= h.haultime and
                      m.datetime <  h.nexthaultime") %>%
        as_tibble()
      
    } else {
      
      t <-
        sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
                           m.datetime, m.haul2, hh.haul from m
            join hh on m.vessel   == hh.vessel and
                       m.trip     == hh.trip and
                       m.datetime >= hh.haultime and
                       m.datetime <  hh.nexthaultime") %>%
        as_tibble()
      
    }
    
    # htest <- h %>% filter(haul %in% c(22,27)) %>% dplyr::select(vessel, trip, haul, haultime, nexthaultime)
    # mtest <- m %>% filter(lotnummer==299)
    # ttest <-
    #   sqldf::sqldf("select mtest.vessel, mtest.trip, mtest.lotnummer, mtest.soorten, mtest.maat, mtest.gewicht,
    #                        mtest.datetime, mtest.haul2, h.haul from mtest
    #         join h on mtest.vessel   == h.vessel and
    #                   mtest.trip     == h.trip and
    #                   mtest.datetime >= h.haultime and
    #                   mtest.datetime <  h.nexthaultime")
    
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
      file.copy(kisten_list[i], file.path(tripdir,myvessel), overwrite = TRUE)        
      file.remove(kisten_list[i])        
    } 
    
  } # end of marelec for loop
  
} # end of not empty kisten_list




# ----------------------------------------------------------------------------
# read the pefa elog data
# ----------------------------------------------------------------------------

if(!is_empty(pefa_list)){
  
  i <- 1
  for (i in 1:length(pefa_list)) {

    myvessel <- stringr::word(basename(pefa_list[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(pefa_list[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog pefa", myvessel, mytrip))
    
    # TO DO: SET HAULID TO START AT 1 (haulid-min(haulid))
    
    e  <-
      readxl::read_excel(pefa_list[i], col_names=TRUE, col_types="text",
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
      
      mutate(across (any_of(c("boxes", "meshsize", "haul")),
                     as.integer)) %>%
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "weight", "lat", "lon", "conversionfactor"),
                     as.numeric)) %>%
      # mutate(across(c("catchdate"),
      #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                     ~excel_timezone_to_utc(., timezone="UTC"))) %>% 
      
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

    # add to database
    if(add_data) {
      
      elog <- 
        elog %>%
        filter(paste0(vessel, trip) %notin% paste0(myvessel, mytrip)) %>%
        bind_rows(e)
      
      save(elog,         file = file.path(onedrive, "elog.RData"))  
      
    }
    
    if (move_data) {
      file.copy(pefa_list[i], file.path(tripdir,myvessel), overwrite = TRUE)        
      file.remove(pefa_list[i])        
    } 
    
  } # end of pefa elog for loop

} # end of not empty pefa_list

# ----------------------------------------------------------------------------
# read the pefa elog data per trek
# ----------------------------------------------------------------------------

if(!is_empty(pefa_trek_list)){
  
  i <- 1
  for (i in 1:length(pefa_trek_list)) {
    
    myvessel <- stringr::word(basename(pefa_trek_list[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(pefa_trek_list[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog pefa per trek", myvessel, mytrip))
    
    # TO DO: SET HAULID TO START AT 1 (haulid-min(haulid))
    
    e  <-
      readxl::read_excel(pefa_trek_list[i], col_names=TRUE, col_types="text",
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
      
      mutate(across (any_of(c("boxes", "meshsize", "haul")),
                     as.integer)) %>%
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "weight", "lat", "lon", "conversionfactor"),
                     as.numeric)) %>%
      # mutate(across(c("catchdate"),
      #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
      mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                     ~excel_timezone_to_utc(., timezone="UTC"))) %>% 
      
      mutate(date   = as.Date(catchdate)) %>% 
      
      {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 
      # dplyr::select(-catchdate) %>% 
      
      mutate(
        year       = lubridate::year(date),
        quarter    = lubridate::quarter(date),
        month      = lubridate::month(date),
        week       = lubridate::week(date),
        yday       = lubridate::yday(date)) %>% 
      
      mutate(vessel = myvessel) %>% 
      mutate(trip = mytrip) %>% 
      
      # Keep only the first lat long observation of each haul
      group_by(vessel, trip, haul) %>% 
      mutate(
        lat = first(na.omit(lat)),
        lon = first(na.omit(lon))
      ) %>%
      mutate(across (c("lat","lon"),    ~zoo::na.locf(.))) %>% 
      ungroup()
      

    # add to database
    if(add_data) {
      
      elog_trek <- 
        elog_trek %>%
        filter(paste0(vessel, trip) %notin% paste0(myvessel, mytrip)) %>%
        bind_rows(e)
      
      save(elog_trek,         file = file.path(onedrive, "elog_trek.RData"))  
      
    }
    
    if (move_data) {
      file.copy(pefa_trek_list[i], file.path(tripdir,myvessel), overwrite = TRUE)        
      file.remove(pefa_trek_list[i])        
    } 
    
  } # end of pefa elog for loop
  
} # end of not empty pefa_trek_list

# ----------------------------------------------------------------------------
# read the m-catch elog data
# ----------------------------------------------------------------------------

if(!is_empty(mcatch_list)){
  
  # i <- 1
  for (i in 1:length(mcatch_list)) {

    myvessel <- stringr::word(basename(mcatch_list[i]), 1, sep=" ") %>%
      unlist()     
    mytrip <- stringr::word(basename(mcatch_list[i]), 2, sep=" ") %>%
      gsub("_","",.) %>%
      unlist()     
    
    print(paste("elog mcatch", myvessel, mytrip))
    
    e  <-
      readxl::read_excel(mcatch_list[i], 
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
      
      # mutate(landingdate = as.character(landingdate)) %>% 
      
      mutate(
        year       = lubridate::year(date),
        quarter    = lubridate::quarter(date),
        month      = lubridate::month(date),
        week       = lubridate::week(date),
        yday       = lubridate::yday(date)) %>% 
      
      dplyr::select(-lat, -lon) %>% 
      left_join(rect_df, by="rect") %>% 
      mutate(lon = lon + 0.5) %>% 
      mutate(lat = lat + 0.25) %>% 
      
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
      file.copy(mcatch_list[i], file.path(tripdir,myvessel), overwrite = TRUE)        
      file.remove(mcatch_list[i])        
    } 

    
  } # end of mcatch elog for loop

} # end of not empty mcatch_list

