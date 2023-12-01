# add tripdata functions

# ------------------------------------------------------------------------------
# get_haul_treklijst
# ------------------------------------------------------------------------------

get_haul_treklijst <- function(my_vessel, my_trip2, my_file) {

  # check in trip number
  if (c(readxl::read_excel(my_file, 
                           sheet = "Haul",  
                           col_names=TRUE, 
                           .name_repair =  ~make.names(., unique = TRUE)) %>% 
        filter(row_number()==1) %>% 
        dplyr::pull(trip)) != my_trip2) 
    stop(paste(basename(my_file), ": tripnumber problem in list not equal to tripnumber in filename"))
  
  # number of used rows    
  r <-
    readxl::read_excel(my_file,
                       sheet = "Haul",  
                       col_names=TRUE, 
                       col_types="text",
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
  
  if (r==0) stop(paste(basename(my_file), ": no hauls in treklijst"))
  
  # read the haul data
  h <-
    readxl::read_excel(my_file,
                       sheet = "Haul",  
                       col_names=TRUE, 
                       col_types="text",
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
    mutate(date   = as.Date(as.integer(date), origin="1899-12-30", tz=timezone)) %>% 
    
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

    # add shoottime if missing
    mutate(
      shoottime = ifelse(is.na(shoottime) & !is.na(haultime), 
                        haultime-lubridate::hm("1:20"), 
                        shoottime),
      shoottime = lubridate::as_datetime(shoottime)
    ) %>% 
    
    # add shoottime2 if missing
    mutate(
      shoottime2 = ifelse(is.na(shoottime2) & !is.na(haultime), 
                         haultime-lubridate::hm("0:40"), 
                         shoottime2),
      shoottime2 = lubridate::as_datetime(shoottime2)
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
    mutate(nexthaultime = as_datetime(nexthaultime))  %>% 
    
    dplyr::rename(
      windforce     = windforcebft,
      catchheight   = catchhoogtevangstincm, 
      boxtype       = boxvolledigofkleinvk, 
      landingweight = marktwaardigeviskg,
      catchweight    = totalevangstkgberekend, 
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
    mutate(across (c("waterdepth","vertopening", "landingweight", "catchweight"), 
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
      catchheight, boxtype, landingweight, catchweight,
      bycatchperc, captain=skipper, 
      departuredate=dateembarked, departureport=portembarked,
      arrivaldate=datedisembarked, arrivalport=portdisembarked, 
      gear, meshsize, vertopening,
      cablelength, cablethickness, lengthgroundrope, escapepanel,
      duration, year, quarter, month, week, yday
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
    left_join(., h_rect, by=c("vessel","trip","haul")) %>% 
    mutate(source="treklijst") %>% 
    mutate(file=basename(my_file)) %>% 
    ungroup()
    
} # end of function

# janitor::compare_df_cols(h, haul) %>% filter(h != haul)
# skimr::skim(bind_rows(haul, h))

# ------------------------------------------------------------------------------
# get_haul_kisten (simplified treklijst)
# ------------------------------------------------------------------------------

get_haul_kisten <- function(my_vessel, my_trip, my_file, my_kisten) {
  
  print(paste(".. getting hauls from simplified treklijst & kisten", my_vessel, my_trip))
  
  # get data from kisten
  
  mystartrow <-
    readxl::read_excel(my_kisten,
                       range="A1:A20",
                       col_names=FALSE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    mutate(rownumber = row_number()) %>% 
    filter(tolower(X) == "lotnummer") %>% 
    dplyr::select(rownumber) %>% 
    as.integer()
  
  
  k <-
    readxl::read_excel(my_kisten,
                       skip=mystartrow-1,
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    filter(!grepl("totaal", tolower(soorten))) %>%
    filter(!grepl("^einde", tolower(soorten))) %>%
    tidyr::drop_na(soorten) %>% 
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    
    mutate(datetime = lubridate::dmy_hms(paste(datum, tijd))) %>% 
    arrange(datetime) %>% 
    mutate(lotnummer = row_number()) %>% 
    
    arrange(datetime) %>% 
    mutate(time_diff = as.numeric(datetime - lag(datetime))/60) %>% 
    
    mutate(counter = ifelse(time_diff > 20, 1, 0)) %>% 
    mutate(counter = ifelse(row_number() == 1, 1, counter)) %>% 
    mutate(haul = as.integer(cumsum(counter))) %>% 
    
    # filter(time_diff > 20 | row_number()==1) %>% 
    # mutate(haul = row_number()) %>% 
    mutate(gewicht=as.numeric(gewicht)) %>% 
    
    group_by(vessel, trip, haul, datum) %>% 
    summarise(
      landingweight = sum(gewicht, na.rm=TRUE),
      haultime      = min(datetime) - minutes(5)
    ) %>% 
    
    mutate(shoottime = haultime - lubridate::hm("1:20")) %>% 
    mutate(shoottime2 = shoottime + (haultime - shoottime)/2) %>% 
    rename(date=datum) %>%
    mutate(date=dmy(date)) %>% 
    
    # calculate haul duration: haul_time-shoot_time*24 
    mutate(duration   = as.numeric(as.duration(shoottime %--% haultime))/3600 ) %>% 
    
    # add next haul time  
    group_by(vessel, trip) %>% 
    mutate(nexthaultime = lead(haultime)) %>% 
    mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
    mutate(nexthaultime = as_datetime(nexthaultime)) 
  
  # check in trip number
  if (c(readxl::read_excel(my_file, 
                           sheet = "Haul",  
                           col_names=TRUE, 
                           .name_repair =  ~make.names(., unique = TRUE)) %>% 
        filter(row_number()==1) %>% 
        dplyr::pull(trip)) != my_trip2) 
    stop(paste(basename(my_file), ": tripnumber problem in list not equal to tripnumber in filename"))
  
  # number of used rows    
  r <-
    readxl::read_excel(my_file,
                       sheet = "Haul",  
                       col_names=TRUE, 
                       col_types="text",
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
  
  if (r==0) stop(paste(basename(my_file), ": no hauls in treklijst"))
  
  # read the haul data
  h <-
    k %>% 
    left_join(readxl::read_excel(my_file,
                                 sheet = "Haul",  
                                 col_names=TRUE, 
                                 col_types="text",
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
                mutate(date   = as.Date(as.integer(date), origin="1899-12-30", tz=timezone)) %>% 
                dplyr::select(-haul),
              by=c("vessel","trip","date")) %>% 
    
    dplyr::rename(
      windforce     = windforcebft,
      catchheight   = catchhoogtevangstincm, 
      boxtype       = boxvolledigofkleinvk, 
      # landingweight = marktwaardigeviskg,
      catchweight    = totalevangstkgberekend, 
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
    mutate(across (c("waterdepth","vertopening", "landingweight", "catchweight"), 
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
      catchheight, boxtype, landingweight, catchweight,
      bycatchperc, captain=skipper, 
      departuredate=dateembarked, departureport=portembarked,
      arrivaldate=datedisembarked, arrivalport=portdisembarked, 
      gear, meshsize, vertopening,
      cablelength, cablethickness, lengthgroundrope, escapepanel,
      duration, year, quarter, month, week, yday
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
    left_join(., h_rect, by=c("vessel","trip","haul")) %>% 
    mutate(source="treklijst") %>% 
    mutate(file=basename(my_file)) %>% 
    ungroup()
  
  return(h)
  
  # intersect(names(h), names(haul))
  # setdiff(names(h),names(haul))
  
} # end of function

# ------------------------------------------------------------------------------
# get_haul_kisten_pefa
# ------------------------------------------------------------------------------

get_haul_kisten_pefa <- function(my_vessel, my_trip, my_kisten, my_pefa) {
  
  print(paste(".. getting hauls from kisten & pefa", my_vessel, my_trip))
  
  # get data from kisten
  
  mystartrow <-
    readxl::read_excel(my_kisten,
                       range="A1:A20",
                       col_names=FALSE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    mutate(rownumber = row_number()) %>% 
    filter(tolower(X) == "lotnummer") %>% 
    dplyr::select(rownumber) %>% 
    as.integer()
  
  
  k <-
    readxl::read_excel(my_kisten,
                       skip=mystartrow-1,
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    filter(!grepl("totaal", tolower(soorten))) %>%
    filter(!grepl("^einde", tolower(soorten))) %>%
    tidyr::drop_na(soorten) %>% 
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    
    mutate(datetime = lubridate::dmy_hms(paste(datum, tijd))) %>% 
    arrange(datetime) %>% 
    mutate(lotnummer = row_number()) %>% 
    
    arrange(datetime) %>% 
    mutate(time_diff = as.numeric(datetime - lag(datetime))/60) %>% 
    
    mutate(counter = ifelse(time_diff > 20, 1, 0)) %>% 
    mutate(counter = ifelse(row_number() == 1, 1, counter)) %>% 
    mutate(haul = as.integer(cumsum(counter))) %>% 
    
    # filter(time_diff > 20 | row_number()==1) %>% 
    # mutate(haul = row_number()) %>% 
    mutate(gewicht=as.numeric(gewicht)) %>% 
    
    group_by(vessel, trip, haul, datum) %>% 
    summarise(
      landingweight = sum(gewicht, na.rm=TRUE),
      haultime      = min(datetime) - minutes(5)
    ) %>% 
  
    mutate(shoottime = haultime - lubridate::hm("1:20")) %>% 
    mutate(shoottime2 = shoottime + (haultime - shoottime)/2) %>% 
    rename(date=datum) %>%
    mutate(date=dmy(date)) %>% 
    
    # calculate haul duration: haul_time-shoot_time*24 
    mutate(duration   = as.numeric(as.duration(shoottime %--% haultime))/3600 ) %>% 
    
    # add next haul time  
    group_by(vessel, trip) %>% 
    mutate(nexthaultime = lead(haultime)) %>% 
    mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
    mutate(nexthaultime = as_datetime(nexthaultime)) 
      
  # get data from pefa
  e  <-
    readxl::read_excel(my_pefa, 
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    rename(rect = icesrectangle) %>% 
    rename(lat = latitude) %>% 
    rename(lon = longitude) %>% 
    
    {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
    
    mutate(across (any_of(c("boxes", "meshsize", "haul")),
                   as.integer)) %>%
    mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "weight", "lat", "lon", "conversionfactor"),
                   as.numeric)) %>%
    mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                   ~excel_timezone_to_utc(., timezone="UTC"))) %>% 
    
    mutate(date   = as.Date(catchdate)) %>% 
    
    mutate(
      year       = lubridate::year(date),
      quarter    = lubridate::quarter(date),
      month      = lubridate::month(date),
      week       = lubridate::week(date),
      yday       = lubridate::yday(date)) %>% 
    
    left_join(rect_df, by="rect") %>%
    mutate(
      lat = ifelse(is.na(lat.x), as.numeric(lat.y) + 0.25, as.numeric(lat.x)),
      lon = ifelse(is.na(lon.x), as.numeric(lon.y) + 0.5, as.numeric(lon.x))
    ) %>%
    dplyr::select(-lon.x, -lon.y, -lat.x, -lat.y ) %>% 
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    
    rename(
      portembarked = departureport,
      dateembarked = departuredate,
      portdisembarked = arrivalport,
      datedisembarked = arrivaldate,
      gear            = geartype,
      division        = faozone,
      skipper         = captain
    ) %>% 
    mutate(
      dateembarked = as.Date(dateembarked),
      datedisembarked = as.Date(datedisembarked)
    ) %>% 
    
    # temporary: take rectangle with highest catch of the day
    group_by(vessel, trip, skipper, dateembarked, portembarked, datedisembarked, portdisembarked, date, year, quarter, month, week, yday, rect, lat, lon, economiczone, division, gear, meshsize) %>% 
    summarise(landingweight = sum(weight, na.rm=TRUE)) %>% 
    group_by(vessel, trip, date) %>% 
    arrange(desc(landingweight)) %>% 
    slice_head(n=1) %>% 
    ungroup() %>% 
    dplyr::select(-landingweight)
  
    # distinct(vessel, trip, dateembarked, portembarked, datedisembarked, portdisembarked, date, year, quarter, month, week, yday, rect, lat, lon, economiczone, division, gear, meshsize)
  
  h <-
    k %>% 
    left_join(e, by=c("vessel","trip","date"))
  
  h_fao <- 
    h %>%
    drop_na(lat, lon) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = TRUE) %>% 
    sf::st_join(., fao_sf, join = st_within) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(vessel, trip, haul, F_LEVEL, F_CODE) %>%
    mutate(F_LEVEL = tolower(F_LEVEL)) %>% 
    filter(F_LEVEL != "division") %>% 
    mutate(F_LEVEL = ifelse(F_LEVEL=="major", "area", F_LEVEL)) %>% 
    tidyr::pivot_wider(names_from = F_LEVEL, values_from = F_CODE)

  h <- 
    left_join(h, h_fao,  by=c("vessel","trip","haul")) %>% 
    
    left_join(harbours, by=c("portembarked"="harbourcode")) %>% 
    dplyr::select(-portembarked, -valid_until) %>% 
    rename(portembarked = harbourname) %>% 
    
    left_join(harbours, by=c("portdisembarked"="harbourcode")) %>% 
    dplyr::select(-portdisembarked, -valid_until) %>% 
    rename(portdisembarked = harbourname)  %>% 
    mutate(source="kisten & pefa") %>% 
    ungroup()
  

  return(h)
  
  # intersect(names(h), names(haul))
  # setdiff(names(h),names(haul))
  
} # end of function


# ------------------------------------------------------------------------------
# get_trip_from_haul
# ------------------------------------------------------------------------------

get_trip_from_haul <- function(h, my_vessel, my_trip) {
  
  print(paste(".. getting trip from hauls", my_vessel, my_trip))
  
  tmp <-
    h %>% 
    filter(row_number() == 1) %>% 
    dplyr::select(departuredate, arrivaldate, departureport, arrivalport) %>% 
    t() %>% 
    data.frame() %>%
    setNames("value") %>% 
    rownames_to_column(var="variable") %>% 
    mutate(
      action   = ifelse(grepl("arrival", variable), "arrival", "departure"),
      variable = gsub("arrival|departure","", variable) 
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
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    
    ungroup()
    
  return(t)
  

} # end of function

# ------------------------------------------------------------------------------
# get_kisten
# ------------------------------------------------------------------------------

get_kisten <- function(my_vessel, my_trip2, my_file, h) {
  
  # my_trip <- my_trip2
  
  mystartrow <-
    readxl::read_excel(my_file,
                       range="A1:A20",
                       col_names=FALSE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    mutate(rownumber = row_number()) %>% 
    filter(tolower(X) == "lotnummer") %>% 
    dplyr::select(rownumber) %>% 
    as.integer()
  
  print(paste(".. getting marelec kisten", my_vessel, my_trip2))

  if(nrow(h)==0)     stop(paste("Probleem: treklijst leeg",my_vessel, my_trip2))
  
  m  <-
    readxl::read_excel(my_file,
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
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip2) %>% 
    
    mutate(datetime = lubridate::dmy_hms(paste(datum, tijd))) %>% 
    arrange(datetime) %>% 
    mutate(lotnummer = row_number()) %>% 
    mutate(gewicht = as.numeric(gewicht)) %>% 
    
    # assign haul; could be done with sqldf instead, but time registration is 
    # currently problematic
    arrange(datetime) %>% 
    mutate(time_diff = as.numeric(datetime - lag(datetime))/60) %>% 
    
    # assign haul numbers
    mutate(haul = ifelse(time_diff > 20 | is.na(time_diff), 1, 0)) %>% 
    mutate(haul = cumsum(haul)) %>% 
    dplyr::select(-datum, -tijd) %>% 
    left_join(soorten, by="soorten") 
    
    # # @@@!!!! TEMP
    # filter(lotnummer==241)
  
  tmp <-
    # sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
    #                      m.datetime, m.haul2, h.haul from m
    sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
                         m.datetime, h.haul, h.haultime, h.nexthaultime from m
          join h on m.vessel   == h.vessel and
                    m.trip     == h.trip and
                    m.datetime >= h.haultime and
                    m.datetime <  h.nexthaultime") %>%
    as_tibble()
  
  # # t <- m %>% group_by(haul) %>% summarise(xmin = min(datetime, na.rm=TRUE), xmax=max(datetime, na.rm=TRUE))
  # t <- tmp %>% group_by(haul) %>% summarise(xmin = min(datetime, na.rm=TRUE), xmax=max(datetime, na.rm=TRUE))
  # h %>% 
  #   # filter(nexthaultime-haultime < 24) %>% 
  #   ggplot(aes(x=haultime, y=1)) +
  #   theme_publication() +
  #   geom_point(aes(size=catchweight), shape=1, fill=NA) +
  #   # geom_point(aes(x=nexthaultime, y=1.2), colour="red") +
  #   ggalt::geom_dumbbell(aes(x = shoottime, xend = haultime, y=1, group=1),
  #                        size=3, size_x = 0, size_xend = 0, alpha=0.4) +
  #   # geom_segment(aes(xend=nexthaultime, y=1, yend=1.2)) +
  #   geom_text(aes(label=haul), vjust=1, nudge_y = -0.05)+
  #   geom_point(data=tmp,
  #              aes(x=datetime, y=1.1),
  #              colour="blue") +
  #   # geom_point(data=m,
  #   #            aes(x=datetime, y=1.1),
  #   #            colour="blue") +
  #   geom_segment(data=t, aes(x=xmin, xend=xmin, y=1, yend=1.2), colour="blue", linetype="dashed") +
  #   geom_segment(data=t, aes(x=xmax, xend=xmax, y=1, yend=1.2), colour="red", linetype="dashed") +
  #   geom_text(data=t, aes(x=xmin, y=1.25, label=haul), colour="blue") +
  #   expand_limits(y=0.75)
  
  
  m <- left_join(dplyr::select(m,
                               -haul),
                 dplyr::select(tmp,
                               lotnummer, haul),
                 by="lotnummer")  
    # group_by(haul) %>% 
    # mutate(haul = ifelse(haul == 6 & row_number() < 94, 5, haul))
  
  # x %>% group_by(haul) %>% summarise(n=n())
  
  return(m)
  
} # end of function


# ------------------------------------------------------------------------------
# get_pefa
# ------------------------------------------------------------------------------

get_pefa <- function(my_vessel, my_trip, my_file) {
  
  print(paste(".. getting pefa elog", my_vessel, my_trip))
  
  e  <-
    readxl::read_excel(my_file, 
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    rename(rect = icesrectangle) %>% 

    rename(lat = latitude) %>% 
    rename(lon = longitude) %>% 
    
    {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
    
    mutate(across (any_of(c("boxes", "meshsize", "haul")),
                   as.integer)) %>%
    mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "catchdate", "weight", "lat", "lon", "conversionfactor"),
                   as.numeric)) %>%
    mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                   ~excel_timezone_to_utc(., timezone="UTC"))) %>% 
    
    mutate(date   = as.Date(catchdate)) %>% 
    
    {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 

    mutate(
      year       = lubridate::year(date),
      quarter    = lubridate::quarter(date),
      month      = lubridate::month(date),
      week       = lubridate::week(date),
      yday       = lubridate::yday(date)) %>% 
    
    left_join(rect_df, by="rect") %>%
    mutate(
      lat = ifelse(is.na(lat.x), as.numeric(lat.y) + 0.25, as.numeric(lat.x)),
      lon = ifelse(is.na(lon.x), as.numeric(lon.y) + 0.5, as.numeric(lon.x))
    ) %>%
    dplyr::select(-lon.x, -lon.y, -lat.x, -lat.y ) %>% 
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    mutate(source="pefa") %>% 
    ungroup()
  
  
  return(e)
  
} # end of function

# ------------------------------------------------------------------------------
# get_pefa_trek
# ------------------------------------------------------------------------------

get_pefa_trek <- function(my_vessel, my_trip, my_file) {
  
  print(paste(".. getting pefa elog by haul", my_vessel, my_trip))
  
  et  <-
    readxl::read_excel(my_file, 
                       col_names=TRUE, 
                       col_types="text",
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
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    
    # Keep only the first lat long observation of each haul
    group_by(vessel, trip, haul) %>% 
    mutate(
      lat = first(na.omit(lat)),
      lon = first(na.omit(lon))
    ) %>%
    mutate(across (c("lat","lon"),    ~zoo::na.locf(.))) %>% 
    ungroup()
  
  
  return(et)
  
} # end of function

# ------------------------------------------------------------------------------
# get_haul_from_pefa_trek
# ------------------------------------------------------------------------------

get_haul_from_pefa_trek <- function(my_vessel, my_trip, my_file) {
  
  print(paste(".. getting haul from pefa trek", my_vessel, my_trip))
  
  h  <-
    readxl::read_excel(my_file, 
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    rename(rect = icesrectangle) %>% 
    # rename(vessel = vesselnumber) %>% 
    # mutate(vessel = gsub(" ","", vessel)) %>% 
    # mutate(vessel = ifelse(vessel=="SL09", "SL9","")) %>% 
    
    mutate(vessel = my_vessel) %>% 
    mutate(trip   = my_trip) %>% 
    
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
    
    group_by(vessel, trip, haul) %>% 
    mutate(haultime      = min(catchdate) - minutes(5)) %>% 
    
    mutate(shoottime = haultime - lubridate::hm("1:20")) %>% 
    mutate(shoottime2 = shoottime + (haultime - shoottime)/2) %>% 

    # calculate haul duration: haul_time-shoot_time*24 
    mutate(duration   = as.numeric(as.duration(shoottime %--% haultime))/3600 ) %>% 
    
    # Keep only the first lat long observation of each haul
    group_by(vessel, trip, haul) %>% 
    mutate(
      lat = first(na.omit(lat)),
      lon = first(na.omit(lon))
    ) %>%
    mutate(across (c("lat","lon"),    ~zoo::na.locf(.))) %>% 
    
    rename(
      portembarked = departureport,
      dateembarked = departuredate,
      portdisembarked = arrivalport,
      datedisembarked = arrivaldate,
      gear            = geartype,
      division        = faozone,
      skipper         = captain
    ) %>% 
    mutate(
      dateembarked = as.Date(dateembarked),
      datedisembarked = as.Date(datedisembarked)
    ) %>% 
    
    group_by(vessel, trip, skipper, dateembarked, portembarked, datedisembarked, portdisembarked, haul,
             shoottime, shoottime2, haultime,
             date, year, quarter, month, week, yday, rect, lat, lon, economiczone, division, gear, meshsize) %>% 
    summarise(landingweight = sum(weight, na.rm=TRUE)) %>% 

    # add next haul time  
    group_by(vessel, trip) %>% 
    mutate(nexthaultime = lead(haultime)) %>% 
    mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
    mutate(nexthaultime = as_datetime(nexthaultime)) %>% 
    
    ungroup()

  h_fao <- 
    h %>%
    drop_na(lat, lon) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = TRUE) %>% 
    sf::st_join(., fao_sf, join = st_within) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(vessel, trip, haul, F_LEVEL, F_CODE) %>%
    mutate(F_LEVEL = tolower(F_LEVEL)) %>% 
    filter(F_LEVEL != "division") %>% 
    mutate(F_LEVEL = ifelse(F_LEVEL=="major", "area", F_LEVEL)) %>% 
    tidyr::pivot_wider(names_from = F_LEVEL, values_from = F_CODE)
  
  h <- 
    left_join(h, h_fao,  by=c("vessel","trip","haul")) %>% 
    
    left_join(harbours, by=c("portembarked"="harbourcode")) %>% 
    dplyr::select(-portembarked, -valid_until) %>% 
    rename(portembarked = harbourname) %>% 
    
    left_join(harbours, by=c("portdisembarked"="harbourcode")) %>% 
    dplyr::select(-portdisembarked, -valid_until) %>% 
    rename(portdisembarked = harbourname)  %>% 
    mutate(source="pefa per trek") %>% 
    ungroup() 
  
  # janitor::compare_df_cols(h, haul)
  # haul <- haul %>% janitor::remove_empty(which = "cols")
  # skimr::skim(h)
  
  return(h)
  
} # end of function


# ------------------------------------------------------------------------------
# get_raw_from_pefa_trek
# ------------------------------------------------------------------------------

get_raw_from_pefa_trek <- function(my_file) {
  
  print(paste(".. getting RAW from pefa trek", basename(my_file)))
  
  raw  <-
    readxl::read_excel(my_file, 
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    rename(rect = icesrectangle) %>% 
    
    mutate(vessel = my_vessel) %>% 
    # mutate(trip   = my_trip) %>% 
    rename(trip   = tripidentifier) %>% 
    
    rename(lat = latitude) %>% 
    rename(lon = longitude) %>% 
    
    {if(any(grepl("haulid",names(.)))) {rename(., haul = haulid)} else{.}} %>% 
    
    mutate(across (any_of(c("boxes", "meshsize", "haul")),
                   as.integer)) %>%
    mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate", "weight", "lat", "lon", "conversionfactor"),
                   as.numeric)) %>%
    mutate(across (c("catchdate", "departuredate","arrivaldate", "auctiondate"), 
                   ~excel_timezone_to_utc(., timezone="UTC"))) %>% 
    
    
    # Keep only the first lat long observation of each haul
    group_by(vessel, trip, haul) %>% 
    mutate(
      lat = first(na.omit(lat)),
      lon = first(na.omit(lon))
    ) %>%
    mutate(across (c("lat","lon"),    ~zoo::na.locf(.))) %>% 

    # Keep only the first date of each haul
    mutate(date   = as.Date(catchdate)) %>% 
    mutate(date   = first(na.omit(date))) %>%
    mutate(across (c("date"),    ~zoo::na.locf(.))) %>% 
    
    # start hauls at 1
    group_by(vessel, trip) %>% 
    {if(any(grepl("haul",names(.)))) {mutate(., haul = haul - min(haul, na.rm=TRUE)+1)} else{.}} %>% 
    
    mutate(
      year       = lubridate::year(date),
      quarter    = lubridate::quarter(date),
      month      = lubridate::month(date),
      week       = lubridate::week(date),
      yday       = lubridate::yday(date)) %>% 
    
    ungroup()
  
  # raw %>% group_by(catchdate) %>% summarise(n=n()) %>% View()
  # raw %>% filter(catchdate == lag(catchdate) & weight == lag(weight)) %>% View()
  # raw %>% filter(species=="MAC") %>% View()
  

  return(raw)
  
} # end of function


# ------------------------------------------------------------------------------
# get_haul_from_raw
# ------------------------------------------------------------------------------

get_haul_from_raw <- function(raw) {
  
  print(paste(".. getting haul from raw"))
  
  h  <-

    raw %>% 
    
    group_by(vessel, trip, haul) %>% 
    mutate(haultime      = min(catchdate) - minutes(5)) %>% 
    
    mutate(shoottime = haultime - lubridate::hm("1:20")) %>% 
    mutate(shoottime2 = shoottime + (haultime - shoottime)/2) %>% 
    
    # calculate haul duration: haul_time-shoot_time*24 
    mutate(duration   = as.numeric(as.duration(shoottime %--% haultime))/3600 ) %>% 
    
    rename(
      portembarked = departureport,
      dateembarked = departuredate,
      portdisembarked = arrivalport,
      datedisembarked = arrivaldate,
      gear            = geartype,
      division        = faozone,
      skipper         = captain
    ) %>% 
    mutate(
      dateembarked = as.Date(dateembarked),
      datedisembarked = as.Date(datedisembarked)
    ) %>% 
    
    ungroup() %>% 
    
    left_join(harbours, by=c("portembarked"="harbourcode")) %>% 
    dplyr::select(-portembarked, -valid_until) %>% 
    rename(portembarked = harbourname) %>% 
    
    left_join(harbours, by=c("portdisembarked"="harbourcode")) %>% 
    dplyr::select(-portdisembarked, -valid_until) %>% 
    rename(portdisembarked = harbourname)  %>% 

    # convert to sf  and join fao and rect data
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = FALSE) %>% 
    sf::st_join(., fao_sf_area, join = st_within) %>% 
    sf::st_join(., fao_sf_subarea, join = st_within) %>% 
    sf::st_join(., fao_sf_division, join = st_within) %>% 
    sf::st_join(., rect_sf2, join = st_within) %>% 
    sf::st_join(., eez_sf, join = st_within) %>% 
    sf::st_drop_geometry() %>%
    
    rename(rect=rect.x, rect_calc=rect.y) %>% 
    rename(division=division.x, division_calc=division.y) %>% 
    rename(economiczone=economiczone.x, economiczone_calc=economiczone.y) %>% 
    
    group_by(vessel, trip) %>% 
    mutate(across (c("rect","division","economiczone"),    ~zoo::na.locf(.))) %>% 

    group_by(vessel, trip, skipper, dateembarked, portembarked, datedisembarked, portdisembarked, haul,
             shoottime, shoottime2, haultime,
             date, year, quarter, month, week, yday, lat, lon, rect, rect_calc, division, division_calc, economiczone, economiczone_calc, gear, meshsize) %>%
    summarise(landingweight = sum(weight, na.rm=TRUE)) %>%
    
    # add next haul time  
    group_by(vessel, trip) %>% 
    mutate(nexthaultime = lead(haultime)) %>% 
    mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
    mutate(nexthaultime = as_datetime(nexthaultime)) %>% 
    
    ungroup()
  
  # janitor::compare_df_cols(h, haul)
  # haul <- haul %>% janitor::remove_empty(which = "cols")
  # skimr::skim(h)
  
  return(h)
  
} # end of function

# ------------------------------------------------------------------------------
# get_trip_from_h
# ------------------------------------------------------------------------------

get_trip_from_h <- function(h) {
  
  print(paste(".. getting trip from haul (h)"))
  
  tmp  <-
    
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
    mutate(vessel = unique(h$vessel)) %>% 
    mutate(trip = unique(h$trip)) %>% 
    bind_rows(dplyr::select(h,
                            vessel, trip, haul, date, lat, lon)) %>% 
    arrange(haul) %>% 
    
    # calculate distance between shoot and haul positions
    mutate(distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat)))/1852 ) %>% 
    
    # add distance within haul if zero
    mutate(distance = ifelse(distance == 0, 4.0, distance)) %>% 
    
    ungroup()
  
  return(t)
  
} # end of function

# ------------------------------------------------------------------------------
# get_kisten_from_raw
# ------------------------------------------------------------------------------

get_kisten_from_raw <- function(raw) {
  
  print(paste(".. getting kisten from raw"))
  
  m  <-
    
    raw %>% 
    
    # mutate(maat = gsub("KLASSE ","", maat)) %>% 
    rename(datetime = catchdate) %>% 
    rename(gewicht  = weight) %>% 
    rename(soorten  = species) %>% 
    dplyr::select(vessel, trip, haul, datetime, soorten, gewicht) %>% 
    ungroup()
  
  # janitor::compare_df_cols(kisten, raw)
  # haul <- haul %>% janitor::remove_empty(which = "cols")
  # skimr::skim(h)
  
  return(m)
  
} # end of function

# ------------------------------------------------------------------------------
# get_elog_from_raw
# ------------------------------------------------------------------------------

get_elog_from_raw <- function(raw) {
  
  print(paste(".. getting elog from raw"))
  
  e  <-
    raw %>% 
    ungroup() %>% 
    mutate(weight = weight * conversionfactor) %>% 
    
    # group_by(vessel, trip, haul, date, species, economiczone, rect, division=faozone, lon, lat, geartype, meshsize, departuredate, departureport, arrivaldate, arrivalport,
    #          auctiondate, auctionport, captain, tripstatus, year, quarter, month, week, yday) %>%
    group_by(vessel, trip, date, species, economiczone, rect, division=faozone, geartype, meshsize, departuredate, departureport, arrivaldate, arrivalport,
             auctiondate, auctionport, captain, tripstatus, year, quarter, month, week, yday) %>%
    summarise(
      weight = sum(weight, na.rm=TRUE),
      lat    = mean(lat, na.rm=TRUE),
      lon   = mean(lon, na.rm=TRUE),
      fishingoperations = n_distinct(haul)
    ) %>% 
    
    group_by(vessel, trip) %>% 
    mutate(across (c("rect","division","economiczone"),    ~zoo::na.locf(.))) %>% 
    
    mutate(source="pefa_per_trek") %>% 
    ungroup()
  
  # janitor::compare_df_cols(raw, elog, kisten)
  # haul <- haul %>% janitor::remove_empty(which = "cols")
  # skimr::skim(h)

  
  return(m)
  
} # end of function
