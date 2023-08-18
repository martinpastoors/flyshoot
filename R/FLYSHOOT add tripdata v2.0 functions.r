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
      departuredate = dateembarkedutcdate,
      departureport = portofembarkation,
      arrivaldate = datedisembarkedutcdate,
      arrivalport = portofdisembarkation,
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
                     "catchheight", "departuredate","arrivaldate"), 
                   as.integer)) %>% 
    mutate(across (c("waterdepth","vertopening", "landingweight", "catchweight"), 
                   as.numeric)) %>%
    
    # mutate(date   = as.Date(date, origin="1899-12-30" , tz=unique(timezone))) %>% 
    mutate(departuredate   = as.Date(departuredate, origin="1899-12-30" , tz=unique(timezone))) %>% 
    mutate(arrivaldate   = as.Date(arrivaldate, origin="1899-12-30" , tz=unique(timezone))) %>% 
    
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
      departuredate, departureport,
      arrivaldate, arrivalport, 
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
    mutate(source="marelec")
  
  # tmp <-
  #   sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht,
  #                        m.datetime, m.haul2, h.haul from m
  #         join h on m.vessel   == h.vessel and
  #                   m.trip     == h.trip and
  #                   m.datetime >= h.haultime and
  #                   m.datetime <  h.nexthaultime") %>%
  #   as_tibble()
    
  # m <- left_join(m, 
  #                dplyr::select(tmp,
  #                              lotnummer, haul),
  #                by="lotnummer")      
  
  
  return(m)
  
} # end of function


# ------------------------------------------------------------------------------
# get_pefa
# ------------------------------------------------------------------------------

get_pefa <- function(my_file) {
  
  print(paste(".. getting pefa elog", basename(my_file)))
  
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
    
    rename(trip   = tripidentifier) %>% 
    rename(vessel = vesselnumber) %>%
    mutate(vessel = toupper(vessel)) %>% 
    mutate(vessel = ifelse(vessel == "SL 09","SL9",vessel)) %>% 
    
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

    ungroup() %>% 
    
    left_join(harbours, by=c("departureport"="harbourcode")) %>% 
    dplyr::select(-departureport, -valid_until) %>% 
    rename(departureport = harbourname) %>% 
    
    left_join(harbours, by=c("arrivalport"="harbourcode")) %>% 
    dplyr::select(-arrivalport, -valid_until) %>% 
    rename(arrivalport = harbourname)  %>% 
    
    mutate(source="pefa") %>% 
    ungroup()
  
  
  return(e)
  
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
    rename(lat = latitude) %>% 
    rename(lon = longitude) %>% 
    
    rename(trip   = tripidentifier) %>% 
    rename(vessel = vesselnumber) %>%
    mutate(vessel = toupper(vessel)) %>% 
    mutate(vessel = ifelse(vessel == "SL 09","SL9",vessel)) %>% 
    
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
      gear            = geartype,
      division        = faozone,
      skipper         = captain
    ) %>% 

    ungroup() %>% 
    
    left_join(harbours, by=c("departureport"="harbourcode")) %>% 
    dplyr::select(-departureport, -valid_until) %>% 
    rename(departureport = harbourname) %>% 
    
    left_join(harbours, by=c("arrivalport"="harbourcode")) %>% 
    dplyr::select(-arrivalport, -valid_until) %>% 
    rename(arrivalport = harbourname)  %>% 

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

    group_by(vessel, trip, skipper, departuredate, departureport, arrivaldate, arrivalport, haul,
             shoottime, shoottime2, haultime,
             date, year, quarter, month, week, yday, lat, lon, rect, rect_calc, division, division_calc, economiczone, economiczone_calc, gear, meshsize) %>%
    summarise(landingweight = sum(weight, na.rm=TRUE)) %>%
    
    # add next haul time  
    group_by(vessel, trip) %>% 
    mutate(nexthaultime = lead(haultime)) %>% 
    mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
    mutate(nexthaultime = as_datetime(nexthaultime)) %>% 
    mutate(source="pefa trek") %>% 
    
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
    mutate(vessel = unique(h$vessel)) %>% 
    mutate(trip = unique(h$trip)) %>% 
    mutate(source = unique(h$source[!is.na(h$source)])) %>% 
    bind_rows(dplyr::select(h,
                            vessel, trip, haul, date, lat, lon, source)) %>% 
    arrange(haul) %>% 
    
    # calculate distance between shoot and haul positions
    mutate(distance = geosphere::distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat)))/1852 ) %>% 
    
    # add distance within haul if zero
    mutate(distance = ifelse(distance == 0, 4.0, distance)) %>% 
    
    # mutate(source="pefa trek") %>% 
    
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
    dplyr::select(vessel, trip, haul, datetime, species=soorten, gewicht) %>% 
    mutate(source="pefa trek") %>% 
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
    
    mutate(source="pefa trek") %>% 
    ungroup()
  
  # janitor::compare_df_cols(raw, elog, kisten)
  # haul <- haul %>% janitor::remove_empty(which = "cols")
  # skimr::skim(h)

  
  return(m)
  
} # end of function

# ------------------------------------------------------------------------------
# get_tripnumber
# ------------------------------------------------------------------------------

get_tripnumber <- function(my_file) {
  
  print(paste(".. getting pefa tripnumber", basename(my_file)))
  
  my_trip   <- stringr::word(basename(my_elog), 2, sep=" ") %>% gsub("_","",.) %>% unlist()     
  
  e  <-
    readxl::read_excel(my_elog, 
                       col_names=TRUE, 
                       col_types="text",
                       .name_repair =  ~make.names(., unique = TRUE))  %>% 
    data.frame() %>% 
    lowcase() %>% 
    dplyr::distinct(tripidentifier) %>% 
    pull(tripidentifier)
  
  
  return(e)
  
} # end of function
