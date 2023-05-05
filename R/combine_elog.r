# ==============================================================================
# combine elog files
#
# 02/05/2023 Half way in redoing the elog data; need to check OLRAC, Ecatch33 and Ecatch20
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

elog <- loadRData(file.path(onedrive2, "elog.RData"))

# mcatch
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

save(mcatch, file=file.path(onedrive, "elog mcatch.RData"))
load(file=file.path(onedrive, "elog mcatch.RData"))

# skimr::skim(mcatch)
# inspectdf::inspect_num(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(mcatch) %>% inspectdf::show_plot()

# mcatch %>% ggplot(aes(x=lon, y=lat, colour=as.character(year))) + geom_point()
# mcatch %>% filter(weight > 1000) %>% arrange(desc(weight)) %>% View()

# ecatch33
ecatch33 <-
  loadRData(file=file.path(onedrive, "ecatch33.RData")) %>% 
  lowcase() %>% 
  rename(meshsize          = me,
         fishingoperations = fo,
         species           = sn,
         weight            = wt,
         faozone           = fa,
         gear              = ge) %>%
  mutate(faozone = tolower(faozone)) %>% 
  mutate(source="ecatch33") %>% 
  mutate(vessel = gsub("-","",toupper(xr))) %>% 
  dplyr::select(-c(swe,
                   vrs,
                   nad, 
                   nfr,
                   rn,
                   xmlns,
                   tn, 
                   na,
                   xr,
                   ma,
                   gc,
                   et,
                   kv,
                   mv,
                   rnnew,
                   re,
                   dd,
                   vt,
                   nlersid,
                   ))

  filter(date < min(filter(elog, source %in% c("m-catch","pefa"))$date, na.rm=TRUE))

skimr::skim(ecatch33)
inspectdf::inspect_num(mcatch) %>% inspectdf::show_plot()
inspectdf::inspect_imb(mcatch) %>% inspectdf::show_plot()
inspectdf::inspect_cat(mcatch) %>% inspectdf::show_plot()

myvar="arrivaldate"; mcatch %>% distinct(get(myvar)) %>% arrange() %>% View()
mcatch %>% filter(is.na(departuredate)) %>% View()
mcatch %>% distinct(vessel, tripid, departuredate, arrivaldate) %>% 
  group_by(vessel, tripid) %>% 
  summarise(n=sum(is.na(departuredate))) %>% 
  filter(n > 0) %>% 
  View()


# allocate trips at a later stage for combined elog
mcatch_trips <-
  mcatch %>% 
  distinct(vessel, year, departuredate) %>%
  group_by(vessel, year) %>%
  mutate(trip = paste0(year, stringr::str_pad(row_number(), width=3, pad="0")))

myvar="trip"; mcatch_trips %>% distinct(get(myvar)) %>% arrange() %>% View()

  

ecatch20 <-
  loadRData(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch20.RData") %>% 
  mutate(source="ecatch20") %>% 
  mutate(vessel = gsub("-","",XR)) %>% 
  filter(date < min(filter(elog, source %in% c("m-catch","pefa", "ecatch33"))$date, na.rm=TRUE))

olrac <-
  loadRData(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/olrac.RData") %>% 
  mutate(source="olrac") %>% 
  mutate(vessel = gsub("-","",XR)) %>% 
  filter(date < min(filter(elog, source %in% c("m-catch","pefa", "ecatch33", "ecatch20"))$date, na.rm=TRUE))


# ==========================================================================================================
# pefa
# ==========================================================================================================

filelist <- list.files(
  path=file.path(tripdir, "m-catch"),
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
        rename(rect = icesrectangle) %>%
        mutate(across (c("meshsize"), as.integer)) %>%
        mutate(across (c("weight",
                         "catchdate", "departuredate", "arrivaldate"), as.numeric)) %>%
        mutate(across(c("catchdate", "departuredate", "arrivaldate"), ~excel_timezone_to_utc(.,"UTC"))) %>% 
        mutate(month   = lubridate::month(catchdate),
               quarter = lubridate::quarter(catchdate),
               week    = lubridate::week(catchdate),
               yday    = lubridate::yday(catchdate),
               year    = lubridate::year(catchdate),
               date    = as.Date(catchdate)) %>%
        mutate(faozone = tolower(faozone)) %>% 
        mutate(file            = basename(filelist[i])) %>%
        mutate(vessel          = stringr::word(file, 3)) %>%
        mutate(source          = "pefa") %>% 
        relocate(vessel) 
        # relocate(remarks, .after = date)
    )
}

pefa <-
  pefa %>% 
  dplyr::select(-c(boxes,
                   weightundersized,
                   boxesundersized,
                   longitude,
                   latitide,
                   latitude,
                   auctiondate,
                   auctionport,
                   tripstatus))
  
save(pefa, file=file.path(onedrive, "elog pefa.RData"))

skimr::skim(pefa)

# ==============================================================================
# combine datasets
# ==============================================================================

comb <-
  bind_rows(
    olrac ,
    ecatch20, 
    ecatch33,  
    filter(elog,
           source %notin% c("ecatch33", "ecatch20", "olrac"))
  ) %>% 
  
  # remove empty columns
  dplyr::select_if(function(x) !(all(is.na(x)))) %>% 
  mutate(rect = ifelse(is.na(rect) & !is.na(SR), SR, rect)) %>% dplyr::select(-SR)  %>%
  mutate(species = ifelse(is.na(species) & !is.na(SN), SN, species))  %>% dplyr::select(-SN)  %>%
  mutate(faozone = ifelse(is.na(faozone) & !is.na(FA), FA, faozone))  %>% dplyr::select(-FA)  %>%
  mutate(economiczone = ifelse(is.na(economiczone) & !is.na(EZ), EZ, economiczone)) %>% dplyr::select(-EZ) %>%
  mutate(freshness = ifelse(is.na(freshness) & !is.na(FE), FE, freshness)) %>% dplyr::select(-FE)  %>% 
  mutate(geartype = ifelse(is.na(geartype) & !is.na(GE), GE, geartype))  %>% dplyr::select(-GE)  %>%
  mutate(meshsize = ifelse(is.na(meshsize) & !is.na(ME), ME, meshsize)) %>% dplyr::select(-ME)  %>% 
  mutate(weight = ifelse(is.na(weight) & !is.na(WT), WT, weight)) %>% dplyr::select(-WT)  %>% 
  rename(fishingoperations = FO) %>% 
  rename(gearcode = GC) %>% 
  mutate(DA = lubridate::ymd(DA)) %>% 
  mutate(date = ifelse(!is.na(date) & !is.na(DA), DA, date)) %>% 
  mutate(date = as.Date(date)) %>% 
  
  mutate(across(c(fishingoperations, meshsize), as.integer)) %>% 
  mutate(across(c(lat2, lon2), as.numeric)) %>% 
  
  mutate(catchdate = lubridate::ymd_hms(catchdate)) %>% 
  mutate(landingdate = lubridate::ymd_hms(landingdate)) %>% 
  
  mutate(quarter = lubridate::quarter(date)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(week = lubridate::week(date)) %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  dplyr::select(c(vessel,
                  trip, 
                  tripidentifier,
                  year,
                  date, 
                  datetime,
                  faozone,
                  economiczone,
                  rect,
                  fishingoperations,
                  species, 
                  presentation,
                  preservation, 
                  conversionfactor, 
                  boxes, 
                  weight, 
                  geartype,
                  gearcode,
                  meshsize, 
                  captain, 
                  catchdate,
                  departuredate,
                  departureport,
                  arrivaldate,
                  arrivalport,
                  landingdate,
                  landingport,
                  auctionport,
                  quarter,
                  month, 
                  week, 
                  yday,
                  lat2,
                  lon2,
                  haul,
                  source,
                  file))
  
  
skimr::skim(comb)

# comb %>% filter(!is.na(DA)) %>%  mutate(diff = date-DA) %>% View()
# comb %>% filter(is.na(trip)) %>%  View()
# comb %>% filter(is.na(species)) %>% distinct(vessel, year, source) %>% View()
# comb %>% filter(tripidentifier == "2023021902219") %>% View()
# comb %>% filter(is.na(tripidentifier) & !is.na(departuredate)) %>%  View()
# comb %>% filter(is.na(tripidentifier)) %>%  distinct(filename)
# comb %>% filter(is.na(tripidentifier)) %>%  distinct(RN, date, trip) %>% View()
# comb %>% filter(is.na(boxes)) %>%  View()
# comb %>% filter(!is.na(GC)) %>%  View()
# comb %>% group_by(tripidentifier) %>%  summarise(n=n_distinct(date)) %>% View()

# inspectdf::inspect_num(comb) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(comb) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(comb) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(filter(comb, !is.na(RD))) %>% inspectdf::show_plot()

# comb %>% 
#   ggplot(aes(x=date, y=vessel)) +
#   theme_publication() +
#   geom_jitter(aes(colour=source), shape=1, width=0.1, height=0.2) +
#   facet_wrap(~year, scale="free_x")

# comb %>% 
#   drop_na(DA) %>% 
#   ggplot(aes(x=date, y=DA)) +
#   theme_publication() +
#   geom_point(aes(colour=source)) 

# comb %>% 
#   filter(vessel=="SL9") %>% 
#   ggplot(aes(x=date, y=source)) +
#   theme_publication() +
#   geom_jitter(aes(colour=source), shape=1, width=0.0, height=0.2) +
#   facet_wrap(~year, scale="free_x")

# elog <- comb

# save(elog, file=file.path(onedrive, "elog 20230216.RData"))
# save(elog, file=file.path(onedrive, "elog.RData"))
