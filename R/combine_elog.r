
# combine elog files

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../prf/r/my utils.R")

get_onedrive <- function (team="Martin Pastoors", site="FLYSHOOT - General/rdata") {
  
  if (Sys.info()['sysname'] == 'Windows') {
    
    # set onedrive directory
    if(dir.exists(file.path(Sys.getenv('USERPROFILE'), team, site))) {
      onedrive <- file.path(Sys.getenv('USERPROFILE'), team, site)   
    } else if(dir.exists(file.path('C:/DATA/PFA', team, site))) {
      onedrive <- file.path('C:/DATA/PFA', team, site)
    } else if(dir.exists(file.path('D:/DATA/PFA', team, site))) {
      onedrive <- file.path('D:/DATA/PFA', team, site)
    } else {
      stop("Onedrive directory not found")
    }
  }
  
  return(onedrive)
}

onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")

elog <- loadRData(file.path(onedrive, "elog 20230310.RData"))

ecatch33 <-
  loadRData(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch33.RData") %>% 
  mutate(source="ecatch33") %>% 
  mutate(vessel = gsub("-","",XR)) %>% 
  filter(date < min(filter(elog, source %in% c("m-catch","pefa"))$date, na.rm=TRUE))

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

comb %>% 
  ggplot(aes(x=date, y=vessel)) +
  theme_publication() +
  geom_jitter(aes(colour=source), shape=1, width=0.1, height=0.2) +
  facet_wrap(~year, scale="free_x")

comb %>% 
  drop_na(DA) %>% 
  ggplot(aes(x=date, y=DA)) +
  theme_publication() +
  geom_point(aes(colour=source)) 

comb %>% 
  filter(vessel=="SL9") %>% 
  ggplot(aes(x=date, y=source)) +
  theme_publication() +
  geom_jitter(aes(colour=source), shape=1, width=0.0, height=0.2) +
  facet_wrap(~year, scale="free_x")

elog <- comb

# save(elog, file=file.path(onedrive, "elog 20230216.RData"))
save(elog, file=file.path(onedrive, "elog.RData"))
