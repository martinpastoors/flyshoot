
# combine elog files

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

load(file.path(onedrive, "elog.RData"))

ecatch33 <-
  loadRData(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch33.RData") %>% 
  mutate(source="ecatch33") %>% 
  mutate(vessel = gsub("-","",XR)) %>% 
  filter(date < min(elog$date, na.rm=TRUE))
  
ecatch20 <-
  loadRData(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch20.RData") %>% 
  mutate(source="ecatch20") %>% 
  mutate(vessel = gsub("-","",XR)) %>% 
  filter(date < min(ecatch33$date, na.rm=TRUE))

olrac <-
  loadRData(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/olrac.RData") %>% 
  mutate(source="olrac") %>% 
  mutate(vessel = gsub("-","",XR)) %>% 
  filter(date < min(ecatch20$date, na.rm=TRUE))


comb <-
  bind_rows(
    olrac ,
    ecatch20, 
    ecatch33,  
    elog
  )

comb %>% 
  ggplot(aes(x=date, y=vessel)) +
  theme_publication() +
  geom_jitter(aes(colour=source), shape=1, width=0.1, height=0.2) +
  facet_wrap(~year, scale="free_x")

comb %>% 
  filter(vessel=="SL9") %>% 
  ggplot(aes(x=date, y=source)) +
  theme_publication() +
  geom_jitter(aes(colour=source), shape=1, width=0.0, height=0.2) +
  facet_wrap(~year, scale="free_x")

elog <- comb

# save(elog, file=file.path(onedrive, "elog 20230216.RData"))
save(elog, file=file.path(onedrive, "elog.RData"))
