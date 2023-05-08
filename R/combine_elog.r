# ==============================================================================
# combine elog files
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

spatialdir <- "C:/DATA/RDATA"

# spatial data files

load(file.path(spatialdir, "world_lr_sf.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))

load(file.path(spatialdir, "world_lr_df.RData"))
load(file.path(spatialdir, "world_mr_df.RData"))

rect_df <-
  loadRData(file.path(spatialdir, "rect_df.RData")) %>% 
  rename(rect=ICESNAME) %>% 
  group_by(rect) %>% 
  filter(row_number() ==1) %>% 
  dplyr::select(rect, lon=long, lat)


onedrive  <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/data")
onedrive2 <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")
tripdir   <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/tripdata")

# olrac
olrac <- loadRData(file=file.path(onedrive, "elog olrac.RData")) %>% 
  lowcase() %>% 
  mutate(year = year(date) ) %>% 
  rename(meshsize          = me,
         fishingoperations = fo,
         species           = sn,
         weight            = wt,
         faozone           = fa,
         rect              = sr, 
         economiczone      = ez,
         geartype          = ge,
         freshness         = fe,
         remarks           = re,
         vessel            = xr) %>%
  mutate(catchdate = lubridate::ymd_hms(paste0(rd,"T",rt,":00"))) %>% 
  mutate(across(c("meshsize", "fishingoperations"), as.integer)) %>% 
  mutate(vessel = gsub("-","", toupper(vessel))) %>% 
  mutate(faozone = tolower(faozone)) %>% 
  mutate(source="olrac") %>% 
  dplyr::select(-cvd, -cvo, -da, -du, -fd, -gc, -kv, -ma, -md, -na, -nad, -nfr, -nh, -nlersid, -nlspn,
                -rd, -rn, -rnnew, -rv, -ti, -tv, -rt)

# ecatch
ecatch <- loadRData(file=file.path(onedrive, "elog ecatch.RData")) %>% 
  mutate(date = lubridate::as_date(catchdate)) %>% 
  rename(geartype = gear)

# mcatch
mcatch <- loadRData(file=file.path(onedrive, "elog mcatch.RData")) 

# pefa
pefa <- loadRData(file=file.path(onedrive, "elog pefa.RData")) %>% 
  mutate(landingdate = excel_timezone_to_utc(as.numeric(landingdate), timezone="UTC")) 

janitor::compare_df_cols(olrac, ecatch, mcatch, pefa)

# combine from new to old
elog <-
  pefa %>% 
  bind_rows(mcatch %>% filter(paste(vessel, date) %notin% paste(pefa$vessel, pefa$date))) %>% 
  bind_rows(ecatch %>% filter(paste(vessel, date) %notin% paste(mcatch$vessel, mcatch$date))) %>% 
  bind_rows(olrac  %>% filter(paste(vessel, date) %notin% paste(ecatch$vessel, ecatch$date))) %>% 
  mutate(
    lat = ifelse(lat==0 & lon == 0, NA, lat),
    lat = ifelse((lat-floor(lat)) %in% c(0, 0.25, 0.5, 0.75), NA, lat),
    lon = ifelse(lon==0 & is.na(lat), NA, lon),
    lon = ifelse((lon-floor(lon)) %in% c(0, 0.5), NA, lon)
  ) %>% 
  rename(lat2 = lat, lon2 = lon) %>% 
  left_join(rect_df, by="rect") %>% 
  
  mutate(quarter = lubridate::quarter(date)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  mutate(week = lubridate::week(date)) %>% 
  mutate(yday = lubridate::yday(date)) 

skimr::skim(elog)
# inspectdf::inspect_num(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(mcatch) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(mcatch) %>% inspectdf::show_plot()

save(elog, file=file.path(onedrive2, "elog.RData"))
# load(file=file.path(onedrive2, "elog.RData"))

# fishing days per week
elog %>% 
  mutate(catchdate = as_date(catchdate)) %>% 
  group_by(vessel, year, source, week) %>% 
  summarise(ndays = n_distinct(catchdate)) %>% 
  group_by(vessel, year, source) %>%
  summarise(
    ndays = mean(ndays),
    nweeks = n_distinct(week)) %>%
  ggplot(aes(x=year, y=ndays)) +
  theme_publication() +
  geom_point(aes(colour=source, size=nweeks)) +
  facet_wrap(~vessel)

  
# plot by vessel, source and year
xlim <- range(elog$lon, na.rm=TRUE)
ylim <- range(elog$lat, na.rm=TRUE)  
elog %>% 
  group_by(vessel, year, source, rect, lat, lon) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap(xlim=xlim , ylim=ylim) +
  
  geom_polygon(data=world_mr_df, aes(x=long, y=lat, group=group), fill = "grey75") +
  geom_point(aes(colour=source, size=weight), alpha=0.5) +
  facet_grid(vessel~year)


