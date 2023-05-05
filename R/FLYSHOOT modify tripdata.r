# =======================================================================================
# FLYSHOOT: modify tripdata.r
# 
# do some manipulations on the datasets
#
# Martin Pastoors
#
# 25/01/2023 checks on marelec data

# TO DO: 
# 
# =========================================================================================


options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

# Libraries
library(rmarkdown)                   # note: requires knitr 1.21

require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
require(lubridate, quietly=TRUE)     # data handling
require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
require(RColorBrewer)                # colour schemes
library(viridis)
library(pander)

library(captioner)                   # for captions
tab_nums <- captioner::captioner(prefix = "Table " , levels=1, type=c("n"), infix=".")
fig_nums <- captioner::captioner(prefix = "Figure ", levels=1, type=c("n"), infix=".")

# Source all the utils
source("../prf/R/my utils.r")
source("../mptools/R/get_onedrive.r")

spatialdir <- "C:/DATA/RDATA"

# spatial data files

load(file.path(spatialdir, "world_lr_sf.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))
# load(file.path(onedrive, "eez.df.RData"))
# load(file.path(onedrive, "fao.df.RData"))
# load(file.path(onedrive, "depth200.df.RData"))
# load(file.path(onedrive, "icesrectangles.df.RData"))

load(file.path(spatialdir, "world_lr_df.RData"))
load(file.path(spatialdir, "world_mr_df.RData"))

rect_df <-
  loadRData(file.path(spatialdir, "rect_df.RData")) %>% 
  rename(rect=ICESNAME) %>% 
  group_by(rect) %>% 
  filter(row_number() ==1) %>% 
  dplyr::select(rect, lon=long, lat)

load(file.path(spatialdir, "asfis.RData"))

# set onedrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")

# load datasets
load(file.path(onedrive, "haul.RData"))
load(file.path(onedrive, "kisten.RData"))
load(file.path(onedrive, "elog.RData"))
load(file.path(onedrive, "trip.RData"))

# --------------------------------------------------------------------------------
# removing erroneous trips
# --------------------------------------------------------------------------------

# trip %>% filter(vessel=="SCH153") %>% group_by(vessel, trip) %>% summarise(n=n())
# trip <- trip %>% filter(vessel!="SCH153") 
# save(trip,  file = file.path(onedrive,  "trip.RData"))

haul %>% filter(vessel=="Z99") %>% group_by(vessel, trip) %>% summarise(n=n())

haul <- haul %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023420", "2023421",trip)) # %>% View()
trip <- trip %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023320", "2023421",trip))

haul <- haul %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip))
trip <- trip %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip))
kisten <- kisten %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip))
elog <- elog %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip)) 

elog <-
  elog %>% 
  filter(!(vessel=="SL9" & year == 2018 &  week == 52 & source=="m-catch")) %>% 
  filter(!(vessel=="SCH135" & year == 2019 &  week == 1 & source=="m-catch")) %>% 
  filter(!(vessel=="SL65" & year == 2018 &  week == 5 & source=="m-catch")) 
  

# save(haul,   file = file.path(onedrive, "haul.RData"))
# save(trip,   file = file.path(onedrive, "trip.RData"))
# save(kisten, file = file.path(onedrive, "kisten.RData"))
# save(elog,   file = file.path(onedrive, "elog.RData"))

# elog %>% filter(grepl("325", trip) & vessel=="") %>% View()
# elog <- elog %>% filter(!(vessel =="" & trip=="2023325"))
# elog <- elog %>% distinct()
# save(elog,  file = file.path(onedrive,  "elog.RData"))

# elog %>% 
#   ungroup() %>% 
#   filter(year %in% 2016:2019) %>% 
#   distinct(vessel, year, week, yday, source) %>%  
#   ggplot(aes(x=yday, y=vessel, colour=source)) +
#   theme_publication() +
#   geom_point() +
#   facet_wrap(~year, ncol=1)


# --------------------------------------------------------------------------------
# Replacing M-Catch elog data
# --------------------------------------------------------------------------------
tripdir    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/m-catch"

filelist <- list.files(
  path=tripdir,
  pattern="m-catch export",
  full.names = TRUE)

i <- 1
new  <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(filelist)) {
  print(paste(i, filelist[[i]]))
  
  new  <-
    bind_rows(
      new,
      readxl::read_excel(filelist[i],
                         sheet="catch details table", 
                         col_names=TRUE, 
                         col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
        data.frame() %>% 
        lowcase() %>% 
        rename(
          rect         = icesrectangle,
          catchdate    = activitydate,
          weight       = catchweight,
          economiczone = economicalzone,
          species      = fishspecie,
          vesselnumber = vesselhullnumber
        ) %>% 
        # mutate(catchdate       = as.Date(as.numeric(catchdate), origin="1899-12-30")) %>% 
        # mutate(arrivaldate     = as.Date(as.numeric(arrivaldate), origin="1899-12-30")) %>% 
        mutate(file            = basename(filelist[i])) %>% 
        mutate(vessel          = stringr::word(file, 3)) %>% 
        mutate(source          = "m-catch") %>% 
        
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
        mutate(
          tmp = lat,
          lat = lon,
          lon = tmp
        ) %>% 
      dplyr::select(-c(activitydayofyear,
                         activitydayofyearbst,
                         activitydayofyearcet,
                         catchamount,
                         discard,
                         entryid,
                         faoarea, faodivision, faosubarea, faosubdivision, faounit,
                         gearactivity, gearamount, gearlength,
                         juliandate,
                         juvenile,
                         tmp,
                         tripid,
                         vesselcallsign, vesselcfr, vesselflagstate, vesselgbrrss,
                         vesselid, vesselname,
                         zoneactivity))
    )
}

janitor::compare_df_cols(t, new)
skimr::skim(elog)
t %>% distinct(lat) %>% View()

tmp <-
  bind_rows(t, new)

tmp %>% 
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  geom_point(aes(colour=source)) +
  facet_wrap(~year)

t <-
  elog %>% 
  filter(!(source=="m-catch"))

  group_by(vessel, year, source) %>% 
  distinct(week) %>% 
  group_by(vessel, year, source) %>% 
  summarise(minweek = min(week), maxweek=max(week))

# plot of elog data by year
elog %>% 
  left_join(rect_df, by="rect") %>%
  mutate(lat = ifelse(is.na(lat.x), lat.y, lat.x),
         lon = ifelse(is.na(lon.x), lon.y, lon.x)) %>% 
  ggplot(aes(x=lon, y=lat)) + theme_publication() +
  geom_sf(data=world_mr_sf, font = "Arial", inherit.aes = FALSE) +
  coord_sf(xlim=c(-10,10), ylim=c(46,57)) + 
  geom_point(aes(colour=source)) +
  facet_wrap(~year)

# plot of pefa 2023 elog data by date
elog %>% filter(date == dmy("17-03-2023")) %>% View()
elog %>% 
  left_join(rect_df, by="rect") %>%
  filter(year == 2023, source=="pefa") %>% 
  mutate(lat = ifelse(is.na(lat.x), lat.y, lat.x),
         lon = ifelse(is.na(lon.x), lon.y, lon.x)) %>% 
  filter(month==3, lon > 0, lat < 50) %>% 
  View()

  ggplot(aes(x=lon, y=lat)) + theme_publication() + theme(legend.position = "none") +
  geom_sf(data=world_mr_sf, font = "Arial", inherit.aes = FALSE) +
  coord_sf(xlim=c(-10,10), ylim=c(46,57)) + 
  geom_point(aes(colour=as.character(date))) +
  facet_wrap(~month)

  group_by(vessel, year, source, week) %>% 
  summarise(ndays = n_distinct(date)) %>% 
  group_by(vessel, year, source) %>% 
  tidyr::pivot_wider(names_from = week, values_from = ndays, values_fill = 0) %>% 
  tidyr::pivot_longer(names_to = "week", values_to = "ndays", 4:56) %>% 
  mutate(week= as.numeric(week)) %>% 
  
  # check if in applicable week
  left_join(elog_weeks, by=c("vessel","year","source")) %>% 
  filter(week >= minweek, week <= maxweek) %>% 
  
  ggplot(aes(x=week, y=ndays)) +
  theme_publication() +
  geom_point(aes(colour=source), size=0.75, shape=1) +
  expand_limits(y=0) +
  labs(x="weeknummer", y="visdagen per week", title="Aantal visdagen per week") +
  facet_grid(vessel~year)




# --------------------------------------------------------------------------------
# fixing kisten problem
# --------------------------------------------------------------------------------

tmp <-
  kisten %>% 
  filter(trip=="2023409-412") %>% 
  distinct(datetime, .keep_all = TRUE) 

kisten <-
  kisten %>% 
  filter(trip !="2023409-412") %>% 
  bind_rows(tmp)

save(kisten,  file = file.path(onedrive, "kisten.RData"))  

# --------------------------------------------------------------------------------
# Fix linking between marelec and hauls
# --------------------------------------------------------------------------------
kisten <-
  kisten %>% 
  mutate(haul = ifelse(vessel == "SCH135" & trip=="2023321" & haul >= 28, haul+1, haul))

save(kisten, file=file.path(onedrive, "kisten.RData"))

# --------------------------------------------------------------------------------
# Redoing link between marelec and hauls
# --------------------------------------------------------------------------------
h <-
  haul %>% 
  # filter(date >= lubridate::dmy("9/1/2023")) %>% 
  filter(trip=="2023239") 
  # group_by(vessel, trip) %>% 
  # mutate(nexthaultime = lead(haultime)) %>% 
  # mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
  # mutate(nexthaultime = as_datetime(nexthaultime))

m <- 
  kisten %>% 
  filter(trip == "2023239") %>% 
  rename(haulm = haul, haul2m=haul2)
  # filter(datetime >= lubridate::dmy("9/1/2023")) %>% 
  # rename(haul2=haul)

h %>% 
  ggplot(aes(x=haultime, y=1)) +
  theme_publication() +
  geom_point(colour="red", size=3, shape=21) +
  geom_point(data=m, aes(x=datetime, y=1.02), colour="blue", size=1) +
  # geom_point(data=m, aes(x=datetime, y=0.8), colour="blue", size=1)
  expand_limits(y=0)

h %>% 
  ggplot(aes(x=haul, y=haulm)) +
  theme_publication() +
  geom_point(colour="red", size=3, shape=21) +
  geom_point(data=m, aes(x=datetime, y=1.02), colour="blue", size=1) +
  # geom_point(data=m, aes(x=datetime, y=0.8), colour="blue", size=1)
  expand_limits(y=0)

t <-
  sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht, m.datetime, 
  m.haul2, h.haul from m
                join h on m.vessel   == h.vessel and
                          m.trip     == h.trip and
                          m.datetime >= h.haultime and
                          m.datetime <  h.nexthaultime") %>%
  as_tibble() 


kisten <-
  kisten %>% 
  rename(haul2=haul) %>% 
  left_join(dplyr::select(t,
                          vessel, trip, datetime, haul),
            by=c("vessel","trip", "datetime")) 

save(kisten,  file = file.path(onedrive, "kisten.RData"))  


# --------------------------------------------------------------------------------
# Check the number of fishing days per week
# --------------------------------------------------------------------------------

elog_weeks <-
  elog %>% 
  group_by(vessel, year, source) %>% 
  distinct(week) %>% 
  group_by(vessel, year, source) %>% 
  summarise(minweek = min(week), maxweek=max(week))

elog %>% 
  group_by(vessel, year, source, week) %>% 
  summarise(ndays = n_distinct(date)) %>% 
  group_by(vessel, year, source) %>% 
  tidyr::pivot_wider(names_from = week, values_from = ndays, values_fill = 0) %>% 
  tidyr::pivot_longer(names_to = "week", values_to = "ndays", 4:56) %>% 
  mutate(week= as.numeric(week)) %>% 
  
  # check if in applicable week
  left_join(elog_weeks, by=c("vessel","year","source")) %>% 
  filter(week >= minweek, week <= maxweek) %>% 
  
  ggplot(aes(x=week, y=ndays)) +
  theme_publication() +
  geom_point(aes(colour=source), size=0.75, shape=1) +
  expand_limits(y=0) +
  labs(x="weeknummer", y="visdagen per week", title="Aantal visdagen per week") +
  facet_grid(vessel~year)




h %>% 
  rename(datetime = haultime) %>% 
  ggplot(aes(x=datetime, y=1)) +
  theme_publication() +
  theme(legend.position = "none") +
  geom_point(aes(colour=ac(haul))) +
  geom_point(data=m,
             aes(x=datetime, y=1.2, colour=ac(haul2)),
             size=0.5, alpha=0.5) +
  geom_point(data=t,
             aes(x=datetime, y=0.8, colour=ac(haul)),
             size=0.5, alpha=0.5) +
  expand_limits(y=0) +
  facet_wrap(~paste(vessel,trip), scales="free_x", ncol=1)


# redo marelec lots
h <-
  haul %>% 
  group_by(vessel, trip) %>% 
  mutate(nexthaultime = lead(haultime)) %>% 
  mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
  mutate(nexthaultime = as_datetime(nexthaultime))

m <- 
  marelec_lot %>% 
t <-
  sqldf::sqldf("select h.vessel, h.trip, h.haul, 
                       m.lotnummer, m.soorten, m.maat, m.datetime, 
                       m.gewicht, m.time_diff  from h
                join m on m.vessel   == h.vessel and
                          m.trip     == h.trip and
                          m.datetime >= h.haultime and 
                          m.datetime <  h.nexthaultime") %>% 
  as_tibble() 

# remove certain trips
haul <-
  haul %>% 
  filter(trip %notin% c("2023320", "2023237", "2023064")) 

save(haul, file=file.path(onedrive, "haul.RData"))

trip <-
  trip %>% 
  filter(trip %notin% c("2023320", "2023237", "2023064")) 

save(trip, file=file.path(onedrive, "trip.RData"))


# elog vesselname change
elog <-
  elog %>% 
  mutate(vessel = ifelse(vessel=="SL09", "SL9", vessel)) 
save(elog, file=file.path(onedrive, "elog.RData"))

# plot of total catch vs aanvoer
t1 <-
  haul %>% 
  group_by(vessel, trip, haul) %>% 
  summarise(totalcatch = sum(totalcatch, na.rm=TRUE)) 

t2 <-
  kisten %>% 
  group_by(vessel, trip, haul) %>% 
  summarise(aanvoer = sum(gewicht, na.rm=TRUE)) 

cols <- c("negatieve bijvangst" = "red", "bijvangst" = "black")

full_join(t1, t2, by=c("vessel","trip","haul")) %>% 
  filter(!(is.na(totalcatch) | is.na(aanvoer))) %>%
  filter(vessel != "Z99") %>% 
  mutate(berekendebijvangst = aanvoer - totalcatch) %>% 
  mutate(colour = ifelse(berekendebijvangst < 0, "negatieve bijvangst", "bijvangst")) %>% 
  # View()
  group_by(vessel) %>% 
  # mutate(rownumber = row_number()) %>% 
  mutate(rownumber = paste(gsub("2023","", trip), 
                           stringr::str_pad(haul, width=2, pad="0"))) %>% 
  
  ggplot(aes(x=rownumber, y=berekendebijvangst)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(aes(fill=colour),
           stat="identity", alpha=0.5) +
  scale_fill_manual(values=cols) +
  labs(x="trek", y="kg", title="berekende bijvangst per trek") +
  facet_wrap(~vessel, ncol=1, scales="free_x")

full_join(t1, t2, by=c("vessel","trip","haul")) %>% 
  filter(!(is.na(totalcatch) | is.na(aanvoer))) %>%
  filter(vessel != "Z99") %>% 
  mutate(berekendebijvangst = aanvoer - totalcatch) %>% 
  mutate(colour = ifelse(berekendebijvangst < 0, "red", "black")) %>% 
  # View()
  group_by(colour) %>% 
  summarise(n = n()) %>% 
  mutate(prop = 100*n/sum(n))

# --------------------------------------------------------------------------------
# Allocating vessel and trips to certain datasets
# --------------------------------------------------------------------------------
elog <-
  elog %>% 
  mutate(
    vessel = ifelse (year==2023 & vessel=="" & trip=="2023241", "SCH65", vessel),
    vessel = ifelse (year==2023 & vessel=="" & trip %in% c("2023317", "2023318","2023324"), "SCH135",vessel)
  )

tmp <-
  elog %>% 
  mutate(
    trip = ifelse (date >= dmy("2/1/2023") & date <= dmy("5/1/2023")  & trip=="" , "2023062", trip),
    trip = ifelse (date >= dmy("9/1/2023") & date <= dmy("12/1/2023") & trip=="" , "2023063", trip),
  )

tmp %>% filter(trip=="2023063") %>% View()

elog <-
  elog %>% 
  filter(!(vessel=="Z99" & year==2023 & trip==""))

tmp %>% filter(vessel=="Z99", year==2023) %>% group_by(trip) %>% summarise(weight = sum(weight))
save(elog, file=file.path(onedrive, "elog.RData"))

# --------------------------------------------------------------------------------
# Allocating vessel and trips to certain datasets
# --------------------------------------------------------------------------------
elog <-
  elog %>% 
  mutate(species        = ifelse(is.na(species), SN, species)) %>% 
  mutate(datetime       = ifelse(is.na(datetime), date + lubridate::hm(RT), datetime) ) %>% 
  mutate(datetime       = as_datetime(datetime)) %>% 
  mutate(meshsize       = as.integer(ifelse(is.na(meshsize), ME, meshsize))) %>% 
  mutate(geartype       = ifelse(is.na(geartype),GE,geartype)) %>% 
  mutate(faozone        = ifelse(is.na(faozone),FA, faozone)) %>% 
  mutate(economiczone   = ifelse(is.na(economiczone),EZ,economiczone)) %>% 
  mutate(weight         = as.numeric(ifelse(is.na(weight),WT,weight))) %>% 
  mutate(rect           = ifelse(is.na(rect),SR,rect)) %>% 
  mutate(freshness      = ifelse(is.na(freshness),FE,freshness)) %>% 
  mutate(lon            = ifelse(is.na(lon),longitude,lon)) %>% 
  mutate(lat            = ifelse(is.na(lat),latitide,lat)) %>% 
  rename(lat2 = lat, lon2 = lon) %>% 
  rename(version = VRS) %>% 
  rename(vesselidentifier = NFR) %>% 
  rename(vesselidentifier2 = RV) %>% 
  rename(fishingoperations = FO) %>% 
  rename(entryidentifier = RN) %>% 
  mutate(entryidentifier = ifelse(!is.na(RNNEW), RNNEW, entryidentifier)) %>% 
  mutate(quarter         = lubridate::quarter(date)) %>% 
  mutate(month           = lubridate::month(date)) %>% 
  mutate(yday            = lubridate::yday(date)) %>% 
  left_join(rect_df, by="rect") %>% 
  dplyr::select(-NAD, -SN, -RT, -XR, -'NA', -RD, -TV, -CVD, -CVO, -MA, -ME, -GE, -FA, -EZ, -WT, -SR, -NLSPN, -RNNEW,
                -NLERS_id, -FE, -DA, -DD, -ET, -MD, -MV, -SWE, -RE, -NH, -TI, -TN, -VT, -longitude, -latitide)

glimpse(t) 

count_na(t) %>% as.data.frame() %>% setNames("n") %>% rownames_to_column(var="variable") %>% arrange(variable)
t %>% ungroup() %>% summarise_all(sum(is.na(names(.))))
t %>% filter(is.na(date)) %>% View()
t %>% filter(lat==0) %>% View()
sort(unique(t$lon))

elog_raw <- elog
save(elog_raw, file=file.path(onedrive, "elog_raw.RData"))
save(elog, file=file.path(onedrive, "elog.RData"))

t <- 
  read.csv(file=file.path(get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/data"), 
                        "WMR Landings_Discards_all_hauls.csv")) %>% 
  lowcase() %>% 
  group_by(ship, tripcode) %>% 
  mutate(haul = row_number()) %>%
  mutate(date = lubridate::ymd(date)) %>% 
  rename(vessel=ship, landings = kgland, discards = kgdisc) %>% 
  mutate(catch = landings + discards) %>% 
  mutate(percdiscards = discards / catch) 

e <-
  elog %>% 
  filter(paste0(vessel, date) %in% paste0(t$vessel, t$date)) %>% 
  group_by(vessel, trip, date, rect, lat, lon) %>% 
  summarise(landings = sum(weight, na.rm=TRUE))

comb <-
  t %>% 
  group_by(vessel, tripcode, date) %>% 
  summarise(
    nhauls       = n_distinct(haul),
    catch        = sum(catch, na.rm=TRUE),
    landings_wmr = sum(landings, na.rm=TRUE),
    discards     = sum(discards, na.rm=TRUE)
  ) %>% 
  left_join(e, by=c("vessel", "date")) %>% 
  mutate(landings_diff = landings - landings_wmr)

# plot of difference in landings in WMR and elog data
comb %>% 
  ggplot(aes(x=date, y=landings_diff)) +
  theme_publication() +
  geom_bar(stat="identity") +
  facet_wrap(~tripcode, scales="free_x")


t %>% 
  ggplot(aes(x=haul, y=kg_disc)) +
  theme_publication() +
  geom_bar(stat="identity") + 
  facet_wrap(~trip_code, scales = "free_x")

t %>% 
  mutate(perc_disc = kg_disc/kg_total) %>% 
  ggplot(aes(x=haul, y=perc_disc)) +
  theme_publication() +
  geom_bar(stat="identity") + 
  facet_wrap(~trip_code, scales = "free")

t %>% 
  group_by(ship, trip_code, DATE) %>% 
  summarise(
    kg_land = sum(kg_land),
    kg_disc = sum(kg_disc),
    kg_total = sum(kg_total),
  ) %>% 
  ggplot(aes(x=DATE, y=kg_disc)) +
  theme_publication() +
  geom_bar(stat="identity") + 
  facet_wrap(~trip_code, scales = "free_x")
