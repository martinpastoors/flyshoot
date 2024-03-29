# =======================================================================================
# FLYSHOOT: modify tripdata.r
# 
# do some manipulations on the datasets
#
# Martin Pastoors
#
# 25/01/2023 checks on marelec data

# TO DO: 
# check positions of rectangle in elog
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
load(file.path(onedrive, "elog_trek.RData"))
load(file.path(onedrive, "trip.RData"))
load(file.path(onedrive, "soorten.RData"))

# skimr::skim(haul)

# haul %>% distinct(captain) %>% View()
# haul %>% filter(grepl("Harry", captain)) %>% View()

# --------------------------------------------------------------------------------
# check specific trip
# --------------------------------------------------------------------------------

kisten %>% filter(vessel=="SCH99", trip=="2023110", haul %in% 6:8) %>% View()

# --------------------------------------------------------------------------------
# remove trips with hyphen or space in vesselname
# --------------------------------------------------------------------------------

# haul %>% distinct(vessel, trip) %>% filter(grepl("-", vessel)) %>% View()

haul      <- haul %>% filter(!grepl("-| ", vessel)) 
kisten    <- kisten %>% filter(!grepl("-| ", vessel)) 
elog      <- elog %>% filter(!grepl("-| ", vessel)) 
elog_trek <- elog_trek %>% filter(!grepl("-| ", vessel)) 
trip      <- trip %>% filter(!grepl("-| ", vessel)) 

save(haul,      file = file.path(onedrive, "haul.RData"))
save(kisten,    file = file.path(onedrive, "kisten.RData"))
save(elog,      file = file.path(onedrive, "elog.RData"))
save(elog_trek, file = file.path(onedrive, "elog_trek.RData"))
save(trip,      file = file.path(onedrive, "trip.RData"))

# --------------------------------------------------------------------------------
# remove trips with underscore in tripname
# --------------------------------------------------------------------------------

kisten <-
  kisten %>% 
  filter(!grepl("_", trip)) 

save(kisten,   file = file.path(onedrive, "kisten.RData"))

# --------------------------------------------------------------------------------
# correct rectangle lat lon in elog data
# --------------------------------------------------------------------------------

elog %>% 
  distinct(rect, lat, lon) %>% 
  group_by(rect) %>% 
  mutate(nobs=n()) %>% 
  filter(nobs>1) %>% 
  arrange(rect) %>% 
  View()

elog <-
  elog %>% 
  mutate(lat2 = ifelse(is.na(lat2) & (lat-floor(lat)) %notin% c(0, 0.25, 0.5, 0.75),
                       lat,
                       lat2)) %>% 
  mutate(lon2 = ifelse(is.na(lon2) & (lon-floor(lon)) %notin% c(0, 0.5),
                       lon,
                       lon2)) %>% 
  dplyr::select(-lat, -lon) %>% 
  left_join(rect_df, by="rect") 

save(elog,   file = file.path(onedrive, "elog.RData"))

# --------------------------------------------------------------------------------
# remove empty columns and change names of certain columns
# --------------------------------------------------------------------------------
tripdata <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/tripdata")

treklijst <-
  list.files(
    path=file.path(tripdata),
    pattern="treklijst",
    full.names = TRUE, 
    recursive = TRUE) %>% 
  as.data.frame() %>% 
  setNames("filename") %>% 
  mutate(file=basename(filename)) %>% 
  mutate(file= gsub("\\.xlsx","",file)) %>% 
  filter(!grepl("xxx|LEEG",file)) %>% 
  separate(file, into=c("vessel", "trip", "source"), sep=" ", remove = FALSE) %>% 
  mutate(trip = gsub("_","",trip)) %>% 
  dplyr::select(vessel, trip, file, source)

elog_per_trek <-
  list.files(
    path=file.path(tripdata),
    pattern="elog_pefa_per_trek",
    full.names = TRUE, 
    recursive = TRUE) %>% 
  as.data.frame() %>% 
  setNames("filename") %>% 
  mutate(file=basename(filename)) %>% 
  mutate(file= gsub("\\.xlsx","",file)) %>% 
  mutate(source="pefa per trek") %>% 
  filter(!grepl("xxx|LEEG",file)) %>% 
  separate(file, into=c("vessel", "trip", "other"), sep=" ", remove = FALSE) %>% 
  mutate(trip = gsub("_","",trip)) %>% 
  dplyr::select(vessel, trip, file, source)

haul <- 
  haul %>% 
  janitor::remove_empty(which = "cols") %>% 
  # rename(
  #   captain = skipper,
  #   catchweight = totalcatch,
  #   departureport = portembarked,
  #   departuredate = dateembarked,
  #   arrivalport = portdisembarked,
  #   arrivaldate = datedisembarked
  # ) %>% 
  # dplyr::select(-shootlat, -shootns, -shootlong, -shootew) %>% 
  mutate(source = ifelse(is.na(source),"treklijst", source)) %>% 
  left_join(treklijst, by=c("vessel","trip","source")) %>% 
  left_join(elog_per_trek, by=c("vessel","trip","source")) %>% 
  filter(!is.na(haultime)) %>% 
  mutate(
    haultime  = as_datetime(ifelse(lubridate::year(haultime)==2146, haultime - lubridate::years(123), haultime)),
    shoottime = as_datetime(ifelse(lubridate::year(shoottime)==2146, shoottime - lubridate::years(123), shoottime)),
    shoottime = as_datetime(ifelse(is.na(shoottime) & !is.na(haultime), haultime - lubridate::hm("1:20"), shoottime)),
    shoottime2= as_datetime(ifelse(is.na(shoottime2) & !is.na(haultime), haultime - lubridate::hm("0:40"), shoottime2)),
    week      = ifelse(is.na(week), lubridate::week(date), week),
    duration  = as.numeric(ifelse(is.na(duration), as.duration(shoottime %--% haultime)/3600, duration))
  ) %>% 
  group_by(vessel, trip) %>% 
  arrange(vessel, trip, haul) %>% 
  mutate(nexthaultime= as_datetime(ifelse(is.na(nexthaultime), lead(haultime), nexthaultime))) %>% 
  mutate(file = ifelse(is.na(file.x), file.y, file.x)) %>% 
  dplyr::select(-file.x, -file.y) %>% 
  ungroup()

save(haul,   file = file.path(onedrive, "haul.RData"))

trip <-
  trip %>% 
  mutate(action = case_when(
    action == "embarked"    ~ "departure",
    action == "disembarked" ~ "arrival",
    TRUE                    ~ NA
  ))

save(trip,   file = file.path(onedrive, "trip.RData"))

# skimr::skim(haul_test)
# setdiff(names(haul), names(haul_test))

# --------------------------------------------------------------------------------
# calculating live weight in elog files
# --------------------------------------------------------------------------------

elog <-
  elog %>% 
  mutate(liveweight = ifelse(!is.na(conversionfactor), conversionfactor*weight, weight))

elog_trek <-
  elog_trek %>% 
  mutate(liveweight = ifelse(!is.na(conversionfactor), conversionfactor*weight, weight))

save(elog,      file = file.path(onedrive, "elog.RData"))
save(elog_trek, file = file.path(onedrive, "elog_trek.RData"))


# --------------------------------------------------------------------------------
# Adding species to marelec data
# --------------------------------------------------------------------------------

kisten <-
  bind_rows(
    kisten %>% 
      filter(is.na(species)) %>% 
      dplyr::select(-species) %>% 
      left_join(soorten, by="soorten"),
    kisten %>% 
      filter(!is.na(species))
  )

save(kisten,      file = file.path(onedrive, "kisten.RData"))

# sum(kisten$gewicht, na.rm=TRUE)
# sum(t$gewicht, na.rm=TRUE)

# --------------------------------------------------------------------------------
# removing erroneous trips
# --------------------------------------------------------------------------------

# trip %>% filter(vessel=="SCH153") %>% group_by(vessel, trip) %>% summarise(n=n())
# trip <- trip %>% filter(vessel!="SCH153") 
# save(trip,  file = file.path(onedrive,  "trip.RData"))

# haul %>% filter(vessel=="Z99") %>% group_by(vessel, trip) %>% summarise(n=n())

# haul <- haul %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023420", "2023421",trip)) # %>% View()
# trip <- trip %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023320", "2023421",trip))

# haul <- haul %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip))
# trip <- trip %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip))
# kisten <- kisten %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip))
# elog <- elog %>% mutate(trip = ifelse(vessel=="SL9"& trip=="2023418", "2023417",trip)) 

# haul   <- haul %>% filter(vessel != "SCH153")
# trip   <- trip %>% filter(vessel != "SCH153")
# kisten <- kisten %>% filter(vessel != "SCH153")
# elog   <- elog %>% filter(vessel != "SCH153")

# elog <-
#   elog %>% 
#   filter(!(vessel=="SL9" & year == 2018 &  week == 52 & source=="m-catch")) %>% 
#   filter(!(vessel=="SCH135" & year == 2019 &  week == 1 & source=="m-catch")) %>% 
#   filter(!(vessel=="SL65" & year == 2018 &  week == 5 & source=="m-catch")) 
  

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

elog <- elog %>% filter(!grepl("_",trip)) 
save(elog,  file = file.path(onedrive,  "elog.RData"))

elog_trek <- elog_trek %>% filter(!grepl("_",trip)) 
save(elog_trek,  file = file.path(onedrive,  "elog_trek.RData"))

# --------------------------------------------------------------------------------
# Replacing M-Catch elog data
# --------------------------------------------------------------------------------
# tripdir    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/m-catch"
# 
# filelist <- list.files(
#   path=tripdir,
#   pattern="m-catch export",
#   full.names = TRUE)
# 
# i <- 1
# new  <- data.frame(stringsAsFactors = FALSE)
# for (i in 1:length(filelist)) {
#   print(paste(i, filelist[[i]]))
#   
#   new  <-
#     bind_rows(
#       new,
#       readxl::read_excel(filelist[i],
#                          sheet="catch details table", 
#                          col_names=TRUE, 
#                          col_types="text",
#                          .name_repair =  ~make.names(., unique = TRUE))  %>% 
#         data.frame() %>% 
#         lowcase() %>% 
#         rename(
#           rect         = icesrectangle,
#           catchdate    = activitydate,
#           weight       = catchweight,
#           economiczone = economicalzone,
#           species      = fishspecie,
#           vesselnumber = vesselhullnumber
#         ) %>% 
#         # mutate(catchdate       = as.Date(as.numeric(catchdate), origin="1899-12-30")) %>% 
#         # mutate(arrivaldate     = as.Date(as.numeric(arrivaldate), origin="1899-12-30")) %>% 
#         mutate(file            = basename(filelist[i])) %>% 
#         mutate(vessel          = stringr::word(file, 3)) %>% 
#         mutate(source          = "m-catch") %>% 
#         
#         mutate(across (c("meshsize"), as.integer)) %>%
#         mutate(across (c("lat","lon", "weight",
#                          "catchdate", "departuredate", "arrivaldate","landingdate"), as.numeric)) %>%
#         mutate(across(c("catchdate", "departuredate", "arrivaldate","landingdate"), ~excel_timezone_to_utc(.,"UTC"))) %>% 
#         mutate(month   = lubridate::month(catchdate),
#                quarter = lubridate::quarter(catchdate),
#                week    = lubridate::week(catchdate),
#                yday    = lubridate::yday(catchdate),
#                year    = lubridate::year(catchdate),
#                date    = as.Date(catchdate)) %>% 
#         mutate(
#           tmp = lat,
#           lat = lon,
#           lon = tmp
#         ) %>% 
#       dplyr::select(-c(activitydayofyear,
#                          activitydayofyearbst,
#                          activitydayofyearcet,
#                          catchamount,
#                          discard,
#                          entryid,
#                          faoarea, faodivision, faosubarea, faosubdivision, faounit,
#                          gearactivity, gearamount, gearlength,
#                          juliandate,
#                          juvenile,
#                          tmp,
#                          tripid,
#                          vesselcallsign, vesselcfr, vesselflagstate, vesselgbrrss,
#                          vesselid, vesselname,
#                          zoneactivity))
#     )
# }
# 
# janitor::compare_df_cols(t, new)
# skimr::skim(elog)
# t %>% distinct(lat) %>% View()
# 
# tmp <-
#   bind_rows(t, new)
# 
# tmp %>% 
#   ggplot(aes(x=lon, y=lat)) +
#   theme_publication() +
#   geom_point(aes(colour=source)) +
#   facet_wrap(~year)
# 
# t <-
#   elog %>% 
#   filter(!(source=="m-catch"))
# 
#   group_by(vessel, year, source) %>% 
#   distinct(week) %>% 
#   group_by(vessel, year, source) %>% 
#   summarise(minweek = min(week), maxweek=max(week))
# 
# # plot of elog data by year
# elog %>% 
#   left_join(rect_df, by="rect") %>%
#   mutate(lat = ifelse(is.na(lat.x), lat.y, lat.x),
#          lon = ifelse(is.na(lon.x), lon.y, lon.x)) %>% 
#   ggplot(aes(x=lon, y=lat)) + theme_publication() +
#   geom_sf(data=world_mr_sf, font = "Arial", inherit.aes = FALSE) +
#   coord_sf(xlim=c(-10,10), ylim=c(46,57)) + 
#   geom_point(aes(colour=source)) +
#   facet_wrap(~year)
# 
# # plot of pefa 2023 elog data by date
# elog %>% filter(date == dmy("17-03-2023")) %>% View()
# elog %>% 
#   left_join(rect_df, by="rect") %>%
#   filter(year == 2023, source=="pefa") %>% 
#   mutate(lat = ifelse(is.na(lat.x), lat.y, lat.x),
#          lon = ifelse(is.na(lon.x), lon.y, lon.x)) %>% 
#   filter(month==3, lon > 0, lat < 50) %>% 
#   View()
# 
#   ggplot(aes(x=lon, y=lat)) + theme_publication() + theme(legend.position = "none") +
#   geom_sf(data=world_mr_sf, font = "Arial", inherit.aes = FALSE) +
#   coord_sf(xlim=c(-10,10), ylim=c(46,57)) + 
#   geom_point(aes(colour=as.character(date))) +
#   facet_wrap(~month)
# 
#   group_by(vessel, year, source, week) %>% 
#   summarise(ndays = n_distinct(date)) %>% 
#   group_by(vessel, year, source) %>% 
#   tidyr::pivot_wider(names_from = week, values_from = ndays, values_fill = 0) %>% 
#   tidyr::pivot_longer(names_to = "week", values_to = "ndays", 4:56) %>% 
#   mutate(week= as.numeric(week)) %>% 
#   
#   # check if in applicable week
#   left_join(elog_weeks, by=c("vessel","year","source")) %>% 
#   filter(week >= minweek, week <= maxweek) %>% 
#   
#   ggplot(aes(x=week, y=ndays)) +
#   theme_publication() +
#   geom_point(aes(colour=source), size=0.75, shape=1) +
#   expand_limits(y=0) +
#   labs(x="weeknummer", y="visdagen per week", title="Aantal visdagen per week") +
#   facet_grid(vessel~year)




# --------------------------------------------------------------------------------
# fixing kisten problem
# --------------------------------------------------------------------------------

# tmp <-
#   kisten %>% 
#   filter(trip=="2023409-412") %>% 
#   distinct(datetime, .keep_all = TRUE) 
# 
# kisten <-
#   kisten %>% 
#   filter(trip !="2023409-412") %>% 
#   bind_rows(tmp)
# 
# save(kisten,  file = file.path(onedrive, "kisten.RData"))  

# --------------------------------------------------------------------------------
# Fix linking between marelec and hauls
# --------------------------------------------------------------------------------
# kisten <-
#   kisten %>% 
#   mutate(haul = ifelse(vessel == "SCH135" & trip=="2023321" & haul >= 28, haul+1, haul))
# 
# save(kisten, file=file.path(onedrive, "kisten.RData"))

# --------------------------------------------------------------------------------
# Redoing link between marelec and hauls
# --------------------------------------------------------------------------------

# mytrip <- "2023002"
# 
# h <-
#   haul %>% 
#   # filter(date >= lubridate::dmy("9/1/2023")) %>% 
#   filter(trip==mytrip) %>% 
#   # group_by(vessel, trip) %>% 
#   # mutate(nexthaultime = lead(haultime)) %>% 
#   # mutate(nexthaultime = ifelse(is.na(nexthaultime), lubridate::dmy_hm("31/12/2023 23:59"), nexthaultime)) %>% 
#   # mutate(nexthaultime = as_datetime(nexthaultime))
# 
# m <- 
#   kisten %>% 
#   filter(trip == mytrip) %>% 
#   rename(haulm = haul, haul2m=haul2)
#   # filter(datetime >= lubridate::dmy("9/1/2023")) %>% 
#   # rename(haul2=haul)
# 
# h %>% 
#   ggplot(aes(x=haultime, y=1)) +
#   theme_publication() +
#   geom_point(colour="red", size=3, shape=21) +
#   geom_point(data=m, aes(x=datetime, y=1.02), colour="blue", size=1) +
#   # geom_point(data=m, aes(x=datetime, y=0.8), colour="blue", size=1)
#   expand_limits(y=0)
# 
# h %>% 
#   ggplot(aes(x=haul, y=haulm)) +
#   theme_publication() +
#   geom_point(colour="red", size=3, shape=21) +
#   geom_point(data=m, aes(x=datetime, y=1.02), colour="blue", size=1) +
#   # geom_point(data=m, aes(x=datetime, y=0.8), colour="blue", size=1)
#   expand_limits(y=0)
# 
# t <-
#   sqldf::sqldf("select m.vessel, m.trip, m.lotnummer, m.soorten, m.maat, m.gewicht, m.datetime, 
#   m.haul2, h.haul from m
#                 join h on m.vessel   == h.vessel and
#                           m.trip     == h.trip and
#                           m.datetime >= h.haultime and
#                           m.datetime <  h.nexthaultime") %>%
#   as_tibble() 
# 
# 
# kisten <-
#   kisten %>% 
#   rename(haul2=haul) %>% 
#   left_join(dplyr::select(t,
#                           vessel, trip, datetime, haul),
#             by=c("vessel","trip", "datetime")) 
# 
# save(kisten,  file = file.path(onedrive, "kisten.RData"))  

# --------------------------------------------------------------------------------
# checking hauls in haul and kisten
# --------------------------------------------------------------------------------

mytrip <- c("2023003")

h <-
  haul %>% 
  filter(trip %in% mytrip) %>% 
  dplyr::select(vessel, trip, haul, date, shoottime, haultime) %>% 
  mutate(haul = stringr::str_pad(haul, width=2, pad="0")) %>% 
  mutate(shoottime = hour(shoottime) + (minute(shoottime) + second(shoottime)/60)/100 ) %>% 
  mutate(haultime  = hour(haultime) + (minute(haultime) + second(haultime)/60)/100 ) %>% 
  mutate(source="haul")

m <- 
  kisten %>% 
  filter(trip %in% mytrip) %>% 
  mutate(source="kisten") %>% 
  mutate(haul = stringr::str_pad(haul, width=2, pad="0")) %>% 
  mutate(date = as.Date(datetime)) %>% 
  mutate(time = hour(datetime) + (minute(datetime) + second(datetime)/60)/100 ) 
# mutate(time = hms(str_extract(datetime, "[0-9]{2}:[0-9]{2}:[0-9]{2}")))


m1 <-
  m %>% 
  group_by(vessel, trip, haul) %>% 
  filter(row_number()==1)

coord_y_datetime <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  if (!is.null(ylim)) {
    ylim <- lubridate::as_datetime(ylim)
  }
  ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = expand)
}

coord_y_date <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  if (!is.null(ylim)) {
    ylim <- lubridate::as_date(ylim)
  }
  ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = expand)
}

h %>% 
  ggplot(aes(y=date)) +
  theme_publication() +
  theme(legend.position = "none") +
  # geom_point(colour="red", size=3, shape=21) +
  ggalt::geom_dumbbell(aes(x = shoottime, xend = haultime, colour=haul),
                       size=3, size_x = 1, size_xend = 1, alpha=0.5) +
  geom_text(aes(x=shoottime, label=haul, colour=haul), 
            vjust=1, nudge_y=0.02) +
  
  geom_point(data=m,
             aes(x=time, y=date , colour=haul),
             size=1, position = position_nudge(y = -0.05)) +
  geom_line(data=m,
             aes(x=time, y=date , colour=haul),
             linewidth=0.5, position = position_nudge(y = -0.05)) +
  geom_text(data=m1,
            aes(x=time, y=date, label=haul, colour=haul),
            vjust=0, nudge_y=-0.07, inherit.aes = FALSE) +
  coord_y_date(ylim=c(max(m1$date),min(m1$date))) +
  facet_wrap(~trip, scales="free_y")

# --------------------------------------------------------------------------------
# Check overlaps between haultime and next shoottime
# --------------------------------------------------------------------------------

t <-
  haul %>%
  filter(year == 2023) %>% 
  group_by(vessel, trip) %>%
  mutate(nextshoottime = lead(shoottime)) %>% 
  filter(haultime > nextshoottime)

haul %>% filter(vessel=="SCH65", trip=="2023243", haul %in% 11:13) %>% View()
# t %>% mutate(diff=as.integer(difftime(nexthaultime, haultime, units="min"))) %>% pull(diff) %>% hist()
t %>% mutate(diff=as.integer(difftime(nexthaultime, haultime, units="min"))) %>%
  filter(diff < 60) %>% dplyr::select(vessel, trip, haul, shoottime, haultime, nexthaultime, diff) %>% View()




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


# --------------------------------------------------------------------------------
# checking aanvoer en discards van Aravis
# --------------------------------------------------------------------------------

mytrip <- c("2023098", "2023099", "2023100", "2023101", "2023102")

h <-
  haul %>% 
  filter(trip %in% mytrip) %>% 
  filter(!is.na(catchweight)) %>% 
  dplyr::select(vessel, trip, haul, date, shoottime, haultime, catchweight) %>% 
  mutate(haul = stringr::str_pad(haul, width=2, pad="0")) 

m <- 
  kisten %>% 
  mutate(haul = stringr::str_pad(haul, width=2, pad="0")) %>% 
  filter(paste(trip, haul) %in% paste(h$trip,h$haul)) %>% 
  group_by(vessel, trip, haul) %>% 
  summarise(landings=sum(gewicht, na.rm=TRUE))

t <-
  left_join(h, m, by=c("vessel","trip","haul")) %>% 
  mutate(triphaul = paste(trip, haul)) %>%
  mutate(landings=ifelse(is.na(landings), 0, landings)) %>% 
  mutate(discards = catchweight - landings) %>% 
  # filter(discards >= 0) %>% 
  mutate(perc_discards = round((catchweight - landings) / catchweight, digits=2)) %>% 
  mutate(perc_discards = scales::percent(perc_discards, accuracy=1)) %>% 
  mutate(colour = ifelse(landings>catchweight, "red","black")) %>% 
  ungroup()  
  # summarise(
  #   discards = median(discards),
  #   landings = median(landings)
  # )

t %>% 
  dplyr::select(-catchweight) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data",c("landings","discards")) %>% 
  
  ggplot(aes(x=data)) +
  theme_publication() +
  geom_histogram(aes(fill=variable)) +
  facet_wrap(~variable, scales = "free_x")

t %>%
  dplyr::select(-discards) %>% 
  pivot_longer(names_to = "variable", values_to = "data", catchweight:landings) %>% 
  
  ggplot(aes(x=triphaul, y=data)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(aes(fill=variable),
           stat="identity", alpha=1, width=0.8, just=0.8, position = position_dodge2(width=1, preserve = "single")) +
  geom_text(data = dplyr::select(t,
                                 triphaul, catchweight, perc_discards, colour), 
            aes(x=triphaul, y=catchweight, label=perc_discards, colour=colour),
            inherit.aes = FALSE, hjust=0, vjust=0, size=3, nudge_y = 50, angle=90) +
  # scale_x_continuous(breaks=seq(1,max(t1$haul, m$haul, na.rm=TRUE), 1)) +
  scale_colour_manual(values = t$colour) +
  labs(x="trek", y="kg", title="totale vangst en aanvoer per trek") 

# --------------------------------------------------------------------------------
# size information in kisten CTC
# --------------------------------------------------------------------------------
myspecies <- "CTC"

kisten %>% 
  mutate(week = lubridate::week(datetime)) %>% 
  filter(species == myspecies) %>% 
  drop_na(maat) %>% 
  group_by(week, maat) %>% 
  summarise(gewicht = sum(gewicht, na.rm=TRUE)) %>% 
  group_by(week) %>% 
  filter(sum(gewicht) >= 2000) %>% 
  
  ggplot(aes(x=week, y=gewicht)) +
  theme_publication() +
  # geom_bar(aes(fill=maat), stat="identity") +
  geom_bar(aes(fill=maat), stat="identity", position = position_fill()) +
  labs(title=myspecies)

  #geom_point() +
  #facet_wrap(~maat)

# --------------------------------------------------------------------------------
# Aravis 2023 catch by species and trip
# --------------------------------------------------------------------------------

elog %>% 
  filter(vessel %in% c("Z99","SCH99"), year==2023) %>% 
  group_by(vessel, trip, species) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  filter(species=="HER") %>% 
  View()
