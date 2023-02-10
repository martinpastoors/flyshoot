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

load(file.path(spatialdir, "afsis.RData"))

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

elog %>% filter(vessel=="SL09") %>% distinct(vessel, trip)
elog <- elog %>% filter(vessel !="SL09") 
save(elog,  file = file.path(onedrive, "elog.RData"))  

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

save(haul, file=file.path(my_rdata_drive, "haul.RData"))

trip <-
  trip %>% 
  filter(trip %notin% c("2023320", "2023237", "2023064")) 

save(trip, file=file.path(my_rdata_drive, "trip.RData"))


# elog vesselname change
elog <-
  elog %>% 
  mutate(vessel = ifelse(vessel=="SL09", "SL9", vessel)) 
save(elog, file=file.path(my_rdata_drive, "elog.RData"))

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

  