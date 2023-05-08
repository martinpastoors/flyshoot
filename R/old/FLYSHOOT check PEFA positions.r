# =======================================================================================
# FLYSHOOT: check PEFA positions.r
# 
# Martin Pastoors
#
# 09/03/2023
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

# Source all the utils
source("../prf/R/my utils.r")
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

load(file.path(spatialdir, "asfis.RData"))

# set onedrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")

# load datasets
load(file.path(onedrive, "haul.RData"))
load(file.path(onedrive, "kisten.RData"))
load(file.path(onedrive, "elog.RData"))
load(file.path(onedrive, "trip.RData"))

e <-
  elog %>% 
  filter(!is.na(date) & !is.na(lon) & !is.na(lat) & year==2023)

inspectdf::inspect_num(e) %>% inspectdf::show_plot()
inspectdf::inspect_imb(e) %>% inspectdf::show_plot()
inspectdf::inspect_cat(e) %>% inspectdf::show_plot()
distinct(e, vessel)

comb <-
  e %>% 
  dplyr::group_by(vessel, trip, haul, rect, faozone) %>%
  summarise(
    catchdate = mean(catchdate, na.rm=TRUE), 
    lat       = mean(lat, na.rm=TRUE), 
    lon       = mean(lon, na.rm=TRUE),
    weight    = sum(weight, na.rm=TRUE)
  ) %>% 
  left_join(dplyr::select(haul,
                          vessel, trip, haul, date, lat, lon, rect, division, totalcatch), 
            by=c("vessel","trip","haul"))
  
# --------------------------------------------------------------------------------
# plot by trips
# --------------------------------------------------------------------------------

comb %>% 
  dplyr::select(vessel, trip, haul, lon.x, lat.x, lon.y, lat.y) %>% 
  distinct() %>% 
  pivot_longer(names_to = "variable", values_to = "data", lon.x:lat.y) %>% 
  tidyr::separate(variable, c("var","source"), sep="\\.") %>% 
  mutate(source = ifelse(source=="x", "pefa","treklijst")) %>%
  group_by(vessel, trip, haul) %>% 
  tidyr::pivot_wider(names_from = var, values_from = data) %>%
  unnest(c(lat, lon)) %>% 
  
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap(xlim=xlim , ylim=ylim) +
  
  geom_text(data=rect_df, aes(x=lon, y=lat, label=rect),
            hjust=0, vjust=0, nudge_x=0.05, nudge_y=0.025, colour="white") +
  
  geom_polygon(data=world_mr_df, aes(x=long, y=lat, group=group), fill = "grey75") +
  geom_point(aes(colour=source)) +
  facet_wrap(~paste(vessel,trip))


# by rectangle

comb %>% 
  dplyr::select(vessel, trip, haul, rect.x, rect.y) %>% 
  distinct() %>% 
  pivot_longer(names_to = "variable", values_to = "rect", rect.x:rect.y) %>% 
  tidyr::separate(variable, c("var","source"), sep="\\.") %>% 
  mutate(source = ifelse(source=="x", "pefa","treklijst")) %>%

  ggplot(aes(x=rect)) +
  theme_publication() +

  geom_histogram(aes(fill=source), stat="count", position = position_dodge2(preserve="single")) +
  facet_wrap(~paste(vessel,trip), scales="free_x")

