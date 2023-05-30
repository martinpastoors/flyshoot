# =======================================================================================================
# FLYSHOOT project overview
# 
# 16/05/2023 included all steps in generating project results
# =======================================================================================================

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

# set onedrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

reportdir <- file.path(onedrive,"report", "FLYSHOOT project overview May 2023")
dir.create(reportdir, showWarnings = FALSE)

figuresdir <- file.path(reportdir, "figures")
dir.create(figuresdir, showWarnings = FALSE)

presentationdir <- file.path(reportdir, "presentation")
dir.create(presentationdir, showWarnings = FALSE)

tablesdir <- file.path(reportdir, "tables")
dir.create(tablesdir, showWarnings = FALSE)

rdatadir <- file.path(reportdir, "rdata")
dir.create(rdatadir, showWarnings = FALSE)

fao_sf <- loadRData(file.path("C:/DATA/RDATA", "fao_sf.RData")) 
asfis  <- loadRData(file.path("C:/DATA/RDATA", "asfis.RData"))

price  <- 
  readr::read_rds(file.path(onedrive, "rdata/prices.rds")) %>% 
  dplyr::select(year, species, avgprice) %>% 
  mutate(price_cat = cut(avgprice, breaks=c(0,0.5, 1,2,3,4,5,10,15,20), dig.lab=10 ))

# load datasets
haul   <- loadRData(file.path(onedrive, "rdata/haul.RData"))
kisten <- loadRData(file.path(onedrive, "rdata/kisten.RData")) 
elog   <- loadRData(file.path(onedrive, "rdata/elog.RData")) %>% 
  mutate(species = toupper(species)) %>% 
  mutate(species = ifelse(species=="JAX","HOM",species))

soorten <- readxl::read_xlsx(path="soorten.xlsx")

yrs     <- 2012:2023
mnths   <- NA
vessels <- NA

myyear <- max(yrs)

# making the data selections
h <- 
  haul %>%
  ungroup() %>% 
  {if(!is.na(all(vessels))) filter(., vessel %in% vessels) else . } %>% 
  {if(!is.na(all(yrs)))     filter(., year %in% yrs) else . } %>% 
  {if(!is.na(all(mnths)))   filter(., month %in% mnths) else .} %>% 
  ungroup()

k <- 
  kisten %>%
  mutate(
    year  = lubridate::year(datetime),
    month = lubridate::month(datetime)
  ) %>% 
  ungroup() %>% 
  {if(!is.na(all(vessels))) filter(., vessel %in% vessels) else . } %>% 
  {if(!is.na(all(yrs)))     filter(., year %in% yrs) else . } %>% 
  {if(!is.na(all(mnths)))   filter(., month %in% mnths) else .} %>% 
  mutate(haul = ifelse(is.na(haul), haul2, haul)) %>% 
  mutate(soorten = tolower(soorten)) %>% 
  left_join(soorten, by="soorten") %>% 
  left_join(dplyr::select(h,
                          vessel, trip, haul, division),
            by=c("vessel","trip","haul"))

e <- 
  elog %>%
  ungroup() %>% 
  {if(!is.na(all(vessels))) filter(., vessel %in% vessels) else . } %>% 
  {if(!is.na(all(yrs)))     filter(., year %in% yrs) else . } %>% 
  {if(!is.na(all(mnths)))   filter(., month %in% mnths) else .} %>% 
  ungroup() %>% 
  mutate(species = tolower(species)) %>% 
  mutate(species = ifelse(species == "SQR", "SQC", species)) %>% 
  left_join(dplyr::select(asfis,
                          species, scientificname, englishname, dutchname),
            by = "species") %>% 
  mutate(species = toupper(species))  %>% 
  rename(division = faozone) %>% 
  mutate(division = tolower(division)) %>% 
  ungroup()

# revenue
r <-
  e %>% 
  left_join(price, by=c("year","species")) %>% 
  mutate(revenue = weight * avgprice) %>% 
  drop_na(revenue) %>% 
  ungroup()

# calculate the scaling of plots
xmin <- floor(2 * (min(e$lon, na.rm=TRUE)-0.5))/2;
xmax <- ceiling(2 * (max(e$lon, na.rm=TRUE)+0.5))/2;
ymin <- floor(2 * (min(e$lat, na.rm=TRUE)-0.5))/2;
ymax <- ceiling(2* (max(e$lat, na.rm=TRUE)+0.5))/2

n <- 10

# effort <-
#   e %>% 
#   dplyr::group_by(vessel, year) %>% 
#   dplyr::summarize(ndays = n_distinct(paste(vessel, date))) 

# catch <-
#   e %>% 
#   dplyr::group_by(vessel, year, species, 
#                   scientificname, englishname, dutchname) %>% 
#   dplyr::summarize(weight = sum(weight, na.rm=TRUE)) %>% 
#   dplyr::group_by(vessel, year) %>% 
#   dplyr::mutate(perc = weight/sum(weight, na.rm=TRUE)) %>% 
#   left_join(effort, by=c("vessel", "year")) %>% 
#   mutate(catch_day = weight / ndays)

top <-
  r %>% 
  filter(year %in% (myyear-4):myyear) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarize(
    weight     = sum(weight, na.rm=TRUE),
    revenue    = sum(revenue, na.rm=TRUE)
  ) %>% 
  arrange(desc(revenue)) %>% 
  slice_head(n=n)

load(file=file.path(onedrive,"report","FLYSHOOT surveys v2","rdata", "cgfs.RData")) 
load(file=file.path(onedrive,"report","FLYSHOOT surveys v2","rdata", "ibtsq1.RData")) 
load(file=file.path(onedrive,"report","FLYSHOOT surveys v2","rdata", "ibtsq3.RData")) 

# Coastlines
library(rnaturalearth)
library(sf)
bb <- st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
              crs = 4326)
cl <-
  rnaturalearth::ne_countries(scale = 50, continent = "europe", returnclass = "sf") |>
  st_make_valid() |>
  st_crop(bb) |>
  st_cast("MULTIPOLYGON") |>
  sf::st_coordinates() |>
  as_tibble() |>
  mutate(group = paste(L1, L2, L3)) |>
  select(lon = X, lat = Y, group)

# ======================================================================================
# Data collected by trip
# ======================================================================================

t1 <-
  bind_rows(
    h %>% 
      mutate(week = lubridate::week(date)) %>% 
      filter(year %in% myyear) %>% 
      distinct(vessel, week) %>% 
      mutate(type = "haul", value=1),
    k %>% 
      mutate(week = lubridate::week(datetime)) %>% 
      filter(year %in% myyear) %>% 
      distinct(vessel, week) %>% 
      mutate(type = "marelec", value=1),
    e %>% 
      filter(year %in% myyear) %>% 
      distinct(vessel, week) %>% 
      mutate(type = "elog", value=1),
  )  %>% 
  mutate(type=factor(type, levels=c("haul","marelec","elog")))

p <-
  t1 %>% 
  ggplot(aes(x=type, y=week)) +
  theme_publication() +
  geom_point() +
  scale_y_reverse(breaks=seq(1,20,1)) +
  labs(x="") +
  facet_wrap(~vessel, nrow=1)

png(filename=file.path(figuresdir, "data type by week.png"),
    width=9.5, height=5, units="in", res=300, bg="transparent")
plot(p, fit = "fixed", just = "left")
dev.off()

# ======================================================================================
# Haul sampling overviews
# ======================================================================================

# by month

t1 <-
  h %>% 
    filter(year %in% myyear) %>% 
    group_by(year, month) %>% 
    summarise(
      nvessels      = n_distinct(vessel),
      ntrips        = n_distinct(paste(vessel, trip)),
      ndays         = n_distinct(paste(vessel, trip, date)),
      nhauls        = n_distinct(paste(vessel, trip, haul)),
      nrects        = n_distinct(rect),
      # nlandedcatch  = sum(!is.na(landingweight)),
      ntotalcatch   = sum(!is.na(totalcatch)),
      totalcatch    = as.integer(sum(totalcatch, na.rm=TRUE))
    ) %>% 
  mutate(year=as.character(year), month=as.character(month)) 

t2 <-
  t1 %>% 
  ungroup() %>% 
  summarise(
    ndays = sum(ndays),
    nhauls = sum(nhauls),
    ntotalcatch = sum(ntotalcatch),
    totalcatch = as.integer(sum(totalcatch))
  ) %>% 
  mutate(year = as.character(myyear)) %>% 
  mutate(month = "all")

t <-  bind_rows(t1, t2) 
  
ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "haul overview by month.png"),
    width=9.3, height=2.7, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by division

t1 <-
  h %>% 
    filter(year %in% myyear) %>% 
    group_by(year, division) %>% 
    summarise(
      nvessels      = n_distinct(vessel),
      ntrips        = n_distinct(paste(vessel, trip)),
      ndays         = n_distinct(paste(vessel, trip, date)),
      nhauls        = n_distinct(paste(vessel, trip, haul)),
      nrects        = n_distinct(rect),
      # nlandedcatch  = sum(!is.na(landingweight)),
      ntotalcatch   = sum(!is.na(totalcatch)),
      totalcatch    = as.integer(sum(totalcatch, na.rm=TRUE))
  )  %>% 
  mutate(year = as.character(year))

t2 <-
  t1 %>% 
  ungroup() %>% 
  summarise(
    ndays = sum(ndays),
    nhauls = sum(nhauls),
    ntotalcatch = sum(ntotalcatch),
    totalcatch = as.integer(sum(totalcatch))
  ) %>% 
  mutate(year = as.character(myyear)) %>% 
  mutate(division = "all")

t <-  bind_rows(t1, t2) 

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "haul overview by division.png"),
    width=9.3, height=2.4, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# ======================================================================================
# Marelec sampling overviews
# ======================================================================================

# By month

t1 <-
  k %>% 
  filter(year %in% myyear) %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(year, month) %>% 
  summarise(
    nvessels      = n_distinct(vessel),
    ntrips        = n_distinct(paste(vessel, trip)),
    ndays         = n_distinct(paste(vessel, trip, date)),
    nhauls        = n_distinct(paste(vessel, trip, haul)),
    # nrects        = n_distinct(rect),
    # nlandedcatch  = sum(!is.na(landingweight)),
    nobs          = sum(!is.na(gewicht)),
    totallandings = as.integer(sum(gewicht, na.rm=TRUE))
  ) %>% 
  mutate(year=as.character(year), month=as.character(month)) 

t2 <-
  t1 %>% 
  ungroup() %>% 
  summarise(
    ndays = sum(ndays),
    nhauls = sum(nhauls),
    nobs   = sum(nobs),
    totallandings = as.integer(sum(totallandings))
  ) %>% 
  mutate(year = as.character(myyear)) %>% 
  mutate(month = "all")

t <-  bind_rows(t1, t2) 

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "marelec overview by month.png"),
    width=9.3, height=2.7, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by division

t1 <-
  k %>% 
  filter(year %in% myyear) %>% 
  mutate(date = as.Date(datetime)) %>% 
  mutate(division = ifelse(is.na(division), "NA", division)) %>% 
  group_by(year, division) %>% 
  summarise(
    nvessels      = n_distinct(vessel),
    ntrips        = n_distinct(paste(vessel, trip)),
    ndays         = n_distinct(paste(vessel, trip, date)),
    nhauls        = n_distinct(paste(vessel, trip, haul)),
    # nrects        = n_distinct(rect),
    # nlandedcatch  = sum(!is.na(landingweight)),
    nobs          = sum(!is.na(gewicht)),
    totallandings = as.integer(sum(gewicht, na.rm=TRUE))
  ) %>% 
  mutate(year=as.character(year)) 

t2 <-
  t1 %>% 
  ungroup() %>% 
  summarise(
    ndays = sum(ndays),
    nhauls = sum(nhauls),
    nobs   = sum(nobs),
    totallandings = as.integer(sum(totallandings))
  ) %>% 
  mutate(year = as.character(myyear)) %>% 
  mutate(division = "all")

t <-  bind_rows(t1, t2) 

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "marelec overview by division.png"),
    width=9.3, height=2.7, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by species and month

t <-
  k %>% 
  filter(year %in% myyear) %>% 
  mutate(species = ifelse(species %in% top$species, species, "OTH")) %>% 
  mutate(species = factor(species, levels=c(top$species,"OTH"))) %>% 
  group_by(year, month, species) %>% 
  summarise(catch = as.integer(sum(gewicht, na.rm=TRUE))) %>% 
  reshape2::dcast(year+month ~ species, value.var = "catch", sum, margins=c("month","species"))

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "marelec landings by species and month.png"),
    width=11, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by species and division

t <-
  k %>% 
  filter(year %in% myyear) %>% 
  mutate(species = ifelse(species %in% top$species, species, "OTH")) %>% 
  mutate(species = factor(species, levels=c(top$species,"OTH"))) %>% 
  mutate(division = ifelse(is.na(division), "NA", division)) %>% 
  group_by(year, division, species) %>% 
  summarise(catch = as.integer(sum(gewicht, na.rm=TRUE))) %>% 
  reshape2::dcast(year+division ~ species, value.var = "catch", sum, margins=c("division","species"))

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "marelec landings by species and division.png"),
    width=11, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()


# ======================================================================================
# Elog sampling overviews 
# ======================================================================================

# By month

t1 <-
  e %>% 
  filter(year %in% myyear) %>% 
  group_by(year, month) %>% 
  summarise(
    nvessels      = n_distinct(vessel),
    ntrips        = n_distinct(paste(vessel, trip)),
    ndays         = n_distinct(paste(vessel, trip, date)),
    nhauls        = n_distinct(paste(vessel, trip, haul)),
    nrects        = n_distinct(rect),
    nobs          = sum(!is.na(weight)),
    totallandings = as.integer(sum(weight, na.rm=TRUE))
  ) %>% 
  mutate(year=as.character(year), month=as.character(month)) 

t2 <-
  t1 %>% 
  ungroup() %>% 
  summarise(
    ndays = sum(ndays),
    nhauls = sum(nhauls),
    nobs   = sum(nobs),
    totallandings = as.integer(sum(totallandings))
  ) %>% 
  mutate(year = as.character(myyear)) %>% 
  mutate(month = "all")

t <-  bind_rows(t1, t2) 

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "elog overview by month.png"),
    width=9.3, height=2.7, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by division

t1 <-
  e %>% 
  filter(year %in% myyear) %>% 
  mutate(division = ifelse(is.na(division), "NA", division)) %>% 
  group_by(year, division) %>% 
  summarise(
    nvessels      = n_distinct(vessel),
    ntrips        = n_distinct(paste(vessel, trip)),
    ndays         = n_distinct(paste(vessel, trip, date)),
    nhauls        = n_distinct(paste(vessel, trip, haul)),
    # nrects        = n_distinct(rect),
    # nlandedcatch  = sum(!is.na(landingweight)),
    nobs          = sum(!is.na(weight)),
    totallandings = as.integer(sum(weight, na.rm=TRUE))
  ) %>% 
  mutate(year=as.character(year)) 

t2 <-
  t1 %>% 
  ungroup() %>% 
  summarise(
    ndays = sum(ndays),
    nhauls = sum(nhauls),
    nobs   = sum(nobs),
    totallandings = as.integer(sum(totallandings))
  ) %>% 
  mutate(year = as.character(myyear)) %>% 
  mutate(division = "all")

t <-  bind_rows(t1, t2) 

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "elog overview by division.png"),
    width=9.3, height=2.7, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by species and month

t <-
  e %>% 
  filter(year %in% myyear) %>% 
  mutate(species = ifelse(species %in% top$species, species, "OTH")) %>% 
  mutate(species = factor(species, levels=c(top$species,"OTH"))) %>% 
  group_by(year, month, species) %>% 
  summarise(catch = as.integer(sum(weight, na.rm=TRUE))) %>% 
  reshape2::dcast(year+month ~ species, value.var = "catch", sum, margins=c("month","species"))

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "elog landings by species and month.png"),
    width=11, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# by species and division

t <-
  e %>% 
  filter(year %in% myyear) %>% 
  mutate(species = ifelse(species %in% top$species, species, "OTH")) %>% 
  mutate(species = factor(species, levels=c(top$species,"OTH"))) %>% 
  mutate(division = ifelse(is.na(division), "NA", division)) %>% 
  group_by(year, division, species) %>% 
  summarise(catch = as.integer(sum(weight, na.rm=TRUE))) %>% 
  reshape2::dcast(year+division ~ species, value.var = "catch", sum, margins=c("division","species"))

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "elog landings by species and division.png"),
    width=11, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# plot of elog data combined with haul data

# xlim <- range(e$lon, na.rm=TRUE) 
# ylim <- range(e$lat, na.rm=TRUE)
xlim <- c(-8,4) 
ylim <- c(48,53)

k2 <- k %>% 
  group_by(vessel, trip, haul, year, month) %>% 
  summarise(weight = sum(gewicht, na.rm=TRUE)) %>% 
  distinct() %>% 
  left_join(dplyr::select(h,
                          vessel, trip, haul, lat, lon) %>% distinct(),
            by=c("vessel","trip","haul")) 

p <-
  e %>%
  filter(year == 2023) %>% 
  group_by(year, month, rect, lat, lon) %>%
  summarise(weight = sum(weight, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(weight_class = cut(weight, 
                            scales::trans_breaks("sqrt", function(x) x ^ 2)(c(0, max(weight, na.rm=TRUE))), 
                            dig.lab=10 )) %>% 
  
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap(xlim=xlim , ylim=ylim) +
  geom_polygon(data=cl, aes(x=lon, y=lat, group=group), fill = "grey75") +
  geom_rect(aes(xmin=lon, xmax=lon+1, ymin=lat, ymax=lat+0.5, fill=weight_class)) +
  geom_point(data=k2, aes(size=weight), alpha=0.5, shape=1) +
  scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
  labs(x="", y="", fill="elog landings") +
  guides(size = "none") +
  facet_wrap(~month)

png(filename=file.path(figuresdir, "elog landings and haul positions.png"),
    width=11, height=6, units="in", res=300, bg="transparent")
plot(p, fit = "fixed", just = "left")
dev.off()

# ======================================================================================
# Historic Elog data overviews 
# ======================================================================================

# By source and year

t1 <-
  e %>% 
  filter(year %in% yrs) %>% 
  filter(year %notin% myyear) %>% 
  group_by(year, source) %>% 
  summarise(
    nvessels      = n_distinct(vessel),
    ndays         = n_distinct(paste(vessel, date)),
    nrects        = n_distinct(rect),
    nobs          = sum(!is.na(weight)),
    totallandings = as.integer(sum(weight, na.rm=TRUE))
  ) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", nvessels:totallandings) %>% 
  mutate(variable = factor(variable, levels=c("nvessels","ndays","nrects", "nobs", "totallandings"))) %>% 
  reshape2::dcast(year+source ~ variable, value.var = "data", sum, margins=c("year", "source")) %>% 
  filter(!(year %in% c(2012:2015, 2020:2021) & source=="(all)")) %>% 
  mutate(year = as.character(year), source = as.character(source)) %>% 
  mutate(year = ifelse(year == "(all)", "TOTAL", year)) %>% 
  mutate(nvessels = ifelse(source=="(all)", NA, nvessels)) %>% 
  mutate(nrects   = ifelse(source=="(all)", NA, nrects)) %>% 
  mutate(source   = ifelse(source=="(all)", "", source))


ft <-
  t1 %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::hline(i = c(1,2,3, 4, 6, 7, 9,10, 13,14, 16,17, 18,19, 21,22)) %>% 
  flextable::bold(i=c(1,2,3,4,7,10,14,17, 18, 19, 22), bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t1)), bold=TRUE) %>% 
  flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::autofit()


png(filename=file.path(tablesdir, "historic elog overview by year and source.png"),
    width=9.3, height=7.5, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# ======================================================================================
# Survey data overviews 
# ======================================================================================

# By survey and year
t <-
  bind_rows(cgfs_rbys, ibtsq1_rbys, ibtsq3_rbys) %>%
  mutate(survey = paste(survey, paste0("Q", quarter), sep="_")) %>% 
  distinct(survey, year, id) %>% 
  group_by(survey, year) %>% 
  summarise(nstations = n()) %>% 
  reshape2::dcast(year ~ survey, value.var = "nstations", sum, margins="year")
  # tidyr::pivot_wider(names_from = survey, values_from = nstations)

ft <-
  t %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bold(part="header", bold=TRUE) %>% 
  flextable::hline(i = nrow(t)-1) %>% 
  # flextable::hline(i = c(1,2,3, 4, 6, 7, 9,10, 13,14, 16,17, 18,19, 21,22)) %>% 
  # flextable::bold(i=c(1,2,3,4,7,10,14,17, 18, 19, 22), bold=TRUE) %>% 
  flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
  # flextable::align(j=1:2, align="right", part="all") %>% 
  flextable::align(align="center", part="all") %>% 
  flextable::autofit()


png(filename=file.path(tablesdir, "surveys number of stations by year.png"),
    width=4.6, height=5.5, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "left")
dev.off()

# number of fish by species and survey

for (mysurvey in c("FR-CGFS_Q4", "NS-IBTS_Q1", "NS-IBTS_Q3")) {
  
  t <-
    bind_rows(cgfs_rbys, ibtsq1_rbys, ibtsq3_rbys) %>%
    filter(species %in% top$species[1:10]) %>% 
    mutate(survey = paste(survey, paste0("Q", quarter), sep="_")) %>% 
    filter(survey == mysurvey) %>% 
    mutate(species = factor(species, levels=top$species)) %>% 
    group_by(survey, year, species) %>% 
    summarise(
      N      = as.integer(sum(N, na.rm=TRUE))
    ) %>% 
    reshape2::dcast(survey+year ~ species, value.var = "N", sum, margins="year") %>% 
    mutate(survey = ifelse(is.na(year),"",survey)) 
  
  # tidyr::pivot_wider(names_from = survey, values_from = nstations)
  
  ft <-
    t %>% 
    flextable::flextable() %>%
    flextable::fontsize(size = 12, part = "all") %>% 
    flextable::colformat_num(j=1, big.mark="") %>% 
    flextable::bold(part="header", bold=TRUE) %>% 
    flextable::hline(i = nrow(t)-1) %>% 
    # flextable::hline(i = c(1,2,3, 4, 6, 7, 9,10, 13,14, 16,17, 18,19, 21,22)) %>% 
    # flextable::bold(i=c(1,2,3,4,7,10,14,17, 18, 19, 22), bold=TRUE) %>% 
    flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
    # flextable::align(j=1:2, align="right", part="all") %>% 
    flextable::align(align="center", part="all") %>% 
    flextable::autofit()
  
  
  png(filename=file.path(tablesdir, paste(mysurvey, "number of fish by species and year.png")),
      width=9, height=6, units="in", res=300, bg="transparent")
  plot(ft, fit = "fixed", just = "left")
  dev.off()
  
}

# Percentage of zero stations (by survey)

for (mysurvey in c("FR-CGFS_Q4", "NS-IBTS_Q1", "NS-IBTS_Q3")) {

  t <-
    bind_rows(cgfs_rbys, ibtsq1_rbys, ibtsq3_rbys) %>%
    filter(species %in% top$species[1:10]) %>% 
    mutate(survey = paste(survey, paste0("Q", quarter), sep="_")) %>% 
    filter(survey == mysurvey) %>% 
    mutate(species = factor(species, levels=top$species)) %>% 
    group_by(survey, year, species) %>% 
    summarise(
      nhauls = n_distinct(paste(survey, year, id)),
      nzero  = sum(N==0)
    ) %>% 
    mutate(perc_zero = nzero/nhauls) %>% 
    reshape2::dcast(survey+year ~ species, value.var = "perc_zero", mean, margins="year") %>% 
    mutate(across(c("MUR":"PLE"), ~ scales::percent(., accuracy=1)))
  
  # tidyr::pivot_wider(names_from = survey, values_from = nstations)
  
  ft <-
    t %>% 
    flextable::flextable() %>%
    flextable::fontsize(size = 12, part = "all") %>% 
    flextable::colformat_num(j=1, big.mark="") %>% 
    flextable::bold(part="header", bold=TRUE) %>% 
    flextable::hline(i = nrow(t)-1) %>% 
    # flextable::hline(i = c(1,2,3, 4, 6, 7, 9,10, 13,14, 16,17, 18,19, 21,22)) %>% 
    # flextable::bold(i=c(1,2,3,4,7,10,14,17, 18, 19, 22), bold=TRUE) %>% 
    flextable::bold(i=c(nrow(t)), bold=TRUE) %>% 
    # flextable::align(j=1:2, align="right", part="all") %>% 
    flextable::align(align="center", part="all") %>% 
    flextable::autofit()
  
  
  png(filename=file.path(tablesdir, paste(mysurvey, "number of zero stations by species and year.png")),
      width=9, height=6, units="in", res=300, bg="transparent")
  plot(ft, fit = "fixed", just = "left")
  dev.off()
  
}
