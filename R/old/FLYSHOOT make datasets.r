# =======================================================================================
# FLYSHOOT make datasets.r
# 
# Read data and convert to RData sets
#
# Martin Pastoors
#
# 04/01/2023 First coding
# =========================================================================================

# print settings
options(max.print=999999)
options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

# Open relevant packages 
library(readxl)        # excel reader from hadley
library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(lubridate)     # date functions      (this is part of tidyverse, but needs to be loaded separately)
library(stringr)       # string manipulation (this is part of tidyverse, but needs to be loaded separately)
library(writexl)

library(pander)        # for tables
# library(geosphere)     # for geographic manipulations; needed for e.g. distance calculations
library(sp)            # for spatial data; needed for assigning ICES areas
# library(rfishbase)     # fishbase e.g. length-weight relationships

# Install HAFRO geo package
# devtools::install_github("hafro/geo")

# Install and open Einar's GIS package (for FAO areas etc.) ; loading the package gives an error on dependency ora
# devtools::install_github("einarhjorleifsson/gisland", dependencies = FALSE, build_vignettes=TRUE)
# library(gisland)
source("../gisland/r/geo_inside.R")

# source code with a number of utilities
source("../prf/r/my utils.R")

# set onedrive directory
# onedrive <- get_onedrive()
onedrive <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

# load spatial datasets -------------------------

# load(file.path(onedrive, "rdata/world.df.RData"))
# load(file.path(onedrive, "rdata/fao.df.RData"))
# load(file.path(onedrive, "rdata/fao.RData"))
# load(file.path(onedrive, "rdata/eez.RData"))
# load(file.path(onedrive, "rdata/icesrectangles.RData"))

# load fish biology datasets -------------------------
# asfis                   <- loadRData(file.path(onedrive, "rdata/asfis.RData"))

# ================================================================================
#  1. Reading data and converting to initial dataframes
# ================================================================================

## Vessel data --------------------------------------------------
# vessel  <-
#   
#   read_excel(file.path(onedrive, "excel/PFA vessel data.xlsx"), 
#              sheet = "Vessels",  col_names=TRUE, col_types="text")  %>% 
#   lowcase() %>% 
#   
#   mutate_at(c("year","loa","gt","enginemain","constructionyear"), list(as.numeric)) %>%
#   mutate_at(c("vessel"), list(toupper)) %>% 
#   ungroup()

# save(vessel,           file=file.path(onedrive,"rdata/vessel.RData"))

# source mCatch data (both old and new formatted exports)

fn <- "SCH65 2023_235/SCH65 2023_235 treklijst.xlsx"

# Haul ------------------------------------------------------------------

haul  <-
  read_excel(file.path(onedrive, fn),
             sheet = "Haul",  col_names=TRUE, col_types="text",
             .name_repair =  ~make.names(., unique = TRUE))  %>% 
  data.frame() %>% 
  lowcase() %>% 
  filter(!is.na(vessel), !is.na(date)) %>% 
  
  dplyr::select(
    vessel, trip, haul, date, 
    shoottime     = tijdbeginuitzetten, 
    haultime      = tijdeindehalen, 
    shootlat, shootns, shootlong, shootew,  
    winddirection, 
    windforce     = windforcebft,
    waterdepth, 
    catchheight   = catchhoogtevangstincm, 
    boxtype       = boxvolledigofkleinvk, 
    landingweight = marktwaardigeviskg,
    totalcatch    = totalevangstkgberekend, 
    bycatchperc   = percentagebijvangst, 
    gear, meshsize, vertopening, 
    cablelength   = cablelengthm, 
    cablethickness= cablethicknessmm,
    lengthgroundrope= lengthgroundropem,
    escapepanel   = escapepanelyn, 
    timezone
  )  %>%  
  
  mutate(across (c("vessel"), 
                 toupper)) %>% 
  mutate(across (c("haul", "meshsize","date", "windforce", "waterdepth",
                   "catchheight"), 
                 as.integer)) %>% 
  mutate(across (c("waterdepth","vertopening", "landingweight"), 
                 as.numeric)) %>%
  mutate(across (c("shoottime","haultime"), 
                 ~calculate_time_from_string(.))) %>% 
  
  mutate(timezone = case_when(
    timezone == "CET"   ~ "Europe/Amsterdam", 
    timezone == "UTC+1" ~ "Europe/Amsterdam", 
    timezone == "UTC-5" ~ "America/Santiago",
    is.na(timezone)     ~ "UTC",    
    TRUE                ~ timezone) ) %>% 
  
  mutate(date   = as.Date(date, origin="1899-12-30" , tz=unique(timezone))) %>% 

  mutate( 
    shoottime = force_timezone_to_utc(t=date+shoottime, timezone=unique(timezone)),
    haultime  = force_timezone_to_utc(t=date+haultime, timezone=unique(timezone)),
  ) %>%
  
  mutate(
    year       = lubridate::year(date),
    quarter    = lubridate::quarter(date),
    month      = lubridate::month(date),
    week       = lubridate::week(date),
    yday       = lubridate::yday(date)) %>%
  dplyr::select(-timezone) %>%

  mutate(
    lon = calculate_position_from_strings(shootlat, shootns, shootlong, shootew)$lon,
    lat = calculate_position_from_strings(shootlat, shootns, shootlong, shootew)$lat
  )  %>% 

  # calculate haul duration: haul_time-shoot_time*24 
  mutate(duration   = as.numeric(as.duration(shoottime %--% haultime))/3600 ) %>% 
  ungroup()
  

# MAY BE

# store rows without shoot location
# tmp <- haul %>% filter(is.na(lon) | is.na(lat))

# haul1 <-
#   haul1_ %>% 
#   filter(!is.na(shootlon) & !is.na(shootlat)) %>% 
#   
#   # add default area allocations from fao and ices rectangles
#   mutate(area       = geo_inside(lon=shootlon, lat=shootlat, map=fao[fao@data$F_LEVEL=="MAJOR",], variable="F_AREA")) %>% 
#   mutate(subarea    = geo_inside(lon=shootlon, lat=shootlat, map=fao[fao@data$F_LEVEL=="SUBAREA",], variable="F_SUBAREA")) %>%
#   mutate(division   = geo_inside(lon=shootlon, lat=shootlat, map=fao[fao@data$F_LEVEL=="DIVISION",], variable="F_DIVISION")) %>%
#   mutate(subdivision= geo_inside(lon=shootlon, lat=shootlat, map=fao[fao@data$F_LEVEL=="SUBDIVISION",], variable="F_SUBDIVIS")) %>%
#   mutate(rect       = geo_inside(lon=shootlon, lat=shootlat, map=icesrectangles, variable="ICESNAME")) %>%
#   mutate(eez        = geo_inside(lon=shootlon, lat=shootlat, map=eez, variable="Territory1")) %>%
#     
#   # calculate distance between shoot and haul positions
#   mutate(distance = geosphere::distHaversine(cbind(shootlon, shootlat), cbind(haullon, haullat))/1000 ) %>%
#   
#   # mutate(distance = geosphere::distGeo(matrix(c(shootlon, shootlat), ncol=2, byrow=FALSE),
#   #                                 matrix(c(haullon, haullat) , ncol=2, byrow=FALSE) )/1000 ) %>%
#   
#   # bind the rows without shoot location
#   bind_rows(tmp) %>% 
#   
#   # sort
#   arrange(year, vessel, trip, haul) %>% 
#   ungroup()

# Read Marelec data
filelist <- list.files(
              path=file.path(onedrive, "SCH65 2023_235/marelec"),
              pattern="xlsx",
              full.names = TRUE)
  
t <- data.frame(stringsAsFactors = FALSE)

for (i in 1:length(filelist)) {
  
  tmp <- readxl::read_xlsx(
    path = filelist[i],
    range = "A1",
    col_names = FALSE,
    .name_repair = "unique"
  ) %>% as.character()

  mytrip <-
    stringr::word(tmp, 2, sep=" ") %>%
    str_extract_all(.,"\\(?[0-9]+\\)?") %>%
    unlist() %>% 
    paste0("2023", .)

  myhaul <-
    stringr::word(tmp, 4, sep=" ") %>%
    str_extract_all(.,"\\(?[0-9]+\\)?") %>%
    unlist() %>%
    as.integer() 

  t <-
    t %>%
    bind_rows(
      readxl::read_xlsx(
        path = filelist[i],
        range = "A8:C100") %>%
      lowcase() %>%
      filter(!is.na(soorten)) %>%
      filter(!grepl("Totaal", soorten)) %>%
      filter(!grepl("Einde dag", soorten)) %>%
        mutate(
        trip = mytrip,
        haul = myhaul)
    )
}

marelec <-
  t %>% 
  mutate(haul = ifelse(haul >= 25, haul+1, haul))

# writexl::write_xlsx(marelec, "marelec export.xlsx")


comb <-
  marelec %>% 
  left_join(haul, by=c("trip", "haul")) %>% 
  group_by(vessel, trip, date, haul, lon, lat, soorten) %>% 
  summarise(landings = sum(totaalgewicht, na.rm=TRUE))


# spatial stuff; MOVE TO SF now!!

create_spatial_df_from_sp <- function(sp) {
  
  if(class(world) != "SpatialPolygonsDataFrame") stop("sp must be a SpatialPolygonsDataFrame")
  
  sp@data$id <- rownames(sp@data)
  sp.points  <- broom::tidy(sp)
  df         <- left_join(sp.points, sp@data, by="id")
  return(df)
}

world <-
  rnaturalearth::ne_countries (returnclass = "sp", scale="medium") %>% 
  sp::spTransform(., CRS("+init=epsg:4326")) %>% 
  rgeos::gBuffer(., byid=TRUE, width=0) %>% 
  create_spatial_df_from_sp()

# plot by haul
haul %>% 
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap(xlim=c(-1.5,1.5), ylim=c(49,50.5)) +
  geom_path(data=world, aes(x=long, group=group), fill="cornsilk") +
  geom_point(aes(fill=as.character(date), 
                 size=landingweight),
             shape=21, colour="black", alpha=0.5) +
  geom_path() +
  scale_size(range = c(1, 10))

# plot by haul and species
comb %>% 
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap(xlim=c(-1.5,1.5), ylim=c(49,50.5)) +
  geom_path(data=world, aes(x=long, group=group), fill="cornsilk") +
  geom_point(data=haul, colour="gray", size=0.5) +
  geom_point(aes(size=landings),
             shape=21, fill="red", alpha=0.5) +
  # geom_point(aes(fill=as.character(date), 
  #                size=landings),
  #            shape=21, colour="black", alpha=0.5) +
  scale_size(range = c(1, 10)) +
  facet_wrap(~soorten, ncol=6)


haul %>% 
  ggplot(aes(x=haul, y=landingweight)) +
  theme_publication() +
  geom_bar(aes(fill=as.character(date)), stat="identity")

comb %>% 
  ggplot(aes(x=haul, y=landings)) +
  theme_publication() +
  geom_bar(aes(fill=soorten), stat="identity")
