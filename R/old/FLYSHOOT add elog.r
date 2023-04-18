# =======================================================================================
# FLYSHOOT: add elog.r
# 
# Function to read data, check it and add to RData sets
#
# Martin Pastoors
#
# 11/01/2023 First coding
# =========================================================================================

# print settings
options(max.print=999999)
options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

check_data = TRUE
add_data = TRUE
move_data = TRUE
my_data_drive    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata"
my_rdata_drive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"
my_spatial_drive = "C:/DATA/RDATA"
  
# Define function to read, check and add datasets

# add_elog <- function(
#     check_data       = TRUE, 
#     add_data         = TRUE,
#     move_data        = TRUE, 
#     my_data_drive    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
#     my_rdata_drive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
#     my_spatial_drive = "C:/DATA/RDATA") {
  
  # Open relevant packages 
  library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
  library(readxl)        # excel reader from hadley
  library(writexl)       # write excel files
  library(zoo)           # manipulating data
  library(lubridate)     # date functions      (this is part of tidyverse, but needs to be loaded separately)
  library(stringr)       # string manipulation (this is part of tidyverse, but needs to be loaded separately)
  library(sf)            # simple features

  # source("../gisland/r/geo_inside.R")
  source("../prf/r/my utils.R")

  
  # load spatial datasets -------------------------
  
  rect_df <-
    loadRData(file.path(my_spatial_drive, "rect_df.RData")) %>% 
    rename(rect=ICESNAME) %>% 
    group_by(rect) %>% 
    filter(row_number() ==1) %>% 
    dplyr::select(rect, lon=long, lat)
    
  
  # load fish biology datasets -------------------------
  
  asfis <- loadRData(file.path(my_spatial_drive, "asfis.RData"))

  # load fishery datasets -------------------------
  
  load(file.path(my_rdata_drive, "elog.RData"))
  # elog <- data.frame(stringsAsFactors = FALSE)
  
  # ----------------------------------------------------------------------------
  # read the pefa elog data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(my_data_drive, "/_te verwerken"),
    pattern="elog",
    full.names = TRUE)
  
  # i <- 1
  for (i in 1:length(filelist)) {
    
    e  <-
      readxl::read_excel(filelist[i], col_names=TRUE, col_types="text",
                 .name_repair =  ~make.names(., unique = TRUE))  %>% 
      data.frame() %>% 
      lowcase() %>% 
      rename(rect = icesrectangle) %>% 
      rename(vessel = vesselnumber) %>% 
      mutate(vessel = gsub(" ","", vessel)) %>% 
      mutate(
        rect = gsub("000000000", "E9", rect),
        rect = gsub("00000000", "E8", rect),
        rect = gsub("0000000", "E7", rect),
        rect = gsub("000000", "E6", rect),
        rect = gsub("00000", "E5", rect),
        rect = gsub("0000", "E4", rect),
        rect = gsub("000", "E3", rect),
        rect = gsub("00", "E2", rect)
      ) %>% 

      mutate(across (c("boxes", "catchdate"),
                     as.integer)) %>%
      mutate(across (c("departuredate","arrivaldate", "catchdate", "weight"),
                     as.numeric)) %>%
      # mutate(across(c("catchdate"),
      #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
      mutate(across (c("departuredate","arrivaldate"), 
                     ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
      
      mutate(date   = as.Date(catchdate, origin="1899-12-30" , tz="Europe/Amsterdam")) %>% 
      dplyr::select(-catchdate)
    
      mutate(
        year       = lubridate::year(date),
        quarter    = lubridate::quarter(date),
        month      = lubridate::month(date),
        week       = lubridate::week(date),
        yday       = lubridate::yday(date)) %>% 
      
      left_join(rect_df, by="rect") %>% 
      mutate(
        lat = lat + 0.25,
        lon = lon + 0.5
      ) %>% 
      mutate(source="pefa")
    
      # add to haul data
      elog <- 
        elog %>%
        filter(paste0(vessel, tripidentifier) %notin% paste0(e$vessel, e$tripidentifier)) %>%
        bind_rows(e)
      
      # haul <- data.frame(stringsAsFactors = FALSE)
      save(elog,         file = file.path(my_rdata_drive, "elog.RData"))  
      
      
  } # end of pefa elog for loop

  # ----------------------------------------------------------------------------
  # read the m-catch elog data
  # ----------------------------------------------------------------------------
  
  filelist <- list.files(
    path=file.path(my_data_drive, "/_te verwerken"),
    pattern="m-catch",
    full.names = TRUE)
  
  # i <- 1
  for (i in 1:length(filelist)) {
    
    e  <-
      readxl::read_excel(filelist[i], 
                         sheet = "landed catch details table",
                         col_names=TRUE, col_types="text",
                         .name_repair =  ~make.names(., unique = TRUE))  %>% 
      data.frame() %>% 
      lowcase() %>% 
      rename(rect = icesrectangle) %>% 
      
      { if(any(grepl("vesselhullnumber", names(.)))) {
        rename(., vessel = vesselhullnumber)
      } else if (any(grepl("vesselnumber", names(.)))){
        rename(., vessel = vesselnumber)
      } } %>%

      { if(any(grepl("activitydate", names(.)))) {
        rename(., catchdate = activitydate)
      } } %>%

      { if(any(grepl("catchweight", names(.)))) {
        rename(., weight = catchweight)
      } } %>%

      { if(any(grepl("fishspecie", names(.)))) {
        rename(., species = fishspecie)
      } } %>%
      
      rename(economiczone = economicalzone ) %>% 
      rename(freshness = fishfreshness) %>% 
      rename(presentation = fishpresentation) %>% 
      rename(preservation = fishpreservation) %>% 
      
      mutate(vessel = gsub("-","", vessel)) %>% 
      mutate(vessel = gsub("\\.","", vessel)) %>% 
      
      mutate(across (c("catchdate"),
                     as.integer)) %>%
      mutate(across (c("departuredate","arrivaldate", "landingdate", "catchdate", "weight", "lon","lat"),
                     as.numeric)) %>%
      # mutate(across(c("catchdate"),
      #               ~as.POSIXct(. * (60*60*24), origin="1899-12-30", tz="UTC"))) %>% 
      mutate(across (c("departuredate","arrivaldate", "landingdate"), 
                     ~excel_timezone_to_utc(., timezone="Europe/Amsterdam"))) %>% 
      mutate(across (c("faozone"),
                     toupper)) %>%
      
      mutate(date   = as.Date(catchdate, origin="1899-12-30" , tz="Europe/Amsterdam")) %>% 
      dplyr::select(-catchdate) %>% 
    
      mutate(landingdate = as.character(landingdate)) %>% 
      
      mutate(
        year       = lubridate::year(date),
        quarter    = lubridate::quarter(date),
        month      = lubridate::month(date),
        week       = lubridate::week(date),
        yday       = lubridate::yday(date)) %>% 
      
      dplyr::select_if(names(.) %in% names(elog)) %>% 
      mutate(source="m-catch")
      

    # add to haul data
    elog <- 
      elog %>%
      filter(paste0(vessel, tripidentifier) %notin% paste0(e$vessel, e$tripidentifier)) %>%
      bind_rows(e)
    
    # elog <- elog %>% filter(vessel != "Z99")
    elog <- elog %>% mutate(faozone = toupper(faozone))
    
    # elog <- elog %>% mutate(vessel = gsub("0", "", vessel))
    # elog <- 
    #   elog %>% 
    #   dplyr::select(-lat, -lon) %>% 
    #   left_join(rect_df, by="rect") %>% 
    #   mutate(
    #     lat = lat + 0.25,
    #     lon = lon + 0.5
    #   ) 
      
    save(elog,         file = file.path(my_rdata_drive, "elog.RData"))  
    
    
  } # end of pefa elog for loop

# } # End of function




# add_elog     (check_data = TRUE, 
#               add_data = TRUE,
#               move_data = TRUE,
#               my_data_drive    = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata",
#               my_rdata_drive   = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata",
#               my_spatial_drive = "C:/DATA/RDATA")
  

xlim <- c(-6,6); ylim <- c(48,56)

load(file.path(my_rdata_drive, "elog.RData"))

load(file.path(my_spatial_drive, "world_mr_sf.RData"))
load(file.path(my_spatial_drive, "world_hr_sf.RData"))
load(file.path(my_spatial_drive, "rect_lr_sf.RData"))

# Totale vangst per rect
elog %>% 
  group_by(vessel, year, lat, lon, species) %>%
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  filter(species %in% c("CTC", "GUU", "GUR", "MUR", "MAC")) %>% 
  drop_na(lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  
  # ggplot(aes(x=shootlon, y=shootlat)) + 
  ggplot() + 
  
  theme_publication() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12),
        plot.margin      = unit(c(0,0,0,0), "cm")
  ) +  
  

  # geom_sf(data=rect_lr_sf, fill=NA, size=0.5) +
  geom_sf(data=world_mr_sf, fill="cornsilk", size=0.5) +
  
  geom_sf(aes(size = weight, colour = vessel), alpha = 0.5) +
  scale_size(range = c(1,10)) +
  
  # geom_sf(data=rect_sf, fill=NA) +
  coord_sf(xlim=xlim, ylim=ylim)  +
  
  labs(x = NULL, y = NULL, size = "kg") +
  guides(size = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) + 
  
  facet_grid(species~year)

# Gemiddelde vangst per dag per rect
elog %>% 
  group_by(vessel, year, lat, lon, species) %>%
  summarise(
    weight = sum(weight, na.rm=TRUE),
    days   = n_distinct(date)
  ) %>% 
  mutate(
    catchperday = weight/days
  ) %>% 
  filter(species %in% c("CTC", "GUU", "GUR", "MUR", "MAC")) %>% 
  drop_na(lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  
  # ggplot(aes(x=shootlon, y=shootlat)) + 
  ggplot() + 
  
  theme_publication() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12),
        plot.margin      = unit(c(0,0,0,0), "cm")
  ) +  
  
  
  geom_sf(data=rect_lr_sf, fill=NA, size=0.5) +
  geom_sf(data=world_hr_sf, fill="cornsilk", size=0.5) +
  
  geom_sf(aes(size = catchperday, colour = vessel), alpha = 0.5) +
  scale_size(range = c(1,10)) +
  
  # geom_sf(data=rect_sf, fill=NA) +
  coord_sf(xlim=xlim, ylim=ylim)  +
  
  labs(x = NULL, y = NULL, size = "kg/dag") +
  guides(size = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) + 
  
  facet_grid(species~year)

# Gemiddelde vangst per dag per divisie
elog %>% 
  group_by(vessel, year, faozone, species) %>%
  summarise(
    weight = sum(weight, na.rm=TRUE),
    days   = n_distinct(date)
  ) %>% 
  mutate(
    catchperday = weight/days
  ) %>% 
  filter(species %in% c("CTC", "GUU", "GUR", "MUR", "MAC")) %>% 
  filter(faozone %in% c("27.4.B", "27.4.C", "27.7.D", "27.7.E")) %>% 
  
  # ggplot(aes(x=shootlon, y=shootlat)) + 
  ggplot() + 
  
  theme_publication() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12),
        plot.margin      = unit(c(0,0,0,0), "cm")
  ) +  
  
  geom_bar(aes(x=year, y=catchperday, fill=vessel), 
           stat="identity", alpha=0.5, position = position_dodge(preserve="single", width=0.9)) +  
  expand_limits(y=0) +
  labs(x = NULL, y = NULL, size = "kg/dag") +
  guides(size = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) + 
  
  facet_grid(species~faozone, scales="free_y")

# Gemiddelde vangst per dag per divisie (boxplot)
elog %>% 
  
  filter(species %in% c("CTC", "GUU", "GUR", "MUR", "MAC")) %>% 
  filter(faozone %in% c("27.4.B", "27.4.C", "27.7.D", "27.7.E")) %>% 
  
  group_by(year, faozone, species) %>%
  summarise(across(c("weight"), funs(n=n(), mean, sd, se=sd(.)/sqrt(n())))) %>% 

  # ggplot(aes(x=shootlon, y=shootlat)) + 
  ggplot() + 
  
  theme_publication() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text             = element_text(size=12),
        plot.margin      = unit(c(0,0,0,0), "cm")
  ) +  
  
  # geom_violin(aes(x=year, y=weight, group=year) ) +  
  
  # geom_boxplot(aes(x=year, y=weight, group=year) ) +  
  
  # geom_jitter(aes(x=year, y=weight, colour = vessel), alpha=0.5,
  #             position=position_jitter(w=0.1,h=0.0)) +
  geom_point(aes(x=year, y=weight_mean, size=weight_n)) +
  geom_errorbar(aes(x=year,
                    ymin = weight_mean - weight_se, 
                    ymax = weight_mean + weight_se),
                width=0.2, size = 0.2) +  
  scale_size_continuous(range = c(0.1, 1.5)) + 
  scale_x_continuous(breaks=seq(min(elog$year), max(elog$year), 1)) +
  expand_limits(y=0) +
  labs(x = NULL, y = NULL, size = "aantal visdagen", title="vangst per dag") +
  guides(size = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1)) + 
  
  facet_grid(species~faozone, scales="free_y")
