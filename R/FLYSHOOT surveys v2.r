# =======================================================================================================
# FLYSHOOT surveys
# 
# 11/01/2023 first coding
# 16/03/2023 full GLM modelling of top 15 species in the catch
# 17/03/2023 combining survey and cpue indices
# 30/03/2023 only survey info for top species
# 26/04/2023 included all steps in generating the survey results
# =======================================================================================================

knitr::opts_chunk$set(echo = FALSE,	message = FALSE,	warning = FALSE,	comment = "",	crop = TRUE )
knitr::opts_chunk$set(fig.width=10) 

options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

recalculatedata <- FALSE
recalculatemaps <- TRUE              # recalculate catch maps (takes relatively long)

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


library(tidydatras)

# Source all the utils
source("../prf/R/my utils.r")
source("../mptools/R/get_onedrive.r")
source("R/datras_data.R")
source("R/datras_bubble.R")
source("R/datras_index.R")
source("R/datras_length.R")
source("R/datras_zero.R")
source("../tidydatras/R/calc_datras.R")
# source("../R/ices_scales.R")

# set onedrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")
# datrasdir  <- "../../tidydatras/vignettes/articles"

reportdir <- file.path(onedrive,"report", "FLYSHOOT surveys v2")
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

top      <- 
  readr::read_rds(file=file.path(onedrive,"rdata", "top.rds")) %>%  
  lowcase() %>% 
  mutate(PANEL=row_number())


# Coastlines
library(rnaturalearth)
library(sf)
bb <- st_bbox(c(xmin = -40, ymin = 27, xmax = 40, ymax = 70),
              crs = 4326)
cl <-
  rnaturalearth::ne_countries(scale = 50, continent = "europe", returnclass = "sf") |>
  st_make_valid() |>
  st_crop(bb) |>
  st_coordinates() |>
  as_tibble() |>
  mutate(group = paste(L1, L2, L3)) |>
  select(lon = X, lat = Y, group)

# ======================================================================================
# metric
# ======================================================================================

# my_metric <- "cpue_number_per_km2"
# my_lab    <- "N/km2"

my_metric <- "cpue_number_per_hour"
my_lab    <- "N/hour"


# ======================================================================================
# Top species
# ======================================================================================

png(filename=file.path(figuresdir, paste0("top.png")),
    width=7, height=4, units="in", res=300)

top %>% 
  mutate(species = factor(species, levels=top$species)) %>% 
  pivot_longer(names_to = "variable", values_to = "data", weight:value) %>% 
  ggplot(aes(x=species, y=data)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(x="",y="", title="top species in value and weight (2018-2022)") +
  facet_wrap(~variable, scales="free_y")

dev.off()

ft <-
  top %>% 
  dplyr::select(name = englishname, 
                code = species,
                latin=scientificname,
                dutch=dutchname,
                value) %>% 
  filter(row_number() <= 8) %>% 
  mutate(across(c("value"), as.integer)) %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  # # flextable::colformat_num(j=1, big.mark="") %>% 
  # flextable::bg(j = c('advice', 'adv_c'), bg = myColors[1], part = "body") %>% 
  # flextable::bg(j = c('advice', 'adv_c'), bg = myColors[2], part = "header") %>% 
  # flextable::bg(j = c('tac', 'tac_c'), bg = myColors[3], part = "body") %>% 
  # flextable::bg(j = c('tac', 'tac_c'), bg = myColors[4], part = "header") %>% 
  # flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[7], part = "body") %>% 
  # flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[8], part = "header") %>% 
  # flextable::set_header_labels(values = list(year   = "YEAR",
  #                                            advice = "ADVICE",
  #                                            adv_c  = paste0("\u394", "adv"),
  #                                            tac    = "TAC",
#                                            tac_c  = paste0("\u394", "tac"),
#                                            uniq   = "Uni Quota",
#                                            uni_c  = paste0("\u394", "uni"))) %>% 
# flextable::align(align = "center", part = "all") %>% 
# flextable::height(height = .8) 
flextable::autofit()

png(filename=file.path(tablesdir, "top species.png"),
    width=8.95, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()



# ======================================================================================
# CGFS
# ======================================================================================

if(recalculatedata) {

  cgfs_hh <- 
    datras_hh_from_file(survey = "FR-CGFS", quarters = 4, years = 2005:2022, 
                     folder="C:/DATA/DATRAS/raw") 
  
  # all survey hauls
  cgfs_hauls <-
    cgfs_hh %>% 
    ungroup() %>% 
    rename(lon=shootlong, lat=shootlat) %>% 
    distinct(survey, year, id, lat, lon)
  
  cgfs_raw <- 
    datras_from_file(survey = "FR-CGFS", quarters = 4, years = 2005:2022, 
                     folder="C:/DATA/DATRAS/raw", metric=my_metric) %>% 
    filter(species %in% top$species) %>% 
    dplyr::select(
      c("id", "survey", "quarter","country","ship", "gear", "sweeplngt", "haulno", "year", "month", "day", "timeshot",
        "hauldur", "daynight","shootlat","shootlong","statrec", "depth", "haulval", "datatype", "netopening", "distance",
        "warplngt","warpdia", "doorspread","wingspread", "groundspeed", "speedwater", "winddir", "windspeed", "surtemp", 
        "bottemp", "codendmesh", "vessel", "species", "latin", "english_name", "length","n", "metric")
    ) %>% 
    arrange(survey, quarter, year, id, species, length)
  
  cgfs_rbys <-
    cgfs_raw %>% 
    
    
    mutate(B = n * 0.001 * length^3) |> 
    
    # sum over lengths
    group_by(survey, quarter, vessel, id, year, species, metric) |>
    summarise(
      B = sum(B), 
      N = sum(n),
      meanlength = weighted.mean(length, n), 
      .groups = "drop") |> 
    
    
    # Winsorize by species
    group_by(survey, species) %>%
    mutate(N = DescTools::Winsorize(N, probs=c(0, 0.98), na.rm=TRUE)) %>%
    
    # filter(species == "MAC") %>% 
    # View()

    # add zero hauls by species
    group_by(survey, quarter, vessel, year, metric) %>% 
    tidyr::complete(id, species, fill=list(N=0, B=0)) %>% 
    mutate(type = ifelse(N==0, "zero","value")) %>% 
  
    # add species names and haul attributes
    ungroup() %>%
    left_join(dplyr::select(top, -weight), by="species") %>% 
    left_join(cgfs_hauls, by=c("survey","year", "id")) %>% 
    arrange(vessel, year, quarter, id, species) 
  
  # cgfs_raw %>% group_by(species) %>% filter(species=="HOM") %>% summarise(N=sum(n)) %>% View() 
  # cgfs_rbys %>% group_by(species) %>% filter(species=="HOM") %>% summarise(N=sum(N)) %>% View()
  
  p <-
    cgfs_rbys |> 
    mutate(species = factor(species, levels=top$species)) %>% 
    # filter(N > 0) %>% 
    # filter(species=="SQR") %>% 
    
    ggplot(aes(year, N, colour = vessel)) +
    theme_bw() +
    stat_summary(fun.data = "mean_cl_boot") +
    expand_limits(y = 0) +
    scale_colour_brewer(palette = "Set1") +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    facet_wrap(~species)
  
  cgfs_index <-
    ggplot_build(p)$data[[1]] %>% 
    mutate(PANEL=as.integer(as.character(PANEL))) %>% 
    left_join(top) %>% 
    rename(year=x, est=y, lwr=ymin, upr=ymax) %>% 
    # calculate mean from 2015 onwards
    group_by(englishspecies) %>% 
    mutate(z = mean(est[year>= 2015], na.rm=TRUE)) %>% 
    mutate(
      std_est = est/z-1,
      std_upr = upr/z-1,
      std_lwr = lwr/z-1
    ) %>% 
    
    mutate(vessel = ifelse(year< 2015, "Gwen Drez", "Thalassa")) %>% 
    mutate(survey = unique(cgfs_raw$survey)) %>% 
    mutate(quarter = unique(cgfs_raw$quarter)) %>% 
    
    # add missing years 
    # group_by(survey, quarter) %>% 
    ungroup() %>% 
    tidyr::complete(survey, quarter, vessel, year, nesting(species, scientificname, englishname, dutchname, englishspecies))  
    
  
  # cgfs_index %>% filter(species=="SQR")
  
  
  # length compositions by year
  cgfs_rbyl <-
    cgfs_raw %>% 
    group_by(survey, quarter, year, species, length, metric) %>% 
    reframe(N = sum(n)) %>% 
    left_join(cgfs_hh  %>%  count(survey, year, quarter, name = "nhauls")) %>% 
    mutate(n = N / nhauls)  %>% 
    dplyr::select(-nhauls) %>% 
    left_join(dplyr::select(top,
                            -weight, -value, -PANEL), 
              by="species") %>% 
    group_by(survey, quarter, species)  
    # mutate(
    #   N = DescTools::Winsorize(N, probs=c(0, 0.98), na.rm=TRUE),
    #   n = DescTools::Winsorize(n, probs=c(0, 0.98), na.rm=TRUE)
    # )
    
  
  cgfs_rbl <-
    cgfs_rbyl %>% 
    group_by(survey, quarter, species, length, metric) %>% 
    reframe(n = mean(n)) %>% 
    left_join(dplyr::select(top,
                            -weight, -value), 
              by="species")

    
  save(cgfs_hh, cgfs_hauls, cgfs_raw, cgfs_rbys, cgfs_index, cgfs_rbl, cgfs_rbyl,
       file=file.path(rdatadir, "cgfs.RData"))
  
} else {

    load(file=file.path(rdatadir, "cgfs.RData"))
  
}


# generate maps?

if(recalculatemaps) {
  
  # for report
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("FR-CGFS bubble ",sp," ",my_metric, ".png")),
         width=7, height=9.8, units="in", res=300)
    
    print(datras_bubble(data=cgfs_rbys, SUR="FR-CGFS", SPECIES=sp, 
                  YRS=2005:2022, var="N", cl=cl, lab=my_lab, NROW=5,
                  legend.position=c(0.9, 0.1)))
    dev.off()
  }
  
  # for presentations
  for (sp in top$species[1:10]) {
    png(filename=file.path(presentationdir, paste0("FR-CGFS bubble ",sp," ",my_metric, ".png")),
         width=12.5, height=5.5, units="in", res=300, bg="transparent")
    
    print(datras_bubble(data=cgfs_rbys, SUR="FR-CGFS", SPECIES=sp, 
                  YRS=2005:2022, var="N", cl=cl, lab=my_lab, NROW=3,
                  legend.position="right"))
    dev.off()
  }
  
  # length plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("FR-CGFS length ",sp," ",my_metric, ".png")),
         width=7, height=9.8, units="in", res=300)

    print(datras_length(cgfs_rbl, cgfs_rbyl, SUR="FR-CGFS", SPECIES=sp, var=n,
                        lab = "Mean number per haul and lengthclass", NROW=5))
    dev.off()
  }
  
  
  # index plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("FR-CGFS index ",sp," ",my_metric, ".png")),
         width=7, height=3, units="in", res=300)
    
    print(
      datras_index(cgfs_index, SPECIES=sp, YRS=2005:2022, lab = "N/hour", legend.position="right", byvessel=TRUE)
    )
    
    dev.off()
  }
  
  # zero plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("FR-CGFS zero ",sp," ",my_metric, ".png")),
        width=7, height=3, units="in", res=300)
    
    print(
      datras_zero(cgfs_rbys, SPECIES=sp, YRS=2005:2022, lab = "perc. zero hauls", 
                   legend.position="right", byvessel=TRUE)
    )
    
    dev.off()
  }
  
}


# ======================================================================================
# IBTS Q1
# ======================================================================================

my_divisions <- c("27.4.b","27.4.c")

if(recalculatedata) {

  ibtsq1_hh <- 
    datras_hh_from_file(survey = "NS-IBTS", quarters = 1, years = 2005:2022, 
                     folder="C:/DATA/DATRAS/raw") 
  
  ibtsq1_hauls <-
    ibtsq1_hh %>% 
    ungroup() %>% 
    rename(lon=shootlong, lat=shootlat) %>% 
    distinct(survey, year, id, lat, lon)
  
  # calculate FAO areas  
  ibtsq1_fao <- 
    ibtsq1_hauls %>%
    drop_na(lat, lon) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = TRUE) %>% 
    sf::st_join(., fao_sf, join = st_within) %>% 
    sf::st_drop_geometry() %>% 
    filter(F_LEVEL == "DIVISION") %>% 
    dplyr::select(survey, year, id, division=F_CODE) 
  
  ibtsq1_hauls <- ibtsq1_hauls %>% left_join(ibtsq1_fao)
  
  ibtsq1_raw <- 
    datras_from_file(survey = "NS-IBTS", quarters = 1, years = 2005:2022, 
                     folder="C:/DATA/DATRAS/raw", metric=my_metric) %>% 
    filter(species %in% top$species) %>% 
    dplyr::select(
      c("id", "survey", "quarter","country","ship", "gear", "sweeplngt", "haulno", "year", "month", "day", "timeshot",
        "hauldur", "daynight","shootlat","shootlong","statrec", "depth", "haulval", "datatype", "netopening", "distance",
        "warplngt","warpdia", "doorspread","wingspread", "groundspeed", "speedwater", "winddir", "windspeed", "surtemp", 
        "bottemp", "codendmesh", "vessel", "species", "latin", "english_name", "length","n", "metric")
    ) 
    
  ibtsq1_rbys <-
    ibtsq1_raw %>% 
    
    left_join(ibtsq1_fao) %>% 
    filter(division %in% my_divisions) %>% 
    
    mutate(B = n * 0.001 * length^3) |> 
    group_by(survey, quarter, id, year, species, metric) |>
    summarise(
      B = sum(B), 
      N = sum(n),
      meanlength = weighted.mean(length, n), 
      .groups = "drop") |> 
    
    # Winsorize by species
    group_by(survey, species) %>%
    mutate(N = DescTools::Winsorize(N, probs=c(0, 0.98), na.rm=TRUE)) %>% 
  
   # add zero hauls by species
    group_by(survey, quarter, year, metric) %>% 
    tidyr::complete(id, species, fill=list(N=0, B=0)) %>% 
    mutate(type = ifelse(N==0, "zero","value")) %>% 
      
    # add species names and haul attributes
    ungroup() %>%
    left_join(dplyr::select(top, -weight), by="species") %>% 
    left_join(ibtsq1_hauls, by=c("survey","year", "id")) %>% 
    arrange(survey, year, quarter, id, species) 
    
  p <-
    ibtsq1_rbys |> 
    mutate(species = factor(species, levels=top$species)) %>% 
    
    ggplot(aes(year, N)) +
    theme_bw() +
    stat_summary(fun.data = "mean_cl_boot") +
    expand_limits(y = 0) +
    scale_colour_brewer(palette = "Set1") +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    facet_wrap(~species)
  
  ibtsq1_index <-
    ggplot_build(p)$data[[1]] %>% 
    mutate(PANEL=as.integer(as.character(PANEL))) %>% 
    left_join(top) %>% 
    rename(year=x, est=y, lwr=ymin, upr=ymax) %>% 
    # calculate mean from 2015 onwards
    group_by(englishspecies) %>% 
    mutate(z = mean(est[year>= 2015], na.rm=TRUE)) %>% 
    mutate(
      std_est = est/z-1,
      std_upr = upr/z-1,
      std_lwr = lwr/z-1
    ) %>% 
    mutate(survey = unique(ibtsq1_raw$survey)) %>% 
    mutate(quarter = unique(ibtsq1_raw$quarter)) %>% 
    
    # add missing years 
    ungroup() %>% 
    tidyr::complete(survey, quarter, year, nesting(species, scientificname, englishname, dutchname, englishspecies))  

  # length compositions by year
  ibtsq1_rbyl <-
    ibtsq1_raw %>% 
    group_by(survey, quarter, year, species, length, metric) %>% 
    reframe(N = sum(n)) %>% 
    left_join(ibtsq1_hh  %>%  count(survey, year, quarter, name = "nhauls")) %>% 
    mutate(n = N / nhauls)  %>% 
    dplyr::select(-nhauls) %>% 
    left_join(dplyr::select(top,
                            -weight, -value, -PANEL), 
              by="species") %>% 
    group_by(survey, quarter, species)  
    # mutate(
    #   N = DescTools::Winsorize(N, probs=c(0, 0.98), na.rm=TRUE),
    #   n = DescTools::Winsorize(n, probs=c(0, 0.98), na.rm=TRUE)
    # )
    
  
  ibtsq1_rbl <-
    ibtsq1_rbyl %>% 
    group_by(survey, quarter, species, length, metric) %>% 
    reframe(n = mean(n)) %>% 
    left_join(dplyr::select(top,
                            -weight, -value), 
              by="species")

  save(ibtsq1_hh, ibtsq1_hauls, ibtsq1_raw, ibtsq1_rbys, ibtsq1_index, ibtsq1_rbyl, ibtsq1_rbl,
       file=file.path(rdatadir, "ibtsq1.RData"))
  
} else {

    load(file=file.path(rdatadir, "ibtsq1.RData"))
}


# generate maps?

if(recalculatemaps) {
  # for report
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q1 bubble ",sp," ",my_metric,".png")),
         width=7, height=9.8, units="in", res=300)
    
    print(datras_bubble(data=ibtsq1_rbys, SUR="NS-IBTS", SPECIES=sp, 
                  YRS=2005:2022, var="N", cl=cl, lab=my_lab, NROW=5,
                  legend.position=c(0.9, 0.1)))
    dev.off()
  }
  
  # for presentations
  for (sp in top$species[1:10]) {
    png(filename=file.path(presentationdir, paste0("IBTS-Q1 bubble ",sp," ",my_metric,".png")),
         width=12.5, height=5.5, units="in", res=300, bg="transparent")
    
    print(datras_bubble(data=ibtsq1_rbys, SUR="NS-IBTS", SPECIES=sp, 
                  YRS=2005:2022, var="N", cl=cl, lab=my_lab, NROW=3,
                  legend.position="right"))
    dev.off()
  }
  
    # length plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q1 length ",sp," ",my_metric, ".png")),
         width=7, height=9.8, units="in", res=300)

    print(datras_length(ibtsq1_rbl, ibtsq1_rbyl, SUR="NS-IBTS", SPECIES=sp, var=n,
                        lab = "Mean number per haul and lengthclass", NROW=5))
    dev.off()
  }
  
  
  # index plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q1 index ",sp," ",my_metric, ".png")),
         width=7, height=3, units="in", res=300)
    
    print(
      datras_index(ibtsq1_index, SPECIES=sp, YRS=2005:2022, lab = "N/hour", legend.position="right", byvessel=FALSE)
    )
    
    dev.off()
  }
  
  # zero plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q1 zero ",sp," ",my_metric, ".png")),
        width=7, height=3, units="in", res=300)
    
    print(
      datras_zero(ibtsq1_rbys, SPECIES=sp, YRS=2005:2022, lab = "perc. zero hauls", 
                  legend.position="right", byvessel=FALSE)
    )
    
    dev.off()
  }
  
}

# ======================================================================================
# IBTS Q3
# ======================================================================================

my_divisions <- c("27.4.b","27.4.c")

if(recalculatedata) {

  ibtsq3_hh <- 
    datras_hh_from_file(survey = "NS-IBTS", quarters = 3, years = 2005:2022, 
                     folder="C:/DATA/DATRAS/raw") 
  
  ibtsq3_hauls <-
    ibtsq3_hh %>% 
    ungroup() %>% 
    rename(lon=shootlong, lat=shootlat) %>% 
    distinct(survey, year, id, lat, lon)
  
  # calculate FAO areas  
  ibtsq3_fao <- 
    ibtsq3_hauls %>%
    drop_na(lat, lon) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = TRUE) %>% 
    sf::st_join(., fao_sf, join = st_within) %>% 
    sf::st_drop_geometry() %>% 
    filter(F_LEVEL == "DIVISION") %>% 
    dplyr::select(survey, year, id, division=F_CODE) 
  
  ibtsq3_hauls <- ibtsq3_hauls %>% left_join(ibtsq3_fao)
  
  ibtsq3_raw <- 
    datras_from_file(survey = "NS-IBTS", quarters = 3, years = 2005:2022, 
                     folder="C:/DATA/DATRAS/raw", metric=my_metric) %>% 
    filter(species %in% top$species) %>% 
    dplyr::select(
      c("id", "survey", "quarter","country","ship", "gear", "sweeplngt", "haulno", "year", "month", "day", "timeshot",
        "hauldur", "daynight","shootlat","shootlong","statrec", "depth", "haulval", "datatype", "netopening", "distance",
        "warplngt","warpdia", "doorspread","wingspread", "groundspeed", "speedwater", "winddir", "windspeed", "surtemp", 
        "bottemp", "codendmesh", "vessel", "species", "latin", "english_name", "length","n", "metric")
    ) 
    
  
  ibtsq3_rbys <-
    ibtsq3_raw %>% 
    
    left_join(ibtsq3_fao) %>% 
    filter(division %in% my_divisions) %>% 
    
    mutate(B = n * 0.001 * length^3) |> 
    group_by(survey, quarter, id, year, species, metric) |>
    summarise(
      B = sum(B), 
      N = sum(n),
      meanlength = weighted.mean(length, n), 
      .groups = "drop") |> 
    
    # Winsorize by species
    group_by(survey, species) %>%
    mutate(N = DescTools::Winsorize(N, probs=c(0, 0.98), na.rm=TRUE)) %>% 
  
   # add zero hauls by species
    group_by(survey, quarter, year, metric) %>% 
    tidyr::complete(id, species, fill=list(N=0, B=0)) %>% 
    mutate(type = ifelse(N==0, "zero","value")) %>% 
      
    # add species names and haul attributes
    ungroup() %>%
    left_join(dplyr::select(top, -weight), by="species") %>% 
    left_join(ibtsq3_hauls, by=c("survey","year", "id")) %>% 
    arrange(survey, year, quarter, id, species) 
    
  p <-
    ibtsq3_rbys |> 
    mutate(species = factor(species, levels=top$species)) %>% 
    
    ggplot(aes(year, N)) +
    theme_bw() +
    stat_summary(fun.data = "mean_cl_boot") +
    expand_limits(y = 0) +
    scale_colour_brewer(palette = "Set1") +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    facet_wrap(~species)
  
  ibtsq3_index <-
    ggplot_build(p)$data[[1]] %>% 
    mutate(PANEL=as.integer(as.character(PANEL))) %>% 
    left_join(top) %>% 
    rename(year=x, est=y, lwr=ymin, upr=ymax) %>% 
    # calculate mean from 2015 onwards
    group_by(englishspecies) %>% 
    mutate(z = mean(est[year>= 2015], na.rm=TRUE)) %>% 
    mutate(
      std_est = est/z-1,
      std_upr = upr/z-1,
      std_lwr = lwr/z-1
    ) %>% 
    mutate(survey = unique(ibtsq3_raw$survey)) %>% 
    mutate(quarter = unique(ibtsq3_raw$quarter)) %>% 
    
    # add missing years 
    ungroup() %>% 
    tidyr::complete(survey, quarter, year, nesting(species, scientificname, englishname, dutchname, englishspecies))  

  # length compositions by year
  ibtsq3_rbyl <-
    ibtsq3_raw %>% 
    group_by(survey, quarter, year, species, length, metric) %>% 
    reframe(N = sum(n)) %>% 
    left_join(ibtsq3_hh  %>%  count(survey, year, quarter, name = "nhauls")) %>% 
    mutate(n = N / nhauls)  %>% 
    dplyr::select(-nhauls) %>% 
    left_join(dplyr::select(top,
                            -weight, -value, -PANEL), 
              by="species") %>% 
    group_by(survey, quarter, species) 
    # mutate(
    #   N = DescTools::Winsorize(N, probs=c(0, 0.98), na.rm=TRUE),
    #   n = DescTools::Winsorize(n, probs=c(0, 0.98), na.rm=TRUE)
    # )
    
  
  ibtsq3_rbl <-
    ibtsq3_rbyl %>% 
    group_by(survey, quarter, species, length, metric) %>% 
    reframe(n = mean(n)) %>% 
    left_join(dplyr::select(top,
                            -weight, -value), 
              by="species")

  save(ibtsq3_hh, ibtsq3_hauls, ibtsq3_raw, ibtsq3_rbys, ibtsq3_index, ibtsq3_rbyl, ibtsq3_rbl,
       file=file.path(rdatadir, "ibtsq3.RData"))
  
} else {

    load(file=file.path(rdatadir, "ibtsq3.RData"))
}


# generate maps?

if(recalculatemaps) {

  # for report
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q3 bubble ",sp," ",my_metric,".png")),
         width=7, height=9.8, units="in", res=300)
    
    print(datras_bubble(data=ibtsq3_rbys, SUR="NS-IBTS", SPECIES=sp, 
                  YRS=2005:2022, var="N", cl=cl, lab=my_lab, NROW=5,
                  legend.position=c(0.9, 0.1)))
    dev.off()
  }
  
  # for presentations
  for (sp in top$species[1:10]) {
    png(filename=file.path(presentationdir, paste0("IBTS-Q3 bubble ",sp," ",my_metric,".png")),
         width=12.5, height=5.5, units="in", res=300, bg="transparent")
    
    print(datras_bubble(data=ibtsq3_rbys, SUR="NS-IBTS", SPECIES=sp, 
                  YRS=2005:2022, var="N", cl=cl, lab=my_lab, NROW=3,
                  legend.position="right"))
    dev.off()
  }

      # length plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q3 length ",sp," ",my_metric, ".png")),
         width=7, height=9.8, units="in", res=300)

    print(datras_length(ibtsq3_rbl, ibtsq3_rbyl, SUR="NS-IBTS", SPECIES=sp, var=n,
                        lab = "Mean number per haul and lengthclass", NROW=5))
    dev.off()
  }
  
  
  # index plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q3 index ",sp," ",my_metric, ".png")),
         width=7, height=3, units="in", res=300)
    
    print(
      datras_index(ibtsq3_index, SPECIES=sp, YRS=2005:2022, lab = "N/hour", legend.position="right", byvessel=FALSE)
    )
    
    dev.off()
  }

  # zero plots
  for (sp in top$species[1:10]) {
    png(filename=file.path(figuresdir, paste0("IBTS-Q3 zero ",sp," ",my_metric, ".png")),
        width=7, height=3, units="in", res=300)
    
    print(
      datras_zero(ibtsq3_rbys, SPECIES=sp, YRS=2005:2022, lab = "perc. zero hauls", 
                  legend.position="right", byvessel=FALSE)
    )
    
    dev.off()
  }
  
}
  
# load(file=file.path(rdatadir, "cgfs.RData"))
# load(file=file.path(rdatadir, "ibtsq1.RData"))
# load(file=file.path(rdatadir, "ibtsq3.RData"))

# combine rbys
comb_rbys <- 
  bind_rows(
    cgfs_rbys   %>% mutate(survey="CGFS"   , area="Channel"), 
    ibtsq1_rbys %>% mutate(survey="IBTS Q1", area="North Sea"),
    ibtsq3_rbys %>% mutate(survey="IBTS Q3", area="North Sea"))

# combine index
comb_index <-
  bind_rows(
    cgfs_index   %>% mutate(survey="CGFS"   , area="Channel"),
    ibtsq1_index %>% mutate(survey="IBTS Q1", area="North Sea"),
    ibtsq3_index %>% mutate(survey="IBTS Q3", area="North Sea"))

top2 <-
  top %>% 
  filter(row_number() %in% c(1:8))

nrs <- 1
for (nrs in c(1,5)) {

  # index plots
  
  png(filename=file.path(figuresdir, paste0("standardized cpue ",nrs,"-", nrs+3,".png")),
      width=7, height=9.8, units="in", res=300)
  
  t <-
    comb_index %>% 
    filter(species %in% slice(top2, nrs:(nrs+3))$species) %>% 
    drop_na(std_est) %>% 
    mutate(englishspecies = factor(englishspecies, levels=top$englishspecies)) %>% 
    mutate(vessel = ifelse(is.na(vessel), "Several", vessel))

  s <-
    comb_index %>% 
    drop_na(est) %>% 
    filter(year >= 2015) %>% 
    filter(species %in% slice(top2, nrs:(nrs+3))$species) %>% 
    filter(!(species %in% c("SQR")        & survey %in% c("IBTS Q3"))) %>% 
    filter(!(species %in% c("CTC", "BSS") & survey %in% c("IBTS Q1", "IBTS Q3"))) %>% 
    filter(!(species %in% c("GUU")        & survey %in% c("IBTS Q1"))) %>% 
    group_by(survey, area, species, englishspecies) %>%
    summarise(
      slope     = lm(std_est ~ year)$coefficients[2],
      intercept = lm(std_est ~ year)$coefficients[1]) 
  
  print(
    t %>% 
      ggplot(aes(x=year, y=std_est)) +
      theme_minimal(base_size = 11) +
      theme(legend.position="bottom") +
      
      geom_segment(aes(x=2015, xend=2022, y=0, yend=0), linewidth=0.5) +
      geom_line(aes(year, std_est, colour=survey, linetype=vessel)) +
      geom_point(aes(year, std_est, colour=survey, shape=vessel)) +
      geom_ribbon(aes(ymin=std_lwr, ymax=std_upr, fill=survey), alpha=0.2) +
      geom_smooth(data=filter(t, year >=2015), 
                  aes(colour=survey),
                  method="lm", se=FALSE, linetype="longdash", line_width=0.5) +
      # geom_abline(data=s,
      #             aes(slope=slope, intercept=intercept), 
      #             inherit.aes=FALSE) +
      scale_linetype_manual(values = c("Gwen Drez" = "dotted", "Thalassa" = "solid", "Several" = "solid")) +
      scale_shape_manual(values = c("Gwen Drez" = 1, "Thalassa" = 16, "Several" = 16)) +
      # ylim(-(max(abs(t$std_est))), (max(abs(t$std_est)))) +
      ylim(-2, 2) +
      facet_grid(englishspecies~area)  
  )
  
  dev.off()
  
  # zero plots
  
  png(filename=file.path(figuresdir, paste0("zero hauls ",nrs,"-", nrs+3,".png")),
      width=7, height=9.8, units="in", res=300)
  
  t <-
    comb_rbys %>% 
    filter(species %in% slice(top2, nrs:(nrs+3))$species) %>% 
    dplyr::group_by(., survey, quarter, species, englishspecies, dutchname, year, vessel, area) %>% 
    summarise(
      nhauls = n(),
      nzero = sum(N==0)
    ) %>% 
    ungroup() %>% 
    mutate(prop_zero = nzero/nhauls) %>% 
    mutate(englishspecies = factor(englishspecies, levels=top$englishspecies)) %>% 
    mutate(vessel = ifelse(is.na(vessel), "Several", vessel))
  
  print(
    t %>% 
      ggplot(aes(x=year, y=prop_zero)) +
      theme_minimal(base_size = 11) +
      theme(legend.position="bottom") +
      
      geom_line(aes(colour=survey, linetype=vessel)) +
      geom_point(aes(colour=survey, shape=vessel)) +
      scale_linetype_manual(values = c("GWEN DREZ" = "dotted", "THALASSA" = "solid", "Several" = "solid")) +
      scale_shape_manual(values = c("GWEN DREZ" = 1, "THALASSA" = 16, "Several" = 16)) +
      scale_y_continuous(limits=c(0, 1), labels=scales::percent) +
      facet_grid(englishspecies~area)  
  )
  
  dev.off()
  
}

# ------------------------------------------------------------------------------
# Table with average proportion of zero hauls
# ------------------------------------------------------------------------------

ft <-
  comb_rbys %>% 
  mutate(englishname = factor(englishname, levels=top$englishname)) %>% 
  filter(species %in% slice(top, 1:8)$species) %>% 
  dplyr::group_by(., survey, quarter, species, englishname, scientificname, dutchname, year, vessel, area) %>% 
  summarise(
    nhauls = n(),
    nzero = sum(N==0)
  ) %>% 
  ungroup() %>% 
  mutate(prop_zero = nzero/nhauls) %>% 
  
  filter(year %in% 2015:2022) %>% 
  group_by(survey, name=englishname, code=species, latin=scientificname, dutch=dutchname) %>% 
  summarise(avg_prop_zero = mean(prop_zero)) %>% 
  mutate(avg_prop_zero = scales::percent(avg_prop_zero, accuracy=1)) %>% 
  pivot_wider(names_from = survey, values_from = avg_prop_zero) %>% 

  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "zero hauls.png"),
    width=8.95, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()



# ------------------------------------------------------------------------------
# table with slope in index between 2015 and 2022
# ------------------------------------------------------------------------------

t <-
  comb_index %>% 
  mutate(englishname = factor(englishname, levels=top$englishname)) %>% 
  drop_na(est) %>% 
  filter(year >= 2015) %>% 
  filter(species %in% slice(top, 1:8)$species) %>%
  filter(!(species %in% c("SQR")        & survey %in% c("IBTS Q3"))) %>% 
  filter(!(species %in% c("CTC", "BSS") & survey %in% c("IBTS Q1", "IBTS Q3"))) %>% 
  filter(!(species %in% c("GUU")        & survey %in% c("IBTS Q1"))) %>% 
  group_by(survey, name=englishname, code=species, latin=scientificname, dutch=dutchname) %>% 
  # group_by(survey, species) %>%
  summarise(slope     = lm(std_est ~ year)$coefficients[2]) %>% 
  mutate(slope = round(slope, digits=2))  
  # pivot_wider(names_from = survey, values_from = slope) 
  

bgmatrix <- 
  t %>% 
  mutate(colour=  case_when(
    slope > 0.15 ~ "green",
    slope < -0.15 ~ "red",
    slope > 0.05 ~ "lightgreen",
    slope < -0.05 ~ "pink",
    TRUE ~ "lightgray")
    ) %>% 
  dplyr::select(-slope) %>% 
  pivot_wider(names_from = survey, values_from = colour) %>% 
  ungroup() %>% 
  dplyr::select(-c(1:4)) %>% 
  as.matrix()
  

ft <-
  t %>% 
  pivot_wider(names_from = survey, values_from = slope)  %>% 
  flextable::flextable() %>%
  flextable::fontsize(size = 12, part = "all") %>% 
  flextable::bg(j=5:7, bg=bgmatrix) %>% 
  # flextable::bg(i = ~ CGFS>0.1, j=~CGFS, "lightgreen") %>% 
  flextable::autofit()

png(filename=file.path(tablesdir, "slopes.png"),
    width=8.95, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()
