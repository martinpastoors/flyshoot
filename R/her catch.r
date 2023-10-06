# ==============================================================================
# her catch
#
# 18/9/2023
# ==============================================================================

# devtools::install_github("alastairrushworth/inspectdf")

library(tidyverse)
library(lubridate)

rm(list=ls())

source("../prf/r/my utils.R")
source("../mptools/R/get_onedrive.r")

onedrive  <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/data")

# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

df  <-
  read.csv(file.path(onedrive,"HERCatchData.csv"))  %>% 
  data.frame() %>% 
  lowcase() %>% 
  dplyr::select(-tripname) %>% 
  tidyr::separate(tripcode, into=c("year","month","vessel","tripnr"), sep="_", remove=FALSE) %>% 
  mutate(tripnr = ifelse(is.na(tripnr), 1, tripnr)) %>% 
  mutate(across(c(year,month), as.integer)) %>% 
  mutate(cpue = ifelse(is.na(cpue), 0, cpue)) %>% 
  mutate(cpue = as.numeric(cpue)) %>% 
  mutate(cpue_class = cut(cpue, breaks=seq(0,1300,100), dig.lab=10, include.lowest = TRUE)) %>% 
  rename(haul = haulnum) 

# ------------------------------------------------------------------------------
# Overzicht bijvangst haring
# ------------------------------------------------------------------------------

df %>% 
  group_by(month, cpue_class) %>% 
  summarise(nhauls = n()) %>% 
  
  ggplot(aes(x=cpue_class, y=nhauls)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity") +
  facet_wrap(~month) 

# ------------------------------------------------------------------------------
# Overzicht gem bijvangst haring (kg/uur)
# ------------------------------------------------------------------------------

df %>% 
  group_by(month) %>% 
  summarise(
    nhauls = n(), 
    catch = sum(cpue)) %>% 
  mutate(
    cpue = catch/nhauls
  ) %>% 
  
  ggplot(aes(x=month, y=cpue)) +
  theme_publication() +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  labs(y="cpue (kg/uur)", title="gemiddelde haring bijvangst per uur")





  ggplot(aes(x=cpue_class, y=nhauls)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat="identity") +
  facet_wrap(~month) 

