# read afslag data
#
# 20/03/2023

library(tidyverse)
library(lubridate)
library(readxl)

rm(list=ls())

source("../prf/r/my utils.R")

# read ERS and ASFIS
ers       <- 
  readxl::read_excel("C:/DATA/ERS-NL codeboek V3 vissoort codes e-catch MP.xls", sheet="Vissoorten") %>% 
  filter(!grepl("NIET", OPMERKING)) %>% 
  
  # dplyr::select(-OPMERKING) %>%
  # rename(dutch_name2=NEDERLANDSE_NAAM, scientific_name=WETENSCHAPPELIJKE_NAAM, species=CODE) %>% 
  # group_by(species, scientific_name) %>% 
  
  dplyr::select(-OPMERKING) %>%
  rename(dutch_name2=NEDERLANDSE_NAAM, species=CODE) %>% 
  group_by(species) %>%
  
  summarise(dutch_name2 = paste(dutch_name2, collapse = " / "))

# old asfis
asfis_old <- 
  loadRData("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata/afsis.RData") %>% 
  mutate(
    species = toupper(species)
  ) %>% 
  ungroup() %>% 
  drop_na(dutch_name) %>% 
  dplyr::select(species, dutch_name, german_name, pfa)


asfis <- 
  readxl::read_excel("C:/DATA/FAO ASFIS_sp_2022_REV1.xlsx") %>% 
  rename(species = "3A_CODE") %>% 
  left_join(asfis_old) %>% 
  left_join(ers) %>% 
  mutate(dutch_name = ifelse(is.na(dutch_name), dutch_name2, dutch_name)) %>% 
  mutate(dutch_name = case_when(
    dutch_name == "Auxis rochei" ~ "Kogeltonijn",
    dutch_name == "Brama australis" ~ "Zeebraam",
    dutch_name == "Caranx rhonchus" ~ "Gele horsmakreel",
    dutch_name == "Harder(diklip)" ~ "Diklipharder",
    dutch_name == "Inktvis (kraak)" ~ "Octopus",
    dutch_name == "Pieterman (grote)" ~ "Grote pieterman",
    dutch_name == "Centropristis striata" ~ "Zwarte zeebaars",
    TRUE                         ~ dutch_name
  )) %>% 
  dplyr::select(-dutch_name2)
  

save(asfis, file="C:/DATA/RDATA/afsis.RData") 

  