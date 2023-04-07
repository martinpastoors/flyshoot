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
  readxl::read_excel("C:/DATA/FAO ASFIS 6 languages_2014 plus dutch and english.xlsx", col_types = "text") %>% 
  rename(species = "3A_CODE") %>% 
  lowcase() %>% 
  ungroup() %>% 
  drop_na(dutchname) %>% 
  dplyr::select(species, dutchname, germanname, pfa)


asfis <- 
  readxl::read_excel("C:/DATA/FAO ASFIS_sp_2022_REV1.xlsx") %>% 
  rename(species = "3A_CODE") %>% 
  lowcase() %>% 
  left_join(asfis_old) %>% 
  left_join(ers) %>% 
  mutate(dutchname = ifelse(is.na(dutchname), dutch_name2, dutchname)) %>% 
  mutate(dutchname = case_when(
    dutchname == "Auxis rochei" ~ "Kogeltonijn",
    dutchname == "Brama australis" ~ "Zeebraam",
    dutchname == "Caranx rhonchus" ~ "Gele horsmakreel",
    dutchname == "Harder(diklip)" ~ "Diklipharder",
    dutchname == "Inktvis (kraak)" ~ "Octopus",
    dutchname == "Pieterman (grote)" ~ "Grote pieterman",
    dutchname == "Centropristis striata" ~ "Zwarte zeebaars",
    TRUE                         ~ dutchname
  )) %>% 
  dplyr::select(-dutch_name2) 
  

save(asfis, file="C:/DATA/RDATA/asfis.RData") 

  