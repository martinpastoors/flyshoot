library(tidyverse)
library(lubridate)
library(readxl)

datadir  <- "C:/Users/MartinPastoors/Martin Pastoors/Aanlandplicht - aanlandplicht"
rdatadir <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"

source(file.path(here::here(), "R/FLYSHOOT utils.R"))

load(file=file.path(rdatadir, "asfis.RData")) 

reg <-
  read_excel(file.path(flyshootdir, "regulated species.xlsx")) 

df <-
  read_excel(file.path(datadir, "20260106 diepzee soorten.xlsx")) %>% 
  distinct() %>% 
  left_join(asfis, by="species") %>% 
  left_join(dplyr::select(reg, species, regulated), by="species") %>% 
  dplyr::select(species, scientificname, englishname, dutchname, frenchname, family, order, regulated) %>% 
  arrange(family, order, species)
# writexl::write_xlsx(df, path="temp.xlsx")

tac <-
  read_excel(file.path(datadir, "20260122 EU_TAC_Species_Annex1_2025.xlsx")) %>% 
  lowcase() %>% 
  rename(
    species = "faocode",
    englishname = commonnameenglish,
    dutchname = dutchnamenederlands
  )

pfa <-
  read_csv(file.path(datadir, "20260121 PFA species 2025.csv")) %>% 
  lowcase() %>% 
  mutate(species = toupper(species)) %>% 
  left_join(dplyr::select(tac, -c(scientificname, englishname, dutchname)), by="species") %>% 
  mutate(notregulated = ifelse(is.na(ia)&is.na(ib)&is.na(id)&is.na(ih)&is.na(prohibitedart59)&is.na(prohibitedeu20191241), "X", NA_character_)) %>% 
  left_join(dplyr::select(asfis, species, scientificname, englishname, dutchname), by="species") %>% 
  relocate(c(species, scientificname, englishname, dutchname)) %>% 
  arrange(species)


writexl::write_xlsx(pfa, path="pfa species.xlsx")
  
