# ------------------------------------------------------------------------------
# ERS codes.r
# 
# Get harbour data into rdata file
#
# "ERS-NL codeboekV3_3_2016_publicatie v1.0.xls"
#
# 03/08/2023
# ------------------------------------------------------------------------------


library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(readxl)        # excel reader

# source code with a number of utilities
source("r/FLYSHOOT utils.r")

onedrive <- get_onedrive() 


harbours <- 
  read_excel(file.path(onedrive,"data", "ERS-NL codeboekV3_3_2016_publicatie v1.0.xls"),
             sheet = "Havens",  col_names=TRUE, col_types="text") %>% 
  setNames(c("harbourcode","harbourname","valid_until")) %>% 
  mutate(valid_until   = as.Date(as.integer(valid_until), origin="1899-12-30", tz="UTC")) 

save(harbours, file= file.path(onedrive, "rdata/harbours.RData"))

# harbours %>% filter(grepl("^FR", harbourcode)) %>% View()

