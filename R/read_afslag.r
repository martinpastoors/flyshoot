# read afslag data
#
# 20/03/2023

library(tidyverse)
library(lubridate)
library(readxl)

rm(list=ls())

source("../prf/r/my utils.R")

asfis <-
  loadRData("C:/DATA/RDATA/asfis.RData") %>% 
  rename_all(tolower) %>% 
  dplyr::select(species, scientificname, englishname, dutchname)

p <-
  readxl::read_excel(file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data","regulated species.xlsx"),
                     col_names=TRUE, 
                     # col_types="text",
                     range = c("A1:G1000"),
                     .name_repair =  ~make.names(., unique = TRUE)) %>% 
  dplyr::select(species, dutchname=dutch_name, regulated)

  
filelist <- list.files(
  path=file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"),
  pattern="totaal aanvoer",
  recursive = TRUE,
  full.names = TRUE)

df <- data.frame(stringsAsFactors = FALSE)

for (i in 1:length(filelist)) {
  df <-
    bind_rows(
      df, 
      readxl::read_excel(filelist[i],
                         col_names=TRUE, 
                         # col_types="text",
                         range = c("C2:H1000"),
                         .name_repair =  ~make.names(., unique = TRUE)) %>%
        lowcase() %>% 
        drop_na(specie) %>% 
        mutate(vessel = stringr::word(basename(filelist[i]), 1, sep=" ")) %>% 
        mutate(vessel = toupper(gsub("-","",vessel))) %>% 
        mutate(year = stringr::word(basename(filelist[i]), 4, sep=" ")) %>% 
        mutate(year = as.integer(gsub(".xlsx","", year))) %>% 
        setNames(gsub("eur","", names(.))) %>% 
        dplyr::select(-x) %>% 
        rename(dutchname=specie) %>%
        left_join(p, by="dutchname") 
    )
}

df <-
  df %>% 
  group_by(year, species, regulated) %>% 
  summarise(
    avgprice = weighted.mean(avgprice, weight),
    avgauction = weighted.mean(avgauction, weight),          
    weight   = sum(weight, na.rm=TRUE)
  ) %>% 
  left_join(asfis, by="species") %>% 
  drop_na(species)


readr::write_rds(df, file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata","prices.rds"))
        

# No longer needed    

# top20 <-
#   df %>% 
#   group_by(species) %>% 
#   summarise(weight = sum(weight, na.rm=TRUE)) %>% 
#   arrange(desc(weight)) %>% 
#   slice_head(n=20)
# 
# # price by species, vessel and year
# df %>% 
#   filter(species %in% top20$species) %>% 
#   mutate(species = factor(species, levels=top20$species)) %>% 
#   
#   ggplot(aes(x=year, y=avgprice)) +
#   theme_publication() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   
#   geom_point(aes(colour=vessel)) +
#   geom_line(aes(colour=vessel)) +
#   scale_x_continuous(breaks=seq(min(df$year), max(df$year), 1)) +
#   facet_wrap(~species, scales="free_y")
# 
# # price vs weight by species and vessel  
# df %>% 
#   filter(species %in% top20$species) %>% 
#   mutate(species = factor(species, levels=top20$species)) %>% 
#   
#   ggplot(aes(x=weight, y=avgprice)) +
#   theme_publication() +
#   geom_smooth(method="lm", formula="y~x", se=FALSE, span=1, colour="black", linewidth=0.6) +
# 
#   geom_point(aes(colour=vessel)) +
#   facet_wrap(~species, scales="free")
# 
# # create dateset
# df %>% 
#   filter(species %in% top20$species) %>% 
#   mutate(species = factor(species, levels=top20$species)) %>% 
#   group_by(year, species) %>% 
#   summarise(
#     avgprice = weighted.mean(avgprice, weight),
#     avgauction = weighted.mean(avgauction, weight),          
#     weight   = sum(weight, na.rm=TRUE)
#   ) %>% 
#   dplyr::select(species, year, avgprice) %>% 
#   readr::write_rds(., file=file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data", "prices.rds"))
# 
# # create excel
# df %>% 
#   group_by(species) %>% 
#   summarise(
#     avgprice = weighted.mean(avgprice, weight),
#     avgauction = weighted.mean(avgauction, weight),          
#     weight   = sum(weight, na.rm=TRUE)
#   ) %>% 
#   writexl::write_xlsx(., path=file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data", "prices.xlsx"))

