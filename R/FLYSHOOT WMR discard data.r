# =======================================================================================
# FLYSHOOT WMR discard dta.r
# 
# Martin Pastoors
#
# =========================================================================================


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

library(captioner)                   # for captions
tab_nums <- captioner::captioner(prefix = "Table " , levels=1, type=c("n"), infix=".")
fig_nums <- captioner::captioner(prefix = "Figure ", levels=1, type=c("n"), infix=".")

# Source all the utils
source("../prf/R/my utils.r")
source("../mptools/R/get_onedrive.r")

t <- 
  readxl::read_xlsx(path=file.path(get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/data"), 
                        "WMR Landings_Discards_all_hauls.xlsx")) %>% 
  lowcase() %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  rename(vessel=ship, landings = kgland, discards = kgdisc, haul=haulnum, catch=kgcatch, percdiscards=discardratio) %>% 
  mutate(landings = as.numeric(landings, discards = as.numeric(discards))) %>% 
  ungroup() 

range(t$percdiscards, na.rm=TRUE)

t %>% summarise(percdiscards=mean(percdiscards, na.rm=TRUE)) 

# find outliers
t %>% filter(abs(percdiscards)>1) %>% View()

# unweighted average; exlcuding outliers
t %>% filter(abs(percdiscards) <1) %>% summarise(percdiscards=mean(percdiscards, na.rm=TRUE))

# unweighted average; positive only
t %>% filter(abs(percdiscards) <1 & neg == "POSITIVE") %>% summarise(percdiscards=mean(percdiscards, na.rm=TRUE))

# weighted by trip; all included
t %>% filter(abs(percdiscards) <1) %>% 
  group_by(tripcode) %>% 
  summarise(
    landings=mean(landings, na.rm=TRUE),
    discards =mean(discards, na.rm=TRUE)
  ) %>% 
  mutate(percdiscards=discards/(discards+landings)) %>% 
  ungroup() %>% 
  summarise(percdiscards=mean(percdiscards))

t %>% 
  filter(abs(percdiscards) <1) %>% 
  filter(percdiscards> 0.9) %>% 
  View()

t %>% 
  filter(abs(percdiscards) <1) %>% 
  mutate(disc_interval = cut(percdiscards, breaks=seq(-1,1,0.1))) %>% 
  ggplot(aes(x=disc_interval)) +
  theme_publication() +
  geom_bar(stat="count") + 
  facet_wrap(~tripcode, scales = "free_x")

t %>% 
  # filter(vessel=="SL9", date==dmy("23-5-2022")) %>% 
  filter(tripcode=="SL9_2022_20") %>% 
  dplyr::select(vessel, trip=tripcode, haul, landings, catch) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", landings:catch) %>% 
  
  ggplot(aes(x=haul, y=data)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(aes(fill=variable),
           stat="identity", alpha=0.5, width=0.5, just=0.8,
           position = position_dodge2()) +
  coord_flip() +
  scale_x_reverse(breaks=seq(min(t$haul), max(t$haul), 1)) +
  facet_wrap(~trip, ncol=1)

  View()


