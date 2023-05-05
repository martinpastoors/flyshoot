# -----------------------------------------------------------------------------------------------
# FLSYSHOOT Surveys v2 report
#
# 21/04/2023 using Officer package (while saving files as png and jpg)
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
# library(directlabels)  # for printing labels at end of geom lines
library(scales)
library(stringr)
library(readxl)
library(officer)
library(flextable)

# Load utils code
source("../prf/r/my utils.r")
source("../mptools/R/get_onedrive.r")

onedrive   <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

reportdir       <- file.path(onedrive,"report", "FLYSHOOT surveys v2")
figuresdir      <- file.path(reportdir, "figures")
presentationdir <- file.path(reportdir, "presentation")
tablesdir       <- file.path(reportdir, "tables")
rdatadir        <- file.path(reportdir, "rdata")

myfile          <- file.path(reportdir, "Pastoors 20230505 Survey analysis.docx")

top      <- 
  readr::read_rds(file=file.path(onedrive,"rdata", "top.rds")) %>%  
  lowcase() %>% 
  mutate(PANEL=row_number())

# load(file=file.path(rdatadir, "cgfs.RData"))
# load(file=file.path(rdatadir, "ibtsq1.RData"))
# load(file=file.path(rdatadir, "ibtsq3.RData"))

my_metric <- "cpue_number_per_hour"

# ---------------------------------------------------------------------------------------------
# start word document
# ---------------------------------------------------------------------------------------------

my_docx<-
  read_docx("MPFF empty document.docx")

my_docx <- 
  my_docx %>%  
  # body_add_title("Analysis of survey data relevant to the key target species of the flyshoot fishery") %>% 
  body_add_par("Analysis of survey data relevant to the key target species of the flyshoot fishery", style = "Title") %>% 
  body_add_par("Martin Pastoors", style = "Normal") %>% 
  
  body_add_par(" ", style = "Normal") %>% 
  
  body_add_par(" ", style = "Normal") %>% 
  
  body_add_img(src = file.path(figuresdir, "MPFF logo with text.png"), 
               height = 399/332*1, width = 1) %>% 
  
  # ------------------------------------------------------------------------------
  # Exec summary 
  # ------------------------------------------------------------------------------
  
  body_add_par("Executive summary", style = "heading 1") %>% 
  
  body_add_par("The flyshoot fishery is operating largely within the Channel and southern North Sea. Main target species are European squid, Red mullet, Common cuttlefish, mackerel and gurnards. For most of the target species there are no regular assessments and they are not regulated via quota. Therefore, it is important to assess the development of those species/stocks via other means than regular assessments. Here, we used available survey information in the Channel and southern North Sea to develop a reliable information base to underpin assessments of the status of those species", style = "Normal") %>% 
  
  body_add_par("Bottom trawl survey data directly downloaded from Datras database using the icesDatras and tidyDatras packages. The following surveys were included: FR-CGFS (Q4, Channel), NS-IBTS Q1 (North Sea), NS-IBTS Q3 (North Sea).", style = "Normal") %>% 
  
  body_add_par("TidyDatras was used to standardize all observations by length to numbers per hour. Outliers were excluded via the Winsorize function whereby all observations larger than the 98 percentile per species were set to the value at 98 percentile. Zero hauls were added for each species and survey.", style = "Normal") %>% 
  
  body_add_par("The FR-CGFS survey experienced a change in the survey vessel in 2015 (from Gwen Drez to Thalassa). This had substantial implications for catchability, especially for pelagic species. Therefore results are presented separately by vessel for that survey.", style = "Normal") %>% 
  
  body_add_par("Results", style = "caption") %>% 
  
  body_add_par("The following top 8 species in value (€) for the Jaczon flyshoot fishery 2018-2022 were included in the analysis: ", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "top species.png"), 
               height = 0.32*6, width = 6) %>% 

  body_add_par("The average number of zero hauls over the years 2015-2022 was calculated by species and survey. For some species and surveys, the number of zero hauls clearly prevented the calculation of a useable index of abundance due to the high number of zero hauls (e.g. European squid in IBTS Q3, Common cuttlefish and Seabass in IBTS Q1 and IBTS Q3 and Tub gurnard in IBTS Q1)", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "zero hauls.png"), 
               height = 0.32*6, width = 6) %>% 
  
  body_add_par("We calculated the slope of the index of abundance by species and survey to derive the general tendency for the species. We only used data from the years 2015-2022, to exclude the survey vessel effect in the CGFS survey. The slopes were categorized as strongly increasing (>0.15; green), moderately increasing (0.05 > x <= 0.15; light green), flat (-0.05 > x <= 0.05; light gray), moderately decreasing (-0.15 >= x < -0.05;pink) and strongly decreasing (x < -0.15; red). In the Channel area, we observe increasing trends for European squid, Common cuttlefish, Tub gurnard and European seabass and a decling trend for horse mackerel and whiting (the latter mostly due to a very low survey value in 2022). In the IBTS survey, we observe a more mixed pattern with some increases and some decreases. Whiting shows a consistent positive trend in the North sea.", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "slopes.png"), 
               height = 0.32*6, width = 6) %>% 
  # ------------------------------------------------------------------------------
  # Introduction 
  # ------------------------------------------------------------------------------
  
  body_add_par("Introduction", style = "heading 1") %>% 
  
  body_add_par("The flyshoot fishery is operating largely within the Channel and southern North Sea. Main target species are European squid, Red mullet, Common cuttlefish, mackerel and gurnards. For most of the target species there are no regular assessments and they are not regulated via quota. Therefore, it is important to assess the development of those species/stocks via other means than regular assessments. Here, we used available survey information in the Channel and southern North Sea to develop a reliable information base to underpin assessments of the status of those species", style = "Normal") %>% 
  
  # ------------------------------------------------------------------------------
  # Material and methods 
  # ------------------------------------------------------------------------------

  body_add_par("Material and methods", style = "heading 1") %>% 
  
  body_add_par("Top species in value and weight from Jaczon Flyshoot vessels 2018-2022", style = "Normal") %>% 
  
  body_add_img(src = file.path(figuresdir, "top.png"), 
               height = 1500/2100*6, width = 6) %>% 
  
  body_add_par("We included the top 8 species in value from the graph above", style = "Normal") %>% 

  body_add_img(src = file.path(tablesdir, "top species.png"), 
               height = 0.32*6, width = 6) %>% 
  
  body_add_par("The following surveys were included: FR-CGFS (Q4, Channel), NS-IBTS Q1 (North Sea), NS-IBTS Q3 (North Sea)", style = "Normal") %>% 
  
  body_add_par("Bottom trawl survey data directly downloaded from Datras database using icesDatras and tidyDatras", style = "Normal") %>% 
  
  body_add_par("TidyDatras was used to standardize all observations by length to numbers per hour. Outliers were excluded via the Winsorize function whereby all observations larger than the 98 percentile per species were set to the value at 98 percentile. Zero hauls were added for each species and survey.", style = "Normal") %>% 
  
  body_add_par("The FR-CGFS survey experienced a change in the survey vessel in 2015 (from Gwen Drez to Thalassa). This had substantial implications for catchability, especially for pelagic species. Therefore results are presented separately by vessel for that survey.", style = "Normal") %>% 
  
  body_add_par("For the North Sea surveys, we only included observations in divisions 27.4.b and 27.4.c, as these are the only areas fishing by the flyshoot vessels.", style = "Normal") %>% 
  
  body_add_par("Ideally we would work with numbers per swept area, but the standardization of gear parameters was not yet fully available for all surveys. Therefore, results are expressed as number per hour.", style = "Normal") %>% 
  
  body_add_par("The average number of zero hauls over the years 2015-2022 was calculated by species and survey. For some species and surveys, the number of zero hauls clearly prevented the calculation of a useable index of abundance due to the high number of zero hauls (e.g. European squid in IBTS Q3, Common cuttlefish and Seabass in IBTS Q1 and IBTS Q3 and Tub gurnard in IBTS Q1)", style = "Normal") %>% 
  
  body_add_par("The abundance index was calculated as the mean abundance per haul. A bootstrap procedure was applied to estimating the associated uncertainty. ", style = "Normal") %>% 
  
  body_add_par("A standardized abundance index was also calculated to make the units comparable across species and areas. Standardization was carried out by dividing the index by the mean over the years 2015-2022 (the years when the Thalassa vessel has been used for the CGFS survey) and subtracting 1. This means that the average of the standardized abundance indices over the standardization years would be zero. The standardization was also applied to the bootstrapped values. ", style = "Normal") %>% 

  body_add_par("We calculated the slope of the index of abundance by species and survey to derive the general tendency for the species. We only used data from the years 2015-2022, to exclude the survey vessel effect in the CGFS survey. The slopes were categorized as strongly increasing (>0.15; green), moderately increasing (0.05 > x <= 0.15; light green), flat (-0.05 > x <= 0.05; light gray), moderately decreasing (-0.15 >= x < -0.05;pink) and strongly decreasing (x < -0.15; red).", style = "Normal") %>% 
  
# ------------------------------------------------------------------------------
# Results 
# ------------------------------------------------------------------------------

body_add_par("Results", style = "heading 1")  
  
# ------------------------------------------------------------------------------
# top$species[1]: SQR  
# ------------------------------------------------------------------------------

i  <- 1
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("European squid (SQR) is a key target species for the flyshoot fishery. The species has regularly been caught in the FR-CGFS survey and is increasingly being caught in the NS-IBTS surveys. Within the FR-CGFS survey, the species has only been included from 2014 onwards.", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the survey is very low, meaning that the species is caught in most of the survey hauls within the area. The index shows an increase in abundance of European squid. The main distribution is within the southern part of the Channel. Lengths are between 5 and 20 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 
    
my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been very high (~100%) but appears to be lowering. From around 2015, 75% of the hauls are zero hauls. The index shows an overall increase in abundance of from 2014 onwards, but with a low value for 2022. The main distribution is within the southern part of division 4c. Lengths are generally between 10 and 20 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q3 survey is very high and stable (~100%). There are no indications of squid being available in the survey area within the 3rd quarter. Therefore, the index calculations are probably not useable as an index of abundance for the species.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 



# ------------------------------------------------------------------------------
# top$species[2]: MUR  
# ------------------------------------------------------------------------------

i  <- 2
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Red mullet or Surmullet (MUR) is the second important target species for the flyshoot fishery.", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the survey is been around 50% between 2005 and 2013 and around 25% between 2014 and 2022. It is unclear whether this decline in zero hauls is due to the change in survey vessel or due to increased abundance, but observations from the fishery would suggest that the latter is happening: red mullet have become more abundant in the Channel. The index shows an increase in abundance of Red mullet from 2014 onwards.The years 2015 and 2018 showed high occurrence of red mullet in the Channel, mostly in the northeastern and southwestern parts of the area. Lengths are generally between 10 and 25 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been high (>90%) but appears to be lowering. From around 2014. The index shows an slight increase in abundance from 2014 onwards, but is very variable. The main distribution is within the western part of the North Sea. Lengths are generally between 10 and 20 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("Similar to the Q1 survey, the number of zero-hauls in the IBTS Q3 survey has been high (>90%) but appears to be lowering. The index appears to relatively high in the most recent four years. Red mullet is mostly found in the southern part of the North Sea during the third quarter.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 


# ------------------------------------------------------------------------------
# top$species[3]: CTC  
# ------------------------------------------------------------------------------

i  <- 3
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Common cuttlefish (CTC) is increasingly a target species for the flyshoot fishery. The species has regularly been caught in the FR-CGFS survey and is incidentally being caught in the NS-IBTS surveys. Within the FR-CGFS survey, the species has only been included from 2014 onwards.", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the survey has been declined between 2014-2022. The index shows an strong increase in abundance of cuttlefish. The main distribution is within the southern and southeastern part of the Channel. Lengths are between 5 and 10 cm and sometimes up to 20 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been very high (~100%), therefore ,the index is probably not really useable. The occurrences of cuttlefish appear to be irregular", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q3 survey has been very high (~100%), therefore ,the index is probably not really useable. The occurrences of cuttlefish appear to be irregular", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 


# ------------------------------------------------------------------------------
# top$species[4]: MAC  
# ------------------------------------------------------------------------------

i  <- 4
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Mackerel is regularly being caught within the flyshoot fishery. Since this is a schooling pelagic species, catches can be substantial within individual hauls. The species is very widely distributed throughout the Northeastern Atlantic, which means that the survey indices derived here, may not be really representative of the development of the stock. ", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the survey is strongly declined with the change in survey vessel from Gwen Drez to Thalassa in 2015. This is likely due to the change in catchability and location of the survey hauls. Likewise, the index of abundance has been substantially higher from 2015 onwards. There is no clear trand in the abundance during the period 2015-2022. Mackerel are mostly caught in the northeastern part of the Channelwith lengths between 25 and 35 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been high (>90%) but appears to be lowering. The index shows an slight increase in abundance in the more recent years, but seems to be largely driven by higher abundances in the northern part of 4b.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("During the IBTS Q3 survey, the number of zero-hauls has generally been below 50%. The index appears to have a slight tendency for decline.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 



# ------------------------------------------------------------------------------
# top$species[5]: GUU  
# ------------------------------------------------------------------------------

i  <- 5
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Tub gurnard (GUU) is, confusingly enough, call 'rode poon' in Dutch. It is a regular target species within the flyshoot fishery, although high catches of gurnards may lead to lower prices.", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the survey is has declined from around 60% to 50%. The index of abundance has been substantially higher in the four most recent years. The species are mostly caught in the  coastal areas with lengths between 10 and 40 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been high (>90%). The index shows an slight increase in abundance but this may not be useable given the low number of positive hauls.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("During the IBTS Q3 survey, the number of zero-hauls has generally been around 90%. There is no clear trend in the index of abundance.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 


# ------------------------------------------------------------------------------
# top$species[6]: WHG  
# ------------------------------------------------------------------------------

i  <- 6
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Whiting (WHG) is not really a target species within the flyshoot fishery, although it is regularly caught and landed. Only limited quota for whiting is available in the flyshoot fishery. Whiting is assessed as one stock, covering both the North Sea and the Channel", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the survey in the Channel has been between 35 and 75%. The index of abundance appears to be declining, with a very low value in 2022. The species is mostly caught in the coastal area, notably in the northeastern part and with lengths between 10 and 30 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey is very low (close to zero). The index shows a strong increase in the three most recent years. Whiting is mostly caught in the western part of the North Sea with lengths between 10 and 35 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q3 survey has been low (~10%). The index shows an increase in the four most recent years. Whiting in the 3rd quarter is mostly caught in the southern part of the North Sea with lengths between 5 and 30 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

# ------------------------------------------------------------------------------
# top$species[7]: HOM  
# ------------------------------------------------------------------------------

i  <- 7
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Horse mackerel can be an important bycatch species in the flyshoot fishery.The stock assessment is carried out separately for the western stock (subareas 6, 7 and 8 without 7.d) and the North Sea stock (4b, 4c and 7d).", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls for horse mackerel is very low (close to zero) in the survey. The index of abundance is much higher in the recent years due to the change in survey vessel. Therefore the index before and after 2015 cannot be directly compared. The species is caught throughout the Channel areas with lengths between 20 and 25 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been high (between 50 and 90%). The index shows no real trend. In some years, catches were predominantly in the north while in recent years it is more in the south of the North sea.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q3 survey has been between 50 and 75%. The index shows an increase in the recent two year, but with a large uncertainty (due to the low number of positive hauls).", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 


# ------------------------------------------------------------------------------
# top$species[8]: BSS  
# ------------------------------------------------------------------------------

i  <- 8
sp <- top$species[i]
es <- top$englishspecies[i]
dn <- top$dutchname[i]

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(es, dn, sep= " / "), style = "heading 2") %>% 
  body_add_par("Seabass can be a bycatch species in the flyshoot fishery, although the landing of seabass is highly restricted.", style = "Normal")

my_survey <- "FR-CGFS"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls for seabass has been between 50-75%. The index of abundance shows an increase in abundance in the recent years. The species is mostly caught in the coastal areas with lengths between 30 and 40 cm.", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q1"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q1 survey has been high close to 100%. The index of abundances is not useable", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 

my_survey <- "IBTS-Q3"
my_index  <- paste0(paste(my_survey, "index", sp, my_metric), ".png")
my_zero   <- paste0(paste(my_survey, "zero", sp, my_metric), ".png")
my_bubble <- paste0(paste(my_survey, "bubble", sp, my_metric), ".png")
my_length <- paste0(paste(my_survey, "length", sp, my_metric), ".png")
my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par(paste(my_survey, es, dn, sep=" / "), style = "heading 3") %>% 
  body_add_par("The number of zero-hauls in the IBTS Q3 survey has been high close to 100%. The index of abundances is not useable", style = "Normal") %>% 
  body_add_img(src = file.path(figuresdir, my_zero), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_index), height = 900/2100*6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_bubble), height = 2940/2100 * 6, width = 6) %>% 
  body_add_img(src = file.path(figuresdir, my_length), height = 2940/2100 * 6, width = 6) 




# ------------------------------------------------------------------------------
# add summary plots
# ------------------------------------------------------------------------------

my_docx <-
  my_docx %>% 
  body_add_break() %>% 
  body_add_par("Discussion and overview", style = "heading 2") %>%
  
  body_add_par("Number of zero hauls", style = "heading 3") %>% 
  body_add_par("The number of zero hauls per species and survey shows very different patterns.", style = "Normal") %>% 
  body_add_par("The average number of zero hauls over the years 2015-2022 was calculated by species and survey. For some species and surveys, the number of zero hauls clearly prevented the calculation of a useable index of abundance due to the high number of zero hauls (e.g. European squid in IBTS Q3, Common cuttlefish and Seabass in IBTS Q1 and IBTS Q3 and Tub gurnard in IBTS Q1)", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "zero hauls.png"), 
               height = 0.32*6, width = 6) %>% 
  
  body_add_img(src = file.path(figuresdir, "zero hauls 1-4.png"), height = 2940/2100*6, width = 6)  %>% 
  
  body_add_img(src = file.path(figuresdir, "zero hauls 5-8.png"), height = 2940/2100*6, width = 6)  %>% 
  
  body_add_break() %>% 
  
  body_add_par("Relative index of abundance", style = "heading 3") %>% 
  
  body_add_par("A relative abundance index was calculated to make the units comparable across species and areas. Standardization was carried out by dividing the index by the mean over the years 2015-2022 (the years when the Thalassa vessel has been used for the CGFS survey) and subtracting 1. This means that the average of the standardized abundance indices over the standardization years would be zero. The standardization was also applied to the bootstrapped values. ", style = "Normal") %>% 
  
  body_add_par("For each survey and species, the slope over the period 2015-2022 was estimated using linear regression. The slope of the regression lines provides an indication of the general tendency in the development of species and survey. The resulting slopes are summarized in the table below and shown as dashed lines in the plots by species and survey", style = "Normal") %>% 
  
  body_add_par("The slopes were categorized as strongly increasing (>0.15; green), moderately increasing (0.05 > x <= 0.15; light green), flat (-0.05 > x <= 0.05; light gray), moderately decreasing (-0.15 >= x < -0.05;pink) and strongly decreasing (x < -0.15; red). In the Channel area, we observe increasing trends for European squid, Common cuttlefish, Tub gurnard and European seabass and a decling trend for horse mackerel and whiting (the latter mostly due to a very low survey value in 2022). In the IBTS survey, we observe a more mixed pattern with some increases and some decreases. Whiting shows a consistent positive trend in the North sea.", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "slopes.png"), 
               height = 0.32*6, width = 6) %>% 
  
  body_add_img(src = file.path(figuresdir, "standardized cpue 1-4.png"), height = 2940/2100*6, width = 6)  %>% 
  
  body_add_img(src = file.path(figuresdir, "standardized cpue 5-8.png"), height = 2940/2100*6, width = 6)  


fileout <- myfile
print(my_docx, target = fileout)

