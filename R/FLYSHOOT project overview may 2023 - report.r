# -----------------------------------------------------------------------------------------------
# FLSYSHOOT project overview may 2023 report
#
# 16/05/2023 using Officer package
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(scales)
library(stringr)
library(readxl)
library(officer)
library(flextable)

# Load utils code
source("../prf/r/my utils.r")
source("../mptools/R/get_onedrive.r")

onedrive   <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

mytitle         <- "FLYSHOOT project overview May 2023"
myfile          <- file.path(reportdir, "Pastoors 20230531 FLYSHOOT Project Overview May 2023.docx")

reportdir       <- file.path(onedrive,"report", mytitle)
figuresdir      <- file.path(reportdir, "figures")
presentationdir <- file.path(reportdir, "presentation")
tablesdir       <- file.path(reportdir, "tables")
rdatadir        <- file.path(reportdir, "rdata")


# load(file=file.path(rdatadir, "cgfs.RData"))
# load(file=file.path(rdatadir, "ibtsq1.RData"))
# load(file=file.path(rdatadir, "ibtsq3.RData"))


# ---------------------------------------------------------------------------------------------
# start word document
# ---------------------------------------------------------------------------------------------

my_docx<-
  read_docx("MPFF empty document.docx") %>% 
  set_doc_properties(
    title = "FLYSHOOT project overview May 2023",
    subject = NULL,
    creator = "M.A. Pastoors",
    description = NULL,
    created = now()
  )

my_docx <- 
  my_docx %>%  
  # body_add_title("Analysis of survey data relevant to the key target species of the flyshoot fishery") %>% 
  body_add_par("Jaczon Flyshoot data collection project. Status report: May 2023", style = "Title") %>% 
  body_add_par("Project status report: May 2023", style = "Title2") %>% 
  
  body_add_par(" ", style = "Normal") %>% 
  
  body_add_par("Martin Pastoors", style = "Normal") %>% 
  
  body_add_img(src = file.path(figuresdir, "IMG_20230102_092247.jpg"),
             height = 3/4*6, width = 6) %>%
  
  body_add_par(" ", style = "Normal") %>% 
  
  body_add_par(" ", style = "Normal") %>% 
  
  # body_add_img(src = file.path(figuresdir, "MPFF logo with text.png"), 
  #              height = 399/332*1, width = 1) %>% 
  body_add_fpar(fpar(
    external_img(src = file.path(figuresdir, "MPFF logo with text.png"), 
                 height = 399/332*1, width = 1),
    fp_p = fp_par(text.align = "right")    
  )) %>% 

  # ------------------------------------------------------------------------------
  # Exec summary 
  # ------------------------------------------------------------------------------
  
  body_add_par("Executive summary", style = "Hheading 1") %>% 
  
  body_add_par("[text here]", style = "Normal") %>% 
  
  # ------------------------------------------------------------------------------
  # Introduction 
  # ------------------------------------------------------------------------------
  
  body_add_par("Introduction", style = "heading 1") %>% 
  
  body_add_par("[text here]", style = "Normal") %>% 
  
  # ------------------------------------------------------------------------------
  # Self-sampling 
  # ------------------------------------------------------------------------------

  body_add_par("Self-sampling", style = "heading 1") %>% 
  
  body_add_par("[text here]", style = "Normal") %>% 

  body_add_par("Overview of data collected to date", style = "heading 2") %>% 
  
  body_add_par("[text here]", style = "Normal") %>% 

  body_add_par("Tripreports", style = "heading 2") %>% 
    
  body_add_par("[text here]", style = "Normal") %>% 
  
  body_add_par("Estimation of total catch by haul", style = "heading 2") %>% 
    
  body_add_par("[text here]", style = "Normal") %>% 
  
  body_add_par("Bycatch composition data", style = "heading 2") %>% 
    
  body_add_par("[text here]", style = "Normal") %>% 
    
  # ------------------------------------------------------------------------------
  # Electronic logbook data and CPUE analysis 
  # ------------------------------------------------------------------------------
  
  body_add_par("Electronic logbook data and CPUE analysis", style = "heading 1") %>% 
    
  body_add_par("[text here]", style = "Normal") %>% 
  
  # ------------------------------------------------------------------------------
  # Survey data 
  # ------------------------------------------------------------------------------
  
  body_add_par("Survey data", style = "heading 1") %>% 
    
  body_add_par("[text here]", style = "Normal") 
    
fileout <- myfile
print(my_docx, target = fileout)

  
  body_add_img(src = file.path(tablesdir, "top species.png"), 
               height = 0.32*6, width = 6) %>% 

  body_add_par("The average number of zero hauls over the years 2015-2022 was calculated by species and survey. For some species and surveys, the number of zero hauls clearly prevented the calculation of a useable index of abundance due to the high number of zero hauls (e.g. European squid in IBTS Q3, Common cuttlefish and Seabass in IBTS Q1 and IBTS Q3 and Tub gurnard in IBTS Q1)", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "zero hauls.png"), 
               height = 0.32*6, width = 6) %>% 
  
  body_add_par("We calculated the slope of the index of abundance by species and survey to derive the general tendency for the species. We only used data from the years 2015-2022, to exclude the survey vessel effect in the CGFS survey. The slopes were categorized as strongly increasing (>0.15; green), moderately increasing (0.05 > x <= 0.15; light green), flat (-0.05 > x <= 0.05; light gray), moderately decreasing (-0.15 >= x < -0.05;pink) and strongly decreasing (x < -0.15; red). In the Channel area, we observe increasing trends for European squid, Common cuttlefish, Tub gurnard and European seabass and a decling trend for horse mackerel and whiting (the latter mostly due to a very low survey value in 2022). In the IBTS survey, we observe a more mixed pattern with some increases and some decreases. Whiting shows a consistent positive trend in the North sea.", style = "Normal") %>% 
  
  body_add_img(src = file.path(tablesdir, "slopes.png"), 
               height = 0.32*6, width = 6) %>% 

fileout <- myfile
print(my_docx, target = fileout)

