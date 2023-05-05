# -----------------------------------------------------------------------------------------------
# FLSYSHOOT Presentation
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

get_onedrive <- function (team="Martin Pastoors", site="FLYSHOOT - General") {
  
  if (Sys.info()['sysname'] == 'Windows') {
    
    # set onedrive directory
    if(dir.exists(file.path(Sys.getenv('USERPROFILE'), team, site))) {
      onedrive <- file.path(Sys.getenv('USERPROFILE'), team, site)   
    } else if(dir.exists(file.path('C:/DATA/PFA', team, site))) {
      onedrive <- file.path('C:/DATA/PFA', team, site)
    } else if(dir.exists(file.path('D:/DATA/PFA', team, site))) {
      onedrive <- file.path('D:/DATA/PFA', team, site)
    } else {
      stop("Onedrive directory not found")
    }
  }
  
  return(onedrive)
}

onedrive   <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")
figuresdir <- file.path(onedrive, "report","CPUE 27.7.d-27.7.e", "figures")
myfile     <- file.path(onedrive, "presentations", "Pastoors 20230421 CPUE presentation.pptx")

# ---------------------------------------------------------------------------------------------
# start presentation
# ---------------------------------------------------------------------------------------------

my_pres<-
  read_pptx("MPFF_powerpoint_template.pptx") %>%
  
  # Add a title slide
  add_slide(layout="Title Slide2", 
            master="Office Theme") %>%
  ph_with(value = "Jaczon flyshoot CPUE analysis 7d & 7e", 
          location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = "Martin Pastoors", 
          location = ph_location_type(type = "subTitle")) %>% 
  ph_with(value = "Jaczon, 21 April 2023, Stellendam", 
          location = ph_location_label(ph_label="Location"))  
  # ph_with(value = external_img(file.path(figuresdir, "mackerel2.jpg")), 
  #         location = ph_location_label(ph_label="Main picture")) 
  
# fileout <- "test.pptx"
# print(my_pres, target = fileout)

# knitr::kable(layout_summary(my_pres))
# layout_properties ( x = my_pres, layout = "Title Slide" )
# layout_properties ( x = my_pres, layout = "Title Slide2" )
# layout_properties ( x = my_pres, layout = "Title and Content")
# layout_properties ( x = my_pres, layout = "Section Header")
# layout_properties ( x = my_pres, layout = "Two Content")
# layout_properties ( x = my_pres, layout = "Blank")

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "CPUE analysis based on electronic logbook data", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = c("Historical data (OLRAC, E-Catch, PEFA logbooks)",
                    "Recovered from harddisks or exported from cloud",
                    "Harmonized and sanitized"), 
          location = ph_location_type(type = "body"))

fileout <- myfile
print(my_pres, target = fileout)

# add average fishing days per week
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Average number fishing days per week", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "average days at sea by week.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

# Add catch by species
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Catch by species", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "landings by species.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

# Add price by species
# Add catch by species
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Price by species", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "prices by species.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

# Add revenue by species
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Revenue by species", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "revenue by species.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

# Add proportion of targeted weeks
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Proportion of targeted weeks by species (highest revenue by week)", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "proportion of targeted weeks.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

# Add average landings per day
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Average landings by day and species", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "average landings per day.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

# add standardized CPUE
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Standardized CPUE (landings by day) by species", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "CPUE timetrends.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

# add observed and predicted
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Observed and predicted CPUE (landings by day) by species", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "CPUE obspred.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- myfile
print(my_pres, target = fileout)

