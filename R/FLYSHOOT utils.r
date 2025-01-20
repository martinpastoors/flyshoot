# --------------------------------------------------------------------------------
# FLYSHOOT utils.r
#
# collection of r utils; Martin Pastoors
# 
# 04/04/2017 combined code for several utils
# 03/09/2017 added z_chords
# 06/09/2017 added get_dropbox
# 20/10/2017 removed full stop from lowcase
# 07/02/2018 added more characters to lowcase
# 05/11/2019 added an, ac
# 27/01/2020 added excel_timezone_to_utc
# 05/03/2020 added count for Inf and for non_finite (=NA, NaN, Inf, -Inf)
# 23/10/2024 added spatial functions
# --------------------------------------------------------------------------------
#
# an                : as.numeric (now also deals with factors)
# ac                : as.character (now also deals with factors)
# theme_publication : set up the publication theme 
# excel_timezone_to_utc : read excel datetime with timezone and convert to UTC
# calculate_time    : Convert Excel time to R time object (ignores timezone)
# map_aspect        : calculate aspect ratio of plots
# count_na          : count number of na in dataset
# count_not_na      : count number of not-na in dataset
# count_zeroes      : count number of zeroes in numeric fields
# count_inf         : count number of Inf in dataset
# count_not_finite  : count number of NA, NaN and Inf in dataset
# crayola           : plot of age compositions (overtaken by ggmisc)
# list_all_objects_in_package   : list all objects in a package
# lowcase           : convert variables to lowcase and remove special characters
# no.emphasis.table : function to remove first column of tables
# rbind.all.columns : function to bind equal columns in two datasets
# ensureNonNaRange  : remove all NA columns and rows
# ensureNonNaColumns: remove all NA columns 
# ensureNonNaRows   : remove all NA rows 
# d2ir              : decimal lat long to ices rectangle
# dms2dec           : convert degrees minutes seconds to decimal positions
# ICESarea          : calculate ICES area from positions
# remove_roman      : remove roman numbers from ICES area names
# csquare           : collection of csquare utilities
# encode_zchords    : coding for z-coordinate system
# get_dropbox       : get local dropbox folder
# get_onedrive      : get local onedrive folder for PFA
# su                : unique sort of a variable
# sortunique        : get sorted unique values of a variable
# %notin%
# loadrdata
# plotallvars       : plot histogram of all numerical variables in data frame
# integer_breaks    : integer breaks for plots axis
# calc_boxplot_stat 
# create_sf
# create_spatial
# create_spatial_df
# create_spatial_df_from_sp

# --------------------------------------------------------------------------------

options(dplyr.summarise.inform = FALSE)

# as integer
ai <- function(x){ 
  if (is.null(levels(x))) {
    return(as.integer(x))
  } else {
    return(as.integer(levels(x))[x])
  }
}

# as.numeric
# an <- function(x){ return(as.numeric(x))}
an <- function(x){ 
  if (is.null(levels(x))) {
    return(as.numeric(x))
  } else {
    return(as.numeric(levels(x))[x])
  }
}

# as.character
ac <- function(x){ 
  if (is.null(levels(x))) {
    return(as.character(x))
  } else {
    return(as.character(levels(x))[x])
  }
}


# publication theme,  updated: 20170704
theme_publication <- function(base_size=14, base_family="Helvetica") {
  # library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title       = element_text(face = "bold",size = rel(1.0), hjust = 0.5),
            plot.margin      = unit(c(10,5,5,5),"mm"),
            plot.background  = element_rect(colour = NA),
            text             = element_text(),
            axis.title       = element_text(face = "bold",size = rel(1)),
            axis.title.y     = element_text(angle=90,vjust =2),
            axis.title.x     = element_text(vjust = -0.2),
            axis.text        = element_text(), 
            axis.line        = element_line(colour="black"),
            axis.ticks       = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(colour="black" , linewidth=0.1),
            panel.background = element_rect(colour = NA),
            strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text       = element_text(face="bold"),
            legend.key       = element_rect(colour = NA),
            legend.position  = "bottom",
            legend.direction = "horizontal",
            legend.key.size  = unit(0.2, "cm"),
            legend.spacing   = unit(0, "cm"),  # updated from legend.margin which is deprecated
            legend.title     = element_text(face="italic", size=rel(0.8))
    ))
}

# -----------------------------------------------------------------------------------

# Convert Excel datetime with timezone to UTC datetime,  updated: 20200127

excel_timezone_to_utc <- function(t,timezone) {
  x1 = as.POSIXct( t * (60*60*24), origin="1899-12-30", tz="UTC")
  x2 = force_tz(x1, unique(timezone))
  x3 = lubridate::with_tz(x2, tz="UTC")
  return(x3)
}
# excel_timezone_to_utc(t=35451.45, timezone="Europe/Amsterdam")

# -----------------------------------------------------------------------------------

# calculate_time: Convert Excel time to R time object (ignores timezone)

calculate_time <- function(t) {
  
  if(all(is.na(t))) {
    return(t) 
  } else {
    t = t - floor(t) # remove date if needed
    t = t*24
    h = floor(t)
    m = floor(60*((t)-h))
    s = floor(60*((t*60-h*60-m)))
    tst = lubridate::hms(paste(h, m, s, sep="/"))
    return(tst)
  }
}

# calculate_time(42653.99)

# -----------------------------------------------------------------------------------

map_aspect = function(x, y) {
  
  require(ggplot2)
  
  x.center <- sum(range(x, na.rm=TRUE)) / 2
  y.center <- sum(range(y, na.rm=TRUE)) / 2
  
  print("x")
  print(range(x, na.rm=TRUE))
  print(x.center)

  print("y")
  print(range(y, na.rm=TRUE))
  print(y.center)
  
  x.dist <- ggplot2:::dist_central_angle(x.center + c(-0.5, 0.5), rep(y.center, 2))
  y.dist <- ggplot2:::dist_central_angle(rep(x.center, 2), y.center + c(-0.5, 0.5))
  y.dist / x.dist
}

# -----------------------------------------------------------------------------------

count_na <- function(x) {
  sapply(x, function(y) sum(is.na(y)))
}

# -----------------------------------------------------------------------------------

count_not_na <- function(x) {
  sapply(x, function(y) sum(!is.na(y)))
}

# -----------------------------------------------------------------------------------

count_zeroes <- function(x) {
  sapply(x, function(y) sum(y == 0, na.rm=T))
}

# -----------------------------------------------------------------------------------

count_inf <- function(x) {
  sapply(x, function(y) sum(is.infinite(y), na.rm=T))
}

# -----------------------------------------------------------------------------------

count_not_finite <- function(x) {
  sapply(x, function(y) sum(!is.finite(y), na.rm=T))
}

# -----------------------------------------------------------------------------------

crayola <- function(d, t="", g) {

  # The crayola function originates from Einar Hjorleifsson
  # Converted into a function, by Martin Pastoors
  # note: data needs to be in long format: year, age, number
  # requires libraries: dplyr, tidyr, RColorBrewer, ggplot2
  # input: df (d) and title (t)
  
  # to do: check whether the df is in the right format
  
  # to do: make the function flexible by allowing input of which variable names to use
  
  # create the colour scheme
  PAIRED <- rep(brewer.pal(12, "Paired"), 100)
  
  # make sure the data is numbers and add yearclass
  d <-
    d %>%
    mutate(
      age  = as.integer(as.character(age)),
      year = as.integer(as.character(year)),
      yc   = year - age
    )
  
  # number of yearclasses
  n <- length(unique(d$yc))
  
  # create and show the plot
  if(missing(g)) {
    # plot without groups
    print(
      d %>%
        ggplot(aes(year, number, fill = factor(yc))) +
        theme_bw() +
        theme(legend.position = "none") + 
        theme(axis.text.y = element_blank()) +
        geom_hline(yintercept = 1, col = "grey") +
        geom_bar(stat = "identity") +
        expand_limits(x = c(min(d$year), max(d$year)), y = 0) +
        scale_fill_manual(values = PAIRED[1:n]) +
        facet_grid(age ~ ., scale = "free_y", switch = "y") +
        labs(x = NULL, y = NULL, title = t) +
        scale_y_continuous(NULL, NULL)
    ) # end of print
  } else {
    
    # plot with groups (area)
    i     <- 0
    plist <- list()
    
    for (a in unique(d[,g]) ) {
        i <- i + 1
        p <- ggplot(filter(d, area==a), 
                 aes(year, number, fill = factor(yc))) +
            theme_bw() +
            theme(legend.position = "none") + 
            theme(axis.text.y = element_blank()) +
            geom_hline(yintercept = 1, col = "grey") +
            geom_bar(stat = "identity") +
            expand_limits(x = c(min(d$year), max(d$year)), y = 0) +
            scale_fill_manual(values = PAIRED[1:n]) +
            facet_grid(age ~ ., scale = "free_y", switch = "y") +
            labs(x = NULL, y = NULL, title=a) +
            scale_y_continuous(NULL, NULL)
        plist[[i]] <- p
        
        # mystr <- paste0(mystr, "p",as.character(i),", ", sep="")
    } # end of for loop
    
    print(plot_grid(plotlist=plist, ncol=length(plist), scale=0.95, align="hv")) 
    
  } # end of if else (missing)
  
  return()
  
}  # end of crayola function


# -----------------------------------------------------------------------------------

# List all objects in a package
list_all_objects_in_package <- function(x) {
  print(lsf.str(paste("package:",x, sep="")))
}


# -----------------------------------------------------------------------------------

# lowcase function
lowcase <- function(df) {
  names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|_+|\\(|\\)","",.) 
  df
}


# -----------------------------------------------------------------------------------

no.emphasis.table <- function(df){
  the.row.names <- rownames(df) 
  
  # For some reason, when 'pandoc' writes the markdown 
  # table to LaTeX, it doesn't make the first column 
  # wide enough unless some padding is added to the row 
  # names
  add.space <- function(x){
    return(paste0(x, ""))
  }
  the.row.names.m <- as.vector(sapply(the.row.names, add.space))
  rownames(df) <- NULL
  df <- cbind(the.row.names.m, df)
  colnames(df)[1] <- "  " 
  
  # Set horizontal justification for columns
  v.justify <- vector()
  v.justify[seq(1, length(df))] <- 'center'
  v.justify[1] <- 'left'
  set.alignment(v.justify) 
  return(df)
}

# -----------------------------------------------------------------------------------
# function to bind all columns
# https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/

rbind.all.columns <- function(x, y) {
  
  if (class(x) != "data.frame") { stop ("dataset 1 is not a data.frame") }
  if (class(y) != "data.frame") { stop ("dataset 2 is not a data.frame") }
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}

# -----------------------------------------------------------------------------------
# Ensure non NA range: function to remove empty columns from readxl
# https://github.com/tidyverse/readxl/issues/162
ensureNonNaRange <- function(dat) {
  idx_col <- ! dat %>% sapply(function(ii) ii %>% is.na() %>% all())
  idx_row <- ! sapply(1:nrow(dat),function(ii) unlist(dat[ii, ]) %>% is.na() %>% all())
  dat[idx_row, idx_col]
}

ensureNonNaColumns <- function(dat) {
  idx_col <- ! dat %>% sapply(function(ii) ii %>% is.na() %>% all())
  dat[ , idx_col]
}

ensureNonNaRows <- function(dat) {
  idx_row <- ! sapply(1:nrow(dat),function(ii) unlist(dat[ii, ]) %>% is.na() %>% all())
  dat[idx_row , ]
}

ensureNonNaFirstRow <- function(dat) {
  idx_col <- ! dat %>% filter(row_number()==1) %>% is.na()
  dat[ , idx_col]
}


# -----------------------------------------------------------------------------------
# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  # see: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# -----------------------------------------------------------------------------------

d2ir <- function (lat, lon = NULL, useI = FALSE) 
{
  if (is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon + 1e-06
  outside <- lat < 36 | lat >= 85.5 | lon <= -44 | lon > 68.5
  # if (any(outside)) 
  #   warning("Positions outside of ICES statistical area")
  lat <- floor(lat * 2) - 71
  lat <- ifelse(lat < 10, paste("0", lat, sep = ""), lat)
  if (useI) 
    lettersUsed <- LETTERS[1:12]
  else lettersUsed <- LETTERS[c(1:8, 10:13)]
  lon1 <- lettersUsed[(lon + 60)%/%10]
  lon2 <- ifelse(lon1 == "A", floor(lon%%4), floor(lon%%10))
  ir <- paste(lat, lon1, lon2, sep = "")
  ir[outside] <- NA
  ir
}

# -----------------------------------------------------------------------------------

dms2dec <- function(dms, separators = c("º", "°", "\'", "’", "’’", "\"", "\'\'", "\\?","\\-")) {
  
  # version 1.4 (2 Feb 2022)
  # dms: a vector of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
  # separators: the characters that are separating degrees, minutes and seconds in 'dms'; mind these are taken in the order in which they appear and not interpreted individually, i.e. 7'3º will be taken as 7 degrees, 3 minutes! input data are assumed to be properly formatted
  
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    
    if (length(splits[[i]]) < 4) {
      hem[i] <- splits[[i]][3]
    } else {
      sec[i] <- splits[[i]][3]
      hem[i] <- splits[[i]][4]
    }
  }
  
  # print(hem)
  
  dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  hem_miss <- which(is.na(hem))
  if (length(hem_miss) > 0) {
    warning("Hemisphere not specified at position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
  }
  dec <- sign * dec
  return(dec)
}  # end dms2dec function


ICESarea <- 
  function (chrons, roman = F) 
  {
    library(sp)
    ICES.area <- rep(NA, dim(chrons)[1])
    ICES.area[point.in.polygon(point.x = chrons$shootlon, 
                               point.y = chrons$shootlat, 
                               pol.x = c(68.5, 30.7, 26,     26, 30, 30, 68.5), 
                               pol.y = c(63,   63,   70.648, 72, 72, 90, 90)) > 0] <- ifelse(roman, 
                                                                                             "Ib", "1b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(33.7, 34.55, 35.28, 36.38, 37.57, 38.31, 39.05, 
                                         39.61, 41.24, 42.81, 43.06, 44.68, 43.51, 43.18, 
                                         41.73, 41.56, 40.66, 40.51, 39.76, 38.96, 37.74, 
                                         36.61, 35.7, 33.7), pol.y = c(73.98, 74.18, 74.36, 
                                                                       74.71, 75.14, 75.45, 75.84, 76.26, 76.61, 76.9, 76.9, 
                                                                       76.75, 75.99, 75.39, 74.82, 73.98, 73.17, 72.2, 72.26, 
                                                                       72.62, 73.04, 73.37, 73.56, 73.98)) > 0] <- ifelse(roman, 
                                                                                                                          "Ia", "1a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-0.2, 7.21, 7.28, 7.83, 8.65, 9.33, 9.83, 10.29, 
                                         9.94, 9.7, 8.75, 7.93, 7.42, 6.73, 5.64, 5.01, 4.74, 
                                         4.32, 4, 3.73, 3.57, 3.4, 3.27, 3.19, 3.16, 3.15, 
                                         3.18, 3.24, 3.31, 3.42, 3.27, 3.18, 2.79, 2.24, 1.79, 
                                         1.44, 1.26, 0.72, 0.04, -0.489, -1.309, -1.559, -2.169, 
                                         -2.539, -3.189, -3.729, -4.189, -4.559, -5.579, -5.599, 
                                         -5.669, -5.779, -6.249, -6.619, -5.329, -4.189, -3.419, 
                                         -2.389, -1.559, -0.609, 0.08, 0.68, 1.18, 1.46, 1.72, 
                                         1.94, 2.09, 2.25, 2.35, 2.39, 2.38, 2.31, 2.22, 2.06, 
                                         1.89, 1.68, 1.48, 1.08, 0.34, -0.199), pol.y = c(73.5, 
                                                                                          73.5, 73.45, 73.14, 72.76, 72.49, 72.31, 72.18, 71.98, 
                                                                                          71.91, 71.64, 71.36, 71.13, 70.79, 70.17, 69.79, 
                                                                                          69.56, 69.32, 69.1, 68.86, 68.69, 68.46, 68.23, 67.98, 
                                                                                          67.77, 67.57, 67.37, 67.18, 67.01, 66.84, 66.43, 
                                                                                          66.39, 66.23, 65.95, 65.64, 65.38, 65.32, 65.08, 
                                                                                          64.72, 64.43, 64.84, 64.92, 65.13, 65.22, 65.39, 
                                                                                          65.47, 65.55, 65.59, 65.69, 65.96, 66.22, 66.47, 
                                                                                          67.09, 67.61, 67.77, 67.96, 68.1, 68.33, 68.55, 68.86, 
                                                                                          69.14, 69.44, 69.76, 69.97, 70.21, 70.43, 70.63, 
                                                                                          70.89, 71.14, 71.35, 71.61, 71.83, 72.01, 72.24, 
                                                                                          72.43, 72.6, 72.75, 72.99, 73.31, 73.5)) > 0] <- ifelse(roman, 
                                                                                                                                                  "IIa1", "2a1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -11, -0.2, 0.34, 1.08, 1.48, 1.68, 1.89, 
                                         2.06, 2.22, 2.31, 2.38, 2.39, 2.35, 2.25, 2.09, 1.94, 
                                         1.72, 1.46, 1.18, 0.68, 0.08, -0.609, -1.559, -2.389, 
                                         -3.419, -4.189, -5.329, -6.619, -6.249, -5.779, -5.669, 
                                         -5.599, -5.579, -4.559, -4.189, -3.729, -3.189, -2.539, 
                                         -2.169, -1.559, -1.309, -0.489, 0.04, 0.72, 1.26, 
                                         1.44, 1.79, 2.24, 2.79, 3.18, 3.27, 3.42, 3.31, 3.24, 
                                         3.18, 3.15, 3.16, 3.19, 3.27, 3.4, 3.57, 3.73, 4, 
                                         4.32, 4.74, 5.01, 5.64, 6.73, 7.42, 7.93, 8.75, 9.7, 
                                         9.94, 10.29, 9.83, 9.33, 8.65, 7.83, 7.28, 7.21, 
                                         30, 30, 26, 26, 11, -4, -4), pol.y = c(63, 73.5, 
                                                                                73.5, 73.31, 72.99, 72.75, 72.6, 72.43, 72.24, 72.01, 
                                                                                71.83, 71.61, 71.35, 71.14, 70.89, 70.63, 70.43, 
                                                                                70.21, 69.97, 69.76, 69.44, 69.14, 68.86, 68.55, 
                                                                                68.33, 68.1, 67.96, 67.77, 67.61, 67.09, 66.47, 66.22, 
                                                                                65.96, 65.69, 65.59, 65.55, 65.47, 65.39, 65.22, 
                                                                                65.13, 64.92, 64.84, 64.43, 64.72, 65.08, 65.32, 
                                                                                65.38, 65.64, 65.95, 66.23, 66.39, 66.43, 66.84, 
                                                                                67.01, 67.18, 67.37, 67.57, 67.77, 67.98, 68.23, 
                                                                                68.46, 68.69, 68.86, 69.1, 69.32, 69.56, 69.79, 70.17, 
                                                                                70.79, 71.13, 71.36, 71.64, 71.91, 71.98, 72.18, 
                                                                                72.31, 72.49, 72.76, 73.14, 73.45, 73.5, 73.5, 72, 
                                                                                72, 69.8, 62, 62, 63)) > 0] <- ifelse(roman, "IIa2", 
                                                                                                                      "2a2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(7.21, -0.2, -0.48, -1.88, -2.7, -5, -4.38, 
                                         -4.29, -4.19, -4.3, -4.09, -2.52, -2.1, -1.6, 0.8, 
                                         1.12, 1.71, 3.06, 4.07, 4.55, 5.19, 6.39, 6.51, 6.74, 
                                         7.06, 7.21), pol.y = c(73.5, 73.5, 73.6, 73.94, 74.09, 
                                                                74.21, 74.5, 75, 75.3, 76.05, 76.18, 76.57, 76.67, 
                                                                76.56, 76, 75.87, 75.64, 75.21, 74.96, 74.86, 74.69, 
                                                                74.34, 74.13, 73.89, 73.6, 73.5)) > 0] <- ifelse(roman, 
                                                                                                                 "IIb1", "2b1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -0.199, -0.479, -1.879, -2.699, -4.999, 
                                         -4.379, -4.289, -4.189, -4.299, -4.089, -2.519, -2.099, 
                                         -1.599, 0.8, 1.12, 1.71, 3.06, 4.07, 4.55, 5.19, 
                                         6.39, 6.51, 6.74, 7.06, 7.21, 30, 30, -11), pol.y = c(73.5, 
                                                                                               73.5, 73.6, 73.94, 74.09, 74.21, 74.5, 75, 75.3, 
                                                                                               76.05, 76.18, 76.57, 76.67, 76.56, 76, 75.87, 75.64, 
                                                                                               75.21, 74.96, 74.86, 74.69, 74.34, 74.13, 73.89, 
                                                                                               73.6, 73.5, 73.5, 90, 90)) > 0] <- ifelse(roman, 
                                                                                                                                         "IIb2", "2b2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377, 
                                         10.22958, 10.689431, 11.084742, 11.617201, 12.068985, 
                                         11.972174, 10.59262, 9.971417, 9.39862, 8.648336), 
                               pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015, 
                                         59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, 
                                         57.74247, 57.50441, 57.10708, 57.08073)) > 0] <- ifelse(roman, 
                                                                                                 "IIIan", "3an")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992, 
                                         12.80622, 12.95073, 12.72185, 12.45127, 12.29556, 
                                         12.13384, 11.99063, 11.58487, 11.58487, 11.63281, 
                                         11.49492, 11.3094, 11.27652, 10.71374, 10.70218, 
                                         10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247, 
                                                                                            57.4653, 57.48032, 56.94085, 56.46389, 56.36135, 
                                                                                            56.19091, 56.16918, 56.29535, 56.12728, 55.49119, 
                                                                                            55.28764, 55.63113, 55.91101, 55.90623, 55.94866, 
                                                                                            55.97965, 56.00988, 56.14253, 56.25853, 56.49587, 
                                                                                            57.11107, 57.63566, 57.74247)) > 0] <- ifelse(roman, 
                                                                                                                                          "IIIas", "3as")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4), 
                               pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2, 
                                         58.4)) > 0] <- ifelse(roman, "IVa", "4a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2, 
                                         9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073, 
                                                                              57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) > 
                0] <- ifelse(roman, "IVb", "4b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51, 
                                                                      53.5)) > 0] <- ifelse(roman, "IVc", "4c")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-24, -27, -27, -24, -24), pol.y = c(62, 62, 
                                                                             63, 63, 62)) > 0] <- ifelse(roman, "Va1", "5a1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-27, -11, -11, -15, -15, -24, -24, -27), pol.y = c(68, 
                                                                                            68, 63, 63, 62, 62, 63, 63)) > 0] <- ifelse(roman, 
                                                                                                                                        "Va2", "5a2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-13.5, -15, -15, -14, -13.29, -13.5), pol.y = c(60, 
                                                                                         60, 60, 60.7, 60.15, 60)) > 0] <- ifelse(roman, "Vb1a", 
                                                                                                                                  "5b1a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-4, -5, -5, -8, -8, -7.5, -7.5, -8, -10, -10, 
                                         -12, -13.5, -13.29, -13.99, -15, -15, -15, -11, -4), 
                               pol.y = c(60.5, 60.5, 60, 60, 60.5, 60.5, 61.25, 61.5, 
                                         61.5, 60, 60, 60, 60.15, 60.71, 60.49, 62, 63, 63, 
                                         63)) > 0] <- ifelse(roman, "Vb1b", "5b1b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-8, -10, -10, -8, -7.5, -7.5, -8, -8), pol.y = c(60, 
                                                                                          60, 61.5, 61.5, 61.25, 60.5, 60.5, 60)) > 0] <- ifelse(roman, 
                                                                                                                                                 "Vb2", "5b2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-4, -4, -4.6, -4.6, -4, -6, -8, -12, -12, -5, 
                                         -5), pol.y = c(60.5, 58.4, 58.2, 57.3, 55, 55, 54.5, 
                                                        54.5, 60, 60, 60.5)) > 0] <- ifelse(roman, "VIa", 
                                                                                            "6a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-15.99, -18, -18, -15, -13.5, -13.99, -14.57, 
                                         -14.79, -14.88, -14.63, -14.34, -14.44, -14.54, -14.62, 
                                         -14.72, -14.8, -14.89, -14.97, -15.04, -15.11, -15.19, 
                                         -15.27, -15.34, -15.41, -15.47, -15.54, -15.6, -15.65, 
                                         -15.7, -15.75, -15.79, -15.83, -15.87, -15.9, -15.92, 
                                         -15.95, -15.97, -15.99, -15.99), pol.y = c(54.5, 
                                                                                    54.5, 60, 60, 60, 59.65, 59.01, 58.51, 57.87, 57.01, 
                                                                                    56.57, 56.5, 56.44, 56.37, 56.31, 56.24, 56.17, 56.09, 
                                                                                    56.02, 55.95, 55.88, 55.8, 55.73, 55.65, 55.57, 55.5, 
                                                                                    55.42, 55.34, 55.26, 55.18, 55.09, 55.01, 54.93, 
                                                                                    54.84, 54.76, 54.68, 54.59, 54.51, 54.5)) > 0] <- ifelse(roman, 
                                                                                                                                             "VIb1", "6b1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-12, -15.99, -15.99, -15.97, -15.95, -15.92, 
                                         -15.9, -15.87, -15.83, -15.79, -15.75, -15.7, -15.65, 
                                         -15.6, -15.54, -15.47, -15.41, -15.34, -15.27, -15.19, 
                                         -15.11, -15.04, -14.97, -14.89, -14.8, -14.72, -14.62, 
                                         -14.54, -14.44, -14.34, -14.63, -14.88, -14.79, -14.57, 
                                         -13.99, -13.5, -12, -12), pol.y = c(54.5, 54.5, 54.51, 
                                                                             54.59, 54.68, 54.76, 54.84, 54.93, 55.01, 55.09, 
                                                                             55.18, 55.26, 55.34, 55.42, 55.5, 55.57, 55.65, 55.73, 
                                                                             55.8, 55.88, 55.95, 56.02, 56.09, 56.17, 56.24, 56.31, 
                                                                             56.37, 56.44, 56.5, 56.57, 57.01, 57.87, 58.51, 59.01, 
                                                                             59.65, 60, 60, 54.5)) > 0] <- ifelse(roman, "VIb2", 
                                                                                                                  "6b2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-6.3, -7.8, -2.5, -2.5), pol.y = c(55, 52, 
                                                                            52, 55)) > 0] <- ifelse(roman, "VIIa", "7a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-8, -8, -12, -12), pol.y = c(54.5, 52.5, 52.5, 
                                                                      54.5)) > 0] <- ifelse(roman, "VIIb", "7b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-16.06, -18, -18, -15.99, -15.99, -16, -16.01, 
                                         -16.01, -16.01, -16, -15.99, -15.97, -15.96, -15.94, 
                                         -15.91, -15.9, -15.89, -15.88, -15.86, -15.84, -15.88, 
                                         -15.92, -15.95, -15.98, -16, -16.02, -16.04, -16.06, 
                                         -16.06), pol.y = c(52.5, 52.5, 54.5, 54.5, 54.42, 
                                                            54.34, 54.25, 54.17, 54.08, 53.99, 53.91, 53.82, 
                                                            53.74, 53.66, 53.57, 53.49, 53.42, 53.34, 53.2, 53.18, 
                                                            53.1, 53.02, 52.94, 52.86, 52.77, 52.69, 52.61, 52.52, 
                                                            52.5)) > 0] <- ifelse(roman, "VIIc1", "7c1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-12, -16.06, -16.06, -16.04, -16.02, -16, -15.98, 
                                         -15.95, -15.92, -15.88, -15.84, -15.86, -15.88, -15.89, 
                                         -15.9, -15.91, -15.94, -15.96, -15.97, -15.99, -16, 
                                         -16.01, -16.01, -16.01, -16, -15.99, -15.99, -12, 
                                         -12), pol.y = c(52.5, 52.5, 52.52, 52.61, 52.69, 
                                                         52.77, 52.86, 52.94, 53.02, 53.1, 53.18, 53.26, 53.34, 
                                                         53.42, 53.49, 53.57, 53.66, 53.74, 53.82, 53.91, 
                                                         53.99, 54.08, 54.17, 54.25, 54.34, 54.42, 54.5, 54.5, 
                                                         52.5)) > 0] <- ifelse(roman, "VIIc2", "7c2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(2, -2, -2, -1.956, -1, 2), pol.y = c(51, 51, 
                                                                              50.6, 49.705, 49, 49)) > 0] <- ifelse(roman, "VIId", 
                                                                                                                    "7d")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-2, -2.22, -5.17, -5.24, -7, -7, -5, -5, -1, 
                                         -1, -1.956), pol.y = c(50.6, 50.88, 50.21, 50, 50, 
                                                                49.5, 49.5, 48, 48, 49.3, 49.705)) > 0] <- ifelse(roman, 
                                                                                                                  "VIIe", "7e")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-7, -7, -5.2, -5.2, -2.4, -2.4, -5, -5, -6, 
                                         -6), pol.y = c(50.5, 50, 50, 50.25, 51.4, 51.9, 51.9, 
                                                        51, 51, 50.5)) > 0] <- ifelse(roman, "VIIf", "7f")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-9, -7, -7, -6, -6, -5, -5, -9), pol.y = c(50, 
                                                                                    50, 50.5, 50.5, 51, 51, 52, 52)) > 0] <- ifelse(roman, 
                                                                                                                                    "VIIg", "7g")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-5, -8, -9, -9, -7, -7, -5, -5), pol.y = c(48, 
                                                                                    48, 48, 50, 50, 49, 49, 48)) > 0] <- ifelse(roman, 
                                                                                                                                "VIIh", "7h")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-10.64, -11, -12, -12, -11.99, -11.87, -11.75, 
                                         -11.64, -11.52, -11.39, -11.27, -11.14, -11.02, -10.89, 
                                         -10.77, -10.68, -10.64), pol.y = c(48, 48, 48, 48.43, 
                                                                            48.42, 48.39, 48.36, 48.33, 48.3, 48.27, 48.25, 48.23, 
                                                                            48.21, 48.19, 48.17, 48.03, 48)) > 0] <- ifelse(roman, 
                                                                                                                            "VIIj1", "7j1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-9, -9, -10.64, -10.68, -10.77, -10.89, -11.02, 
                                         -11.14, -11.27, -11.39, -11.52, -11.64, -11.75, -11.87, 
                                         -11.99, -12, -12), pol.y = c(52.5, 48, 48, 48.03, 
                                                                      48.17, 48.19, 48.21, 48.23, 48.25, 48.27, 48.3, 48.33, 
                                                                      48.36, 48.39, 48.42, 48.43, 52.5)) > 0] <- ifelse(roman, 
                                                                                                                        "VIIj2", "7j2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-12, -12, -18, -18, -16.06, -16.07, -16.08, 
                                         -16.09, -16.09, -16.09, -16.08, -16.07, -16.07, -16.07, 
                                         -16.04, -16.02, -16, -15.96, -15.93, -15.9, -15.86, 
                                         -15.82, -15.77, -15.73, -15.68, -15.63, -15.57, -15.52, 
                                         -15.47, -15.42, -15.36, -15.3, -15.24, -15.17, -15.11, 
                                         -15.04, -14.97, -14.89, -14.82, -14.74, -14.65, -14.57, 
                                         -14.48, -14.39, -14.3, -14.22, -14.13, -14.04, -13.95, 
                                         -13.86, -13.77, -13.67, -13.57, -13.47, -13.37, -13.27, 
                                         -13.17, -13.07, -12.96, -12.85, -12.74, -12.64, -12.54, 
                                         -12.43, -12.32, -12.22, -12.11, -12), pol.y = c(48.43, 
                                                                                         48, 48, 52.5, 52.5, 52.44, 52.36, 52.27, 52.19, 52.11, 
                                                                                         52.02, 51.94, 51.85, 51.77, 51.68, 51.6, 51.52, 51.43, 
                                                                                         51.34, 51.27, 51.18, 51.1, 51.02, 50.94, 50.86, 50.78, 
                                                                                         50.7, 50.62, 50.54, 50.47, 50.39, 50.32, 50.24, 50.17, 
                                                                                         50.1, 50.03, 49.96, 49.89, 49.82, 49.75, 49.69, 49.62, 
                                                                                         49.56, 49.5, 49.44, 49.38, 49.32, 49.27, 49.21, 49.15, 
                                                                                         49.1, 49.05, 49, 48.95, 48.9, 48.86, 48.81, 48.77, 
                                                                                         48.73, 48.69, 48.65, 48.62, 48.58, 48.55, 48.52, 
                                                                                         48.49, 48.46, 48.43)) > 0] <- ifelse(roman, "VIIk1", 
                                                                                                                              "7k1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-12, -12.11, -12.22, -12.32, -12.43, -12.54, 
                                         -12.64, -12.74, -12.85, -12.96, -13.07, -13.17, -13.27, 
                                         -13.37, -13.47, -13.57, -13.67, -13.77, -13.86, -13.95, 
                                         -14.04, -14.13, -14.22, -14.3, -14.39, -14.48, -14.57, 
                                         -14.65, -14.74, -14.82, -14.89, -14.97, -15.04, -15.11, 
                                         -15.17, -15.24, -15.3, -15.36, -15.42, -15.47, -15.52, 
                                         -15.57, -15.63, -15.68, -15.73, -15.77, -15.82, -15.86, 
                                         -15.9, -15.93, -15.96, -15.99, -16.02, -16.04, -16.05, 
                                         -16.07, -16.07, -16.08, -16.09, -16.09, -16.09, -16.08, 
                                         -16.07, -16.06, -12, -12), pol.y = c(48.43, 48.46, 
                                                                              48.49, 48.52, 48.55, 48.58, 48.62, 48.65, 48.69, 
                                                                              48.73, 48.77, 48.81, 48.86, 48.9, 48.95, 49, 49.05, 
                                                                              49.1, 49.15, 49.21, 49.27, 49.32, 49.38, 49.44, 49.5, 
                                                                              49.56, 49.62, 49.69, 49.75, 49.82, 49.89, 49.96, 
                                                                              50.03, 50.1, 50.17, 50.24, 50.32, 50.39, 50.47, 50.54, 
                                                                              50.62, 50.7, 50.78, 50.86, 50.94, 51.02, 51.1, 51.18, 
                                                                              51.27, 51.34, 51.43, 51.52, 51.6, 51.68, 51.77, 51.85, 
                                                                              51.94, 52.02, 52.11, 52.19, 52.27, 52.36, 52.44, 
                                                                              52.5, 52.5, 48.43)) > 0] <- ifelse(roman, "VIIk2", 
                                                                                                                 "7k2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-8, -8, -6, -6, -5, -5, -0.5, -0.5), pol.y = c(48, 
                                                                                        47.5, 47.5, 47, 57, 46, 46, 48)) > 0] <- ifelse(roman, 
                                                                                                                                        "VIIIa", "8a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-4, -4, -3, -3, -2, -2, 0, 0), pol.y = c(46, 
                                                                                  45.5, 45.5, 44.5, 44.5, 43, 43, 46)) > 0] <- ifelse(roman, 
                                                                                                                                      "VIIIb", "8b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -11, -2, -2), pol.y = c(43, 44.5, 44.5, 
                                                                      43)) > 0] <- ifelse(roman, "VIIIc", "8c")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -11, -10.64, -10.37, -9.89, -9.62, -10.95, 
                                         -11), pol.y = c(46.32, 48, 48, 47.77, 47.45, 46.88, 
                                                         46.34, 46.32)) > 0] <- ifelse(roman, "VIIId1", "8d1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-4, -4, -3, -3, -11, -11, -10.95, -9.62, -9.89, 
                                         -10.37, -10.64, -9, -8, -8, -6, -5, -4, -4, -4), 
                               pol.y = c(46, 45.5, 45.5, 44.5, 44.5, 46.32, 46.34, 46.88, 
                                         47.45, 47.77, 48, 48, 48, 47.5, 47.5, 47, 47, 46, 
                                         46)) > 0] <- ifelse(roman, "VIIId2", "8d2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(11, -13.31, -13.49, -13.8, -18, -18, -12, -11, 
                                         -11), pol.y = c(46.32, 44.72, 44.07, 43, 43, 48, 
                                                         48, 48, 46.32)) > 0] <- ifelse(roman, "VIIIe1", "8e1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -11, -13.8, -13.49, -13.31, -11, -11), 
                               pol.y = c(44.5, 43, 43, 44.07, 44.72, 46.32, 44.5)) > 
                0] <- ifelse(roman, "VIIIe2", "8e2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -11, -5.5, -5.5), pol.y = c(36, 43, 43, 
                                                                          36)) > 0] <- ifelse(roman, "IXa", "9a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-17, -18, -13.8, -13.84, -13.64, -13.27, -13.27, 
                                         -13.49, -13.78, -13.69, -12.73, -15.3, -17.9, -18), 
                               pol.y = c(36, 43, 43, 42.88, 42.04, 41.38, 41.13, 40.06, 
                                         38.75, 38.17, 36.03, 36.04, 36.02, 36)) > 0] <- ifelse(roman, 
                                                                                                "IXb1", "9b1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -18, -17.9, -15.3, -12.73, -13.69, -13.78, 
                                         -13.49, -13.27, -13.27, -13.64, -13.84, -13.8, -11, 
                                         -11), pol.y = c(36, 36, 36, 36.04, 36.03, 38.17, 
                                                         38.75, 40.06, 41.13, 41.38, 42.04, 42.88, 43, 43, 
                                                         36)) > 0] <- ifelse(roman, "IXb2", "9b2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-18, -22.25, -20.62, -21.32, -23.91, -24.65, 
                                         -25.79, -28.45, -29.95, -35.11, -35.26, -35.48, -31.76, 
                                         -32.03, -42, -42, -18, -18), pol.y = c(36, 36, 37.58, 
                                                                                39.16, 40.97, 41.35, 41.91, 42.34, 42.05, 41.02, 
                                                                                40.04, 38.74, 36.03, 36, 36, 43, 43, 36)) > 0] <- ifelse(roman, 
                                                                                                                                         "Xa1", "10a1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-22.25, -31.76, -32.03, -35.48, -35.26, -35.11, 
                                         -29.95, -28.45, -25.79, -24.65, -23.91, -21.32, -20.62, 
                                         -22.25), pol.y = c(36, 36, 36.03, 38.74, 40.04, 41.02, 
                                                            42.05, 42.34, 41.91, 41.35, 40.97, 39.16, 37.58, 
                                                            36)) > 0] <- ifelse(roman, "Xa2", "10a2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-18, -42, -42, -18, -18), pol.y = c(43, 43, 
                                                                             48, 48, 43)) > 0] <- ifelse(roman, "Xb", "10b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-20.55, -24, -24, -18, -18, -42, -42, -41.5, 
                                         -41, -40.5, -40, -39.5, -39, -38.5, -38.25, -38, 
                                         -37.5, -37.2, -37, -36.77, -27, -27, -26.46, -25.09, 
                                         -23.96, -23.27, -21.77, -20.57), pol.y = c(60, 60, 
                                                                                    54.5, 54.5, 52.5, 52.5, 56.55, 56.64, 56.75, 56.88, 
                                                                                    57.03, 57.2, 57.37, 57.62, 57.78, 57.97, 58.26, 58.5, 
                                                                                    58.63, 59, 59, 60.85, 60.69, 60.45, 60.37, 60.22, 
                                                                                    60.02, 60)) > 0] <- ifelse(roman, "XIIa1", "12a1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-20.55, -18.65, -17.32, -15.22, -15, -15, -18, 
                                         -20.55), pol.y = c(60, 60.05, 60.11, 60.44, 60.49, 
                                                            60, 60, 60)) > 0] <- ifelse(roman, "XIIa2", "12a2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-42, -36.77, -37, -37.2, -37.5, -38, -38.25, 
                                         -38.5, -39, -39.5, -40, -40.5, -41, -41.5, -42, -42), 
                               pol.y = c(59, 59, 58.63, 58.5, 58.26, 57.97, 57.78, 57.62, 
                                         57.37, 57.2, 57.03, 56.88, 56.75, 56.64, 56.55, 59)) > 
                0] <- ifelse(roman, "XIIa3", "12a3")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-27, -27, -24, -15, -15, -15.22, -17.32, -18.65, 
                                         -20.55, -21.76, -23.27, -23.96, -25.09, -26.46, -27), 
                               pol.y = c(60.85, 62, 62, 62, 60.49, 60.44, 60.11, 60.05, 
                                         60, 60.02, 60.22, 60.37, 60.45, 60.69, 60.85)) > 
                0] <- ifelse(roman, "XIIa4", "12a4")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-18, -24, -24, -20.55, -18, -18), pol.y = c(54.5, 
                                                                                     54.5, 60, 60, 60, 54.5)) > 0] <- ifelse(roman, "XIIb", 
                                                                                                                             "12b")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-18, -42, -42, -18, -18), pol.y = c(48, 48, 
                                                                             52.5, 52.5, 48)) > 0] <- ifelse(roman, "XIIc", "12c")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-11, -27, -27, -40, -40, -11), pol.y = c(68, 
                                                                                  68, 68.57, 68.57, 90, 90)) > 0] <- ifelse(roman, 
                                                                                                                            "XIVa", "14a")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-27, -27, -36.77, -36.5, -36.35, -36.16, -35.96, 
                                         -35.76, -35.5, -35.37, -35.15, -34.97, -34.65, -34.5, 
                                         -34.31, -34, -33.7, -33.53, -33.27, -33, -32.5, -32.3, 
                                         -32, -31.5, -31, -30.86, -30.61, -29.87, -29.25, 
                                         -28.61, -27.69, -27), pol.y = c(60.85, 59, 59, 59.35, 
                                                                         59.5, 59.75, 60, 60.25, 60.55, 60.75, 61, 61.25, 
                                                                         61.5, 61.6, 61.75, 61.98, 62.25, 62.45, 62.5, 62.56, 
                                                                         62.69, 62.75, 62.87, 63.03, 63.25, 63.31, 63, 62.23, 
                                                                         61.79, 61.44, 61.06, 60.85)) > 0] <- ifelse(roman, 
                                                                                                                     "XIVb1", "14b1")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(-27, -27, -27.69, -28.61, -29.25, -29.87, -30.61, 
                                         -30.86, -31, -31.5, -32, -32.3, -32.5, -33, -33.27, 
                                         -33.53, -33.7, -34, -34.31, -34.5, -34.65, -34.97, 
                                         -35.15, -35.37, -35.5, -35.76, -35.96, -36.16, -36.35, 
                                         -36.5, -36.77, -42, -44, -44), pol.y = c(68.6, 60.85, 
                                                                                  61.06, 61.44, 61.79, 62.23, 63, 63.31, 63.25, 63.03, 
                                                                                  62.87, 62.75, 62.69, 62.56, 62.5, 62.45, 62.25, 61.98, 
                                                                                  61.75, 61.6, 61.5, 61.25, 61, 60.75, 60.55, 60.25, 
                                                                                  60, 59.75, 59.5, 59.35, 59, 59, 59, 68.6)) > 0] <- ifelse(roman, 
                                                                                                                                            "XIVb2", "14b2")
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78, 
                                         14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75, 
                                                                              54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4, 
                                                                              55.5, 55.38, 55.33)) > 0] <- "24"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4, 
                                                                                       55.3, 55, 53, 53, 56.5, 56.5)) > 0] <- "25"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5, 
                                                                    56.5)) > 0] <- "26"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5, 
                                                                                          57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0] <- "27"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98, 
                                         22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58, 
                                                                         57, 57, 56.65, 56.5, 57.57, 57.97, 58.04, 58.15, 
                                                                         58.5)) > 0] <- "28-1"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23, 
                                         25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15, 
                                                            58.35, 58.5, 58.5, 56.5)) > 0] <- "28-2"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191, 
                                         23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92, 
                                         19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965, 
                                                        59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5, 
                                                        58.35, 58.5, 58.5)) > 0] <- "29"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5, 
                                                                                        63.7, 63.7, 63.5, 63.5, 60.5)) > 0] <- "30"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7, 
                                                                                  63.5, 63.5, 67, 67)) > 0] <- "31"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5), 
                               pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) > 
                0] <- "32"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21, 
                                         12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38, 
                                                                  55.41, 55.71, 56.29, 56.305)) > 0] <- "23"
    ICES.area[point.in.polygon(point.x = chrons$shootlon, point.y = chrons$shootlat, 
                               pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94, 
                                         11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15, 
                                                                             55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75, 
                                                                             56.6)) > 0] <- "22"
    return(ICES.area)
  }



# NOT WORKING?
remove_roman <- function(area) {
      area  <- gsub("xiv" ,"14", area)
      area  <- gsub("xiii","13", area)
      area  <- gsub("xii" ,"12", area)
      area  <- gsub("xi"  ,"11", area)
      area  <- gsub("ix"  ,"9" , area)
      area  <- gsub("x"   ,"10", area)
      area  <- gsub("viii","8" , area)
      area  <- gsub("vii" ,"7" , area)
      area  <- gsub("vi"  ,"6" , area)
      area  <- gsub("iv"  ,"4" , area)
      area  <- gsub("v"   ,"5" , area)
      area  <- gsub("iii" ,"3" , area)
      area  <- gsub("ii"  ,"2" , area)
      area  <- gsub("i"   ,"1" , area)
      return(area)
}

# -----------------------------------------------------------------------------------

# csquare functions
#   csquare_encode
#   csquare_triplet - helper function
#   csquare_area
#   csquare_decode
#   csquare_decode_lat - internal function, used by csquare_lat
#   csquare_lat
#   csquare_decode_lon - internal function, used by csquare_lon
#   csquare_lon

#' @title Calculate the C-squares from degrees longitudes and latitudes
#' 
#' @description C-square: A notation system of c-squares provides a compact 
#' encoding of latitude and longitude coordinates into a machine- and 
#' human-readable code. See https://en.wikipedia.org/wiki/C-squares
#' 
#' @param lat A vector or decimal degrees latitude
#' @param lon A vector of cecimal degrees longitude
#' @param resolution A value specifying the returned resolution of C-squares: 
#' 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 in degree units
#'
#' @return A character vector
#' @export

csquare_encode <- function(lat, lon, resolution) {
  
  if(length(lon) != length(lon)) stop("length of longitude not equal to length of latitude")
  if(!resolution %in% c(10,5,1,0.5,0.1,0.05,0.01)) stop("resolution not in range: c(10,5,1,0.5,0.1,0.05,0.01)")
  
  lat.abs <- abs(lat)
  lon.abs <- abs(lon)
  
  # 10 degree square - first 4 characters
  g = 4-(((2*trunc(1 + (lon/200)))-1) * ((2 * trunc(1 + (lat/200)))+1))
  y = lat.abs%/%10 #trunc(lat.abs/10)
  x = lon.abs%/%10 # trunc(lon.abs/10)
  csquare <- paste0(g,y*10,x)
  if(resolution == 10) return(csquare)
  
  #  5 degree square
  csquare <- paste0(csquare,":",code_triplet(lat.abs%%10,lon.abs%%10))
  if(resolution == 5) {
    n = nchar(csquare) - 2
    return(substr(csquare,1,n))
  }
  #  1 degree square
  if(resolution == 1) return(csquare)
  
  #   0.5 degree square
  csquare <- paste0(csquare,":",code_triplet(lat.abs%%1 * 10,lon.abs%%1 * 10))
  if(resolution == 0.5) {
    n = nchar(csquare) - 2
    return(substr(csquare,1,n))
  }
  # 0.1 degree square
  if(resolution == 0.1) return(csquare)
  
  # 0.05 degree square
  csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.1 * 100,lon.abs%%0.1 * 100))
  if(resolution == 0.05) {
    n = nchar(csquare) - 2
    return(substr(csquare,1,n))
  }
  # 0.01 degree square
  if(resolution == 0.01) return(csquare)
  
  # ... on and on:
  #csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.01 * 1000,lon.abs%%0.01 * 1000))
  #csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.001 * 10000,lon.abs%%0.001 * 10000))
  #csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.0001 * 100000,lon.abs%%0.0001 * 100000))
  #return(csquare)
}

# -----------------------------------------------------------------------------------

#' @title Returns a triplet C-square code
#' 
#' @description Internal function, used in \code{csquare}.
#'
#' @param lat A vector of the "remainder" of the lat degrees
#' @param lon A vector of the "remainder" of the lon degrees
#'
#' @return A triplet character vector of numbers
#'
code_triplet <- function(lat, lon) {
  digit1 <- (2*trunc(lat * 0.2)) + trunc(lon *0.2) + 1
  digit2 <- trunc(lat)
  digit3 <- trunc(lon)
  return(paste0(digit1,digit2,digit3))
}

# -----------------------------------------------------------------------------------

#' @title Calculate csquare area
#' 
#' @description Internal function as of now. Think there is a
#' function in DATRAS that does things better.
#' 
#' @export 
#'
#' @param x A c-square 
#' @param method Default ("geo") implemented
#'
csquare_area <- function(x, method = "geo") {
  
  # center point
  x <-   csquare_decode(x) 
  # create a "polygon" for one csquare
  x <- data.frame(lon = c(x$lon-0.025,x$lon-0.025,x$lon+0.025,x$lon+0.025,x$lon-0.025),
                  lat = c(x$lat-0.025,x$lat+0.025,x$lat+0.025,x$lat-0.025,x$lat-0.025))
  # calculate area
  if(method == "geo") {
    x <- geo::geoarea(x)
    return(x)
  }
  
  
  #if(method != "geo") {
  #  x <- sp::Polygon(x[,c("lon","lat")]) %>%
  #    list() %>%
  #    Polygons(ID = "1") %>%
  #    list() %>%
  #    SpatialPolygons(proj4string = PRO) %>%
  #    geo_area()
  #  return(x)
  #}
}


# -----------------------------------------------------------------------------------

#' @title Calculate the C-square resolution
#' 
#' @description Calculates the resolution of a C-square code
#' 
#' @export
#' 
#' @param csquare A string of csquare codes
#' 
csquare_resolution <- function(csquare) {
  n <- nchar(csquare)  # length of character determines resolution
  r <- 10^(1 - floor((n-4)/4))  - 
    ((round((n-4)/4,1) - floor((n-4)/4)) * 10^(1-floor((n-4)/4))) 
  return(r)
}

# -----------------------------------------------------------------------------------

#' @title Calculate the longitudes and latitudes from C-squares
#'
#' @description C-square: A notation system of c-squares provides a compact 
#' encoding of latitude and longitude coordinates into a machine- and 
#' human-readable code. See https://en.wikipedia.org/wiki/C-squares
#' 
#' @export
#' 
#' @return A data frame with longitude (lon) and latitudes (lat)
#'
#' @param x A string of csquare codes
#' @param resolution A value specifying the returned resolution of C-squares: 
#' 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 in degree units
#' @param baf a value if default (0) no adjustment made. May only be of use for boundary values (-180/180 and -90/90).

csquare_decode <- function(x, resolution, baf = 0) {
  
  if(missing(resolution)) {
    resolution <- csquare_resolution(x)
  }
  
  if(!resolution %in% c(10,5,1,0.5,0.1,0.05,0.01)) stop("resolution not in range: c(10,5,1,0.5,0.1,0.05,0.01)")
  
  ### --------------------------------------------------------------------------
  # second trial
  
  # put in check here if .e.g. mismatch between res and resolution
  
  g1     = as.integer(substr(x,1,1))
  g1lat  = as.integer(substr(x,2,2))
  g1lon  = as.integer(substr(x,3,4))
  
  g2     = as.integer(substr(x,6,6))
  g2lat  = as.integer(substr(x,7,7))
  g2lon  = as.integer(substr(x,8,8))
  g2lat2 = round(g2*2,-1)/10
  g2lon2 = (round((g2-1)/2,1) - trunc((g2-1)/2)) * 2
  
  g3     = as.integer(substr(x,10,10))
  g3lat  = as.integer(substr(x,11,11))
  g3lon  = as.integer(substr(x,12,12))
  g3lat2 = round(g3*2,-1)/10
  g3lon2 = (round((g3-1)/2,1) - trunc((g3-1)/2)) * 2
  
  g4     = as.integer(substr(x,14,14))
  g4lat  = as.integer(substr(x,15,15))
  g4lon  = as.integer(substr(x,16,16))
  g4lat2 = round(g4*2,-1)/10
  g4lon2 = (round((g4-1)/2,1) - trunc((g4-1)/2)) * 2
  
  signY  = (round(abs(g1 - 4) * 2,-1)/5)-1
  signX  = ((2 * (round(g1,-1)/10)) - 1) * -1
  
  # central position
  if(resolution == 10) {
    lat1 = ((g1lat*10) + 5) * signY
    lon1 = ((g1lon*10) + 5) * signX
    return(data.frame(lat = lat1, lon = lon1))
  }
  if(resolution == 5) {
    lat2 = ((g1lat*10) + (g2lat2 * 5) + 2.5) * signY
    lon2 = ((g1lon*10) + (g2lon2 * 5) + 2.5) * signX
    return(data.frame(lat = lat2, lon = lon2))
  }
  if(resolution == 1) {
    lat3 = ((g1lat*10) + g2lat + 0.5) * signY
    lon3 = ((g1lon*10) + g2lon + 0.5) * signX
    return(data.frame(lat = lat3, lon = lon3))
  }
  if(resolution == 0.5) {
    lat4 = ((g1lat*10) + g2lat + (g3lat2 * 0.5) + 0.25) * signY
    lon4 = ((g1lon*10) + g2lon + (g3lon2 * 0.5) + 0.25) * signX
    return(data.frame(lat = lat4, lon = lon4))
  }
  if(resolution == 0.1) {
    lat5 = ((g1lat*10) + g2lat + (g3lat * 0.1) + 0.05) * signY
    lon5 = ((g1lon*10) + g2lon + (g3lon * 0.1) + 0.05) * signX
    return(data.frame(lat = lat5, lon = lon5))
  }
  if(resolution == 0.05) {
    lat6 = ((g1lat*10) + g2lat + (g3lat * 0.1) + (g4lat2 * 0.05) + 0.025) * signY
    lon6 = ((g1lon*10) + g2lon + (g3lon * 0.1) + (g4lon2 * 0.05) + 0.025) * signX
    return(data.frame(lat = lat6, lon = lon6))
  }
  if(resolution == 0.01) {
    lat7 = ((g1lat*10) + g2lat + (g3lat * 0.1) + (g4lat * 0.01) + 0.005)  * signY
    lon7 = ((g1lon*10) + g2lon + (g3lon * 0.1) + (g4lon * 0.01) + 0.005)  * signX
    return(data.frame(lat = lat7, lon = lon7))
  }
}

# -----------------------------------------------------------------------------------

# internal - used in csquare_lat
csquare_decode_lat <- function(x, resolution, baf = 0) {
  csquare_decode(x = x, resolution = resolution)$lat
}

#' Rounded latitude given csquare resolution
#'
#' @param lat Degrees latitude
#' @param lon Degrees longitude
#' @param resolution A value specifying the C-square resolution. 
#' Valid values are 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 degree units
#'
#' @return A vector of rounded latitudes
#' @export
#'
csquare_lat <- function(lat, lon, resolution) {
  sq <- csquare_encode(lat = lat, lon = lon , resolution = resolution)
  lat <- csquare_decode_lat(sq, resolution = resolution)
  return(lat)
}

# internal - unsed in csquare_lon
csquare_decode_lon <- function(x, resolution, baf = 0) {
  csquare_decode(x = x, resolution = resolution)$lon
}

# -----------------------------------------------------------------------------------

#' Rounded longitude given csquare resolution
#'
#' @param lat Degrees latitude
#' @param lon Degrees longitude
#' @param resolution A value specifying the C-square resolution. 
#' Valid values are 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 degree units
#'
#' @return A vector of rounded longitudes
#' @export
#'
csquare_lon <- function(lat, lon, resolution) {
  sq <- csquare_encode(lat = lat, lon = lon , resolution = resolution)
  lon <- csquare_decode_lon(sq, resolution = resolution)
  return(lon)
}

# -----------------------------------------------------------------------------------

# Taken from http://www.hafro.is/~einarhj/education/tcrenv2017/b_highresolutiongridding.html
# 3/9/2017

# encode_zchords <- function(x, y, dx = 1, dy = 0.5 * dx, invalids = TRUE) {
#   
#   x.brks <- seq(floor(min(x)),ceiling(max(x)),dx)
#   x.ints <- findInterval(x, x.brks, all.inside = TRUE)
#   x <- (x.brks[x.ints] + x.brks[x.ints + 1]) / 2
#   
#   y.brks <- seq(floor(min(y)),ceiling(max(y)),dy)
#   y.ints <- findInterval(y, y.brks, all.inside = TRUE)
#   y <- (y.brks[y.ints] + y.brks[y.ints + 1]) / 2
#   
#   if(invalids) {
#     x <- ifelse(x >= -180 & x <= 180, x, NA)
#     y <- ifelse(y >= -90  & y <= 90 , y, NA)
#   }
#   
#   return(paste(round(x,6), round(y,6), sep = ":"))
#   
# }

encode_zchords <- function(x, y, dx = 1, dy = 0.5 * dx, invalids = TRUE) {
  
  x.brks <- seq(floor(min(x)), max(ceiling(max(x)), ceiling(min(x)+dx)), dx)

  x.ints <- findInterval(x, x.brks, all.inside = TRUE)
  x      <- (x.brks[x.ints] + x.brks[x.ints + 1]) / 2
  
  y.brks <- seq(floor(min(y)), max(ceiling(max(y)), ceiling(min(y)+dy)), dy)
  y.ints <- findInterval(y, y.brks, all.inside = TRUE)
  y      <- (y.brks[y.ints] + y.brks[y.ints + 1]) / 2
  
  if(invalids) {
    x <- ifelse(x >= -180 & x <= 180, x, NA)
    y <- ifelse(y >= -90  & y <= 90 , y, NA)
  }
  
  return(paste(round(x,6), round(y,6), sep = ":"))
  
}

# df <- 
#   data_frame(lon =    rnorm(n = 1e6, mean =  -28, sd =   6),
#                   lat =    rnorm(n = 1e6, mean =   64, sd =   0.3),
#                   effort = rnorm(n = 1e6, mean = 1000, sd = 200))  %>% 
#   mutate(sq = encode_zchords(lon, lat, dx = 0.05, dy = 0.025)) %>% 
#   group_by(sq) %>% 
#   summarise(effort = sum(effort) / 60/1000) %>% # scale to lets say thousand hours
#   separate(sq, c("lon", "lat"), sep = ":", convert = TRUE, remove = FALSE)

# -----------------------------------------------------------------------------------
# Taken from: https://www.r-bloggers.com/finding-my-dropbox-in-r/

get_dropbox <- function () {
  
  # install.packages("rjson")
  if (Sys.info()['sysname'] == 'Darwin') {
    info <- RJSONIO::fromJSON(
      file.path(path.expand("~"),'.dropbox','info.json'))
  }
  if (Sys.info()['sysname'] == 'Windows') {
    info <- RJSONIO::fromJSON(
      if (file.exists(file.path(Sys.getenv('APPDATA'), 'Dropbox','info.json'))) {
        file.path(Sys.getenv('APPDATA'), 'Dropbox', 'info.json')
      } else {
        file.path(Sys.getenv('LOCALAPPDATA'),'Dropbox','info.json')
      }
    )
  }
  
  dir <- info$personal$path
  return(dir)
}

# -----------------------------------------------------------------------------------

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

# get_onedrive()

# -----------------------------------------------------------------------------------
# Sortunique

su <- function(var) {
  return(sort(unique(var)))
}

sortunique <- function (var) {
  return(sort(unique(var)))
}

# -----------------------------------------------------------------------------------
# %notin%

`%notin%` <- Negate(`%in%`)

# -----------------------------------------------------------------------------------
# loadrdata

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Read specific object from RData file

loadRDataObject <- function(fileName, object){
  
  try(missing(fileName), "Stop: no fileName supplied")
  try(missing(object), "Stop: no object supplied")
  try(length(object)>1, "Stop: only one object can be supplied")
  load(fileName)
  get(ls()[ls() %in% object])
}


listDataObjects <- function(fileName){
  
  try(missing(fileName), "Stop: no fileName supplied")
  load(fileName, ex <- new.env())
  print(ls(ex)) 
  remove(ex)
}

plotallvars <- function(df){
  
  require(tidyverse)
  try(missing(df), "Stop: no df supplied")
  df %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram() +
    ggtitle(label=deparse(substitute(df)))
}

# -----------------------------------------------------------------------------------
# integer_breaks

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
#  scale_x_continuous(breaks= integer_breaks()) 

# -----------------------------------------------------------------------------------
# calculate_position_from_strings(shootlat, shootns, shootlong, shootew)

calculate_position_from_strings <- function(lat, ns, lon, ew) {
  
  lat = stringr::str_pad(lat, width=4, pad="0")
  ns  = toupper(ns)
  lon = stringr::str_pad(lon, width=5, pad="0")
  ew  = toupper(ew)
  return(
    list(lat = ifelse(ns=="N",1,-1) * (an(substr(lat,1,2)) + an(substr(lat,3,4))/60),
      lon = ifelse(ew=="W",-1,1) * (an(substr(lon,1,3)) + an(substr(lon,4,5))/60)) 
    )
}

# lat <- "5230"
# ns  <- "N"
# lon <- "1520"
# ew  <- "W"
# calculate_position_from_strings(lat, ns, lon, ew)$lat

calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

create_sf <- function(path, folder, layer, simplify=NA) {
  sf <-
    sf::st_read(dsn   = file.path(path, folder), layer = layer) %>% 
    sf::st_transform(crs = 4326) %>%
    sf::st_make_valid() %>% 
    { if(is.numeric(simplify)) {rmapshaper::ms_simplify(., keep = simplify, keep_shapes = TRUE) } else {.}}
  
  return(sf)
}

create_spatial <- function(path, folder, layer, simplify=NA) {
  sp  <-
    rgdal::readOGR(dsn=file.path(path, folder), layer=layer) %>%
    sp::spTransform(sp::CRS("+init=epsg:4326")) %>%  
    { if(is.numeric(simplify)) {rmapshaper::ms_simplify(., keep = simplify, keep_shapes = TRUE) } else {.}}
  
  return(sp)
}

create_spatial_df <- function(path, folder, layer, simplify=NA) {
  
  sp         <- create_spatial(path=path, folder=folder, layer=layer, simplify=simplify) 
  sp@data$id <- rownames(sp@data)
  sp.points  <- broom::tidy(sp)
  
  df  <- left_join(sp.points, sp@data, by="id")
  
  return(df)
}

create_spatial_df_from_sp <- function(sp) {
  
  if(class(sp) != "SpatialPolygonsDataFrame") stop("sp must be a SpatialPolygonsDataFrame")
  
  sp@data$id <- rownames(sp@data)
  sp.points  <- broom::tidy(sp)
  
  df  <- left_join(sp.points, sp@data, by="id")
  
  return(df)
}

