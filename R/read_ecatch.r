# ===============================================================================================
# code for reading ecatch XML files
#
# 26/04/2023 Still not completely finished; some errors arising from missing RTP or DEP messages
# 04/05/2023 Finalized the code to read ecatch 2.0 and 3.3
# 08/05/2023 Finalized the code; seems to work OK. sqsql function to get tripnumber from date range in ecatch 2.0
# ===============================================================================================

library(xml2)
library(tidyverse)
library(lubridate)
library(sqldf)
library(RColorBrewer)                # colour schemes

rm(list=ls())

source("../prf/r/my utils.R")
source("../mptools/R/get_onedrive.r")

spatialdir <- "C:/DATA/RDATA"

# spatial data files

load(file.path(spatialdir, "world_lr_sf.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))

load(file.path(spatialdir, "world_lr_df.RData"))
load(file.path(spatialdir, "world_mr_df.RData"))

rect_df <-
  loadRData(file.path(spatialdir, "rect_df.RData")) %>% 
  rename(rect=ICESNAME) %>% 
  group_by(rect) %>% 
  filter(row_number() ==1) %>% 
  dplyr::select(rect, lon=long, lat)


onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

# filelist <- list.files(
#   path=file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"),
#   pattern="xml",
#   recursive = TRUE,
#   full.names = TRUE)
# filelist <- filelist[grepl("ecatch", filelist)]

# save(filelist, file="C:/TEMP/filelist.RData")
load(file="C:/TEMP/filelist.RData")


# get the raw information from a file; check whether it is a log file or a correction file
get_raw <- function(xml, version, type) {
  
  if (type == "NLLOG") {
    
    raw <-
      tibble::as_tibble(xml) %>% 
      unnest_longer(NLERS) %>% 
      slice_tail() %>% 
      # filter(NLERS_id == "NLFAR") %>% 
      dplyr::select(1) %>% 
      unnest_longer(NLERS) 
    
  } else if (type == "NLCOR") {
    
    raw <-
      # suppressMessages(
      tibble::as_tibble(xml) %>% 
      unnest_longer(NLERS) %>% 
      
      # first the correction
      filter(NLERS_id=="NLLOG") %>% 
      dplyr::select(1) %>% 
      unnest_longer(NLERS) %>% 
      
      # now the message itself
      slice_tail() %>% 
      # filter(NLERS_id == "NLFAR") %>% 
      dplyr::select(1) %>% 
      unnest_longer(NLERS)
    
  } # end of NLLOG or NLCOR
  
  return(raw)

} # end of function


# Read NLFAR messages/files
read_nlfar <- function(xml, version, type) {
  
  raw <-
   suppressMessages(
      get_raw(xml, version, type) %>% 
        filter(NLERS_id == "NLGEA") %>% 
        slice_head() %>%              # CHECK: sometimes more than one row of NLGEA?? 
        dplyr::select(1) %>% 
        unnest_wider(NLERS, names_repair = "unique")
   )
  
  # if species in NLFAR
  if(any(grepl("NLSPE", names(raw)))) {
    
    nspecies <- sum(grepl("NLSPE", names(raw)))
    
    effort <-
      raw %>% 
      dplyr::select(!contains("NLSPE")) %>% 
      unnest_wider(NLGE) %>%
      unnest(names(.)) %>% 
      unnest(cols = names(.)) %>%   
      unnest(cols = names(.)) 
    
    catch <-
      raw %>% 
      dplyr::select(contains("NLSPE")) %>% 
      pivot_longer(names(.)) %>% 
      unnest_wider(value) %>%
      dplyr::select(!contains("NLGE")) %>% 
      unnest(cols = names(.)) %>% 
      unnest(cols = NLRAS) %>% 
      
      {if(nrow(.)/nspecies == 3) {
        bind_cols(., data.frame(variable=rep(c("FA","EZ", "SR"), nrow(.)/3)))
      } else if (nrow(.)/nspecies == 4) {
        bind_cols(., data.frame(variable=rep(c("FA","EZ", "SR", "FE"), nrow(.)/4)))
      } else {
        stop("not the right number of dimensions") 
      } } %>% 
      
      # bind_cols(data.frame(variable=rep(c("FA","EZ", "SR", "FE"), nrow(.)/4))) %>% 
      tidyr::pivot_wider(names_from = variable, values_from = NLRAS) %>% 
      dplyr::select(-name) %>% 
      unnest(cols = names(.)) %>%   
      unnest(cols = names(.)) %>% 
      dplyr::select(-any_of("GE"))
    
    
    # print("final")
    
    final <- 
      bind_cols(effort) %>% 
      bind_cols(catch) 
    
    # print(catch)
    
    return(final)
    
  } # end of NLSPE

} # end of function


# Read NLDEP messages
read_nldep <- function(xml, version, type) {
  
  dep <-
    get_raw(xml, version, type) %>% 
    # remove GE
    filter(NLERS_id %notin% c("NLGE","NLSPE")) %>% 
    tidyr::pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
    unnest(names(.)) %>% 
    unnest(names(.)) 

  # print(dep)
  
  return(dep)
    
}

# Read NLRTP messages
read_nlrtp <- function(xml, version, type) {
  
  rtp <-
    get_raw(xml, version, type) %>% 
    filter(NLERS_id %notin% c("NLQTB")) %>% 
    tidyr::pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
    unnest(names(.)) %>% 
    unnest(names(.)) 
  
  # print(rtp)
  
  return(rtp)
  
}

# Read header information of files
read_header <- function(xml, version, type) {

  if(type == "NLLOG") {
    
    header <- 
      bind_cols(
        # add general header
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          filter(is.na(NLERS_id)|NLERS_id=="") %>%
          dplyr::select(-NLERS_id) %>%
          bind_cols(., variable=names[-grep("NLLOG|NLCOR", names)]) %>% 
          pivot_wider(names_from = variable, values_from = NLERS) %>% 
          unnest(names(.)),
        # add version
        data.frame(version = version, stringsAsFactors = FALSE),
        # add message
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          slice_tail() %>% 
          dplyr::select(2) %>% 
          setNames("message"), 
        # add vessel and trip (trip only available in v3.3)
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          filter(NLERS_id %in% c("XR","TN")) %>%
          pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
          unnest(names(.)) %>% 
          unnest(names(.)) %>% 
          # glimpse()
          setNames(gsub("XR","vessel", names(.))) %>% 
          setNames(gsub("TN","trip", names(.))) %>% 
          mutate(vessel = gsub("-","",vessel))
      )

    return(header)
    
  } else if (type == "NLCOR") {
    
    header <- 
      bind_cols(
        # add general header
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          filter(is.na(NLERS_id)|NLERS_id=="") %>%
          dplyr::select(-NLERS_id) %>%
          bind_cols(., variable=names[-grep("NLLOG|NLCOR", names)]) %>% 
          pivot_wider(names_from = variable, values_from = NLERS) %>% 
          unnest(names(.)) ,
        
        # add version
        data.frame(version = version, stringsAsFactors = FALSE),
        
        # add message
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          slice_tail() %>% 
          dplyr::select(1) %>% 
          unnest_longer(NLERS) %>% 
          slice_tail() %>% 
          dplyr::select(2) %>% 
          setNames("message"),
        
        # add correction
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>%
          filter(NLERS_id %in% c("RN","RE")) %>% 
          # pivot_wider(names_from = NLERS_id, values_from = NLERS)  %>% 
          unnest_longer(NLERS) %>% 
          pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
          rename(RN2 = RN),
        
        # add vessel
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          
          # first the correction
          filter(NLERS_id=="NLLOG") %>% 
          dplyr::select(1) %>% 
          unnest_longer(NLERS) %>% 
          
          filter(NLERS_id %in% c("XR","TN")) %>%
          pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
          unnest(names(.)) %>% 
          unnest(names(.)) %>% 
          # glimpse()
          setNames(gsub("XR","vessel", names(.))) %>% 
          setNames(gsub("TN","trip", names(.))) %>% 
          mutate(vessel = gsub("-","",vessel))
        
      )
    
    return(header)
    
  # NLTRN is a transport message
  } else if (type == "NLTRN") {
    
    header <- 
      bind_cols(
        # add general header
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          filter(is.na(NLERS_id)|NLERS_id=="") %>%
          dplyr::select(-NLERS_id) %>%
          bind_cols(., variable=names[-grep("NLLOG|NLCOR|NLTRN", names)]) %>% 
          pivot_wider(names_from = variable, values_from = NLERS) %>% 
          unnest(names(.)),
        # add version
        data.frame(version = version, stringsAsFactors = FALSE),
        # add message
        data.frame(message = type, stringsAsFactors = FALSE),
        # add vessel
        tibble::as_tibble(xml) %>% 
          unnest_longer(NLERS) %>% 
          filter(NLERS_id == "XR") %>% 
          dplyr::select(1) %>% 
          unnest(NLERS) %>% 
          unnest(NLERS) %>% 
          rename(vessel=NLERS) %>% 
          mutate(vessel = gsub("-","",vessel))
      )
    
  } else {
    
    print(paste("NOT USED:",type))
    
  }
}

# NLDEP v2.0
# filename = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/SCH65/ecatch/20170213001000000647.xml"

# NLDEP v3.3
# filename = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/SCH65/ecatch/20170609000000992.xml"

i <- 108
i <- 1409 # dep
i <- 1417 # rtp
i <- 1439 # corr
i <- 1450 # far
i <- 3005 # trn
i <- 1607 # far 2.0
filename <- filelist[grep("20180310000002107", filelist)]
filename <- filelist[grepl("20180108000001825", filelist)] # v3.3 NLCOE
filename <- filelist[grepl("20160614001000000039", filelist)] # V2.0 NLDEP

headers <- dep <- far <- rtp <- data.frame(stringsAsFactors = FALSE)

for (i in 1:length(filelist)) {
# for (i in c(1600:1700, 3000:3200)) {
# for (i in c(1400:1450)) {
# for (i in c(1:100)) {
  print(i)
  
  filename <- filelist[i]
  xml     <- read_xml(filename) %>% as_list()
  names   <- attributes(xml[["NLERS"]])$names
  xmlns   <- attributes(xml[["NLERS"]])$xmlns 
  type    <- names[length(names)]               # NLLOG or NLCOR
  version <- stringr::word(xmlns,-1,sep="/")    # 2.0 or 3.3
  # far_df  <- tibble::as_tibble(xml) %>% unnest_longer(NLERS) 
  
  header  <- read_header(xml, version, type)
  
  # skip loop if character
  if(is.character(header)) next

  message <- header$message
  
  # add header to data frame
  headers <- bind_rows(headers, header)
  
  # add FAR
  if (message == "NLFAR") far <- bind_rows(far, 
                                           bind_cols(
                                             header, 
                                             read_nlfar(xml, version, type)
                                           )
                                         )
  if (nrow(far)>0) {
    if (any(grepl("NLERS", names(far)))) stop("Error: NLERS header in far")
  } 
  
  # add DEP
  if (message == "NLDEP") dep <- bind_rows(dep, 
                                           bind_cols(
                                             header, 
                                             read_nldep(xml, version, type)
                                           )
  )
  if (nrow(dep)>0) {
    if (any(grepl("NLERS", names(dep)))) stop("Error: NLERS header in dep")
  } 
  
  # add RTP
  if (message == "NLRTP") rtp <- bind_rows(rtp, 
                                           bind_cols(
                                             header, 
                                             read_nlrtp(xml, version, type)
                                           )
  )
  if (nrow(rtp)>0) {
    if (any(grepl("NLERS", names(rtp)))) stop("Error: NLERS header in rtp")
  } 
  
}

save(dep, rtp, far, headers, file=file.path(onedrive, "rdata", "ecatch temp.RData"))
load(file=file.path(onedrive, "rdata", "ecatch temp.RData"))

# all corrections to DEP, RTP or FAR
corr <-
  bind_rows(
    dep %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, DA, TI, PO, RN2, RE, AA),
    rtp %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, DA, TI, PO, RN2, RE),
    far %>% distinct     (NFR, RN, RD, RT, message, vessel, version, trip, RN2, RE, FO, GE, ME, SN, WT, FA, EZ, SR)
  ) %>%
  filter(!is.na(RN2))

# ------------------------------------------------------------------------------
# Version 2.0 does not have tripnumber included
# ------------------------------------------------------------------------------

dep20 <-
  bind_rows(
    dep %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, DA, TI, PO, RN2, RE, AA),
    corr %>% filter(message %in% c("NLDEP")) %>% dplyr::select(NFR, RN, RD, RT, message, version,  vessel, trip, DA, TI, PO, RN2, RE, AA)
  ) %>% 
  filter(AA == "FSH") %>% 
  # rename(departuredate = DA, departureport = PO) %>% 
  filter(is.na(trip)) %>% 
  ungroup() %>% 
  distinct() 

rtp20 <-  
  bind_rows(
    rtp %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, version, DA, TI, PO, RN2, RE),
    corr %>% filter(message %in% c("NLRTP")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, version, DA, TI, PO, RN2, RE)
  ) %>% 
  # rename(arrivaldate = DA, arrivalport = PO) %>% 
  filter(is.na(trip)) %>% 
  ungroup() %>% 
  distinct()  

# trip 2.0
trip20 <-
  bind_rows(
    dplyr::select(dep20, message, vessel, version, DA, TI, PO, RE),
    dplyr::select(rtp20, message, vessel, version, DA, TI, PO, RE),
  ) %>% 
  group_by(vessel) %>% 

  mutate(date = lubridate::ymd_hms(paste0(DA,"T",TI,":00"))) %>% 
  # dplyr::select(-DA, -TI) %>% 
  arrange(vessel, date) %>% 
  
  # remove trips that start with RTP
  filter(!(DA == "2016-06-02")) %>%
  filter(!(DA == "2016-05-27")) %>%

  # remove duplicated NLDEP or NLRTP
  filter(!(message=="NLDEP" & lead(message) == "NLDEP")) %>% 
  filter(!(message=="NLRTP" & lead(message) == "NLRTP")) %>% 
  
  # add tripnumber
  mutate(counter = ifelse(message=="NLDEP", 1, 0)) %>% 
  mutate(id      = cumsum(counter)) %>%
  mutate(year    = lubridate::year(date)) %>% 
  mutate(trip    = paste0(year, stringr::str_pad(id, width=3, pad="0"))) %>% 
  
  mutate(message = factor(message, levels=c("NLDEP", "NLRTP"))) %>% 
  unite("z", c(date, PO, RE), sep="#", remove = TRUE) %>% 
  dplyr::select(-DA, -TI, -counter, -id, -year) %>% 
  

  pivot_wider(names_from = message, values_from = z) %>% 
  drop_na(NLRTP) %>% 
  tidyr::separate(NLDEP, into=c("departuredate","departureport","departureremark"), sep="#") %>% 
  tidyr::separate(NLRTP, into=c("arrivaldate","arrivalport","arrivalremark"), sep="#") %>% 
  mutate(
    departuredate = lubridate::ymd_hms(departuredate),
    arrivaldate   = lubridate::ymd_hms(arrivaldate)
  )
  
# Couple tripinformation to FAR2.0
far20 <-
  bind_rows(
    far %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, RN2, RE, 
                                                          GE, ME, SN, WT, FA, EZ, SR),
    corr %>% filter(message %in% c("NLFAR")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, RN2, RE,
                                                               GE, ME, SN, WT, FA, EZ, SR)
  ) %>% 
  # rename(catchdate = RD) %>% 
  mutate(catchdate = lubridate::ymd_hms(paste0(RD,"T",RT,":00"))) %>% 
  
  filter(is.na(trip)) %>% 
  filter(!is.na(WT), WT>0) %>% 
  distinct()  

merged20 <-
  sqldf::sqldf("select far20.NFR, far20.RN, far20.vessel, far20.version, far20.catchdate, 
                       far20.GE, far20.ME, far20.SN, far20.WT, far20.FA, far20.EZ, far20.SR,
                       trip20.departuredate, trip20.departureport, trip20.departureremark,
                       trip20.arrivaldate, trip20.arrivalport, trip20.arrivalremark, 
                       trip20.trip from far20
                join trip20 on far20.vessel    == trip20.vessel and
                               far20.catchdate >= trip20.departuredate and
                               far20.catchdate <= trip20.arrivaldate")
  
# ------------------------------------------------------------------------------
# Version 3.3 has tripnumber included
# ------------------------------------------------------------------------------

dep33 <-
  bind_rows(
    dep %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, DA, TI, PO, RN2, RE, AA),
    corr %>% filter(message %in% c("NLDEP")) %>% dplyr::select(NFR, RN, RD, RT, message, version,  vessel, trip, DA, TI, PO, RN2, RE, AA)
  ) %>% 
  filter(AA == "FSH") %>% 
  rename(departuredate = DA, departureport = PO) %>% 
  drop_na(trip) %>% 
  distinct() %>% 
  group_by(vessel, trip) %>% 
  slice_tail() 

  
rtp33 <-  
  bind_rows(
    rtp %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, version, DA, TI, PO, RN2, RE),
    corr %>% filter(message %in% c("NLRTP")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, version, DA, TI, PO, RN2, RE)
  ) %>% 
  rename(arrivaldate = DA, arrivalport = PO) %>% 
  drop_na(trip) %>% 
  distinct() %>% 
  group_by(vessel, trip) %>% 
  slice_tail() 

far33 <-
  bind_rows(
    far %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, RN2, RE, 
                                                          FO, GE, ME, SN, WT, FA, EZ, SR),
    corr %>% filter(message %in% c("NLFAR")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, version, trip, RN2, RE,
                                                               FO, GE, ME, SN, WT, FA, EZ, SR)
  ) %>% 
  rename(catchdate = RD) %>% 
  drop_na(trip) %>% 
  filter(!is.na(WT), WT>0) %>% 
  distinct()  

merged33 <-
  left_join(
    far33, 
    dplyr::select(dep33, vessel, trip, version, departureport, departuredate),
    by=c("vessel","trip", "version")
  ) %>% 
  left_join(
    dplyr::select(rtp33, vessel, trip, version, arrivalport, arrivaldate),
    by=c("vessel","trip", "version")
  ) %>% 
  
  mutate(
    catchdate     = lubridate::ymd_hms(catchdate),
    departuredate = lubridate::ymd_hms(departuredate),
    arrivaldate   = lubridate::ymd_hms(arrivaldate)
  )



# corr %>% filter(RN %in% corr$RN2) %>% View()
# dep %>% filter(RN %notin% corr$RN2) %>% View()
# dep %>% filter(RN %in% corr$RN2) %>% View()
# dep %>% filter(RD == "2016-06-02") %>% View()
# dep %>% group_by(AA) %>% summarise(n=n())

# janitor::compare_df_cols(merged20, merged33)

# setup the final dataset
ecatch <-
  bind_rows(merged20, merged33) %>% 
  lowcase() %>% 
  mutate(year = year(catchdate) ) %>% 
  rename(meshsize          = me,
         fishingoperations = fo,
         species           = sn,
         weight            = wt,
         faozone           = fa,
         rect              = sr, 
         economiczone      = ez,
         gear              = ge) %>%
  mutate(faozone = tolower(faozone)) %>% 
  mutate(source="ecatch") %>% 
  dplyr::select(-c(nfr,
                   rn,
                   departureremark,
                   arrivalremark,
                   rn2,
                   rt,
                   re,
                   message
  )) %>% 
  relocate(vessel, year, trip, catchdate) %>% 
  mutate(across(c("meshsize","fishingoperations"), as.integer)) %>% 
  mutate(across(c("weight"), as.numeric)) %>% 
  left_join(rect_df, by="rect")

save(ecatch, file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/elog ecatch.RData")
# load(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch20.RData")



skimr::skim(ecatch)
ecatch %>%  distinct(vrs)
ecatch %>% filter(!is.na(mv)) %>% distinct(version)


xlim <- range(ecatch$lon, na.rm=TRUE)
ylim <- range(ecatch$lat, na.rm=TRUE)

# plot by vessel and year
ecatch %>% 
  group_by(vessel, year, version, rect, lat, lon) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap(xlim=xlim , ylim=ylim) +
  
  geom_polygon(data=world_mr_df, aes(x=long, y=lat, group=group), fill = "grey75") +
  geom_point(aes(colour=version, size=weight), alpha=0.5) +
  facet_grid(vessel~paste(year, version))


