# ===============================================================================================
# code for reading ecatch XML files
#
# 26/04/2023 Still not completely finished; some errors arising from missing RTP or DEP messages
# 04/05/2023 Finalized the code to read ecatch 2.0 and 3.3
# ===============================================================================================

library(xml2)
library(tidyverse)
library(lubridate)
library(sqldf)

rm(list=ls())

source("../prf/r/my utils.R")
source("../mptools/R/get_onedrive.r")

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
    dep %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE, AA),
    rtp %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE),
    far %>% distinct(NFR, RN, RD, RT, message, vessel, trip, RN2, RE)
  ) %>%
  filter(!is.na(RN2))

# corr %>% filter(RN %in% corr$RN2) %>% View()
# dep %>% filter(RN %notin% corr$RN2) %>% View()
# dep %>% filter(RN %in% corr$RN2) %>% View()
# dep %>% filter(RD == "2016-06-02") %>% View()
# dep %>% group_by(AA) %>% summarise(n=n())

trips <-
  
  # DEP with FSH; handle corrections, RTP and FAR that are not in corrections
  bind_rows(
    dep %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE, AA),
    corr %>% filter(message %in% c("NLDEP")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE, AA)
  ) %>% 
  filter(AA == "FSH") %>% 
  rename(departuredate = DA, departureport = PO) %>% 
  
  # RTP
  left_join(
    bind_rows(
      rtp %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE),
      corr %>% filter(message %in% c("NLRTP")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE)
    ) %>% 
    rename(arrivaldate = DA, arrivalport = PO),
    by=c("vessel","trip")
  ) 
    
    # far %>% filter(RN %notin% corr$RN2) %>% distinct(NFR, RN, RD, RT, message, vessel, RN2, RE)
    # corr %>% filter(message %in% c("NLFAR"))         %>% distinct(NFR, RN, RD, RT, message, vessel, RN2, RE)
  ) %>%
  mutate(
    RD = ifelse(!is.na(RT), paste0(RD,"T",RT,":00"),RD),
    DA = ifelse(!is.na(TI), paste0(DA,"T",TI,":00"),DA),
  ) %>% 
  mutate(DA = ifelse(is.na(DA), RD, DA)) %>% 
  mutate(across(c(RD, DA), lubridate::ymd_hms)) %>% 
  dplyr::select(-RT, -TI) %>% 
  mutate(year = year(DA)) %>% 
  arrange(vessel, year, DA) %>% 
  group_by(vessel, year) %>% 
  # mutate(id = ifelse(row_number()==1, 0, NA)) %>% 
  mutate(id = ifelse(message == "NLDEP", 1, 0)) %>% 
  mutate(trip = paste0(year, stringr::str_pad(cumsum(id), width=3, pad="0"))) %>% 
  
  # remove erroneous RTPs
  group_by(vessel, year, trip) %>% 
  arrange(vessel, year, trip, DA) %>%
  mutate(id = row_number()) %>% 
  filter(message != "NLRTP" | (message == "NLRTP" & id == max(id))) %>% 
  ungroup()

# rtp %>% filter(RN == "20170720000001103") 
# rtp %>% filter(RN == "20170724000001105") 
# test <- rtp %>% filter(!(rtp$RN %in% rtp$RN2)) 
# test %>% filter(RN == "20170720000001103") 
# test %>% filter(RN == "20170724000001105") 


# trips_dep <- trips %>% filter(message == "NLDEP") %>% dplyr::select(vessel, trip, DA, PO) %>% distinct()
# trips_rtp <- trips %>% filter(message == "NLRTP") %>% dplyr::select(vessel, trip, DA, PO) %>% distinct()
# test <- left_join(trips_dep, trips_rtp, by=c("vessel","trip"), multiple="all")
# test %>% filter(row_number() %in% 28) %>% View()
# test %>% group_by(vessel, trip) %>% summarise(n=n()) %>% filter(n>1) %>% View()

# trips_dep %>% filter(row_number() %in% 134:137) %>% View()
# trips_rtp %>% filter(trip=="2017065") %>% View()
# trips %>% filter(trip=="2016028") %>% View()
# rtp %>% filter(RN=="20160831001000000228") %>% View()
# rtp %>% filter(RN2=="20160831001000000228") %>% View()
trips %>% 
  
# setup the final dataset
ecatch <-
  far %>% 
  
  # add tripnumber to FAR message
  left_join(trips %>% 
              dplyr::select("RN", "trip"),
            by=c("RN")) %>% 

  # add departure information  
  left_join(trips %>% 
              filter(message=="NLDEP") %>% 
              dplyr::select(vessel, trip, departuredate=DA, departureport=PO),
            by=c("vessel","trip")) %>% 
  
  # add return to port information
  left_join(trips %>% 
            filter(message=="NLRTP") %>% 
            dplyr::select(vessel, trip, arrivaldate=DA, arrivalport=PO),
          by=c("vessel","trip")) %>% 
  
  # catchdate
  mutate(catchdate = ifelse(!is.na(RT), paste0(RD,"T",RT,":00"),RD)) %>% 
  mutate(across(c(catchdate), lubridate::ymd_hms)) %>% 
  mutate(year = year(catchdate) ) %>% 

  # set arrival datetime to last FAR if missing
  group_by(vessel, year, trip) %>% 
  mutate(arrivaldate = ifelse(is.na(arrivaldate), max(catchdate, na.rm=TRUE), arrivaldate)) %>% 
  mutate(arrivaldate = as_datetime(arrivaldate)) %>% 
  ungroup() %>% 
  lowcase() %>% 
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
  mutate(version = ifelse(is.na(vrs), "2.0", vrs)) %>% 
  dplyr::select(-c(swe,
                   vrs,
                   nad, 
                   nfr,
                   rn,
                   gc,
                   du,
                   kv,
                   rn2,
                   re,
                   et, 
                   mv,
                   rd,
                   rt
  )) %>% 
  relocate(vessel, year, trip, catchdate)




skimr::skim(ecatch)
ecatch %>%  distinct(vrs)
ecatch %>% filter(!is.na(mv)) %>% distinct(version)

# save(ecatch20, file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch20.RData")
# load(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch20.RData")




dep %>% left_join(rtp, by=c("vessel","trip"))

dep2 <-
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

rtp2 <-  
  bind_rows(
    rtp %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, version, DA, TI, PO, RN2, RE),
    corr %>% filter(message %in% c("NLRTP")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, version, DA, TI, PO, RN2, RE)
  ) %>% 
  rename(arrivaldate = DA, arrivalport = PO) %>% 
  drop_na(trip) %>% 
  distinct() %>% 
  group_by(vessel, trip) %>% 
  slice_tail() 

far2 <-
  bind_rows(
    far %>% filter(RN %notin% corr$RN2) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE),
    corr %>% filter(message %in% c("NLRTP")) %>% dplyr::select(NFR, RN, RD, RT, message, vessel, trip, DA, TI, PO, RN2, RE)
  ) %>% 
  rename(arrivaldate = DA, arrivalport = PO) %>% 
  drop_na(trip) %>% 
  distinct() %>% 
  group_by(vessel, trip) %>% 
  slice_tail() 

merged <-
  left_join(
    dplyr::select(dep2, vessel, trip, source, version, departureport, departuredate),
    dplyr::select(rtp2, vessel, trip, arrivalport, arrivaldate),
    by=c("vessel","trip")
  )
