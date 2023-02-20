library(xml2)
library(tidyverse)
library(lubridate)

rm(list=ls())

source("../prf/r/my utils.R")

# filelist <- list.files(
#   path=file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"),
#   pattern="xml",
#   recursive = TRUE,
#   full.names = TRUE)
# filelist <- filelist[grepl("ecatch", filelist)]

# check function
# check <- function(filename) {
#   far_list <- read_xml(filename) %>% as_list()
#   far_df = 
#     tibble::as_tibble(far_list) %>% 
#     unnest_longer(NLERS) %>% 
#     slice_tail(n=1) %>% 
#     mutate(file=filename)
#   
#   return(far_df)
# }

# tst <- data.frame(stringsAsFactors = FALSE)
# for (i in 1:length(filelist)) {
# # for (i in 1:500) {
#   print(i)
#   tst <- bind_rows(
#     tst,
#     check(filelist[i]) )
# }

# testing the different messages
# types <- data.frame(stringsAsFactors = FALSE)
# i <- 1
# for (i in 1:length(filelist)) {
#     
#   filename <- filelist[i]
#   
#   far_list <- read_xml(filename) %>% as_list()
#   
#   far_df = 
#     tibble::as_tibble(far_list) %>% 
#     unnest_longer(NLERS)  %>% 
#     drop_na(NLERS_id) 
#   
#   t <- far_df %>% slice_tail(n=1) %>% dplyr::select(NLERS_id) %>% as.character()
#   
#   if(t %notin% names(types)) {
#     
#     print(paste(i, t))
#     
#     types <- bind_rows(
#       types,
#       far_df %>% pivot_wider(names_from = NLERS_id, values_from = NLERS)
#     )
#   }
# }

  

# filelists with only NLFAR and NLLOG
# filelist2 <-
#   tst %>% 
#   filter(NLERS_id %in% c("NLFAR", "NLLOG")) %>%
#   # filter(NLERS_id %in% c("NLLOG")) %>% 
#   dplyr::select(file) %>% 
#   unlist()

# save(filelist, filelist2, file="C:/TEMP/filelist.RData")

load(file="C:/TEMP/filelist.RData")




raw <- data.frame(stringsAsFactors = FALSE)

filename = filelist2[9]

for (i in 291:length(filelist2)) {
# for (i in 1:25) {
    
  print(i)
  
  filename <- filelist2[i]

  far_list <- read_xml(filename) %>% as_list()
  
  names <- attributes(far_list[["NLERS"]])$names
  xmlns <- attributes(far_list[["NLERS"]])$xmlns 
  
  far_df = 
    tibble::as_tibble(far_list) %>% 
    unnest_longer(NLERS) 
  
  t <-
    far_df %>% 
    slice_tail(n=1) %>% 
    dplyr::select(NLERS_id) %>% 
    as.character()
  
  if(grepl("3.3", xmlns)) {
    
    # -------------------------------------------------------------------------  
    # FAR
    # -------------------------------------------------------------------------  
    
    if (t == "NLFAR") {
      
      # print(paste("FAR", filename))  
      
      print("NLFAR")  
      
      message <- 
        far_df %>%
        filter(is.na(NLERS_id)) %>%
        dplyr::select(-NLERS_id) %>%
        bind_cols(., variable=names[-grep("NLLOG", names)]) %>% 
        pivot_wider(names_from = variable, values_from = NLERS) %>% 
        mutate(xmlns = xmlns)
      
      # print("vessel")
      
      vessel <-
        far_df %>% 
        filter(NLERS_id != "NLFAR" & !is.na(NLERS_id)) %>% 
        unnest(NLERS) %>% 
        pivot_wider(names_from = NLERS_id, values_from = NLERS)
      
      # print("tmp")
      tmp <-
        suppressMessages(
          far_df %>% 
            filter(NLERS_id == "NLFAR") %>% 
            unnest_wider(NLERS) %>% 
            
            {if(ncol(.[grepl("NLGEA", names(.))])>1) {
              tidyr::pivot_longer(., names_to = "test", values_to="NLGEA", cols=starts_with("NLGEA")  ) %>% 
                slice_tail(., n=1) %>% 
                dplyr::select(., NLGEA) %>% 
                unnest_wider(., NLGEA, names_repair = "unique")
              
            } else if (ncol(.[grepl("NLGEA", names(.))])==1) {
              dplyr::select(., NLGEA) %>%        
                unnest_wider(., NLGEA, names_repair = "unique")
            }}  
        )
      
      if(any(grepl("NLSPE", names(tmp)))) {
        
        print("--- with species")
        
        nspecies <- sum(grepl("NLSPE", names(tmp)))
        
        # print("effort")
        
        effort <-
          tmp %>% 
          dplyr::select(!contains("NLSPE")) %>% 
          unnest_wider(NLGE) %>%
          unnest(names(.)) %>% 
          dplyr::select(-GE)
        
        # print("catch")

        gear <-
          tmp %>% 
          dplyr::select(contains("NLGE")) %>% 
          pivot_longer(names(.)) %>% 
          unnest_wider(value) %>%
          unnest(cols = names(.))  
          
        catch <-
          tmp %>% 
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
          dplyr::select(-name) 
        
        
        # print("final")
        
        final <- 
          message %>% 
          bind_cols(vessel) %>% 
          bind_cols(effort) %>% 
          bind_cols(catch) %>% 
          bind_cols(file=filename) %>% 
          as_tibble()
        
        raw <- bind_rows(
          raw,
          final  
        )
        
        # return(final)
        
      } # end of NLSPE
    } # end of NLFAR
    
    # -------------------------------------------------------------------------  
    # Corrections
    # -------------------------------------------------------------------------  
    
    if (t=="NLLOG") {
      
      print("NLLOG")  
      
      message0 <-
        far_df %>% 
        filter(is.na(NLERS_id)) %>% 
        mutate(NLERS_id = names[-grep("NLCOR|NLLOG", names)]) %>% 
        mutate(NLERS_id = ifelse(NLERS_id=="RN", "RNNEW", NLERS_id)) %>% 
        pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
        mutate(xmlns = xmlns)
      
      message1 <-
        far_df %>% 
        filter(NLERS_id != "NLLOG") %>% 
        unnest(NLERS) %>% 
        pivot_wider(names_from = NLERS_id, values_from = NLERS)  
      # rename(RNOLD = RN)
      
      tmp <-
        far_df %>% 
        filter(NLERS_id == "NLLOG") %>% 
        unnest_wider(NLERS)  
      
      if("NLFAR" %in% names(tmp)) {
        
        print("--- with FAR")  
        
        vessel <-
          tmp %>% 
          dplyr::select(-NLFAR) %>%
          unnest(names(.))
        
        message2 <-
          suppressMessages(
            tmp %>% 
              dplyr::select(NLFAR) %>%
              unnest_wider(NLFAR, names_repair = "unique") %>% 
              dplyr::select(-starts_with(c("NLGEA", "NLINS"))) %>% 
              unnest(names(.)) %>% 
              dplyr::select(-any_of( c("FO", "DU")))
          )
        
        tmp2 =
          suppressMessages(
            tmp %>% 
              dplyr::select(NLFAR) %>%
              unnest_wider(NLFAR, names_repair = "unique") %>%
              
              {if(ncol(.[grepl("NLGEA", names(.))])>1) {
                tidyr::pivot_longer(., names_to = "test", values_to="NLGEA", cols=starts_with("NLGEA")  ) %>% 
                  slice_tail(., n=1) %>% 
                  dplyr::select(., NLGEA) %>% 
                  unnest_wider(., NLGEA, names_repair = "unique")
                
              } else if (ncol(.[grepl("NLGEA", names(.))])==1) {
                dplyr::select(., NLGEA) %>%        
                  unnest_wider(., NLGEA, names_repair = "unique")
              }}  
          )
        
        
        if(any(grepl("NLSPE", names(tmp2)))) {
          
          nspecies <- sum(grepl("NLSPE", names(tmp2)))
          
          # print("effort")
          
          effort <-
            tmp2 %>% 
            dplyr::select(!contains(c("NLSPE", "NLSPN"))) %>% 
            unnest_wider(NLGE) %>%
            unnest(names(.)) 
          
          catch <-
            tmp2 %>% 
            dplyr::select(contains("NLSPE")) %>% 
            pivot_longer(names(.)) %>% 
            unnest_wider(value) %>%
            dplyr::select(!contains("NLGE")) %>% 
            # 2nd time to nest the single list in each cell?
            unnest(cols = names(.)) %>% 
            unnest(cols = NLRAS) %>% 
            # bind_cols(data.frame(variable=rep(c("FA","EZ", "SR", "FE"), nrow(.)/4))) %>% 
            
            {if(nrow(.)/nspecies == 3) {
              bind_cols(., data.frame(variable=rep(c("FA","EZ", "SR"), nrow(.)/3)))
            } else if (nrow(.)/nspecies == 4) {
              bind_cols(., data.frame(variable=rep(c("FA","EZ", "SR", "FE"), nrow(.)/4)))
            } else {
              stop("not the right number of dimensions") 
            } } %>% 
            
            tidyr::pivot_wider(names_from = variable, values_from = NLRAS) %>% 
            dplyr::select(-name)
          
          
          final <- 
            message0 %>% 
            bind_cols(message1) %>%
            bind_cols(message2) %>% 
            bind_cols(vessel) %>% 
            bind_cols(effort) %>% 
            bind_cols(catch) %>% 
            bind_cols(file=filename) %>% 
            as_tibble()
          
          raw <- bind_rows(
            raw,
            final  
          )
        } # end of NLSP
      } # end of if NLFAR
    }  # end of t=NLLOG
    
  } # end of check on version 2.0
  
} # end of loop over filenames




far <-
  raw %>% 
  unnest(cols = names(.)) %>% 
  filter(is.na(NLERS_id)) %>% 
  mutate(WT   = as.numeric(WT)) %>% 
  mutate(datetime = lubridate::ymd_hms(RD)) %>% 
  mutate(date     = as.Date(datetime)) %>% 
  mutate(year = lubridate::year(date))

corr <-
  raw %>% 
  unnest(cols = names(.)) %>% 
  filter(!is.na(NLERS_id)) %>% 
  mutate(datetime = lubridate::ymd_hms(RD)) %>% 
  mutate(date     = as.Date(datetime)) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(RN) %>% 
  filter(datetime == max(datetime)) %>% 
  mutate(WT   = as.numeric(WT)) 

ecatch33 <-
  far %>% 
  filter(RN %notin% corr$RN) %>% 
  bind_rows(corr) %>% 
  filter(year >= 2012) 

# ignore
remains <-
  corr %>% 
  filter(RN %notin% far$RN)


save(ecatch33, file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch33.RData")
load(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/ecatch33.RData")

