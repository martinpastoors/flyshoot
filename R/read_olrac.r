library(xml2)
library(tidyverse)

rm(list=ls())


filelist <- list.files(
  path=file.path("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"),
  pattern="xml",
  recursive = TRUE,
  full.names = TRUE)
filelist <- filelist[grepl("olrac", filelist)]

# filename = "C:/TEMP/FAR20120105041048-1.xml"

filename = filelist[1]
# filename = filelist[2]
# filename = filelist[3]
filename = filelist[1371]

read_olrac <- function(filename) {
  
  far_list <- read_xml(filename) %>% as_list()
  
  far_df = 
    tibble::as_tibble(far_list) %>% unnest_longer(NLERS)

  # -------------------------------------------------------------------------  
  # FAR
  # -------------------------------------------------------------------------  
  
  if (nrow(filter(far_df, NLERS_id=="NLFAR"))==1) {
    
    # print(paste("FAR", filename))  
    
    # print("message")
    
    message <- 
      far_df %>%
      filter(is.na(NLERS_id)) %>%
      dplyr::select(-NLERS_id) %>% 
      bind_cols(variable=c("NAD","NFR","RN","RD","RT")) %>% 
      pivot_wider(names_from = variable, values_from = NLERS)
    
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
      
      nspecies <- sum(grepl("NLSPE", names(tmp)))
      
      # print("effort")
      
      effort <-
        tmp %>% 
        dplyr::select(!contains("NLSPE")) %>% 
        unnest_wider(NLGE) %>%
        unnest(names(.)) %>% 
        dplyr::select(-GE)
      
      # print("catch")
      
      catch <-
        tmp %>% 
        dplyr::select(contains("NLSPE")) %>% 
        pivot_longer(names(.)) %>% 
        unnest_wider(value) %>%
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
      
      return(final)
      
    } # end of NLSP
    
  } # end of FAR

  # -------------------------------------------------------------------------  
  # Corrections
  # -------------------------------------------------------------------------  
  
  if (nrow(filter(far_df, NLERS_id=="NLLOG"))==1) {

    # print(paste("COR", filelist[i]))  
    
    message0 <-
      far_df %>% 
      filter(is.na(NLERS_id)) %>% 
      
      mutate(NLERS_id = c("NAD","NFR","RN","RD","RT")) %>% 
      pivot_wider(names_from = NLERS_id, values_from = NLERS)
    
    message1 <-
      far_df %>% 
      filter(NLERS_id != "NLLOG") %>% 
      unnest(NLERS) %>% 
      pivot_wider(names_from = NLERS_id, values_from = NLERS) %>% 
      rename(RNOLD = RN)
    
    tmp <-
      far_df %>% 
      filter(NLERS_id == "NLLOG") %>% 
      unnest_wider(NLERS)  
    
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
        unnest(names(.)) %>% 
        dplyr::select(-GE)
      
      catch <-
        tmp2 %>% 
        dplyr::select(contains("NLSPE")) %>% 
        pivot_longer(names(.)) %>% 
        unnest_wider(value) %>%
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
      
      return(final)
      
    } # end of NLSP
    
  }  

}
  
olrac <- data.frame(stringsAsFactors = FALSE)

for (i in 1:length(filelist)) {
# for (i in 1371:2103) {
  
  print(i)
  
  olrac <- bind_rows(
    olrac,
    read_olrac(filelist[i])  
  )
  
  # print(head(olrac))
  
}

olrac <- 
  olrac %>% 
  unnest(names(.)) %>% 
  mutate(WT   = as.numeric(WT)) %>% 
  mutate(date = lubridate::ymd(RD)) %>% 
  mutate(year = lubridate::year(date))
  
save(olrac, file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/olrac.RData")
load(file="C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/olrac.RData")

# simple plot
olrac %>% 
  group_by(XR, year, GE) %>% 
  summarise(catch = sum(WT, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=catch)) +
  theme_bw() +
  geom_bar(aes(fill=GE), stat="identity") +
  facet_wrap(~XR)

olrac %>% 
  group_by(XR, year, date) %>% 
  summarise(catch = sum(WT, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=catch)) +
  theme_bw() +
  geom_boxplot(aes(group=year)) +
  facet_wrap(~XR)


# aanvoer vanuit PEFA
aanvoer <- 
  readxl::read_excel("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/Totaal aanvoer.xlsx",
                     sheet="data") %>% 
  lowcase() %>% 
  group_by(vessel, year) %>% 
  summarise(weight = sum(weight, na.rm=TRUE)) %>% 
  mutate(source="aanvoer")

# compare
olrac %>% 
  filter(year >= 2012) %>% 
  mutate(vessel = gsub("-","", XR)) %>% 
  group_by(vessel, year) %>% 
  summarise(weight = sum(WT, na.rm=TRUE)) %>% 
  mutate(source="olrac") %>% 
  bind_rows(aanvoer) %>% 
  
  ggplot(aes(x=year, y=weight)) +
  theme_bw() +
  geom_bar(aes(fill=source), position=position_dodge2(preserve="single" ), stat="identity") +
  facet_wrap(~vessel)
