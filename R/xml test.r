library(xml2)
tb <- 
  tibble::as_tibble(driver_data) %>% 
  # mutate(NLERS = map(NLERS, as.character)) %>% 
  unnest_wider(NLERS)

tb %>% 
  filter(is.na(...1)) %>% 
  unnest_longer(col=c(XR,RV, TV, CVD, CVO,MA))

tb %>% 
  filter(is.na(...1)) %>% 
  unnest_longer(col=c(NLFAR)) %>% 
  unnest_wider(col=NLFAR, names_repair = "unique") %>% 
  View()


xmlDocu <- read_xml("C:/TEMP/FAR20123108223734-1.xml")
#find namespace
ns<-xml_ns(xmlDocu)



xml_list = read_xml("C:/TEMP/FAR20120105041048-1.xml") %>% as_list()
xml_df = tibble::as_tibble(xml_list) %>% unnest_longer(NLERS)

lp_wider = 
  xml_df %>%
  dplyr::filter(NLERS_id == "NLFAR") %>%
  unnest_wider(NLERS)

lp_df = 
  lp_wider %>%
  # 1st time unnest to release the 2-dimension list?
  unnest(cols = names(.)) %>%
  # 2nd time to nest the single list in each cell?
  unnest(cols = names(.)) 

lp_df2 <-
  lp_df %>% 
  slice_tail(n=1) %>% 
  unnest(cols = names(.)) 
  

  unnest(cols = names(.)) %>%
  # convert data type
  readr::type_convert() 

# <?xml version="1.0" encoding="iso-8859-2"?>
#   <nlvis:NLERS xmlns:nlvis="http://www.vishub.nl/xsd/NLVIS-Basis-v2.0" SID="19740523001">
#   <nlvis:NAD>VCC</nlvis:NAD>
#   <nlvis:NFR>143770512</nlvis:NFR>
#   <nlvis:RN>20120831223734219</nlvis:RN>
#   <nlvis:RD>2012-08-31</nlvis:RD>
#   <nlvis:RT>20:37</nlvis:RT>
#   <nlvis:NLLOG>
#   <nlvis:XR>SL-9</nlvis:XR>
#   <nlvis:NA>JOHANNA</nlvis:NA>
#   <nlvis:RV>FRA000544858</nlvis:RV>
#   <nlvis:TV>Vissersvaartuig</nlvis:TV>
#   <nlvis:CVD>1970-01-01</nlvis:CVD>
#   <nlvis:CVO>J</nlvis:CVO>
#   <nlvis:MA>143770512</nlvis:MA>
#   <nlvis:MD/>
#   <nlvis:NLFAR>
#   <nlvis:DA>2012-08-31</nlvis:DA>
#   <nlvis:FO>11</nlvis:FO>
#   <nlvis:DU>14</nlvis:DU>
#   <nlvis:NLGEA>
#   <nlvis:NLGE>
#   <nlvis:GE>SSC</nlvis:GE>
#   <nlvis:ME>80</nlvis:ME>
#   </nlvis:NLGE>
#   <nlvis:GC>30m</nlvis:GC>
#   <nlvis:FO>11</nlvis:FO>
#   <nlvis:DU>14</nlvis:DU>
#   <nlvis:NLSPE>
#   <nlvis:SN>WHG</nlvis:SN>
#   <nlvis:KV>N</nlvis:KV>
#   <nlvis:WT>613.6</nlvis:WT>
#   <nlvis:GE>SSC</nlvis:GE>
#   <nlvis:NLRAS>
#   <nlvis:FA>27.7.e</nlvis:FA>
#   <nlvis:EZ>EU</nlvis:EZ>
#   <nlvis:SR>29E7</nlvis:SR>
#   <nlvis:FE>E</nlvis:FE>
#   </nlvis:NLRAS>
#   </nlvis:NLSPE>
#   </nlvis:NLGEA>
#   </nlvis:NLFAR>
#   </nlvis:NLLOG>
#   </nlvis:NLERS>
#  
  
xml_address = "http://www.fehd.gov.hk/english/licensing/license/text/LP_Restaurants_EN.XML"

restaurant_license_xml <- 
  as_list(read_xml(xml_address))

xml_df = 
  tibble::as_tibble(restaurant_license_xml) %>%
  unnest_longer(DATA)
  
lp_wider <-
  xml_df %>%
  dplyr::filter(DATA_id == "LP") %>%
  unnest_wider(DATA) 

lp_df <-
  lp_wider %>%
  # 1st time unnest to release the 2-dimension list?
  unnest(cols = names(.)) %>%
  # 2nd time to nest the single list in each cell?
  unnest(cols = names(.)) %>%
  # convert data type
  readr::type_convert() 


far_list = 
  read_xml("C:/TEMP/FAR20120105041048-1.xml") %>% as_list()

far_df = 
  tibble::as_tibble(xml_list) %>% unnest_longer(NLERS)

nlfar_wider = 
  far_df %>%
  dplyr::filter(NLERS_id == "NLFAR") %>%
  unnest_wider(NLERS)

nlfar_test = 
  nlfar_wider %>%
  dplyr::select(NLGEA) %>%
  unnest_wider(NLGEA, names_repair = "unique") %>% 
  dplyr::select(contains("NLSPE")) %>% 
  pivot_longer(cols=names(.)) %>% 
  unnest_wider(value) %>%
  # 2nd time to nest the single list in each cell?
  unnest(cols = names(.)) 


nlfar_df = 
  nlfar_wider %>%
  # 1st time unnest to release the 2-dimension list?
  unnest(cols = names(.)) %>%
  # 2nd time to nest the single list in each cell?
  unnest(cols = names(.)) 
