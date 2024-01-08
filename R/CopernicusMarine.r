# CopernicusMarine
#

  options(dplyr.summarise.inform = FALSE)
  
  # Reset lists
  rm(list=ls())
  
  # Libraries
  require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
  require(lubridate, quietly=TRUE)     # data handling
  require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
  library(sf)
  
  # install.packages("CopernicusMarine")
  library(CopernicusMarine)
  
  source("R/FLYSHOOT utils.r")
  
  spatialdir <- "C:/DATA/RDATA"
  load(file.path(spatialdir, "world_mr_df.RData"))
  load(file.path(spatialdir, "world_mr_sf.RData"))
  
  fao_sf          <- loadRData(file.path(spatialdir, "fao_sf.RData"))
  fao_sf_division <- fao_sf %>% filter(F_LEVEL=="DIVISION") %>% dplyr::select(F_DIVISION) %>% rename(division = F_DIVISION)
  
  # flyshoot info
  onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")
  
  destination <- tempfile("copernicus", fileext = ".nc")
  
  options(CopernicusMarine_uid = "mpastoors1")
  options(CopernicusMarine_pwd = "PelagicFishWithPlankton")

  
  # ============================================================================
  # Do a look over copernicus products
  
  allproducts <- CopernicusMarine::copernicus_products_list()

  allselected <- 
    allproducts %>% 
    # filter(grepl("reanalysis", tolower(title))) %>% 
    filter(grepl("atlantic", tolower(title))) %>% 
    filter(grepl("PHY", product_id)) 
  
  ds <- "NWSHELF_MULTIYEAR_PHY_004_009"
  ds <- "NWSHELF_ANALYSISFORECAST_PHY_004_013"
  # ds <- "SST_BAL_SST_L3S_NRT_OBSERVATIONS_010_032"

  df <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:length(  copernicus_product_details(product=ds)$layers )) {
    print(i)
    df <- bind_rows(df, 
                    bind_cols(
                      data.frame(ds=ds),
                      as.data.frame(unlist(copernicus_product_details(product=ds)$layers[[i]])) %>%  
                        rownames_to_column() %>% 
                        setNames(c("var","value")) %>% 
                        pivot_wider(names_from = var, values_from = value)  
                    )
    )
  }
  
  df %>% filter(grepl("monthly",tolower(subdatasetTitle))) %>% filter(row_number()==1) %>% pull(tValues)
  

  copernicus_product_details(product=ds)$layers  
  copernicus_product_details(product=ds, layer="sea_surface_temperature") 
  copernicus_product_details(t[1,1])$layers[[1]] %>% View()
  
t <- copernicus_products_list() %>% filter(grepl("temperature", tolower(title))) 
t <- copernicus_products_list() %>% filter(grepl("cmems", tolower(title))) 

s1 <- 1
for (s1 in 1:nrow(t) ) {
  cat("   ")
  print(paste(s1, t[s1,1]))
  pd <- copernicus_product_details(product=t[s1,1])
  for (s2 in 1:length(pd$layers)) {
    print(paste(s2, 
                pd$layers[[s2]]$variableId
                # paste(pd$layers[[s2]]$bbox, collapse=" "), 
                )
          )
  }
}

unlist(t[[1,"geoResolution"]]) %>% as.data.frame() %>% rownames_to_column() %>%  setNames(c("variable", "value")) %>% 
  tidyr::separate(variable, into=c("rc","type"), sep="\\.") %>% tidyr::pivot_wider(names_from = type, values_from = value)

unlist(t[[1,"stacOrCswTbox"]]) %>% as.data.frame() %>% setNames("datetime") %>% bind_cols(data.frame(type=c("start","end"))) %>% 
  mutate(datetime=trunc(as.POSIXct(datetime))) %>% pivot_wider(names_from = type, values_from = datetime)

copernicus_product_details(t[[9,1]]) %>% View()

names(t)


ds <- "NWSHELF_ANALYSISFORECAST_PHY_004_013"
ly <- "cmems_mod_nws_phy_anfc_0.027deg-3D_P1M-m"
va <- "sea_water_potential_temperature_at_sea_floor"

# ds <- "NWSHELF_MULTIYEAR_PHY_004_009"
# ly <- "cmems_mod_nws_phy-bottomt_my_7km-2D_P1D-m"
# va <- "bottomT"

ds <- "NWSHELF_MULTIYEAR_PHY_004_009"
ly <- "cmems_mod_nws_phy-bottomt_my_7km-2D_P1M-m"
va <- "sea_water_potential_temperature_at_sea_floor"

copernicus_product_details(product=ds) %>% View()

copernicus_product_details(product=ds,
                           layer  = ly) %>% View()

copernicus_product_details(product  = ds, layer=ly, variable = va) %>% View()

destination <- tempfile("copernicus", fileext = ".nc")

copernicus_download_motu(
  destination   = destination,
  product       = "NWSHELF_MULTIYEAR_PHY_004_009",
  layer         = "cmems_mod_nws_phy-bottomt_my_7km-2D_P1M-m",
  variable      = "bottomT",
  output        = "netcdf",
  region        = c(-2, 49, 10, 58),  # xmin, ymin, xmax, ymax.
  timerange     = c("1995-12-16", "2022-12-17"), 
  sub_variables = c("bottomT"),
  overwrite = TRUE
)

# dev.off()
mydata <- stars::read_stars(destination)

# dim(mydata)
# dim(mydata[1,,,c(1,265)])
plot(mydata[1,,,seq(1,325,12)], col = hcl.colors(100), axes = FALSE, mfrow=c(5,7))

as.data.frame(mydata) %>% setNames(c("x","y","time","value")) %>% drop_na(value) %>% mutate(year=year(time), month=month(time)) %>% filter(month==12) %>% 
  group_by(year) %>% 
  summarise(value = as.numeric(mean(value))) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  geom_point() +
  geom_line()




mydata[1][,,c(1,13,25,37)] %>% slice(index=1:2, along="time") 
mydata
glimpse(mydata)

mydata2 <- mydata[1] %>% subset(month(time) == 12)
  
plot(mydata[1][,,1:10], col = hcl.colors(100), axes = TRUE)











leaflet::leaflet() %>%
  leaflet::setView(lng = 3, lat = 54, zoom = 4) %>%
  leaflet::addProviderTiles("Esri.WorldImagery") %>%
  addCopernicusWMSTiles(
    product     = ds,
    layer       = ly,
    variable    = va
  )


copernicus_product_details(product  = "GLOBAL_ANALYSISFORECAST_PHY_001_024") %>% View()

copernicus_product_details(product  = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
                           layer    = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m") %>% View()

copernicus_product_details(product  = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
                           layer    = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
                           variable = "sea_water_velocity") %>% View()

copernicus_download_motu(
  destination   = destination,
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
  variable      = "sea_water_velocity",
  output        = "netcdf",
  region        = c(-1, 50, 10, 55),
  timerange     = c("2021-01-01", "2021-01-02"),
  verticalrange = c(0, 2),
  sub_variables = c("uo", "vo"),
  overwrite     = TRUE
)

mydata <- stars::read_stars(destination)

plot(mydata["vo"], col = hcl.colors(100), axes = TRUE)




copernicus_download_motu(
  destination   = destination,
  product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
  variable      = "sea_water_velocity",
  output        = "netcdf",
  region        = c(-1, 50, 10, 55),
  timerange     = c("2021-01-01", "2021-01-02"),
  verticalrange = c(0, 2),
  sub_variables = c("uo", "vo")
)

mydata <- stars::read_stars(destination)

plot(mydata["vo"], col = hcl.colors(100), axes = TRUE)

leaflet::leaflet() %>%
  leaflet::setView(lng = 3, lat = 54, zoom = 4) %>%
  leaflet::addProviderTiles("Esri.WorldImagery") %>%
  addCopernicusWMSTiles(
    product     = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
    layer       = "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m",
    variable    = "thetao"
  )




out_dir = setwd("C://Users/MartinPastoors") # set work directory
USERNAME = "mpastoors1"
PASSWORD = "PelagicFishWithPlankton"

# Product and dataset IDs
serviceId = "NORTHWESTSHELF_ANALYSIS_FORECAST_PHY_004_013-TDS"
productId = "MetO-NWS-PHY-dm-SAL"

# Ocean Variable(s)
variable <- c("--variable so")

# Time range
date_min = ymd(20231115)               # start_date
date_max = ymd(20231204)               # end_date

# Geographic area and depth level 
lon = list(-6.93, 2.30)                # lon_min, lon_max
lat = list(47.49, 51.65)               # lat_min, lat_max
depth = list(0.49, 11.4)               # depth_min, depth_max 

while (date_min <= date_max) {    
  delta=date_min+ddays(7)                 #loop per week      
  
  # Output filename    
  out_name = paste("NWS_data_",date_min,".nc", sep="")
  
  command <- paste ("python -m motuclient --motu https://nrt.cmems-du.eu/motu-web/Motu --service-id", serviceId,                    
                    "--product-id", productId, 
                    "--longitude-min ", lon[1],"--longitude-max ", lon[2], 
                    "--latitude-min ", lat[1], "--latitude-max ", lat[2], 
                    "--date-min ", date_min, "12:00:00 --date-max ", delta, 
                    "12:00:00 --depth-min", depth[1], "--depth-max", depth[2],                    
                    variable, "--out-dir", out_dir, "--out-name", out_name, 
                    "--user", USERNAME, "--pwd", PASSWORD, sep = " ")
  
  print(paste("======= Download starting on",date_min,"======="))
  
  print(command) 
  
  date_min=date_min+ddays(8)               #time incrementation
  
  system(command, intern = TRUE)
}

print(paste("============= Download completed! All files are stored in ",out_dir,"=============", sep=" "))


