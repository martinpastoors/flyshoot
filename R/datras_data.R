#' Download and tidy length data
#'
#' @param survey_quarter like "NS-IBTS_3"
#' @param years like "2000:2022"
#'
#' @return a tibble
#' @export
#'
datras_from_web <- function(survey_quarter = "FR-CGFS_4", years = 2005:2022) {

  s <- stringr::str_sub(survey_quarter, 1, nchar(survey_quarter) - 2)
  q <- stringr::str_sub(survey_quarter, nchar(survey_quarter)) |> as.integer()

  HH <-
    dr_getdata("HH", s, years, q)
  hh <-
    HH |>
    dr_tidy() %>%                                # Make tidy with column specifications
    dr_idunite(., remove = FALSE) %>%            # Add identifier
    dplyr::left_join(reco, by=c("ship" = "code"))  # Add vesselname

  HL <-
    tidyices::dr_getdata("HL", s, years, q)
  hl <-
    HL |>
    dr_tidy() %>%                               # Make tidy with column specifications
    dr_idunite(., remove = FALSE) |>            # Add identifier
    dplyr::left_join(aphia_latin) %>%                  # Add scientific name
    dplyr::left_join(asfis) %>%                        # Add FAO species names and codes
    dplyr::left_join(reco, by = c("ship" = "code")) %>%    # Add vesselname
    dplyr::left_join(cgfs_corr) %>%
    dplyr::mutate(factor = ifelse(is.na(factor),1,factor), hlnoatlngt = hlnoatlngt * factor)

  hl <-
    hl |>
    dr_calccpue(hh) |>                          # Calculated CPUE per hour
    #                                           # fill in the zero's
    dplyr::mutate(length = floor(length)) |>
    dplyr::group_by(id, english_name, length) |>
    dplyr::summarise(cpue_number_per_hour = mean(cpue_number_per_hour),
              .groups = "drop") |>
    tidyr::spread(length, cpue_number_per_hour, fill = 0) |>
    tidyr::gather(key = length,
           value = n,
           -c(id, english_name),
           convert = TRUE) |>
    dplyr::group_by(english_name) |>
    dplyr::filter(length <= max(length[n != 0])) |>
    dplyr::ungroup()

  # get some stations details that may be of interst to use downstream
  res <-
    hh |>
    dplyr::left_join(hl)

  return(res)

}

# survey = "NS-IBTS";quarters = 1;years = 2005:2022;folder="C:/DATA/DATRAS/raw" 
# survey = "FR-CGFS";quarters = 4;years = 2005:2022;folder="C:/DATA/DATRAS/raw";metric = "cpue_number_per_hour"

datras_from_file <- function(survey = "FR-CGFS",
                             quarters = 4,
                             years = 2005:2022,
                             folder="C:/DATA/DATRAS/raw",
                             metric = "cpue_number_per_hour") {
  
  HH <-
    readr::read_rds(file.path(folder,paste0(tolower(survey),"_hh.rds")))
  
  hh <-
    HH |>
    dr_tidy() %>%                                # Make tidy with column specifications
    filter(year %in% years, quarter %in% quarters) %>%
    dr_idunite(., remove = FALSE) %>%            # Add identifier
    dplyr::left_join(reco, by=c("ship" = "code"))  # Add vesselname
  
  # print(names(hh))
  
  HL <-
    readr::read_rds(file.path(folder,paste0(tolower(survey),"_hl.rds")))
  hl <-
    HL |>
    dr_tidy() %>%                               # Make tidy with column specifications
    filter(year %in% years, quarter %in% quarters) %>%
    dr_idunite(., remove = FALSE) |>            # Add identifier
    dplyr::left_join(aphia_latin) %>%                  # Add scientific name
    dplyr::left_join(asfis) %>%                        # Add FAO species names and codes
    dplyr::left_join(reco, by = c("ship" = "code"))    # Add vesselname
  
  # print(names(hl))
  
  # d <- hl
  hl <-
    hl |>
    dr_calccpue(hh) |>                          # Calculated CPUE per hour
    #                                           # fill in the zero's
    dplyr::mutate(length = floor(length)) |>
    dplyr::group_by(id, species, latin, english_name, length) |>
    dplyr::summarise(value = mean(get(metric)),
                     .groups = "drop") |>
    tidyr::spread(length, value, fill = 0) |>
    dplyr::mutate(metric = metric) |>
    tidyr::gather(key = length,
                  value = n,
                  -c(id, species, latin, english_name, metric),
                  convert = TRUE) |>
    dplyr::group_by(species, latin, english_name) |>
    dplyr::filter(length <= max(length[n != 0])) |>
    dplyr::ungroup() 
    
    #left join hh
    # dplyr::left_join(hh %>% dplyr::select(id,
    #                                       datatype, hauldur, statrec, shootlong, shootlat, haulval),
    #                  by = "id") |>
    # 
    # # catch per hour
    # dplyr::mutate(cpue_number_per_hour = dplyr::case_when(
    #   datatype %in% c("S", "R")  ~ hlnoatlngt * 60 / hauldur,
    #   datatype == "C"            ~ hlnoatlngt,
    #   TRUE                       ~ as.numeric(NA)))
  
  # get some stations details that may be of interst to use downstream
  res <-
    hh |>
    dplyr::left_join(hl)
  
  return(res)
  
}


datras_hh_from_file <- function(survey = "FR-CGFS",
                             quarters = 4,
                             years = 2005:2022,
                             folder="C:/DATA/DATRAS/raw") {
  
  HH <-
    readr::read_rds(file.path(folder,paste0(tolower(survey),"_hh.rds")))
  
  hh <-
    HH |>
    dr_tidy() %>%                                # Make tidy with column specifications
    filter(year %in% years, quarter %in% quarters) %>%
    dr_idunite(., remove = FALSE) %>%            # Add identifier
    dplyr::left_join(reco, by=c("ship" = "code"))  %>% # Add vesselname
    
    dplyr::mutate(sweptarea = case_when(
      is.na(wingspread) & survey == "FR-CGFS"  ~ distance * (7.08 + 2.54*log(depth))/1000000,
      !is.na(wingspread)                       ~ distance * wingspread / 1000000
    )) %>% 
    
    dplyr::mutate(sweptareatype = case_when(
      is.na(wingspread) & survey == "FR-CGFS"  ~ "formula",
      !is.na(wingspread)                       ~ "data"
    )) 
  
    
  return(hh)
  
  }
