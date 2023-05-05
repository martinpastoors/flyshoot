#' Title
#'
#' @param data
#' @param SUR
#' @param SID
#' @param var
#' @param lab
#' @param cl
#'
#' @return
#' @export
#'

# data = cgfs_index; SPECIES="CTC"; YRS=2005:2022; lab="N/hour"; legend.position="right" 
datras_zero <- function(data, SPECIES, YRS=2000:2022, 
                          lab = "perc. zero hauls",
                          legend.position="right",
                         byvessel = FALSE) {
  
  
  data <-
    data %>%
    dplyr::filter(year %in% YRS) %>% 
    {if (byvessel) {
      dplyr::group_by(., survey, quarter, species, englishspecies, dutchname, year, vessel) 
    } else {
      dplyr::group_by(., survey, quarter, species, englishspecies, dutchname, year) 
    }} %>% 
    summarise(
      nhauls = n(),
      nzero = sum(N==0)
    ) %>% 
    ungroup() %>% 
    mutate(prop_zero = nzero/nhauls) %>% 
    
    # add missing years 
    ungroup() %>% 
    tidyr::complete(survey, quarter, year, nesting(species, dutchname, englishspecies))  %>% 
    
    filter(species == SPECIES)
  
  
  tt <-
    paste(unique(data$survey),
          paste0("Q", unique(data$quarter)),
          unique(data$englishspecies),
          unique(data$dutchname),
          sep = " / ")
  
  data %>% 
    ggplot2::ggplot(ggplot2::aes(year, prop_zero)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::geom_line(linewidth=0.3, colour="darkgray") +
    {if(byvessel) {ggplot2::geom_point(ggplot2::aes(colour=vessel), size=2) 
      } else {
      ggplot2::geom_point(size=2)
    }} +
    ggplot2::scale_colour_discrete(na.translate = F) +
    ggplot2::scale_y_continuous(limits=c(0,1), labels=scales::percent) +
    ggplot2::labs(x = NULL, y = lab, title=tt)
  
}

