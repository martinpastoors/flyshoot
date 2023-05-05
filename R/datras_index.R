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
datras_index <- function(data, SPECIES, YRS=2000:2022, 
                          lab = "N/hour",
                          legend.position="right",
                         byvessel = FALSE) {
  
  
  data <-
    data %>%
    dplyr::filter(year %in% YRS,
                  species == SPECIES) 

  tt <-
    paste(unique(data$survey),
          paste0("Q", unique(data$quarter)),
          unique(data$englishspecies),
          unique(data$dutchname),
          sep = " / ")
  
  data %>% 
    ggplot2::ggplot(ggplot2::aes(year, est)) +
    ggplot2::theme_minimal(base_size = 12) +
    {if(byvessel) {ggplot2::geom_pointrange(ggplot2::aes(year, est, ymin = lwr, ymax = upr, colour=vessel)) 
      } else {
      ggplot2::geom_pointrange(ggplot2::aes(year, est, ymin = lwr, ymax = upr))
    }} +
    # ggplot2::geom_segment(aes(x=2015, xend=max(year), y=0, yend=0), linewidth=0.5) +
    # ggplot2::scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = "N/hour", title=tt)
  
}

