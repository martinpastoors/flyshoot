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

# data=cgfs_rbys; SUR="FR-CGFS"; SPECIES="CTC"; YRS=2005:2022; lab = "test"; cl=cl; var = "N"
# data=cgfs_rbys; SUR="FR-CGFS"; SPECIES="COD"; YRS=2005:2022; lab = "test"; cl=cl; var = "N"

datras_bubble <- function(data, SUR, SPECIES, YRS=2000:2022, 
                          var, lab = "N/hour", cl, NROW=4,
                          legend.position="right") {


  data <-
    data %>%
    dplyr::filter(year %in% YRS,
                  survey %in% SUR,
                  species == SPECIES) %>% 
    bind_rows(data.frame(year=YRS, 
                         lat=mean(.[["lat"]], na.rm=TRUE),
                         lon=mean(.[["lon"]], na.rm=TRUE),
                         N  = NA,
                         B  = NA))

  p <-
    ggplot() +
    theme_minimal(base_size = 12) +
    theme(legend.position = legend.position) +
    scale_x_continuous(NULL, NULL, expand = expansion(0)) +
    scale_y_continuous(NULL, NULL, expand = expansion(0)) +
    geom_polygon(data = cl, aes(lon, lat, group = group), colour = "grey", fill = "grey") +
    coord_quickmap(xlim = range(data$lon), ylim = range(data$lat)) +
    theme(panel.spacing.x=unit(0.3, "lines"),panel.spacing.y=unit(0.3, "lines"))
  
  cols <- c("value" = "red", "zero" = "blue")
  shapes <- c("value" = 1, "zero" = 3)
  
  p +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(lon, lat, size = .data[[var]] ),
                        alpha = 0.2, colour = "red") +
    ggplot2::geom_point(data=data,
                        ggplot2::aes(lon, lat, colour = type, shape=type),
                        size = 0.2, show.legend = F) +
    scale_shape_manual(values=shapes) +
    scale_colour_manual(values=cols) +
    ggplot2::scale_size_area(max_size = 10) +
    ggplot2::labs(size = lab, title=paste(unique(data$survey),
                                          paste0("Q", unique(data$quarter)),
                                          unique(data$englishspecies),
                                          sep = " / ")) +
    ggplot2::facet_wrap(~ year, nrow = NROW, dir = "h", drop=FALSE) 
}

