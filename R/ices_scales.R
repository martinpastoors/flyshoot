scale_longitude_ices <- function(min = -44, max = 68.5, step = 1, ...) {
  breaks <- seq(min + 0.5, max - 0.5, step)
  labels <- d2ir(60, breaks) %>% stringr::str_sub(3)
  return(ggplot2::scale_x_continuous(name = NULL, breaks = breaks, labels = labels, ...))
}

scale_latitude_ices <- function(min = 36, max = 84.5, step = 0.5, ...) {
  breaks <- seq(min + 0.25, max - 0.25, step)
  labels <- d2ir(breaks, 0) %>% stringr::str_sub(1, 2)
  return(ggplot2::scale_y_continuous(name = NULL, breaks = breaks, labels = labels, ...))
}
