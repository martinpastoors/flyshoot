#' Title
#'
#' @param rbl
#' @param rbyl
#' @param SUR
#' @param SID
#' @param var
#' @param lab
#'
#' @return
#' @export
#'

# rbl <- cgfs_rbl; rbyl <- cgfs_rbyl; SUR="FR-CGFS"; SPECIES="MUR"; var=n; lab="test"; NROW=5

datras_length <- function(rbl, rbyl, SUR, SPECIES, var, lab = "number per length class",
                          NROW=4) {

  length.trim <-
    rbyl  %>%
    dplyr::filter(survey == SUR, species == SPECIES) %>% 
    group_by(species, length) %>% 
    reframe(N=sum(N)) %>% 
    arrange(species, length) %>% 
    group_by(species) |>
    mutate(cN = cumsum(N),
           cN.trim = 0.999 * max(cN),
           length.trim = ifelse(length > 30 & cN > cN.trim, NA, length),
           length.trim = ifelse(!is.na(length.trim), length.trim, max(length.trim, na.rm = TRUE))) |>
    dplyr::select(species, length, length.trim) |>
    ungroup()    
  
  dyl <-
    rbyl |>
    dplyr::filter(survey == SUR, species == SPECIES) %>%
    
    # fill in full cm lengths from min to max 
    select(survey, quarter, year, species, englishspecies, dutchname, length) |> # step not really needed, just added for clarity
    group_by(survey, quarter, species, englishspecies, dutchname) |>
    expand(year = full_seq(c(min(rbyl$year), max(rbyl$year)), 1),
           length = full_seq(length, 1)) |>
    
    # join back to get the N and B
    left_join(rbyl) |>
    mutate(
      N = replace_na(N, 0),
      n = replace_na(n, 0)
    ) %>% 
  
    left_join(length.trim) |>
    mutate(length = length.trim) %>% 
    
    group_by(survey, quarter, year, species, englishspecies, dutchname, length) %>% 
    summarise(
      N = sum(N, na.rm=TRUE),
      n = sum(n, na.rm=TRUE)
    )
  
  dl <- 
    rbl %>%
    dplyr::filter(survey == SUR, species == SPECIES) %>% 
    
    # fill in full cm lengths from min to max 
    select(survey, quarter, species, englishspecies, dutchname, length) |> # step not really needed, just added for clarity
    group_by(survey, quarter, species, englishspecies, dutchname) |>
    expand(length = full_seq(length, 1)) |>
    
    # join back to get the N and B
    left_join(rbl) |>
    mutate(
      n = replace_na(n, 0)
    ) %>% 
    
    left_join(length.trim) |>
    mutate(length = length.trim) %>% 
    
    group_by(survey, quarter, species, englishspecies, dutchname, length) %>% 
    summarise(
      n = sum(n, na.rm=TRUE)
    ) 
  
  # dl <- data.frame(year=rep(unique(dyl$year), each=nrow(dl))) %>% cbind(dl) 
  
  tt <-
    paste(unique(dl$survey),
          paste0("Q", unique(dl$quarter)),
          unique(dl$englishspecies),
          unique(dl$dutchname),
          sep = " / ")
  
  ggplot2::ggplot() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::geom_ribbon(data = dl,
                         ggplot2::aes(length, ymax = {{ var }}, ymin = 0), fill = "grey") +
    ggplot2::geom_line(data = dyl,
                       ggplot2::aes(length, {{ var }})) +
    ggplot2::facet_wrap(~ year, dir = "h", nrow = NROW) +
    ggplot2::labs(x = "length", y = lab) +
    ggplot2::labs(title=tt) +
    
    ggplot2::scale_x_continuous(breaks = seq(0, 200, by = 20)) 

}

