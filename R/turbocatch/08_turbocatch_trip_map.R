# =============================================================================
# 08_turbocatch_trip_map.R
# Quick catch map for one TurboCatch trip — styled like Figure 1 of the
# FLYSHOOT tripreport harmonized.
#
# Shows: haul positions as bubbles (size = total catch kg, colour = day),
#        top-N species as facets, ICES rectangles, coastline.
#
# Usage:
#   source("08_turbocatch_trip_map.R")
#   turbocatch_trip_map(
#     parquet_dir = "C:/.../parquet/CC545762",
#     trip_id     = "20260641"
#   )
# =============================================================================

library(arrow)
library(dplyr)
library(ggplot2)
library(sf)
library(glue)
library(lubridate)
library(RColorBrewer)

# ── spatial helpers (same as tripreport) ─────────────────────────────────────

# Convert ICES rectangle code to centroid lon/lat
# e.g. "29F0" -> lon = -1.0, lat = 50.25
ices_rect_to_lonlat <- function(rect) {
  # Row part: numeric prefix (e.g. 29) -> latitude band
  # Col part: letter + digit (e.g. F0) -> longitude band
  row_num <- as.integer(substr(rect, 1, 2))
  col_let <- substr(rect, 3, 3)
  col_dig <- as.integer(substr(rect, 4, 4))

  lat <- 40 + (row_num - 1) * 0.5 + 0.25   # bottom of band + half-step
  lon_base <- (utf8ToInt(col_let) - utf8ToInt("A")) * 10 - 40
  lon <- lon_base + col_dig + 0.5

  data.frame(lon = lon, lat = lat)
}

# ── main map function ─────────────────────────────────────────────────────────

#' Plot a catch map for one TurboCatch trip
#'
#' @param parquet_dir  Folder containing turbocatch parquets (haul, elog, elog_trek, trip)
#' @param trip_id      ELOG TN value (NULL = most recent trip)
#' @param top_n        Number of top species to show as facets (default 6)
#' @param output_file  If supplied, save to this path (.png or .pdf).
#'                     If NULL, print to screen.
#' @param spatial_dir  Folder containing world.RData and icesrectangles.RData
#'                     (same location used by the tripreport). If NULL, uses
#'                     a simple rnaturalearth coastline fallback.
turbocatch_trip_map <- function(parquet_dir,
                                 trip_id      = NULL,
                                 top_n        = 6,
                                 output_file  = NULL,
                                 spatial_dir  = NULL) {

  # ── 1. Load parquet ─────────────────────────────────────────────────────
  tc_trip  <- read_parquet(file.path(parquet_dir, "trip.parquet"))
  tc_haul  <- read_parquet(file.path(parquet_dir, "haul.parquet"))

  # Catch data: combine elog_trek (per_haul regime, haul-level) and
  # elog (daily regime, day-level). For daily regime trips, haul_id = NA
  # so the map will show day-level positions from the haul table instead.
  elog_trek_path <- file.path(parquet_dir, "elog_trek.parquet")
  elog_path      <- file.path(parquet_dir, "elog.parquet")

  tc_catch <- dplyr::bind_rows(
    if (file.exists(elog_trek_path)) read_parquet(elog_trek_path) else tibble(),
    if (file.exists(elog_path))      read_parquet(elog_path)      else tibble()
  )

  if (is.null(trip_id)) {
    trip_id <- tc_trip %>% arrange(desc(departure_date)) %>% slice(1) %>% pull(trip_id)
    message(glue("Using most recent trip: {trip_id}"))
  }

  trip_meta <- tc_trip  %>% filter(trip_id == !!trip_id)
  haul      <- tc_haul  %>% filter(trip_id == !!trip_id)
  catch     <- tc_catch %>% filter(trip_id == !!trip_id)

  # For daily-regime trips haul table may be empty — fall back to elog positions
  using_elog_positions <- nrow(haul) == 0 && nrow(catch) > 0

  vessel_id <- trip_meta$vessel
  far_regime_val <- if ("far_regime" %in% names(trip_meta)) trip_meta$far_regime else "unknown" 
  dep_date  <- as.Date(trip_meta$departure_date)
  arr_date  <- as.Date(trip_meta$arrival_date)

  message(glue("Trip {trip_id} | {vessel_id} | {dep_date} – {arr_date}"))
  message(glue("  {nrow(haul)} hauls, {nrow(catch)} catch rows"))

  # ── 2. Resolve haul positions ────────────────────────────────────────────
  # Prefer shoot_lat/lon; fall back to ICES rect centroid
  haul_pos <- haul %>%
    mutate(
      pos_source = if_else(!is.na(shoot_lat) & !is.na(shoot_lon),
                           "GPS", "rect_centroid")
    )

  # Fill missing positions from ICES rect centroids
  missing_pos <- haul_pos %>% filter(pos_source == "rect_centroid" & !is.na(ices_rect))
  if (nrow(missing_pos) > 0) {
    centroids <- bind_cols(
      missing_pos %>% select(trip_id, haul_id),
      map_dfr(missing_pos$ices_rect, ices_rect_to_lonlat) %>%
        rename(shoot_lat = lat, shoot_lon = lon)
    )
    haul_pos <- haul_pos %>%
      rows_update(centroids, by = c("trip_id", "haul_id"), unmatched = "ignore")
    message(glue("  {nrow(missing_pos)} haul(s) positioned via ICES rect centroid"))
  }

  haul_pos <- haul_pos %>% filter(!is.na(shoot_lat) & !is.na(shoot_lon))

  if (nrow(haul_pos) == 0) {
    stop("No haul positions available — cannot draw map")
  }

  # ── 3. Join catch totals onto haul positions ─────────────────────────────
  catch_totals <- catch %>%
    group_by(trip_id, haul_id) %>%
    summarise(total_kg = sum(weight_kg, na.rm = TRUE), .groups = "drop")

  haul_pos <- haul_pos %>%
    left_join(catch_totals, by = c("trip_id", "haul_id")) %>%
    mutate(
      total_kg = replace_na(total_kg, 0),
      day_lbl  = factor(format(as.Date(date), "%d/%m"),
                        levels = sort(unique(format(as.Date(date), "%d/%m"))))
    )

  # ── 4. Top-N species catch per haul ─────────────────────────────────────
  top_species <- catch %>%
    group_by(species_code) %>%
    summarise(total_kg = sum(weight_kg, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_kg)) %>%
    slice_head(n = top_n) %>%
    pull(species_code)

  catch_sp <- catch %>%
    filter(species_code %in% top_species) %>%
    group_by(trip_id, haul_id, species_code) %>%
    summarise(weight_kg = sum(weight_kg, na.rm = TRUE), .groups = "drop") %>%
    left_join(haul_pos %>% select(trip_id, haul_id, shoot_lon, shoot_lat,
                                   day_lbl, pos_source),
              by = c("trip_id", "haul_id")) %>%
    mutate(species_code = factor(species_code, levels = top_species))

  # ── 5. Map extent ────────────────────────────────────────────────────────
  all_lons <- c(haul_pos$shoot_lon)
  all_lats <- c(haul_pos$shoot_lat)
  lon_buf  <- max(0.5, diff(range(all_lons, na.rm = TRUE)) * 0.15)
  lat_buf  <- max(0.3, diff(range(all_lats, na.rm = TRUE)) * 0.15)

  xlim <- range(all_lons, na.rm = TRUE) + c(-lon_buf, lon_buf)
  ylim <- range(all_lats, na.rm = TRUE) + c(-lat_buf, lat_buf)

  # ── 6. Spatial layers ────────────────────────────────────────────────────
  # Try to load from spatial_dir (same RData files as tripreport)
  world_sf  <- NULL
  ices_rect_sf <- NULL

  if (!is.null(spatial_dir)) {
    world_rdata <- file.path(spatial_dir, "world.RData")
    ices_rdata  <- file.path(spatial_dir, "icesrectangles.RData")

    if (file.exists(world_rdata)) {
      load(world_rdata)   # loads object named `world`
      world_sf <- get("world") %>% sf::st_as_sf() %>%
        sf::st_crop(sf::st_bbox(c(xmin=xlim[1], xmax=xlim[2],
                                  ymin=ylim[1], ymax=ylim[2])))
      message("  Loaded world coastline from spatial_dir")
    }
    if (file.exists(ices_rdata)) {
      load(ices_rdata)    # loads object named `icesrectangles`
      ices_rect_sf <- get("icesrectangles") %>% sf::st_as_sf() %>%
        sf::st_crop(sf::st_bbox(c(xmin=xlim[1], xmax=xlim[2],
                                  ymin=ylim[1], ymax=ylim[2])))
      message("  Loaded ICES rectangles from spatial_dir")
    }
  }

  # Fallback: rnaturalearth (no ICES rects)
  if (is.null(world_sf)) {
    if (requireNamespace("rnaturalearth", quietly = TRUE)) {
      world_sf <- rnaturalearth::ne_countries(scale = "medium",
                                               returnclass = "sf") %>%
        sf::st_crop(sf::st_bbox(c(xmin=xlim[1], xmax=xlim[2],
                                  ymin=ylim[1], ymax=ylim[2])))
      message("  Using rnaturalearth coastline (install spatial_dir for ICES rects)")
    } else {
      message("  No coastline available — install rnaturalearth or supply spatial_dir")
    }
  }

  # ── 7. Colour palette for days ───────────────────────────────────────────
  n_days   <- nlevels(haul_pos$day_lbl)
  day_cols <- if (n_days <= 8) {
    setNames(RColorBrewer::brewer.pal(max(3, n_days), "Dark2")[seq_len(n_days)],
             levels(haul_pos$day_lbl))
  } else {
    setNames(scales::hue_pal()(n_days), levels(haul_pos$day_lbl))
  }

  # ── 8. Build map ─────────────────────────────────────────────────────────
  base_map <- ggplot() +
    # ICES rectangles
    { if (!is.null(ices_rect_sf))
        geom_sf(data = ices_rect_sf, fill = NA, colour = "grey70",
                linewidth = 0.3, linetype = "dashed")
    } +
    # Coastline
    { if (!is.null(world_sf))
        geom_sf(data = world_sf, fill = "grey90", colour = "grey50",
                linewidth = 0.4)
    } +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "grey92"),
      legend.position  = "bottom",
      legend.box       = "horizontal"
    ) +
    labs(x = NULL, y = NULL,
         colour = "Day", size = "Catch (kg)",
         title  = glue("Trip {trip_id} — {vessel_id}"),
         subtitle = glue("{dep_date} – {arr_date}  |  regime: {far_regime_val}  |  ",
                         "{nrow(haul_pos)} position(s)  |  ",
                         "{round(sum(haul_pos$total_kg)/1000, 1)} t total"))

  # ── 9. Figure A: total catch per haul (all species combined) ─────────────
  fig_total <- base_map +
    geom_point(data = haul_pos,
               aes(x = shoot_lon, y = shoot_lat,
                   size  = total_kg,
                   colour = day_lbl),
               alpha = 0.8) +
    geom_text(data = haul_pos,
              aes(x = shoot_lon, y = shoot_lat, label = haul_id),
              size = 2.5, vjust = -1, colour = "grey30") +
    scale_size_area(max_size = 12) +
    scale_colour_manual(values = day_cols) +
    labs(title = glue("Trip {trip_id} — {vessel_id} — Total catch per haul"))

  # ── 10. Figure B: top-N species faceted ──────────────────────────────────
  fig_species <- base_map +
    geom_point(data = catch_sp,
               aes(x = shoot_lon, y = shoot_lat,
                   size   = weight_kg,
                   colour = day_lbl),
               alpha = 0.8) +
    scale_size_area(max_size = 10) +
    scale_colour_manual(values = day_cols) +
    facet_wrap(~species_code, ncol = 3) +
    labs(title  = glue("Trip {trip_id} — {vessel_id} — Catch by species"),
         subtitle = glue("{dep_date} – {arr_date}  |  top {top_n} species by weight"))

  # ── 11. Output ────────────────────────────────────────────────────────────
  if (!is.null(output_file)) {
    ext <- tolower(tools::file_ext(output_file))

    # Two-page PDF or two PNG files
    if (ext == "pdf") {
      pdf(output_file, width = 10, height = 7)
      print(fig_total)
      print(fig_species)
      dev.off()
      message(glue("  ✓ Saved: {output_file}"))
    } else {
      # PNG: save as _total and _species
      base_path <- sub("\\.[^.]+$", "", output_file)
      total_path   <- paste0(base_path, "_total.",   ext)
      species_path <- paste0(base_path, "_species.", ext)
      ggsave(total_path,   fig_total,   width = 10, height = 7, dpi = 150)
      ggsave(species_path, fig_species, width = 10, height = 9, dpi = 150)
      message(glue("  ✓ Saved: {total_path}"))
      message(glue("  ✓ Saved: {species_path}"))
    }
  } else {
    print(fig_total)
    readline("Press Enter for species facet map...")
    print(fig_species)
  }

  invisible(list(total = fig_total, species = fig_species))
}

# =============================================================================
# EXAMPLE
# =============================================================================
# source("08_turbocatch_trip_map.R")
#
# # Print to screen
# turbocatch_trip_map(
#   parquet_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch",
#   trip_id     = "20260641"
# )
#
# # Save to PDF (two pages)
# turbocatch_trip_map(
#   parquet_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch",
#   trip_id     = "20260641",
#   output_file = "C:/.../reports/CC545762_20260641_map.pdf",
#   spatial_dir = "C:/Users/MartinPastoors/DATA/RDATA"
# )
# =============================================================================
