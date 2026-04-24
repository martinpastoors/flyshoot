flatten_for_csv <- function(data) {
  # Identify list columns
  list_cols <- names(data)[sapply(data, is.list)]
  
  if (length(list_cols) > 0) {
    message(sprintf("  Converting %d list columns for CSV export", length(list_cols)))
    
    # Convert list columns to JSON strings or remove them
    data_flat <- data %>%
      dplyr::mutate(across(
        all_of(list_cols),
        ~sapply(., function(x) {
          if (is.null(x) || length(x) == 0) {
            return(NA_character_)
          } else {
            return(jsonlite::toJSON(x, auto_unbox = TRUE))
          }
        })
      ))
    
    return(data_flat)
  }
  
  return(data)
}

# ============================================================================
# Function: Update cumulative datasets (IMPROVED)
# ============================================================================
update_cumulative_data <- function(new_events,
                                   new_trips,
                                   new_events_with_trips,
                                   output_dir,
                                   save_csv = FALSE) {
  
  message("\n=== Updating Cumulative Data ===")
  
  cumulative_events_file <- file.path(output_dir, "gfw_events_cumulative.rds")
  cumulative_trips_file <- file.path(output_dir, "gfw_trips_cumulative.rds")
  cumulative_events_trips_file <- file.path(output_dir, "gfw_events_by_trip_cumulative.rds")
  
  # ---- EVENTS ----
  if (!is.null(new_events) && nrow(new_events) > 0) {
    
    if (file.exists(cumulative_events_file)) {
      existing_events <- readRDS(cumulative_events_file)
      
      # IMPROVED: Remove old versions of events that appear in new data
      # This allows GFW updates to replace old data
      existing_events <- existing_events %>%
        dplyr::filter(!eventId %in% new_events$eventId)
      
      # Add new events
      all_events <- dplyr::bind_rows(existing_events, new_events)
      
      message(sprintf("Events: %d existing + %d new = %d total (replaced %d updated)",
                      nrow(existing_events), 
                      nrow(new_events),
                      nrow(all_events),
                      nrow(existing_events) + nrow(new_events) - nrow(all_events)))
    } else {
      all_events <- new_events
      message(sprintf("Events: %d new (first run)", nrow(all_events)))
    }
    
    # Save
    saveRDS(all_events, cumulative_events_file)
    
    # Also save as CSV (optional)
    if (save_csv) {
      cumulative_events_csv <- file.path(output_dir, "gfw_events_cumulative.csv")
      all_events_flat <- flatten_for_csv(all_events)
      write.csv(all_events_flat, cumulative_events_csv, row.names = FALSE)
    }
    
  } else {
    all_events <- NULL
  }
  
  # ---- TRIPS ----
  if (!is.null(new_trips) && nrow(new_trips) > 0) {
    
    if (file.exists(cumulative_trips_file)) {
      existing_trips <- readRDS(cumulative_trips_file)
      
      # IMPROVED: Remove old versions of trips that appear in new data
      existing_trips <- existing_trips %>%
        dplyr::filter(!trip_id %in% new_trips$trip_id)
      
      # Add new trips
      all_trips <- dplyr::bind_rows(existing_trips, new_trips)
      
      message(sprintf("Trips: %d existing + %d new = %d total (replaced %d updated)",
                      nrow(existing_trips),
                      nrow(new_trips),
                      nrow(all_trips),
                      nrow(existing_trips) + nrow(new_trips) - nrow(all_trips)))
    } else {
      all_trips <- new_trips
      message(sprintf("Trips: %d new (first run)", nrow(all_trips)))
    }
    
    # Save
    saveRDS(all_trips, cumulative_trips_file)
    
    # Also save as CSV (optional)
    if (save_csv) {
      cumulative_trips_csv <- file.path(output_dir, "gfw_trips_cumulative.csv")
      all_trips_flat <- flatten_for_csv(all_trips)
      write.csv(all_trips_flat, cumulative_trips_csv, row.names = FALSE)
    }
    
  } else {
    all_trips <- NULL
  }
  
  # ---- EVENTS WITH TRIPS ----
  if (!is.null(new_events_with_trips) && nrow(new_events_with_trips) > 0) {
    
    if (file.exists(cumulative_events_trips_file)) {
      existing_events_trips <- readRDS(cumulative_events_trips_file)
      
      # IMPROVED: Remove old versions
      existing_events_trips <- existing_events_trips %>%
        dplyr::filter(!eventId %in% new_events_with_trips$eventId)
      
      # Add new
      all_events_trips <- dplyr::bind_rows(existing_events_trips, new_events_with_trips)
      
      message(sprintf("Events by trip: %d existing + %d new = %d total",
                      nrow(existing_events_trips),
                      nrow(new_events_with_trips),
                      nrow(all_events_trips)))
    } else {
      all_events_trips <- new_events_with_trips
      message(sprintf("Events by trip: %d new (first run)", nrow(all_events_trips)))
    }
    
    # Save
    saveRDS(all_events_trips, cumulative_events_trips_file)
    
    # Also save as CSV (optional)
    if (save_csv) {
      cumulative_events_trips_csv <- file.path(output_dir, "gfw_events_by_trip_cumulative.csv")
      all_events_trips_flat <- flatten_for_csv(all_events_trips)
      write.csv(all_events_trips_flat, cumulative_events_trips_csv, row.names = FALSE)
    }
    
  } else {
    all_events_trips <- NULL
  }
  
  message("=== Cumulative Data Updated ===\n")
  
  return(list(
    events = all_events,
    trips = all_trips,
    events_by_trip = all_events_trips
  ))
}


# ============================================================================
# Function: Plot trips overview (FIXED - handles missing trip_id)
# ============================================================================
plot_trips_overview <- function(trips_data, 
                                events_data = NULL) {
  
  require(ggplot2)
  require(dplyr)
  require(maps)
  
  message("Creating overview plot...")
  
  # Clean data
  trips_clean <- trips_data %>%
    dplyr::filter(
      !is.na(port_departure_lon),
      !is.na(port_departure_lat),
      !is.na(port_arrival_lon),
      !is.na(port_arrival_lat)
    )
  
  message(sprintf("Plotting %d trips", nrow(trips_clean)))
  
  # Calculate bounds
  lon_range <- range(c(trips_clean$port_departure_lon, trips_clean$port_arrival_lon))
  lat_range <- range(c(trips_clean$port_departure_lat, trips_clean$port_arrival_lat))
  
  xlim <- lon_range + c(-1, 1)
  ylim <- lat_range + c(-1, 1)
  
  # Get coastline
  world_map <- map_data("world") %>%
    dplyr::filter(
      long >= xlim[1] & long <= xlim[2],
      lat >= ylim[1] & lat <= ylim[2]
    )
  
  # Create plot
  p <- ggplot() +
    geom_polygon(data = world_map,
                 aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "grey60", linewidth = 0.3) +
    geom_segment(data = trips_clean,
                 aes(x = port_departure_lon, y = port_departure_lat,
                     xend = port_arrival_lon, yend = port_arrival_lat,
                     color = trip_id),
                 alpha = 0.6, 
                 linewidth = 0.8,
                 arrow = arrow(length = unit(0.15, "cm"))) +
    geom_point(data = trips_clean,
               aes(x = port_departure_lon, y = port_departure_lat, 
                   color = trip_id),
               size = 2, shape = 16) +
    geom_point(data = trips_clean,
               aes(x = port_arrival_lon, y = port_arrival_lat, 
                   color = trip_id),
               size = 2, shape = 17) +
    coord_map(xlim = xlim, ylim = ylim) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "lightblue"),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.3)
    ) +
    labs(
      title = "Fishing Vessel Trips Overview",
      subtitle = sprintf("%d trips from %d vessels", 
                         nrow(trips_clean), 
                         length(unique(trips_clean$vessel_name)))
    )
  
  # Add events if provided AND if they have trip_id
  if (!is.null(events_data)) {
    if ("trip_id" %in% names(events_data)) {
      message("Adding event locations...")
      
      events_clean <- events_data %>%
        dplyr::filter(
          !is.na(trip_id), 
          !is.na(lat), 
          !is.na(lon),
          lat >= ylim[1] & lat <= ylim[2],
          lon >= xlim[1] & lon <= xlim[2]
        )
      
      if (nrow(events_clean) > 0) {
        p <- p + 
          geom_point(data = events_clean,
                     aes(x = lon, y = lat, color = trip_id),
                     size = 1, shape = 4, alpha = 0.5)
        
        message(sprintf("  Added %d events", nrow(events_clean)))
      }
    } else {
      message("Note: events_data doesn't have 'trip_id' column - skipping events")
      message("  Use gfw_events_by_trip_cumulative.rds for events with trip assignments")
    }
  }
  
  return(p)
}

# ============================================================================
# Function: Plot trips on map with faceting (FIXED)
# ============================================================================
plot_trips_map <- function(trips_data, 
                           events_data = NULL,
                           facet_by_month = TRUE,
                           facet_by_vessel = TRUE,
                           point_size = 2,
                           line_alpha = 0.6) {
  
  require(ggplot2)
  require(dplyr)
  require(maps)
  require(lubridate)
  
  message("Preparing trip visualization...")
  
  # Clean trips data
  trips_clean <- trips_data %>%
    dplyr::filter(
      !is.na(port_departure_lon),
      !is.na(port_departure_lat),
      !is.na(port_arrival_lon),
      !is.na(port_arrival_lat)
    ) %>%
    dplyr::mutate(
      month_label = format(lubridate::floor_date(port_departure, "month"), "%Y-%m")
    )
  
  if (nrow(trips_clean) == 0) {
    stop("No valid trips with complete coordinate data")
  }
  
  message(sprintf("Using %d trips (removed %d with missing coordinates)", 
                  nrow(trips_clean), 
                  nrow(trips_data) - nrow(trips_clean)))
  
  # Calculate map bounds
  lon_range <- range(c(trips_clean$port_departure_lon, trips_clean$port_arrival_lon))
  lat_range <- range(c(trips_clean$port_departure_lat, trips_clean$port_arrival_lat))
  
  xlim <- lon_range + c(-1, 1)
  ylim <- lat_range + c(-1, 1)
  
  message(sprintf("Map extent: lon %.1f to %.1f, lat %.1f to %.1f", 
                  xlim[1], xlim[2], ylim[1], ylim[2]))
  
  # Get coastline data
  world_map <- map_data("world") %>%
    dplyr::filter(
      long >= xlim[1] & long <= xlim[2],
      lat >= ylim[1] & lat <= ylim[2]
    )
  
  # Create plot
  p <- ggplot() +
    geom_polygon(data = world_map,
                 aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "grey60", linewidth = 0.3) +
    geom_segment(data = trips_clean,
                 aes(x = port_departure_lon, y = port_departure_lat,
                     xend = port_arrival_lon, yend = port_arrival_lat,
                     color = trip_id),
                 alpha = line_alpha, 
                 linewidth = 0.8,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    geom_point(data = trips_clean,
               aes(x = port_departure_lon, y = port_departure_lat, 
                   color = trip_id),
               size = point_size, 
               shape = 16, 
               alpha = 0.8) +
    geom_point(data = trips_clean,
               aes(x = port_arrival_lon, y = port_arrival_lat, 
                   color = trip_id),
               size = point_size, 
               shape = 17, 
               alpha = 0.8) +
    coord_map(xlim = xlim, ylim = ylim) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightblue", color = NA),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      strip.text = element_text(size = 9, face = "bold"),
      axis.title = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    labs(
      title = "Fishing Vessel Trips",
      subtitle = sprintf("%d trips from %d vessels", 
                         nrow(trips_clean), 
                         length(unique(trips_clean$vessel_name)))
    )
  
  # Add event points if provided
  if (!is.null(events_data)) {
    if ("trip_id" %in% names(events_data)) {
      message("Adding event locations...")
      
      events_clean <- events_data %>%
        dplyr::filter(
          !is.na(trip_id), 
          !is.na(lat), 
          !is.na(lon),
          lat >= ylim[1] & lat <= ylim[2],
          lon >= xlim[1] & lon <= xlim[2]
        ) %>%
        dplyr::mutate(month_label = format(lubridate::floor_date(start, "month"), "%Y-%m"))
      
      if (nrow(events_clean) > 0) {
        p <- p +
          geom_point(data = events_clean,
                     aes(x = lon, y = lat, color = trip_id),
                     size = point_size * 0.7, 
                     shape = 4,
                     alpha = 0.5)
        
        message(sprintf("  Added %d event locations", nrow(events_clean)))
      }
    } else {
      message("Note: events_data doesn't have 'trip_id' - skipping events")
    }
  }
  
  # Add faceting
  if (facet_by_month && facet_by_vessel) {
    p <- p + facet_grid(vessel_name ~ month_label)
  } else if (facet_by_month) {
    p <- p + facet_wrap(~ month_label, ncol = 3)
  } else if (facet_by_vessel) {
    p <- p + facet_wrap(~ vessel_name, ncol = 2)
  }
  
  message("Plot created successfully!")
  
  return(p)
}

# ============================================================================
# Function: Single vessel timeline (FIXED)
# ============================================================================
plot_vessel_trips_timeline <- function(trips_data, 
                                       vessel_name,
                                       events_data = NULL) {
  
  require(ggplot2)
  require(dplyr)
  require(maps)
  require(lubridate)
  
  message(sprintf("Plotting trips for: %s", vessel_name))
  
  # Filter to specific vessel
  vessel_trips <- trips_data %>%
    dplyr::filter(vessel_name == !!vessel_name) %>%
    dplyr::filter(
      !is.na(port_departure_lon),
      !is.na(port_departure_lat),
      !is.na(port_arrival_lon),
      !is.na(port_arrival_lat)
    ) %>%
    dplyr::mutate(month_label = format(lubridate::floor_date(port_departure, "month"), "%Y-%m"))
  
  if (nrow(vessel_trips) == 0) {
    stop(sprintf("No valid trips found for vessel: %s", vessel_name))
  }
  
  message(sprintf("Found %d trips", nrow(vessel_trips)))
  
  # Calculate bounds
  lon_range <- range(c(vessel_trips$port_departure_lon, vessel_trips$port_arrival_lon))
  lat_range <- range(c(vessel_trips$port_departure_lat, vessel_trips$port_arrival_lat))
  
  xlim <- lon_range + c(-1, 1)
  ylim <- lat_range + c(-1, 1)
  
  # Get coastline
  world_map <- map_data("world") %>%
    dplyr::filter(
      long >= xlim[1] & long <= xlim[2],
      lat >= ylim[1] & lat <= ylim[2]
    )
  
  # Create plot
  p <- ggplot() +
    geom_polygon(data = world_map,
                 aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "grey60", linewidth = 0.3) +
    geom_segment(data = vessel_trips,
                 aes(x = port_departure_lon, y = port_departure_lat,
                     xend = port_arrival_lon, yend = port_arrival_lat,
                     color = trip_id),
                 alpha = 0.6, 
                 linewidth = 1,
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    geom_point(data = vessel_trips,
               aes(x = port_departure_lon, y = port_departure_lat, 
                   color = trip_id),
               size = 3, shape = 16) +
    geom_point(data = vessel_trips,
               aes(x = port_arrival_lon, y = port_arrival_lat, 
                   color = trip_id),
               size = 3, shape = 17) +
    coord_map(xlim = xlim, ylim = ylim) +
    facet_wrap(~ month_label, ncol = 3) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "lightblue"),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      strip.text = element_text(size = 9, face = "bold")
    ) +
    labs(
      title = sprintf("Trip Timeline: %s", vessel_name),
      subtitle = sprintf("%d trips", nrow(vessel_trips))
    )
  
  # Add events if provided
  if (!is.null(events_data) && "trip_id" %in% names(events_data)) {
    vessel_events <- events_data %>%
      dplyr::filter(
        trip_id %in% vessel_trips$trip_id,
        !is.na(lat), 
        !is.na(lon),
        lat >= ylim[1] & lat <= ylim[2],
        lon >= xlim[1] & lon <= xlim[2]
      ) %>%
      dplyr::mutate(month_label = format(lubridate::floor_date(start, "month"), "%Y-%m"))
    
    if (nrow(vessel_events) > 0) {
      p <- p + 
        geom_point(data = vessel_events,
                   aes(x = lon, y = lat, color = trip_id),
                   size = 1.5, shape = 4, alpha = 0.6)
      
      message(sprintf("Added %d events", nrow(vessel_events)))
    }
  }
  
  return(p)
}# ============================================================================
# Function: Dumbbell plot for trip overview
# ============================================================================
plot_trips_dumbbell <- function(trips_data, 
                                events_data = NULL,
                                show_fishing_events = TRUE,
                                color_by = "vessel",
                                facet_by_month = FALSE,
                                date_range = NULL,
                                show_trip_numbers = TRUE,
                                point_size = 3,
                                line_size = 1.5) {
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  require(lubridate)
  require(scales)
  
  message("Creating dumbbell plot...")
  
  # Filter date range if specified
  if (!is.null(date_range)) {
    trips_data <- trips_data %>%
      dplyr::filter(
        port_departure >= date_range[1],
        port_departure <= date_range[2]
      )
  }
  
  # Extract trip number (last 3 digits)
  trips_plot <- trips_data %>%
    dplyr::filter(
      !is.na(port_departure),
      !is.na(port_arrival)
    ) %>%
    dplyr::mutate(
      trip_num_label = sprintf("%03d", trip_number),
      month_label = format(lubridate::floor_date(port_departure, "month"), "%Y-%m"),
      # For positioning the label at midpoint
      label_date = port_departure + (port_arrival - port_departure) / 2
    ) %>%
    dplyr::arrange(vessel_name, port_departure)
  
  if (nrow(trips_plot) == 0) {
    stop("No valid trips to plot")
  }
  
  message(sprintf("Plotting %d trips from %d vessels", 
                  nrow(trips_plot), 
                  length(unique(trips_plot$vessel_name))))
  
  # Prepare fishing events if provided
  fishing_events <- NULL
  if (!is.null(events_data) && show_fishing_events) {
    fishing_events <- events_data %>%
      dplyr::filter(
        event_type_requested == "FISHING",
        !is.na(trip_id),
        !is.na(start),
        trip_id %in% trips_plot$trip_id,
        !is.na(vessel_name)  # vessel_name already exists in events_data
      ) %>%
      dplyr::mutate(
        month_label = format(lubridate::floor_date(start, "month"), "%Y-%m")
      )
    
    if (nrow(fishing_events) > 0) {
      message(sprintf("Adding %d fishing events", nrow(fishing_events)))
    } else {
      fishing_events <- NULL
    }
  }
  
  # Determine color aesthetic
  if (color_by == "vessel") {
    color_aes <- aes(color = vessel_name)
    color_label <- "Vessel"
  } else if (color_by == "duration") {
    color_aes <- aes(color = trip_duration_hours)
    color_label <- "Duration (hours)"
  } else {
    color_aes <- aes(color = vessel_name)
    color_label <- "Vessel"
  }
  
  # Create base plot
  p <- ggplot(trips_plot, aes(y = vessel_name)) +
    ggalt::geom_dumbbell(
      aes(x = port_departure, xend = port_arrival),
      size = line_size,
      size_x = point_size,
      size_xend = point_size,
      colour = "grey70",
      colour_x = "#2b8cbe",    # Departure color (blue)
      colour_xend = "#de2d26",  # Arrival color (red)
      alpha = 0.7
    )
  
  # Add fishing events as vertical lines
  if (!is.null(fishing_events)) {
    p <- p +
      geom_segment(
        data = fishing_events,
        aes(x = start, xend = start, 
            y = as.numeric(vessel_name) - 0.3,
            yend = as.numeric(vessel_name) + 0.3),
        color = "#238b45",  # Green color for fishing
        linewidth = 0.6,
        alpha = 0.7
      )
  }
  
  p <- p +
    scale_x_datetime(
      date_breaks = "1 week",
      date_labels = "%b %d",
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey95"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      strip.text = element_text(size = 10, face = "bold")
    ) +
    labs(
      x = "Date",
      y = NULL,
      title = "Fishing Vessel Trips",
      subtitle = sprintf("%d trips | Blue = Departure, Red = Arrival%s",
                         nrow(trips_plot),
                         if (!is.null(fishing_events)) ", Green = Fishing" else "")
    )
  
  # Add trip number labels if requested
  if (show_trip_numbers) {
    p <- p +
      geom_text(
        aes(x = label_date, label = trip_num_label),
        size = 2.5,
        vjust = -0.8,
        color = "grey30",
        fontface = "bold"
      )
  }
  
  # Add faceting if requested
  if (facet_by_month) {
    p <- p + facet_wrap(~ month_label, scales = "free_x", ncol = 2)
  }
  
  message("Plot created successfully!")
  
  return(p)
}

# ============================================================================
# Function: Compact dumbbell plot (one vessel per row, trips side by side)
# ============================================================================
plot_trips_dumbbell_compact <- function(trips_data,
                                        events_data = NULL,
                                        show_fishing_events = TRUE,
                                        date_range = NULL,
                                        show_trip_numbers = TRUE,
                                        point_size = 2,
                                        line_size = 1) {
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  require(lubridate)
  require(forcats)
  
  message("Creating compact dumbbell plot...")
  
  # Filter date range if specified
  if (!is.null(date_range)) {
    trips_data <- trips_data %>%
      dplyr::filter(
        port_departure >= date_range[1],
        port_departure <= date_range[2]
      )
  }
  
  # Prepare data
  trips_plot <- trips_data %>%
    dplyr::filter(
      !is.na(port_departure),
      !is.na(port_arrival)
    ) %>%
    dplyr::mutate(
      trip_num_label = sprintf("%03d", trip_number),
      label_date = port_departure + (port_arrival - port_departure) / 2,
      # Create a row ID for each trip within vessel
      trip_row = dplyr::row_number()
    ) %>%
    dplyr::arrange(vessel_name, port_departure)
  
  if (nrow(trips_plot) == 0) {
    stop("No valid trips to plot")
  }
  
  message(sprintf("Plotting %d trips from %d vessels", 
                  nrow(trips_plot), 
                  length(unique(trips_plot$vessel_name))))
  
  # Prepare fishing events if provided
  fishing_events <- NULL
  if (!is.null(events_data) && show_fishing_events) {
    fishing_events <- events_data %>%
      dplyr::filter(
        event_type_requested == "FISHING",
        !is.na(trip_id),
        !is.na(start),
        trip_id %in% trips_plot$trip_id,
        !is.na(vessel_name)  # vessel_name already exists in events_data
      )
    
    if (nrow(fishing_events) > 0) {
      message(sprintf("Adding %d fishing events", nrow(fishing_events)))
    } else {
      fishing_events <- NULL
    }
  }
  
  # Create plot with all trips for each vessel on same row
  p <- ggplot(trips_plot, aes(y = forcats::fct_rev(vessel_name))) +
    ggalt::geom_dumbbell(
      aes(x = port_departure, xend = port_arrival),
      size = line_size,
      size_x = point_size,
      size_xend = point_size,
      colour = "grey60",
      colour_x = "#2b8cbe",
      colour_xend = "#de2d26",
      alpha = 0.8
    )
  
  # Add fishing events as vertical lines
  if (!is.null(fishing_events)) {
    p <- p +
      geom_segment(
        data = fishing_events,
        aes(x = start, xend = start, 
            y = as.numeric(forcats::fct_rev(vessel_name)) - 0.3,
            yend = as.numeric(forcats::fct_rev(vessel_name)) + 0.3),
        color = "#238b45",  # Green color for fishing
        linewidth = 0.6,
        alpha = 0.7
      )
  }
  
  p <- p +
    scale_x_datetime(
      date_breaks = "1 week",
      date_labels = "%b %d",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey95", linewidth = 0.3),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(
      x = "Date",
      y = NULL,
      title = "Fishing Vessel Trips Timeline",
      subtitle = sprintf("%d trips from %d vessels | Blue = Departure, Red = Arrival%s",
                         nrow(trips_plot),
                         length(unique(trips_plot$vessel_name)),
                         if (!is.null(fishing_events)) ", Green = Fishing" else "")
    )
  
  # Add trip number labels
  if (show_trip_numbers) {
    p <- p +
      geom_text(
        aes(x = label_date, label = trip_num_label),
        size = 2.2,
        vjust = -0.7,
        color = "grey20",
        fontface = "bold"
      )
  }
  
  message("Plot created successfully!")
  
  return(p)
}

# ============================================================================
# Function: Detailed dumbbell plot (each trip on separate row)
# ============================================================================
plot_trips_dumbbell_detailed <- function(trips_data,
                                         events_data = NULL,
                                         show_fishing_events = TRUE,
                                         vessel_filter = NULL,
                                         date_range = NULL,
                                         color_by_duration = FALSE,
                                         show_trip_numbers = TRUE) {
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  require(lubridate)
  require(forcats)
  require(scales)
  
  message("Creating detailed dumbbell plot...")
  
  # Filter by vessel if specified
  if (!is.null(vessel_filter)) {
    trips_data <- trips_data %>%
      dplyr::filter(vessel_name %in% vessel_filter)
  }
  
  # Filter date range if specified
  if (!is.null(date_range)) {
    trips_data <- trips_data %>%
      dplyr::filter(
        port_departure >= date_range[1],
        port_departure <= date_range[2]
      )
  }
  
  # Prepare data - each trip gets its own row
  trips_plot <- trips_data %>%
    dplyr::filter(
      !is.na(port_departure),
      !is.na(port_arrival)
    ) %>%
    dplyr::mutate(
      trip_num_label = sprintf("%03d", trip_number),
      trip_label = paste0(vessel_name, " - ", trip_num_label),
      label_date = port_departure + (port_arrival - port_departure) / 2,
      duration_days = as.numeric(difftime(port_arrival, port_departure, units = "days"))
    ) %>%
    dplyr::arrange(vessel_name, port_departure) %>%
    dplyr::mutate(
      trip_label = forcats::fct_inorder(trip_label)
    )
  
  if (nrow(trips_plot) == 0) {
    stop("No valid trips to plot")
  }
  
  message(sprintf("Plotting %d trips from %d vessels", 
                  nrow(trips_plot), 
                  length(unique(trips_plot$vessel_name))))
  
  # Prepare fishing events if provided
  fishing_events <- NULL
  if (!is.null(events_data) && show_fishing_events) {
    fishing_events <- events_data %>%
      dplyr::filter(
        event_type_requested == "FISHING",
        !is.na(trip_id),
        !is.na(start),
        trip_id %in% trips_plot$trip_id
      ) %>%
      dplyr::left_join(
        trips_plot %>% dplyr::select(trip_id, trip_label),
        by = "trip_id",
        relationship = "many-to-one"  # Multiple events can belong to one trip
      ) %>%
      dplyr::filter(!is.na(trip_label))
    
    if (nrow(fishing_events) > 0) {
      message(sprintf("Adding %d fishing events", nrow(fishing_events)))
    } else {
      fishing_events <- NULL
    }
  }
  
  # Create plot
  if (color_by_duration) {
    p <- ggplot(trips_plot, aes(y = forcats::fct_rev(trip_label))) +
      ggalt::geom_dumbbell(
        aes(x = port_departure, xend = port_arrival, colour = duration_days),
        size = 1.5,
        size_x = 2.5,
        size_xend = 2.5,
        alpha = 0.8
      ) +
      scale_color_viridis_c(name = "Duration\n(days)", option = "plasma")
  } else {
    p <- ggplot(trips_plot, aes(y = forcats::fct_rev(trip_label))) +
      ggalt::geom_dumbbell(
        aes(x = port_departure, xend = port_arrival),
        size = 1.5,
        size_x = 2.5,
        size_xend = 2.5,
        colour = "grey60",
        colour_x = "#2b8cbe",
        colour_xend = "#de2d26",
        alpha = 0.8
      )
  }
  
  # Add fishing events as vertical lines
  if (!is.null(fishing_events)) {
    p <- p +
      geom_segment(
        data = fishing_events,
        aes(x = start, xend = start, 
            y = as.numeric(forcats::fct_rev(trip_label)) - 0.35,
            yend = as.numeric(forcats::fct_rev(trip_label)) + 0.35),
        color = "#238b45",  # Green color for fishing
        linewidth = 0.7,
        alpha = 0.7
      )
  }
  
  p <- p +
    scale_x_datetime(
      date_breaks = "1 week",
      date_labels = "%b %d",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey95"),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10)
    ) +
    labs(
      x = "Date",
      y = NULL,
      title = "Detailed Trip Timeline",
      subtitle = sprintf("%d trips | Each row = one trip%s",
                         nrow(trips_plot),
                         if (!is.null(fishing_events)) " | Green = Fishing" else "")
    )
  
  message("Plot created successfully!")
  
  return(p)
}
# ============================================================================
