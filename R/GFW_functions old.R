# ============================================================================
# GFW Data Extraction Functions
# ============================================================================
# Author: Martin Pastoors
# Purpose: Extract and process GFW event data with trip construction
# ============================================================================

library(gfwr)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)

# ============================================================================
# Function: Get vessel ID from identifier
# ============================================================================
get_vessel_id <- function(identifier, 
                          search_by = "mmsi",
                          flag = NULL,
                          key = gfw_auth()) {
  
  tryCatch({
    # Build WHERE clause - this determines HOW we search
    where_clause <- switch(search_by,
                           "mmsi" = sprintf("ssvid = '%s'", identifier),
                           "imo" = sprintf("imo = '%s'", identifier),
                           "name" = sprintf("shipname LIKE '%%%s%%'", identifier),  # shipname here is CORRECT - it's a GFW database field
                           stop("search_by must be 'mmsi', 'imo', or 'name'")
    )
    
    # Add flag filter if specified
    if (!is.null(flag)) {
      where_clause <- sprintf("%s AND flag = '%s'", where_clause, flag)
    }
    
    # Query GFW API with the WHERE clause
    vessel_info <- gfw_vessel_info(
      where = where_clause,  # e.g., "ssvid = '244682000' AND flag = 'NLD'"
      search_type = "search",
      key = key
    )
    
    if (is.null(vessel_info) || nrow(vessel_info$selfReportedInfo) == 0) {
      return(NULL)
    }
    
    # Extract only the fields we need - NO shipname in SELECT
    result <- vessel_info$selfReportedInfo %>%
      dplyr::select(
        vesselId,           # GFW's unique vessel ID
        ssvid,              # Self-reported MMSI
        flag,               # Vessel flag
        imo,                # IMO number
        transmissionDateFrom,  # First AIS transmission
        transmissionDateTo     # Last AIS transmission
        # NOTE: NO shipname - we'll use vessel_name from events instead
      ) %>%
      dplyr::mutate(
        search_identifier = identifier,
        search_by = search_by
      )
    
    return(result)
    
  }, error = function(e) {
    warning(sprintf("Error looking up %s %s: %s", search_by, identifier, e$message))
    return(NULL)
  })
}



# ============================================================================
# Function: Extract GFW events (batched)
# ============================================================================
extract_gfw_events_batched_strict <- function(vessel_identifiers,
                                              search_by = "mmsi",
                                              event_types = c("FISHING", "PORT_VISIT"),
                                              start_date,
                                              end_date,
                                              flag = NULL,
                                              use_current_identity_only = TRUE,
                                              batch_size = 50,
                                              additional_params = list(),
                                              key = gfw_auth()) {
  
  message("\n=== Batched GFW Events Extraction ===")
  message(sprintf("Date range: %s to %s", start_date, end_date))
  message(sprintf("Vessels: %d", length(vessel_identifiers)))
  message(sprintf("Event types: %s\n", paste(event_types, collapse = ", ")))
  
  # Get vessel IDs
  vessel_lookup <- vessel_identifiers %>%
    purrr::map_df(function(id) {
      result <- get_vessel_id(id, search_by = search_by, flag = flag, key = key)
      if (!is.null(result)) {
        result$search_mmsi <- id
      }
      return(result)
    })
  
  if (is.null(vessel_lookup) || nrow(vessel_lookup) == 0) {
    stop("No vessels found.")
  }
  
  # Filter to exact matches if requested
  if (use_current_identity_only && search_by == "mmsi") {
    vessel_lookup <- vessel_lookup %>%
      dplyr::filter(ssvid == search_mmsi)
  }
  
  all_vessel_ids <- unique(vessel_lookup$vesselId)
  message(sprintf("Using %d unique vesselIds\n", length(all_vessel_ids)))
  
  # Batch the vessels
  n_batches <- ceiling(length(all_vessel_ids) / batch_size)
  vessel_batches <- split(all_vessel_ids, 
                          ceiling(seq_along(all_vessel_ids) / batch_size))
  
  # Extract events by type
  all_events <- list()
  
  for (event_type in event_types) {
    message(sprintf("Extracting %s events...", event_type))
    
    extra_params <- if (event_type %in% names(additional_params)) {
      additional_params[[event_type]]
    } else {
      list()
    }
    
    batch_results <- purrr::map(vessel_batches, function(batch) {
      
      params <- list(
        event_type = event_type,
        vessels = batch,
        start_date = start_date,
        end_date = end_date,
        key = key
      )
      
      params <- c(params, extra_params)
      
      message(sprintf("  Batch: %d vessels, event_type=%s, dates=%s to %s", 
                      length(batch), event_type, start_date, end_date))
      
      events <- tryCatch({
        result <- do.call(gfw_event, params)
        
        if (!is.null(result) && nrow(result) > 0) {
          message(sprintf("    Retrieved %d events", nrow(result)))
        } else {
          message(sprintf("    No events found"))
        }
        
        result
        
      }, error = function(e) {
        message(sprintf("    ERROR in batch: %s", e$message))
        message(sprintf("    Vessels in batch: %s", paste(head(batch, 3), collapse = ", ")))
        message("    Trying vessels individually...")
        
        individual_results <- purrr::map(batch, function(single_vessel) {
          tryCatch({
            single_params <- params
            single_params$vessels <- single_vessel
            single_result <- do.call(gfw_event, single_params)
            
            if (!is.null(single_result) && nrow(single_result) > 0) {
              message(sprintf("      Vessel %s: %d events", single_vessel, nrow(single_result)))
            }
            
            return(single_result)
            
          }, error = function(e2) {
            message(sprintf("      Vessel %s: ERROR - %s", single_vessel, e2$message))
            return(NULL)
          })
        })
        
        combined <- dplyr::bind_rows(individual_results)
        return(combined)
      })
      
      return(events)
    })
    
    event_data <- dplyr::bind_rows(batch_results)
    
    if (!is.null(event_data) && nrow(event_data) > 0) {
      event_data$event_type_requested <- event_type
      all_events[[length(all_events) + 1]] <- event_data
      message(sprintf("  Total %s events: %s", 
                      event_type,
                      format(nrow(event_data), big.mark = ",")))
    } else {
      message(sprintf("  No %s events retrieved", event_type))
    }
  }
  
  if (length(all_events) == 0) {
    message("\n=== No events found ===")
    return(NULL)
  }
  
  combined <- dplyr::bind_rows(all_events)
  
  # UPDATED: Add vessel identifier information with better naming
  # Select only the fields we need from vessel_lookup
  # Drop shipname from lookup since vessel_name already exists in events
  combined <- combined %>%
    dplyr::left_join(
      vessel_lookup %>% 
        dplyr::select(vesselId, search_mmsi, ssvid, flag, imo),
      by = "vesselId"
    )
  
  # OPTIONAL: Rename vessel_name to vessel_name_gfw for clarity
  # if ("vessel_name" %in% names(combined)) {
  #   combined <- combined %>%
  #     dplyr::rename(vessel_name_gfw = vessel_name)
  # }
  
  message(sprintf("\nTotal events: %s\n", format(nrow(combined), big.mark = ",")))
  
  return(combined)
}


# ============================================================================
# Function: Construct trips with persistent numbering
# ============================================================================
construct_trips_with_numbering <- function(events, 
                                           previous_trips = NULL,
                                           output_dir = NULL) {
  
  message("\n=== Constructing Trips ===")
  
  # Load previous trips if directory specified and file exists
  if (!is.null(output_dir) && is.null(previous_trips)) {
    previous_file <- file.path(output_dir, "gfw_trips_latest.rds")
    if (file.exists(previous_file)) {
      message("Loading previous trips for continuous numbering...")
      previous_trips <- readRDS(previous_file)
      message(sprintf("  Loaded %d previous trips", nrow(previous_trips)))
    }
  }
  
  # Extract port visits
  port_visits <- events %>%
    filter(event_type_requested == "PORT_VISIT") %>%
    arrange(search_mmsi, shipname, start) %>%
    dplyr::select(search_mmsi, vesselId, shipname, start, end, lat, lon, 
           regions, eventId, event_type_requested)
  
  if (nrow(port_visits) == 0) {
    warning("No port visits found - cannot construct trips")
    return(list(trips = NULL, events_with_trips = NULL))
  }
  
  # Create trips by pairing consecutive port visits
  new_trips <- port_visits %>%
    group_by(search_mmsi, vesselId, shipname) %>%
    arrange(start) %>%
    mutate(
      port_departure = end,
      port_departure_lat = lat,
      port_departure_lon = lon,
      port_departure_eventId = eventId,
      port_arrival = lead(start),
      port_arrival_lat = lead(lat),
      port_arrival_lon = lead(lon),
      port_arrival_eventId = lead(eventId)
    ) %>%
    ungroup() %>%
    filter(!is.na(port_arrival)) %>%
    mutate(
      trip_year = year(port_departure),
      trip_duration_hours = as.numeric(difftime(port_arrival, port_departure, units = "hours"))
    ) %>%
    dplyr::select(search_mmsi, vesselId, shipname, trip_year,
           port_departure, port_departure_lat, port_departure_lon, port_departure_eventId,
           port_arrival, port_arrival_lat, port_arrival_lon, port_arrival_eventId,
           trip_duration_hours)
  
  message(sprintf("Found %d new trip segments", nrow(new_trips)))
  
  # Combine with previous trips if available
  if (!is.null(previous_trips)) {
    # Keep only relevant columns from previous trips
    previous_trips_clean <- previous_trips %>%
      dplyr::select(trip_id, vessel_name, trip_year, trip_number,
             port_departure, port_arrival)
    
    all_trips_temp <- bind_rows(
      previous_trips_clean %>%
        rename(shipname = vessel_name),
      new_trips %>%
        mutate(trip_id = NA_character_, trip_number = NA_integer_) %>%
        select(shipname, trip_year, port_departure, port_arrival, trip_id, trip_number)
    ) %>%
      arrange(shipname, port_departure)
    
  } else {
    all_trips_temp <- new_trips %>%
      mutate(trip_id = NA_character_, trip_number = NA_integer_)
  }
  
  # Assign trip numbers per vessel per year
  trips_numbered <- all_trips_temp %>%
    group_by(shipname, trip_year) %>%
    arrange(port_departure) %>%
    mutate(
      trip_number = if_else(
        is.na(trip_number),
        row_number(),
        trip_number
      )
    ) %>%
    ungroup() %>%
    mutate(
      trip_id = sprintf("%s_%d_%03d", 
                        gsub(" ", "_", shipname), 
                        trip_year, 
                        trip_number)
    )
  
  # Join back to get all columns for new trips
  trips_final <- new_trips %>%
    left_join(
      trips_numbered %>%
        select(shipname, trip_year, port_departure, trip_id, trip_number),
      by = c("shipname", "trip_year", "port_departure")
    ) %>%
    rename(vessel_name = shipname) %>%
    select(trip_id, vessel_name, search_mmsi, vesselId, trip_year, trip_number,
           port_departure, port_departure_lat, port_departure_lon, port_departure_eventId,
           port_arrival, port_arrival_lat, port_arrival_lon, port_arrival_eventId,
           trip_duration_hours)
  
  message(sprintf("Assigned trip IDs to %d trips", nrow(trips_final)))
  
  # Assign events to trips
  other_events <- events %>%
    filter(event_type_requested != "PORT_VISIT") %>%
    select(search_mmsi, vesselId, shipname, event_type_requested, eventId, 
           start, end, lat, lon)
  
  events_with_trips <- NULL
  
  if (nrow(other_events) > 0) {
    events_with_trips <- other_events %>%
      left_join(
        trips_final %>% 
          select(trip_id, vessel_name, search_mmsi, vesselId, 
                 port_departure, port_arrival),
        by = c("search_mmsi", "vesselId"),
        relationship = "many-to-many"
      ) %>%
      filter(
        start >= port_departure & start <= port_arrival
      ) %>%
      select(-port_departure, -port_arrival) %>%
      rename(vessel_name = vessel_name)
    
    message(sprintf("Assigned %d events to trips", nrow(events_with_trips)))
    
    # Summarize events by trip
    trip_summary <- events_with_trips %>%
      group_by(trip_id, event_type_requested) %>%
      summarise(n_events = n(), .groups = "drop") %>%
      pivot_wider(
        names_from = event_type_requested,
        values_from = n_events,
        values_fill = 0,
        names_prefix = "n_"
      )
    
    # Add to trips
    trips_final <- trips_final %>%
      left_join(trip_summary, by = "trip_id") %>%
      mutate(across(starts_with("n_"), ~replace_na(.x, 0)))
  }
  
  # Display summary
  message("\n=== Trip Summary ===")
  message(sprintf("Total trips: %d", nrow(trips_final)))
  message(sprintf("Vessels: %s", 
                  paste(unique(trips_final$vessel_name), collapse = ", ")))
  
  trip_count_by_vessel <- trips_final %>%
    group_by(vessel_name, trip_year) %>%
    summarise(n_trips = n(), .groups = "drop")
  
  print(trip_count_by_vessel)
  
  message("====================\n")
  
  return(list(
    trips = trips_final,
    events_with_trips = events_with_trips
  ))
}

# ============================================================================
# Function: Main extraction with integrated trip construction (UPDATED)
# ============================================================================
extract_gfw_data_with_trips <- function(vessel_identifiers,
                                        event_types = c("FISHING", "PORT_VISIT", "ENCOUNTER", "GAP"),
                                        start_date,
                                        end_date,
                                        flag = NULL,
                                        output_dir,
                                        save_results = TRUE,
                                        update_cumulative = TRUE,  
                                        batch_size = 50,  
                                        key = gfw_auth()) {
  
  message(sprintf("\n========== GFW Data Extraction =========="))
  message(sprintf("Start time: %s", Sys.time()))
  message(sprintf("Date range: %s to %s", start_date, end_date))
  message("=========================================\n")
  
  # Step 1: Extract events
  events <- extract_gfw_events_batched_strict(
    vessel_identifiers = vessel_identifiers,
    search_by = "mmsi",
    event_types = event_types,
    start_date = start_date,
    end_date = end_date,
    flag = flag,
    use_current_identity_only = TRUE,
    batch_size = batch_size,
    additional_params = list(
      PORT_VISIT = list(confidences = c(3, 4))
    ),
    key = key
  )
  
  if (is.null(events)) {
    message("No events extracted - stopping")
    return(NULL)
  }
  
  # Step 2: Construct trips
  trip_data <- construct_trips_with_numbering(
    events = events,
    previous_trips = NULL,
    output_dir = output_dir
  )
  
  # Step 3: Save results if requested
  if (save_results) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Save timestamped files (individual run archives)
    events_file <- file.path(output_dir, 
                             sprintf("gfw_events_%s.rds", timestamp))
    saveRDS(events, events_file)
    message(sprintf("Saved events: %s", events_file))
    
    events_csv_file <- file.path(output_dir, 
                                 sprintf("gfw_events_%s.csv", timestamp))
    events_flat <- flatten_for_csv(events)
    write.csv(events_flat, events_csv_file, row.names = FALSE)
    
    if (!is.null(trip_data$trips)) {
      trips_file <- file.path(output_dir, 
                              sprintf("gfw_trips_%s.rds", timestamp))
      saveRDS(trip_data$trips, trips_file)
      message(sprintf("Saved trips: %s", trips_file))
      
      trips_latest <- file.path(output_dir, "gfw_trips_latest.rds")
      saveRDS(trip_data$trips, trips_latest)
      
      trips_csv_file <- file.path(output_dir, 
                                  sprintf("gfw_trips_%s.csv", timestamp))
      trips_flat <- flatten_for_csv(trip_data$trips)
      write.csv(trips_flat, trips_csv_file, row.names = FALSE)
    }
    
    if (!is.null(trip_data$events_with_trips)) {
      events_trips_file <- file.path(output_dir, 
                                     sprintf("gfw_events_by_trip_%s.rds", timestamp))
      saveRDS(trip_data$events_with_trips, events_trips_file)
      
      events_trips_csv_file <- file.path(output_dir, 
                                         sprintf("gfw_events_by_trip_%s.csv", timestamp))
      events_trips_flat <- flatten_for_csv(trip_data$events_with_trips)
      write.csv(events_trips_flat, events_trips_csv_file, row.names = FALSE)
    }
    
    # NEW: Update cumulative datasets
    if (update_cumulative) {
      cumulative_data <- update_cumulative_data(
        new_events = events,
        new_trips = trip_data$trips,
        new_events_with_trips = trip_data$events_with_trips,
        output_dir = output_dir
      )
    }
  }
  
  message(sprintf("\n========== Extraction Complete =========="))
  message(sprintf("End time: %s", Sys.time()))
  message("=========================================\n")
  
  return(list(
    events = events,
    trips = trip_data$trips,
    events_with_trips = trip_data$events_with_trips,
    cumulative = if (exists("cumulative_data")) cumulative_data else NULL
  ))
}



# ============================================================================
# Function: Flatten data for CSV export
# ============================================================================
flatten_for_csv <- function(data) {
  # Identify list columns
  list_cols <- names(data)[sapply(data, is.list)]
  
  if (length(list_cols) > 0) {
    message(sprintf("  Converting %d list columns for CSV export", length(list_cols)))
    
    # Convert list columns to JSON strings or remove them
    data_flat <- data %>%
      mutate(across(
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
                                   output_dir) {
  
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
    
    # Also save as CSV
    cumulative_events_csv <- file.path(output_dir, "gfw_events_cumulative.csv")
    all_events_flat <- flatten_for_csv(all_events)
    write.csv(all_events_flat, cumulative_events_csv, row.names = FALSE)
    
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
    
    # Also save as CSV
    cumulative_trips_csv <- file.path(output_dir, "gfw_trips_cumulative.csv")
    all_trips_flat <- flatten_for_csv(all_trips)
    write.csv(all_trips_flat, cumulative_trips_csv, row.names = FALSE)
    
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
    
    # Also save as CSV
    cumulative_events_trips_csv <- file.path(output_dir, "gfw_events_by_trip_cumulative.csv")
    all_events_trips_flat <- flatten_for_csv(all_events_trips)
    write.csv(all_events_trips_flat, cumulative_events_trips_csv, row.names = FALSE)
    
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
    filter(
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
    filter(
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
        filter(
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
    filter(
      !is.na(port_departure_lon),
      !is.na(port_departure_lat),
      !is.na(port_arrival_lon),
      !is.na(port_arrival_lat)
    ) %>%
    mutate(
      month_label = format(floor_date(port_departure, "month"), "%Y-%m")
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
    filter(
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
        filter(
          !is.na(trip_id), 
          !is.na(lat), 
          !is.na(lon),
          lat >= ylim[1] & lat <= ylim[2],
          lon >= xlim[1] & lon <= xlim[2]
        ) %>%
        mutate(month_label = format(floor_date(start, "month"), "%Y-%m"))
      
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
    filter(vessel_name == !!vessel_name) %>%
    filter(
      !is.na(port_departure_lon),
      !is.na(port_departure_lat),
      !is.na(port_arrival_lon),
      !is.na(port_arrival_lat)
    ) %>%
    mutate(month_label = format(floor_date(port_departure, "month"), "%Y-%m"))
  
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
    filter(
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
      filter(
        trip_id %in% vessel_trips$trip_id,
        !is.na(lat), 
        !is.na(lon),
        lat >= ylim[1] & lat <= ylim[2],
        lon >= xlim[1] & lon <= xlim[2]
      ) %>%
      mutate(month_label = format(floor_date(start, "month"), "%Y-%m"))
    
    if (nrow(vessel_events) > 0) {
      p <- p + 
        geom_point(data = vessel_events,
                   aes(x = lon, y = lat, color = trip_id),
                   size = 1.5, shape = 4, alpha = 0.6)
      
      message(sprintf("Added %d events", nrow(vessel_events)))
    }
  }
  
  return(p)
}