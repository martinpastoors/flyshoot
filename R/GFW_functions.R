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
    where_clause <- switch(search_by,
                           "mmsi" = sprintf("ssvid = '%s'", identifier),
                           "imo" = sprintf("imo = '%s'", identifier),
                           "name" = sprintf("shipname LIKE '%%%s%%'", identifier),
                           stop("search_by must be 'mmsi', 'imo', or 'name'")
    )
    
    if (!is.null(flag)) {
      where_clause <- sprintf("%s AND flag = '%s'", where_clause, flag)
    }
    
    vessel_info <- gfw_vessel_info(
      where = where_clause,
      search_type = "search",
      key = key
    )
    
    if (is.null(vessel_info) || nrow(vessel_info$selfReportedInfo) == 0) {
      return(NULL)
    }
    
    result <- vessel_info$selfReportedInfo %>%
      dplyr::select(vesselId, ssvid, flag, imo, 
             transmissionDateFrom, transmissionDateTo) %>%
      dplyr::mutate(search_identifier = identifier,
             search_by = search_by)
    
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
  # Drop vessel_name from lookup since vessel_name already exists in events
  combined <- combined %>%
    dplyr::left_join(
      vessel_lookup %>% 
        dplyr::select(vesselId, search_mmsi, ssvid, flag, imo),
      by = "vesselId"
    )
  
  # Data quality filters
  n_before <- nrow(combined)
  
  combined <- combined %>%
    dplyr::filter(
      # Must have vessel name
      !is.na(vessel_name),
      # Start date must be within requested range (with small buffer)
      start >= lubridate::ymd(start_date) - lubridate::days(1),
      start <= lubridate::ymd(end_date) + lubridate::days(1),
      # End date must be reasonable (within 1 year of start)
      end <= start + lubridate::days(365)
    )
  
  n_removed <- n_before - nrow(combined)
  
  if (n_removed > 0) {
    message(sprintf("Removed %d records with data quality issues:", n_removed))
    message("  - Missing vessel name")
    message("  - Dates outside requested range")
    message("  - Unrealistic event duration (>1 year)")
  }
  
  message(sprintf("\nTotal valid events: %s\n", format(nrow(combined), big.mark = ",")))
  
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
    dplyr::filter(event_type_requested == "PORT_VISIT") %>%
    dplyr::arrange(search_mmsi, vessel_name, start) %>%
    dplyr::select(search_mmsi, vesselId, vessel_name, start, end, lat, lon, 
           regions, eventId, event_type_requested)
  
  if (nrow(port_visits) == 0) {
    warning("No port visits found - cannot construct trips")
    return(list(trips = NULL, events_with_trips = NULL))
  }
  
  # Create trips by pairing consecutive port visits
  new_trips <- port_visits %>%
    dplyr::group_by(search_mmsi, vesselId, vessel_name) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(
      port_departure = end,
      port_departure_lat = lat,
      port_departure_lon = lon,
      port_departure_eventId = eventId,
      port_arrival = lead(start),
      port_arrival_lat = lead(lat),
      port_arrival_lon = lead(lon),
      port_arrival_eventId = lead(eventId)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(port_arrival)) %>%
    dplyr::mutate(
      trip_year = lubridate::year(port_departure),
      trip_duration_hours = as.numeric(difftime(port_arrival, port_departure, units = "hours"))
    ) %>%
    dplyr::select(search_mmsi, vesselId, vessel_name, trip_year,
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
    
    all_trips_temp <- dplyr::bind_rows(
      previous_trips_clean,
      new_trips %>%
        dplyr::mutate(trip_id = NA_character_, trip_number = NA_integer_) %>%
        dplyr::select(vessel_name, trip_year, port_departure, port_arrival, trip_id, trip_number)
    ) %>%
      dplyr::arrange(vessel_name, port_departure)
    
  } else {
    all_trips_temp <- new_trips %>%
      dplyr::mutate(trip_id = NA_character_, trip_number = NA_integer_)
  }
  
  # Assign trip numbers per vessel per year
  trips_numbered <- all_trips_temp %>%
    dplyr::group_by(vessel_name, trip_year) %>%
    dplyr::arrange(port_departure) %>%
    dplyr::mutate(
      trip_number = if_else(
        is.na(trip_number),
        row_number(),
        trip_number
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      trip_id = sprintf("%s_%d_%03d", 
                        gsub(" ", "_", vessel_name), 
                        trip_year, 
                        trip_number)
    )
  
  # Join back to get all columns for new trips
  trips_final <- new_trips %>%
    dplyr::left_join(
      trips_numbered %>%
        dplyr::select(vessel_name, trip_year, port_departure, trip_id, trip_number),
      by = c("vessel_name", "trip_year", "port_departure")
    ) %>%
    dplyr::select(trip_id, vessel_name, search_mmsi, vesselId, trip_year, trip_number,
           port_departure, port_departure_lat, port_departure_lon, port_departure_eventId,
           port_arrival, port_arrival_lat, port_arrival_lon, port_arrival_eventId,
           trip_duration_hours)
  
  message(sprintf("Assigned trip IDs to %d trips", nrow(trips_final)))
  
  # Assign events to trips
  other_events <- events %>%
    dplyr::filter(event_type_requested != "PORT_VISIT") %>%
    dplyr::select(search_mmsi, vesselId, vessel_name, event_type_requested, eventId, 
           start, end, lat, lon)
  
  events_with_trips <- NULL
  
  if (nrow(other_events) > 0) {
    events_with_trips <- other_events %>%
      dplyr::left_join(
        trips_final %>% 
          dplyr::select(trip_id, search_mmsi, vesselId, 
                 port_departure, port_arrival),
        by = c("search_mmsi", "vesselId"),
        relationship = "many-to-many"
      ) %>%
      dplyr::filter(
        start >= port_departure & start <= port_arrival
      ) %>%
      dplyr::select(-port_departure, -port_arrival) %>%
      rename(vessel_name = vessel_name)
    
    message(sprintf("Assigned %d events to trips", nrow(events_with_trips)))
    
    # Summarize events by trip
    trip_summary <- events_with_trips %>%
      dplyr::group_by(trip_id, event_type_requested) %>%
      dplyr::summarise(n_events = n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = event_type_requested,
        values_from = n_events,
        values_fill = 0,
        names_prefix = "n_"
      )
    
    # Add to trips
    trips_final <- trips_final %>%
      dplyr::left_join(trip_summary, by = "trip_id") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), ~replace_na(.x, 0)))
  }
  
  # Display summary
  message("\n=== Trip Summary ===")
  message(sprintf("Total trips: %d", nrow(trips_final)))
  message(sprintf("Vessels: %s", 
                  paste(unique(trips_final$vessel_name), collapse = ", ")))
  
  trip_count_by_vessel <- trips_final %>%
    dplyr::group_by(vessel_name, trip_year) %>%
    dplyr::summarise(n_trips = n(), .groups = "drop")
  
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
                                        save_csv = FALSE,  
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
    
    # Create weekly archives subdirectory
    weekly_dir <- file.path(output_dir, "weekly")
    if (!dir.exists(weekly_dir)) {
      dir.create(weekly_dir, recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Save timestamped files (weekly archives) in weekly/ subdirectory
    events_file <- file.path(weekly_dir, 
                             sprintf("gfw_events_%s.rds", timestamp))
    saveRDS(events, events_file)
    message(sprintf("Saved events archive: %s", events_file))
    
    if (save_csv) {
      events_csv_file <- file.path(weekly_dir, 
                                   sprintf("gfw_events_%s.csv", timestamp))
      events_flat <- flatten_for_csv(events)
      write.csv(events_flat, events_csv_file, row.names = FALSE)
      message(sprintf("Saved events CSV: %s", events_csv_file))
    }
    
    if (!is.null(trip_data$trips)) {
      trips_file <- file.path(weekly_dir, 
                              sprintf("gfw_trips_%s.rds", timestamp))
      saveRDS(trip_data$trips, trips_file)
      message(sprintf("Saved trips archive: %s", trips_file))
      
      # Keep trips_latest in main folder (used for continuous numbering)
      trips_latest <- file.path(output_dir, "gfw_trips_latest.rds")
      saveRDS(trip_data$trips, trips_latest)
      
      if (save_csv) {
        trips_csv_file <- file.path(weekly_dir, 
                                    sprintf("gfw_trips_%s.csv", timestamp))
        trips_flat <- flatten_for_csv(trip_data$trips)
        write.csv(trips_flat, trips_csv_file, row.names = FALSE)
        message(sprintf("Saved trips CSV: %s", trips_csv_file))
      }
    }
    
    if (!is.null(trip_data$events_with_trips)) {
      events_trips_file <- file.path(weekly_dir, 
                                     sprintf("gfw_events_by_trip_%s.rds", timestamp))
      saveRDS(trip_data$events_with_trips, events_trips_file)
      
      if (save_csv) {
        events_trips_csv_file <- file.path(weekly_dir, 
                                           sprintf("gfw_events_by_trip_%s.csv", timestamp))
        events_trips_flat <- flatten_for_csv(trip_data$events_with_trips)
        write.csv(events_trips_flat, events_trips_csv_file, row.names = FALSE)
        message(sprintf("Saved events_by_trip CSV: %s", events_trips_csv_file))
      }
    }
    
    # NEW: Update cumulative datasets
    if (update_cumulative) {
      cumulative_data <- update_cumulative_data(
        new_events = events,
        new_trips = trip_data$trips,
        new_events_with_trips = trip_data$events_with_trips,
        output_dir = output_dir,
        save_csv = save_csv
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
