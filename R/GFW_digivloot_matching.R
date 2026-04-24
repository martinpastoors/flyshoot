# ============================================================================
# DIGIvloot <-> GFW Trip Matching Functions
# ============================================================================
# Purpose: Link DIGIvloot trip data with GFW extracted trips
# Author: Martin Pastoors
# ============================================================================
# 
# Functions for:
# - Matching trips between DIGIvloot and GFW datasets
# - Diagnostic tools for troubleshooting matches
# - Comparison visualizations
# 
# Column naming convention:
# - vessel_name = GFW vessel names (e.g., "SCH-99 ARAVIS")
# - vessel = DIGIvloot vessel codes (e.g., "SCH99")
# - trip_id = GFW trip IDs (e.g., "SCH-99_ARAVIS_2026_001")
# - trip = DIGIvloot trip IDs (e.g., "2026272")
# 
# ============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggalt)

# ============================================================================
# DIGIvloot <-> GFW Trip Matching Functions
# ============================================================================
# Author: Martin Pastoors
# Purpose: Link DIGIvloot trip data with GFW extracted trips
# ============================================================================

library(dplyr)
library(lubridate)

# ============================================================================
# Function 1: Create vessel name mapping
# ============================================================================
create_vessel_mapping <- function() {
  # Map DIGIvloot vessel codes to GFW vessel names
  # Update this table with all your vessels
  
  mapping <- tibble::tribble(
    ~vessel,          ~vessel_name,
    "SCH99",          "SCH-99 ARAVIS",
    "SCH135",         "SCH-135 GALIBIER",
    "SCH65",          "SCH65 SIMPLON",
    "SCH144",         "SCH-144 VERTROUWEN",
    "SL9",            "SL-9 JOHANNA"
  )
  
  return(mapping)
}

# ============================================================================
# Function 2: Extract trip summary from DIGIvloot data
# ============================================================================
extract_digivloot_trip_summary <- function(digivloot_data) {
  
  # Extract departure and arrival events
  trip_summary <- digivloot_data %>%
    dplyr::filter(action %in% c("departure", "arrival")) %>%
    dplyr::mutate(
      # date = lubridate::ymd(date)
      date = as.Date(date)
    ) %>%
    dplyr::select(vessel, trip, action, date, port, lat, lon) %>%
    tidyr::pivot_wider(
      id_cols = c(vessel, trip),
      names_from = action,
      values_from = c(date, port, lat, lon),
      names_sep = "_"
    ) %>%
    dplyr::rename(
      # vessel = vessel,
      # trip = trip,
      departure_date = date_departure,
      arrival_date = date_arrival,
      departure_port = port_departure,
      arrival_port = port_arrival,
      departure_lat = lat_departure,
      departure_lon = lon_departure,
      arrival_lat = lat_arrival,
      arrival_lon = lon_arrival
    ) %>%
    dplyr::mutate(
      # Calculate trip duration
      trip_duration_days = as.numeric(difftime(arrival_date, departure_date, units = "days"))
    )
  
  return(trip_summary)
}

# ============================================================================
# Function 3: Match DIGIvloot trips with GFW trips
# ============================================================================

# digivloot_data = trip
# vessel_mapping = create_vessel_mapping()
# time_tolerance_hours = 24

match_digivloot_gfw_trips <- function(gfw_trips, 
                                      digivloot_data,
                                      vessel_mapping = NULL,
                                      time_tolerance_hours = 24) {
  
  message("\n=== Matching DIGIvloot and GFW Trips ===")
  
  # Create vessel mapping if not provided
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # Extract DIGIvloot trip summaries
  dv_trips <- extract_digivloot_trip_summary(digivloot_data)
  
  message(sprintf("DIGIvloot trips: %d", nrow(dv_trips)))
  message(sprintf("GFW trips: %d", nrow(gfw_trips)))
  
  # Add vessel name mapping to DIGIvloot trips
  dv_trips <- dv_trips %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    )
  
  # Check for unmapped vessels
  unmapped <- dv_trips %>%
    dplyr::filter(is.na(vessel_name)) %>%
    dplyr::pull(vessel) %>%
    unique()
  
  if (length(unmapped) > 0) {
    warning(sprintf("Unmapped vessels in DIGIvloot data: %s", 
                    paste(unmapped, collapse = ", ")))
  }
  
  # Match trips by vessel and time
  matched <- gfw_trips %>%
    dplyr::left_join(
      dv_trips,
      by = c("vessel_name" = "vessel_name"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      # Calculate time differences (in hours)
      departure_diff_hours = abs(as.numeric(difftime(
        port_departure, 
        departure_date, 
        units = "hours"
      ))),
      arrival_diff_hours = abs(as.numeric(difftime(
        port_arrival, 
        arrival_date, 
        units = "hours"
      ))),
      # Total time difference
      total_diff_hours = departure_diff_hours + arrival_diff_hours
    ) %>%
    # Filter to matches within tolerance
    dplyr::filter(
      departure_diff_hours <= time_tolerance_hours,
      arrival_diff_hours <= time_tolerance_hours
    ) %>%
    # For each GFW trip, pick the best DIGIvloot match
    dplyr::group_by(trip_id) %>%
    dplyr::slice_min(total_diff_hours, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  message(sprintf("Matched trips: %d", nrow(matched)))
  message(sprintf("GFW trips without DIGIvloot match: %d", 
                  nrow(gfw_trips) - nrow(matched)))
  
  # Summary statistics
  if (nrow(matched) > 0) {
    message(sprintf("Average time difference - Departure: %.1f hours, Arrival: %.1f hours",
                    mean(matched$departure_diff_hours, na.rm = TRUE),
                    mean(matched$arrival_diff_hours, na.rm = TRUE)))
  }
  
  return(matched)
}

# ============================================================================
# Function 4: Add DIGIvloot trip IDs to GFW trips
# ============================================================================
add_digivloot_ids <- function(gfw_trips, 
                              digivloot_data,
                              vessel_mapping = NULL,
                              time_tolerance_hours = 24) {
  
  # Match trips
  matched <- match_digivloot_gfw_trips(
    gfw_trips = gfw_trips,
    digivloot_data = digivloot_data,
    vessel_mapping = vessel_mapping,
    time_tolerance_hours = time_tolerance_hours
  )
  
  # Create lookup table
  trip_lookup <- matched %>%
    dplyr::select(
      trip_id,
      trip,
      vessel,
      departure_diff_hours,
      arrival_diff_hours
    )
  
  # Add to original GFW trips
  gfw_with_dv <- gfw_trips %>%
    dplyr::left_join(trip_lookup, by = "trip_id")
  
  return(gfw_with_dv)
}

# ============================================================================
# Function 5: Create cross-reference table
# ============================================================================
create_trip_crossref <- function(gfw_trips, 
                                 digivloot_data,
                                 vessel_mapping = NULL,
                                 time_tolerance_hours = 24) {
  
  matched <- match_digivloot_gfw_trips(
    gfw_trips = gfw_trips,
    digivloot_data = digivloot_data,
    vessel_mapping = vessel_mapping,
    time_tolerance_hours = time_tolerance_hours
  )
  
  crossref <- matched %>%
    dplyr::select(
      # GFW identifiers
      trip_id = trip_id,
      gfw_vessel = vessel_name,
      gfw_departure = port_departure,
      gfw_arrival = port_arrival,
      gfw_duration_hours = trip_duration_hours,
      
      # DIGIvloot identifiers
      trip,
      vessel,
      dv_departure = departure_date,
      dv_arrival = arrival_date,
      dv_duration_days = trip_duration_days,
      
      # Match quality
      departure_diff_hours,
      arrival_diff_hours,
      total_diff_hours
    ) %>%
    dplyr::arrange(gfw_vessel, gfw_departure)
  
  return(crossref)
}

# ============================================================================
# Function 6: Link DIGIvloot hauls to GFW fishing events
# ============================================================================
link_hauls_to_fishing_events <- function(gfw_events,
                                         digivloot_data,
                                         trip_crossref) {
  
  # Get hauls only (where action is NA and haul > 0)
  hauls <- digivloot_data %>%
    dplyr::filter(is.na(action), haul > 0) %>%
    dplyr::mutate(
      date = lubridate::ymd(date)
    ) %>%
    dplyr::rename(
      # trip = trip,
      # vessel = vessel,
      haul_number = haul,
      haul_date = date,
      haul_lat = lat,
      haul_lon = lon
    )
  
  # Add GFW trip_id to hauls via crossref
  hauls_with_trip <- hauls %>%
    dplyr::left_join(
      trip_crossref %>% dplyr::select(trip_id, trip),
      by = "trip"
    )
  
  # Match with GFW fishing events by trip and position/time
  linked <- gfw_events %>%
    dplyr::filter(event_type_requested == "FISHING") %>%
    dplyr::left_join(
      hauls_with_trip,
      by = c("trip_id" = "trip_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      # Calculate spatial distance (rough approximation)
      distance_km = sqrt(
        ((lat - haul_lat) * 111)^2 + 
        ((lon - haul_lon) * 111 * cos(lat * pi/180))^2
      ),
      # Time difference
      time_diff_hours = abs(as.numeric(difftime(start, haul_date, units = "hours")))
    ) %>%
    # Filter to reasonable matches (within 5 km and 6 hours)
    dplyr::filter(
      distance_km <= 5,
      time_diff_hours <= 6
    ) %>%
    # Pick best match for each GFW fishing event
    dplyr::group_by(eventId) %>%
    dplyr::slice_min(distance_km + time_diff_hours, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  return(linked)
}

# ============================================================================
# Function 7: Generate matching report
# ============================================================================
generate_matching_report <- function(gfw_trips, 
                                     digivloot_data,
                                     vessel_mapping = NULL) {
  
  crossref <- create_trip_crossref(
    gfw_trips = gfw_trips,
    digivloot_data = digivloot_data,
    vessel_mapping = vessel_mapping
  )
  
  cat("\n========================================\n")
  cat("DIGIvloot <-> GFW Trip Matching Report\n")
  cat("========================================\n\n")
  
  cat("Overall Statistics:\n")
  cat(sprintf("  Total GFW trips: %d\n", nrow(gfw_trips)))
  cat(sprintf("  Matched with DIGIvloot: %d (%.1f%%)\n", 
              nrow(crossref),
              100 * nrow(crossref) / nrow(gfw_trips)))
  cat(sprintf("  Unmatched GFW trips: %d\n\n", 
              nrow(gfw_trips) - nrow(crossref)))
  
  cat("Match Quality:\n")
  cat(sprintf("  Average departure time difference: %.1f hours\n",
              mean(crossref$departure_diff_hours, na.rm = TRUE)))
  cat(sprintf("  Average arrival time difference: %.1f hours\n",
              mean(crossref$arrival_diff_hours, na.rm = TRUE)))
  cat(sprintf("  Max departure difference: %.1f hours\n",
              max(crossref$departure_diff_hours, na.rm = TRUE)))
  cat(sprintf("  Max arrival difference: %.1f hours\n\n",
              max(crossref$arrival_diff_hours, na.rm = TRUE)))
  
  cat("By Vessel:\n")
  vessel_summary <- crossref %>%
    dplyr::group_by(gfw_vessel) %>%
    dplyr::summarise(
      n_matched = n(),
      avg_dept_diff = mean(departure_diff_hours, na.rm = TRUE),
      avg_arr_diff = mean(arrival_diff_hours, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(vessel_summary)
  
  cat("\n========================================\n\n")
  
  return(crossref)
}




# ============================================================================
# Diagnostic Functions for DIGIvloot-GFW Matching
# ============================================================================
# Use these to debug why trips aren't matching
# ============================================================================

library(dplyr)
library(lubridate)

# ============================================================================
# Function 1: Identify unmatched trips
# ============================================================================
diagnose_unmatched_trips <- function(gfw_trips, 
                                     digivloot_data,
                                     vessel_mapping = NULL,
                                     time_tolerance_hours = 24) {
  
  cat("\n========================================\n")
  cat("DIAGNOSTIC: Unmatched Trips Analysis\n")
  cat("========================================\n\n")
  
  # Get matched trips
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  matched <- match_digivloot_gfw_trips(
    gfw_trips = gfw_trips,
    digivloot_data = digivloot_data,
    vessel_mapping = vessel_mapping,
    time_tolerance_hours = time_tolerance_hours
  )
  
  # Find unmatched GFW trips
  unmatched_gfw <- gfw_trips %>%
    dplyr::filter(!trip_id %in% matched$trip_id) %>%
    dplyr::select(trip_id, vessel_name, port_departure, port_arrival, 
                  trip_duration_hours) %>%
    dplyr::arrange(vessel_name, port_departure)
  
  # Extract DIGIvloot trip summary
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    )
  
  # Find unmatched DIGIvloot trips
  unmatched_dv <- dv_summary %>%
    dplyr::filter(!trip %in% matched$trip) %>%
    dplyr::select(trip, vessel, vessel_name,
                  departure_date, arrival_date, trip_duration_days)
  
  # Print summary
  cat("SUMMARY:\n")
  cat(sprintf("  Total GFW trips: %d\n", nrow(gfw_trips)))
  cat(sprintf("  Total DIGIvloot trips: %d\n", nrow(dv_summary)))
  cat(sprintf("  Matched: %d\n", nrow(matched)))
  cat(sprintf("  Unmatched GFW trips: %d\n", nrow(unmatched_gfw)))
  cat(sprintf("  Unmatched DIGIvloot trips: %d\n\n", nrow(unmatched_dv)))
  
  # Show unmatched by vessel
  cat("UNMATCHED BY VESSEL:\n")
  unmatched_by_vessel <- unmatched_gfw %>%
    dplyr::group_by(vessel_name) %>%
    dplyr::summarise(n_unmatched = n(), .groups = "drop")
  print(unmatched_by_vessel)
  
  cat("\n")
  
  # Return detailed info
  return(list(
    unmatched_gfw = unmatched_gfw,
    unmatched_dv = unmatched_dv,
    matched = matched,
    summary = list(
      total_gfw = nrow(gfw_trips),
      total_dv = nrow(dv_summary),
      matched = nrow(matched),
      unmatched_gfw = nrow(unmatched_gfw),
      unmatched_dv = nrow(unmatched_dv)
    )
  ))
}

# ============================================================================
# Function 2: Check near-misses (trips that almost matched)
# ============================================================================
find_near_misses <- function(gfw_trips, 
                             digivloot_data,
                             vessel_mapping = NULL,
                             max_time_diff_hours = 72) {
  
  cat("\n========================================\n")
  cat("DIAGNOSTIC: Near-Miss Analysis\n")
  cat("========================================\n\n")
  cat(sprintf("Looking for trips within %d hours...\n\n", max_time_diff_hours))
  
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # Extract DIGIvloot summary
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    )
  
  # Join and calculate differences
  near_misses <- gfw_trips %>%
    dplyr::left_join(
      dv_summary,
      by = c("vessel_name" = "vessel_name"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      departure_diff_hours = abs(as.numeric(difftime(
        port_departure, departure_date, units = "hours"
      ))),
      arrival_diff_hours = abs(as.numeric(difftime(
        port_arrival, arrival_date, units = "hours"
      ))),
      total_diff_hours = departure_diff_hours + arrival_diff_hours
    ) %>%
    dplyr::filter(
      total_diff_hours <= max_time_diff_hours * 2  # Either departure or arrival within range
    ) %>%
    dplyr::arrange(vessel_name, total_diff_hours) %>%
    dplyr::select(
      trip_id, trip, vessel_name,
      port_departure, departure_date, departure_diff_hours,
      port_arrival, arrival_date, arrival_diff_hours,
      total_diff_hours
    )
  
  cat(sprintf("Found %d potential near-misses\n\n", nrow(near_misses)))
  
  # Categorize near-misses
  near_misses <- near_misses %>%
    dplyr::mutate(
      match_category = dplyr::case_when(
        departure_diff_hours <= 24 & arrival_diff_hours <= 24 ~ "Good match (should work)",
        departure_diff_hours <= 48 & arrival_diff_hours <= 48 ~ "Close (increase tolerance)",
        departure_diff_hours <= 72 | arrival_diff_hours <= 72 ~ "Marginal (verify manually)",
        TRUE ~ "Unlikely match"
      )
    )
  
  # Summary by category
  cat("NEAR-MISS CATEGORIES:\n")
  category_summary <- near_misses %>%
    dplyr::group_by(match_category) %>%
    dplyr::summarise(n = n(), .groups = "drop")
  print(category_summary)
  cat("\n")
  
  return(near_misses)
}

# ============================================================================
# Function 3: Compare date coverage
# ============================================================================
compare_date_coverage <- function(gfw_trips, 
                                  digivloot_data,
                                  vessel_mapping = NULL) {
  
  cat("\n========================================\n")
  cat("DIAGNOSTIC: Date Coverage Comparison\n")
  cat("========================================\n\n")
  
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # GFW date range by vessel
  gfw_coverage <- gfw_trips %>%
    dplyr::group_by(vessel_name) %>%
    dplyr::summarise(
      n_trips = n(),
      first_departure = min(port_departure, na.rm = TRUE),
      last_departure = min(port_arrival, na.rm = TRUE),
      .groups = "drop"
    )
  
  # DIGIvloot date range by vessel
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    )
  
  dv_coverage <- dv_summary %>%
    dplyr::group_by(vessel_name) %>%
    dplyr::summarise(
      n_trips = n(),
      first_departure = min(departure_date, na.rm = TRUE),
      last_departure = max(arrival_date, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Compare
  coverage_comparison <- dplyr::full_join(
    gfw_coverage %>% dplyr::rename(
      gfw_trips = n_trips,
      gfw_first = first_departure,
      gfw_last = last_departure
    ),
    dv_coverage %>% dplyr::rename(
      vessel_name = vessel_name,
      dv_trips = n_trips,
      dv_first = first_departure,
      dv_last = last_departure
    ),
    by = "vessel_name"
  ) %>%
    dplyr::mutate(
      trip_difference = gfw_trips - dv_trips,
      coverage_overlap = !is.na(gfw_first) & !is.na(dv_first)
    )
  
  cat("DATE COVERAGE BY VESSEL:\n")
  print(coverage_comparison)
  cat("\n")
  
  # Check for gaps
  cat("POTENTIAL ISSUES:\n")
  issues <- coverage_comparison %>%
    dplyr::filter(
      is.na(gfw_trips) | is.na(dv_trips) | abs(trip_difference) > 2
    )
  
  if (nrow(issues) > 0) {
    cat("⚠ Vessels with significant trip count differences:\n")
    print(issues)
  } else {
    cat("✓ All vessels have similar trip counts\n")
  }
  
  cat("\n")
  
  return(coverage_comparison)
}

# ============================================================================
# Function 4: Inspect specific unmatched trip
# ============================================================================
inspect_unmatched_trip <- function(trip_id, 
                                   gfw_trips,
                                   digivloot_data,
                                   vessel_mapping = NULL) {
  
  cat("\n========================================\n")
  cat("DIAGNOSTIC: Inspect Specific Trip\n")
  cat("========================================\n\n")
  
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # Get GFW trip details
  gfw_trip <- gfw_trips %>%
    dplyr::filter(trip_id == trip_id)
  
  if (nrow(gfw_trip) == 0) {
    cat(sprintf("✗ GFW trip '%s' not found\n", trip_id))
    return(NULL)
  }
  
  cat("GFW TRIP DETAILS:\n")
  cat(sprintf("  Trip ID: %s\n", gfw_trip$trip_id))
  cat(sprintf("  Vessel: %s\n", gfw_trip$vessel_name))
  cat(sprintf("  Departure: %s\n", gfw_trip$port_departure))
  cat(sprintf("  Arrival: %s\n", gfw_trip$port_arrival))
  cat(sprintf("  Duration: %.1f hours\n\n", gfw_trip$trip_duration_hours))
  
  # Find vessel code in DIGIvloot
  vessel_code <- vessel_mapping %>%
    dplyr::filter(vessel_name == gfw_trip$vessel_name) %>%
    dplyr::pull(vessel)
  
  if (length(vessel_code) == 0) {
    cat("✗ PROBLEM: Vessel not found in mapping!\n")
    cat(sprintf("  GFW vessel: %s\n", gfw_trip$vessel_name))
    cat("  Action: Add this vessel to vessel_mapping\n")
    return(NULL)
  }
  
  cat(sprintf("Vessel mapping: %s → %s\n\n", vessel_code, gfw_trip$vessel_name))
  
  # Get all DIGIvloot trips for this vessel
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::filter(vessel == vessel_code)
  
  if (nrow(dv_summary) == 0) {
    cat("✗ PROBLEM: No DIGIvloot trips for this vessel!\n")
    cat(sprintf("  DIGIvloot vessel code: %s\n", vessel_code))
    cat("  Action: Check if DIGIvloot data includes this vessel\n")
    return(NULL)
  }
  
  cat(sprintf("DIGIvloot has %d trips for vessel %s\n\n", 
              nrow(dv_summary), vessel_code))
  
  # Calculate time differences to all DIGIvloot trips
  candidates <- dv_summary %>%
    dplyr::mutate(
      departure_diff_hours = as.numeric(difftime(
        gfw_trip$port_departure, departure_date, units = "hours"
      )),
      arrival_diff_hours = as.numeric(difftime(
        gfw_trip$port_arrival, arrival_date, units = "hours"
      )),
      total_diff_hours = abs(departure_diff_hours) + abs(arrival_diff_hours)
    ) %>%
    dplyr::arrange(total_diff_hours)
  
  cat("CLOSEST DIGIvloot TRIPS:\n")
  print(candidates %>% 
          head(5) %>%
          dplyr::select(trip, departure_date, arrival_date,
                        departure_diff_hours, arrival_diff_hours, total_diff_hours))
  
  # Diagnosis
  cat("\n")
  best_match <- candidates %>% dplyr::slice(1)
  
  if (abs(best_match$departure_diff_hours) <= 24 & abs(best_match$arrival_diff_hours) <= 24) {
    cat("✓ SHOULD MATCH: Best candidate within 24h tolerance\n")
    cat("  → Check if matching function is being called correctly\n")
  } else if (abs(best_match$departure_diff_hours) <= 48 & abs(best_match$arrival_diff_hours) <= 48) {
    cat("⚠ NEAR MISS: Best candidate within 48h\n")
    cat(sprintf("  → Increase time_tolerance_hours to at least %.0f\n", 
                max(abs(best_match$departure_diff_hours), abs(best_match$arrival_diff_hours)) + 1))
  } else {
    cat("✗ NO GOOD MATCH: Closest candidate is far away\n")
    cat("  Possible reasons:\n")
    cat("  - This trip not in DIGIvloot data\n")
    cat("  - Different time zones\n")
    cat("  - Date format issues\n")
  }
  
  cat("\n")
  
  return(list(
    gfw_trip = gfw_trip,
    dv_candidates = candidates
  ))
}

# ============================================================================
# Function 5: Master diagnostic report
# ============================================================================
run_full_diagnostic <- function(gfw_trips, 
                                digivloot_data,
                                vessel_mapping = NULL,
                                time_tolerance_hours = 24) {
  
  cat("\n")
  cat("╔════════════════════════════════════════════════╗\n")
  cat("║  FULL DIAGNOSTIC REPORT                        ║\n")
  cat("║  DIGIvloot ↔ GFW Trip Matching                 ║\n")
  cat("╚════════════════════════════════════════════════╝\n")
  
  # 1. Basic matching
  diagnosis <- diagnose_unmatched_trips(
    gfw_trips, digivloot_data, vessel_mapping, time_tolerance_hours
  )
  
  # 2. Date coverage
  coverage <- compare_date_coverage(gfw_trips, digivloot_data, vessel_mapping)
  
  # 3. Near misses
  near_misses <- find_near_misses(
    gfw_trips, digivloot_data, vessel_mapping, 
    max_time_diff_hours = time_tolerance_hours * 2
  )
  
  # 4. Recommendations
  cat("\n========================================\n")
  cat("RECOMMENDATIONS\n")
  cat("========================================\n\n")
  
  if (diagnosis$summary$unmatched_gfw > 0) {
    cat(sprintf("You have %d unmatched GFW trips.\n\n", diagnosis$summary$unmatched_gfw))
    
    # Check near-misses
    close_matches <- near_misses %>%
      dplyr::filter(match_category == "Close (increase tolerance)")
    
    if (nrow(close_matches) > 0) {
      cat(sprintf("✓ QUICK FIX: %d trips would match with higher tolerance\n", 
                  nrow(close_matches)))
      recommended_tolerance <- ceiling(max(close_matches$departure_diff_hours, 
                                           close_matches$arrival_diff_hours,
                                           na.rm = TRUE))
      cat(sprintf("  → Try: time_tolerance_hours = %d\n\n", recommended_tolerance))
    }
    
    # Check for missing DIGIvloot data
    if (diagnosis$summary$total_gfw > diagnosis$summary$total_dv) {
      cat(sprintf("⚠ DIGIvloot has fewer trips (%d) than GFW (%d)\n",
                  diagnosis$summary$total_dv, diagnosis$summary$total_gfw))
      cat("  → Some GFW trips may not exist in DIGIvloot data\n")
      cat("  → Check if DIGIvloot data is complete for this period\n\n")
    }
    
    # Check for vessel mapping issues
    unmapped_vessels <- diagnosis$unmatched_gfw %>%
      dplyr::distinct(vessel_name) %>%
      dplyr::anti_join(
        vessel_mapping, 
        by = c("vessel_name" = "vessel_name")
      )
    
    if (nrow(unmapped_vessels) > 0) {
      cat("✗ VESSEL MAPPING ISSUE:\n")
      cat("  Vessels without mapping:\n")
      print(unmapped_vessels)
      cat("\n")
    }
  } else {
    cat("✓ All GFW trips matched successfully!\n\n")
  }
  
  cat("========================================\n\n")
  
  # Return comprehensive results
  return(list(
    diagnosis = diagnosis,
    coverage = coverage,
    near_misses = near_misses
  ))
}


# ============================================================================
# Comparison Visualization Functions
# ============================================================================


# ============================================================================
# Comparison Visualization Functions
# ============================================================================

# ============================================================================
# Function: Compare GFW and DIGIvloot trips visually (separate rows)
# ============================================================================
plot_trips_comparison_dumbbell <- function(gfw_trips,
                                           digivloot_data,
                                           vessel_mapping = NULL,
                                           date_range = NULL,
                                           show_trip_numbers = TRUE,
                                           point_size = 2.5,
                                           line_size = 1.5) {
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  require(lubridate)
  require(forcats)
  
  message("Creating comparison dumbbell plot...")
  
  # Create vessel mapping if not provided
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # Extract DIGIvloot trip summary
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    ) %>%
    dplyr::filter(!is.na(vessel_name))
  
  message(sprintf("DIGIvloot trips: %d", nrow(dv_summary)))
  message(sprintf("GFW trips: %d", nrow(gfw_trips)))
  
  # Prepare GFW trips
  gfw_plot <- gfw_trips %>%
    dplyr::filter(!is.na(port_departure), !is.na(port_arrival)) %>%
    dplyr::mutate(
      source = "GFW",
      departure = port_departure,
      arrival = port_arrival,
      trip_num = sprintf("%03d", trip_number),
      duration_hours = trip_duration_hours
    ) %>%
    dplyr::select(vessel_name, source, departure, arrival, trip_num, duration_hours, trip_id)
  
  # Prepare DIGIvloot trips
  dv_plot <- dv_summary %>%
    dplyr::mutate(
      source = "DIGIvloot",
      departure = departure_date,
      arrival = arrival_date,
      trip_num = substr(trip, nchar(trip) - 2, nchar(trip)),
      duration_hours = trip_duration_days * 24,
      trip_identifier = trip
    ) %>%
    dplyr::select(vessel_name, source, departure, arrival, trip_num, duration_hours, trip_identifier)
  
  # Combine datasets
  all_trips <- dplyr::bind_rows(
    gfw_plot %>% dplyr::rename(trip_identifier = trip_id),
    dv_plot
  )
  
  # Filter by date range if specified
  if (!is.null(date_range)) {
    all_trips <- all_trips %>%
      dplyr::filter(
        departure >= lubridate::ymd(date_range[1]),
        departure <= lubridate::ymd(date_range[2])
      )
  }
  
  # Create vessel-source combination for y-axis positioning
  all_trips <- all_trips %>%
    dplyr::mutate(
      vessel_source = paste(vessel_name, source, sep = " - "),
      label_date = departure + (arrival - departure) / 2
    ) %>%
    dplyr::arrange(vessel_name, source, departure)
  
  message(sprintf("Plotting %d trips total", nrow(all_trips)))
  
  # Create plot
  p <- ggplot(all_trips, aes(y = forcats::fct_rev(vessel_source))) +
    ggalt::geom_dumbbell(
      aes(x = departure, xend = arrival, color = source),
      size = line_size,
      size_x = point_size,
      size_xend = point_size,
      alpha = 0.8
    ) +
    scale_color_manual(
      name = "Source",
      values = c("GFW" = "#2b8cbe", "DIGIvloot" = "#de2d26"),
      labels = c("GFW" = "GFW", "DIGIvloot" = "DIGIvloot")
    ) +
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
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    labs(
      x = "Date",
      y = NULL,
      title = "Trip Comparison: GFW vs DIGIvloot",
      subtitle = sprintf("Blue = GFW (%d trips) | Red = DIGIvloot (%d trips)",
                         nrow(gfw_plot), nrow(dv_plot))
    )
  
  # Add trip numbers if requested
  if (show_trip_numbers) {
    p <- p +
      geom_text(
        aes(x = label_date, label = trip_num, color = source),
        size = 2.2,
        vjust = -0.7,
        fontface = "bold",
        show.legend = FALSE
      )
  }
  
  message("Plot created successfully!")
  
  return(p)
}

# ============================================================================
# Function: Compact comparison (vessels on same row, sources overlaid)
# ============================================================================
plot_trips_comparison_compact <- function(gfw_trips,
                                          digivloot_data,
                                          vessel_mapping = NULL,
                                          date_range = NULL,
                                          show_trip_numbers = TRUE,
                                          point_size = 2.5,
                                          line_size = 1.5) {
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  require(lubridate)
  require(forcats)
  
  message("Creating compact comparison plot...")
  
  # Create vessel mapping if not provided
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # Extract DIGIvloot trip summary
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    ) %>%
    dplyr::filter(!is.na(vessel_name))
  
  # Prepare GFW trips
  gfw_plot <- gfw_trips %>%
    dplyr::filter(!is.na(port_departure), !is.na(port_arrival)) %>%
    dplyr::mutate(
      source = "GFW",
      departure = port_departure,
      arrival = port_arrival,
      trip_num = sprintf("%03d", trip_number)
    ) %>%
    dplyr::select(vessel_name, source, departure, arrival, trip_num, trip_id)
  
  # Prepare DIGIvloot trips
  dv_plot <- dv_summary %>%
    dplyr::mutate(
      source = "DIGIvloot",
      departure = departure_date,
      arrival = arrival_date,
      trip_num = substr(trip, nchar(trip) - 2, nchar(trip)),
      trip_identifier = trip
    ) %>%
    dplyr::select(vessel_name, source, departure, arrival, trip_num, trip_identifier)
  
  # Combine datasets
  all_trips <- dplyr::bind_rows(
    gfw_plot %>% dplyr::rename(trip_identifier = trip_id),
    dv_plot
  )
  
  # Filter by date range if specified
  if (!is.null(date_range)) {
    all_trips <- all_trips %>%
      dplyr::filter(
        departure >= lubridate::ymd(date_range[1]),
        departure <= lubridate::ymd(date_range[2])
      )
  }
  
  all_trips <- all_trips %>%
    dplyr::mutate(
      label_date = departure + (arrival - departure) / 2,
      # Slight y-offset for overlapping trips
      y_offset = ifelse(source == "GFW", 0, 0.15)
    ) %>%
    dplyr::arrange(vessel_name, source, departure)
  
  message(sprintf("Plotting %d trips total", nrow(all_trips)))
  
  # Create plot with overlay
  p <- ggplot(all_trips, aes(y = as.numeric(forcats::fct_rev(vessel_name)) + y_offset)) +
    ggalt::geom_dumbbell(
      aes(x = departure, xend = arrival, color = source),
      size = line_size,
      size_x = point_size,
      size_xend = point_size,
      alpha = 0.7
    ) +
    scale_color_manual(
      name = "Source",
      values = c("GFW" = "#2b8cbe", "DIGIvloot" = "#de2d26")
    ) +
    scale_y_continuous(
      breaks = seq_along(unique(all_trips$vessel_name)),
      labels = rev(sort(unique(all_trips$vessel_name)))
    ) +
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
      legend.position = "top",
      legend.title = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    labs(
      x = "Date",
      y = NULL,
      title = "Trip Comparison: GFW vs DIGIvloot (Overlaid)",
      subtitle = sprintf("Blue = GFW (%d) | Red = DIGIvloot (%d) | Overlapping trips indicate good match",
                         nrow(gfw_plot), nrow(dv_plot))
    )
  
  # Add trip numbers
  if (show_trip_numbers) {
    p <- p +
      geom_text(
        aes(x = label_date, label = trip_num, color = source),
        size = 2,
        vjust = -0.9,
        fontface = "bold",
        show.legend = FALSE
      )
  }
  
  message("Plot created successfully!")
  
  return(p)
}

# ============================================================================
# Function: Side-by-side faceted comparison
# ============================================================================
plot_trips_comparison_faceted <- function(gfw_trips,
                                          digivloot_data,
                                          vessel_mapping = NULL,
                                          date_range = NULL,
                                          show_trip_numbers = FALSE) {
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  require(lubridate)
  
  message("Creating faceted comparison plot...")
  
  # Create vessel mapping if not provided
  if (is.null(vessel_mapping)) {
    vessel_mapping <- create_vessel_mapping()
  }
  
  # Extract DIGIvloot trip summary
  dv_summary <- extract_digivloot_trip_summary(digivloot_data) %>%
    dplyr::left_join(
      vessel_mapping %>% dplyr::select(vessel, vessel_name),
      by = "vessel"
    ) %>%
    dplyr::filter(!is.na(vessel_name))
  
  # Prepare GFW trips
  gfw_plot <- gfw_trips %>%
    dplyr::filter(!is.na(port_departure), !is.na(port_arrival)) %>%
    dplyr::mutate(
      source = "GFW",
      departure = port_departure,
      arrival = port_arrival,
      trip_num = sprintf("%03d", trip_number)
    ) %>%
    dplyr::select(vessel_name, source, departure, arrival, trip_num)
  
  # Prepare DIGIvloot trips
  dv_plot <- dv_summary %>%
    dplyr::mutate(
      source = "DIGIvloot",
      departure = departure_date,
      arrival = arrival_date,
      trip_num = substr(trip, nchar(trip) - 2, nchar(trip))
    ) %>%
    dplyr::select(vessel_name, source, departure, arrival, trip_num)
  
  # Combine
  all_trips <- dplyr::bind_rows(gfw_plot, dv_plot)
  
  # Filter by date
  if (!is.null(date_range)) {
    all_trips <- all_trips %>%
      dplyr::filter(
        departure >= lubridate::ymd(date_range[1]),
        departure <= lubridate::ymd(date_range[2])
      )
  }
  
  all_trips <- all_trips %>%
    dplyr::mutate(label_date = departure + (arrival - departure) / 2)
  
  # Plot with facets
  p <- ggplot(all_trips, aes(y = 1)) +
    ggalt::geom_dumbbell(
      aes(x = departure, xend = arrival),
      size = 1.5,
      size_x = 2.5,
      size_xend = 2.5,
      color = "grey60",
      colour_x = "#2b8cbe",
      colour_xend = "#de2d26",
      alpha = 0.8
    ) +
    facet_grid(vessel_name ~ source, scales = "free_y", space = "free_y") +
    scale_x_datetime(
      date_breaks = "1 week",
      date_labels = "%b %d"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    labs(
      x = "Date",
      y = NULL,
      title = "Side-by-Side Comparison: GFW vs DIGIvloot",
      subtitle = "Each vessel shown separately | Directly compare trip timing"
    )
  
  if (show_trip_numbers) {
    p <- p +
      geom_text(
        aes(x = label_date, label = trip_num),
        size = 2,
        vjust = -0.5
      )
  }
  
  return(p)
}
