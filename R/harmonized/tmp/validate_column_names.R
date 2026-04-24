# ==============================================================================
# Column Name Validation Functions
# ==============================================================================
# Check if data objects conform to Poseidat/standardized naming conventions
# ==============================================================================

library(tidyverse)
library(glue)

#' Check if column names conform to Poseidat naming conventions
#' 
#' @param data dataframe to check
#' @param data_type type of data: "haul", "trip", "catch", or "all"
#' @param strict if TRUE, warns about unexpected columns; if FALSE, only checks for required columns
#' @return list with validation results
#' 
#' @examples
#' check_poseidat_columns(haul_data, "haul")
#' check_poseidat_columns(catch_data, "catch", strict = TRUE)
check_poseidat_columns <- function(data, data_type = "all", strict = FALSE) {
  
  current_cols <- names(data)
  
  # Define expected columns for each data type
  expected_columns <- list(
    
    # Haul data columns
    haul_required = c("vessel", "trip_id", "haul_id", "date"),
    haul_optional = c(
      "shoot_lat", "shoot_lon", "shoot_time",
      "haul_lat", "haul_lon", "haul_time",
      "fishing_time_hours", "mesh_size", "water_depth", "gear_type",
      "area", "division", "rect", "economiczone"
    ),
    
    # Trip data columns
    trip_required = c("vessel", "trip_id"),
    trip_optional = c(
      "departure_date", "departure_port", 
      "arrival_date", "arrival_port",
      "captain", "n_hauls", "total_catch_kg"
    ),
    
    # Catch data columns
    catch_required = c("vessel", "trip_id", "haul_id", "species_code", "weight_kg"),
    catch_optional = c(
      "date", "box_count", "box_number"
    ),
    
    # Metadata columns (can appear in any data type)
    metadata = c("save_date", "save_timestamp", "fetch_date", "fetch_timestamp",
                 "migration_date", "migration_timestamp", "source_format")
  )
  
  # Select which columns to check based on data_type
  if (data_type == "haul") {
    required <- expected_columns$haul_required
    optional <- expected_columns$haul_optional
  } else if (data_type == "trip") {
    required <- expected_columns$trip_required
    optional <- expected_columns$trip_optional
  } else if (data_type == "catch") {
    required <- expected_columns$catch_required
    optional <- expected_columns$catch_optional
  } else if (data_type == "all") {
    # For "all", check against all possible columns
    required <- c()
    optional <- c(
      expected_columns$haul_required, expected_columns$haul_optional,
      expected_columns$trip_required, expected_columns$trip_optional,
      expected_columns$catch_required, expected_columns$catch_optional
    )
  } else {
    stop(glue("Invalid data_type: {data_type}. Must be 'haul', 'trip', 'catch', or 'all'"))
  }
  
  # Check for required columns
  missing_required <- setdiff(required, current_cols)
  present_required <- intersect(required, current_cols)
  
  # Check for optional columns
  missing_optional <- setdiff(optional, current_cols)
  present_optional <- intersect(optional, current_cols)
  
  # Check for unexpected columns (not in standard or metadata)
  all_standard <- c(required, optional, expected_columns$metadata)
  unexpected <- setdiff(current_cols, all_standard)
  
  # Build results
  results <- list(
    data_type = data_type,
    total_columns = length(current_cols),
    required = list(
      present = present_required,
      missing = missing_required,
      n_present = length(present_required),
      n_missing = length(missing_required)
    ),
    optional = list(
      present = present_optional,
      missing = missing_optional,
      n_present = length(present_optional),
      n_missing = length(missing_optional)
    ),
    unexpected = unexpected,
    n_unexpected = length(unexpected),
    valid = length(missing_required) == 0
  )
  
  # Print summary
  cat("\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
  cat(glue("POSEIDAT COLUMN NAME VALIDATION - {toupper(data_type)} DATA\n"))
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  
  cat(glue("Total columns: {length(current_cols)}\n\n"))
  
  # Required columns
  if (length(required) > 0) {
    cat("REQUIRED COLUMNS:\n")
    if (length(present_required) > 0) {
      cat(glue("  ✓ Present ({length(present_required)}/{length(required)}): "))
      cat(paste(present_required, collapse = ", "), "\n")
    }
    if (length(missing_required) > 0) {
      cat(glue("  ✗ MISSING ({length(missing_required)}/{length(required)}): "))
      cat(paste(missing_required, collapse = ", "), "\n")
    }
    cat("\n")
  }
  
  # Optional columns
  if (length(optional) > 0) {
    cat("OPTIONAL COLUMNS:\n")
    cat(glue("  Present: {length(present_optional)}/{length(optional)}\n"))
    if (length(present_optional) > 0 && length(present_optional) <= 10) {
      cat("    ", paste(present_optional, collapse = ", "), "\n")
    } else if (length(present_optional) > 10) {
      cat("    ", paste(present_optional[1:10], collapse = ", "), "... (", 
          length(present_optional) - 10, " more)\n")
    }
    cat("\n")
  }
  
  # Unexpected columns
  if (strict && length(unexpected) > 0) {
    cat("UNEXPECTED COLUMNS (not in standard):\n")
    cat(glue("  ⚠ Found {length(unexpected)}: "))
    cat(paste(unexpected, collapse = ", "), "\n\n")
  }
  
  # Overall validation result
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
  if (results$valid) {
    cat("✓ VALIDATION PASSED - All required columns present\n")
  } else {
    cat("✗ VALIDATION FAILED - Missing required columns\n")
  }
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  
  invisible(results)
}


#' Compare column names between two datasets
#' 
#' @param data1 first dataframe
#' @param data2 second dataframe
#' @param name1 name for first dataset (default: "Data 1")
#' @param name2 name for second dataset (default: "Data 2")
#' @return list with comparison results
#' 
#' @examples
#' compare_columns(old_haul, new_haul, "Old Format", "New Format")
compare_columns <- function(data1, data2, name1 = "Data 1", name2 = "Data 2") {
  
  cols1 <- names(data1)
  cols2 <- names(data2)
  
  # Find differences
  only_in_1 <- setdiff(cols1, cols2)
  only_in_2 <- setdiff(cols2, cols1)
  in_both <- intersect(cols1, cols2)
  
  # Print comparison
  cat("\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
  cat(glue("COLUMN NAME COMPARISON\n"))
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  
  cat(glue("{name1}: {length(cols1)} columns\n"))
  cat(glue("{name2}: {length(cols2)} columns\n"))
  cat(glue("In both: {length(in_both)} columns\n\n"))
  
  if (length(only_in_1) > 0) {
    cat(glue("Only in {name1} ({length(only_in_1)}):\n"))
    cat("  ", paste(only_in_1, collapse = ", "), "\n\n")
  }
  
  if (length(only_in_2) > 0) {
    cat(glue("Only in {name2} ({length(only_in_2)}):\n"))
    cat("  ", paste(only_in_2, collapse = ", "), "\n\n")
  }
  
  if (length(in_both) > 0) {
    cat(glue("In both ({length(in_both)}):\n"))
    if (length(in_both) <= 15) {
      cat("  ", paste(in_both, collapse = ", "), "\n")
    } else {
      cat("  ", paste(in_both[1:15], collapse = ", "), "... (", 
          length(in_both) - 15, " more)\n")
    }
  }
  
  cat("\n")
  cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  
  invisible(list(
    only_in_1 = only_in_1,
    only_in_2 = only_in_2,
    in_both = in_both,
    identical = length(only_in_1) == 0 && length(only_in_2) == 0
  ))
}


#' Get suggested column name mappings for non-standard columns
#' 
#' @param data dataframe with non-standard column names
#' @return tibble with suggested mappings
#' 
#' @examples
#' suggest_mappings(my_data)
suggest_mappings <- function(data) {
  
  current_cols <- tolower(names(data))
  
  # Common variations and their standard equivalents
  mapping_suggestions <- list(
    vessel = c("vessel", "vessel_id", "ship", "boot", "schip"),
    trip_id = c("trip_id", "trip", "tripnumber", "tripnr", "reis", "reisnummer"),
    haul_id = c("haul_id", "haul", "trek", "trekid", "haalnr"),
    date = c("date", "datum", "day", "dag"),
    shoot_lat = c("shoot_lat", "latitude", "lat", "latitude_shoot", "breedtegraad"),
    shoot_lon = c("shoot_lon", "longitude", "lon", "longitude_shoot", "lengtegraad"),
    haul_lat = c("haul_lat", "latitude_haul", "lat_haul", "latitude_halen"),
    haul_lon = c("haul_lon", "longitude_haul", "lon_haul", "longitude_halen"),
    shoot_time = c("shoot_time", "tijd", "time", "tijd_shoot", "tijdbeginuitzetten"),
    haul_time = c("haul_time", "tijd_haul", "time_haul", "tijdeindehalen"),
    species_code = c("species_code", "species", "soort", "vis"),
    weight_kg = c("weight_kg", "catch_kg", "gewicht", "weight", "catch", "vangst"),
    box_count = c("box_count", "box_number", "kist", "box", "kistnummer", "aantal")
  )
  
  suggestions <- tibble()
  
  for (standard_name in names(mapping_suggestions)) {
    variations <- mapping_suggestions[[standard_name]]
    matching <- variations[variations %in% current_cols]
    
    if (length(matching) > 0 && matching[1] != standard_name) {
      suggestions <- suggestions %>%
        add_row(
          current_name = matching[1],
          suggested_name = standard_name,
          reason = "Common variation"
        )
    }
  }
  
  if (nrow(suggestions) > 0) {
    cat("\n")
    cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
    cat("SUGGESTED COLUMN NAME MAPPINGS\n")
    cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
    
    print(suggestions, n = Inf)
    
    cat("\n")
    cat("To apply these mappings, use:\n")
    cat("data %>% rename(\n")
    for (i in 1:nrow(suggestions)) {
      cat(glue("  {suggestions$suggested_name[i]} = {suggestions$current_name[i]}"))
      if (i < nrow(suggestions)) cat(",")
      cat("\n")
    }
    cat(")\n\n")
    
  } else {
    cat("\nNo mapping suggestions found - column names look good!\n\n")
  }
  
  invisible(suggestions)
}


#' Quick validation - just check and return TRUE/FALSE
#' 
#' @param data dataframe to check
#' @param data_type type of data: "haul", "trip", "catch"
#' @return TRUE if valid, FALSE if not
is_poseidat_compliant <- function(data, data_type) {
  results <- check_poseidat_columns(data, data_type, strict = FALSE)
  return(results$valid)
}
