# ==============================================================================
# FLYSHOOT Storage Functions
# ==============================================================================
# Parquet-based storage functions following Poseidat conventions
# ==============================================================================

message(paste0("02_storage_functions.R loaded — saved: ",
               format(file.mtime(file.path(here::here(), "R/harmonized", "02_storage_functions.R")),
                      "%Y-%m-%d %H:%M:%S")))

library(tidyverse)
library(arrow)
library(glue)
library(fs)
library(lubridate)

# Load configuration
if (file.exists("config.json")) {
  config <- jsonlite::read_json("config.json")
} else {
  stop("Configuration file not found. Please run 00_setup.R first.")
}

# Save flyshoot data to Parquet ----
save_flyshoot_data <- function(data, data_type, date_range = NULL, subfolder = NULL) {
  
  valid_types <- c("haul", "trip", "kisten", "elog", "elog_trek",
                   "vessel_movement", "harbours")
  
  if (!data_type %in% valid_types) {
    stop(glue("data_type must be one of: {paste(valid_types, collapse = ', ')}"))
  }
  
  # Determine directory path
  dir_path <- if (!is.null(subfolder)) {
    glue("{config$raw_data_path}/{data_type}/{subfolder}")
  } else {
    glue("{config$raw_data_path}/{data_type}")
  }
  
  dir_create(dir_path)
  
  # Fixed filename — one file per data type, overwritten each run.
  # The date_range argument is kept for API compatibility but no longer
  # used in the filename (it was misleading: the name changed every run
  # as new trips were added, and old files were never deleted, causing
  # open_dataset() to read duplicate data from accumulating files).
  filename <- glue("{dir_path}/{data_type}.parquet")
  
  # Remove any old parquet files in this directory (including date-suffixed
  # legacy files from previous runs) before writing the new one.
  old_files <- list.files(dir_path, pattern = "\\.parquet$", full.names = TRUE)
  old_files <- old_files[old_files != filename]  # keep the target if it exists
  if (length(old_files) > 0) {
    file.remove(old_files)
    message(glue("  Removed {length(old_files)} old parquet file(s) from {data_type}/"))
  }
  
  # Add metadata columns
  data_with_meta <- data %>%
    mutate(
      save_date      = Sys.Date(),
      save_timestamp = Sys.time(),
      .after         = last_col()
    )
  
  write_parquet(data_with_meta, filename)
  if (isTRUE(getOption("flyshoot.verbose", default = FALSE)))
    message(glue("✓ Saved {nrow(data)} rows to {filename}"))
  
  invisible(filename)
}

# Save all flyshoot data types at once ----
save_all_flyshoot_data <- function(data_list, date_range = NULL, subfolder = NULL) {
  
  valid_names <- c("haul", "trip", "kisten", "elog", "elog_trek", "harbours")
  
  if (!all(names(data_list) %in% valid_names)) {
    warning("data_list should have names from: 'haul', 'trip', 'kisten', 'elog', 'elog_trek', 'harbours'")
  }
  
  paths <- vector("list", length(data_list))
  names(paths) <- names(data_list)
  for (nm in names(data_list)) {
    d <- data_list[[nm]]
    if (!is.null(d) && nrow(d) > 0) {
      paths[[nm]] <- save_flyshoot_data(d, nm, date_range, subfolder)
    } else {
      message(glue("⚠ Skipping {nm} - no data"))
    }
  }
  
  invisible(paths)
}

# Load flyshoot data from Parquet ----
load_flyshoot_data <- function(data_type, 
                               date_from = NULL, 
                               date_to = NULL, 
                               vessel_ids = NULL, 
                               trip_ids = NULL,
                               subfolder = NULL) {
  
  # Determine data path
  data_path <- if (!is.null(subfolder)) {
    glue("{config$raw_data_path}/{data_type}/{subfolder}")
  } else {
    glue("{config$raw_data_path}/{data_type}")
  }
  
  # Check if directory exists
  if (!dir.exists(data_path)) {
    warning(glue("No data found at {data_path}"))
    return(tibble())
  }
  
  # Open dataset
  ds <- open_dataset(data_path)
  
  # Apply filters — vessel and trip_id apply to all data types;
  # date filters use different column names depending on data type.
  if (!is.null(vessel_ids)) {
    ds <- ds %>% filter(vessel %in% vessel_ids)
  }

  if (!is.null(trip_ids)) {
    ds <- ds %>% filter(trip_id %in% trip_ids)
  }

  if (!is.null(date_from) || !is.null(date_to)) {
    date_col <- dplyr::case_when(
      data_type == "trip" ~ "departure_date",
      TRUE                ~ "date"
    )
    if (date_col %in% names(ds)) {
      if (!is.null(date_from)) ds <- ds %>% filter(.data[[date_col]] >= as.Date(date_from))
      if (!is.null(date_to))   ds <- ds %>% filter(.data[[date_col]] <= as.Date(date_to))
    }
  }
  
  # Collect data
  result <- ds %>% collect()
  
  if (isTRUE(getOption("flyshoot.verbose", default = FALSE)))
    message(glue("✓ Loaded {nrow(result)} {data_type} records"))
  
  return(result)
}

# Load all flyshoot data types ----
load_all_flyshoot_data <- function(date_from = NULL, 
                                   date_to = NULL, 
                                   vessel_ids = NULL,
                                   trip_ids = NULL,
                                   subfolder = NULL) {
  
  list(
    haul = load_flyshoot_data("haul", date_from, date_to, vessel_ids, trip_ids, subfolder),
    trip = load_flyshoot_data("trip", date_from, date_to, vessel_ids, trip_ids, subfolder),
    kisten = load_flyshoot_data("kisten", date_from, date_to, vessel_ids, trip_ids, subfolder),
    elog = load_flyshoot_data("elog", date_from, date_to, vessel_ids, trip_ids, subfolder),
    elog_trek = load_flyshoot_data("elog_trek", date_from, date_to, vessel_ids, trip_ids, subfolder)
  )
}

# Append new data to existing parquet files ----
append_flyshoot_data <- function(new_data, data_type, check_duplicates = TRUE) {
  
  # Load existing data
  existing_data <- tryCatch(
    load_flyshoot_data(data_type),
    error = function(e) tibble()
  )
  
  # Check for duplicates if requested
  if (check_duplicates && nrow(existing_data) > 0) {
    
    # Define key columns for each data type
    key_cols <- list(
      haul = c("vessel", "trip_id", "haul_id"),
      trip = c("vessel", "trip_id"),
      kisten = c("vessel", "trip_id", "box_number"),
      elog = c("vessel", "trip_id"),
      elog_trek = c("vessel", "trip_id", "haul_id")
    )
    
    if (data_type %in% names(key_cols)) {
      keys <- key_cols[[data_type]]
      keys <- keys[keys %in% names(new_data)]
      
      if (length(keys) > 0) {
        # Find duplicates
        duplicates <- new_data %>%
          semi_join(existing_data, by = keys)
        
        if (nrow(duplicates) > 0) {
          message(glue("⚠ Found {nrow(duplicates)} duplicate records - removing"))
          new_data <- new_data %>%
            anti_join(existing_data, by = keys)
        }
      }
    }
  }
  
  # Combine data
  if (nrow(existing_data) > 0) {
    combined_data <- bind_rows(existing_data, new_data)
  } else {
    combined_data <- new_data
  }
  
  # Save with current date
  save_flyshoot_data(combined_data, data_type)
  
  message(glue("✓ Added {nrow(new_data)} new {data_type} records (total: {nrow(combined_data)})"))
  
  invisible(combined_data)
}

# Create processed summaries ----
create_trip_summary <- function(date_from = NULL, date_to = NULL, save_output = TRUE) {
  
  # Load data
  trips <- load_flyshoot_data("trip", date_from, date_to)
  hauls <- load_flyshoot_data("haul", date_from, date_to)
  
  if (nrow(trips) == 0 || nrow(hauls) == 0) {
    warning("No trip or haul data available for summary")
    return(tibble())
  }
  
  # Create summary
  trip_summary <- trips %>%
    left_join(
      hauls %>%
        group_by(trip_id) %>%
        summarise(
          n_hauls = n(),
          total_catch_kg = sum(total_catch_kg, na.rm = TRUE),
          fishing_days = as.numeric(max(date, na.rm = TRUE) - min(date, na.rm = TRUE)),
          first_haul = min(date, na.rm = TRUE),
          last_haul = max(date, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "trip_id"
    ) %>%
    mutate(
      cpue = if_else(n_hauls > 0, total_catch_kg / n_hauls, NA_real_),
      catch_per_day = if_else(fishing_days > 0, total_catch_kg / fishing_days, NA_real_)
    )
  
  # Save if requested
  if (save_output && nrow(trip_summary) > 0) {
    output_path <- file.path(config$processed_data_path, "trip_summaries")
    dir_create(output_path)
    
    filename <- glue("{output_path}/trip_summary_{format(Sys.Date(), '%Y%m%d')}.parquet")
    write_parquet(trip_summary, filename)
    message(glue("✓ Saved trip summary to {filename}"))
  }
  
  return(trip_summary)
}

# ==============================================================================
# PIPELINE STORAGE HELPERS
# (used by 03_main_workflow.R to safely update parquet files)
# ==============================================================================

#' Safely remove processed trips from an existing dataset before appending new data.
#' @param existing existing parquet tibble
#' @param trips_lookup tibble with vessel + trip_id columns to remove
#' @param new_data optionally used for secondary date-range stale removal
#' @return list(cleaned = tibble, n_removed = integer)
safe_remove_trips <- function(existing, trips_lookup, new_data = NULL) {
  if (nrow(existing) == 0 ||
      !all(c("vessel", "trip_id") %in% names(existing))) {
    return(list(cleaned = existing, n_removed = 0L))
  }
  cleaned <- dplyr::anti_join(existing, trips_lookup, by = c("vessel", "trip_id"))
  if (!is.null(new_data) && nrow(new_data) > 0 &&
      all(c("vessel", "date") %in% names(new_data)) &&
      "date" %in% names(cleaned)) {
    date_ranges <- new_data %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarise(date_min = min(date, na.rm = TRUE),
                       date_max = max(date, na.rm = TRUE), .groups = "drop")
    stale <- cleaned %>%
      dplyr::inner_join(date_ranges, by = "vessel") %>%
      dplyr::filter(date >= date_min & date <= date_max) %>%
      dplyr::select(vessel, trip_id) %>% dplyr::distinct()
    if (nrow(stale) > 0) {
      message(glue("  Removing {nrow(stale)} stale trip_id(s): ",
                   "{paste(unique(stale$trip_id), collapse=', ')}"))
      cleaned <- dplyr::anti_join(cleaned, stale, by = c("vessel", "trip_id"))
    }
  }
  list(cleaned = cleaned, n_removed = nrow(existing) - nrow(cleaned))
}


#' Reconcile column types between existing parquet data and new data before bind_rows.
#' @param existing existing tibble (from parquet)
#' @param new_data new tibble being appended
#' @return existing with shared column types coerced to match new_data
reconcile_types <- function(existing, new_data) {
  if (nrow(existing) == 0) return(existing)
  shared <- intersect(names(existing), names(new_data))
  for (col in shared) {
    type_new <- class(new_data[[col]])[1]
    type_old <- class(existing[[col]])[1]
    if (type_new != type_old) {
      existing[[col]] <- tryCatch(
        switch(type_new,
               "integer"   = as.integer(suppressWarnings(as.numeric(existing[[col]]))),
               "numeric"   = suppressWarnings(as.numeric(existing[[col]])),
               "double"    = suppressWarnings(as.numeric(existing[[col]])),
               "character" = as.character(existing[[col]]),
               "logical"   = as.logical(existing[[col]]),
               "Date"      = as.Date(existing[[col]]),
               "POSIXct"   = as.POSIXct(existing[[col]], tz = "UTC"),
               existing[[col]]
        ),
        error = function(e) {
          message(glue("  \u26a0 Could not reconcile type for '{col}': {type_old} -> {type_new}"))
          existing[[col]]
        }
      )
    }
  }
  existing
}


# Get storage statistics ----
get_storage_stats <- function() {
  
  stats <- tibble(
    data_type = character(),
    n_files = integer(),
    total_size_mb = numeric(),
    n_records = integer(),
    oldest_date = as.Date(character()),
    newest_date = as.Date(character())
  )
  
  data_types <- c("haul", "trip", "kisten", "elog", "elog_trek", "vessel_movement")
  
  for (dtype in data_types) {
    data_path <- glue("{config$raw_data_path}/{dtype}")
    
    if (dir.exists(data_path)) {
      files <- list.files(data_path, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
      
      if (length(files) > 0) {
        total_size <- sum(file.size(files)) / (1024^2)  # Convert to MB
        
        # Load data to get record count and date range
        data <- tryCatch(
          load_flyshoot_data(dtype),
          error = function(e) tibble()
        )
        
        # Extract dates based on data type
        dates <- if ("date" %in% names(data)) {
          data$date
        } else if ("departure_date" %in% names(data)) {
          data$departure_date
        } else if ("save_date" %in% names(data)) {
          data$save_date
        } else {
          NULL
        }
        
        stats <- stats %>%
          add_row(
            data_type = dtype,
            n_files = length(files),
            total_size_mb = round(total_size, 2),
            n_records = nrow(data),
            oldest_date = if (!is.null(dates)) min(dates, na.rm = TRUE) else NA,
            newest_date = if (!is.null(dates)) max(dates, na.rm = TRUE) else NA
          )
      }
    }
  }
  
  if (nrow(stats) > 0) {
    cat("\n")
    cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
    cat("FLYSHOOT DATA STORAGE STATISTICS\n")
    cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
    print(stats, n = Inf)
    cat("=" |> rep(70) |> paste0(collapse = ""), "\n")
    cat("Total storage:", sum(stats$total_size_mb), "MB\n")
    cat("Total records:", format(sum(stats$n_records), big.mark = ","), "\n")
    cat("=" |> rep(70) |> paste0(collapse = ""), "\n\n")
  } else {
    message("No data files found")
  }
  
  invisible(stats)
}

# List data files ----
list_data_files <- function(data_type = NULL) {
  
  if (is.null(data_type)) {
    data_types <- c("haul", "trip", "kisten", "elog", "elog_trek", "vessel_movement")
  } else {
    data_types <- data_type
  }
  
  all_files <- list()
  
  for (dtype in data_types) {
    data_path <- glue("{config$raw_data_path}/{dtype}")
    
    if (dir.exists(data_path)) {
      files <- list.files(data_path, pattern = "\\.parquet$", 
                          full.names = TRUE, recursive = TRUE)
      
      if (length(files) > 0) {
        all_files[[dtype]] <- tibble(
          data_type = dtype,
          filename = basename(files),
          full_path = files,
          size_mb = round(file.size(files) / (1024^2), 2),
          modified = file.mtime(files)
        )
      }
    }
  }
  
  if (length(all_files) > 0) {
    result <- bind_rows(all_files) %>%
      arrange(data_type, desc(modified))
    
    print(result)
    invisible(result)
  } else {
    message("No parquet files found")
    invisible(tibble())
  }
}

# Archive old data ----
archive_flyshoot_data <- function(data_type, before_date, confirm = FALSE) {
  
  if (!confirm) {
    stop("Set confirm = TRUE to archive data. This will move files to the archive folder.")
  }
  
  data_path <- glue("{config$raw_data_path}/{data_type}")
  archive_path <- file.path(config$archive_path, data_type)
  
  if (!dir.exists(data_path)) {
    message(glue("No data found at {data_path}"))
    return(invisible(NULL))
  }
  
  dir_create(archive_path)
  
  files <- list.files(data_path, pattern = "\\.parquet$", full.names = TRUE)
  
  archived <- 0
  for (file in files) {
    # Check file modification date
    if (file.mtime(file) < as.POSIXct(before_date)) {
      dest_file <- file.path(archive_path, basename(file))
      file.copy(file, dest_file)
      file.remove(file)
      archived <- archived + 1
      message(glue("✓ Archived: {basename(file)}"))
    }
  }
  
  message(glue("\n✓ Archived {archived} files to {archive_path}"))
  invisible(archived)
}
