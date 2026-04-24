# GFW
#

options(dplyr.summarise.inform = FALSE)

# Reset lists
rm(list=ls())

# Libraries
require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
require(lubridate, quietly=TRUE)     # data handling
require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
library(sf)

# devtools::install_github("GlobalFishingWatch/gfwr", dependencies = TRUE)
library(gfwr)
key <- gfw_auth()

# source("../../prf/R/my utils.r")
# source("../../mptools/R/get_onedrive.r")
source("R/FLYSHOOT utils.r")

spatialdir <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
load(file.path(spatialdir, "world_mr_df.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))

fao_sf          <- loadRData(file.path(spatialdir, "fao_sf.RData"))
fao_sf_division <- fao_sf %>% filter(F_LEVEL=="DIVISION") %>% dplyr::select(F_DIVISION) %>% rename(division = F_DIVISION)

# flyshoot info
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General")

# read vessels from excel file
r <- 
  readxl::read_excel(path=file.path(onedrive, "data","flyshoot vessels update EvL.xlsx")) %>% 
  mutate(mmsi = as.character(mmsi))

# === NEW CODE, 5/2/2026, derived from Claude

library(gfwr)
library(dplyr)
library(purrr)
library(lubridate)

# mmsi <- "24468200"
# vessel_info <- gfw_vessel_info(query='24468200',search_type = "search",key = key)
# vessel_info <- gfw_vessel_info(where = "ssvid = '24468200' AND flag = 'NLD'", key = key)
# vessel_info <- gfw_vessel_info(query = "24468200",search_type = "search", key=key)
# vessel_info <- gfw_vessel_info(where = "imo = '9917268' AND flag = 'NLD'", key = key)
# vessel_info <- gfw_vessel_info(query='244682000',search_type = "search",key = key)

# Check how many vessels were found
# print(paste("Total vessels found:", length(unique(vessel_info$registryInfo$recordId))))

# Look at the self-reported information to see the flags
# vessel_info$selfReportedInfo %>%
#   select(vesselId, ssvid, shipname, flag, callsign, imo, 
#          transmissionDateFrom, transmissionDateTo) %>%
#   print()

# ============================================================================
# Function 1: Get vesselId from MMSI or IMO
# ============================================================================
get_vessel_id <- function(identifier, 
                          search_by = "mmsi",  # "mmsi", "imo", or "name"
                          flag = NULL,
                          key = gfw_auth()) {
  
  message(sprintf("Looking up vessel: %s (by %s)", identifier, search_by))
  
  tryCatch({
    # Build the where clause based on search type
    where_clause <- switch(search_by,
                           "mmsi" = sprintf("ssvid = '%s'", identifier),
                           "imo" = sprintf("imo = '%s'", identifier),
                           "name" = sprintf("shipname LIKE '%%%s%%'", identifier),
                           stop("search_by must be 'mmsi', 'imo', or 'name'")
    )
    
    # Add flag filter if provided
    if (!is.null(flag)) {
      where_clause <- sprintf("%s AND flag = '%s'", where_clause, flag)
    }
    
    # Search for vessel
    vessel_info <- gfw_vessel_info(
      where = where_clause,
      search_type = "search",
      key = key
    )
    
    if (is.null(vessel_info) || nrow(vessel_info$selfReportedInfo) == 0) {
      warning(sprintf("No vessel found for %s: %s", search_by, identifier))
      return(NULL)
    }
    
    # Return all vesselIds and metadata
    result <- vessel_info$selfReportedInfo %>%
      select(vesselId, ssvid, shipname, flag, imo, 
             transmissionDateFrom, transmissionDateTo) %>%
      mutate(search_identifier = identifier,
             search_by = search_by)
    
    message(sprintf("  Found %d vesselId(s) for %s", nrow(result), identifier))
    
    return(result)
    
  }, error = function(e) {
    warning(sprintf("Error looking up %s %s: %s", search_by, identifier, e$message))
    return(NULL)
  })
}

# ============================================================================
# Function 2: Get events for a single vessel
# ============================================================================
get_vessel_events <- function(vessel_ids,
                              event_type = "FISHING",
                              start_date,
                              end_date,
                              identifier = NULL,
                              additional_params = list(),
                              key = gfw_auth()) {
  
  if (is.null(vessel_ids) || length(vessel_ids) == 0) {
    return(NULL)
  }
  
  id_label <- ifelse(is.null(identifier), 
                     paste(vessel_ids, collapse = ", "), 
                     identifier)
  
  message(sprintf("  Fetching %s events for %s (%s to %s)", 
                  event_type, id_label, start_date, end_date))
  
  tryCatch({
    # Base parameters
    params <- list(
      event_type = event_type,
      vessels = vessel_ids,
      start_date = start_date,
      end_date = end_date,
      key = key
    )
    
    # Add any additional parameters (e.g., confidences for PORT_VISIT)
    params <- c(params, additional_params)
    
    # Call gfw_event with parameters
    events <- do.call(gfw_event, params)
    
    if (!is.null(events) && nrow(events) > 0) {
      # Add identifier for tracking
      events$search_identifier <- id_label
      events$event_type_requested <- event_type
      
      message(sprintf("    Retrieved %d %s events", nrow(events), event_type))
      return(events)
    } else {
      message(sprintf("    No %s events found", event_type))
      return(NULL)
    }
    
  }, error = function(e) {
    warning(sprintf("Error fetching %s events for %s: %s", 
                    event_type, id_label, e$message))
    return(NULL)
  })
}

# ============================================================================
# Function 3: Extract multiple event types for multiple vessels
# ============================================================================
extract_gfw_events <- function(vessel_identifiers,
                               search_by = "mmsi",  # "mmsi", "imo", or "name"
                               event_types = c("FISHING", "PORT_VISIT"),
                               start_date,
                               end_date,
                               flag = NULL,
                               use_all_vessel_ids = TRUE,  # Use complete vessel history
                               additional_params = list(),  # Named list by event type
                               delay_seconds = 1,
                               key = gfw_auth()) {
  
  message(sprintf("\n=== Extracting GFW Events ==="))
  message(sprintf("Vessels: %d", length(vessel_identifiers)))
  message(sprintf("Event types: %s", paste(event_types, collapse = ", ")))
  message(sprintf("Date range: %s to %s", start_date, end_date))
  message(sprintf("Search by: %s", search_by))
  if (!is.null(flag)) message(sprintf("Flag filter: %s", flag))
  message("===============================\n")
  
  # Step 1: Get vessel IDs for all identifiers
  message("Step 1: Looking up vessel IDs...")
  
  vessel_lookup <- vessel_identifiers %>%
    map_df(function(id) {
      result <- get_vessel_id(id, search_by = search_by, flag = flag, key = key)
      Sys.sleep(delay_seconds * 0.5)  # Light rate limiting
      return(result)
    })
  
  if (is.null(vessel_lookup) || nrow(vessel_lookup) == 0) {
    stop("No vessels found. Check your identifiers and search_by parameter.")
  }
  
  message(sprintf("\nFound %d vessel identities across %d search terms\n", 
                  nrow(vessel_lookup), 
                  length(unique(vessel_lookup$search_identifier))))
  
  # Step 2: Organize vessel IDs by identifier
  vessel_groups <- vessel_lookup %>%
    group_by(search_identifier) %>%
    summarise(
      vessel_ids = list(vesselId),
      n_ids = n(),
      .groups = "drop"
    )
  
  # Step 3: Extract events for each vessel and event type
  message("Step 2: Extracting events...")
  
  all_events <- list()
  
  for (i in 1:nrow(vessel_groups)) {
    identifier <- vessel_groups$search_identifier[i]
    vessel_ids <- vessel_groups$vessel_ids[[i]]
    
    message(sprintf("\nProcessing vessel %d/%d: %s (%d vesselId(s))", 
                    i, nrow(vessel_groups), identifier, length(vessel_ids)))
    
    # Decide whether to use all vessel IDs or just the most recent
    if (use_all_vessel_ids) {
      ids_to_use <- vessel_ids
    } else {
      # Use only the most recent vessel ID
      most_recent <- vessel_lookup %>%
        filter(search_identifier == identifier) %>%
        arrange(desc(transmissionDateTo)) %>%
        slice(1)
      ids_to_use <- most_recent$vesselId
      message(sprintf("  Using most recent vesselId only"))
    }
    
    # Get events for each event type
    for (event_type in event_types) {
      # Get additional parameters for this event type if specified
      extra_params <- if (event_type %in% names(additional_params)) {
        additional_params[[event_type]]
      } else {
        list()
      }
      
      events <- get_vessel_events(
        vessel_ids = ids_to_use,
        event_type = event_type,
        start_date = start_date,
        end_date = end_date,
        identifier = identifier,
        additional_params = extra_params,
        key = key
      )
      
      if (!is.null(events)) {
        all_events[[length(all_events) + 1]] <- events
      }
      
      Sys.sleep(delay_seconds)  # Rate limiting
    }
  }
  
  # Step 4: Combine all events
  if (length(all_events) == 0) {
    message("\n=== No events found ===")
    return(NULL)
  }
  
  combined_events <- bind_rows(all_events)
  
  message(sprintf("\n=== Extraction Complete ==="))
  message(sprintf("Total events retrieved: %s", 
                  format(nrow(combined_events), big.mark = ",")))
  message(sprintf("Event type breakdown:"))
  
  event_summary <- combined_events %>%
    group_by(event_type_requested) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  for (i in 1:nrow(event_summary)) {
    message(sprintf("  - %s: %s", 
                    event_summary$event_type_requested[i],
                    format(event_summary$count[i], big.mark = ",")))
  }
  
  message("===========================\n")
  
  return(combined_events)
}

# ============================================================================
# Function 4: Helper function to save results
# ============================================================================
save_gfw_events <- function(events, 
                            output_dir = "gfw_data",
                            prefix = "events") {
  
  if (is.null(events) || nrow(events) == 0) {
    message("No events to save")
    return(invisible(NULL))
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate filename with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Save as RDS
  rds_file <- file.path(output_dir, sprintf("%s_%s.rds", prefix, timestamp))
  saveRDS(events, rds_file)
  message(sprintf("Saved RDS: %s", rds_file))
  
  # Save as CSV
  csv_file <- file.path(output_dir, sprintf("%s_%s.csv", prefix, timestamp))
  write.csv(events, csv_file, row.names = FALSE)
  message(sprintf("Saved CSV: %s", csv_file))
  
  # Save summary
  summary_file <- file.path(output_dir, sprintf("%s_%s_summary.txt", prefix, timestamp))
  
  summary_text <- paste(
    "=== GFW Events Extraction Summary ===",
    sprintf("Extraction date: %s", Sys.time()),
    sprintf("Total events: %s", format(nrow(events), big.mark = ",")),
    sprintf("Event types: %s", paste(unique(events$event_type_requested), collapse = ", ")),
    sprintf("Vessels: %s", paste(unique(events$search_identifier), collapse = ", ")),
    sprintf("Date range: %s to %s", min(events$start, na.rm = TRUE), max(events$end, na.rm = TRUE)),
    "\nEvent type breakdown:",
    paste(capture.output(table(events$event_type_requested)), collapse = "\n"),
    "=====================================",
    sep = "\n"
  )
  
  writeLines(summary_text, summary_file)
  message(sprintf("Saved summary: %s", summary_file))
  
  return(invisible(list(rds = rds_file, csv = csv_file, summary = summary_file)))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Example 1: Extract fishing and port visit events for multiple vessels by MMSI
vessel_mmsi <- c("244682000", "244630637", "246742000", "244938000", "244810000")

events <- extract_gfw_events(
  vessel_identifiers = vessel_mmsi,
  search_by = "mmsi",
  event_types = c("FISHING", "PORT_VISIT"),
  start_date = "2026-01-01",
  end_date = "2026-01-31",
  flag = NULL,  # No flag filter
  use_all_vessel_ids = TRUE,  # Use complete vessel history
  delay_seconds = 1
)

# Save results
save_gfw_events(events, output_dir = "gfw_data", prefix = "flyshoot_vessels")


# Example 2: Extract multiple event types with specific parameters
events_multi <- extract_gfw_events(
  vessel_identifiers = c("9917268"),  # IMO number
  search_by = "imo",
  event_types = c("FISHING", "PORT_VISIT", "ENCOUNTER", "GAP"),
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  flag = "NLD",  # Filter for Dutch-flagged identity
  use_all_vessel_ids = FALSE,  # Use only most recent identity
  additional_params = list(
    PORT_VISIT = list(confidences = c(3, 4))  # High confidence port visits only
  ),
  delay_seconds = 1
)


# Example 3: Extract for vessels by name
events_by_name <- extract_gfw_events(
  vessel_identifiers = c("ARAVIS", "SCOTIA"),
  search_by = "name",
  event_types = c("FISHING", "PORT_VISIT"),
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  delay_seconds = 1
)


# Example 4: Extract only fishing events for a fleet
fishing_only <- extract_gfw_events(
  vessel_identifiers = c("244682000", "244820483", "246326000"),
  search_by = "mmsi",
  event_types = c("FISHING"),
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  use_all_vessel_ids = TRUE
)

# Analyze fishing events
if (!is.null(fishing_only)) {
  library(ggplot2)
  
  # Summarize by month
  fishing_summary <- fishing_only %>%
    mutate(month = floor_date(as.Date(start), "month")) %>%
    group_by(search_identifier, month) %>%
    summarise(
      n_events = n(),
      .groups = "drop"
    )
  
  # Plot
  ggplot(fishing_summary, aes(x = month, y = n_events, color = search_identifier)) +
    geom_line() +
    geom_point() +
    labs(title = "Fishing Events Over Time",
         x = "Month",
         y = "Number of Fishing Events",
         color = "Vessel") +
    theme_minimal()
}



















# ============================================================================
# Optimized Function: Parallel event extraction
# ============================================================================

library(gfwr)
library(dplyr)
library(purrr)
library(furrr)  # For parallel processing
library(future)

extract_gfw_events_fast <- function(vessel_identifiers,
                                    search_by = "mmsi",
                                    event_types = c("FISHING", "PORT_VISIT"),
                                    start_date,
                                    end_date,
                                    flag = NULL,
                                    use_all_vessel_ids = TRUE,
                                    additional_params = list(),
                                    n_cores = 4,  # Number of parallel workers
                                    key = gfw_auth()) {
  
  message(sprintf("\n=== Fast GFW Events Extraction ==="))
  message(sprintf("Vessels: %d", length(vessel_identifiers)))
  message(sprintf("Event types: %s", paste(event_types, collapse = ", ")))
  message(sprintf("Parallel cores: %d", n_cores))
  message("==================================\n")
  
  # Step 1: Batch lookup all vessels (no delays needed)
  message("Step 1: Looking up vessel IDs (batched)...")
  
  vessel_lookup <- vessel_identifiers %>%
    map_df(function(id) {
      get_vessel_id(id, search_by = search_by, flag = flag, key = key)
    })
  
  if (is.null(vessel_lookup) || nrow(vessel_lookup) == 0) {
    stop("No vessels found.")
  }
  
  message(sprintf("Found %d vessel identities\n", nrow(vessel_lookup)))
  
  # Step 2: Prepare vessel groups
  vessel_groups <- vessel_lookup %>%
    group_by(search_identifier) %>%
    summarise(
      vessel_ids = list(if (use_all_vessel_ids) vesselId else vesselId[1]),
      .groups = "drop"
    )
  
  # Step 3: Create all combinations of vessels and event types
  message("Step 2: Preparing extraction tasks...")
  
  extraction_tasks <- expand.grid(
    vessel_idx = 1:nrow(vessel_groups),
    event_type = event_types,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      identifier = vessel_groups$search_identifier[vessel_idx],
      vessel_ids = vessel_groups$vessel_ids[vessel_idx]
    )
  
  message(sprintf("Total tasks: %d\n", nrow(extraction_tasks)))
  
  # Step 4: Set up parallel processing
  message("Step 3: Extracting events in parallel...")
  plan(multisession, workers = n_cores)
  
  # Parallel extraction
  all_events <- extraction_tasks %>%
    mutate(row_id = row_number()) %>%
    split(.$row_id) %>%
    future_map(function(task) {
      
      # Get additional parameters for this event type
      extra_params <- if (task$event_type %in% names(additional_params)) {
        additional_params[[task$event_type]]
      } else {
        list()
      }
      
      # Extract events
      tryCatch({
        params <- list(
          event_type = task$event_type,
          vessels = task$vessel_ids[[1]],
          start_date = start_date,
          end_date = end_date,
          key = key
        )
        params <- c(params, extra_params)
        
        events <- do.call(gfw_event, params)
        
        if (!is.null(events) && nrow(events) > 0) {
          events$search_identifier <- task$identifier
          events$event_type_requested <- task$event_type
          return(events)
        }
        return(NULL)
        
      }, error = function(e) {
        return(NULL)
      })
      
    }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  # Close parallel workers
  plan(sequential)
  
  # Step 5: Combine results
  all_events <- all_events %>%
    compact() %>%
    bind_rows()
  
  if (is.null(all_events) || nrow(all_events) == 0) {
    message("\n=== No events found ===")
    return(NULL)
  }
  
  # Summary
  message(sprintf("\n=== Extraction Complete ==="))
  message(sprintf("Total events: %s", format(nrow(all_events), big.mark = ",")))
  
  event_summary <- all_events %>%
    group_by(event_type_requested) %>%
    summarise(count = n(), .groups = "drop")
  
  for (i in 1:nrow(event_summary)) {
    message(sprintf("  - %s: %s", 
                    event_summary$event_type_requested[i],
                    format(event_summary$count[i], big.mark = ",")))
  }
  message("===========================\n")
  
  return(all_events)
}

# ============================================================================
# Even Faster: Batch vessels together in single API calls
# ============================================================================
extract_gfw_events_batched <- function(vessel_identifiers,
                                       search_by = "mmsi",
                                       event_types = c("FISHING", "PORT_VISIT"),
                                       start_date,
                                       end_date,
                                       flag = NULL,
                                       use_current_identity_only = TRUE,  # NEW: Only current MMSI
                                       batch_size = 50,
                                       additional_params = list(),
                                       key = gfw_auth()) {
  
  message("\n=== Batched GFW Events Extraction ===\n")
  
  # Step 1: Get all vessel IDs
  message("Step 1: Looking up vessel IDs...")
  
  vessel_lookup <- vessel_identifiers %>%
    map_df(function(id) {
      result <- get_vessel_id(id, search_by = search_by, flag = flag, key = key)
      if (!is.null(result)) {
        result$search_mmsi <- id  # Track which MMSI was searched
      }
      return(result)
    })
  
  if (is.null(vessel_lookup) || nrow(vessel_lookup) == 0) {
    stop("No vessels found.")
  }
  
  message(sprintf("Found %d vessel identities for %d MMSIs", 
                  nrow(vessel_lookup), 
                  length(unique(vessel_lookup$search_mmsi))))
  
  # Show what was found
  vessel_summary <- vessel_lookup %>%
    select(search_mmsi, ssvid, shipname, flag, vesselId) %>%
    arrange(search_mmsi, ssvid)
  
  print(vessel_summary)
  
  # Step 2: Filter to only matching identities if requested
  if (use_current_identity_only && search_by == "mmsi") {
    message("\nFiltering to exact MMSI matches only...")
    
    vessel_lookup_filtered <- vessel_lookup %>%
      filter(ssvid == search_mmsi)  # Only keep exact MMSI matches
    
    message(sprintf("Kept %d identities with exact MMSI match", 
                    nrow(vessel_lookup_filtered)))
    
    vessel_lookup <- vessel_lookup_filtered
  }
  
  # Get vessel IDs to use
  all_vessel_ids <- unique(vessel_lookup$vesselId)
  
  message(sprintf("\nUsing %d unique vesselIds\n", length(all_vessel_ids)))
  
  # Step 3: Split into batches
  n_batches <- ceiling(length(all_vessel_ids) / batch_size)
  vessel_batches <- split(all_vessel_ids, 
                          ceiling(seq_along(all_vessel_ids) / batch_size))
  
  message(sprintf("Processing %d batches of ~%d vessels each...\n", 
                  n_batches, batch_size))
  
  # Step 4: Extract events
  all_events <- list()
  
  for (event_type in event_types) {
    message(sprintf("Extracting %s events...", event_type))
    
    extra_params <- if (event_type %in% names(additional_params)) {
      additional_params[[event_type]]
    } else {
      list()
    }
    
    batch_results <- map(vessel_batches, function(batch) {
      params <- list(
        event_type = event_type,
        vessels = batch,
        start_date = start_date,
        end_date = end_date,
        key = key
      )
      params <- c(params, extra_params)
      
      events <- tryCatch({
        do.call(gfw_event, params)
      }, error = function(e) {
        warning(sprintf("Batch failed: %s", e$message))
        return(NULL)
      })
      
      return(events)
    })
    
    event_data <- bind_rows(batch_results)
    
    if (!is.null(event_data) && nrow(event_data) > 0) {
      event_data$event_type_requested <- event_type
      all_events[[length(all_events) + 1]] <- event_data
      message(sprintf("  Retrieved %s events", 
                      format(nrow(event_data), big.mark = ",")))
    }
  }
  
  if (length(all_events) == 0) {
    message("\n=== No events found ===")
    return(NULL)
  }
  
  combined <- bind_rows(all_events)
  
  # Add vessel identifier information with proper join
  combined <- combined %>%
    left_join(
      vessel_lookup %>% 
        select(vesselId, search_mmsi, ssvid, shipname, flag),
      by = "vesselId"
    )
  
  # Summary by original search MMSI
  message("\n=== Event Summary by Vessel ===")
  event_by_vessel <- combined %>%
    group_by(search_mmsi, ssvid, event_type_requested) %>%
    summarise(n_events = n(), .groups = "drop") %>%
    arrange(search_mmsi, event_type_requested)
  
  print(event_by_vessel)
  
  message(sprintf("\nTotal events: %s\n", format(nrow(combined), big.mark = ",")))
  
  return(combined)
}


# ============================================================================
# USAGE EXAMPLES
# ============================================================================

vessel_mmsi <- c("244682000", "244630637", "246742000", "244938000", "244810000")

# Option 1: Parallel processing (good for 10-50 vessels)
events_parallel <- extract_gfw_events_fast(
  vessel_identifiers = vessel_mmsi,
  search_by = "mmsi",
  event_types = c("FISHING", "PORT_VISIT"),
  start_date = "2026-01-01",
  end_date = "2026-01-31",
  n_cores = 12  # Adjust based on your computer
)

# Option 2: Batched processing (best for 50+ vessels)
events_batched <- extract_gfw_events_batched(
  vessel_identifiers = vessel_mmsi,
  search_by = "mmsi",
  event_types = c("FISHING", "PORT_VISIT"),
  start_date = "2026-01-01",
  end_date = "2026-01-31",
  batch_size = 50  # Process 50 vessels per API call
)

# Option 3: Just reduce delays in original function
events_fast <- extract_gfw_events(
  vessel_identifiers = vessel_mmsi,
  search_by = "mmsi",
  event_types = c("FISHING", "PORT_VISIT"),
  start_date = "2026-01-01",
  end_date = "2026-01-31",
  delay_seconds = 0.05  # Much faster!
)

# Summarize by month
fishing_summary <- 
  events_batched %>%
  mutate(month = floor_date(as.Date(start), "month")) %>%
  group_by(vessel_name, month) %>%
  summarise(
    n_events = n(),
    .groups = "drop"
  )

ggplot(fishing_summary, aes(x = month, y = n_events, color = vessel_name)) +
  geom_line() +
  geom_point() +
  labs(title = "Fishing Events Over Time",
       x = "Month",
       y = "Number of Fishing Events",
       color = "Vessel") +
  theme_minimal()


# Plot on the map
events_batched %>% 
  filter(!is.na(vessel_name) & vessel_name != "ARAVIS") %>% 
  
  ggplot(aes(x = lon, y = lat, color = eventType)) +
  geom_line() +
  geom_point() +
  labs(title = "Fishing Events Over Time",
       x = "Month",
       y = "Number of Fishing Events",
       color = "Vessel") +
  facet_wrap(~vessel_name) +
  theme_minimal()


# ============================================================================
# Trip Construction from GFW Events
# ============================================================================

construct_trips <- function(events) {
  
  message("Constructing trips from events...")
  
  # Step 1: Extract port visits and sort by vessel and time
  port_visits <- events %>%
    filter(event_type_requested == "PORT_VISIT") %>%
    arrange(search_mmsi, start) %>%
    select(search_mmsi, vesselId, start, end, lat, lon, 
           regions, eventId, event_type_requested)
  
  if (nrow(port_visits) == 0) {
    warning("No port visits found - cannot construct trips")
    return(NULL)
  }
  
  # Step 2: Create trips by pairing consecutive port visits
  trips <- port_visits %>%
    group_by(search_mmsi, vesselId) %>%
    mutate(
      trip_id = paste(search_mmsi, row_number(), sep = "_trip_"),
      port_departure = end,  # When vessel left port
      port_departure_lat = lat,
      port_departure_lon = lon,
      port_arrival = lead(start),  # When vessel arrived at next port
      port_arrival_lat = lead(lat),
      port_arrival_lon = lead(lon),
      port_departure_id = eventId,
      port_arrival_id = lead(eventId)
    ) %>%
    ungroup() %>%
    filter(!is.na(port_arrival)) %>%  # Remove last port visit (no return)
    select(trip_id, search_mmsi, vesselId,
           port_departure, port_departure_lat, port_departure_lon, port_departure_id,
           port_arrival, port_arrival_lat, port_arrival_lon, port_arrival_id) %>%
    mutate(
      trip_duration_hours = as.numeric(difftime(port_arrival, port_departure, units = "hours"))
    )
  
  message(sprintf("Created %d trips", nrow(trips)))
  
  # Step 3: Assign other events to trips
  other_events <- events %>%
    filter(event_type_requested != "PORT_VISIT") %>%
    select(search_mmsi, vesselId, event_type_requested, eventId, 
           start, end, lat, lon, everything())
  
  if (nrow(other_events) > 0) {
    # For each event, find which trip it belongs to
    events_with_trips <- other_events %>%
      left_join(
        trips %>% select(trip_id, search_mmsi, vesselId, 
                         port_departure, port_arrival),
        by = c("search_mmsi", "vesselId"),
        relationship = "many-to-many"
      ) %>%
      filter(
        start >= port_departure & start <= port_arrival
      ) %>%
      select(-port_departure, -port_arrival)
    
    message(sprintf("Assigned %d events to trips", nrow(events_with_trips)))
    
    # Summarize events by trip
    trip_summary <- events_with_trips %>%
      group_by(trip_id, event_type_requested) %>%
      summarise(
        n_events = n(),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = event_type_requested,
        values_from = n_events,
        values_fill = 0,
        names_prefix = "n_"
      )
    
    # Add summary to trips
    trips <- trips %>%
      left_join(trip_summary, by = "trip_id")
    
  } else {
    message("No other events to assign to trips")
  }
  
  # Step 4: Calculate trip statistics
  trips <- trips %>%
    mutate(
      across(starts_with("n_"), ~replace_na(.x, 0))
    )
  
  # Display summary
  message("\n=== Trip Summary ===")
  message(sprintf("Total trips: %d", nrow(trips)))
  message(sprintf("Average trip duration: %.1f hours", 
                  mean(trips$trip_duration_hours, na.rm = TRUE)))
  
  if ("n_FISHING" %in% names(trips)) {
    message(sprintf("Trips with fishing: %d", sum(trips$n_FISHING > 0)))
    message(sprintf("Average fishing events per trip: %.1f", 
                    mean(trips$n_FISHING, na.rm = TRUE)))
  }
  
  message("====================\n")
  
  return(list(
    trips = trips,
    events_with_trips = if (exists("events_with_trips")) events_with_trips else NULL
  ))
}

# ============================================================================
# Usage Example
# ============================================================================

# Construct trips
trip_data <- construct_trips(events_batched)

# Access the results
trips <- trip_data$trips
events_with_trips <- trip_data$events_with_trips

# Analyze trips
if (!is.null(trips)) {
  # View trip summary
  trips %>%
    select(trip_id, search_mmsi, trip_duration_hours, 
           starts_with("n_")) %>%
    print()
  
  # Find longest trips
  trips %>%
    arrange(desc(trip_duration_hours)) %>%
    head(10) %>%
    select(trip_id, search_mmsi, trip_duration_hours, 
           port_departure, port_arrival, n_FISHING)
  
  # Calculate trip-level statistics
  trip_stats <- trips %>%
    group_by(search_mmsi) %>%
    summarise(
      n_trips = n(),
      avg_duration_hours = mean(trip_duration_hours),
      total_fishing_events = sum(n_FISHING, na.rm = TRUE),
      trips_with_gaps = sum(n_GAP > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(trip_stats)
}

# Save trips
saveRDS(trip_data, "data/gfw_trips.rds")
write.csv(trips, "data/gfw_trips.csv", row.names = FALSE)
if (!is.null(events_with_trips)) {
  write.csv(events_with_trips, "data/gfw_events_by_trip.csv", row.names = FALSE)
}













# === OLD CODE, NO LONGER WORKING ==============================================

trawlers <- 
  gfwr::gfw_vessel_info(
  query = paste0("mmsi=",paste(r$mmsi, collapse=" OR mmsi=")),
  search_type = "advanced",
  dataset = "fishing_vessel",
  key = key
)

# Collapse vessel ids into a commas separated list to pass to Events API
trawler_ids <- paste0(trawlers$id, collapse = ',')

gfw <-
  get_event(event_type='fishing',
            vessel = trawler_ids,
            # start_date = "2012-01-01", end_date = "2023-12-31",
            start_date = "2023-01-01", end_date = as.Date(now()),
            key = key) %>%
  mutate(year = lubridate::year(start),
         month = lubridate::month(start),
         week  = lubridate::week(start),
         date = as.Date(start)) %>%
  unnest_wider(regions, names_repair = "unique") %>%
  unnest_wider(fao, names_sep = "_") %>%
  unnest_wider(eez, names_sep = "_") %>%
  unnest_wider(vessel, names_repair = "unique") %>%
  unnest_wider(event_info, names_repair = "unique") %>%
  unnest_wider(distances, names_repair = "unique") %>%
  dplyr::select(-mpa, -rfmo, -majorFao) %>%
  left_join(r, by=c("ssvid"="mmsi")) %>%
  arrange(vesselname, year, week, end) %>%
  group_by(vesselname, year, week) %>%
  mutate(haul = row_number()) %>%
  
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, stringsAsFactors = FALSE, remove = FALSE) %>%
  sf::st_join(., fao_sf_division, join = st_within) %>%
  sf::st_drop_geometry()

save(gfw, file = file.path(onedrive, "rdata", "gfw.RData"))
  
