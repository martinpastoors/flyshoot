# ==========================================================================================================
# Batch Render FLYSHOOT Tripreports
# 
# This script renders tripreports for multiple vessels with automatically generated filenames
# and directory structures.
#
# Features:
# - Process multiple vessels in one run
# - Automatic trip discovery based on date range
# - Skip vessels with no data
# - Error handling for individual vessels
# - Summary statistics at end
#
# Output structure: reports/YYYY/WWW/FLYSHOOT_VESSEL_YYYYWWW_Ntrips.docx
# ==========================================================================================================

library(rmarkdown)
library(tidyverse)
library(lubridate)

# Source the utility functions
source("r/FLYSHOOT utils.R")

# ==========================================================================================================
# CONFIGURATION
# ==========================================================================================================

# Vessels to process
vessels <- c("SCH135", "SCH144", "SCH65", "SL9")

# Date range (same for all vessels)
startdate <- dmy("01/12/2025")
enddate   <- dmy("07/12/2025")

# Alternatively, process last week automatically:
# enddate <- Sys.Date()
# startdate <- enddate - days(7)

# Output directory base
output_base_dir <- "reports"

# RMarkdown input file
rmd_file <- "FLYSHOOT_tripreport_harmonized.Rmd"

# Processing options
open_reports <- FALSE  # Open each report after rendering (Windows only)
quiet_mode <- TRUE     # Suppress RMarkdown rendering messages

# ==========================================================================================================
# LOAD DATA
# ==========================================================================================================

cat("==========================================\n")
cat("BATCH RENDER FLYSHOOT TRIPREPORTS\n")
cat("==========================================\n")
cat(sprintf("Date range:   %s to %s\n", 
            format(startdate, '%d/%m/%Y'),
            format(enddate, '%d/%m/%Y')))
cat(sprintf("Vessels:      %s\n", paste(vessels, collapse=", ")))
cat("==========================================\n\n")

cat("Loading data...\n")

# Set OneDrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")

# Load elog data
load(file.path(onedrive, "elog.RData"))

cat("Data loaded successfully.\n\n")

# ==========================================================================================================
# FUNCTION TO RENDER REPORT FOR ONE VESSEL
# ==========================================================================================================

render_vessel_report <- function(vessel_name, start_date, end_date) {
  
  cat(sprintf("Processing %s...\n", vessel_name))
  
  # Get trip info
  trip_info <- elog %>%
    filter(vessel == vessel_name, 
           catchdate >= start_date, 
           catchdate <= end_date) %>%
    summarise(
      trips = paste(unique(trip), collapse="_"),
      ntrips = n_distinct(trip),
      year = year(min(catchdate, na.rm = TRUE)),
      week = week(min(catchdate, na.rm = TRUE)),
      month = month(min(catchdate, na.rm = TRUE)),
      startdate = min(catchdate, na.rm = TRUE),
      enddate = max(catchdate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Skip if no data
  if(trip_info$ntrips == 0) {
    cat(sprintf("  ⊘ No trips found for %s - skipping\n\n", vessel_name))
    return(list(
      vessel = vessel_name,
      status = "skipped",
      reason = "No trips found",
      path = NA
    ))
  }
  
  cat(sprintf("  Found %d trip(s) in week %d\n", 
              trip_info$ntrips, trip_info$week))
  
  # Create filename
  output_filename <- sprintf(
    "FLYSHOOT_%s_%dW%02d_%dtrip%s.docx",
    vessel_name,
    trip_info$year,
    trip_info$week,
    trip_info$ntrips,
    ifelse(trip_info$ntrips > 1, "s", "")
  )
  
  # Create directory structure: reports/YYYY/WWW/
  output_dir <- file.path(
    output_base_dir,
    as.character(trip_info$year),
    sprintf("W%02d", trip_info$week)
  )
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_path <- file.path(output_dir, output_filename)
  
  cat(sprintf("  Rendering to: %s\n", output_filename))
  
  # Render
  start_time <- Sys.time()
  
  result <- tryCatch({
    
    rmarkdown::render(
      input = rmd_file,
      output_file = output_path,
      params = list(
        vessel = vessel_name,
        startdate = start_date,
        enddate = end_date
      ),
      quiet = quiet_mode,
      envir = new.env()
    )
    
    elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
    file_size <- round(file.info(output_path)$size / 1024, 1)
    
    cat(sprintf("  ✓ Success! (%ss, %s KB)\n\n", elapsed, file_size))
    
    # Open file if requested (Windows only)
    if(open_reports && .Platform$OS.type == "windows") {
      shell.exec(output_path)
    }
    
    list(
      vessel = vessel_name,
      status = "success",
      path = output_path,
      trips = trip_info$ntrips,
      elapsed = elapsed,
      size_kb = file_size
    )
    
  }, error = function(e) {
    cat(sprintf("  ✗ Error: %s\n\n", e$message))
    
    list(
      vessel = vessel_name,
      status = "error",
      reason = e$message,
      path = NA
    )
  })
  
  return(result)
}

# ==========================================================================================================
# PROCESS ALL VESSELS
# ==========================================================================================================

cat("Starting batch processing...\n")
cat("------------------------------------------\n\n")

overall_start <- Sys.time()

# Process each vessel
results <- map(vessels, ~render_vessel_report(.x, startdate, enddate))

overall_elapsed <- round(as.numeric(difftime(Sys.time(), overall_start, units = "secs")), 1)

# ==========================================================================================================
# SUMMARY REPORT
# ==========================================================================================================

cat("\n==========================================\n")
cat("BATCH PROCESSING SUMMARY\n")
cat("==========================================\n\n")

# Convert results to tibble for easier analysis
results_df <- results %>%
  map_df(~as_tibble(.))

# Count outcomes
n_success <- sum(results_df$status == "success")
n_skipped <- sum(results_df$status == "skipped")
n_errors <- sum(results_df$status == "error")

cat(sprintf("Total vessels processed: %d\n", length(vessels)))
cat(sprintf("  ✓ Successful:          %d\n", n_success))
cat(sprintf("  ⊘ Skipped (no data):   %d\n", n_skipped))
cat(sprintf("  ✗ Errors:              %d\n", n_errors))
cat(sprintf("\nTotal time:              %ss\n", overall_elapsed))

if(n_success > 0) {
  avg_time <- mean(results_df$elapsed[results_df$status == "success"], na.rm = TRUE)
  total_size <- sum(results_df$size_kb[results_df$status == "success"], na.rm = TRUE)
  cat(sprintf("Average render time:     %ss\n", round(avg_time, 1)))
  cat(sprintf("Total output size:       %s KB\n", round(total_size, 1)))
}

# Detailed results table
cat("\n------------------------------------------\n")
cat("DETAILED RESULTS\n")
cat("------------------------------------------\n\n")

results_df %>%
  select(vessel, status, path) %>%
  mutate(
    path = ifelse(is.na(path), "-", basename(path))
  ) %>%
  knitr::kable(format = "simple") %>%
  print()

# List of successful reports
if(n_success > 0) {
  cat("\n------------------------------------------\n")
  cat("GENERATED REPORTS\n")
  cat("------------------------------------------\n\n")
  
  results_df %>%
    filter(status == "success") %>%
    pull(path) %>%
    walk(~cat(sprintf("  %s\n", .x)))
}

# List errors with details
if(n_errors > 0) {
  cat("\n------------------------------------------\n")
  cat("ERRORS\n")
  cat("------------------------------------------\n\n")
  
  results_df %>%
    filter(status == "error") %>%
    select(vessel, reason) %>%
    walk2(function(v, r) {
      cat(sprintf("  %s: %s\n", v, r))
    })
}

cat("\n==========================================\n")
cat("Batch processing complete!\n")
cat("==========================================\n")

# Return results invisibly for further processing if needed
invisible(results_df)
