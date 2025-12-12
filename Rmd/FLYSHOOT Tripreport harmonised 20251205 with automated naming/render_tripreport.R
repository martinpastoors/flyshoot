# ==========================================================================================================
# Render FLYSHOOT Tripreport with Dynamic Naming
# 
# This script renders a tripreport with automatically generated filename and directory structure
# based on vessel, year, and week information.
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

# Vessel and date range
setvessel <- "SCH135"
startdate <- dmy("01/12/2025")
enddate   <- dmy("04/12/2025")

# Output directory base (where to store all reports)
output_base_dir <- "reports"

# RMarkdown input file
rmd_file <- "FLYSHOOT_tripreport_harmonized.Rmd"

# ==========================================================================================================
# LOAD DATA TO DETERMINE TRIP DETAILS
# ==========================================================================================================

cat("Loading data to determine trip details...\n")

# Set OneDrive directory
onedrive <- get_onedrive(team="Martin Pastoors", site="FLYSHOOT - General/rdata")

# Load elog data
load(file.path(onedrive, "elog.RData"))

# Get trip details for filename
trip_info <- elog %>%
  filter(vessel == setvessel, 
         catchdate >= startdate, 
         catchdate <= enddate) %>%
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

# Check if any data found
if(trip_info$ntrips == 0) {
  stop(sprintf("No trips found for vessel %s between %s and %s", 
               setvessel, 
               format(startdate, '%d/%m/%Y'),
               format(enddate, '%d/%m/%Y')))
}

# Display trip info
cat(sprintf("\nFound %d trip(s) for %s:\n", trip_info$ntrips, setvessel))
cat(sprintf("  Period: %s to %s\n", 
            format(trip_info$startdate, '%d/%m/%Y'),
            format(trip_info$enddate, '%d/%m/%Y')))
cat(sprintf("  Year: %d, Week: %d\n", trip_info$year, trip_info$week))

# ==========================================================================================================
# CREATE DYNAMIC FILENAME
# ==========================================================================================================

# Generate filename based on vessel, year, week, and number of trips
output_filename <- sprintf(
  "FLYSHOOT_%s_%dW%02d_%dtrip%s.docx",
  setvessel,
  trip_info$year,
  trip_info$week,
  trip_info$ntrips,
  ifelse(trip_info$ntrips > 1, "s", "")  # Pluralize if needed
)

cat(sprintf("\nOutput filename: %s\n", output_filename))

# ==========================================================================================================
# CREATE DYNAMIC DIRECTORY STRUCTURE
# ==========================================================================================================

# Create directory structure: reports/YYYY/WWW/
output_dir <- file.path(
  output_base_dir,
  as.character(trip_info$year),
  sprintf("W%02d", trip_info$week)
)

# Create directory if it doesn't exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(sprintf("Created directory: %s\n", output_dir))
} else {
  cat(sprintf("Using existing directory: %s\n", output_dir))
}

# Full output path
output_path <- file.path(output_dir, output_filename)

# ==========================================================================================================
# RENDER THE DOCUMENT
# ==========================================================================================================

cat("\nRendering report...\n")
cat("-----------------------------------\n")

start_time <- Sys.time()

tryCatch({
  
  rmarkdown::render(
    input = rmd_file,
    output_file = output_path,
    params = list(
      vessel = setvessel,
      startdate = startdate,
      enddate = enddate
    ),
    envir = new.env()  # Use clean environment
  )
  
  end_time <- Sys.time()
  elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)
  
  cat("-----------------------------------\n")
  cat("✓ SUCCESS!\n")
  cat(sprintf("  Report saved to: %s\n", output_path))
  cat(sprintf("  Time elapsed: %s seconds\n", elapsed))
  cat(sprintf("  File size: %s KB\n", 
              round(file.info(output_path)$size / 1024, 1)))
  
  # Open file (Windows only)
  if(.Platform$OS.type == "windows") {
    shell.exec(output_path)
  }
  
}, error = function(e) {
  cat("-----------------------------------\n")
  cat("✗ ERROR!\n")
  cat(sprintf("  Failed to render report: %s\n", e$message))
  cat("\nTroubleshooting tips:\n")
  cat("  1. Check that the RMarkdown file exists: ", rmd_file, "\n")
  cat("  2. Verify data files are accessible in OneDrive\n")
  cat("  3. Check for errors in the RMarkdown code\n")
  cat("  4. Ensure all required R packages are installed\n")
})

# ==========================================================================================================
# SUMMARY
# ==========================================================================================================

cat("\n==========================================\n")
cat("RENDER SUMMARY\n")
cat("==========================================\n")
cat(sprintf("Vessel:       %s\n", setvessel))
cat(sprintf("Date range:   %s to %s\n", 
            format(startdate, '%d/%m/%Y'),
            format(enddate, '%d/%m/%Y')))
cat(sprintf("Trips found:  %d\n", trip_info$ntrips))
cat(sprintf("Output file:  %s\n", output_filename))
cat(sprintf("Output path:  %s\n", output_path))
cat("==========================================\n")
