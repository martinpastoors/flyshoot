# ============================================================================
# GFW Weekly Extraction Script
# ============================================================================
# Purpose: Automated weekly extraction of GFW data with trip construction
# Schedule: Every Thursday at 20:00
# ============================================================================

# Load functions
source(file.path(here::here(), "R/gfw_functions.R"))  # UPDATE THIS PATH
source(file.path(here::here(), "R/gfw_plotting.R"))  # UPDATE THIS PATH

# Configuration
CONFIG <- list(
  vessel_mmsi = c("244682000", "244630637", "246742000", "244938000", "244810000"),
  event_types = c("FISHING", "PORT_VISIT", "ENCOUNTER", "GAP"),
  output_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/GFW",  # UPDATE THIS PATH
  lookback_days = 14,  # Extract data for last 2 weeks
  flag = "NLD",
  batch_size =50
)

# Create log file
log_file <- file.path(CONFIG$output_dir, "logs", 
                      sprintf("extraction_log_%s.txt", format(Sys.Date(), "%Y%m")))

# Ensure log directory exists
log_dir <- dirname(log_file)
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

# Start logging
sink(log_file, append = TRUE, split = TRUE)

cat(sprintf("\n========== GFW Weekly Extraction ==========\n"))
cat(sprintf("Run time: %s\n", Sys.time()))

# Calculate date range
end_date <- Sys.Date()   # Yesterday
start_date <- end_date - CONFIG$lookback_days

cat(sprintf("Date range: %s to %s\n", start_date, end_date))
cat(sprintf("Vessels: %d\n", length(CONFIG$vessel_mmsi)))

# Run extraction
tryCatch({
  
  result <- extract_gfw_data_with_trips(
    vessel_identifiers = CONFIG$vessel_mmsi,
    event_types = CONFIG$event_types,
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    flag = CONFIG$flag,
    output_dir = CONFIG$output_dir,
    save_results = TRUE, 
    update_cumulative = TRUE,
    batch_size = CONFIG$batch_size 
  )
  
  if (!is.null(result)) {
    cat(sprintf("\nStatus: SUCCESS\n"))
    cat(sprintf("Events extracted: %d\n", nrow(result$events)))
    if (!is.null(result$trips)) {
      cat(sprintf("Trips constructed: %d\n", nrow(result$trips)))
      cat(sprintf("Most recent date: %s\n", as.Date(max(result$trips$port_arrival))))
    }
  } else {
    cat(sprintf("\nStatus: WARNING - No data extracted\n"))
  }
  
}, error = function(e) {
  cat(sprintf("\nStatus: ERROR\n"))
  cat(sprintf("Error message: %s\n", e$message))
  cat(sprintf("Traceback:\n"))
  print(traceback())
})

cat(sprintf("End time: %s\n", Sys.time()))
cat("===========================================\n\n")

# Stop logging
sink()


# Plot the available trips during the last month

# readRDS(file.path(CONFIG$output_dir, "gfw_trips_cumulative.rds")) %>% 
#   filter(as.Date(port_arrival) >= (Sys.Date()-30)) %>% 
#   plot_trips_dumbbell_compact(trips_data = .)




