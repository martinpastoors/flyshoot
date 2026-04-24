# ============================================================================
# Setup GFW Automation using taskscheduleR (Windows)
# ============================================================================
# Run this script once to set up the weekly automated extraction
# ============================================================================

# Install required packages if needed
required_packages <- c("gfwr", "dplyr", "purrr", "lubridate", "tidyr", 
                       "jsonlite", "taskscheduleR")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(taskscheduleR)

# Define paths (UPDATE THESE!)
script_path <- "C:/GIT/flyshoot/R/GFW_weekly_extraction.R"
log_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/GFW/logs" 

# Create log directory if it doesn't exist
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

# Schedule the task to run every Thursday at 20:00
taskscheduler_create(
  taskname = "GFW_Weekly_Extraction",
  rscript = script_path,
  schedule = "WEEKLY",
  starttime = "08:00",
  days = "FRI",
  rscript_args = ""  # No additional arguments needed
)

# Verify the task was created
cat("\n✓ Task scheduled successfully!\n")

# View tasks
taskscheduler_ls() %>% View()

cat("Delete this task:    taskscheduler_delete('GFW_Weekly_Extraction')\n")

# Instructions for managing the task
cat("\n========================================\n")
cat("Task Management Commands:\n")
cat("========================================\n")
cat("View all tasks:      taskscheduler_ls()\n")
cat("Delete this task:    taskscheduler_delete('GFW_Weekly_Extraction')\n")
cat("Run task now:        taskscheduler_runnow('GFW_Weekly_Extraction')\n")
cat("Stop running task:   taskscheduler_stop('GFW_Weekly_Extraction')\n")
cat("========================================\n")

taskscheduler_delete('GFW_Weekly_Extraction')
