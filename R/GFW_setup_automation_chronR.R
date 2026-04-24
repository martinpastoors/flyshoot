# ============================================================================
# Setup GFW Automation
# ============================================================================
# Run this script once to set up the weekly automated extraction
# ============================================================================

# Install required packages if needed
required_packages <- c("gfwr", "dplyr", "purrr", "lubridate", "tidyr", "cronR")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(cronR)

# Define paths (UPDATE THESE!)
script_path <- "/full/path/to/gfw_weekly_extraction.R"
log_path <- "/full/path/to/data/gfw_weekly/logs/cron.log"

# Create cron command
cmd <- cron_rscript(
  rscript = script_path,
  rscript_log = log_path,
  log_append = TRUE
)

# Schedule for every Thursday at 20:00
cron_add(
  command = cmd,
  frequency = "weekly",
  at = "20:00",
  days = "Thu",
  id = "gfw_weekly_extraction",
  description = "Weekly GFW data extraction with trip construction"
)

# Verify it was added
cat("Scheduled jobs:\n")
print(cron_ls())

cat("\n✓ Automation setup complete!\n")
cat(sprintf("Script will run every Thursday at 20:00\n"))
cat(sprintf("Check logs at: %s\n", log_path))