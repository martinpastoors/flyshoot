# 1. Save the three files:
#    - gfw_functions.R
#    - gfw_weekly_extraction.R  
#    - setup_automation.R

# 2. Update paths in all files (search for "UPDATE THIS PATH")

# 3. Test manually first:
source(file.path(here::here(),"R/GFW_functions.R"))

test_result <- 
  extract_gfw_data_with_trips(
  vessel_identifiers = c("244682000", "244630637", "246742000", "244938000", "244810000"),
  event_types = c("FISHING", "PORT_VISIT"),
  start_date = "2026-01-01",
  end_date = "2026-01-31",
  flag = "NLD",
  output_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/GFW",
  save_results = TRUE
)

# Check the results
head(test_result$trips)

# 4. Once working, set up automation:
source(file.path(here::here(), "R/GFW_setup_automation.R"))