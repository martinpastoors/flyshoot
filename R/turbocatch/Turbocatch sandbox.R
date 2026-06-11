source(file.path(here::here(), "R/turbocatch/parse_ers.R"))
source(file.path(here::here(), "R/turbocatch/04_turbocatch_adapter.R"))
source(file.path(here::here(), "R/turbocatch/05_turbocatch_to_parquet.R"))

write_turbocatch_parquet(
  xml_folder = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC545762",
  output_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC545762/parquet", 
  overwrite  = TRUE
)

write_turbocatch_parquet(
  xml_folder = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC622598",
  output_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC622598/parquet", 
  overwrite  = TRUE
)

source(file.path(here::here(), "R/turbocatch/06_turbocatch_diagnostics.R"))
source(file.path(here::here(), "R/turbocatch/07_render_turbocatch_tripreport.R"))

turbo_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC545762/2026/parquet"

turbocatch_diagnostics(
  turbo_dir
)

output_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC545762/2026/output"

source(file.path(here::here(), "R/turbocatch/08_turbocatch_trip_map.R"))

# Print to screen
turbocatch_trip_map(
  parquet_dir = turbo_dir,
  trip_id     = "20260641"
)

# Or save to PDF with your existing spatial data
turbocatch_trip_map(
  parquet_dir = turbo_dir,
  trip_id     = "20260641",
  output_file = file.path(output_dir, "CC545762_20260641_map.pdf"),
  spatial_dir = "C:/Users/MartinPastoors/DATA/RDATA"
)

library(arrow)
library(dplyr)

read_parquet(file.path(turbo_dir, "turbocatch_haul.parquet")) %>%
  filter(trip_id == "20260641") %>%
  select(haul_id, date, shoot_time, ices_rect, n_shots, gear_type) %>%
  arrange(haul_id) %>%
  print(n = Inf)



source(file.path(here(), "R/turbocatch/check_far_frequency.R"))
turbo_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC545762/parquet"
check_far_frequency(turbo_dir)
turbo_dir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/CC622598/parquet"
check_far_frequency(turbo_dir)


source(file.path(here(), "R/turbocatch/parse_ers.R"))
source(file.path(here(), "R/turbocatch/04_turbocatch_adapter.R"))
source(file.path(here(), "R/turbocatch/09_integrate_turbocatch_historic.R"))

# Dry run — inspect counts, no files written
result <- integrate_turbocatch_historic(dry_run = TRUE)

# If counts look right, write for real
integrate_turbocatch_historic()





source(file.path(here(), "R/harmonized", "01_flyshoot_functions.R"))
source(file.path(here(), "R/harmonized/parse_ers.R"))
source(file.path(here(), "R/harmonized/09_integrate_turbocatch_historic.R"))

# Step 1 — dry run to check counts
result <- process_turbocatch_to_parquet(dry_run = TRUE)

# Step 2 — write to separate folder
process_turbocatch_to_parquet()
# Writes to: .../FLYSHOOT - General/tripdata/turbocatch/
#   haul.parquet, elog_trek.parquet, elog.parquet, trip.parquet

# Step 3 — inspect
tc_trip <- read_parquet(file.path(TURBOCATCH_PARQUET_DIR, "trip.parquet"))
View(tc_trip)

tc_elog <- read_parquet(file.path(TURBOCATCH_PARQUET_DIR, "elog.parquet"))
View(tc_elog)

tc_elog_trek <- read_parquet(file.path(TURBOCATCH_PARQUET_DIR, "elog_trek.parquet"))
View(tc_elog_trek)

tc_haul <- read_parquet(file.path(TURBOCATCH_PARQUET_DIR, "haul.parquet"))
View(tc_haul)


# Step 4 — when satisfied
integrate_turbocatch_to_pipeline()


# Print to screen
source(file.path(here(), "R/turbocatch/08_turbocatch_trip_map.R"))


