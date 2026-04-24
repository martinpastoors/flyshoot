# GFW sandbox

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

source(file.path(here::here(), "R/gfw_functions.R"))  # UPDATE THIS PATH
source(file.path(here::here(), "R/gfw_plotting.R"))  # UPDATE THIS PATH
source(file.path(here::here(), "R/gfw_digivloot_matching.R"))  # UPDATE THIS PATH

output_dir <- CONFIG$output_dir

# Load the complete historical dataset
trips  <- readRDS(file.path(output_dir, "gfw_trips_cumulative.rds"))
events <- readRDS(file.path(output_dir, "gfw_events_by_trip_cumulative.rds"))
digivloot_data <- loadRData(file.path(onedrive, "trip.RData")) %>% filter(year(date) == 2026)

plot_trips_dumbbell_compact(
  trips_data = trips,
  events_data = events,
  show_fishing_events=FALSE,
  line_size = 1.5
)

p1 <- plot_trips_comparison_dumbbell(
  gfw_trips = gfw_trips,
  digivloot_data = digivloot_data,
  vessel_mapping = vessel_mapping,
  show_trip_numbers = TRUE,
  line_size = 1.5
)


full_report <- run_full_diagnostic(
  gfw_trips = gfw_trips,
  digivloot_data = digivloot_data,
  vessel_mapping = vessel_mapping,
  time_tolerance_hours = 24  # Current tolerance
)


# Try overview (just trips, no events)
p_overview <- plot_trips_overview(
  trips_data = trips
)
print(p_overview)

# Check data span
range(events$start)
range(trips$port_departure)

summary(trips[, c("port_departure_lon", "port_departure_lat", 
                  "port_arrival_lon", "port_arrival_lat")])




p_overview <- plot_trips_overview(
  trips_data = trips,
  events_data = events
)
print(p_overview)

p_overview2 <- plot_trips_overview(
  trips_data = trips,
  events_data = events_with_trips  # Use this file!
)
print(p_overview2)

p1 <- plot_trips_map(
  trips_data = trips,
  events_data = events_with_trips,  # Use this file!
  facet_by_month = TRUE,
  facet_by_vessel = TRUE
)
print(p1)

# Save
ggsave("output/trips_map_faceted.png", p1, 
       width = 14, height = 10, dpi = 300)


# Example 2: Simple overview without faceting
p2 <- plot_trips_simple(
  trips_data = trips,
  events_data = events,
  region = "north_sea"  # Or "auto", "europe"
)

print(p2)

p3 <- plot_vessel_trips_timeline(
  trips_data = trips,
  vessel_name = "SCH-99 ARAVIS",
  events_data = events_with_trips  # Use this file!
)
print(p3)



tasks <- taskscheduler_ls()
print(tasks)

"GFW_Weekly_Extraction" %in% tasks$TaskName

task_details <- tasks %>%
  filter(TaskName == "GFW_Weekly_Extraction")

print(task_details)

source(file.path(here::here(), "R/gfw_weekly_extraction.R"))






ggsave("output/trips_overview.png", p2, 
       width = 10, height = 8, dpi = 300)


# Example 3: Single vessel timeline
p3 <- plot_vessel_trips_timeline(
  trips_data = trips,
  vessel_name = "SCH-99 ARAVIS",
  events_data = events
)

print(p3)
ggsave("output/aravis_trips_timeline.png", p3, 
       width = 12, height = 8, dpi = 300)


# Example 4: Filter to specific time period
trips_jan <- trips %>%
  filter(port_departure >= "2026-01-01", 
         port_departure < "2026-02-01")

p4 <- plot_trips_map(
  trips_data = trips_jan,
  region = "auto",
  facet_by_month = FALSE,
  facet_by_vessel = TRUE
)

print(p4)


# Example 5: Focus on specific region
p5 <- plot_trips_map(
  trips_data = trips,
  region = c(xmin = 2, xmax = 8, ymin = 51, ymax = 56),  # Custom North Sea area
  facet_by_month = TRUE,
  facet_by_vessel = TRUE
)

print(p5)
