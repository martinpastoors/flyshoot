# ============================================================================
# Dumbbell Plot Examples with Fishing Events
# ============================================================================

library(dplyr)
library(ggplot2)
library(ggalt)
library(lubridate)

# Load functions and data
source("R/gfw_functions.R")

# Load cumulative data
trips <- readRDS("data/gfw_weekly/gfw_trips_cumulative.rds")
events <- readRDS("data/gfw_weekly/gfw_events_by_trip_cumulative.rds")

# ============================================================================
# Example 1: Compact Overview WITH Fishing Events (RECOMMENDED)
# ============================================================================

p1 <- plot_trips_dumbbell_compact(
  trips_data = trips,
  events_data = events,         # Add events data
  show_fishing_events = TRUE,   # Show fishing events (default)
  show_trip_numbers = TRUE
)

ggsave("trips_with_fishing.png", p1, 
       width = 14, height = 6, dpi = 300)

# ============================================================================
# Example 2: Compact WITHOUT Fishing Events
# ============================================================================

p2 <- plot_trips_dumbbell_compact(
  trips_data = trips,
  events_data = events,
  show_fishing_events = FALSE,  # Hide fishing events
  show_trip_numbers = TRUE
)

ggsave("trips_no_fishing.png", p2,
       width = 14, height = 6, dpi = 300)

# ============================================================================
# Example 3: Recent Trips with Fishing Activity
# ============================================================================

p3 <- plot_trips_dumbbell_compact(
  trips_data = trips,
  events_data = events,
  show_fishing_events = TRUE,
  date_range = c(ymd("2026-01-01"), ymd("2026-02-28")),
  show_trip_numbers = TRUE
)

ggsave("recent_trips_fishing.png", p3,
       width = 12, height = 6, dpi = 300)

# ============================================================================
# Example 4: Detailed View with Fishing Events
# ============================================================================

p4 <- plot_trips_dumbbell_detailed(
  trips_data = trips,
  events_data = events,          # Add events data
  show_fishing_events = TRUE,    # Show fishing events
  vessel_filter = c("SCH-99 ARAVIS", "SCH-6 HENDRIKA BARTELDS"),
  color_by_duration = FALSE
)

ggsave("detailed_with_fishing.png", p4,
       width = 12, height = 8, dpi = 300)

# ============================================================================
# Example 5: Faceted View with Fishing Events
# ============================================================================

p5 <- plot_trips_dumbbell(
  trips_data = trips,
  events_data = events,          # Add events data
  show_fishing_events = TRUE,    # Show fishing events
  facet_by_month = TRUE,
  show_trip_numbers = TRUE
)

ggsave("faceted_with_fishing.png", p5,
       width = 14, height = 10, dpi = 300)

# ============================================================================
# Example 6: Analyze Fishing Intensity
# ============================================================================

# Count fishing events per trip
fishing_summary <- events %>%
  filter(event_type_requested == "FISHING") %>%
  group_by(trip_id, vessel_name) %>%
  summarise(
    n_fishing_events = n(),
    total_fishing_hours = sum(as.numeric(difftime(end, start, units = "hours")), na.rm = TRUE),
    .groups = "drop"
  )

# Join with trips
trips_fishing <- trips %>%
  left_join(fishing_summary, by = c("trip_id", "vessel_name")) %>%
  mutate(
    n_fishing_events = replace_na(n_fishing_events, 0),
    total_fishing_hours = replace_na(total_fishing_hours, 0)
  )

# Plot trips colored by fishing intensity
print(trips_fishing %>% select(trip_id, vessel_name, n_fishing_events, total_fishing_hours))

# Plot
p6 <- plot_trips_dumbbell_compact(
  trips_data = trips_fishing,
  events_data = events,
  show_fishing_events = TRUE,
  show_trip_numbers = TRUE
)

# Add custom title
p6 <- p6 + labs(
  title = "Fishing Trips with Activity Indicators",
  subtitle = sprintf("%d trips | Green lines = Fishing events | Blue = Departure, Red = Arrival",
                     nrow(trips))
)

ggsave("fishing_intensity.png", p6,
       width = 14, height = 6, dpi = 300)

# ============================================================================
# Example 7: Compare Vessels' Fishing Patterns
# ============================================================================

# Filter to specific date range
recent <- trips %>%
  filter(port_departure >= ymd("2026-01-01"))

recent_events <- events %>%
  filter(trip_id %in% recent$trip_id)

p7 <- plot_trips_dumbbell_compact(
  trips_data = recent,
  events_data = recent_events,
  show_fishing_events = TRUE
)

ggsave("vessel_comparison_fishing.png", p7,
       width = 12, height = 6, dpi = 300)

# ============================================================================
# Example 8: Single Vessel Deep Dive
# ============================================================================

# Pick one vessel
vessel <- "SCH-99 ARAVIS"

vessel_trips <- trips %>%
  filter(vessel_name == vessel)

vessel_events <- events %>%
  filter(trip_id %in% vessel_trips$trip_id)

p8 <- plot_trips_dumbbell_detailed(
  trips_data = vessel_trips,
  events_data = vessel_events,
  show_fishing_events = TRUE,
  color_by_duration = TRUE  # Color by trip duration
)

ggsave(sprintf("fishing_detail_%s.png", gsub(" ", "_", vessel)), p8,
       width = 10, height = 8, dpi = 300)

# ============================================================================
# Example 9: Filter to Only Trips with Fishing
# ============================================================================

# Get trip IDs that have fishing events
trips_with_fishing <- events %>%
  filter(event_type_requested == "FISHING") %>%
  pull(trip_id) %>%
  unique()

# Filter trips
fishing_trips <- trips %>%
  filter(trip_id %in% trips_with_fishing)

p9 <- plot_trips_dumbbell_compact(
  trips_data = fishing_trips,
  events_data = events,
  show_fishing_events = TRUE
)

p9 <- p9 + labs(
  title = "Trips with Fishing Activity Only",
  subtitle = sprintf("%d trips had fishing events", nrow(fishing_trips))
)

ggsave("only_fishing_trips.png", p9,
       width = 14, height = 6, dpi = 300)

# ============================================================================
# Example 10: Compare Event Types
# ============================================================================

# Count events by type per trip
event_summary <- events %>%
  group_by(trip_id, event_type_requested) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = event_type_requested,
    values_from = n,
    values_fill = 0,
    names_prefix = "n_"
  )

# Join with trips
trips_events <- trips %>%
  left_join(event_summary, by = "trip_id")

# View summary
print(trips_events %>% 
  select(trip_id, vessel_name, starts_with("n_")) %>%
  arrange(desc(n_FISHING)))

# Plot
p10 <- plot_trips_dumbbell_compact(
  trips_data = trips,
  events_data = events,
  show_fishing_events = TRUE
)

# ============================================================================
# Visual Legend
# ============================================================================

# The plots show:
# - Blue points (●) = Departure from port
# - Red points (●) = Arrival at port
# - Grey line (─) = Time at sea
# - Green vertical lines (|) = Fishing events
# - Numbers = Trip number (last 3 digits)

# ============================================================================
# Tips for Using Fishing Events
# ============================================================================

# 1. Always load BOTH trips and events data:
#    trips <- readRDS("data/gfw_weekly/gfw_trips_cumulative.rds")
#    events <- readRDS("data/gfw_weekly/gfw_events_by_trip_cumulative.rds")

# 2. Events data must have trip_id column (from events_by_trip file)

# 3. To hide fishing events:
#    plot_trips_dumbbell_compact(..., show_fishing_events = FALSE)

# 4. Fishing events are shown as thin green vertical lines

# 5. The function automatically filters to only FISHING event types

# 6. If no events data provided, plots work normally without fishing indicators

# 7. Each green line represents one fishing event start time

# 8. Multiple fishing events in a trip show as multiple green lines

# 9. Use detailed view to see exact timing of fishing within each trip

# 10. Combine with trip statistics to understand fishing intensity
