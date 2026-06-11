# =============================================================================
# Extract vessels and MMSI numbers active in ICES divisions 27.4.c,
# 27.7.d and 27.7.e using the gfwr package
#
# ICES divisions:
#   27.4.c  =  Southern North Sea
#   27.7.d  =  Irish Sea
#   27.7.e  =  Eastern Celtic Sea
#
# Requirements:
#   install.packages(c("gfwr", "sf", "dplyr", "lubridate"))
#   A valid GFW API token set as environment variable GFW_TOKEN
#   (set via: Sys.setenv(GFW_TOKEN = "your_token_here")
#    or add to ~/.Renviron as GFW_TOKEN=your_token_here)
# =============================================================================

library(gfwr)
library(sf)
library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. Authenticate
# -----------------------------------------------------------------------------
key <- gfw_auth()   # reads GFW_TOKEN from environment


# -----------------------------------------------------------------------------
# 2. Define ICES division polygons as sf objects
#
#    These are approximate bounding polygons. For exact ICES boundaries,
#    replace with geometries from the official ICES shapefile:
#    https://gis.ices.dk/sf/  (search "ICES Statistical Rectangles Areas")
# -----------------------------------------------------------------------------

div_27_4c <- st_polygon(list(rbind(
  c(-4.0, 51.0),
  c( 8.0, 51.0),
  c( 8.0, 54.5),
  c(-4.0, 54.5),
  c(-4.0, 51.0)
))) |>
  st_sfc(crs = 4326) |>
  st_sf(division = "27.4.c")

div_27_7d <- st_polygon(list(rbind(
  c(-7.5, 51.0),
  c(-2.5, 51.0),
  c(-2.5, 55.5),
  c(-7.5, 55.5),
  c(-7.5, 51.0)
))) |>
  st_sfc(crs = 4326) |>
  st_sf(division = "27.7.d")

div_27_7e <- st_polygon(list(rbind(
  c(-9.0, 48.5),
  c(-4.5, 48.5),
  c(-4.5, 51.0),
  c(-9.0, 51.0),
  c(-9.0, 48.5)
))) |>
  st_sfc(crs = 4326) |>
  st_sf(division = "27.7.e")

# If you have the official ICES shapefile, load it instead:
# ices_areas <- st_read("path/to/ICES_Areas_20160601_cut_dense_3n.shp")
# div_27_4c  <- ices_areas |> filter(Area_27 == "4.c")
# div_27_7d  <- ices_areas |> filter(Area_27 == "7.d")
# div_27_7e  <- ices_areas |> filter(Area_27 == "7.e")


# -----------------------------------------------------------------------------
# 3. Union all three divisions into a single multipolygon for one API call
# -----------------------------------------------------------------------------

divisions_combined <- bind_rows(div_27_4c, div_27_7d, div_27_7e) |>
  st_union() |>
  st_as_sf()


# -----------------------------------------------------------------------------
# 4. Set time window
# -----------------------------------------------------------------------------

end_date   <- dmy("15-12-2018")
start_date <- end_date - 30   # last 30 days
message("Time window: ", start_date, " to ", end_date)


# -----------------------------------------------------------------------------
# 5. Single query across all three divisions
# -----------------------------------------------------------------------------

message("\nQuerying all divisions (27.4.c + 27.7.d + 27.7.e) ...")

all_events <- tryCatch(
  get_event(
    event_type    = "fishing",
    start_date    = format(start_date, "%Y-%m-%d"),
    end_date      = format(end_date,   "%Y-%m-%d"),
    region        = divisions_combined,
    region_source = "USER_SHAPEFILE",
    key           = key
  ),
  error = function(e) {
    message("Query failed: ", conditionMessage(e))
    NULL
  }
)


# -----------------------------------------------------------------------------
# 6. Extract unique vessels from events
# -----------------------------------------------------------------------------

unique_vessels <- all_events |>
  group_by(vesselId) |>
  summarise(
    vessel_name = first(na.omit(vessel_name)),
    mmsi        = first(na.omit(vessel_ssvid)),
    flag        = first(na.omit(vessel_flag)),
    vessel_type = first(na.omit(vessel_type)),
    n_events    = n(),
    .groups     = "drop"
  ) |>
  arrange(flag, vessel_name)


# -----------------------------------------------------------------------------
# 7. Summary output
# -----------------------------------------------------------------------------

message("\n=== SUMMARY ===")
message("Total fishing events : ", nrow(all_events))
message("Total unique vessels : ", nrow(unique_vessels))

print(unique_vessels)


# -----------------------------------------------------------------------------
# 8. Export
# -----------------------------------------------------------------------------

write.csv(unique_vessels,
          file = paste0("fishing_vessels_27.4.c_27.7.d_27.7.e_", end_date, ".csv"),
          row.names = FALSE)

# MMSI-only vector if you need it elsewhere
mmsi_vector <- unique_vessels$mmsi
message("\nMMSI list (first 20): ", paste(head(mmsi_vector, 20), collapse = ", "))
