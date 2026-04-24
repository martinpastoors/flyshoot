# ============================================================================
# patch_map_pefa_columns.R
#
# PURPOSE: Fix map_pefa_columns() in 01_flyshoot_functions.R.
#
# PROBLEM: map_pefa_columns() is missing several mappings that exist in
# map_columns(). Specifically for the PEFA elog_trek file format:
#   - shoot_lat is completely absent (latitude stays unmapped)
#   - trip_identifier not recognised as trip
#   - many other columns from the elog_trek schema not mapped
#
# APPLY TO: R/harmonized/01_flyshoot_functions.R
# Replace the column_mapping list inside map_pefa_columns() (~line 1695)
# ============================================================================

# ── FIND this block inside map_pefa_columns() (~lines 1695-1722): ─────────────

# column_mapping <- list(
#   # Trip identifier
#   trip = c("trip", "tripnumber", "tripnr", "trip_id", "tripid", "reis", "reisnummer", "reisnr"),
#   
#   # Haul identifier
#   haul_id = c("trek", "haul", "haul_id", "haulid", "trekid", "haalnr", "haalnummer"),
#   
#   # Date
#   date = c("datum", "date", "day", "dag", "catch_date"),
#   shoot_lon = c("longitude_shoot", "lon_shoot", "shoot_lon", "longitude", "lon", "lengtegraad"),
#   
#   # Positions - hauling (may not exist)
#   haul_lat = c("latitude_haul", "lat_haul", "haul_lat", "latitude_halen", "lat_halen"),
#   haul_lon = c("longitude_haul", "lon_haul", "haul_lon", "longitude_halen", "lon_halen"),
#   
#   # Times
#   shoot_time = c("tijd_shoot", "time_shoot", "shoot_time", "tijdbeginuitzetten", "tijd", "time"),
#   haul_time = c("tijd_haul", "time_haul", "haul_time", "tijdeindehalen", "tijd_halen"),
#   
#   # Catch
#   catch_kg = c("gewicht", "weight", "catch", "vangst", "weight_kg", "catch_kg"),
#   
#   # Species
#   species = c("soort", "species", "vis", "fish"),
#   
#   # Box/number
#   box_number = c("kist", "box", "kistnummer", "boxnumber", "nummer", "number")
# )

# ── REPLACE WITH: ─────────────────────────────────────────────────────────────

#   column_mapping <- list(
#     # Trip identifier — includes trip_identifier from PEFA elog_trek format
#     trip = c("trip", "tripnumber", "tripnr", "trip_id", "tripid",
#              "trip_identifier", "tripidentifier",
#              "reis", "reisnummer", "reisnr"),
#     
#     # Haul identifier
#     haul_id = c("trek", "haul", "haul_id", "haulid", "hauldid",
#                 "trekid", "haalnr", "haalnummer"),
#     
#     # Date
#     date = c("datum", "date", "day", "dag", "catch_date"),
#     
#     # Positions - shooting (shoot_lat was completely missing before)
#     shoot_lat = c("latitude_shoot", "lat_shoot", "shoot_lat",
#                   "latitude", "lat", "breedtegraad", "shootlat"),
#     shoot_lon = c("longitude_shoot", "lon_shoot", "shoot_lon",
#                   "longitude", "lon", "lengtegraad", "shootlong"),
#     
#     # Positions - hauling
#     haul_lat = c("latitude_haul", "lat_haul", "haul_lat",
#                  "latitude_halen", "lat_halen"),
#     haul_lon = c("longitude_haul", "lon_haul", "haul_lon",
#                  "longitude_halen", "lon_halen"),
#     
#     # Times
#     shoot_time = c("tijd_shoot", "time_shoot", "shoot_time",
#                    "tijdbeginuitzetten", "tijd", "time"),
#     haul_time  = c("tijd_haul", "time_haul", "haul_time",
#                    "tijdeindehalen", "tijd_halen"),
#     
#     # Catch weight
#     catch_kg = c("gewicht", "weight", "catch", "vangst", "weight_kg", "catch_kg"),
#     
#     # Species
#     species = c("soort", "species", "vis", "fish"),
#     
#     # Box/number
#     box_number = c("kist", "box", "kistnummer", "boxnumber", "nummer", "number"),
#     
#     # Presentation / preservation / freshness / size
#     presentation = c("presentation", "presentatie"),
#     preservation  = c("preservation", "conservering"),
#     freshness     = c("freshness", "versheid"),
#     size_class    = c("size", "maat", "grootte", "klasse"),
#     
#     # Conversion factor
#     conversion_factor = c("conversion_factor", "conversionfactor", "conversiefactor"),
#     
#     # Undersized
#     weight_undersized = c("weight_undersized", "weightundersized",
#                           "weight_undersized_kg", "ondermaats_kg"),
#     boxes_undersized  = c("boxes_undersized", "boxesundersized"),
#     
#     # Loss
#     loss_grams = c("loss_grams", "lossgrams", "verlies_gram"),
#     
#     # ICES / FAO spatial
#     ices_rect  = c("ices_rectangle", "icesrectangle", "rect", "ices_rect"),
#     fao_zone   = c("fao_zone", "faozone", "fao"),
#     economic_zone = c("economic_zone", "economiczone"),
#     
#     # Gear
#     gear_type    = c("gear_type", "geartype", "tuig"),
#     mesh_size_mm = c("mesh_size", "meshsize", "maaswijdte"),
#     
#     # Vessel / trip admin
#     vessel_nr       = c("vessel_number", "vesselnumber", "scheepsnummer"),
#     skipper         = c("captain", "skipper", "gezagvoerder"),
#     trip_status     = c("trip_status", "tripstatus"),
#     departure_date  = c("departure_date", "departuredate"),
#     departure_port  = c("departure_port", "departureport"),
#     arrival_date    = c("arrival_date", "arrivaldate"),
#     arrival_port    = c("arrival_port", "arrivalport"),
#     auction_date    = c("auction_date", "auctiondate"),
#     auction_port    = c("auction_port", "auctionport")
#   )

cat("Apply the replacement above to map_pefa_columns() in 01_flyshoot_functions.R\n")
cat("Then reprocess affected trips through 03_main_workflow.R\n")
