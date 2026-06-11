# ============================================================
# GFW Effort Analysis — gfwr v3.0
# Version 7 — separate RData files per step + recreation flags
#
# Each slow step is controlled by a boolean flag at the top:
#   recreate_* = TRUE  → re-run the step and overwrite saved data
#   recreate_* = FALSE → load saved data from disk, skip the step
#
# Saved RData files (one per step):
#   gfw_s2_discovery.RData   — discovery_events, mmsi_master
#   gfw_s2_registry.RData    — gfw_vessel_registry, gfw_vessel_periods,
#                              gfw_vessel_ids
#   gfw_s3_vessel_meta.RData — vessel_meta
#   gfw_s4_events.RData      — events_df
#   gfw_s5_divisions.RData   — events_with_division
#   gfw_s6_marine.RData      — events_marine_clean
#   gfw_s7_zones.RData       — events_classified
#   gfw_s8_effort.RData      — fishing_days, fishing_days_study
#
# Recommended first run: set all recreate_* = TRUE
# Re-run after vessel_meta corrections: set recreate_s3 and
#   recreate_s8 = TRUE, all others FALSE
# Re-run after new events: set recreate_s4 through recreate_s8 = TRUE
# ============================================================


# ============================================================
# STEP 1 — Setup
# ============================================================

library(tidyverse)
library(lubridate)
library(gfwr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

key <- gfw_auth()

spatialdir  <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"
gisdir      <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/GIS"

source("R/FLYSHOOT utils.r")

# ---- Recreation flags ----
# Set TRUE to re-run and overwrite; FALSE to load from disk

recreate_s2_discovery <- FALSE   # vessel discovery (get_event per ref year)
recreate_s2_registry  <- FALSE   # vessel registry  (gfw_vessel_info per MMSI)
recreate_s2c <- TRUE
recreate_s3           <- FALSE   # vessel_meta + manual corrections
recreate_s4           <- FALSE   # fishing events   (gfw_event per vesselId)
recreate_s56          <- FALSE   # spatial join → ICES division
                                # marine filter (EEZ + internal waters)
recreate_s7           <- TRUE   # coastal zone classification
recreate_s8           <- TRUE   # fishing days aggregation

# ---- Spatial layers ----

world <- ne_countries(scale = "medium", returnclass = "sf")

study_divisions <- c("27.4.C", "27.7.D", "27.7.E")
study_years     <- 2013:2025

fao_sf <- loadRData(file.path(spatialdir, "fao_sf.RData"))

fao_sf_division <-
  fao_sf %>%
  filter(F_LEVEL == "DIVISION") %>%
  dplyr::select(division = F_DIVISION) %>%
  mutate(division = toupper(division)) %>%
  filter(division %in% study_divisions) %>%
  group_by(division) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

# Combined study area polygon — used in Step 2 vessel discovery
study_area_polygon <- fao_sf_division %>%
  st_union() %>%
  st_as_sf()

eez_sf <- loadRData(file.path(spatialdir, "eez_sf.RData"))

load(file.path(spatialdir, "internal_waters.RData"))
load(file.path(spatialdir, "territorial_sea.RData"))
load(file.path(spatialdir, "French 3NM 6NM 12NM zones.RData"))

study_eez <- eez_sf %>%
  dplyr::select(eez = ISO_SOV1, geometry) %>%
  filter(eez %in% c("GBR", "NLD", "BEL", "FRA", "DEU", "DNK",
                    "NOR", "IRL", "ESP"))

# ---- Plot settings ----

flag_colours <- c(
  "BEL"   = "#4E79A7",
  "NLD"   = "#F28E2B",
  "GBR"   = "#59A14F",
  "FRA"   = "#E15759",
  "DEU"   = "#76B7B2",
  "DNK"   = "#EDC948",
  "NOR"   = "#B07AA1",
  "IRL"   = "#FF9DA7",
  "Other" = "#BAB0AC"
)

size_colours <- c(
  "S1 <100 GT"     = "#1D9E75",
  "S2 100-300 GT"  = "#BA7517",
  "S3 300-600 GT"  = "#7F77DD",
  "S4 600-1200 GT" = "#D85A30",
  "S5 >1200 GT"    = "#A32D2D"
)

threshold_pct <- 1
xlim <- c(-7, 8)
ylim <- c(48, 54)

gfw_gear_types <- c(
  "TRAWLERS", "OTHER_SEINES", "DREDGE_FISHING", "PURSE_SEINES",
  "TUNA_PURSE_SEINES", "SET_GILLNETS", "DRIFTNETS", "DRIFTING_LONGLINES",
  "POTS_AND_TRAPS", "POLE_AND_LINE", "SEINERS", "OTHER_PURSE_SEINES",
  "OTHER_FISHING", "FISHING"
)

reference <- tibble(
  valid_gear_types   = gfw_gear_types,
  valid_size_classes = c(
    "S1 <100 GT", "S2 100-300 GT", "S3 300-600 GT",
    "S4 600-1200 GT", "S5 >1200 GT",
    rep(NA_character_, length(gfw_gear_types) - 5)
  )
)

load_object <- function(filepath, object_name) {
  local({
    e <- new.env()
    load(filepath, envir = e)
    if (!object_name %in% ls(e)) {
      stop("Object '", object_name, "' not found in ", basename(filepath),
           "\nAvailable objects: ", paste(ls(e), collapse = ", "))
    }
    e[[object_name]]
  })
}

# Usage examples
# fishing_days_study <- load_object(file.path(flyshootdir, "gfw_s8_effort.RData"), "fishing_days_study" )

# ============================================================
# STEP 2a — Vessel discovery
# ============================================================
# Queries GFW for all vessels that fished in the study area
# during reference windows. Uses official ICES polygons.
#
# Output: discovery_events, mmsi_master
# File:   gfw_s2_discovery.RData

if (recreate_s2_discovery) {

  reference_dates <- as.Date(c(
    "2013-12-15",
    "2018-12-15",
    "2023-12-15",
    "2025-04-15"
  ))

  discovery_events <- map_dfr(reference_dates, function(end_date) {
    start_date <- end_date - 30
    message(sprintf("Querying %s to %s ...", start_date, end_date))
    tryCatch(
      gfw_event(
        event_type    = "FISHING",
        start_date    = format(start_date, "%Y-%m-%d"),
        end_date      = format(end_date,   "%Y-%m-%d"),
        region        = study_area_polygon,
        region_source = "USER_SHAPEFILE",
        key           = key
      ) %>%
        mutate(reference_date = end_date),
      error = function(e) {
        message("  Failed for period ending ", end_date, ": ", e$message)
        NULL
      }
    )
  })

  message("Total discovery events: ", nrow(discovery_events))
  message("Unique vesselIds:       ", n_distinct(discovery_events$vesselId))
  message("Unique MMSIs:           ", n_distinct(discovery_events$vessel_ssvid))

  mmsi_master <- discovery_events %>%
    group_by(mmsi = vessel_ssvid) %>%
    summarise(
      vessel_name = first(na.omit(vessel_name)),
      flag        = first(na.omit(vessel_flag)),
      vessel_type = first(na.omit(vessel_type)),
      n_events    = n(),
      n_periods   = n_distinct(reference_date),
      .groups     = "drop"
    ) %>%
    filter(!is.na(mmsi)) %>%
    arrange(flag, vessel_name)

  message("Master vessel list: ", nrow(mmsi_master), " unique MMSIs")
  message("Flags: ", paste(sort(unique(na.omit(mmsi_master$flag))),
                           collapse = ", "))

  save(discovery_events, 
       mmsi_master,
       file = file.path(flyshootdir, "gfw_s2_discovery.RData"))
  message("Saved gfw_s2_discovery.RData")

} else {

  load(file.path(flyshootdir, "gfw_s2_discovery.RData"))
  message("Loaded gfw_s2_discovery.RData: ",
          nrow(mmsi_master), " MMSIs")

}

#  mmsi_master <- load_object(file.path(flyshootdir, "gfw_s2_discovery.RData"), "mmsi_master" )

# ============================================================
# STEP 2b — Vessel registry
# ============================================================
# For each MMSI in mmsi_master, retrieves all vesselId periods
# and registry metadata (gear, GT, length) from GFW.
#
# Output: gfw_vessel_registry, gfw_vessel_periods, gfw_vessel_ids
# File:   gfw_s2_registry.RData

if (recreate_s2_registry) {

  n_total_mmsi <- nrow(mmsi_master)

  gfw_vessel_registry <- map_dfr(seq_along(mmsi_master$mmsi), function(i) {
    m <- mmsi_master$mmsi[i]
    if (i %% 50 == 0 || i == 1)
      message(sprintf("[%d/%d] querying MMSI %s", i, n_total_mmsi, m))
    tryCatch({
      res <- gfw_vessel_info(query = m, search_type = "search", key = key)

      ri <- if (!is.null(res$registryInfo) && nrow(res$registryInfo) > 0 &&
                "latestVesselInfo" %in% names(res$registryInfo)) {
        res$registryInfo %>%
          arrange(desc(latestVesselInfo)) %>%
          slice(1) %>%
          dplyr::select(any_of(c("geartypes", "tonnageGt", "lengthM")))
      } else {
        tibble(geartypes = NA_character_,
               tonnageGt = NA_real_,
               lengthM   = NA_real_)
      }

      res$selfReportedInfo %>%
        mutate(mmsi_queried = m) %>%
        bind_cols(ri)

    }, error = function(e) {
      message("  Failed MMSI: ", m, " — ", e$message)
      NULL
    })
  })

  # Check for missing MMSIs
  n_missing <- mmsi_master %>%
    anti_join(gfw_vessel_registry, by = c("mmsi" = "mmsi_queried")) %>%
    nrow()
  message("MMSIs absent from registry (should be 0): ", n_missing)

  gfw_vessel_periods <- gfw_vessel_registry %>%
    dplyr::select(vesselId, ssvid, shipname,
                  transmissionDateFrom, transmissionDateTo, mmsi_queried)

  gfw_vessel_ids <- unique(gfw_vessel_periods$vesselId)

  message(n_distinct(gfw_vessel_periods$vesselId), " unique vesselIds for ",
          n_distinct(gfw_vessel_periods$ssvid), " unique MMSIs")

  save(gfw_vessel_registry, gfw_vessel_periods, gfw_vessel_ids,
       file = file.path(flyshootdir, "gfw_s2_registry.RData"))
  message("Saved gfw_s2_registry.RData")

} else {

  load(file.path(flyshootdir, "gfw_s2_registry.RData"))
  message("Loaded gfw_s2_registry.RData: ",
          n_distinct(gfw_vessel_registry$vesselId), " vesselIds")

}


# Check how many of your fishing_days_study records
# are contaminated by MMSI collision
# Key indicator: same ssvid, different IMO numbers in registry

# mmsi_collision_check <- gfw_vessel_registry %>%
#   filter(!is.na(imo), imo != "", imo != "0") %>%
#   group_by(ssvid) %>%
#   filter(n_distinct(imo) > 1) %>%
#   summarise(
#     n_imo    = n_distinct(imo),
#     imos     = paste(unique(imo),     collapse = " / "),
#     names    = paste(unique(shipname), collapse = " / "),
#     flags    = paste(unique(flag),     collapse = " / "),
#     .groups  = "drop"
#   ) %>%
#   arrange(desc(n_imo))
# 
# message("MMSIs with multiple IMOs (collision): ",
#         nrow(mmsi_collision_check))
# print(mmsi_collision_check, n = 20)

# How many fishing days in your study are from collision MMSIs?

# fishing_days_study %>%
#   filter(ssvid %in% mmsi_collision_check$ssvid) %>%
#   summarise(
#     n_vessels    = n_distinct(ssvid),
#     fishing_days = sum(fishing_days),
#     pct_days     = round(fishing_days /
#                            sum(fishing_days_study$fishing_days) * 100, 1)
#   )




# ============================================================
# STEP 3 — Vessel metadata: build vessel_meta lookup
# ============================================================
#
# *** MUST RUN BEFORE STEP 4 ***
# vessel_meta defines which vesselIds are queried for effort data.
#
# Sub-steps:
#   3a — Build base vessel_meta from GFW registry
#   3b — Apply gear corrections (non-fishing vessel fixes)
#   3c — Apply missing gear assignments (manual Excel file)
#   3d — Validation checks
#   3e — Final catch-all imputation (gear, size class, flag)
#   3f — GT imputation (median by size class × flag × length)
#
# Inputs:
#   gfw_vessel_registry          — from STEP 2b
#   mmsi_master                  — from STEP 2a
#   vessel_gear_corrections.xlsx — manual fixes for non-fishing gears
#   vessel_gear_missing.xlsx     — manual gear assignments for NA-gear vessels
#
# Output:
#   vessel_meta — one row per vesselId, fully corrected and enriched
#
# File: gfw_s3_vessel_meta.RData
#
# Re-run triggers (set recreate_s3 = TRUE):
#   - After updating either correction Excel file
#   - After adding IMO enrichment (Step 2c)
#   - After adding new vessels to mmsi_master
# ============================================================

if (recreate_s3) {
  
  # ---- 3a: Build base vessel_meta from registry ----
  # One row per vesselId. For each MMSI, propagate the best available
  # gear, GT, length and IMO across all identity periods.
  
  vessel_meta <- gfw_vessel_registry %>%
    dplyr::select(
      vesselId, ssvid, shipname,
      transmissionDateFrom, transmissionDateTo,
      vessel_flag = flag, geartypes, tonnageGt, lengthM, imo
    ) %>%
    distinct() %>%
    
    # Add vessel_name from master MMSI list (Step 2a)
    left_join(
      mmsi_master %>% dplyr::select(mmsi, vessel_name, flag),
      by = c("ssvid" = "mmsi")
    ) %>%
    mutate(vessel_flag = coalesce(vessel_flag, flag)) %>%
    
    # Propagate best values across all periods for the same MMSI
    group_by(ssvid) %>%
    mutate(
      best_gear   = first(na.omit(geartypes)),
      best_gt     = median(tonnageGt[tonnageGt > 0], na.rm = TRUE),
      best_length = median(lengthM[lengthM > 0],     na.rm = TRUE),
      best_imo    = first(na.omit(imo[imo != "" & imo != "0"]))
    ) %>%
    ungroup() %>%
    
    # Keep one row per vesselId — prefer rows with gear and GT
    dplyr::select(
      vesselId, ssvid, vessel_name, vessel_flag,
      gear     = best_gear,
      gt       = best_gt,
      length_m = best_length,
      imo      = best_imo,
      transmissionDateFrom, transmissionDateTo
    ) %>%
    group_by(vesselId) %>%
    arrange(desc(!is.na(gear)), desc(!is.na(gt))) %>%
    slice(1) %>%
    ungroup() %>%
    
    # ---- Size classification ----
  # Primary: GT. Fallback: length converted to approximate GT class.
  mutate(
    size_var = case_when(
      !is.na(gt) & gt > 0             ~ gt,
      !is.na(length_m) & length_m > 0 ~ case_when(
        length_m < 15  ~   50,   # proxy for S1 <100 GT
        length_m < 20  ~  120,   # proxy for S2 100-300 GT
        length_m < 28  ~  280,   # proxy for S2 100-300 GT
        length_m < 42  ~  550,   # proxy for S3 300-600 GT
        length_m < 70  ~  900,   # proxy for S4 600-1200 GT
        TRUE           ~ 1500    # proxy for S5 >1200 GT
      ),
      TRUE ~ NA_real_
    ),
    size_source = case_when(
      !is.na(gt) & gt > 0             ~ "GT",
      !is.na(length_m) & length_m > 0 ~ "length proxy",
      TRUE                             ~ "unknown"
    ),
    size_class = case_when(
      is.na(size_var)    ~ "Unknown",
      size_var <  100    ~ "S1 <100 GT",
      size_var <  300    ~ "S2 100-300 GT",
      size_var <  600    ~ "S3 300-600 GT",
      size_var < 1200    ~ "S4 600-1200 GT",
      size_var >= 1200   ~ "S5 >1200 GT"
    ),
    size_class = factor(size_class, levels = c(
      "S1 <100 GT", "S2 100-300 GT", "S3 300-600 GT",
      "S4 600-1200 GT", "S5 >1200 GT", "Unknown"
    ))
  )
  
  message("vessel_meta (raw): ", nrow(vessel_meta), " rows | ",
          n_distinct(vessel_meta$vesselId), " vesselIds | ",
          n_distinct(vessel_meta$ssvid), " unique MMSIs")
  
  
  # ---- 3b: Apply gear corrections (non-fishing vessel fixes) ----
  # Source: vessel_gear_corrections.xlsx
  # action = "exclude" → remove from analysis
  # action = "correct" → overwrite gear and/or size_class
  #
  # To generate this file for the first time:
  #   non_fishing_gears <- c("CARGO", "PASSENGER", "SUPPLY_VESSEL",
  #                          "SEISMIC_VESSEL", "NON_FISHING",
  #                          "DREDGE_NON_FISHING")
  #   vessel_meta %>%
  #     distinct(ssvid, vessel_name, vessel_flag, gear, size_class) %>%
  #     filter(gear %in% non_fishing_gears) %>%
  #     arrange(gear, vessel_flag) %>%
  #     mutate(gear_corrected = NA_character_,
  #            size_class_corrected = NA_character_,
  #            action = NA_character_) %>%
  #     writexl::write_xlsx(
  #       list(corrections = ., reference = reference),
  #       path = file.path(flyshootdir, "vessel_gear_corrections.xlsx"))
  
  corrections <- readxl::read_xlsx(
    file.path(flyshootdir, "vessel_gear_corrections.xlsx")
  ) %>%
    mutate(ssvid = as.character(ssvid)) %>%
    filter(!is.na(gear_corrected) | action == "exclude")
  
  vessel_meta <- vessel_meta %>%
    left_join(
      corrections %>% dplyr::select(ssvid, gear_corrected,
                                    size_class_corrected, action),
      by = "ssvid"
    ) %>%
    filter(is.na(action) | action != "exclude") %>%
    mutate(
      gear = case_when(
        action == "correct" & !is.na(gear_corrected) ~ gear_corrected,
        TRUE ~ gear
      ),
      size_class = case_when(
        action == "correct" & !is.na(size_class_corrected) ~
          factor(size_class_corrected, levels = levels(size_class)),
        TRUE ~ size_class
      )
    ) %>%
    dplyr::select(-gear_corrected, -size_class_corrected, -action)
  
  message("After gear corrections — vessels remaining: ",
          n_distinct(vessel_meta$ssvid))
  
  
  # ---- 3c: Apply missing gear assignments ----
  # Source: vessel_gear_missing.xlsx
  # Vessels with no gear in GFW registry, manually assigned.
  #
  # To generate this file for the first time:
  #   vessel_meta %>%
  #     filter(is.na(gear)) %>%
  #     distinct(ssvid, vessel_name, vessel_flag, gt, length_m, size_class) %>%
  #     arrange(vessel_flag, vessel_name) %>%
  #     mutate(gear_corrected = NA_character_,
  #            size_class_corrected = NA_character_,
  #            action = "correct",
  #            notes = NA_character_) %>%
  #     writexl::write_xlsx(
  #       list(missing_gear = ., reference = reference),
  #       path = file.path(flyshootdir, "vessel_gear_missing.xlsx"))
  
  missing_gear_corrections <- readxl::read_xlsx(
    file.path(flyshootdir, "vessel_gear_missing.xlsx"),
    sheet = "missing_gear"
  ) %>%
    mutate(ssvid = as.character(ssvid)) %>%
    filter(!is.na(gear_corrected) | action == "exclude")
  
  vessel_meta <- vessel_meta %>%
    left_join(
      missing_gear_corrections %>%
        dplyr::select(ssvid, gear_corrected,
                      size_class_corrected, action),
      by = "ssvid"
    ) %>%
    filter(is.na(action) | action != "exclude") %>%
    mutate(
      gear = coalesce(gear_corrected, gear),
      size_class = case_when(
        !is.na(size_class_corrected) ~
          factor(size_class_corrected, levels = levels(size_class)),
        TRUE ~ size_class
      )
    ) %>%
    dplyr::select(-gear_corrected, -size_class_corrected, -action)
  
  message("After missing gear corrections — vessels remaining: ",
          n_distinct(vessel_meta$ssvid))
  message("Remaining NA gear: ", sum(is.na(vessel_meta$gear)))
  
  
  # ---- 3d: Validation checks ----
  
  vessel_meta %>%
    distinct(ssvid, size_class) %>%
    count(size_class) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()
  
  vessel_meta %>%
    distinct(ssvid, gear, size_class) %>%
    count(size_class, gear) %>%
    arrange(size_class, desc(n)) %>%
    print(n = 40)
  
  
  # ---- 3e: Final catch-all imputation ----
  # Remaining NA gear vessels are confirmed small inshore vessels
  # by manual inspection — assigned OTHER_FISHING / S1 <100 GT.
  # Survey vessels and non-fishing vessels explicitly excluded.
  # All imputation decisions recorded as boolean flags for
  # downstream filtering and methods transparency.
  
  # Remove confirmed non-fishing and survey vessels
  vessel_meta <- vessel_meta %>%
    filter(gear != "DIVE_VESSEL" | is.na(gear)) %>%
    filter(!ssvid %in% c(
      "205130000",   # BELGICA — Belgian research vessel
      "999999000"    # French survey vessel
    ))
  
  # Apply gear and size class defaults to remaining NAs
  vessel_meta <- vessel_meta %>%
    mutate(
      gear_imputed       = is.na(gear),
      gear               = coalesce(gear, "OTHER_FISHING"),
      size_class_imputed = is.na(size_class) | size_class == "Unknown",
      size_class         = case_when(
        is.na(size_class) | size_class == "Unknown" ~
          factor("S1 <100 GT", levels = levels(size_class)),
        TRUE ~ size_class
      )
    )
  
  # Impute missing flag from MMSI Maritime Identification Digits (MID)
  # First 3 digits of MMSI encode the flag state per ITU standard
  mid_to_flag <- tribble(
    ~mid, ~flag,
    "201", "ALB", "202", "AND", "203", "AUT", "204", "PRT",
    "205", "BEL", "206", "BLR", "207", "BGR", "208", "VAT",
    "209", "CYP", "210", "CYP", "211", "DEU", "212", "CYP",
    "213", "GEO", "214", "MDA", "215", "MLT", "216", "ARM",
    "218", "DEU", "219", "DNK", "220", "DNK", "221", "GRL",
    "222", "FRO", "224", "ESP", "225", "ESP", "226", "FRA",
    "227", "FRA", "228", "FRA", "229", "MLT", "230", "FIN",
    "231", "FRO", "232", "GBR", "233", "GBR", "234", "GBR",
    "235", "GBR", "236", "GIB", "237", "GRC", "238", "HRV",
    "239", "GRC", "240", "GRC", "241", "GRC", "242", "MOR",
    "243", "HUN", "244", "NLD", "245", "NLD", "246", "NLD",
    "247", "ITA", "248", "MLT", "249", "MLT", "250", "IRL",
    "251", "ISL", "252", "LIE", "253", "LUX", "254", "MCO",
    "255", "PRT", "256", "MLT", "257", "NOR", "258", "NOR",
    "259", "NOR", "261", "POL", "262", "MNE", "263", "PRT",
    "264", "ROU", "265", "SWE", "266", "SWE", "267", "SVK",
    "268", "SMR", "269", "CHE", "270", "CZE", "271", "TUR",
    "272", "UKR", "273", "RUS", "274", "MKD", "275", "LVA",
    "276", "EST", "277", "LTU", "278", "SVN", "279", "SRB"
  )
  
  vessel_meta <- vessel_meta %>%
    mutate(
      mid           = substr(ssvid, 1, 3),
      flag_from_mid = mid_to_flag$flag[match(mid, mid_to_flag$mid)],
      flag_imputed  = is.na(vessel_flag) & !is.na(flag_from_mid),
      vessel_flag   = case_when(
        !is.na(vessel_flag)   ~ vessel_flag,
        !is.na(flag_from_mid) ~ flag_from_mid,
        TRUE                  ~ vessel_flag
      )
    ) %>%
    dplyr::select(-mid, -flag_from_mid)
  
  message("After final imputation:")
  message("  Remaining NA gear:        ",
          sum(is.na(vessel_meta$gear)))
  message("  Remaining Unknown class:  ",
          sum(vessel_meta$size_class == "Unknown", na.rm = TRUE))
  message("  Remaining NA flag:        ",
          sum(is.na(vessel_meta$vessel_flag)))
  message("  Gear imputed:             ",
          sum(vessel_meta$gear_imputed,       na.rm = TRUE))
  message("  Size class imputed:       ",
          sum(vessel_meta$size_class_imputed, na.rm = TRUE))
  message("  Flag imputed from MID:    ",
          sum(vessel_meta$flag_imputed,       na.rm = TRUE))
  
  
  # ---- 3f: GT imputation ----
  # Vessels missing GT are assigned the median GT of vessels with
  # known GT in the same size_class × vessel_flag × length bracket.
  # Three-level fallback hierarchy:
  #   L3: size_class × vessel_flag × length bracket (most specific)
  #   L2: size_class × vessel_flag
  #   L1: size_class only (least specific)
  # Median used instead of mean — GT distributions are right-skewed.
  # Original gt column preserved; gt_final used for GT-day calculations.
  
  # Build lookup tables at each level
  length_brackets <- function(df) {
    df %>%
      mutate(length_bracket = case_when(
        is.na(length_m)    ~ "unknown",
        length_m < 12      ~ "<12m",
        length_m < 18      ~ "12-18m",
        length_m < 24      ~ "18-24m",
        length_m < 30      ~ "24-30m",
        length_m < 40      ~ "30-40m",
        length_m >= 40     ~ ">40m"
      ))
  }
  
  gt_base <- vessel_meta %>%
    filter(!is.na(gt), gt > 0,
           !is.na(size_class), size_class != "Unknown") %>%
    length_brackets()
  
  # L3: size_class × vessel_flag × length bracket
  gt_lookup_l3 <- gt_base %>%
    group_by(size_class, vessel_flag, length_bracket) %>%
    summarise(n = n(), gt_l3 = round(median(gt), 1),
              .groups = "drop") %>%
    filter(n >= 3) %>%
    dplyr::select(-n)
  
  # L2: size_class × vessel_flag
  gt_lookup_l2 <- gt_base %>%
    group_by(size_class, vessel_flag) %>%
    summarise(n = n(), gt_l2 = round(median(gt), 1),
              .groups = "drop") %>%
    filter(n >= 3) %>%
    dplyr::select(-n)
  
  # L1: size_class only
  gt_lookup_l1 <- gt_base %>%
    group_by(size_class) %>%
    summarise(gt_l1 = round(median(gt), 1), .groups = "drop")
  
  message("GT lookup (size class only):")
  print(gt_lookup_l1)
  
  # Apply imputation — join all three levels then coalesce
  vessel_meta <- vessel_meta %>%
    length_brackets() %>%
    left_join(gt_lookup_l3, by = c("size_class", "vessel_flag",
                                   "length_bracket")) %>%
    left_join(gt_lookup_l2, by = c("size_class", "vessel_flag")) %>%
    left_join(gt_lookup_l1, by = "size_class") %>%
    mutate(
      gt_imputed = is.na(gt),
      gt_source  = case_when(
        !is.na(gt)   ~ "observed",
        !is.na(gt_l3) ~ "imputed: size×flag×length",
        !is.na(gt_l2) ~ "imputed: size×flag",
        !is.na(gt_l1) ~ "imputed: size class only",
        TRUE          ~ "imputed: unknown"
      ),
      gt_final   = case_when(
        !is.na(gt)    ~ gt,
        !is.na(gt_l3) ~ gt_l3,
        !is.na(gt_l2) ~ gt_l2,
        !is.na(gt_l1) ~ gt_l1,
        TRUE          ~ NA_real_
      )
    ) %>%
    dplyr::select(-gt_l3, -gt_l2, -gt_l1, -length_bracket)
  
  message("GT imputation summary:")
  vessel_meta %>%
    count(gt_source, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()
  
  message("Remaining NA gt_final: ",
          sum(is.na(vessel_meta$gt_final)))
  
  
  # ---- Save ----
  save(vessel_meta,
       file = file.path(flyshootdir, "gfw_s3_vessel_meta.RData"))
  message("Saved gfw_s3_vessel_meta.RData: ",
          n_distinct(vessel_meta$ssvid), " vessels | ",
          ncol(vessel_meta), " columns")
  
} else {
  
  vessel_meta <- local({
    e <- new.env()
    load(file.path(flyshootdir, "gfw_s3_vessel_meta.RData"), envir = e)
    e$vessel_meta
  })
  
  message("Loaded gfw_s3_vessel_meta.RData: ",
          n_distinct(vessel_meta$ssvid), " vessels | ",
          sum(is.na(vessel_meta$gear)), " NA gear | ",
          sum(is.na(vessel_meta$gt_final)), " NA gt_final")
  
}

# ============================================================
# STEP 4 — Fishing events: pull 2012-2025 per vesselId
# ============================================================
#
# Downloads all fishing events from GFW for each vessel in
# vessel_meta that was directly observed in the study area
# during the discovery queries (Step 2).
#
# Key design decisions:
#   1. Only queries vesselIds present in discovery_events
#      (target_vessel_ids_strict) — avoids wasting API calls
#      on registry-only vesselIds that never generated events
#   2. Year range per vessel is derived from transmissionDateFrom/To
#      in gfw_vessel_periods, clipped to 2012-2025 — avoids
#      querying years outside the vessel's active period
#   3. Parallel processing using furrr (4 workers by default)
#      reduces runtime by ~75% vs sequential
#   4. Checkpoint saves every 100 vessels — safe to interrupt
#      and resume; already-completed vessels are skipped
#   5. No shapefile restriction in gfw_event() — speed testing
#      showed server-side spatial filtering is ~4x SLOWER than
#      downloading all events and filtering locally in Step 5
#
# Inputs:
#   vessel_meta         — from Step 3 (cleaned vessel list)
#   discovery_events    — from Step 2 (vessels seen in study area)
#   vessel_year_ranges  — derived below from gfw_vessel_periods
#   GFW_TOKEN           — must be set in ~/.Renviron
#
# Output:
#   events_df           — all fishing events for study vessels
# File:
#   gfw_s4_events.RData
#
# Runtime: ~14-17 hours with 4 parallel workers for ~1600 vessels
#
# To RESUME after interruption:
#   Set recreate_s4 = TRUE — completed vessels are auto-skipped
# To SKIP entirely:
#   Set recreate_s4 = FALSE — loads gfw_s4_events.RData from disk
# ============================================================

if (recreate_s4) {
  
  library(furrr)
  library(future)
  library(progressr)
  
  # ---- 4a: Derive vessel year ranges ----
  # For each vesselId, determine which years to query based on
  # the transmission period in the registry. Clipped to 2012-2025.
  # This avoids querying 14 years for a vessel only active in 2019-2021.
  
  vessel_year_ranges <- vessel_meta %>%
    mutate(
      year_from = year(as.Date(substr(transmissionDateFrom, 1, 10))),
      year_to   = year(as.Date(substr(transmissionDateTo,   1, 10)))
    ) %>%
    mutate(
      year_from = pmax(year_from, 2012),
      year_to   = pmin(year_to,   2025)
    ) %>%
    filter(year_from <= year_to) %>%
    group_by(vesselId) %>%
    summarise(
      year_from = min(year_from),
      year_to   = max(year_to),
      .groups   = "drop"
    )
  
  # ---- 4b: Identify vessels to query ----
  # Strict target: only vesselIds directly seen in discovery events.
  # This excludes registry-only vesselIds that never generated
  # fishing events (e.g. small inshore vessels with sparse AIS).
  
  target_vessel_ids_strict <- intersect(
    unique(vessel_meta$vesselId),
    unique(discovery_events$vesselId)
  )
  
  # Check for already-completed vessels (resume support)
  # Loads only events_df from disk without overwriting other objects
  completed_ids <- if (exists("events_df") && nrow(events_df) > 0) {
    unique(events_df$vesselId)
  } else if (file.exists(file.path(flyshootdir, "gfw_s4_events.RData"))) {
    tmp <- new.env()
    load(file.path(flyshootdir, "gfw_s4_events.RData"), envir = tmp)
    unique(tmp$events_df$vesselId)
  } else {
    character(0)
  }
  
  remaining_ids_to_run <- setdiff(target_vessel_ids_strict, completed_ids)
  
  # Runtime estimate
  calls_remaining <- vessel_year_ranges %>%
    filter(vesselId %in% remaining_ids_to_run) %>%
    mutate(n_years = year_to - year_from + 1) %>%
    summarise(total = sum(n_years)) %>%
    pull(total)
  
  message("===== STEP 4 — Fishing events =====")
  message("Target vesselIds (strict):  ", length(target_vessel_ids_strict))
  message("Already completed:          ", length(completed_ids))
  message("Remaining to run:           ", length(remaining_ids_to_run))
  message("Remaining API calls:        ", calls_remaining)
  message("Estimated runtime (1 worker):  ",
          round(calls_remaining * 24 / 3600, 1), " hours")
  message("Estimated runtime (4 workers): ",
          round(calls_remaining * 24 / 3600 / 4, 1), " hours")
  
  if (length(remaining_ids_to_run) == 0) {
    
    message("All vessels already completed — loading from disk")
    tmp <- new.env()
    load(file.path(flyshootdir, "gfw_s4_events.RData"), envir = tmp)
    events_df <- tmp$events_df
    
  } else {
    
    # ---- 4c: Set up parallel workers ----
    # 4 workers gives ~75% runtime reduction vs sequential.
    # Reduce to 2 if GFW API returns HTTP 429 rate-limit errors.
    
    n_cores   <- parallel::detectCores()
    n_workers <- 4
    plan(multisession, workers = n_workers)
    
    message("Available cores: ", n_cores, " | Using workers: ", n_workers)
    
    # ---- 4d: Build vessel lookup table for workers ----
    # Pre-computed lookup passed to each worker to avoid repeated
    # filtering inside the parallel function
    
    vessel_lookup <- vessel_meta %>%
      distinct(vesselId, ssvid, vessel_name, vessel_flag, size_class) %>%
      left_join(vessel_year_ranges, by = "vesselId")
    
    # ---- 4e: Vessel processing function ----
    # Runs inside each parallel worker — must load its own libraries
    # and authenticate independently (workers don't share environment)
    
    process_vessel <- function(vid, vessel_lookup, key_token) {
      
      library(gfwr)
      library(dplyr)
      library(purrr)
      
      key <- key_token
      
      # Get vessel info and year range from lookup
      vinfo  <- vessel_lookup %>% filter(vesselId == vid)
      y_from <- if (nrow(vinfo) > 0 && !is.na(vinfo$year_from)) vinfo$year_from else 2012
      y_to   <- if (nrow(vinfo) > 0 && !is.na(vinfo$year_to))   vinfo$year_to   else 2025
      vname  <- if (nrow(vinfo) > 0 && !is.na(vinfo$vessel_name)) vinfo$vessel_name else "unknown"
      vflag  <- if (nrow(vinfo) > 0 && !is.na(vinfo$vessel_flag)) vinfo$vessel_flag else "?"
      
      # Query GFW year by year — API capped at 366 days per call
      events_v <- map_dfr(y_from:y_to, function(y) {
        Sys.sleep(0.5)   # rate limiting per worker
        tryCatch({
          result <- suppressMessages(
            gfw_event(
              event_type = "FISHING",
              vessels    = vid,
              start_date = paste0(y, "-01-01"),
              end_date   = paste0(y, "-12-31"),
              key        = key
            )
          )
          if (!is.null(result) && nrow(result) > 0) result else NULL
        }, error = function(e) NULL)
      })
      
      n_events <- if (is.null(events_v) || nrow(events_v) == 0) 0 else nrow(events_v)
      
      list(
        vesselId    = vid,
        vessel_name = vname,
        vessel_flag = vflag,
        year_from   = y_from,
        year_to     = y_to,
        n_years     = y_to - y_from + 1,
        n_events    = n_events,
        events      = events_v
      )
    }
    
    # ---- 4f: Authenticate for worker sessions ----
    # Token passed explicitly to workers — must be set in ~/.Renviron
    # as GFW_TOKEN=your_token_here
    
    key_token <- Sys.getenv("GFW_TOKEN")
    if (nchar(key_token) == 0) stop("GFW_TOKEN not found in ~/.Renviron")
    
    # ---- 4g: Chunk loop with checkpointing ----
    # Processes vessels in chunks of 100. After each chunk:
    #   - results appended to events_df
    #   - checkpoint saved to disk
    #   - progress and ETA reported to console
    # If interrupted, restart with recreate_s4 = TRUE —
    # completed vessels are automatically skipped in step 4b.
    
    chunk_size    <- 100
    vessel_chunks <- split(remaining_ids_to_run,
                           ceiling(seq_along(remaining_ids_to_run) / chunk_size))
    n_chunks      <- length(vessel_chunks)
    n_total       <- length(remaining_ids_to_run)
    chunk_log     <- tibble()
    
    message(sprintf("Processing %d vessels in %d chunks of ~%d",
                    n_total, n_chunks, chunk_size))
    
    for (chunk_i in seq_along(vessel_chunks)) {
      
      chunk_ids   <- vessel_chunks[[chunk_i]]
      chunk_start <- Sys.time()
      
      message(sprintf("\n[Chunk %d/%d] %d vessels | started %s",
                      chunk_i, n_chunks, length(chunk_ids),
                      format(chunk_start, "%H:%M:%S")))
      
      # Parallel processing with progress bar
      with_progress({
        p <- progressor(steps = length(chunk_ids))
        
        chunk_results <- future_map(
          chunk_ids,
          function(vid) {
            p(sprintf("%s", vid))
            process_vessel(vid, vessel_lookup, key_token)
          },
          .options = furrr_options(
            seed     = TRUE,
            packages = c("gfwr", "dplyr", "purrr")
          )
        )
      })
      
      # Unpack events from results list
      chunk_events <- bind_rows(
        keep(map(chunk_results, "events"),
             ~ !is.null(.x) && nrow(.x) > 0)
      )
      
      # Chunk summary
      chunk_summary <- tibble(
        chunk         = chunk_i,
        n_vessels     = length(chunk_ids),
        n_with_events = sum(map_int(chunk_results, "n_events") > 0),
        n_events      = sum(map_int(chunk_results, "n_events")),
        elapsed_min   = round(as.numeric(
          Sys.time() - chunk_start, units = "mins"), 1)
      )
      chunk_log <- bind_rows(chunk_log, chunk_summary)
      
      message(sprintf("  Vessels with events: %d/%d | Events: %d | Time: %.1f min",
                      chunk_summary$n_with_events,
                      chunk_summary$n_vessels,
                      chunk_summary$n_events,
                      chunk_summary$elapsed_min))
      
      # Append chunk events to events_df
      if (!is.null(chunk_events) && nrow(chunk_events) > 0) {
        events_df <- if (exists("events_df") && nrow(events_df) > 0) {
          bind_rows(events_df, chunk_events)
        } else {
          chunk_events
        }
      }
      
      # Checkpoint save
      save(events_df,
           file = file.path(flyshootdir, "gfw_s4_events.RData"))
      
      # ETA based on actual throughput of this chunk
      vessels_done <- min(chunk_i * chunk_size, n_total)
      vessels_left <- n_total - vessels_done
      rate_per_min <- chunk_summary$n_vessels / chunk_summary$elapsed_min
      eta_hours    <- round(vessels_left / rate_per_min / 60, 1)
      
      message(sprintf("  Overall: %d/%d vessels | ETA: %.1f hours | Total events: %d",
                      vessels_done, n_total, eta_hours, nrow(events_df)))
      
      # Print running chunk log every 5 chunks
      if (chunk_i %% 5 == 0) {
        message("\n  --- Chunk log so far ---")
        print(chunk_log)
      }
    }
    
    # ---- 4h: Finalise ----
    plan(sequential)   # release parallel workers
    
    message("\n===== STEP 4 COMPLETE =====")
    message("Total events:    ", nrow(events_df))
    message("Total vesselIds: ", n_distinct(events_df$vesselId))
    message("Chunk summary:")
    print(chunk_log)
    
    save(events_df,
         file = file.path(flyshootdir, "gfw_s4_events.RData"))
    message("Saved gfw_s4_events.RData")
    
  }
  
} else {
  
  # Load only events_df — does not overwrite vessel_meta,
  # gfw_vessel_registry or any other object in the environment
  events_df <- local({
    e <- new.env()
    load(file.path(flyshootdir, "gfw_s4_events.RData"), envir = e)
    e$events_df
  })
  
  message("Loaded gfw_s4_events.RData:")
  message("  Total events:    ", nrow(events_df))
  message("  Total vesselIds: ", n_distinct(events_df$vesselId))
  
}

#   > message("Total events:    ", nrow(events_df))
# Total events:    2548627
# > message("Total vesselIds: ", n_distinct(events_df$vesselId))
# Total vesselIds: 3335
# > message("Chunk summary:")
# Chunk summary:
#   > print(chunk_log)
# # A tibble: 16 × 5
# chunk n_vessels n_with_events n_events elapsed_min
# <int>     <int>         <int>    <int>       <dbl>
#   1     1       100           100    90523        69.3
# 2     2       100           100    83592        82.2
# 3     3       100           100    84786        48  
# 4     4       100           100    83415        28.7
# 5     5       100           100    90126        25.2
# 6     6       100           100    81326        46.8
# 7     7       100           100    79397        62.5
# 8     8       100           100    89808        64.2
# 9     9       100           100    76231        55.7
# 10    10       100           100    86000        38.6
# 11    11       100           100    91384        36  
# 12    12       100           100    93314        54.5
# 13    13       100           100    78686        55.6
# 14    14       100           100    81538        76.1
# 15    15       100           100    67485        24.1
# 16    16        19            19     9558         6.1

# ---- Sanity check ----
message("Matched vesselIds (events vs vessel_meta): ",
        n_distinct(intersect(events_df$vesselId,
                             vessel_meta$vesselId)))


# ============================================================
# STEP 5 — Spatial join: events → ICES division
# ============================================================
# ============================================================
# STEP 6 — Marine filter: internal waters + EEZ
# ============================================================
# Output: events_marine_clean
# File:   gfw_s5_6_marine_clean.RData

if (recreate_s56) {

  sf_use_s2(FALSE)
  
  events_marine_clean <- events_df %>%
    # Drop any columns from previous runs that would interfere
    dplyr::select(-any_of(c("division", "eez", "zone", "geometry",
                            "internal"))) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    
    # Convert to sf once
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    
    # Step 5: assign ICES division
    st_join(fao_sf_division, join = st_within) %>%
    
    # Step 6a: assign EEZ and keep only study area
    st_join(study_eez, join = st_within) %>%
    
    # Step 6b: flag internal waters
    mutate(internal = lengths(st_intersects(., internal_waters)) > 0) %>%
    
    # Drop geometry — back to plain tibble
    st_drop_geometry() %>%
    
    # Apply all filters in one step
    filter(!is.na(division),
           !is.na(eez),
           !internal) %>%
    dplyr::select(-internal)
  
  sf_use_s2(TRUE)
  
  gc()
  
  message("events_marine_clean: ", nrow(events_marine_clean), " events | ",
          n_distinct(events_marine_clean$vesselId), " vesselIds")
  message("Division breakdown:")
  events_marine_clean %>%
    count(division, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()
  
  save(events_marine_clean,
       file = file.path(flyshootdir, "gfw_s5_6_marine_clean.RData"))
  message("Saved gfw_s5_6_marine_clean.RData")
  
} else {

  load(file.path(flyshootdir, "gfw_s5_6_marine_clean.RData"))
  message("Loaded gfw_s5_6_marine_clean.RData: ",
          nrow(events_marine_clean), " events")

}

load(file.path(flyshootdir, "gfw_s5_6_marine_clean.RData"))
message("Loaded gfw_s5_6_marine_clean.RData: ",
        nrow(events_marine_clean), " events")
message("events_marine_clean loaded: ",
        format(object.size(events_marine_clean), units = "MB"))
gc()





# ============================================================
# ---- Minimal load for Steps 7-11 (post-events processing) ----
# ============================================================

# You do NOT need events_df, discovery_events, eez_sf, fao_sf
# once events_marine_clean and fishing_days_study are built

# Load only what Steps 8-11 need
vessel_meta <- local({
  e <- new.env()
  load(file.path(flyshootdir, "gfw_s3_vessel_meta.RData"), envir = e)
  e$vessel_meta
})
gc()

fishing_days_study <- local({
  e <- new.env()
  load(file.path(flyshootdir, "gfw_s8_effort.RData"), envir = e)
  e$fishing_days_study
})
gc()
message("fishing_days_study loaded: ",
        format(object.size(fishing_days_study), units = "MB"))

# For Step 9 (AIS coverage diagnostics) you also need:
gfw_vessel_periods <- local({
  e <- new.env()
  load(file.path(flyshootdir, "gfw_s2_registry.RData"), envir = e)
  e$gfw_vessel_periods
})
gc()
message("gfw_vessel_periods loaded: ",
        format(object.size(gfw_vessel_periods), units = "MB"))

discovery_events <- local({
  e <- new.env()
  load(file.path(flyshootdir, "gfw_s2_discovery.RData"), envir = e)
  e$discovery_events
})
gc()
message("discovery_events loaded: ",
        format(object.size(discovery_events), units = "MB"))
# rm(discovery_events)

# For Step 10 (vessel coverage chart):
# gfw_vessel_periods already loaded above

# For Step 11 (effort plots):
# fishing_days_study already loaded above



# ============================================================
# STEP 7 — Coastal zone classification (0-12NM vs >12NM)
# ============================================================
# Output: events_classified
# File:   gfw_s7_zones.RData

if (recreate_s7) {

  sf_use_s2(FALSE)
  events_classified <-
    events_marine_clean %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    mutate(zone = if_else(
      lengths(st_intersects(., territorial_sea)) > 0,
      "Coastal (0-12NM)", "Offshore (>12NM)"
    )) %>%
    st_drop_geometry()
  sf_use_s2(TRUE)

  events_classified %>%
    count(zone) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()

  sf_use_s2(FALSE)
  events_classified_fr <- events_classified %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_join(french_zones, join = st_within) %>%
    st_drop_geometry() %>%
    # For non-French waters zone_fr will be NA — fill with zone labels
    mutate(
      zone_detail = case_when(
        !is.na(zone_fr)          ~ zone_fr,           # French sub-zones
        zone == "Coastal (0-12NM)" ~ "0-12NM (other)", # non-French coastal
        TRUE                       ~ "Offshore (>12NM)"
      )
    )
  sf_use_s2(TRUE)
  
  # Check coverage
  events_classified_fr %>%
    count(zone_detail, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()
  
  # rm(events_marine_clean)
  save(events_classified, events_classified_fr, 
       file = file.path(flyshootdir, "gfw_s7_zones.RData"))
  message("Saved gfw_s7_zones.RData")

} else {

  load(file.path(flyshootdir, "gfw_s7_zones.RData"))
  message("Loaded gfw_s7_zones.RData: ",
          nrow(events_classified), " events")

}


# ============================================================
# STEP 8 — Fishing days aggregation
# ============================================================
# A fishing day = one distinct calendar date with at least one
# fishing event per vessel × division × zone × year × quarter.
# Uses n_distinct(date) to avoid double-counting multiple events
# on the same day.
#
# Input:  events_classified_fr — from Step 7 (marine only,
#         study divisions, coastal/offshore zone, French CFP zones)
# Output: fishing_days_study
# File:   gfw_s8_effort.RData

if (recreate_s8) {
  
  fishing_days_study <-
    events_classified_fr %>%
    mutate(date    = as.Date(start),
           year    = year(date),
           quarter = quarter(date)) %>%
    dplyr::select(-any_of(c("vessel_name", "vessel_flag"))) %>%
    left_join(vessel_meta, by = "vesselId",
              relationship = "many-to-one") %>%
    filter(!is.na(ssvid)) %>%
    distinct(ssvid, vessel_name, vessel_flag, gear, gt, gt_final,
             size_class, division, zone, zone_fr,
             year, quarter, date) %>%
    group_by(ssvid, vessel_name, vessel_flag, gear, gt, gt_final,
             size_class, division, zone, zone_fr,
             year, quarter) %>%
    summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
    mutate(gt_fishing_days = fishing_days * gt_final)
  
  message("fishing_days_study: ", nrow(fishing_days_study), " rows | ",
          n_distinct(fishing_days_study$ssvid), " vessels | ",
          sum(fishing_days_study$fishing_days), " total days")
  message("NA ssvid: ", sum(is.na(fishing_days_study$ssvid)))
  message("NA flag:  ", sum(is.na(fishing_days_study$vessel_flag)))
  message("NA gear:  ", sum(is.na(fishing_days_study$gear)))
  
  # Zone breakdown
  fishing_days_study %>%
    group_by(zone) %>%
    summarise(
      fishing_days = sum(fishing_days),
      n_vessels    = n_distinct(ssvid),
      pct          = round(fishing_days /
                             sum(fishing_days_study$fishing_days) * 100, 1),
      .groups      = "drop"
    ) %>%
    print()
  
  # French zone breakdown
  fishing_days_study %>%
    mutate(zone_detail = coalesce(zone_fr, zone)) %>%
    group_by(zone_detail) %>%
    summarise(
      fishing_days = sum(fishing_days),
      n_vessels    = n_distinct(ssvid),
      pct          = round(fishing_days /
                             sum(fishing_days_study$fishing_days) * 100, 1),
      .groups      = "drop"
    ) %>%
    arrange(desc(fishing_days)) %>%
    print()
  
  save(fishing_days_study,
       file = file.path(flyshootdir, "gfw_s8_effort.RData"))
  message("Saved gfw_s8_effort.RData")
  
} else {
  
  fishing_days_study <- local({
    e <- new.env()
    load(file.path(flyshootdir, "gfw_s8_effort.RData"), envir = e)
    e$fishing_days_study
  })
  
  message("Loaded gfw_s8_effort.RData: ",
          n_distinct(fishing_days_study$ssvid), " vessels | ",
          sum(fishing_days_study$fishing_days), " total days")
  
}

# apply_flag_other always defined regardless of recreate_s8
apply_flag_other <- function(df, flag_col = "vessel_flag",
                             days_col = "fishing_days") {
  df %>%
    group_by(across(all_of(flag_col))) %>%
    mutate(total_flag_days = sum(.data[[days_col]])) %>%
    ungroup() %>%
    mutate(
      total_days = sum(.data[[days_col]]),
      flag_pct   = total_flag_days / total_days * 100,
      across(all_of(flag_col),
             ~ if_else(flag_pct < threshold_pct, "Other", .x))
    ) %>%
    rename_with(~ str_replace(., "fishing_days", days_col),
                any_of(c("fishing_days", "total_flag_days",
                         "total_days")))
}


# fishing_days_study %>%
#   group_by(gear) %>%
#   summarise(
#     fishing_days = sum(fishing_days),
#     n_vessels    = n_distinct(ssvid),
#     pct_days     = round(fishing_days / sum(fishing_days_study$fishing_days) * 100, 1)
#   ) %>%
#   arrange(desc(fishing_days)) %>%
#   print()

# fishing_days_study %>%
#   group_by(vessel_flag) %>%
#   summarise(
#     fishing_days = sum(fishing_days),
#     n_vessels    = n_distinct(ssvid),
#     pct_days     = round(fishing_days / sum(fishing_days_study$fishing_days) * 100, 1)
#   ) %>%
#   arrange(desc(fishing_days)) %>%
#   print()


# ============================================================
# STEP 10 — Vessel coverage chart (PDF)
# ============================================================

# Check extent of duplication in gfw_vessel_periods
gfw_vessel_periods %>%
  count(vesselId, sort = TRUE) %>%
  filter(n > 1) %>%
  summarise(
    n_duplicate_vesselIds = n(),
    max_copies            = max(n),
    total_extra_rows      = sum(n - 1)
  ) %>%
  print()

# Fix: deduplicate gfw_vessel_periods — one row per vesselId
gfw_vessel_periods_clean <- gfw_vessel_periods %>%
  group_by(vesselId) %>%
  arrange(desc(!is.na(shipname))) %>%   # prefer rows with a shipname
  slice(1) %>%
  ungroup()

vessel_coverage <- gfw_vessel_periods_clean %>%
  left_join(
    vessel_meta %>% dplyr::select(vesselId, gear, vessel_flag),
    by = "vesselId"
  ) %>%
  mutate(
    date_from       = as.Date(substr(transmissionDateFrom, 1, 10)),
    date_to         = as.Date(substr(transmissionDateTo,   1, 10)),
    bar_width_years = as.numeric(date_to - date_from) / 365,
    bar_text        = if_else(bar_width_years >= 1.5,
                              paste0(shipname, " (", vessel_flag, ")"), "")
  ) %>%
  group_by(ssvid) %>%
  mutate(
    recent_name = shipname[which.max(date_to)],
    recent_flag = vessel_flag[which.max(date_to)],
    y_label     = paste0(recent_name, "\n", ssvid)
  ) %>%
  ungroup() %>%
  semi_join(fishing_days_study %>% dplyr::select(ssvid), by = "ssvid") %>%
  arrange(recent_flag, recent_name, ssvid, date_from) %>%
  mutate(y_label = factor(y_label, levels = unique(y_label)))

message("vessel_coverage: ", nrow(vessel_coverage), " rows | ",
        n_distinct(vessel_coverage$ssvid), " vessels")

flag_levels_all  <- sort(unique(na.omit(vessel_coverage$vessel_flag)))
flag_colours_all <- setNames(
  colorRampPalette(c(
    "#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#76B7B2",
    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
  ))(length(flag_levels_all)),
  flag_levels_all
)
flag_colours_all[names(flag_colours[names(flag_colours) != "Other"])] <-
  flag_colours[names(flag_colours) != "Other"]

rows_per_page <- 30
all_labels    <- levels(vessel_coverage$y_label)
n_pages       <- ceiling(length(all_labels) / rows_per_page)

pdf(
  file   = file.path(flyshootdir,
                     paste0("vessel_coverage_", Sys.Date(), ".pdf")),
  width  = 14,
  height = max(8, rows_per_page * 0.4)
)

for (p in seq_len(n_pages)) {
  row_start   <- (p - 1) * rows_per_page + 1
  row_end     <- min(p * rows_per_page, length(all_labels))
  page_labels <- all_labels[row_start:row_end]

  df_page <- vessel_coverage %>%
    filter(y_label %in% page_labels) %>%
    mutate(y_label = factor(y_label, levels = page_labels))

  print(
    ggplot(df_page,
           aes(xmin = date_from, xmax = date_to,
               ymin = as.numeric(y_label) - 0.4,
               ymax = as.numeric(y_label) + 0.4,
               fill = vessel_flag)) +
      theme_bw() +
      geom_rect(colour = "white", linewidth = 0.4) +
      geom_text(
        aes(x     = date_from + (date_to - date_from) / 2,
            y     = as.numeric(y_label),
            label = bar_text),
        size = 2.5, colour = "white", fontface = "bold",
        hjust = 0.5, vjust = 0.5
      ) +
      scale_fill_manual(values = flag_colours_all, na.value = "grey70") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits      = as.Date(c("2012-01-01", "2026-12-31"))) +
      scale_y_continuous(
        breaks = seq_along(page_labels),
        labels = page_labels,
        expand = expansion(add = 0.6)
      ) +
      theme(
        axis.text.y        = element_text(size = 7, family = "mono"),
        axis.text.x        = element_text(size = 9),
        axis.title         = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "bottom"
      ) +
      labs(
        fill     = "Flag",
        title    = paste0("Vessel fishing activity (", p, "/", n_pages, ")"),
        subtitle = "One row = one MMSI  |  segments show name/flag changes over time"
      )
  )
}

dev.off()
message("Vessel coverage PDF saved")

# Vessel coverage Excel export
# One sheet per flag + one combined sheet
# Each row = one vesselId period with full vessel characteristics
# and total fishing days from fishing_days_study

# ---- Build export table ----
vessel_coverage_export <- vessel_coverage %>%
  # Add vessel characteristics from vessel_meta
  left_join(
    vessel_meta %>%
      dplyr::select(vesselId, gt, length_m, size_class,
                    size_source, gear, vessel_name),
    by = "vesselId"
  ) %>%
  # Resolve gear name conflict — gear.x from vessel_coverage,
  # gear.y from vessel_meta (prefer vessel_meta as it has corrections)
  rename(
    gear_registry = gear.y,   # from vessel_meta (corrected)
    gear_gfw      = gear.x    # from vessel_coverage (original GFW)
  ) %>%
  # Add total fishing days per ssvid from fishing_days_study
  left_join(
    fishing_days_study %>%
      group_by(ssvid) %>%
      summarise(
        total_fishing_days  = sum(fishing_days),
        first_year_activity = min(year),
        last_year_activity  = max(year),
        divisions           = paste(sort(unique(division)),
                                    collapse = ", "),
        gears_observed      = paste(sort(unique(na.omit(gear))),
                                    collapse = ", "),
        .groups             = "drop"
      ),
    by = "ssvid"
  ) %>%
  dplyr::select(
    # MMSI level
    ssvid,
    recent_name,
    recent_flag,
    total_fishing_days,
    first_year_activity,
    last_year_activity,
    divisions,
    # vesselId period level
    vesselId,
    shipname,
    vessel_flag,
    vessel_name,
    gear_registry,
    gear_gfw,
    gt,
    length_m,
    size_class,
    size_source,
    date_from,
    date_to,
    bar_width_years,
    gears_observed
  ) %>%
  mutate(
    bar_width_years = round(bar_width_years, 2),
    date_from       = as.Date(date_from),
    date_to         = as.Date(date_to)
  ) %>%
  arrange(recent_flag, recent_name, ssvid, date_from)


message("vessel_coverage_export: ", nrow(vessel_coverage_export),
        " rows | ", n_distinct(vessel_coverage_export$ssvid), " vessels")

# ---- Split into per-flag sheets ----
flag_sheets <- vessel_coverage_export %>%
  mutate(sheet_flag = if_else(is.na(recent_flag), "Unknown", recent_flag)) %>%
  split(.$sheet_flag)

# ---- Combined sheet summary (one row per MMSI) ----
vessel_summary <- vessel_coverage_export %>%
  group_by(ssvid, recent_name, recent_flag,
           total_fishing_days,
           first_year_activity, last_year_activity,
           divisions, gears_observed) %>%
  summarise(
    n_vessel_ids    = n_distinct(vesselId),
    n_name_changes  = n_distinct(shipname) - 1,
    n_flag_changes  = n_distinct(vessel_flag) - 1,
    gt_median       = round(median(gt,       na.rm = TRUE), 1),
    length_median   = round(median(length_m, na.rm = TRUE), 1),
    size_class      = first(na.omit(size_class)),
    first_period    = min(date_from),
    last_period     = max(date_to),
    .groups         = "drop"
  ) %>%
  arrange(recent_flag, recent_name, ssvid)

# ---- Write Excel file ----
excel_sheets <- c(
  list(
    "All vessels"     = vessel_coverage_export,
    "Summary by MMSI" = vessel_summary
  ),
  flag_sheets
)

writexl::write_xlsx(
  excel_sheets,
  path = file.path(flyshootdir,
                   paste0("vessel_coverage_", Sys.Date(), ".xlsx"))
)
message("Vessel coverage Excel saved: ",
        length(excel_sheets), " sheets | ",
        n_distinct(vessel_coverage_export$ssvid), " vessels")


library(openxlsx)

# ---- Colour scheme by data source ----
# GFW registry (gfw_vessel_registry / gfw_vessel_periods) — blue
# vessel_meta (corrected) — green  
# fishing_days_study (derived) — orange
# MMSI-level identifiers — grey

# ---- Colour scheme by data source ----
col_gfw      <- "#BDD7EE"   # light blue
col_meta     <- "#E2EFDA"   # light green
col_effort   <- "#FCE4D6"   # light orange
col_id       <- "#F2F2F2"   # light grey

source_colours <- c(
  "id"     = col_id,
  "gfw"    = col_gfw,
  "meta"   = col_meta,
  "effort" = col_effort
)

# ---- Column source mapping ----
col_sources <- tribble(
  ~column,               ~source,
  "ssvid",               "id",
  "recent_name",         "gfw",
  "recent_flag",         "gfw",
  "total_fishing_days",  "effort",
  "first_year_activity", "effort",
  "last_year_activity",  "effort",
  "divisions",           "effort",
  "vesselId",            "gfw",
  "shipname",            "gfw",
  "vessel_flag",         "gfw",
  "vessel_name",         "meta",
  "gear_registry",       "meta",
  "gear_gfw",            "gfw",
  "gt",                  "meta",
  "length_m",            "meta",
  "size_class",          "meta",
  "size_source",         "meta",
  "date_from",           "gfw",
  "date_to",             "gfw",
  "bar_width_years",     "gfw",
  "gears_observed",      "effort"
)


# ---- Helper: apply header colours to a sheet ----
apply_header_colours <- function(wb, sheet_name, df) {
  
  cols <- names(df)
  
  for (j in seq_along(cols)) {
    col_name   <- cols[j]
    src        <- col_sources$source[col_sources$column == col_name]
    fill_color <- if (length(src) > 0) source_colours[src] else col_id
    
    addStyle(wb, sheet_name,
             style = createStyle(
               fgFill     = fill_color,
               fontColour = "#000000",
               textDecoration = "bold",
               border     = "Bottom",
               borderColour = "#999999",
               wrapText   = FALSE
             ),
             rows = 1, cols = j,
             gridExpand = FALSE)
  }
}

# ---- Build workbook ----
wb <- createWorkbook()

# Style for data rows — alternating for readability
row_style_odd  <- createStyle(fgFill = "#FFFFFF")
row_style_even <- createStyle(fgFill = "#F7F7F7")
date_style     <- createStyle(numFmt = "YYYY-MM-DD")
num_style      <- createStyle(numFmt = "#,##0")

# ---- Function to write a formatted sheet ----
write_sheet <- function(wb, sheet_name, df) {
  
  addWorksheet(wb, sheet_name)
  
  # Write data
  writeData(wb, sheet_name, df, startRow = 1, headerStyle = createStyle(
    fontColour = "#000000", textDecoration = "bold"
  ))
  
  # Apply source colour to headers
  apply_header_colours(wb, sheet_name, df)
  
  # Column widths
  setColWidths(wb, sheet_name, cols = seq_along(names(df)),
               widths = "auto")
  
  # Freeze top row
  freezePane(wb, sheet_name, firstRow = TRUE)
  
  # Date formatting
  date_cols <- which(names(df) %in% c("date_from", "date_to"))
  if (length(date_cols) > 0 && nrow(df) > 0) {
    addStyle(wb, sheet_name, date_style,
             rows = 2:(nrow(df) + 1),
             cols = date_cols,
             gridExpand = TRUE)
  }
  
  # Number formatting for fishing days
  num_cols <- which(names(df) %in%
                      c("total_fishing_days", "gt", "bar_width_years"))
  if (length(num_cols) > 0 && nrow(df) > 0) {
    addStyle(wb, sheet_name, num_style,
             rows = 2:(nrow(df) + 1),
             cols = num_cols,
             gridExpand = TRUE)
  }
}

# ---- Legend sheet ----
addWorksheet(wb, "Legend")
legend_df <- tibble(
  Colour      = c("Light blue", "Light green", "Light orange", "Light grey"),
  Source      = c("GFW registry / vessel periods",
                  "vessel_meta (manually corrected)",
                  "fishing_days_study (derived effort)",
                  "MMSI identifier"),
  Columns     = c(
    "recent_name, recent_flag, vesselId, shipname, vessel_flag, gear_gfw, date_from, date_to",
    "vessel_name, gear_registry, gt, length_m, size_class, size_source",
    "total_fishing_days, first_year_activity, last_year_activity, divisions, gears_observed",
    "ssvid"
  ),
  Description = c(
    "Data as returned by GFW API — may contain errors or gaps",
    "Data after manual review and correction via Excel correction files",
    "Aggregated from fishing events — reflects actual observed activity",
    "MMSI number — primary vessel identifier"
  )
)
writeData(wb, "Legend", legend_df)

# Colour the legend cells to match
for (i in seq_len(nrow(legend_df))) {
  fill <- c(col_gfw, col_meta, col_effort, col_id)[i]
  addStyle(wb, "Legend",
           style = createStyle(fgFill = fill),
           rows = i + 1, cols = 1:4,
           gridExpand = TRUE)
}
setColWidths(wb, "Legend", cols = 1:4, widths = c(15, 35, 60, 50))
freezePane(wb, "Legend", firstRow = TRUE)

# ---- All vessels sheet ----
write_sheet(wb, "All vessels",     vessel_coverage_export)
write_sheet(wb, "Summary by MMSI", vessel_summary)

# ---- Per-flag sheets ----
flag_sheets <- vessel_coverage_export %>%
  mutate(sheet_flag = if_else(is.na(recent_flag), "Unknown", recent_flag)) %>%
  split(.$sheet_flag)

for (flag_name in sort(names(flag_sheets))) {
  write_sheet(wb, flag_name, flag_sheets[[flag_name]])
}

# ---- Save ----
saveWorkbook(wb,
             file = file.path(flyshootdir,
                              paste0("vessel_coverage_", Sys.Date(), ".xlsx")),
             overwrite = TRUE)

message("Vessel coverage Excel saved with colour coding:")
message("  Sheets: Legend + All vessels + Summary by MMSI + ",
        length(flag_sheets), " flag sheets")
message("  Columns colour coded: blue=GFW | green=vessel_meta | ",
        "orange=effort | grey=MMSI")

# ============================================================
# STEP 11 — Effort plots
# ============================================================

# ---- Plot 1: fishing days by year and flag (absolute) ----
fishing_days_study %>%
  filter(year %in% study_years) %>% 
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country")

# ---- Plot 2: fishing days by year and flag, faceted by division ----
fishing_days_study %>%
  filter(!is.na(division)) %>%
  filter(year %in% study_years) %>% 
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days at sea by year, country and ICES division") +
  facet_wrap(~ division)

# ---- Plot 3: fishing days by year and flag (%) ----
fishing_days_study %>%
  filter(year %in% study_years) %>% 
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year and country (%)")

# ---- Plot 4: fishing days by year and flag, faceted by division (%) ----
fishing_days_study %>%
  filter(!is.na(division)) %>%
  filter(year %in% study_years) %>% 
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year, division) %>%
  mutate(pct = fishing_days / sum(fishing_days) * 100) %>%
  ggplot(aes(x = year, y = pct, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "% Fishing days", fill = "Flag",
       title = "Fishing days at sea by year, country and ICES division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: fishing days by year and gear, faceted by division ----
fishing_days_study %>%
  filter(!is.na(division), !is.na(gear)) %>%
  filter(year %in% study_years) %>% 
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  group_by(gear) %>%
  mutate(total_gear_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(
    total_days = sum(fishing_days),
    gear_pct   = total_gear_days / total_days * 100,
    gear       = if_else(gear_pct < threshold_pct, "Other", gear)
  ) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = gear)) +
  theme_bw() + geom_col() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Gear type",
       title = "Fishing days at sea by year and gear type") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 6: GT fishing days by year and flag ----
fishing_days_study %>%
  filter(!is.na(gt_final)) %>%
  filter(year %in% study_years) %>% 
  group_by(year, vessel_flag) %>%
  summarise(gt_fishing_days = sum(gt_fishing_days, na.rm = TRUE),
            .groups = "drop") %>%
  apply_flag_other(days_col = "gt_fishing_days") %>%
  group_by(year, vessel_flag) %>%
  summarise(gt_fishing_days = sum(gt_fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = gt_fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "GT fishing days", fill = "Flag",
       title = "GT fishing days at sea by year and country")

# ---- Plot 7: fishing days by year and size class ----
fishing_days_study %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  filter(year %in% study_years) %>% 
  group_by(year, size_class) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = size_class)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Size class",
       title = "Fishing days at sea by year and vessel size class")

# ---- Plot 8: fishing days by year and size class, faceted by division ----
fishing_days_study %>%
  filter(!is.na(size_class), size_class != "Unknown", !is.na(division)) %>%
  filter(year %in% study_years) %>% 
  group_by(year, size_class, division) %>%
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = size_class)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Size class",
       title = "Fishing days at sea by year, size class and ICES division") +
  facet_wrap(~ division)

# ---- Plot 9: fishing events map — sample coloured by flag ----
events_sample <- events_classified %>%
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
            by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                   ~ "Other",
    vessel_flag %in% names(flag_colours) ~ vessel_flag,
    TRUE                                 ~ "Other"
  )) %>%
  slice_sample(n = 20000)

ggplot() +
  theme_bw() +
  geom_sf(data = fao_sf_division, fill = NA,
          colour = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90",
          colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample,
             aes(x = lon, y = lat, colour = vessel_flag),
             size = 0.5, alpha = 0.9) +
  scale_colour_manual(values = flag_colours, na.value = "grey70") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom") +
  labs(colour = "Flag",
       title  = "Fishing events — marine only (sample of 5000)",
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E")






events_sample_by_year <- events_classified %>%
  mutate(year = lubridate::year(start)) %>% 
  filter(year %in% study_years) %>% 
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
            by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                   ~ "Other",
    vessel_flag %in% names(flag_colours) ~ vessel_flag,
    TRUE                                 ~ "Other"
  )) %>%
  group_by(year) %>% 
  slice_sample(n = 10000)

ggplot() +
  theme_bw() +
  geom_sf(data = fao_sf_division, fill = NA,
          colour = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90",
          colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample_by_year,
             aes(x = lon, y = lat, colour = vessel_flag),
             size = 0.5, alpha = 0.9) +
  scale_colour_manual(values = flag_colours, na.value = "grey70") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom") +
  labs(colour = "Flag",
       title  = "Fishing events — marine only (sample of 5000)",
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E") +
  facet_wrap(~year)


# ---- Plot 10: fishing events map — coastal vs offshore ----
events_sample_zone <- events_classified %>%
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
            by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                   ~ "Other",
    vessel_flag %in% names(flag_colours) ~ vessel_flag,
    TRUE                                 ~ "Other"
  )) %>%
  slice_sample(n = 20000)

ggplot() +
  theme_bw() +
  geom_sf(data = territorial_sea, fill = "lightyellow",
          alpha = 0.7, colour = NA) +
  geom_sf(data = fao_sf_division, fill = NA,
          colour = "grey60", linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90",
          colour = "grey70", linewidth = 0.2) +
  geom_point(data = events_sample_zone,
             aes(x = lon, y = lat, colour = zone),
             size = 0.5, alpha = 0.5) +
  scale_colour_manual(values = c("Coastal (0-12NM)" = "#E15759",
                                 "Offshore (>12NM)"  = "#4E79A7")) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom") +
  labs(colour = "Zone",
       title  = "Fishing events by zone (sample of 5000)",
       subtitle = "Yellow = 12NM territorial sea  |  Red = coastal  |  Blue = offshore") +
  facet_wrap(~ zone)




# ============================================================
# STEP 9 — AIS coverage diagnostics
# ============================================================
# Explores whether vessel count increases reflect real fleet
# growth or AIS mandate effects (vessels obliged to install
# AIS at different times depending on size).

message("\n========== AIS COVERAGE DIAGNOSTICS ==========\n")

# ---- 9a: Vessel first appearance by size class ----
# Uses registry transmission dates — does not require events_df
vessel_first_appearance <- gfw_vessel_periods %>%
  mutate(first_year = year(as.Date(substr(transmissionDateFrom, 1, 10)))) %>%
  group_by(vesselId) %>%
  summarise(first_year = min(first_year, na.rm = TRUE), .groups = "drop")

vessel_appearance_profile <- vessel_meta %>%
  distinct(vesselId, ssvid, vessel_flag, size_class, gt, length_m) %>%
  left_join(vessel_first_appearance, by = "vesselId") %>%
  filter(!is.na(first_year))

# Summary table: when did each size class first appear?
vessel_appearance_profile %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  group_by(size_class) %>%
  summarise(
    n_vessels        = n_distinct(ssvid),
    median_first_yr  = median(first_year, na.rm = TRUE),
    pct_before_2016  = round(mean(first_year < 2016)  * 100, 1),
    pct_2016_to_2019 = round(mean(first_year >= 2016 &
                                    first_year <= 2019) * 100, 1),
    pct_after_2019   = round(mean(first_year > 2019)  * 100, 1),
    .groups          = "drop"
  ) %>%
  print()

# Plot: first appearance year by size class
vessel_appearance_profile %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  group_by(size_class, first_year) %>%
  summarise(n_vessels = n_distinct(ssvid), .groups = "drop") %>%
  ggplot(aes(x = first_year, y = n_vessels, fill = size_class)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~ size_class, scales = "free_y") +
  labs(x = "First year in GFW data", y = "Number of vessels",
       fill = "Size class",
       title = "Year of first appearance in GFW data by vessel size class",
       subtitle = "Peak in 2016-2019 for smaller classes suggests AIS mandate effect")

# Plot: first appearance by flag and size class
vessel_appearance_profile %>%
  filter(vessel_flag %in% c("NLD", "GBR", "BEL", "FRA", "DEU"),
         !is.na(size_class), size_class != "Unknown") %>%
  group_by(vessel_flag, size_class, first_year) %>%
  summarise(n_vessels = n_distinct(ssvid), .groups = "drop") %>%
  ggplot(aes(x = first_year, y = n_vessels, fill = size_class)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_grid(vessel_flag ~ size_class, scales = "free_y") +
  labs(x = "First year in GFW data", y = "Number of vessels",
       fill = "Size class",
       title = "First appearance year by flag and size class",
       subtitle = "National AIS mandate timing differences visible here")

# ---- 9b: Core fleet vs transient vessels ----
core_fleet <- discovery_events %>%
  group_by(vessel_ssvid) %>%
  filter(n_distinct(reference_date) == 4) %>%
  pull(vessel_ssvid) %>%
  unique()

message("Core fleet (all 4 periods): ", length(core_fleet), " vessels")

fishing_days_study %>%
  mutate(fleet = if_else(ssvid %in% core_fleet,
                         "Core fleet (all 4 periods)",
                         "Transient vessels")) %>%
  group_by(year, fleet) %>%
  summarise(
    fishing_days    = sum(fishing_days),
    n_vessels       = n_distinct(ssvid),
    .groups         = "drop"
  ) %>%
  mutate(days_per_vessel = fishing_days / n_vessels) %>%
  pivot_longer(c(fishing_days, n_vessels, days_per_vessel),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = year, y = value, colour = fleet)) +
  theme_bw() +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "Core fleet (all 4 periods)" = "#4E79A7",
    "Transient vessels"          = "#E15759"
  )) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "", colour = "Fleet segment",
       title = "Core fleet vs transient vessels — effort and vessel count by year",
       subtitle = "Flat days/vessel in core fleet confirms AIS coverage drives count increase")

# ---- 9c: Effort intensity — days per vessel per year ----
fishing_days_study %>%
  group_by(year, vessel_flag) %>%
  summarise(
    fishing_days    = sum(fishing_days),
    n_vessels       = n_distinct(ssvid),
    days_per_vessel = fishing_days / n_vessels,
    .groups         = "drop"
  ) %>%
  filter(vessel_flag %in% c("NLD", "GBR", "FRA", "BEL", "DEU")) %>%
  ggplot(aes(x = year, y = days_per_vessel, colour = vessel_flag)) +
  theme_bw() +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = flag_colours) +
  scale_x_continuous(breaks = 2012:2025) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days per vessel", colour = "Flag",
       title = "Fishing effort intensity — days per vessel per year",
       subtitle = "Flat/declining trend = AIS coverage artefact; rising = real intensification")

# ---- 9d: Completeness summary ----
completeness_summary <- function(df, label) {
  message("--- ", label, " (", nrow(df), " rows) ---")
  df %>%
    summarise(
      n_total        = n(),
      n_missing_name = sum(is.na(vessel_name)),
      n_missing_flag = sum(is.na(vessel_flag)),
      n_missing_gear = sum(is.na(gear)),
      n_missing_gt   = sum(is.na(gt)),
      pct_name       = round(n_missing_name / n_total * 100, 1),
      pct_flag       = round(n_missing_flag / n_total * 100, 1),
      pct_gear       = round(n_missing_gear / n_total * 100, 1),
      pct_gt         = round(n_missing_gt   / n_total * 100, 1)
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    print()
}

completeness_summary(vessel_meta,        "vessel_meta")
completeness_summary(fishing_days_study, "fishing_days_study")

fishing_days_study %>%
  group_by(year) %>%
  summarise(
    total_days = sum(fishing_days),
    n_vessels  = n_distinct(ssvid),
    n_flags    = n_distinct(vessel_flag, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  print(n = 20)

fishing_days_study %>%
  group_by(zone) %>%
  summarise(
    fishing_days = sum(fishing_days),
    n_vessels    = n_distinct(ssvid),
    .groups      = "drop"
  ) %>%
  mutate(pct = round(fishing_days / sum(fishing_days) * 100, 1)) %>%
  print()

