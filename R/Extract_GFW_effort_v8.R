# ============================================================
# GFW Effort Analysis — gfwr v3.0
# Version 8
#
# Each slow step is controlled by a boolean flag at the top:
#   recreate_* = TRUE  → re-run the step and overwrite saved data
#   recreate_* = FALSE → load saved data from disk, skip the step
#
# Processing steps:
#   STEP 1  — Setup: libraries, paths, spatial layers, settings
#   STEP 2a — Vessel discovery: query GFW → mmsi_master
#   STEP 2b — Vessel registry: MMSI → vesselId + metadata
#   STEP 3  — Vessel metadata: build vessel_meta + corrections + GT imputation
#   STEP 4  — Fishing events: pull 2013-2025 per vesselId (parallel)
#   STEP 5  — Spatial filter: division join + EEZ + internal waters
#   STEP 6  — Zone classification: 0-12NM vs >12NM + French CFP sub-zones
#   STEP 7  — Fishing days: aggregate to day-level effort (majority-rule dedup)
#   STEP 8  — Diagnostics: completeness + AIS coverage checks
#   STEP 9  — Vessel coverage: PDF chart + Excel export
#   STEP 10 — Effort plots
#
# Saved RData files (one per step):
#   gfw_s2_discovery.RData   — discovery_events, mmsi_master
#   gfw_s2_registry.RData    — gfw_vessel_registry, gfw_vessel_periods
#   gfw_s3_vessel_meta.RData — vessel_meta
#   gfw_s4_events.RData      — events_df
#   gfw_s5_marine.RData      — events_marine_clean
#   gfw_s6_zones.RData       — events_classified_fr
#   gfw_s7_effort.RData      — fishing_days_study
#
# Recommended first run: set all recreate_* = TRUE
# Re-run after vessel_meta corrections: recreate_s3 + recreate_s7 = TRUE
# Re-run after new events: recreate_s4 through recreate_s7 = TRUE
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
library(furrr)
library(future)
library(progressr)

key <- gfw_auth()

spatialdir  <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
flyshootdir <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"
gisdir      <- "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/GIS"

source("R/FLYSHOOT utils.r")

# ---- Recreation flags ----
# Set TRUE to re-run and overwrite; FALSE to load from disk

recreate_s2_discovery <- FALSE   # vessel discovery (gfw_event per ref year)
recreate_s2_registry  <- FALSE   # vessel registry  (gfw_vessel_info per MMSI)
recreate_s3           <- FALSE   # vessel_meta + manual corrections + GT
recreate_s4           <- FALSE   # fishing events   (gfw_event per vesselId)
recreate_s5           <- FALSE   # spatial filter   (division + EEZ + waters)
recreate_s6           <- FALSE   # zone classification (12NM + French CFP)
recreate_s7           <- FALSE   # fishing days aggregation

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

# Combined study area polygon — used in Step 2a vessel discovery
study_area_polygon <- fao_sf_division %>%
  st_union() %>%
  st_as_sf()

eez_sf <- loadRData(file.path(spatialdir, "eez_sf.RData"))

load(file.path(spatialdir, "internal_waters.RData"))
load(file.path(spatialdir, "territorial_sea.RData"))
load(file.path(spatialdir, "French 3NM 6NM 12NM zones.RData"))

study_eez <- eez_sf %>%
  dplyr::select(eez = ISO_SOV1, geometry) %>%
  filter(eez %in% c("GBR", "NLD", "BEL", "FRA", "DEU"))

# ---- Plot settings ----

flag_colours <- c(
  "BEL"   = "#4E79A7",
  "NLD"   = "#F28E2B",
  "GBR"   = "#59A14F",
  "FRA"   = "#E15759",
  "DEU"   = "#76B7B2",
  # "DNK"   = "#EDC948",
  # "NOR"   = "#B07AA1",
  # "IRL"   = "#FF9DA7",
  "Other" = "#BAB0AC"
)

size_colours <- c(
  "S1 <100 GT"     = "#1D9E75",
  "S2 100-300 GT"  = "#BA7517",
  "S3 300-600 GT"  = "#7F77DD",
  "S4 600-1200 GT" = "#D85A30",
  "S5 >1200 GT"    = "#A32D2D"
)

threshold_pct <- 1      # % threshold for "Other" flag grouping in plots
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

# ---- Helper functions ----

# Load a single named object from an RData file without polluting environment
load_object <- function(filepath, object_name) {
  local({
    e <- new.env()
    load(filepath, envir = e)
    if (!object_name %in% ls(e))
      stop("Object '", object_name, "' not found in ", basename(filepath),
           "\nAvailable: ", paste(ls(e), collapse = ", "))
    e[[object_name]]
  })
}

# Collapse minor flags into "Other" for plots
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
                any_of(c("fishing_days", "total_flag_days", "total_days")))
}


# ============================================================
# STEP 2a — Vessel discovery
# ============================================================
# Queries GFW for all vessels that fished in the study area
# during four reference windows (30 days each).
# Uses official ICES division polygons for the spatial filter.
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

  save(discovery_events, mmsi_master,
       file = file.path(flyshootdir, "gfw_s2_discovery.RData"))
  message("Saved gfw_s2_discovery.RData")

} else {

  load(file.path(flyshootdir, "gfw_s2_discovery.RData"))
  message("Loaded gfw_s2_discovery.RData: ", nrow(mmsi_master), " MMSIs")

}

# ============================================================
# STEP 2b — Vessel registry
# ============================================================
# For each MMSI, retrieves all vesselId periods and registry
# metadata (gear, GT, length, IMO) from GFW.
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


# ============================================================
# STEP 3 — Vessel metadata
# ============================================================
# Builds vessel_meta from registry and applies manual corrections.
# Must run before Step 4 — vessel_meta defines which vesselIds
# are queried for effort data.
#
# Sub-steps:
#   3a — Build base vessel_meta from registry
#   3b — Apply gear corrections (non-fishing vessel fixes)
#   3c — Apply missing gear assignments (manual Excel)
#   3d — Validation checks
#   3e — Final catch-all imputation (gear, size class, flag)
#   3f — GT imputation (median by size class × flag × length)
#
# Output: vessel_meta
# File:   gfw_s3_vessel_meta.RData

if (recreate_s3) {

  # ---- 3a: Build base vessel_meta ----
  vessel_meta <- gfw_vessel_registry %>%
    dplyr::select(
      vesselId, ssvid, shipname,
      transmissionDateFrom, transmissionDateTo,
      vessel_flag = flag, geartypes, tonnageGt, lengthM, imo
    ) %>%
    distinct() %>%
    left_join(
      mmsi_master %>% dplyr::select(mmsi, vessel_name, flag),
      by = c("ssvid" = "mmsi")
    ) %>%
    mutate(vessel_flag = coalesce(vessel_flag, flag)) %>%
    group_by(ssvid) %>%
    mutate(
      best_gear   = first(na.omit(geartypes)),
      best_gt     = median(tonnageGt[tonnageGt > 0], na.rm = TRUE),
      best_length = median(lengthM[lengthM > 0],     na.rm = TRUE),
      best_imo    = first(na.omit(imo[imo != "" & imo != "0"]))
    ) %>%
    ungroup() %>%
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
    mutate(
      size_var = case_when(
        !is.na(gt) & gt > 0             ~ gt,
        !is.na(length_m) & length_m > 0 ~ case_when(
          length_m < 15  ~   50,
          length_m < 20  ~  120,
          length_m < 28  ~  280,
          length_m < 42  ~  550,
          length_m < 70  ~  900,
          TRUE           ~ 1500
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

  # ---- 3b: Apply gear corrections ----
  # Source: vessel_gear_corrections.xlsx
  # action = "exclude" → remove; action = "correct" → overwrite gear/size_class
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
  missing_gear_corrections <- readxl::read_xlsx(
    file.path(flyshootdir, "vessel_gear_missing.xlsx"),
    sheet = "missing_gear"
  ) %>%
    mutate(ssvid = as.character(ssvid)) %>%
    filter(!is.na(gear_corrected) | action == "exclude")

  vessel_meta <- vessel_meta %>%
    left_join(
      missing_gear_corrections %>%
        dplyr::select(ssvid, gear_corrected, size_class_corrected, action),
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

  # ---- 3d: Validation ----
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
  # Remaining NA gear = confirmed small inshore vessels (manual inspection)
  # Assigned OTHER_FISHING / S1 <100 GT with imputation flags for transparency

  # Remove confirmed non-fishing and survey vessels by ssvid
  vessel_meta <- vessel_meta %>%
    filter(gear != "DIVE_VESSEL" | is.na(gear)) %>%
    filter(!ssvid %in% c(
      "205130000",   # BELGICA — Belgian research vessel
      "999999000"    # French survey vessel
    ))

  # Gear and size class defaults
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

  message("After 3e imputation:")
  message("  Remaining NA gear:       ", sum(is.na(vessel_meta$gear)))
  message("  Remaining NA flag:       ", sum(is.na(vessel_meta$vessel_flag)))
  message("  Gear imputed:            ", sum(vessel_meta$gear_imputed,       na.rm = TRUE))
  message("  Size class imputed:      ", sum(vessel_meta$size_class_imputed, na.rm = TRUE))
  message("  Flag imputed from MID:   ", sum(vessel_meta$flag_imputed,       na.rm = TRUE))

  # ---- 3f: GT imputation ----
  # Three-level fallback: size×flag×length → size×flag → size only
  # Median used (GT distributions are right-skewed)
  # gt_final used for GT-day calculations; original gt preserved

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

  gt_lookup_l3 <- gt_base %>%
    group_by(size_class, vessel_flag, length_bracket) %>%
    summarise(n = n(), gt_l3 = round(median(gt), 1), .groups = "drop") %>%
    filter(n >= 3) %>% dplyr::select(-n)

  gt_lookup_l2 <- gt_base %>%
    group_by(size_class, vessel_flag) %>%
    summarise(n = n(), gt_l2 = round(median(gt), 1), .groups = "drop") %>%
    filter(n >= 3) %>% dplyr::select(-n)

  gt_lookup_l1 <- gt_base %>%
    group_by(size_class) %>%
    summarise(gt_l1 = round(median(gt), 1), .groups = "drop")

  message("GT lookup (size class only):")
  print(gt_lookup_l1)

  vessel_meta <- vessel_meta %>%
    length_brackets() %>%
    left_join(gt_lookup_l3, by = c("size_class", "vessel_flag",
                                    "length_bracket")) %>%
    left_join(gt_lookup_l2, by = c("size_class", "vessel_flag")) %>%
    left_join(gt_lookup_l1, by = "size_class") %>%
    mutate(
      gt_imputed = is.na(gt),
      gt_source  = case_when(
        !is.na(gt)    ~ "observed",
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

  save(vessel_meta,
       file = file.path(flyshootdir, "gfw_s3_vessel_meta.RData"))
  message("Saved gfw_s3_vessel_meta.RData: ",
          n_distinct(vessel_meta$ssvid), " vessels | ",
          ncol(vessel_meta), " columns")

} else {

  vessel_meta <- load_object(
    file.path(flyshootdir, "gfw_s3_vessel_meta.RData"), "vessel_meta")

  message("Loaded gfw_s3_vessel_meta.RData: ",
          n_distinct(vessel_meta$ssvid), " vessels | ",
          sum(is.na(vessel_meta$gear)), " NA gear | ",
          sum(is.na(vessel_meta$gt_final)), " NA gt_final")

}


# ============================================================
# STEP 4 — Fishing events: pull 2013-2025 per vesselId
# ============================================================
# Uses vessel_meta to limit API calls to the cleaned vessel list.
# Only queries vesselIds directly seen in discovery_events.
# Year range per vessel clipped to transmission period.
# Parallel processing with checkpoint saves every 100 vessels.
#
# NOTE: No shapefile restriction — speed testing showed server-side
# spatial filtering is ~4x SLOWER than filtering locally in Step 5.
#
# Output: events_df
# File:   gfw_s4_events.RData

if (recreate_s4) {

  # Derive year ranges from registry transmission periods
  vessel_year_ranges <- vessel_meta %>%
    mutate(
      year_from = year(as.Date(substr(transmissionDateFrom, 1, 10))),
      year_to   = year(as.Date(substr(transmissionDateTo,   1, 10)))
    ) %>%
    mutate(
      year_from = pmax(year_from, 2013),
      year_to   = pmin(year_to,   2025)
    ) %>%
    filter(year_from <= year_to) %>%
    group_by(vesselId) %>%
    summarise(year_from = min(year_from), year_to = max(year_to),
              .groups = "drop")

  # Only query vesselIds directly seen in discovery events
  target_vessel_ids_strict <- intersect(
    unique(vessel_meta$vesselId),
    unique(discovery_events$vesselId)
  )

  # Resume support — detect already-completed vessels
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

  calls_remaining <- vessel_year_ranges %>%
    filter(vesselId %in% remaining_ids_to_run) %>%
    mutate(n_years = year_to - year_from + 1) %>%
    summarise(total = sum(n_years)) %>% pull(total)

  message("===== STEP 4 — Fishing events =====")
  message("Target vesselIds:     ", length(target_vessel_ids_strict))
  message("Already completed:    ", length(completed_ids))
  message("Remaining to run:     ", length(remaining_ids_to_run))
  message("Remaining API calls:  ", calls_remaining)
  message("Est. runtime (4 workers): ",
          round(calls_remaining * 24 / 3600 / 4, 1), " hours")

  if (length(remaining_ids_to_run) == 0) {

    message("All vessels already completed — loading from disk")
    events_df <- load_object(
      file.path(flyshootdir, "gfw_s4_events.RData"), "events_df")

  } else {

    n_cores   <- parallel::detectCores()
    n_workers <- 4
    plan(multisession, workers = n_workers)
    message("Available cores: ", n_cores, " | Using workers: ", n_workers)

    vessel_lookup <- vessel_meta %>%
      distinct(vesselId, ssvid, vessel_name, vessel_flag, size_class) %>%
      left_join(vessel_year_ranges, by = "vesselId")

    process_vessel <- function(vid, vessel_lookup, key_token) {
      library(gfwr); library(dplyr); library(purrr)
      key   <- key_token
      vinfo <- vessel_lookup %>% filter(vesselId == vid)
      y_from <- if (nrow(vinfo) > 0 && !is.na(vinfo$year_from)) vinfo$year_from else 2013
      y_to   <- if (nrow(vinfo) > 0 && !is.na(vinfo$year_to))   vinfo$year_to   else 2025
      vname  <- if (nrow(vinfo) > 0 && !is.na(vinfo$vessel_name)) vinfo$vessel_name else "unknown"
      vflag  <- if (nrow(vinfo) > 0 && !is.na(vinfo$vessel_flag)) vinfo$vessel_flag else "?"

      events_v <- map_dfr(y_from:y_to, function(y) {
        Sys.sleep(0.5)
        tryCatch({
          result <- suppressMessages(gfw_event(
            event_type = "FISHING", vessels = vid,
            start_date = paste0(y, "-01-01"),
            end_date   = paste0(y, "-12-31"),
            key        = key
          ))
          if (!is.null(result) && nrow(result) > 0) result else NULL
        }, error = function(e) NULL)
      })

      n_events <- if (is.null(events_v) || nrow(events_v) == 0) 0 else nrow(events_v)
      list(vesselId = vid, vessel_name = vname, vessel_flag = vflag,
           year_from = y_from, year_to = y_to, n_events = n_events,
           events = events_v)
    }

    key_token <- Sys.getenv("GFW_TOKEN")
    if (nchar(key_token) == 0) stop("GFW_TOKEN not found in ~/.Renviron")

    chunk_size    <- 100
    vessel_chunks <- split(remaining_ids_to_run,
                           ceiling(seq_along(remaining_ids_to_run) / chunk_size))
    n_chunks <- length(vessel_chunks)
    n_total  <- length(remaining_ids_to_run)
    chunk_log <- tibble()

    message(sprintf("Processing %d vessels in %d chunks of ~%d",
                    n_total, n_chunks, chunk_size))

    for (chunk_i in seq_along(vessel_chunks)) {
      chunk_ids   <- vessel_chunks[[chunk_i]]
      chunk_start <- Sys.time()
      message(sprintf("\n[Chunk %d/%d] %d vessels | started %s",
                      chunk_i, n_chunks, length(chunk_ids),
                      format(chunk_start, "%H:%M:%S")))

      with_progress({
        p <- progressor(steps = length(chunk_ids))
        chunk_results <- future_map(
          chunk_ids,
          function(vid) { p(sprintf("%s", vid))
            process_vessel(vid, vessel_lookup, key_token) },
          .options = furrr_options(seed = TRUE,
                                   packages = c("gfwr", "dplyr", "purrr"))
        )
      })

      chunk_events <- bind_rows(
        keep(map(chunk_results, "events"), ~ !is.null(.x) && nrow(.x) > 0))

      chunk_summary <- tibble(
        chunk         = chunk_i,
        n_vessels     = length(chunk_ids),
        n_with_events = sum(map_int(chunk_results, "n_events") > 0),
        n_events      = sum(map_int(chunk_results, "n_events")),
        elapsed_min   = round(as.numeric(Sys.time() - chunk_start,
                                         units = "mins"), 1)
      )
      chunk_log <- bind_rows(chunk_log, chunk_summary)
      message(sprintf("  Events: %d | Vessels with events: %d/%d | Time: %.1f min",
                      chunk_summary$n_events, chunk_summary$n_with_events,
                      chunk_summary$n_vessels, chunk_summary$elapsed_min))

      if (!is.null(chunk_events) && nrow(chunk_events) > 0) {
        events_df <- if (exists("events_df") && nrow(events_df) > 0)
          bind_rows(events_df, chunk_events) else chunk_events
      }

      save(events_df, file = file.path(flyshootdir, "gfw_s4_events.RData"))

      vessels_done <- min(chunk_i * chunk_size, n_total)
      eta_hours    <- round((n_total - vessels_done) /
                              (chunk_summary$n_vessels /
                                 chunk_summary$elapsed_min) / 60, 1)
      message(sprintf("  Overall: %d/%d vessels | ETA: %.1f hours | Total events: %d",
                      vessels_done, n_total, eta_hours, nrow(events_df)))
      if (chunk_i %% 5 == 0) { message("\n  --- Chunk log ---"); print(chunk_log) }
    }

    plan(sequential)
    message("\n===== STEP 4 COMPLETE =====")
    message("Total events:    ", nrow(events_df))
    message("Total vesselIds: ", n_distinct(events_df$vesselId))
    print(chunk_log)
    save(events_df, file = file.path(flyshootdir, "gfw_s4_events.RData"))
    message("Saved gfw_s4_events.RData")
  }

} else {

  events_df <- load_object(
    file.path(flyshootdir, "gfw_s4_events.RData"), "events_df")
  message("Loaded gfw_s4_events.RData: ",
          nrow(events_df), " events | ",
          n_distinct(events_df$vesselId), " vesselIds")

}

message("Matched vesselIds (events vs vessel_meta): ",
        n_distinct(intersect(events_df$vesselId, vessel_meta$vesselId)))


# ============================================================
# STEP 5 — Spatial filter
# ============================================================
# Single pipeline: events_df → division join → EEZ filter →
# internal waters removal → events_marine_clean
#
# Output: events_marine_clean
# File:   gfw_s5_marine.RData

if (recreate_s5) {

  sf_use_s2(FALSE)

  events_marine_clean <- events_df %>%
    dplyr::select(-any_of(c("division", "eez", "zone", "zone_fr",
                             "geometry", "internal"))) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_join(fao_sf_division, join = st_within) %>%
    st_join(study_eez,       join = st_within) %>%
    mutate(internal = lengths(st_intersects(., internal_waters)) > 0) %>%
    st_drop_geometry() %>%
    filter(!is.na(division), !is.na(eez), !internal) %>%
    dplyr::select(-internal)

  sf_use_s2(TRUE)
  gc()

  message("events_marine_clean: ", nrow(events_marine_clean), " events | ",
          n_distinct(events_marine_clean$vesselId), " vesselIds")
  events_marine_clean %>%
    count(division, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()

  save(events_marine_clean,
       file = file.path(flyshootdir, "gfw_s5_marine.RData"))
  message("Saved gfw_s5_marine.RData")

} else {

  events_marine_clean <- load_object(
    file.path(flyshootdir, "gfw_s5_marine.RData"), "events_marine_clean")
  message("Loaded gfw_s5_marine.RData: ",
          nrow(events_marine_clean), " events")

}


# ============================================================
# STEP 6 — Zone classification
# ============================================================
# Classifies each event as Coastal (0-12NM) or Offshore (>12NM)
# using the Marineregions 12NM territorial sea shapefile.
# Additionally assigns French CFP sub-zones (0-3NM, 3-6NM, 6-12NM)
# for events within French waters.
#
# Output: events_classified_fr (single object — replaces both
#         events_classified and events_classified_fr from v7)
# File:   gfw_s6_zones.RData

if (recreate_s6) {

  sf_use_s2(FALSE)

  # Step 6a: classify all events as Coastal vs Offshore
  events_with_zone <-
    events_marine_clean %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    mutate(zone = if_else(
      lengths(st_intersects(., territorial_sea)) > 0,
      "Coastal (0-12NM)", "Offshore (>12NM)"
    )) %>%
    st_drop_geometry()

  # Step 6b: add French CFP sub-zones
  events_classified_fr <-
    events_with_zone %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_join(french_zones, join = st_within) %>%
    st_drop_geometry() %>%
    mutate(
      zone_detail = case_when(
        !is.na(zone_fr)              ~ zone_fr,           # French sub-zones
        zone == "Coastal (0-12NM)"   ~ "0-12NM (other)",  # non-French coastal
        TRUE                         ~ "Offshore (>12NM)"
      )
    )

  sf_use_s2(TRUE)

  message("events_classified_fr: ", nrow(events_classified_fr), " events")
  events_classified_fr %>%
    count(zone, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()
  events_classified_fr %>%
    count(zone_detail, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()

  rm(events_with_zone, events_marine_clean)
  gc()

  save(events_classified_fr,
       file = file.path(flyshootdir, "gfw_s6_zones.RData"))
  message("Saved gfw_s6_zones.RData")

} else {

  events_classified_fr <- load_object(
    file.path(flyshootdir, "gfw_s6_zones.RData"), "events_classified_fr")
  message("Loaded gfw_s6_zones.RData: ",
          nrow(events_classified_fr), " events")

}


# ============================================================
# STEP 7 — Fishing days aggregation
# ============================================================
# Aggregates events to fishing days using a MAJORITY-RULE
# deduplication approach:
#
#   Problem: a vessel fishing across a division or zone boundary
#   on a single calendar date would generate multiple rows if
#   grouped by division + zone, inflating the fishing day count
#   vs logbook-based estimates.
#
#   Solution (majority rule): for each vessel × date, identify
#   the spatial combination (division × zone × zone_fr) where
#   the most fishing events were recorded. Assign that date to
#   that combination only — one fishing day per vessel per date.
#
# This ensures consistency with logbook-derived fishing days
# where one sea trip = one day regardless of area crossings.
#
# Output: fishing_days_study
# File:   gfw_s7_effort.RData

if (recreate_s7) {

  # Step 7a: count events per vessel × date × spatial combination
  events_daily_zones <- events_classified_fr %>%
    mutate(date    = as.Date(start),
           year    = year(date),
           quarter = quarter(date)) %>%
    group_by(vesselId, date, year, quarter, division, zone, zone_fr,
             zone_detail) %>%
    summarise(n_events = n(), .groups = "drop")

  # How many vessel-dates span multiple spatial combinations?
  n_split <- events_daily_zones %>%
    group_by(vesselId, date) %>%
    filter(n() > 1) %>%
    n_groups()
  message("Vessel-dates spanning multiple zones (majority rule applied): ",
          n_split)

  # Step 7b: majority rule — keep only the dominant zone per vessel-date
  events_daily_dominant <- events_daily_zones %>%
    group_by(vesselId, date, year, quarter) %>%
    slice_max(n_events, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr::select(-n_events)

  # Step 7c: join vessel metadata and aggregate to fishing days
  fishing_days_study <- events_daily_dominant %>%
    dplyr::select(-any_of(c("vessel_name", "vessel_flag"))) %>%
    left_join(vessel_meta, by = "vesselId", relationship = "many-to-one") %>%
    filter(!is.na(ssvid)) %>%
    distinct(ssvid, vessel_name, vessel_flag, gear, gt, gt_final, size_class,
             division, zone, zone_fr, zone_detail, year, quarter, date) %>%
    group_by(ssvid, vessel_name, vessel_flag, gear, gt, gt_final, size_class,
             division, zone, zone_fr, zone_detail, year, quarter) %>%
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
    summarise(fishing_days = sum(fishing_days), n_vessels = n_distinct(ssvid),
              pct = round(fishing_days / sum(fishing_days_study$fishing_days)
                          * 100, 1), .groups = "drop") %>%
    print()

  # French zone breakdown
  fishing_days_study %>%
    group_by(zone_detail) %>%
    summarise(fishing_days = sum(fishing_days), n_vessels = n_distinct(ssvid),
              pct = round(fishing_days / sum(fishing_days_study$fishing_days)
                          * 100, 1), .groups = "drop") %>%
    arrange(desc(fishing_days)) %>%
    print()

  save(fishing_days_study,
       file = file.path(flyshootdir, "gfw_s7_effort.RData"))
  message("Saved gfw_s7_effort.RData")

} else {

  fishing_days_study <- load_object(
    file.path(flyshootdir, "gfw_s7_effort.RData"), "fishing_days_study")

  message("Loaded gfw_s7_effort.RData: ",
          n_distinct(fishing_days_study$ssvid), " vessels | ",
          sum(fishing_days_study$fishing_days), " total days")

}


# ============================================================
# STEP 8 — Diagnostics
# ============================================================

message("\n========== DATA COMPLETENESS DIAGNOSTICS ==========\n")

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

# ---- 8a: Completeness ----
completeness_summary(vessel_meta,        "vessel_meta")
completeness_summary(fishing_days_study, "fishing_days_study")

# ---- 8b: Coverage by year ----
message("\n--- fishing days by year ---")
fishing_days_study %>%
  filter(year %in% study_years) %>%
  group_by(year) %>%
  summarise(total_days = sum(fishing_days), n_vessels = n_distinct(ssvid),
            n_flags = n_distinct(vessel_flag, na.rm = TRUE), .groups = "drop") %>%
  print(n = 20)

# ---- 8c: AIS coverage — first appearance by size class ----
message("\n--- AIS coverage diagnostics ---")
vessel_first_appearance <- gfw_vessel_periods %>%
  mutate(first_year = year(as.Date(substr(transmissionDateFrom, 1, 10)))) %>%
  group_by(vesselId) %>%
  summarise(first_year = min(first_year, na.rm = TRUE), .groups = "drop")

vessel_appearance_profile <- vessel_meta %>%
  distinct(vesselId, ssvid, vessel_flag, size_class, gt, length_m) %>%
  left_join(vessel_first_appearance, by = "vesselId") %>%
  filter(!is.na(first_year))

vessel_appearance_profile %>%
  filter(!is.na(size_class), size_class != "Unknown") %>%
  group_by(size_class) %>%
  summarise(
    n_vessels        = n_distinct(ssvid),
    median_first_yr  = median(first_year, na.rm = TRUE),
    pct_before_2016  = round(mean(first_year < 2016)  * 100, 1),
    pct_2016_to_2019 = round(mean(first_year >= 2016 & first_year <= 2019) * 100, 1),
    pct_after_2019   = round(mean(first_year > 2019)  * 100, 1),
    .groups = "drop"
  ) %>%
  print()

# ---- 8d: Core fleet vs transient ----
core_fleet <- discovery_events %>%
  group_by(vessel_ssvid) %>%
  filter(n_distinct(reference_date) == 4) %>%
  pull(vessel_ssvid) %>% unique()
message("Core fleet (all 4 reference periods): ", length(core_fleet), " vessels")

fishing_days_study %>%
  filter(year %in% study_years) %>%
  mutate(fleet = if_else(ssvid %in% core_fleet,
                         "Core fleet", "Transient")) %>%
  group_by(year, fleet) %>%
  summarise(fishing_days = sum(fishing_days), n_vessels = n_distinct(ssvid),
            .groups = "drop") %>%
  mutate(days_per_vessel = round(fishing_days / n_vessels, 1)) %>%
  pivot_wider(names_from = fleet,
              values_from = c(fishing_days, n_vessels, days_per_vessel)) %>%
  print(n = 20)


# ============================================================
# STEP 9 — Vessel coverage chart (PDF + Excel)
# ============================================================

# Deduplicate gfw_vessel_periods — one row per vesselId
gfw_vessel_periods_clean <- gfw_vessel_periods %>%
  group_by(vesselId) %>%
  arrange(desc(!is.na(shipname))) %>%
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

# ---- PDF ----
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

pdf(file   = file.path(flyshootdir,
                       paste0("vessel_coverage_", Sys.Date(), ".pdf")),
    width  = 14,
    height = max(8, rows_per_page * 0.4))

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
      geom_text(aes(x = date_from + (date_to - date_from) / 2,
                    y = as.numeric(y_label), label = bar_text),
                size = 2.5, colour = "white", fontface = "bold",
                hjust = 0.5, vjust = 0.5) +
      scale_fill_manual(values = flag_colours_all, na.value = "grey70") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   limits = as.Date(c("2012-01-01", "2026-12-31"))) +
      scale_y_continuous(breaks = seq_along(page_labels),
                         labels = page_labels,
                         expand = expansion(add = 0.6)) +
      theme(axis.text.y = element_text(size = 7, family = "mono"),
            axis.text.x = element_text(size = 9),
            axis.title  = element_blank(),
            panel.grid.major.x = element_line(colour = "grey85"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank(),
            legend.position    = "bottom") +
      labs(fill     = "Flag",
           title    = paste0("Vessel fishing activity (", p, "/", n_pages, ")"),
           subtitle = "One row = one MMSI  |  segments show name/flag changes over time")
  )
}
dev.off()
message("Vessel coverage PDF saved")

# ---- Excel (colour-coded) ----
library(openxlsx)

col_gfw    <- "#BDD7EE"
col_meta   <- "#E2EFDA"
col_effort <- "#FCE4D6"
col_id     <- "#F2F2F2"

source_colours <- c("id" = col_id, "gfw" = col_gfw,
                    "meta" = col_meta, "effort" = col_effort)

col_sources <- tribble(
  ~column,               ~source,
  "ssvid",               "id",
  "recent_name",         "gfw",    "recent_flag",         "gfw",
  "total_fishing_days",  "effort", "first_year_activity", "effort",
  "last_year_activity",  "effort", "divisions",           "effort",
  "vesselId",            "gfw",    "shipname",            "gfw",
  "vessel_flag",         "gfw",    "vessel_name",         "meta",
  "gear_registry",       "meta",   "gear_gfw",            "gfw",
  "gt",                  "meta",   "length_m",            "meta",
  "size_class",          "meta",   "size_source",         "meta",
  "date_from",           "gfw",    "date_to",             "gfw",
  "bar_width_years",     "gfw",    "gears_observed",      "effort"
)

apply_header_colours <- function(wb, sheet_name, df) {
  for (j in seq_along(names(df))) {
    src        <- col_sources$source[col_sources$column == names(df)[j]]
    fill_color <- if (length(src) > 0) source_colours[src] else col_id
    addStyle(wb, sheet_name,
             style = createStyle(fgFill = fill_color, fontColour = "#000000",
                                 textDecoration = "bold",
                                 border = "Bottom", borderColour = "#999999"),
             rows = 1, cols = j, gridExpand = FALSE)
  }
}

write_sheet <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df, startRow = 1,
            headerStyle = createStyle(fontColour = "#000000",
                                      textDecoration = "bold"))
  apply_header_colours(wb, sheet_name, df)
  setColWidths(wb, sheet_name, cols = seq_along(names(df)), widths = "auto")
  freezePane(wb, sheet_name, firstRow = TRUE)
  date_cols <- which(names(df) %in% c("date_from", "date_to"))
  if (length(date_cols) > 0 && nrow(df) > 0)
    addStyle(wb, sheet_name, createStyle(numFmt = "YYYY-MM-DD"),
             rows = 2:(nrow(df) + 1), cols = date_cols, gridExpand = TRUE)
}

vessel_coverage_export <- vessel_coverage %>%
  left_join(vessel_meta %>%
              dplyr::select(vesselId, gt, length_m, size_class,
                            size_source, gear, vessel_name),
            by = "vesselId") %>%
  rename(gear_registry = gear.y, gear_gfw = gear.x) %>%
  left_join(
    fishing_days_study %>%
      group_by(ssvid) %>%
      summarise(total_fishing_days  = sum(fishing_days),
                first_year_activity = min(year),
                last_year_activity  = max(year),
                divisions     = paste(sort(unique(division)), collapse = ", "),
                gears_observed = paste(sort(unique(na.omit(gear))),
                                        collapse = ", "),
                .groups = "drop"),
    by = "ssvid"
  ) %>%
  dplyr::select(ssvid, recent_name, recent_flag,
                total_fishing_days, first_year_activity, last_year_activity,
                divisions, vesselId, shipname, vessel_flag, vessel_name,
                gear_registry, gear_gfw, gt, length_m, size_class, size_source,
                date_from, date_to, bar_width_years, gears_observed) %>%
  mutate(bar_width_years = round(bar_width_years, 2),
         date_from = as.Date(date_from), date_to = as.Date(date_to)) %>%
  arrange(recent_flag, recent_name, ssvid, date_from)

vessel_summary <- vessel_coverage_export %>%
  group_by(ssvid, recent_name, recent_flag, total_fishing_days,
           first_year_activity, last_year_activity, divisions, gears_observed) %>%
  summarise(n_vessel_ids   = n_distinct(vesselId),
            n_name_changes = n_distinct(shipname) - 1,
            n_flag_changes = n_distinct(vessel_flag) - 1,
            gt_median      = round(median(gt,       na.rm = TRUE), 1),
            length_median  = round(median(length_m, na.rm = TRUE), 1),
            size_class     = first(na.omit(size_class)),
            first_period   = min(date_from),
            last_period    = max(date_to),
            .groups = "drop") %>%
  arrange(recent_flag, recent_name, ssvid)

wb <- createWorkbook()

# Legend sheet
addWorksheet(wb, "Legend")
legend_df <- tibble(
  Colour = c("Light blue", "Light green", "Light orange", "Light grey"),
  Source = c("GFW registry", "vessel_meta (corrected)",
             "fishing_days_study (derived)", "MMSI identifier"),
  Description = c("Data as returned by GFW API",
                  "Data after manual review and correction",
                  "Aggregated from fishing events",
                  "Primary vessel identifier")
)
writeData(wb, "Legend", legend_df)
for (i in seq_len(nrow(legend_df))) {
  addStyle(wb, "Legend",
           style = createStyle(fgFill = c(col_gfw, col_meta, col_effort,
                                          col_id)[i]),
           rows = i + 1, cols = 1:3, gridExpand = TRUE)
}
setColWidths(wb, "Legend", cols = 1:3, widths = c(15, 35, 50))
freezePane(wb, "Legend", firstRow = TRUE)

write_sheet(wb, "All vessels",     vessel_coverage_export)
write_sheet(wb, "Summary by MMSI", vessel_summary)

flag_sheets_xl <- vessel_coverage_export %>%
  mutate(sheet_flag = if_else(is.na(recent_flag), "Unknown", recent_flag)) %>%
  split(.$sheet_flag)
for (fn in sort(names(flag_sheets_xl))) write_sheet(wb, fn, flag_sheets_xl[[fn]])

saveWorkbook(wb,
             file = file.path(flyshootdir,
                              paste0("vessel_coverage_", Sys.Date(), ".xlsx")),
             overwrite = TRUE)
message("Vessel coverage Excel saved: ",
        1 + 2 + length(flag_sheets_xl), " sheets")


# ============================================================
# STEP 10 — Effort plots
# ============================================================

# ---- Plot 1: fishing days by year and flag (absolute) ----
fishing_days_study %>%
  filter(year %in% study_years) %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
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

# ---- Plot 2: by year, flag and division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title = "Fishing days by year, country and ICES division") +
  facet_wrap(~ division)

# ---- Plot 3: % by year and flag ----
fishing_days_study %>%
  filter(year %in% study_years) %>%
  group_by(year, vessel_flag) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
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
       title = "Fishing days by year and country (%)")

# ---- Plot 4: % by year, flag and division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(division)) %>%
  group_by(year, vessel_flag, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
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
       title = "Fishing days by year, country and ICES division (%)") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 5: by year and gear, faceted by division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(division), !is.na(gear)) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(gear) %>%
  mutate(total_gear_days = sum(fishing_days)) %>%
  ungroup() %>%
  mutate(gear = if_else(total_gear_days / sum(fishing_days) * 100
                        < threshold_pct, "Other", gear)) %>%
  group_by(year, gear, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = gear)) +
  theme_bw() + geom_col() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Gear type",
       title = "Fishing days by year and gear type") +
  facet_wrap(~ division, scales = "free_y")

# ---- Plot 6: GT fishing days by year and flag ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(gt_final)) %>%
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
       title = "GT fishing days by year and country")

# ---- Plot 7: by year and size class ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(size_class),
         size_class != "Unknown") %>%
  group_by(year, size_class) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = size_class)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Size class",
       title = "Fishing days by year and vessel size class")

# ---- Plot 8: size class by division ----
fishing_days_study %>%
  filter(year %in% study_years, !is.na(size_class),
         size_class != "Unknown", !is.na(division)) %>%
  group_by(year, size_class, division) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = size_class)) +
  theme_bw() + geom_col() +
  scale_fill_manual(values = size_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "Size class",
       title = "Fishing days by year, size class and ICES division") +
  facet_wrap(~ division)

# ---- Plot 9: fishing events map — flag ----
sample_size <- 20000
events_sample <- events_classified_fr %>%
  dplyr::select(-any_of(c("vessel_flag", "vessel_name"))) %>%
  left_join(vessel_meta %>% dplyr::select(vesselId, vessel_flag),
            by = "vesselId") %>%
  mutate(vessel_flag = case_when(
    is.na(vessel_flag)                   ~ "Other",
    vessel_flag %in% names(flag_colours) ~ vessel_flag,
    TRUE                                 ~ "Other"
  )) %>%
  slice_sample(n = sample_size)

ggplot() +
  theme_bw() +
  geom_sf(data = fao_sf_division, fill = NA, colour = "grey60",
          linewidth = 0.3) +
  geom_sf(data = world, fill = "grey90", colour = "grey70", linewidth = 0.2) +
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
       title  = paste0("Fishing events — marine only (sample of ",sample_size,")"),
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E")

# ---- Plot 10: fishing events map — flag and year ----
sample_size <- 10000
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
  slice_sample(n = sample_size)

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
       title  = paste0("Fishing events — marine only (sample of ",sample_size,")"),
       subtitle = "Internal waters excluded  |  ICES 27.4.C / 27.7.D / 27.7.E") +
  facet_wrap(~year)

# ---- Plot 11: Kernel density maps — all fleets combined, by year ----
# Excludes internal waters (uses events_classified which is already marine-only)
# Uses all events (not a sample) for accurate density estimation

sample_size <- 50000

events_for_density <- events_classified %>%
  mutate(year = lubridate::year(start)) %>%
  filter(year %in% study_years,
         !is.na(lon), !is.na(lat)) %>%
  dplyr::select(vesselId, lon, lat, year) %>% 
  group_by(year) %>% 
  slice_sample(n = sample_size) 

ggplot() +
  theme_bw() +
  stat_density_2d(
    data    = events_for_density,
    aes(x = lon, y = lat, fill = after_stat(ndensity)),
    geom    = "raster",
    contour = FALSE,
    n       = 200,
    h       = c(0.3, 0.3)         # bandwidth in degrees — tune to taste
  ) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Relative\ndensity",
    limits   = c(0, 1),
    breaks   = c(0, 0.5, 1),
    labels   = c("Low", "Mid", "High"),
    na.value = "transparent"
  ) +
  geom_sf(data = fao_sf_division, fill = NA,
          colour = "white", linewidth = 0.4) +
  geom_sf(data = world, fill = "grey30",
          colour = "grey50", linewidth = 0.2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme(
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_line(colour = "grey20"),
    panel.background = element_rect(fill = "grey10"),
    plot.background  = element_rect(fill = "grey10"),
    legend.position  = "bottom",
    legend.title     = element_text(colour = "white", size = 8),
    legend.text      = element_text(colour = "white", size = 7),
    strip.background = element_rect(fill = "grey20"),
    strip.text       = element_text(colour = "white", face = "bold"),
    plot.title       = element_text(colour = "white", face = "bold"),
    plot.subtitle    = element_text(colour = "grey70", size = 8)
  ) +
  labs(title    = "Fishing effort density — all fleets combined",
       subtitle = paste0("Kernel density (normalised per year)  |  Internal waters excluded",
                         "  |  ICES 27.4.C / 27.7.D / 27.7.E")) +
  facet_wrap(~ year)


# ---- Plot 12: 7d concentration analysis — effort in core area over time ----
# Approach: divide 27.7.D into a regular 0.1° × 0.1° grid and compute
# the Gini coefficient of effort across cells per year.
# A rising Gini = effort increasingly concentrated in fewer cells.
# Also report: % of total 7d effort in the top-10% most-fished cells.

library(ineq)   # for Gini(); install.packages("ineq") if needed

# Bounding box for 27.7.D (adjust if your fao_sf_division differs)
bbox_7d <- st_bbox(fao_sf_division %>% filter(division == "27.7.D"))

events_7d <- events_classified %>%
  mutate(year = lubridate::year(start)) %>%
  filter(year %in% study_years,
         !is.na(lon), !is.na(lat)) %>%
  # Spatial filter: keep only events inside 27.7.D
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_filter(fao_sf_division %>% filter(division == "27.7.D")) %>%
  st_drop_geometry()

# Assign each event to a 0.1° grid cell
events_7d <- events_7d %>%
  mutate(
    lon_cell = round(floor(lon / 0.1) * 0.1 + 0.05, 4),
    lat_cell = round(floor(lat / 0.1) * 0.1 + 0.05, 4),
    cell_id  = paste(lon_cell, lat_cell, sep = "_")
  )

# Effort (event count) per cell per year
cell_effort_7d <- events_7d %>%
  group_by(year, cell_id, lon_cell, lat_cell) %>%
  summarise(n_events = n(), .groups = "drop")

# Concentration metrics per year
concentration_7d <- cell_effort_7d %>%
  group_by(year) %>%
  summarise(
    n_cells       = n(),
    total_events  = sum(n_events),
    gini          = ineq::Gini(n_events),
    # % of effort in top-10% most-fished cells
    top10_pct     = {
      sorted  <- sort(n_events, decreasing = TRUE)
      n_top   <- max(1L, ceiling(0.10 * length(sorted)))
      sum(sorted[seq_len(n_top)]) / sum(sorted) * 100
    },
    # Area covered by cells with ≥1% of annual effort each
    n_cells_1pct  = sum(n_events / total_events >= 0.01),
    .groups = "drop"
  )

message("\n===== 7d CONCENTRATION METRICS =====")
print(concentration_7d, n = 20)

# ---- Plot 12a: Gini coefficient over time ----
ggplot(concentration_7d, aes(x = year, y = gini)) +
  theme_bw() +
  geom_line(colour = "#E15759", linewidth = 1) +
  geom_point(colour = "#E15759", size = 3) +
  geom_text(aes(label = round(gini, 3)),
            vjust = -0.8, size = 2.8) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5),
        plot.title    = element_text(face = "bold")) +
  labs(x        = "",
       y        = "Gini coefficient",
       title    = "Spatial concentration of fishing effort — ICES 27.7.D",
       subtitle = paste0("Gini coefficient across 0.1° grid cells  |  ",
                         "Higher = more concentrated  |  All fleets combined"))

# ---- Plot 12b: % effort in top-10% cells ----
ggplot(concentration_7d, aes(x = year, y = top10_pct)) +
  theme_bw() +
  geom_line(colour = "#4E79A7", linewidth = 1) +
  geom_point(colour = "#4E79A7", size = 3) +
  geom_text(aes(label = paste0(round(top10_pct, 1), "%")),
            vjust = -0.8, size = 2.8) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0.05, 0.15))) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5),
        plot.title    = element_text(face = "bold")) +
  labs(x        = "",
       y        = "% of total effort",
       title    = "Effort in top-10% most-fished grid cells — ICES 27.7.D",
       subtitle = "Proportion of total annual events in the 10% busiest 0.1° cells  |  All fleets combined")

# ---- Plot 12c: Density maps for 27.7.D only — year facets ----
ggplot() +
  theme_bw() +
  stat_density_2d(
    data    = events_7d,
    aes(x = lon, y = lat, fill = after_stat(ndensity)),
    geom    = "raster",
    contour = FALSE,
    n       = 200,
    h       = c(0.15, 0.15)
  ) +
  scale_fill_viridis_c(
    option   = "inferno",
    name     = "Relative\ndensity",
    limits   = c(0, 1),
    na.value = "transparent"
  ) +
  geom_sf(data = fao_sf_division %>% filter(division == "27.7.D"),
          fill = NA, colour = "white", linewidth = 0.5) +
  geom_sf(data = world, fill = "grey30",
          colour = "grey50", linewidth = 0.2) +
  coord_sf(
    xlim   = c(bbox_7d["xmin"] - 0.1, bbox_7d["xmax"] + 0.1),
    ylim   = c(bbox_7d["ymin"] - 0.1, bbox_7d["ymax"] + 0.1),
    expand = FALSE
  ) +
  theme(
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    panel.background = element_rect(fill = "grey10"),
    plot.background  = element_rect(fill = "grey10"),
    panel.grid       = element_line(colour = "grey20"),
    legend.position  = "bottom",
    legend.title     = element_text(colour = "white", size = 8),
    legend.text      = element_text(colour = "white", size = 7),
    strip.background = element_rect(fill = "grey20"),
    strip.text       = element_text(colour = "white", face = "bold"),
    plot.title       = element_text(colour = "white", face = "bold"),
    plot.subtitle    = element_text(colour = "grey70", size = 8)
  ) +
  labs(title    = "Fishing effort density — ICES 27.7.D, all fleets combined",
       subtitle = "Kernel density (normalised per year)  |  Internal waters excluded") +
  facet_wrap(~ year)
