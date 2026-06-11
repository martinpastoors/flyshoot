# =============================================================================
# STECF FDI Data Analysis – Channel & Southern North Sea Fisheries
# Summaries: Vessels and Fishing Days by Country / Size Class / Gear / Division
# =============================================================================
#
# DATA SOURCES:
#   • "FDI Effort by country.csv"   – fishing days and effort metrics
#                                     (NO vessel counts in this table)
#   • "FDI Capacity by country.csv" – number of vessels and GT/kW capacity
#                                     (NO spatial breakdown in this table)
#
# WHY TWO FILES ARE NEEDED:
#   STECF publishes effort and capacity as separate tables in the FDI data
#   call. Effort is recorded at metier × division × quarter resolution.
#   Capacity is recorded at vessel-length-class × gear level only – it has
#   NO division column. To obtain vessels-per-division you must join on the
#   shared keys (country / year / vessel_lc / gear_type) and accept that
#   vessel counts are apportioned to divisions in proportion to effort, or –
#   as done here – reported alongside the effort strata with an explicit note
#   that the same vessel pool may operate across multiple divisions.
#
# Key known data-quality issues handled in this script:
#   1. Confidentiality suppression  – cells suppressed as "NK", "na", "-1",
#      "CONFID", blank etc.  Coerced to NA and flagged per stratum.
#   2. Mixed-type columns           – confidential flags arrive as character;
#      coercion with NA-on-warn is therefore intentional.
#   3. "-1" sentinel values         – STECF uses -1 for "not known /
#      confidential"; treated as NA.
#   4. Inconsistent NA strings      – handled via na= on read + safe_numeric().
#   5. Vessel count aggregation     – a vessel active in multiple metiers or
#      divisions appears in multiple rows of the capacity table. Summing
#      produces an upper-bound, not a unique count. Flagged in output.
#   6. Gear / metier harmonisation  – mapped to stable broad gear categories.
#   7. Division labelling           – normalised to upper-case dotted format
#      e.g. "27.7.d" -> "7.D".
#   8. Duplicate rows               – detected and removed before summarising.
#   9. Capacity join                – capacity has no division; joined on
#      country/year/vessel_lc/gear_type and kept as a parallel summary.
# =============================================================================

library(tidyverse)
library(janitor)

# -----------------------------------------------------------------------------
# 0.  USER SETTINGS
# -----------------------------------------------------------------------------

FDI_DIR    <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/FLYSHOOT/stecf 2025_Effort-landings-catches-capacity-biological"
STUDY_DIVS <- c("4.C", "7.D", "7.E")
OUT_DIR    <- "output"

# -----------------------------------------------------------------------------
# 1.  HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' Coerce character to numeric, treating all confidentiality sentinels as NA.
safe_numeric <- function(x) {
  cleaned <- str_trim(x) |>
    str_replace_all(
      regex("^(NK|na|N/A|#N/A|CONFID|confidential|BEL|\\.)$",
            ignore_case = TRUE),
      NA_character_
    )
  val <- suppressWarnings(as.numeric(cleaned))
  val[!is.na(val) & val == -1] <- NA_real_   # -1 sentinel
  val
}

#' TRUE when a value was suppressed (non-empty raw string that coerced to NA).
is_suppressed <- function(raw_chr, numeric_val) {
  is.na(numeric_val) & !is.na(raw_chr) & str_trim(raw_chr) != ""
}

#' Normalise ICES division to upper-case dotted format: "27.7.d" -> "7.D".
normalise_division <- function(x) {
  x <- str_trim(x) |> str_to_upper()
  x <- str_remove(x, "^27\\.")
  x <- str_replace(x, "^(\\d+)([A-Z])$", "\\1.\\2")
  roman_map <- c(
    "^I\\.?"    = "1.", "^II\\.?"   = "2.", "^III\\.?"  = "3.",
    "^IV\\.?"   = "4.", "^V\\.?"    = "5.", "^VI\\.?"   = "6.",
    "^VII\\.?"  = "7.", "^VIII\\.?" = "8.", "^IX\\.?"   = "9.",
    "^X\\.?"    = "10.","^XI\\.?"   = "11.","^XII\\.?"  = "12.",
    "^XIII\\.?" = "13.","^XIV\\.?"  = "14."
  )
  for (pat in names(roman_map)) {
    x <- str_replace(x, regex(pat, ignore_case = TRUE), roman_map[[pat]])
  }
  x
}

#' Map DCF gear codes to analyst-friendly broad categories.
map_gear_category <- function(gear_code) {
  lookup <- c(
    OTB = "Bottom trawl", OTT = "Bottom trawl", PTB = "Bottom trawl",
    OTM = "Midwater trawl", PTM = "Midwater trawl",
    TBB = "Beam trawl",  TBN = "Beam trawl",
    SSC = "Scottish seine", SDN = "Danish seine",
    GNS = "Gillnet",  GND = "Gillnet",  GNC = "Gillnet",
    GTR = "Gillnet",   # trammel net — entangling net, same category as GNS
    GNC = "Gillnet",   # combined gillnet-trammel
    FPN = "Pot/trap", FPO = "Pot/trap", FYK = "Pot/trap",
    LLS = "Longline", LLD = "Longline", LL  = "Longline",
    LHP = "Handline/rod", LHM = "Handline/rod", LTL = "Handline/rod",
    DRB = "Dredge",   HMD = "Dredge",
    PS  = "Purse seine", LA  = "Purse seine",
    MIS = "Other/mixed", NK  = "Unknown"
  )
  dplyr::recode(str_to_upper(str_trim(gear_code)),
                !!!lookup, .default = "Other/mixed")
}

#' Map DCF fishing_technique codes (coarse level, used in Capacity table)
#' to the same broad gear categories as map_gear_category().
#' fishing_technique is the coarsest grouping: DTS covers OTB/OTT/PTB etc.
map_fishing_technique <- function(ftec) {
  lookup <- c(
    DTS      = "Bottom trawl",    # demersal trawl & seine (OTB, OTT, PTB, SSC, SDN)
    TBB      = "Beam trawl",
    PEL      = "Midwater trawl",  # pelagic (OTM, PTM)
    DFN      = "Gillnet",         # drift & fixed nets
    PS       = "Purse seine",
    LL       = "Longline",
    HOK      = "Handline/rod",    # hooks & lines
    FPO      = "Pot/trap",
    DRB      = "Dredge",
    INACTIVE = "Inactive",
    NK       = "Unknown"
  )
  dplyr::recode(str_to_upper(str_trim(ftec)),
                !!!lookup, .default = "Other/mixed")
}

#' Standardise vessel length-class labels to DCF bracket codes.
harmonise_vessel_lc <- function(x) {
  str_trim(x) |> str_to_upper() |>
    str_replace_all(c(
      "VL06-12"  = "VL0612", "VL0012"   = "VL0612",
      "VL12-24"  = "VL1224", "VL0024"   = "VL1224",
      "VL24-40"  = "VL2440",
      "VL>40"    = "VL40XX", "VL>=40"   = "VL40XX",
      "^NK$"     = "UNKNOWN"
    ))
}

# -----------------------------------------------------------------------------
# 2.  LOAD EFFORT FILE
# -----------------------------------------------------------------------------

effort_files <- list.files(
  FDI_DIR,
  pattern    = regex("FDI[_ ]Effort[_ ]by[_ ]country\\.csv", ignore_case = TRUE),
  recursive  = TRUE,
  full.names = TRUE
)

if (length(effort_files) == 0)
  stop("No 'FDI Effort by country.csv' found in: ", FDI_DIR)

message("Effort file(s) found:\n  ",
        paste(basename(effort_files), collapse = "\n  "))

raw_effort <- map_dfr(
  effort_files,
  ~ read_csv(.x,
             col_types      = cols(.default = col_character()),
             na             = c("", "NA", "na", "N/A", "#N/A", "NK",
                                "CONFID", "confidential", "-", "."),
             locale         = locale(encoding = "UTF-8"),
             show_col_types = FALSE),
  .id = "source_file"
) |> clean_names()

message("Effort rows loaded: ", nrow(raw_effort))
message("Effort columns:     ", paste(names(raw_effort), collapse = ", "))

# -----------------------------------------------------------------------------
# 3.  LOAD CAPACITY FILE
# -----------------------------------------------------------------------------

capacity_files <- list.files(
  FDI_DIR,
  pattern    = regex("FDI[_ ]Capacity[_ ]by[_ ]country\\.csv", ignore_case = TRUE),
  recursive  = TRUE,
  full.names = TRUE
)

if (length(capacity_files) == 0)
  stop("No 'FDI Capacity by country.csv' found in: ", FDI_DIR)

message("\nCapacity file(s) found:\n  ",
        paste(basename(capacity_files), collapse = "\n  "))

raw_capacity <- map_dfr(
  capacity_files,
  ~ read_csv(.x,
             col_types      = cols(.default = col_character()),
             na             = c("", "NA", "na", "N/A", "#N/A", "NK",
                                "CONFID", "confidential", "-", "."),
             locale         = locale(encoding = "UTF-8"),
             show_col_types = FALSE),
  .id = "source_file"
) |> clean_names()

message("Capacity rows loaded: ", nrow(raw_capacity))
message("Capacity columns:     ", paste(names(raw_capacity), collapse = ", "))

# -----------------------------------------------------------------------------
# 4.  AUTO-DETECT COLUMNS
# -----------------------------------------------------------------------------

find_col <- function(df, patterns) {
  hits <- names(df)[str_detect(names(df),
                               regex(paste(patterns, collapse = "|"),
                                     ignore_case = TRUE))]
  if (length(hits) == 0) NA_character_ else hits[1]
}

# ---- Effort columns ----
e_year     <- find_col(raw_effort, c("^year$", "^ref_year$"))
e_country  <- find_col(raw_effort, c("^country$", "^country_code$", "^ms$"))
e_quarter  <- find_col(raw_effort, c("^quarter$", "^qtr$", "^q$"))
e_vlc      <- find_col(raw_effort, c("vessel_length", "length_cat",
                                     "sizeclass", "length_class"))
e_gear     <- find_col(raw_effort, c("^gear_type$", "^gear$"))
e_ftec     <- find_col(raw_effort, c("^fishing_technique$", "^fish_tech$", "^ftechnique$"))
e_division <- find_col(raw_effort, c("^sub_region$", "^division$",
                                     "ices_div", "sub_area"))
e_days     <- find_col(raw_effort, c("^total_fishing_days$", "^fishing_days$",
                                     "^days_at_sea$"))
e_target   <- find_col(raw_effort, c("^target_assemblage$", "^target$"))
e_mesh     <- find_col(raw_effort, c("^mesh_size_range$", "^mesh_size$", "^mesh$"))
e_confid   <- find_col(raw_effort, c("^confidential$"))

# ---- Capacity columns ----
c_year     <- find_col(raw_capacity, c("^year$", "^ref_year$"))
c_country  <- find_col(raw_capacity, c("^country$", "^country_code$", "^ms$"))
c_vlc      <- find_col(raw_capacity, c("vessel_length", "length_cat",
                                       "sizeclass", "length_class"))
c_ftec     <- find_col(raw_capacity, c("^fishing_technique$", "^fish_tech$", "^ftechnique$"))
c_vessels  <- find_col(raw_capacity, c("^no_vessels$", "^total_vessels$",
                                       "^num_vessels$", "^vessels$",
                                       "^no_of_vessels$"))
c_confid   <- find_col(raw_capacity, c("^confidential$"))

# Report mapping
message(
  "\nEffort column mapping:",
  "\n  year      -> ", e_year,
  "\n  country   -> ", e_country,
  "\n  quarter   -> ", ifelse(is.na(e_quarter), "(not found – annual data)", e_quarter),
  "\n  vessel_lc -> ", e_vlc,
  "\n  gear_type -> ", e_gear,
  "\n  division  -> ", e_division,
  "\n  fish_days -> ", e_days,
  "\n  target    -> ", ifelse(is.na(e_target), "(not found)", e_target),
  "\n  mesh      -> ", ifelse(is.na(e_mesh),   "(not found)", e_mesh),
  "\n  confid    -> ", ifelse(is.na(e_confid), "(not found)", e_confid),
  "\n\nCapacity column mapping:",
  "\n  year      -> ", c_year,
  "\n  country   -> ", c_country,
  "\n  vessel_lc -> ", c_vlc,
  "\n  gear_type -> ", c_gear,
  "\n  vessels   -> ", c_vessels,
  "\n  confid    -> ", ifelse(is.na(c_confid), "(not found)", c_confid)
)

# Validate essentials
check_cols <- function(cols, label) {
  missing <- names(cols)[is.na(unlist(cols))]
  if (length(missing) > 0)
    warning(label, " – could not auto-detect: ",
            paste(missing, collapse = ", "),
            "\nEdit the '# OVERRIDES' block below to hard-code them.")
}
check_cols(list(year=e_year, country=e_country, vessel_lc=e_vlc,
                gear=e_gear, division=e_division, days=e_days),
           "Effort")
check_cols(list(year=c_year, country=c_country, vessel_lc=c_vlc,
                fish_tech=c_ftec, vessels=c_vessels),
           "Capacity")

# --- COLUMN OVERRIDES (uncomment and edit if auto-detection fails) -----------
# e_year     <- "year"
# e_country  <- "country"
# e_vlc      <- "vessel_length_category"
# e_gear     <- "gear_type"
# e_division <- "sub_region"
# e_days     <- "total_fishing_days"
# e_confid   <- "confidential"
#
# c_year     <- "year"
# c_country  <- "country"
# c_vlc      <- "vessel_length_category"
# c_gear     <- "gear_type"
# c_vessels  <- "no_vessels"
# c_confid   <- "confidential"
# ----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 5.  CLEAN EFFORT TABLE
# -----------------------------------------------------------------------------

# Rename quarter column before the main pipe (native pipe |> does not support
# { if () } branching on the RHS, so this must be done as a separate step).
effort_raw2 <- raw_effort |>
  rename(
    year             = all_of(e_year),
    country          = all_of(e_country),
    vessel_lc        = all_of(e_vlc),
    gear_code         = all_of(e_gear),
    fishing_technique = all_of(e_ftec),
    division_raw      = all_of(e_division),
    fishing_days_raw  = all_of(e_days),
    target_assemblage = all_of(e_target),
    mesh_size_range   = all_of(e_mesh)
  )

# Add quarter column: rename if detected, otherwise create as NA
if (!is.na(e_quarter)) {
  effort_raw2 <- effort_raw2 |> rename(quarter = all_of(e_quarter))
} else {
  effort_raw2 <- effort_raw2 |> mutate(quarter = NA_character_)
}

effort <- effort_raw2 |>
  mutate(
    year          = as.integer(year),
    quarter       = as.integer(quarter),   # NA for annual files
    country       = str_to_upper(str_trim(country)),
    vessel_lc     = harmonise_vessel_lc(vessel_lc),
    gear_code         = str_to_upper(str_trim(gear_code)),
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    gear_category     = map_gear_category(gear_code),   # fine-level, from gear_type
    division          = normalise_division(division_raw),
    fishing_days  = safe_numeric(fishing_days_raw),
    # Confidentiality flag: use explicit column if present, else infer from value
    days_suppressed = if (!is.na(e_confid))
      str_to_upper(str_trim(.data[[e_confid]])) == "Y"
    else
      is_suppressed(fishing_days_raw, fishing_days)
  )

# Deduplicate effort strata.
# The full natural key for the effort table is ALL the stratification columns:
# country / year / quarter / vessel_lc / fishing_technique / gear_type /
# target_assemblage / mesh_size_range / sub_region (division).
# Any subset of these as the group key will incorrectly collapse rows that
# differ in the omitted columns. Only remove rows that are exact duplicates
# across ALL stratification columns.
n_eff_before <- nrow(effort)
effort_dd <- effort |>
  distinct(year, quarter, country, vessel_lc,
           fishing_technique, gear_code,
           target_assemblage, mesh_size_range,
           division,
           eez_indicator, geo_indicator,
           specon_tech, deep,
           .keep_all = TRUE)

n_removed <- nrow(effort) - nrow(effort_dd)   # compare original vs deduplicated
if (n_removed > 0) {
  message("Effort: removed ", n_removed, " fully identical duplicate rows.")
} else {
  message("Effort: no duplicate rows detected.")
}

effort <- effort_dd   # replace with deduplicated version for all downstream steps
rm(effort_dd)         # clean up


# -----------------------------------------------------------------------------
# 6.  CLEAN CAPACITY TABLE
# -----------------------------------------------------------------------------

# The capacity table has NO division column.
# Join key: country + year + vessel_lc + gear_code

capacity <- raw_capacity |>
  rename(
    year              = all_of(c_year),
    country           = all_of(c_country),
    vessel_lc         = all_of(c_vlc),
    fishing_technique = all_of(c_ftec),
    n_vessels_raw     = all_of(c_vessels)
  ) |>
  mutate(
    year              = as.integer(year),
    country           = str_to_upper(str_trim(country)),
    vessel_lc         = harmonise_vessel_lc(vessel_lc),
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    # Map fishing_technique (coarse DCF code) to broad gear category.
    # Capacity has no gear_type; fishing_technique is the finest available key.
    gear_category     = map_fishing_technique(fishing_technique),
    n_vessels     = safe_numeric(n_vessels_raw),
    vessels_suppressed = if (!is.na(c_confid))
      str_to_upper(str_trim(.data[[c_confid]])) == "Y"
    else
      is_suppressed(n_vessels_raw, n_vessels)
  )

# Deduplicate capacity strata using all stratification columns.
# The capacity table key is: country / year / vessel_lc / fishing_technique /
# principal_sub_region (the spatial field in capacity, if present).
# n_cap_before <- nrow(capacity)
# capacity <- capacity |>
#   distinct(year, country, vessel_lc, gear_category, supra_region, geo_indicator, principal_sub_region, 
#            .keep_all = TRUE)
# 
# n_cap_removed <- n_cap_before - nrow(capacity)
# if (n_cap_removed > 0) {
#   message("Capacity: removed ", n_cap_removed, " fully identical duplicate rows.")
# } else {
#   message("Capacity: no duplicate rows detected.")
# }

# -----------------------------------------------------------------------------
# 7.  FILTER EFFORT TO STUDY DIVISIONS
# -----------------------------------------------------------------------------

effort_study <- effort |> filter(division %in% STUDY_DIVS)

message("\nEffort rows in study divisions (",
        paste(STUDY_DIVS, collapse = ", "), "): ", nrow(effort_study))

if (nrow(effort_study) == 0)
  warning("No effort rows matched the study divisions.\n",
          "Divisions present: ",
          paste(sort(unique(effort$division)), collapse = ", "))

# =============================================================================
# 8.  SUMMARY 1 - FISHING DAYS
#     by Country / Vessel Size Class / Gear Category / Division / Year
#
# The effort table is QUARTERLY. Fishing days are additive across quarters
# within the same annual stratum so summing across rows is correct.
# n_quarters_reported shows how many of the 4 possible quarters contributed
# data; fewer than 4 may indicate genuine seasonal absence or suppression.
# =============================================================================

days_summary <- effort_study |>
  group_by(year, country, vessel_lc, gear_category, division) |>
  summarise(
    fishing_days_sum      = sum(fishing_days, na.rm = TRUE),
    n_quarters_reported   = n_distinct(quarter[!is.na(quarter)]),
    n_quarters_suppressed = sum(days_suppressed, na.rm = TRUE),
    days_complete         = all(!days_suppressed),
    .groups = "drop"
  ) |>
  mutate(
    days_note = case_when(
      n_quarters_suppressed > 0 ~ "Partial: >=1 quarter confidential",
      !days_complete            ~ "Partial: >=1 quarter NA",
      TRUE                      ~ "Complete"
    )
  ) |>
  arrange(year, country, vessel_lc, gear_category, division)

message("Fishing days summary rows: ", nrow(days_summary))

# -----------------------------------------------------------------------------
# 9.  SUMMARY 2 – NUMBER OF VESSELS
#     by Country / Vessel Size Class / Gear Category / Year
#     (No division breakdown – capacity table has no spatial dimension)
#
# IMPORTANT CAVEATS:
#   a) The capacity table records vessels per fleet segment (country/vlc/gear).
#      A vessel active in multiple gears appears in multiple rows; summing
#      across gears overestimates the unique vessel count.
#   b) There is NO division column in the capacity table, so vessel counts
#      cannot be attributed to 7.D vs 7.E directly from the FDI data alone.
#      The summary below is therefore at country/vlc/gear/year level.
#   c) The vessels_in_study_divs table (Section 10) shows which fleet
#      segments were *active* in the study divisions via the effort table –
#      use that to contextualise the vessel counts.
# -----------------------------------------------------------------------------

vessels_summary <- capacity |>
  filter(country %in% unique(effort_study$country)) |>   # keep study-area MS
  group_by(year, country, vessel_lc, fishing_technique, gear_category) |>
  summarise(
    n_vessels_sum              = sum(n_vessels, na.rm = TRUE),
    n_strata                   = n(),
    n_strata_suppressed_vessels = sum(vessels_suppressed, na.rm = TRUE),
    vessels_complete           = all(!vessels_suppressed),
    .groups = "drop"
  ) |>
  mutate(
    vessels_note = case_when(
      n_strata_suppressed_vessels > 0 ~ "Partial: ≥1 stratum confidential",
      !vessels_complete               ~ "Partial: ≥1 stratum NA",
      n_strata > 1                    ~ "Sum across gear strata – possible double-count",
      TRUE                            ~ "Complete"
    )
  ) |>
  arrange(year, country, vessel_lc, fishing_technique, gear_category)

message("Vessel summary rows: ", nrow(vessels_summary))

# -----------------------------------------------------------------------------
# 10. JOINED SUMMARY – Vessels + Fishing Days
#     by Country / Vessel Size Class / Gear Category / Division / Year
#
# Strategy: join capacity (no division) onto effort (with division) on the
# shared keys.  The vessel count for a fleet segment is the SAME regardless
# of how many divisions that segment fishes in – so the joined value is
# the fleet-segment vessel count associated with each effort stratum.
# This is the closest approximation possible without vessel-level tracking data.
# -----------------------------------------------------------------------------

joined_summary <- effort_study |>
  # Sum quarterly rows to annual totals per stratum before joining capacity.
  # Include fishing_technique in the key so it is available for the join.
  group_by(year, country, vessel_lc, fishing_technique, gear_category, gear_code, division) |>
  summarise(
    fishing_days_sum    = sum(fishing_days, na.rm = TRUE),
    n_quarters_reported = n_distinct(quarter[!is.na(quarter)]),
    days_suppressed_any = any(days_suppressed, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Left-join vessel counts from capacity on shared fleet-segment key
  # Join capacity on fishing_technique (the common key between the two tables).
  # gear_type in effort maps many-to-one onto fishing_technique:
  # e.g. OTB, OTT, PTB all fall under DTS. The vessel count from capacity
  # is therefore the fleet-segment total for that fishing_technique,
  # shared across all gear_types within it.
  left_join(
    capacity |>
      group_by(year, country, vessel_lc, fishing_technique) |>
      summarise(
        n_vessels            = sum(n_vessels, na.rm = TRUE),
        vessels_suppressed   = any(vessels_suppressed, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("year", "country", "vessel_lc", "fishing_technique")
  ) |>
  mutate(
    vessels_note = case_when(
      is.na(n_vessels) & vessels_suppressed ~ "Vessel count: confidential",
      is.na(n_vessels)                      ~ "Vessel count: not in capacity table",
      vessels_suppressed                    ~ "Vessel count: partial (some strata confidential)",
      TRUE                                  ~ "Vessel count: complete"
    ),
    days_note = case_when(
      days_suppressed_any ~ "Days: partial (≥1 stratum confidential)",
      TRUE                ~ "Days: complete"
    )
  ) |>
  arrange(year, country, vessel_lc, gear_category, division)

message("Joined summary rows: ", nrow(joined_summary))

# =============================================================================
# 10b. SUMMARY – FISHING DAYS BY GEAR CODE
#      by Country / Vessel Size Class / gear_code / Division / Year
#
# This uses gear_type from the effort table directly, giving finer resolution
# than fishing_technique (e.g. OTB and OTT are separate rather than merged
# under DTS). Vessel counts are NOT included here because capacity has no
# gear_type column – use joined_summary for effort + vessels at the coarser
# fishing_technique level, or combine manually if you have a gear_type ->
# fishing_technique lookup.
# =============================================================================

days_by_gearcode <- effort_study |>
  group_by(year, country, vessel_lc, fishing_technique, gear_code, division) |>
  summarise(
    fishing_days_sum      = sum(fishing_days, na.rm = TRUE),
    n_quarters_reported   = n_distinct(quarter[!is.na(quarter)]),
    n_quarters_suppressed = sum(days_suppressed, na.rm = TRUE),
    days_complete         = all(!days_suppressed),
    .groups = "drop"
  ) |>
  mutate(
    gear_category = map_gear_category(gear_code),
    days_note = case_when(
      n_quarters_suppressed > 0 ~ "Partial: >=1 quarter confidential",
      !days_complete            ~ "Partial: >=1 quarter NA",
      TRUE                      ~ "Complete"
    )
  ) |>
  arrange(year, country, vessel_lc, fishing_technique, gear_code, division)

message("Days-by-gear-code summary rows: ", nrow(days_by_gearcode))

# -----------------------------------------------------------------------------
# 11.  QUALITY-ASSURANCE CHECKS
# -----------------------------------------------------------------------------

qa <- list()

# 11a. Suppression rates in effort
qa$effort_suppression <- effort_study |>
  group_by(year, country) |>
  summarise(
    total_strata   = n(),
    suppressed     = sum(days_suppressed, na.rm = TRUE),  # per quarter row
    pct_suppressed = round(100 * suppressed / total_strata, 1),
    .groups = "drop"
  ) |>
  filter(suppressed > 0) |>
  arrange(desc(pct_suppressed))

# 11b. Fleet segments in capacity with no matching effort in study area
qa$capacity_no_effort <- capacity |>
  anti_join(effort_study,
            by = c("year", "country", "vessel_lc", "fishing_technique")) |>
  count(year, country, vessel_lc, fishing_technique, gear_category,
        name = "n_capacity_strata")

# 11c. Effort strata with no matching capacity record
qa$effort_no_capacity <- effort_study |>
  anti_join(capacity,
            by = c("year", "country", "vessel_lc", "fishing_technique")) |>
  count(year, country, vessel_lc, fishing_technique, gear_category, division,
        name = "n_effort_strata")

# 11d. Gear codes mapped to Other/mixed or Unknown
qa$unmapped_gears <- bind_rows(
  effort_study |> distinct(gear_code,         gear_category) |> mutate(source = "effort"),
  capacity     |> distinct(fishing_technique, gear_category) |>
                  rename(gear_code = fishing_technique)      |> mutate(source = "capacity")
) |> distinct() |>
  filter(gear_category %in% c("Other/mixed", "Unknown")) |>
  arrange(source, gear_code)

# 11e. Coverage
qa$coverage <- effort_study |>
  distinct(year, country) |>
  arrange(year, country)

message("\n--- QA Summary ---")
message("Years in effort:   ", paste(sort(unique(effort_study$year)),  collapse = ", "))
message("Countries present: ", paste(sort(unique(effort_study$country)), collapse = ", "))
message("Effort strata suppressed: ",
        sum(effort_study$days_suppressed, na.rm = TRUE), " / ", nrow(effort_study),
        " (", round(100*mean(effort_study$days_suppressed, na.rm=TRUE), 1), "%)")
message("Capacity strata suppressed: ",
        sum(capacity$vessels_suppressed, na.rm = TRUE), " / ", nrow(capacity),
        " (", round(100*mean(capacity$vessels_suppressed, na.rm=TRUE), 1), "%)")

if (nrow(qa$effort_no_capacity) > 0) {
  message("\nEffort strata with no matching capacity record: ",
          nrow(qa$effort_no_capacity), " – see qa_effort_no_capacity.csv")
}
if (nrow(qa$unmapped_gears) > 0) {
  message("Unmapped gear codes: ",
          paste(qa$unmapped_gears$gear_code, collapse = ", "),
          " – see qa_unmapped_gear_codes.csv")
}

# -----------------------------------------------------------------------------
# 12.  WRITE OUTPUTS
# -----------------------------------------------------------------------------

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

write_csv(days_summary,
  file.path(OUT_DIR, "fdi_fishingdays_by_country_sizeclass_gear_division_year.csv"))

write_csv(vessels_summary,
  file.path(OUT_DIR, "fdi_vessels_by_country_sizeclass_gear_year.csv"))

write_csv(joined_summary,
  file.path(OUT_DIR, "fdi_vessels_and_days_by_country_sizeclass_gear_division_year.csv"))

write_csv(days_by_gearcode,
  file.path(OUT_DIR, "fdi_fishingdays_by_country_sizeclass_gearcode_division_year.csv"))

write_csv(qa$effort_suppression,
  file.path(OUT_DIR, "qa_effort_suppression_rate.csv"))

write_csv(qa$coverage,
  file.path(OUT_DIR, "qa_data_coverage.csv"))

if (nrow(qa$effort_no_capacity) > 0)
  write_csv(qa$effort_no_capacity,
    file.path(OUT_DIR, "qa_effort_no_capacity.csv"))

if (nrow(qa$capacity_no_effort) > 0)
  write_csv(qa$capacity_no_effort,
    file.path(OUT_DIR, "qa_capacity_no_effort.csv"))

if (nrow(qa$unmapped_gears) > 0)
  write_csv(qa$unmapped_gears,
    file.path(OUT_DIR, "qa_unmapped_gear_codes.csv"))

message("\nOutputs written to: ", normalizePath(OUT_DIR))

# -----------------------------------------------------------------------------
# 13.  DIAGNOSTIC PLOTS
# -----------------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  p_days <- 
    days_summary |>
    group_by(year, country, division, gear_category) |>
    summarise(
      fishing_days_sum = sum(fishing_days_sum),
      complete         = all(days_complete),
      .groups = "drop"
    ) |>
    ggplot(aes(x = year, y = fishing_days_sum,
               colour = country, linetype = division)) +
    geom_line() +
    geom_point(aes(shape = complete), size = 2) +   # open shape = partial
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                       labels = c("TRUE" = "Complete", "FALSE" = "Partial"),
                       name   = "Data") +
    facet_wrap(~ gear_category, scales = "free_y") +
    theme_bw()
  
  ggsave(file.path(OUT_DIR, "plot_fishingdays_by_year_country_gear.png"),
         p_days, width = 14, height = 8)

  p_vessels <- vessels_summary |>
    filter(vessels_complete) |>
    ggplot(aes(x = year, y = n_vessels_sum, colour = country)) +
    geom_line() + geom_point(size = 2) +
    facet_wrap(~ gear_category + vessel_lc, scales = "free_y") +
    labs(title    = "FDI – Fleet Capacity (vessels)",
         subtitle = "All countries active in study area; no division breakdown available",
         caption  = "Note: sums across gear strata may double-count vessels",
         x = "Year", y = "Number of vessels", colour = "Country") +
    theme_bw()

  ggsave(file.path(OUT_DIR, "plot_vessels_by_year_country_gear.png"),
         p_vessels, width = 16, height = 10)

  p_gearcode <- days_by_gearcode |>
    filter(days_complete) |>
    ggplot(aes(x = year, y = fishing_days_sum,
               colour = country, linetype = division)) +
    geom_line() + geom_point(size = 1.5) +
    facet_wrap(~ gear_code, scales = "free_y") +
    labs(
      title    = "FDI - Fishing Days by Gear Code",
      subtitle = paste("Divisions:", paste(STUDY_DIVS, collapse = " & ")),
      caption  = "Gear codes from gear_type in effort table; no vessel counts available at this level",
      x = "Year", y = "Fishing days", colour = "Country", linetype = "Division"
    ) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))

  ggsave(file.path(OUT_DIR, "plot_fishingdays_by_year_gearcode.png"),
         p_gearcode, width = 16, height = 10)

  message("Diagnostic plots saved.")
}

message("\nDone.")

# sandbox

# 1. Which files were actually loaded?
message("Files loaded:")
print(effort_files)

# 2. What years are present in the raw data before any filtering?
raw_effort |>
  count(year, source_file) |>
  arrange(year) |>
  print(n = 50)

# 3. What years survive after cleaning?
effort |>
  count(year) |>
  arrange(year)

# 4. What years survive after the division filter?
effort_study |>
  count(year) |>
  arrange(year)

days_summary |>
  count(year, days_complete) |>
  arrange(year) %>% 
  print(n=100)

# Suppression summary to accompany the plot
days_summary |>
  group_by(year) |>
  summarise(
    n_total      = n(),
    n_partial    = sum(!days_complete),
    pct_partial  = round(100 * n_partial / n_total, 1),
    days_total   = sum(fishing_days_sum),
    days_partial = sum(fishing_days_sum[!days_complete]),
    pct_days_from_partial = round(100 * days_partial / days_total, 1)
  ) |>
  arrange(year) |>
  print(n = 20)

# What does France report in 7.D and 7.E?
effort_study |>
  filter(country == "FRANCE", division %in% c("7.D", "7.E")) |>
  count(gear_code, fishing_technique, sort = TRUE)

# 1. Is France in the data at all?
effort |> filter(country == "FRANCE") |> count(year)

# 2. What divisions does France report?
effort |> filter(country == "FRANCE") |> count(division, sort = TRUE)

# 3. What does France look like before division normalisation?
effort |> filter(country == "FRANCE") |> count(division_raw, sort = TRUE)

# 4. What country codes are actually in the data?
# (France might be coded as "FR" or "FRANCE" rather than "FRA")
effort |> count(country, sort = TRUE)

# These appear to be French-specific metier groupings
effort_study |>
  filter(country == "FRANCE", fishing_technique %in% c("MGP", "PMP", "PGP")) |>
  count(fishing_technique, gear_code, sort = TRUE) |>
  print(n = 20)

# Check for gear_code / fishing_technique mismatches
effort_study |>
  filter(country == "FRANCE") |>
  count(fishing_technique, gear_code, sort = TRUE) |>
  filter(fishing_technique == "DRB") |>
  print(n = 20)

# France only
# identify top 5 gear categories for France across all years and divisions
top5_gears <- days_summary |>
  filter(country == "FRANCE") |>
  group_by(gear_category) |>
  summarise(total_days = sum(fishing_days_sum), .groups = "drop") |>
  slice_max(total_days, n = 5) |>
  pull(gear_category)

# France only
days_summary |>
  filter(country == "FRANCE") |>
  mutate(gear_plot = if_else(gear_category %in% top5_gears,
                             gear_category, "ZZZ")) |>
  group_by(year, gear_plot, division) |>
  summarise(
    fishing_days_sum = sum(fishing_days_sum),
    complete         = all(days_complete),
    .groups = "drop"
  ) |>
  ggplot(aes(x = year, y = fishing_days_sum,
             colour = gear_plot)) +
  geom_line() +
  geom_point(aes(shape = complete), size = 2) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "Complete", "FALSE" = "Partial"),
                     name   = "Data") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  facet_wrap(~ division) +
  labs(title    = "FDI – France fishing days by gear",
       subtitle = paste("Divisions:", paste(STUDY_DIVS, collapse = " & ")),
       x = "Year", y = "Fishing days") +
  theme_bw()

# 1. Full gear mapping for France with fishing days
effort_study |>
  filter(country == "FRANCE") |>
  group_by(fishing_technique, gear_code, gear_category) |>
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(fishing_days)) |>
  print(n = 40)

# 2. What is in Other/mixed for France?
effort_study |>
  filter(country == "FRANCE", gear_category == "Other/mixed") |>
  group_by(fishing_technique, gear_code) |>
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(fishing_days)) |>
  print(n = 20)

# 3. What is in Pot/trap — is FPO really that large?
effort_study |>
  filter(country == "FRANCE", gear_category == "Handline/rod") |>
  group_by(fishing_technique, gear_code, division) |>
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(fishing_days)) |>
  print(n = 20)

effort_study |>
  filter(country == "FRANCE", gear_category == "Danish seine") |>
  group_by(fishing_technique, gear_code, division) |>
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(fishing_days)) |>
  print(n = 20)
