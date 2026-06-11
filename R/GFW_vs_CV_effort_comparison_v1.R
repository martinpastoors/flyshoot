# ==============================================================================
# GFW vs CV Electronic Logbook — Effort Comparison
# Version 1
#
# PURPOSE:
#   Detailed vessel-level comparison of fishing effort (fishing days) between:
#     1. GFW AIS-based effort  — from Extract_GFW_effort_v7.R outputs
#     2. CV electronic logbook — haul-level trip records per vessel
#
#   Effort metric: unique fishing DAYS per vessel × year × ICES division
#   (one calendar date = one fishing day, regardless of number of hauls/trips)
#
# DATA FLOW:
#   GFW side:  gfw_s8_effort.RData → fishing_days_study → filter to CV vessels
#   CV side:   CV_vessel info file + elog data → derive fishing days per day/div
#   Comparison: bind_rows(gfw_side, cv_side) → plots + diagnostics
#
# SAVED OUTPUTS (in flyshootdata):
#   gfw_vs_cv_comparison.RData   — gfw_cv, fishing_days_gfw_cv (both sources)
#   GFW_vs_CV_comparison_<date>.xlsx — table export
#   Plots to screen (save manually or add ggsave calls)
#
# INPUTS TO CONFIGURE (see STEP 1):
#   flyshootdata     — directory with gfw_s*.RData files and CV data
#   cv_vessel_file  — Excel/CSV with at minimum: vessel, mmsi columns
#   cv_elog_file    — path to CV elog data (RData, parquet, or CSV)
#   cv_elog_type    — "rdata" | "parquet" | "csv"
#   vessel_name_map — named vector mapping GFW vessel_name → short vessel code
#
# REQUIRED CV ELOG FIELDS (after loading):
#   vessel        — short vessel code (e.g. "SL9", "SCH135")
#   date          — Date of fishing activity
#   fao_division  — ICES division string (e.g. "27.7.d" — will be uppercased)
#
# ==============================================================================


# ==============================================================================
# STEP 1 — Configuration
# ==============================================================================

library(tidyverse)
library(lubridate)
library(readxl)

# ---- Paths -------------------------------------------------------------------

flyshootdata    <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data"
flyshootrdata   <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata"

# CV vessel info: must contain columns 'vessel' (short code) and 'mmsi'
# Additional useful columns: vessel_name (as in GFW), flag, gt, gear
cv_vessel_file <- file.path(flyshootdata, "flyshoot vessels CV.xlsx")   # <-- adjust sheet/path

# CV electronic logbook data
cv_elog_file   <- file.path(flyshootrdata, "elog.RData")
cv_elog_type   <- "rdata"   # "rdata" | "parquet" | "csv"

# ---- Study settings ----------------------------------------------------------

study_years  <- 2013:2025
study_divs   <- c("27.4.C", "27.7.D", "27.7.E")

# ---- Vessel name mapping -----------------------------------------------------
# Maps GFW vessel_name values to short CV vessel codes.
# GFW sometimes records multiple name variants for the same vessel —
# list all known aliases. Keys = GFW names, values = CV short codes.
# Example below reflects the vessels known from earlier work; adjust as needed.

vessel_name_map <- c(
  "JOHANNA SL-9"       = "SL9",
  "SL-9 JOHANNA"       = "SL9",
  "SCH-135 GALIBIER"   = "SCH135",
  "SCH-144 VERTROUWEN" = "SCH144",
  "SCH-99 ARAVIS"      = "SCH99",
  "SCH65 SIMPLON"      = "SCH65"
  # Add further vessels here: "GFW_NAME" = "CV_CODE"
)

# ---- Colour scheme -----------------------------------------------------------

source_colours <- c("GFW" = "#4E79A7", "CV" = "#E15759")


# ==============================================================================
# STEP 2 — Load GFW effort data
# ==============================================================================
# Loads fishing_days_study from the v7 pipeline output.
# fishing_days_study columns:
#   ssvid, vessel_name, vessel_flag, gear, gt, size_class,
#   division, zone, year, quarter, fishing_days, gt_fishing_days

message("Loading GFW effort data ...")

fishing_days_study <- local({
  e <- new.env()
  load(file.path(flyshootdata, "gfw_s7_effort.RData"), envir = e)
  e$fishing_days_study
})

message("  fishing_days_study: ",
        nrow(fishing_days_study), " rows | ",
        n_distinct(fishing_days_study$ssvid), " vessels | ",
        sum(fishing_days_study$fishing_days), " total days")


# ==============================================================================
# STEP 3 — Load CV vessel info
# ==============================================================================
# Expects a table linking the short CV vessel codes to MMSI numbers.
# Minimum required columns: vessel (short code), mmsi
# Optional but useful: vessel_name_gfw (as recorded by GFW), flag, gt, gear

message("Loading CV vessel info ...")

vessels_cv <- read_excel(cv_vessel_file) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(!is.na(mmsi), !is.na(vessel))

message("  CV vessels loaded: ", nrow(vessels_cv), " rows")
message("  Vessels: ", paste(sort(unique(vessels_cv$vessel)), collapse = ", "))

# Verify all CV MMSIs appear in GFW data
mmsi_in_gfw  <- unique(fishing_days_study$ssvid)
mmsi_missing <- setdiff(vessels_cv$mmsi, mmsi_in_gfw)

if (length(mmsi_missing) > 0) {
  message("  WARNING: ", length(mmsi_missing),
          " CV MMSIs not found in fishing_days_study:")
  message("    ", paste(mmsi_missing, collapse = ", "))
} else {
  message("  All CV MMSIs present in GFW data")
}


# ==============================================================================
# STEP 4 — Load CV electronic logbook data
# ==============================================================================
# Loads elog and standardises to required fields:
#   vessel       — short vessel code
#   date         — Date
#   fao_division — ICES division (will be uppercased → e.g. "27.7.D")

message("Loading CV electronic logbook data (", cv_elog_type, ") ...")

elog <- switch(cv_elog_type,

  "rdata" = {
    e <- new.env()
    load(cv_elog_file, envir = e)
    # Assumes the RData file contains an object called 'elog'
    # If named differently, adjust here:
    if (exists("elog", envir = e)) {
      e$elog
    } else {
      stop("RData file does not contain 'elog'. ",
           "Objects found: ", paste(ls(e), collapse = ", "))
    }
  },

  "parquet" = {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop("Package 'arrow' needed for parquet. Install with: install.packages('arrow')")
    arrow::read_parquet(cv_elog_file)
  },

  "csv" = {
    read_csv(cv_elog_file, col_types = cols(.default = col_character()))
  },

  stop("Unknown cv_elog_type: '", cv_elog_type,
       "'. Use 'rdata', 'parquet', or 'csv'.")
)

message("  elog: ", nrow(elog), " rows")
message("  Columns: ", paste(names(elog), collapse = ", "))

# ---- Validate required columns ----
required_cols <- c("vessel", "date", "faozone")
missing_cols  <- setdiff(required_cols, names(elog))

if (length(missing_cols) > 0) {
  stop("elog is missing required columns: ",
       paste(missing_cols, collapse = ", "),
       "\nAvailable columns: ", paste(names(elog), collapse = ", "))
}

# ---- Standardise types ----
elog <- elog %>%
  mutate(
    date         = as.Date(date),
    fao_division = toupper(str_trim(faozone))
  )

message("  Date range: ",
        format(min(elog$date, na.rm = TRUE)), " to ",
        format(max(elog$date, na.rm = TRUE)))
message("  Divisions in elog: ",
        paste(sort(unique(elog$fao_division)), collapse = ", "))
message("  Vessels in elog:  ",
        paste(sort(unique(elog$vessel)), collapse = ", "))


# ==============================================================================
# STEP 5 — Derive CV fishing days
# ==============================================================================
# Counts unique fishing dates per vessel × year × ICES division,
# matching the same aggregation used in Extract_GFW_effort_v7.R Step 8.

message("Deriving CV fishing days (majority-rule deduplication)...")

# Step 1: count hauls per vessel × date × division combination
cv_daily_zones <- elog %>%
  filter(
    !is.na(date),
    !is.na(vessel),
    !is.na(fao_division),
    fao_division %in% study_divs,
    year(date) %in% study_years
  ) %>%
  mutate(year = year(date), quarter = quarter(date)) %>%
  group_by(vessel, date, year, quarter, division = fao_division) %>%
  summarise(n_hauls = n(), .groups = "drop")

# How many vessel-dates span multiple divisions?
n_split_cv <- cv_daily_zones %>%
  group_by(vessel, date) %>%
  filter(n_distinct(division) > 1) %>%
  n_groups()
message("  Vessel-dates spanning multiple divisions: ", n_split_cv)

# Step 2: majority rule — keep only the dominant division per vessel-date
cv_daily_dominant <- cv_daily_zones %>%
  group_by(vessel, date, year, quarter) %>%
  slice_max(n_hauls, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(-n_hauls)

# Step 3: aggregate to fishing days
fishing_days_cv <- cv_daily_dominant %>%
  group_by(vessel, year, quarter, division) %>%
  summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
  left_join(
    vessels_cv %>%
      dplyr::select(vessel, mmsi, any_of(c("flag", "gt", "gear"))),
    by = "vessel"
  ) %>%
  mutate(source = "CV")

message("  CV fishing_days: ",
        nrow(fishing_days_cv), " rows | ",
        n_distinct(fishing_days_cv$vessel), " vessels | ",
        sum(fishing_days_cv$fishing_days), " total days")

# Sanity check — compare with naive count
naive_days <- elog %>%
  filter(!is.na(date), !is.na(vessel),
         fao_division %in% study_divs,
         year(date) %in% study_years) %>%
  distinct(vessel, date, fao_division) %>%
  nrow()

message("  Naive count (with double-counting): ", naive_days)
message("  Majority-rule count:                ",
        sum(fishing_days_cv$fishing_days))
message("  Difference:                         ",
        naive_days - sum(fishing_days_cv$fishing_days))


# fishing_days_cv <- elog %>%
#   filter(
#     !is.na(date),
#     !is.na(vessel),
#     !is.na(fao_division),
#     fao_division %in% study_divs,
#     year(date) %in% study_years
#   ) %>%
#   # One fishing day = one distinct date per vessel × division
#   distinct(vessel, date, fao_division) %>%
#   mutate(
#     year    = year(date),
#     quarter = quarter(date)
#   ) %>%
#   group_by(vessel, year, quarter, division = fao_division) %>%
#   summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
#   # Join MMSI and vessel characteristics from CV vessel info
#   left_join(
#     vessels_cv %>% dplyr::select(vessel, mmsi, any_of(c("flag", "gt", "gear"))),
#     by = "vessel"
#   ) %>%
#   mutate(source = "CV")
# 
# message("  CV fishing_days: ",
#         nrow(fishing_days_cv), " rows | ",
#         n_distinct(fishing_days_cv$vessel), " vessels | ",
#         sum(fishing_days_cv$fishing_days), " total days")


# ==============================================================================
# STEP 6 — Derive GFW fishing days for CV vessels only
# ==============================================================================
# Filters fishing_days_study to the CV vessel MMSIs and
# translates GFW vessel_name to the same short vessel codes used in CV data.

message("Deriving GFW fishing days for CV vessels ...")

fishing_days_gfw_cv <- fishing_days_study %>%
  filter(
    ssvid %in% vessels_cv$mmsi,
    year  %in% study_years,
    division %in% study_divs
  ) %>%
  # Map GFW vessel_name → CV short vessel code
  mutate(
    vessel = vessel_name_map[vessel_name]
  ) %>%
  # If no name mapping matched, fall back to the MMSI lookup
  left_join(
    vessels_cv %>% dplyr::select(mmsi, vessel_cv = vessel),
    by = c("ssvid" = "mmsi")
  ) %>%
  mutate(
    vessel = coalesce(vessel, vessel_cv)
  ) %>%
  dplyr::select(-vessel_cv) %>%
  filter(!is.na(vessel)) %>%
  group_by(vessel, ssvid, vessel_name, vessel_flag, gear, gt,
           size_class, division, zone, year, quarter) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(source = "GFW")

message("  GFW fishing_days (CV vessels): ",
        nrow(fishing_days_gfw_cv), " rows | ",
        n_distinct(fishing_days_gfw_cv$vessel), " vessels | ",
        sum(fishing_days_gfw_cv$fishing_days), " total days")

# Check for any CV vessels with no GFW match
vessels_no_gfw <- setdiff(vessels_cv$vessel, fishing_days_gfw_cv$vessel)
if (length(vessels_no_gfw) > 0) {
  message("  WARNING: No GFW events found for CV vessels: ",
          paste(vessels_no_gfw, collapse = ", "))
}

vessels_no_cv <- setdiff(fishing_days_gfw_cv$vessel, fishing_days_cv$vessel)
if (length(vessels_no_cv) > 0) {
  message("  NOTE: GFW records exist but no CV elog records for: ",
          paste(vessels_no_cv, collapse = ", "))
}


# ==============================================================================
# STEP 7 — Combine GFW and CV into comparison table
# ==============================================================================

message("Building comparison table ...")

# ---- Long format: one row per vessel × year × division × source ----
fishing_days_gfw_cv_long <- bind_rows(

  # GFW side — annual fishing days per vessel × division
  fishing_days_gfw_cv %>%
    group_by(vessel, ssvid, vessel_flag, gear, gt, size_class,
             division, year, source) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop"),

  # CV side — annual fishing days per vessel × division
  fishing_days_cv %>%
    group_by(vessel, division, year, source) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    # Attach vessel metadata from vessels_cv for consistent columns
    left_join(
      vessels_cv %>%
        dplyr::select(vessel, ssvid = mmsi,
                      any_of(c("vessel_flag" = "flag",
                               "gt", "gear"))),
      by = "vessel"
    )
)

# ---- Wide format: GFW and CV as side-by-side columns ----
fishing_days_wide <- fishing_days_gfw_cv_long %>%
  pivot_wider(
    id_cols     = c(vessel, division, year),
    names_from  = source,
    values_from = fishing_days,
    values_fill = 0
  ) %>%
  mutate(
    diff         = GFW - CV,
    ratio        = if_else(CV > 0, GFW / CV, NA_real_),
    pct_diff     = if_else(CV > 0, (GFW - CV) / CV * 100, NA_real_),
    agree        = abs(diff) <= 5,    # within 5 days = reasonable agreement
    direction    = case_when(
      diff >  5 ~ "GFW higher",
      diff < -5 ~ "CV higher",
      TRUE      ~ "Agreement"
    )
  ) %>%
  arrange(vessel, division, year)

message("  Comparison table: ", nrow(fishing_days_wide), " rows")

# ---- Summary statistics ----
message("\n===== COMPARISON SUMMARY =====")
fishing_days_wide %>%
  summarise(
    n_vessel_years  = n(),
    mean_GFW        = round(mean(GFW, na.rm = TRUE), 1),
    mean_CV         = round(mean(CV,  na.rm = TRUE), 1),
    mean_diff       = round(mean(diff, na.rm = TRUE), 1),
    median_diff     = round(median(diff, na.rm = TRUE), 1),
    mean_ratio      = round(mean(ratio, na.rm = TRUE), 2),
    pct_agreement   = round(mean(agree, na.rm = TRUE) * 100, 1),
    n_gfw_higher    = sum(direction == "GFW higher"),
    n_cv_higher     = sum(direction == "CV higher"),
    n_agree         = sum(direction == "Agreement")
  ) %>%
  print()

message("\nAgreement by vessel:")
fishing_days_wide %>%
  group_by(vessel) %>%
  summarise(
    n_years       = n(),
    mean_GFW      = round(mean(GFW),  1),
    mean_CV       = round(mean(CV),   1),
    mean_diff     = round(mean(diff), 1),
    mean_ratio    = round(mean(ratio, na.rm = TRUE), 2),
    pct_agreement = round(mean(agree) * 100, 1),
    .groups       = "drop"
  ) %>%
  print()

message("\nAgreement direction by division:")
fishing_days_wide %>%
  group_by(division, direction) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) %>%
  print()

# Save comparison tables
save(fishing_days_gfw_cv_long, fishing_days_wide,
     file = file.path(flyshootdata, "gfw_vs_cv_comparison.RData"))
message("Saved gfw_vs_cv_comparison.RData")


# ==============================================================================
# STEP 8 — Plots
# ==============================================================================

# ---- Plot 1: Annual fishing days — GFW vs CV, faceted by vessel ----
# One panel per vessel, lines show trajectory of both sources

fishing_days_gfw_cv_long %>%
  group_by(vessel, year, source) %>%          # division removed
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days,
             colour = source, shape = source, linetype = source)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_colour_manual(values = source_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", colour = "Source",
       shape = "Source", linetype = "Source",
       title = "GFW vs CV electronic logbook — annual fishing days by vessel (all divisions combined)") +
  facet_wrap(~ vessel)


# ---- Plot 2: Annual fishing days — GFW vs CV, faceted by vessel × division ----

fishing_days_gfw_cv_long %>%
  filter(!vessel %in% c("CC545762","CC622598")) %>% 
  ggplot(aes(x = year, y = fishing_days,
             colour = source, shape = source, linetype = source)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_colour_manual(values = source_colours) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", colour = "Source",
       shape = "Source", linetype = "Source",
       title = "GFW vs CV — annual fishing days by vessel and ICES division") +
  facet_grid(vessel ~ division)


# ---- Plot 3: Scatter — GFW vs CV fishing days, coloured by vessel ----
# Points above the 1:1 line = GFW reports more days than CV

fishing_days_wide %>%
  filter(!vessel %in% c("CC545762", "CC622598")) %>%
  filter(GFW > 0, CV > 0) %>%          # only years present in both sources
  ggplot(aes(x = CV, y = GFW, colour = vessel)) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_colour_brewer(palette = "Set1") +
  coord_equal() +
  labs(x = "CV logbook fishing days", y = "GFW fishing days",
       colour = "Vessel",
       title = "GFW vs CV fishing days — scatter (per vessel × year × division)",
       subtitle = paste0("Dashed line = perfect agreement | above line = GFW higher",
                         "\nOnly vessel-years present in both sources (GFW > 0 & CV > 0)"))


# ---- Plot 4: Difference (GFW − CV) by year, faceted by vessel ----
# Positive = GFW reports more; negative = CV reports more

# fishing_days_wide %>%
#   filter(!vessel %in% c("CC545762","CC622598")) %>% 
#   ggplot(aes(x = year, y = diff, fill = direction)) +
#   theme_bw() +
#   geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.5) +
#   geom_hline(yintercept = c(-5, 5), colour = "grey70",
#              linewidth = 0.4, linetype = "dotted") +
#   geom_col(width = 0.8) +
#   scale_fill_manual(values = c(
#     "GFW higher" = "#4E79A7",
#     "CV higher"  = "#E15759",
#     "Agreement"  = "#59A14F"
#   )) +
#   scale_x_continuous(breaks = study_years) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(x = "", y = "GFW − CV (days)", fill = "Direction",
#        title = "Difference in fishing days: GFW minus CV logbook",
#        subtitle = "Green = agreement (±5 days) | Dotted lines = ±5 day threshold") +
#   facet_grid(division ~ vessel)


# ---- Plot 5: Ratio (GFW / CV) by year, faceted by vessel ----
# Ratio = 1 means perfect agreement; > 1 GFW overcounts; < 1 undercounts

# fishing_days_wide %>%
#   filter(!is.na(ratio)) %>%
#   ggplot(aes(x = year, y = ratio, colour = vessel)) +
#   theme_bw() +
#   geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
#   geom_hline(yintercept = c(0.8, 1.2), linetype = "dotted",
#              colour = "grey70") +
#   geom_point(size = 2.5) +
#   geom_line(linewidth = 0.6, alpha = 0.8) +
#   scale_colour_brewer(palette = "Set1") +
#   scale_x_continuous(breaks = study_years) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(x = "", y = "GFW / CV ratio", colour = "Vessel",
#        title = "GFW / CV fishing days ratio by year and vessel",
#        subtitle = "Dashed = 1.0 (perfect agreement) | Dotted = ±20% band") +
#   facet_wrap(~ division)


# ---- Plot 6: Quarterly comparison — GFW vs CV, faceted by vessel ----
# Shows whether GFW-CV divergence is seasonal

# fishing_days_cv %>%
#   group_by(vessel, division, year, quarter) %>%
#   summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
#   mutate(source = "CV") %>%
#   bind_rows(
#     fishing_days_gfw_cv %>%
#       group_by(vessel, division, year, quarter) %>%
#       summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
#       mutate(source = "GFW")
#   ) %>%
#   mutate(year_q = year + (quarter - 1) / 4) %>%
#   ggplot(aes(x = year_q, y = fishing_days,
#              colour = source, shape = source)) +
#   theme_bw() +
#   geom_point(size = 1.8) +
#   geom_line(linewidth = 0.5, alpha = 0.7) +
#   scale_colour_manual(values = source_colours) +
#   scale_x_continuous(breaks = study_years) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   labs(x = "", y = "Fishing days", colour = "Source", shape = "Source",
#        title = "GFW vs CV — quarterly fishing days by vessel",
#        subtitle = "x-axis = year + quarter fraction") +
#   facet_wrap(~ vessel)


# ==============================================================================
# STEP 9 — Excel export
# ==============================================================================

message("Exporting comparison tables to Excel ...")

writexl::write_xlsx(
  list(
    "GFW vs CV (wide)"  = fishing_days_wide,
    "GFW vs CV (long)"  = fishing_days_gfw_cv_long,
    "GFW days (CV vessels)" = fishing_days_gfw_cv %>%
      group_by(vessel, ssvid, vessel_flag, gear, gt,
               size_class, division, year) %>%
      summarise(fishing_days = sum(fishing_days), .groups = "drop"),
    "CV days"           = fishing_days_cv %>%
      group_by(vessel, division, year) %>%
      summarise(fishing_days = sum(fishing_days), .groups = "drop"),
    "CV vessel info"    = vessels_cv
  ),
  path = file.path(flyshootdata,
                   paste0("GFW_vs_CV_comparison_", Sys.Date(), ".xlsx"))
)

message("Excel export saved: GFW_vs_CV_comparison_", Sys.Date(), ".xlsx")
message("\nDone.")



# ==============================================================================
# GFW vs CV — Discrepancy Diagnostics
# ==============================================================================
# Run after GFW_vs_CV_effort_comparison_v1.R
# Requires in environment: fishing_days_wide, fishing_days_cv,
#   fishing_days_gfw_cv, elog, vessels_cv
# ==============================================================================

focus_vessels <- c("SCH65", "SL9")   # vessels to investigate
focus_years   <- 2017:2020


# ==============================================================================
# DIAG 1 — CV duplicate check
# ==============================================================================
# Are there multiple haul records on the same date for the same vessel?
# A legitimate day = 1 unique (vessel, date, division) combination.
# More than one record per combination = potential double-counting.

message("===== DIAG 1: CV duplicate records =====")

cv_daily_counts <- elog %>%
  filter(vessel %in% focus_vessels,
         year(date) %in% focus_years) %>%
  mutate(division = toupper(fao_division)) %>%
  group_by(vessel, date, division) %>%
  summarise(n_records = n(), .groups = "drop")

cv_daily_counts %>%
  group_by(vessel) %>%
  summarise(
    total_date_div_combos = n(),
    combos_with_1_record  = sum(n_records == 1),
    combos_with_2plus     = sum(n_records > 1),
    max_records_per_day   = max(n_records),
    .groups = "drop"
  ) %>%
  print()

# Show the worst offenders
message("\nDates with most duplicate records:")
cv_daily_counts %>%
  filter(n_records > 1) %>%
  arrange(vessel, desc(n_records)) %>%
  print(n = 30)

# How many extra days does double-counting add per year?
message("\nExtra days from double-counting per year (if any):")
elog %>%
  filter(vessel %in% focus_vessels,
         year(date) %in% focus_years) %>%
  mutate(division = toupper(fao_division)) %>%
  group_by(vessel, date, division) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  mutate(year = year(date)) %>%
  group_by(vessel, year) %>%
  summarise(
    raw_fishing_days      = n(),                        # current method (n_distinct dates)
    deduplicated_days     = n_distinct(date),           # same as current — already distinct
    n_duplicated_combos   = sum(n_records > 1),
    .groups               = "drop"
  ) %>%
  print()

# NOTE: fishing_days_cv already uses n_distinct(date) so date-level
# duplicates within a division are already collapsed.
# But if the same date appears in TWO divisions in the elog, it gets
# counted TWICE. Check for that:

message("\nDates appearing in multiple divisions (adds days to total):")
elog %>%
  filter(vessel %in% focus_vessels,
         year(date) %in% focus_years) %>%
  mutate(division = toupper(fao_division)) %>%
  distinct(vessel, date, division) %>%
  group_by(vessel, date) %>%
  filter(n() > 1) %>%
  arrange(vessel, date) %>%
  print(n = 40)


# ==============================================================================
# DIAG 2 — CV days by division: what is being included?
# ==============================================================================
# Check whether CV days in study_divs match what you expect,
# and whether any CV days fall outside the study divisions.

message("\n===== DIAG 2: CV days by division =====")

elog %>%
  filter(vessel %in% focus_vessels,
         year(date) %in% focus_years) %>%
  mutate(
    division = toupper(fao_division),
    year     = year(date)
  ) %>%
  distinct(vessel, date, division, year) %>%
  group_by(vessel, year, division) %>%
  summarise(fishing_days = n(), .groups = "drop") %>%
  arrange(vessel, year, division) %>%
  print(n = 60)

message("\nTotal CV days OUTSIDE study_divs (", 
        paste(study_divs, collapse=", "), "):")
elog %>%
  filter(vessel %in% focus_vessels,
         year(date) %in% focus_years) %>%
  mutate(
    division = toupper(fao_division),
    year     = year(date),
    in_study = division %in% study_divs
  ) %>%
  distinct(vessel, date, division, year, in_study) %>%
  group_by(vessel, year, in_study) %>%
  summarise(fishing_days = n(), .groups = "drop") %>%
  pivot_wider(names_from = in_study, values_from = fishing_days,
              values_fill = 0, names_prefix = "in_study_") %>%
  rename(in_study_divs = in_study_TRUE,
         outside_study = in_study_FALSE) %>%
  print(n = 40)


# ==============================================================================
# DIAG 3 — GFW AIS gap check
# ==============================================================================
# Are there stretches of time where GFW recorded NO events for these vessels?
# Uses events_df (raw, pre-spatial filter) if available, else events_classified.

message("\n===== DIAG 3: GFW AIS gap check =====")

# Use whichever event table is available — prefer pre-filter (events_df)
gfw_event_source <- if (exists("events_df")) "events_df (pre-spatial filter)" else
  if (exists("events_classified")) "events_classified (post-filter)" else
    stop("No GFW event table found in environment. Load events_df or events_classified.")

message("Using: ", gfw_event_source)

gfw_events_focus <- get(
  if (exists("events_df")) "events_df" else "events_classified"
) %>%
  # Rename FIRST, then filter
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  
  filter(ssvid %in% vessels_cv$mmsi[vessels_cv$vessel %in% focus_vessels])

message("GFW events for focus vessels: ", nrow(gfw_events_focus))

# Join vessel short code
gfw_events_focus <- gfw_events_focus %>%
  left_join(vessels_cv %>% dplyr::select(mmsi, vessel), 
            by = c("ssvid" = "mmsi"))

# Monthly event counts — zeros reveal AIS gaps
monthly_gfw <- gfw_events_focus %>%
  mutate(
    year  = year(as.Date(start)),
    month = month(as.Date(start))
  ) %>%
  filter(year %in% focus_years) %>%
  group_by(vessel, year, month) %>%
  summarise(n_events = n(), .groups = "drop")

# Complete grid — fill missing months with 0
monthly_gfw_complete <- crossing(
  vessel = focus_vessels,
  year   = focus_years,
  month  = 1:12
) %>%
  left_join(monthly_gfw, by = c("vessel", "year", "month")) %>%
  replace_na(list(n_events = 0)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

message("\nMonths with zero GFW events (AIS gaps):")
monthly_gfw_complete %>%
  filter(n_events == 0) %>%
  arrange(vessel, date) %>%
  print(n = 50)

# Plot: monthly GFW event counts with gap highlighting
monthly_gfw_complete %>%
  ggplot(aes(x = date, y = n_events)) +
  theme_bw() +
  geom_col(aes(fill = n_events == 0), width = 25) +
  scale_fill_manual(values = c("FALSE" = "#4E79A7", "TRUE" = "#E15759"),
                    labels = c("FALSE" = "Events recorded", "TRUE" = "No events (gap)"),
                    name = "") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "GFW fishing events per month",
       title = "GFW monthly event counts — AIS gap detection",
       subtitle = "Red bars = months with zero fishing events recorded by GFW") +
  facet_wrap(~ vessel, ncol = 1)


# ==============================================================================
# DIAG 4 — GFW spatial filter loss check
# ==============================================================================
# How many GFW events are lost at each filtering stage for these vessels?
# Requires events_df (raw) AND events_classified to be in environment.

if (exists("events_df") && exists("events_classified")) {
  
  message("\n===== DIAG 4: GFW spatial filter losses =====")
  
  focus_mmsi <- vessels_cv$mmsi[vessels_cv$vessel %in% focus_vessels]
  
  # Count events at each pipeline stage
  stage_counts <- tibble(
    stage = c(
      "1_raw_events_df",
      "2_events_marine_clean",
      "3_events_classified"
    ),
    n_events = c(
      events_df %>%
        rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%   # rename first
        filter(ssvid %in% focus_mmsi) %>%
        nrow(),
      if (exists("events_marine_clean")) {
        events_marine_clean %>%
          rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%   # rename first
          filter(ssvid %in% focus_mmsi) %>%
          nrow()
      } else NA_integer_,
      events_classified %>%
        rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%   # rename first
        filter(ssvid %in% focus_mmsi) %>%
        nrow()
    )
  ) %>%
    mutate(
      pct_retained = round(n_events / first(n_events) * 100, 1),
      lost         = lag(n_events, default = first(n_events)) - n_events
    )
  
  message("Event counts through spatial pipeline:")
  print(stage_counts)
  
  # Annual breakdown — where does the loss concentrate?
  message("\nAnnual GFW events: raw vs post-filter (focus vessels):")
  
  raw_annual <- events_df %>%
    rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
    filter(ssvid %in% focus_mmsi) %>%
    left_join(vessels_cv %>% dplyr::select(mmsi, vessel), by = c("ssvid" = "mmsi")) %>%
    mutate(year = year(as.Date(start))) %>%
    filter(year %in% focus_years) %>%
    group_by(vessel, year) %>%
    summarise(n_raw = n(), .groups = "drop")
  
  filtered_annual <- events_classified %>%
    rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
    filter(ssvid %in% focus_mmsi) %>%
    left_join(vessels_cv %>% dplyr::select(mmsi, vessel), by = c("ssvid" = "mmsi")) %>%
    mutate(year = year(as.Date(start))) %>%
    filter(year %in% focus_years) %>%
    group_by(vessel, year) %>%
    summarise(n_filtered = n(), .groups = "drop")
  
  raw_annual %>%
    left_join(filtered_annual, by = c("vessel", "year")) %>%
    mutate(
      pct_retained = round(n_filtered / n_raw * 100, 1),
      n_lost       = n_raw - n_filtered
    ) %>%
    arrange(vessel, year) %>%
    print()
  
} else {
  message("\nDIAG 4 skipped — needs both events_df and events_classified in environment")
}


# ==============================================================================
# DIAG 5 — Side-by-side day-level comparison for a single vessel × year
# ==============================================================================
# Shows exactly which dates are in CV but not GFW, and vice versa.
# Change vessel_check and year_check to focus on specific problem periods.

vessel_check <- "SCH65"
year_check   <- 2014

message("\n===== DIAG 5: Date-level comparison — ",
        vessel_check, " ", year_check, " =====")

mmsi_check <- vessels_cv$mmsi[vessels_cv$vessel == vessel_check]

cv_dates <- elog %>%
  filter(vessel == vessel_check, year(date) == year_check) %>%
  mutate(division = toupper(fao_division)) %>%
  filter(division %in% study_divs) %>%
  distinct(date, division) %>%
  mutate(in_cv = TRUE)

gfw_dates <- events_classified %>%
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  filter(ssvid == mmsi_check,
         year(as.Date(start)) == year_check,
         division %in% study_divs) %>%
  mutate(date = as.Date(start)) %>%
  distinct(date, division) %>%
  mutate(in_gfw = TRUE)

date_comparison <- full_join(cv_dates, gfw_dates, by = c("date", "division")) %>%
  replace_na(list(in_cv = FALSE, in_gfw = FALSE)) %>%
  mutate(
    status = case_when(
      in_cv & in_gfw  ~ "Both",
      in_cv & !in_gfw ~ "CV only",
      !in_cv & in_gfw ~ "GFW only"
    )
  ) %>%
  arrange(division, date)

message("Summary:")
date_comparison %>% count(status) %>% print()

message("\nDates in CV but not in GFW (possible AIS gaps):")
date_comparison %>%
  filter(status == "CV only") %>%
  arrange(date) %>%
  print()

message("\nDates in GFW but not in CV:")
date_comparison %>%
  filter(status == "GFW only") %>%
  arrange(date) %>%
  print()

# Calendar heatmap for the focus year
date_comparison %>%
  mutate(month = month(date, label = TRUE),
         day   = day(date)) %>%
  ggplot(aes(x = day, y = month, fill = status)) +
  theme_bw() +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Both"     = "#59A14F",
    "CV only"  = "#E15759",
    "GFW only" = "#4E79A7"
  )) +
  labs(x = "Day of month", y = "", fill = "Source",
       title = paste0("Fishing day presence — ", vessel_check, " ", year_check),
       subtitle = "Green = both agree | Red = CV only (AIS gap?) | Blue = GFW only") +
  facet_wrap(~ division)





# Calendar heatmap — all years for a single vessel, faceted by year

vessel_check <- "SL9"   # change as needed

mmsi_check <- vessels_cv$mmsi[vessels_cv$vessel == vessel_check]

cv_dates_all <- elog %>%
  filter(vessel == vessel_check) %>%
  mutate(division = toupper(faozone)) %>%
  filter(division %in% study_divs) %>%
  distinct(date) %>%           # date only
  mutate(in_cv = TRUE)

gfw_dates_all <- events_classified %>%
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  filter(ssvid == mmsi_check,
         division %in% study_divs) %>%
  mutate(date = as.Date(start)) %>%
  distinct(date) %>%           # date only
  mutate(in_gfw = TRUE)

date_comparison_all <- full_join(cv_dates_all, gfw_dates_all,
                                 by = "date") %>%    # join on date only
  replace_na(list(in_cv = FALSE, in_gfw = FALSE)) %>%
  mutate(
    year   = year(date),
    month  = month(date, label = TRUE),
    day    = day(date),
    status = case_when(
      in_cv & in_gfw  ~ "Both",
      in_cv & !in_gfw ~ "CV only",
      !in_cv & in_gfw ~ "GFW only"
    )
  ) %>%
  filter(!is.na(year))

# Summary per year
message("Summary by year for ", vessel_check, ":")
date_comparison_all %>%
  group_by(year) %>%
  count(status) %>%
  pivot_wider(names_from = status, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  print(n = 20)

# Plot — faceted by year, one row per year
date_comparison_all %>%
  filter(year(date) < 2026) %>% 
  
  ggplot(aes(x = day, y = month, fill = status)) +
  theme_bw() +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "Both"     = "#59A14F",
    "CV only"  = "#E15759",
    "GFW only" = "#4E79A7"
  )) +
  scale_x_continuous(breaks = c(1, 10, 20, 31)) +
  facet_wrap(~ year) +
  labs(x = "Day of month", y = "", fill = "Source",
       title = paste0("Fishing day presence — ", vessel_check, " (all years)"),
       subtitle = "Green = both agree | Red = CV only | Blue = GFW only") +
  theme(
    strip.text      = element_text(face = "bold"),
    panel.spacing.y = unit(0.3, "lines"),
    legend.position = "bottom"
  )







# ==============================================================================
# GFW vs CV — Calendar heatmap PDF for all CV vessels
# ==============================================================================

library(tidyverse)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)

pdf(
  file   = file.path(flyshootdir,
                     paste0("GFW_vs_CV_calendar_", Sys.Date(), ".pdf")),
  width  = 14,
  height = 10
)

# ---- Introductory text page --------------------------------------------------

intro_text <- paste0(
  "GFW vs CV Electronic Logbook — Fishing Day Comparison\n",
  "Generated: ", format(Sys.Date(), "%d %B %Y"), "\n\n",
  
  "HOW THIS ANALYSIS WAS GENERATED\n\n",
  
  "Fishing days from two independent sources are compared at the daily level ",
  "for each vessel across all available years. The study area covers ICES ",
  "divisions ", paste(study_divs, collapse = ", "), ".\n\n",
  
  "  GFW (Global Fishing Watch): Fishing events are derived from AIS ",
  "vessel tracking data using a machine-learning classifier that identifies ",
  "fishing activity from vessel speed and movement patterns. Each unique ",
  "calendar date with at least one classified fishing event is counted as ",
  "one fishing day.\n\n",
  
  "  CV (Electronic Logbook): Fishing days are derived from haul-level trip ",
  "records submitted by vessel operators. Each unique calendar date with at ",
  "least one recorded haul within the study divisions is counted as one ",
  "fishing day.\n\n",
  
  "For each vessel, the two sets of fishing dates are matched by calendar ",
  "date (division is used to filter both sources but not for matching). ",
  "Each day is classified as:\n\n",
  
  "  GREEN  — Both: date present in both GFW and CV logbook (agreement)\n",
  "  RED    — CV only: date in logbook but no GFW fishing event recorded\n",
  "  BLUE   — GFW only: date in GFW but no corresponding logbook record\n\n",
  
  "WHAT THE DIFFERENCES MAY MEAN\n\n",
  
  "CV only (red) — possible causes:\n",
  "  - AIS transponder switched off or malfunctioning on that day\n",
  "  - GFW classifier missed the fishing activity (e.g. slow steaming gear)\n",
  "  - Vessel fished in an area with poor satellite AIS coverage\n",
  "  - Logbook records a day as fishing that GFW does not classify as such\n\n",
  
  "GFW only (blue) — possible causes:\n",
  "  - Fishing activity recorded by AIS but not submitted in the logbook\n",
  "  - GFW classifier false positive (vessel steaming classified as fishing)\n",
  "  - Logbook records the trip under a different division or date\n",
  "  - Vessel was active but the trip was not yet processed in the CV data\n\n",
  
  "Systematic patterns (e.g. consistently red in a particular year or season) ",
  "are more informative than isolated mismatches. A high proportion of green ",
  "cells indicates good agreement between the two data sources."
)

grid.newpage()
grid.text(
  intro_text,
  x      = 0.05,
  y      = 0.97,
  just   = c("left", "top"),
  gp     = gpar(fontsize = 10, fontfamily = "mono", lineheight = 1.4)
)

# ---- One page per vessel -----------------------------------------------------

all_vessels <- sort(unique(vessels_cv$vessel))

for (vessel_check in all_vessels) {
  
  message("Processing vessel: ", vessel_check)
  
  mmsi_check <- vessels_cv$mmsi[vessels_cv$vessel == vessel_check]
  
  if (length(mmsi_check) == 0 || is.na(mmsi_check)) {
    message("  Skipping — no MMSI found")
    next
  }
  
  # CV dates
  cv_dates_all <- elog %>%
    filter(vessel == vessel_check) %>%
    mutate(division = toupper(faozone)) %>%
    filter(division %in% study_divs) %>%
    distinct(date) %>%
    mutate(in_cv = TRUE)
  
  # GFW dates
  gfw_dates_all <- events_classified %>%
    rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
    filter(ssvid == mmsi_check,
           division %in% study_divs) %>%
    mutate(date = as.Date(start)) %>%
    distinct(date) %>%
    mutate(in_gfw = TRUE)
  
  # Skip if both are empty
  if (nrow(cv_dates_all) == 0 && nrow(gfw_dates_all) == 0) {
    message("  Skipping — no data in either source")
    next
  }
  
  # Combine
  date_comparison_all <- full_join(cv_dates_all, gfw_dates_all, by = "date") %>%
    replace_na(list(in_cv = FALSE, in_gfw = FALSE)) %>%
    mutate(
      year   = year(date),
      month  = month(date, label = TRUE),
      day    = day(date),
      status = case_when(
        in_cv & in_gfw  ~ "Both",
        in_cv & !in_gfw ~ "CV only",
        !in_cv & in_gfw ~ "GFW only"
      )
    ) %>%
    filter(!is.na(year), year < 2026)
  
  if (nrow(date_comparison_all) == 0) {
    message("  Skipping — no overlapping date range")
    next
  }
  
  # Per-year summary for subtitle
  year_summary <- date_comparison_all %>%
    group_by(year) %>%
    count(status) %>%
    pivot_wider(names_from = status, values_from = n, values_fill = 0) %>%
    mutate(
      Both     = if ("Both"     %in% names(.)) Both     else 0L,
      CV_only  = if ("CV only"  %in% names(.)) `CV only`  else 0L,
      GFW_only = if ("GFW only" %in% names(.)) `GFW only` else 0L,
      total    = Both + CV_only + GFW_only,
      pct_both = round(Both / total * 100)
    ) %>%
    ungroup()
  
  overall_pct <- round(
    sum(year_summary$Both) /
      sum(year_summary$total) * 100
  )
  
  n_cv_days  <- sum(date_comparison_all$in_cv)
  n_gfw_days <- sum(date_comparison_all$in_gfw)
  
  subtitle_text <- paste0(
    "Green = both agree | Red = CV only | Blue = GFW only",
    "     |     CV days: ", n_cv_days,
    "  |  GFW days: ", n_gfw_days,
    "  |  Overall agreement: ", overall_pct, "%"
  )
  
  # Plot
  p <- date_comparison_all %>%
    ggplot(aes(x = day, y = month, fill = status)) +
    theme_bw() +
    geom_tile(colour = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = c("Both" = "#59A14F", "CV only" = "#E15759", "GFW only" = "#4E79A7"),
      drop   = FALSE
    ) +
    scale_x_continuous(breaks = c(1, 10, 20, 31)) +
    facet_wrap(~ year, nrow = 3) +
    labs(
      x        = "Day of month",
      y        = "",
      fill     = "Source",
      title    = paste0("Fishing day presence — ", vessel_check,
                        "  (MMSI: ", mmsi_check, ")"),
      subtitle = subtitle_text
    ) +
    theme(
      strip.text      = element_text(face = "bold"),
      panel.spacing   = unit(0.4, "lines"),
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 9, colour = "grey40")
    )
  
  print(p)
  message("  Done — ", n_cv_days, " CV days | ", n_gfw_days, " GFW days | ",
          overall_pct, "% agreement")
}

dev.off()
message("PDF saved: GFW_vs_CV_calendar_", Sys.Date(), ".pdf")


















