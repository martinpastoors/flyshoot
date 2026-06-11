# =============================================================================
# GFW vs FDI Fishing Effort Comparison
# Channel fisheries: ICES divisions 4.C, 7.D, 7.E
# =============================================================================
#
# WHAT THIS SCRIPT DOES:
#   1. Loads GFW effort data (from gfw_effort_analysis.RData)
#   2. Loads and processes FDI effort data (from FDI Effort by country.csv)
#   3. Aligns gear classifications and country codes between the two sources
#   4. Produces comparison plots at multiple levels of aggregation
#
# KEY ALIGNMENT DECISIONS:
#   • Effort metric: both sources use DAYS AT SEA — direct comparison valid
#   • Gear:          GFW uses broad vessel types; FDI uses DCF gear codes.
#                    A crosswalk maps both to a common set of gear groups.
#                    GFW lumps all trawl types as TRAWLERS — FDI bottom trawl
#                    + beam trawl are therefore combined for comparison.
#   • Countries:     FDI uses full names (FRANCE); GFW uses ISO-3 (FRA).
#                    A crosswalk aligns them.
#   • AIS coverage:  GFW only sees AIS-equipped vessels; FDI is a census.
#                    GFW will systematically undercount small vessels (<12-15m).
#                    This is a real difference, not a data error.
#
# INPUT FILES:
#   gfw_effort_analysis.RData      — saved from GFW extraction script
#                                    contains: fishing_days_study
#   FDI Effort by country.csv      — STECF FDI Table A effort file
#
# OUTPUT FILES:
#   output/comparison_*.png        — comparison plots
#   output/comparison_summary.csv  — combined dataset for further analysis
# =============================================================================

library(tidyverse)
library(janitor)

# -----------------------------------------------------------------------------
# 0.  USER SETTINGS
# -----------------------------------------------------------------------------

GFW_RDATA  <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/gfw_s7_effort.RData"
FDI_DIR    <- "C:/Users/MartinPastoors/Martin Pastoors/MPFF - General/PROJECTS/FLYSHOOT/stecf 2025_Effort-landings-catches-capacity-biological"
STUDY_DIVS <- c("4.C", "7.D", "7.E")
STUDY_YEARS <- 2013:2024
OUT_DIR    <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/output"

# Countries to include in comparison plots (ISO-3)
FOCUS_FLAGS <- c("FRA", "GBR", "NLD", "BEL")

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1.  LOOKUP TABLES
# -----------------------------------------------------------------------------

# Gear crosswalk: maps GFW vessel types AND FDI gear codes to a common
# gear_group used for side-by-side comparison.
#
# IMPORTANT: GFW TRAWLERS covers OTB + OTT + PTB + SSC + SDN — all demersal
# towed gears. In the comparison plots, FDI "Bottom trawl" + "Beam trawl" +
# "Scottish seine" + "Danish seine" are combined into "Trawl (all)" to match.

gear_crosswalk_gfw <- tribble(
  ~gear_gfw,             ~gear_group,
  "OTHER_SEINES",        "Seine/flyshoot",
  "TRAWLERS",            "Trawl (all)",
  "DREDGE_FISHING",      "Dredge",
  "SET_GILLNETS",        "Gillnet/trammel",
  "POTS_AND_TRAPS",      "Pot/trap",
  "SET_LONGLINES",       "Longline",
  "DRIFTING_LONGLINES",  "Longline",
  "TROLLERS",            "Handline/rod",
  "PURSE_SEINES",        "Purse seine",
  "FISHING",             "Other/unknown",
  "SQUID_JIGGER",        "Other/unknown"
)

gear_crosswalk_fdi <- tribble(
  ~gear_category_fdi,    ~gear_group,
  "Bottom trawl",        "Trawl (all)",      # combined with beam/seine for GFW match
  "Beam trawl",          "Trawl (all)",
  "Midwater trawl",      "Trawl (all)",
  "Scottish seine",      "Trawl (all)",      # GFW TRAWLERS includes seines
  "Danish seine",        "Trawl (all)",
  "Dredge",              "Dredge",
  "Gillnet",             "Gillnet/trammel",
  "Pot/trap",            "Pot/trap",
  "Longline",            "Longline",
  "Handline/rod",        "Handline/rod",
  "Purse seine",         "Purse seine",
  "Other/mixed",         "Other/unknown",
  "Unknown",             "Other/unknown"
)

# Country crosswalk: FDI full names -> ISO-3
country_crosswalk <- tribble(
  ~country_fdi,      ~flag,
  "FRANCE",          "FRA",
  "UNITED KINGDOM",  "GBR",
  "NETHERLANDS",     "NLD",
  "BELGIUM",         "BEL",
  "IRELAND",         "IRL",
  "PORTUGAL",        "PRT",
  "DENMARK",         "DNK",
  "GERMANY",         "DEU",
  "SPAIN",           "ESP",
  "SWEDEN",          "SWE",
  "ITALY",           "ITA",
  "CROATIA",         "HRV",
  "POLAND",          "POL",
  "LITHUANIA",       "LTU",
  "LATVIA",          "LVA",
  "ESTONIA",         "EST",
  "FINLAND",         "FIN",
  "GREECE",          "GRC",
  "ROMANIA",         "ROU",
  "BULGARIA",        "BGR",
  "SLOVENIA",        "SVN",
  "MALTA",           "MLT",
  "CYPRUS",          "CYP"
)

# -----------------------------------------------------------------------------
# 2.  HELPERS (reused from FDI script)
# -----------------------------------------------------------------------------

safe_numeric <- function(x) {
  cleaned <- str_trim(x) |>
    str_replace_all(
      regex("^(NK|na|N/A|#N/A|CONFID|confidential|BEL|\\.)$",
            ignore_case = TRUE), NA_character_)
  val <- suppressWarnings(as.numeric(cleaned))
  val[!is.na(val) & val == -1] <- NA_real_
  val
}

normalise_division <- function(x) {
  x <- str_trim(x) |> str_to_upper()
  x <- str_remove(x, "^27\\.")
  x <- str_replace(x, "^(\\d+)([A-Z])$", "\\1.\\2")
  x
}

map_gear_category <- function(gear_code) {
  lookup <- c(
    OTB = "Bottom trawl", OTT = "Bottom trawl", PTB = "Bottom trawl",
    OTM = "Midwater trawl", PTM = "Midwater trawl",
    TBB = "Beam trawl",   TBN = "Beam trawl",
    SSC = "Scottish seine", SDN = "Danish seine",
    GNS = "Gillnet", GND = "Gillnet", GNC = "Gillnet",
    GTR = "Gillnet", GTN = "Gillnet",
    FPN = "Pot/trap", FPO = "Pot/trap", FYK = "Pot/trap",
    LLS = "Longline", LLD = "Longline", LL  = "Longline",
    LHP = "Handline/rod", LHM = "Handline/rod", LTL = "Handline/rod",
    DRB = "Dredge", HMD = "Dredge",
    PS  = "Purse seine", LA  = "Purse seine",
    MIS = "Other/mixed", NK  = "Unknown"
  )
  dplyr::recode(str_to_upper(str_trim(gear_code)),
                !!!lookup, .default = "Other/mixed")
}

# -----------------------------------------------------------------------------
# 3.  LOAD GFW DATA
# -----------------------------------------------------------------------------

message("Loading GFW data from: ", GFW_RDATA)
load(GFW_RDATA)   # loads fishing_days_study

# Confirm expected columns are present
gfw_expected <- c("vessel_flag", "gear", "division", "year", "fishing_days")
gfw_missing  <- setdiff(gfw_expected, names(fishing_days_study))
if (length(gfw_missing) > 0)
  stop("fishing_days_study is missing columns: ",
       paste(gfw_missing, collapse = ", "))

message("GFW rows loaded: ", nrow(fishing_days_study))
message("GFW years:  ", paste(sort(unique(fishing_days_study$year)), collapse = ", "))
message("GFW flags:  ", paste(sort(unique(fishing_days_study$vessel_flag)), collapse = ", "))
message("GFW gears:  ", paste(sort(unique(fishing_days_study$gear)), collapse = ", "))
message("GFW divs:   ", paste(sort(unique(fishing_days_study$division)), collapse = ", "))

# Build GFW comparison dataset
gfw_compare <- fishing_days_study |>
  mutate(division = gsub("27\\.","", division)) %>% 
  filter(year %in% STUDY_YEARS,
         division %in% STUDY_DIVS) |>
  left_join(gear_crosswalk_gfw, by = c("gear" = "gear_gfw")) |>
  mutate(
    flag       = vessel_flag,
    gear_group = coalesce(gear_group, "Other/unknown")
  ) |>
  group_by(year, flag, gear_group, division) |>
  summarise(fishing_days = sum(fishing_days, na.rm = TRUE),
            .groups = "drop") |>
  mutate(source = "GFW")

message("GFW comparison rows: ", nrow(gfw_compare))

# -----------------------------------------------------------------------------
# 4.  LOAD FDI SUMMARIES
# -----------------------------------------------------------------------------
#     Uses pre-computed output from fdi_channel_analysis.R rather than
#     re-reading and re-processing the raw effort CSV.
#     Two summary files are available:
#       FDI_GEAR_CAT_FILE  - by gear_category (broad groups)
#       FDI_GEAR_CODE_FILE - by gear_code (fine level, e.g. OTB, GTR)
#     The gear-category file is used as the primary source for comparison
#     with GFW (which uses broad vessel types). The gear-code file is used
#     for the France fine-detail plot (Plot 3).
# -----------------------------------------------------------------------------

FDI_GEAR_CAT_FILE  <- file.path(OUT_DIR,
                                "fdi_fishingdays_by_country_sizeclass_gear_division_year.csv")
FDI_GEAR_CODE_FILE <- file.path(OUT_DIR,
                                "fdi_fishingdays_by_country_sizeclass_gearcode_division_year.csv")

# If the files are not in OUT_DIR, override the paths here:
# FDI_GEAR_CAT_FILE  <- "path/to/fdi_fishingdays_by_country_sizeclass_gear_division_year.csv"
# FDI_GEAR_CODE_FILE <- "path/to/fdi_fishingdays_by_country_sizeclass_gearcode_division_year.csv"

for (f in c(FDI_GEAR_CAT_FILE, FDI_GEAR_CODE_FILE)) {
  if (!file.exists(f)) stop("FDI summary file not found: ", f)
}

# ---- 4a. Gear-category summary (primary source for GFW comparison) ----------
# Columns: year, country, vessel_lc, gear_category, division,
#          fishing_days_sum, n_quarters_reported, n_quarters_suppressed,
#          days_complete, days_note

fdi_cat <- read_csv(FDI_GEAR_CAT_FILE, show_col_types = FALSE) |>
  clean_names()

message("FDI gear-category summary rows: ", nrow(fdi_cat))

fdi_compare <- fdi_cat |>
  filter(year %in% STUDY_YEARS) |>        # files already filtered to STUDY_DIVS
  left_join(country_crosswalk,
            by = c("country" = "country_fdi")) |>
  left_join(gear_crosswalk_fdi,
            by = c("gear_category" = "gear_category_fdi")) |>
  mutate(gear_group = coalesce(gear_group, "Other/unknown")) |>
  group_by(year, flag, gear_group, division) |>
  summarise(
    fishing_days          = sum(fishing_days_sum,      na.rm = TRUE),
    n_quarters_suppressed = sum(n_quarters_suppressed, na.rm = TRUE),
    days_complete         = all(days_complete),
    .groups = "drop"
  ) |>
  mutate(source = "FDI")

message("FDI comparison rows: ", nrow(fdi_compare))

# ---- 4b. Gear-code summary (for France fine-detail plot) --------------------
fdi_code <- read_csv(FDI_GEAR_CODE_FILE, show_col_types = FALSE) |>
  clean_names()

message("FDI gear-code summary rows: ", nrow(fdi_code))


# -----------------------------------------------------------------------------
# 5.  COMBINE AND WRITE SUMMARY
# -----------------------------------------------------------------------------

comparison <- bind_rows(gfw_compare, fdi_compare) |>
  filter(!is.na(flag), !is.na(gear_group)) |>
  mutate(
    flag       = factor(flag),
    gear_group = factor(gear_group),
    source     = factor(source, levels = c("FDI", "GFW"))
  )

write_csv(comparison,
          file.path(OUT_DIR, "comparison_gfw_fdi_summary.csv"))

message("\nTotal comparison rows: ", nrow(comparison))
message("Flags: ", paste(sort(unique(comparison$flag)), collapse = ", "))
message("Gear groups: ", paste(sort(unique(comparison$gear_group)), collapse = ", "))

# QA: flags in FDI with no GFW match and vice versa
fdi_flags <- unique(fdi_compare$flag)
gfw_flags <- unique(gfw_compare$flag)
message("\nFlags in FDI but not GFW: ",
        paste(setdiff(fdi_flags, gfw_flags), collapse = ", "))
message("Flags in GFW but not FDI: ",
        paste(setdiff(gfw_flags, fdi_flags), collapse = ", "))

# -----------------------------------------------------------------------------
# 6.  PLOTS
# -----------------------------------------------------------------------------

# Shared theme
theme_comp <- function() {
  theme_bw() +
  theme(
    strip.text       = element_text(size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank()
  )
}

source_colours <- c("FDI" = "#2166ac", "GFW" = "#d6604d")
source_shapes  <- c("FDI" = 16,        "GFW" = 17)

# ── PLOT 1: All divisions combined, focus flags, facet by gear group ──────────
p1 <- comparison |>
  filter(flag %in% FOCUS_FLAGS) |>
  group_by(year, flag, gear_group, source) |>
  summarise(fishing_days = sum(fishing_days), .groups = "drop") |>
  ggplot(aes(x = year, y = fishing_days,
             colour = source, shape = source)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = source_colours) +
  scale_shape_manual(values  = source_shapes) +
  scale_x_continuous(breaks  = seq(2013, 2024, by = 2)) +
  facet_grid(flag ~ gear_group, scales = "free_y") +
  labs(title    = "GFW vs FDI — fishing days by country and gear group",
       subtitle = paste("All study divisions combined |",
                        paste(STUDY_DIVS, collapse = " & ")),
       caption  = "GFW: AIS-based, may undercount vessels <12-15m.  FDI: census-based self-reported.",
       x = "Year", y = "Fishing days",
       colour = "Source", shape = "Source") +
  theme_comp()

ggsave(file.path(OUT_DIR, "comparison_p1_by_country_gear.png"),
       p1, width = 18, height = 14)

# ── PLOT 2: Per division, all flags combined, facet by gear group ──────────────
p2 <- comparison |>
  filter(flag %in% FOCUS_FLAGS) |>
  group_by(year, division, gear_group, source) |>
  summarise(fishing_days = sum(fishing_days), .groups = "drop") |>
  ggplot(aes(x = year, y = fishing_days,
             colour = source, shape = source)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = source_colours) +
  scale_shape_manual(values  = source_shapes) +
  scale_x_continuous(breaks  = seq(2013, 2024, by = 2)) +
  facet_grid(division ~ gear_group, scales = "free_y") +
  labs(title    = "GFW vs FDI — fishing days by division and gear group",
       subtitle = paste("Focus flags:", paste(FOCUS_FLAGS, collapse = ", ")),
       caption  = "GFW: AIS-based.  FDI: census-based.",
       x = "Year", y = "Fishing days",
       colour = "Source", shape = "Source") +
  theme_comp()

ggsave(file.path(OUT_DIR, "comparison_p2_by_division_gear.png"),
       p2, width = 18, height = 8)

# ── PLOT 3: France only — fine gear detail, per division ──────────────────────
# Uses fdi_code (gear-code level summary) for FDI fine detail
# GFW side shown as gear_group for context

fdi_france_fine <- fdi_code |>
  filter(country == "FRANCE",
         year %in% STUDY_YEARS) |>
  group_by(year, gear_category, division) |>
  summarise(fishing_days = sum(fishing_days_sum, na.rm = TRUE),
            .groups = "drop") |>
  mutate(source = "FDI", flag = "FRA")

gfw_france <- gfw_compare |>
  filter(flag == "FRA") |>
  rename(gear_category = gear_group)

# Top 5 FDI gear categories for France
top5_fra <- fdi_france_fine |>
  group_by(gear_category) |>
  summarise(total = sum(fishing_days), .groups = "drop") |>
  slice_max(total, n = 5) |>
  pull(gear_category)

p3 <- bind_rows(
  fdi_france_fine |>
    mutate(gear_plot = if_else(gear_category %in% top5_fra,
                               gear_category, "ZZZ Other")),
  gfw_france |>
    mutate(gear_plot = gear_category)
) |>
  group_by(year, division, gear_plot, source) |>
  summarise(fishing_days = sum(fishing_days), .groups = "drop") |>
  ggplot(aes(x = year, y = fishing_days,
             colour = gear_plot, linetype = source)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_linetype_manual(values = c("FDI" = "solid", "GFW" = "dashed")) +
  scale_x_continuous(breaks = seq(2013, 2024, by = 2)) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ division, scales = "free_y") +
  labs(title    = "France — GFW (dashed) vs FDI (solid) fishing days by gear",
       subtitle = "FDI: top 5 gear categories + Other  |  GFW: broad gear groups",
       x = "Year", y = "Fishing days",
       colour = "Gear", linetype = "Source") +
  theme_comp()

ggsave(file.path(OUT_DIR, "comparison_p3_france_gear_detail.png"),
       p3, width = 14, height = 6)

# ── PLOT 4: GFW/FDI ratio over time — where do they diverge most? ─────────────
ratio <- comparison |>
  filter(flag %in% FOCUS_FLAGS) |>
  group_by(year, flag, gear_group, source) |>
  summarise(fishing_days = sum(fishing_days), .groups = "drop") |>
  pivot_wider(names_from  = source,
              values_from = fishing_days) |>
  filter(!is.na(FDI), !is.na(GFW), FDI > 0) |>
  mutate(gfw_fdi_ratio = GFW / FDI)

p4 <- ratio |>
  ggplot(aes(x = year, y = gfw_fdi_ratio, colour = flag)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2013, 2024, by = 2)) +
  scale_colour_brewer(palette = "Set2") +
  facet_wrap(~ gear_group, scales = "free_y") +
  labs(title    = "GFW / FDI ratio of fishing days",
       subtitle = "Ratio = 1 (dashed): perfect agreement. <1: GFW undercounts vs FDI",
       caption  = "Values >1 may indicate FDI suppression or reporting gaps",
       x = "Year", y = "GFW days / FDI days",
       colour = "Flag") +
  theme_comp()

ggsave(file.path(OUT_DIR, "comparison_p4_gfw_fdi_ratio.png"),
       p4, width = 16, height = 10)

message("\nAll plots saved to: ", normalizePath(OUT_DIR))
message("Done.")




# SANDBOX

# =============================================================================
# DIAGNOSTICS: Why do GFW and FDI trawl trends diverge?
# =============================================================================

# ── DIAG 1: AIS adoption effect ───────────────────────────────────────────────
# If AIS adoption drives GFW increase, we expect:
#   - More vessels visible in GFW over time (not just more days per vessel)
#   - The increase concentrated in smaller size classes

# Number of unique vessels per year in GFW trawl
fishing_days_study |>
  mutate(division = gsub("27\\.","", division)) %>% 
  filter(gear == "TRAWLERS",
         division %in% STUDY_DIVS) |>
  filter(vessel_flag %in% FOCUS_FLAGS) |>
  group_by(year, vessel_flag) |>
  summarise(
    n_vessels    = n_distinct(ssvid),
    total_days   = sum(fishing_days),
    days_per_vessel = total_days / n_vessels,
    .groups = "drop"
  ) |>
  # print(n = 50)
  pivot_longer(names_to = "variable", values_to = "data", n_vessels:days_per_vessel) %>% 
  
  ggplot(aes(x = year, y = data, colour = vessel_flag)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2013, 2025, by = 1)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title    = "GFW n_vessels, total_days and days per vessel",
       x = "Year", y = "data",
       colour = "Variable") +
  theme_bw()

# If n_vessels increases but days_per_vessel is stable -> AIS adoption effect
# If n_vessels stable but days_per_vessel increases -> genuine effort increase









# Compare GFW n_vessels with FDI n_vessels for trawl fleet
# to confirm fleet contraction is real and captured by both sources

# GFW side - already have this
gfw_vessels_trawl <- fishing_days_study |>
  mutate(division = gsub("27\\.", "", division)) |>
  filter(gear == "TRAWLERS",
         division %in% STUDY_DIVS,
         vessel_flag %in% FOCUS_FLAGS) |>
  group_by(year, vessel_flag) |>
  summarise(n_vessels_gfw = n_distinct(ssvid), .groups = "drop")

# FDI capacity side - trawl fleet segments
# Load capacity summary if not already in environment
fdi_vessels_trawl <- read_csv(
  file.path(OUT_DIR,
            "fdi_vessels_by_country_sizeclass_gear_year.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  filter(gear_category %in% c("Bottom trawl", "Beam trawl",
                              "Midwater trawl", "Scottish seine",
                              "Danish seine")) |>
  left_join(country_crosswalk, by = c("country" = "country_fdi")) |>
  filter(flag %in% FOCUS_FLAGS) |>
  group_by(year, flag) |>
  summarise(n_vessels_fdi = sum(n_vessels_sum, na.rm = TRUE),
            .groups = "drop")

# Join and plot
left_join(gfw_vessels_trawl,
          fdi_vessels_trawl,
          by = c("year", "vessel_flag" = "flag")) |>
  pivot_longer(cols = c(n_vessels_gfw, n_vessels_fdi),
               names_to  = "source",
               values_to = "n_vessels") |>
  mutate(source = recode(source,
                         "n_vessels_gfw" = "GFW",
                         "n_vessels_fdi" = "FDI")) |>
  ggplot(aes(x = year, y = n_vessels,
             colour = source, shape = source)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_colour_manual(values = source_colours) +
  scale_shape_manual(values  = source_shapes) +
  scale_x_continuous(breaks  = seq(2013, 2024, by = 2)) +
  facet_wrap(~ vessel_flag, scales = "free_y") +
  labs(title    = "Trawl fleet size: GFW vs FDI capacity",
       subtitle = "GFW: AIS-visible vessels; FDI: census (capacity table)",
       caption  = "FDI vessel counts are at national level, not division-specific",
       x = "Year", y = "Number of vessels",
       colour = "Source", shape = "Source") +
  theme_bw() +
  theme(legend.position = "bottom")









# FDI vessels active in study divisions
# Use effort table to identify which fleet segments fished in STUDY_DIVS,
# then look up their vessel counts from capacity

fdi_vessels_active <- fdi_cat |>
  filter(fishing_days_sum > 0) |>
  # Collapse across divisions first — vessel counts are division-independent
  distinct(year, country, vessel_lc, gear_category) |>
  left_join(
    read_csv(file.path(OUT_DIR,
                       "fdi_vessels_by_country_sizeclass_gear_year.csv"),
             show_col_types = FALSE) |>
      clean_names(),
    by = c("year", "country", "vessel_lc", "gear_category")
  ) |>
  left_join(country_crosswalk,
            by = c("country" = "country_fdi")) |>
  filter(flag %in% FOCUS_FLAGS,
         gear_category %in% c("Bottom trawl", "Beam trawl",
                              "Midwater trawl", "Scottish seine",
                              "Danish seine")) |>
  group_by(year, flag) |>
  summarise(
    n_vessels_fdi = sum(n_vessels_sum, na.rm = TRUE),
    .groups = "drop"
  )

fdi_vessels_active %>% 
  ggplot(aes(x = year, y = n_vessels_fdi, colour=flag)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_x_continuous(breaks  = seq(2013, 2024, by = 2)) +
  expand_limits(y=0) +
  labs(title    = "FDI vessels active in study area",
       x = "Year", y = "Number of vessels") +
  facet_wrap(~ flag)
  







# Check what principal_sub_region contains in the raw capacity file
raw_capacity |>
  count(principal_sub_region, sort = TRUE) |>
  print(n = 30)

# And specifically for the study countries
raw_capacity |>
  filter(str_to_upper(str_trim(country)) %in% 
           c("FRANCE", "UNITED KINGDOM", "NETHERLANDS", "BELGIUM")) |>
  count(country, principal_sub_region, sort = TRUE) |>
  print(n = 40)




# Vessel counts from capacity filtered to principal_sub_region in study area
STUDY_DIVS_FULL <- paste0("27.", STUDY_DIVS)   # "4.C" -> "27.4.C"

fdi_vessels_channel <- raw_capacity |>
  mutate(
    country       = str_to_upper(str_trim(country)),
    vessel_lc     = str_trim(vessel_length_category),   # adjust col name if needed
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    n_vessels     = safe_numeric(total_vessels),            # adjust col name if needed
    year          = as.integer(year),
    principal_sub_region = str_trim(principal_sub_region)
  ) |>
  filter(principal_sub_region %in% STUDY_DIVS_FULL) |>
  left_join(country_crosswalk, by = c("country" = "country_fdi")) |>
  filter(flag %in% FOCUS_FLAGS) |>
  mutate(gear_category = map_fishing_technique(fishing_technique)) |>
  filter(gear_category %in% c("Bottom trawl", "Beam trawl",
                              "Midwater trawl", "Scottish seine",
                              "Danish seine")) |>
  group_by(year, flag) |>
  summarise(
    n_vessels_fdi = sum(n_vessels, na.rm = TRUE),
    .groups = "drop"
  )

# Compare with GFW
left_join(
  fishing_days_study |>
    mutate(division = gsub("27\\.", "", division)) |>
    filter(gear      == "TRAWLERS",
           division  %in% STUDY_DIVS,
           vessel_flag %in% FOCUS_FLAGS) |>
    group_by(year, vessel_flag) |>
    summarise(n_vessels_gfw = n_distinct(ssvid), .groups = "drop"),
  fdi_vessels_channel,
  by = c("year", "vessel_flag" = "flag")
) |>
  pivot_longer(cols      = c(n_vessels_gfw, n_vessels_fdi),
               names_to  = "source",
               values_to = "n_vessels") |>
  mutate(source = recode(source,
                         "n_vessels_gfw" = "GFW",
                         "n_vessels_fdi" = "FDI (principal sub-region)")) |>
  ggplot(aes(x = year, y = n_vessels,
             colour = source, shape = source)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("FDI (principal sub-region)" = "#2166ac",
                                 "GFW" = "#d6604d")) +
  scale_x_continuous(breaks = seq(2013, 2024, by = 2)) +
  facet_wrap(~ vessel_flag, scales = "free_y") +
  labs(title    = "Trawl fleet size: GFW vs FDI (principal sub-region filter)",
       subtitle = paste("FDI filtered to vessels whose primary area is",
                        paste(STUDY_DIVS_FULL, collapse = ", ")),
       caption  = paste("FDI principal_sub_region = self-reported primary fishing area.",
                        "GFW: AIS-visible vessels only."),
       x = "Year", y = "Number of vessels",
       colour = "Source", shape = "Source") +
  expand_limits(y=0) +
  theme_bw() +
  theme(legend.position = "bottom")




# Vessel counts by country AND size class
fdi_vessels_channel_lc <- raw_capacity |>
  mutate(
    country           = str_to_upper(str_trim(country)),
    vessel_lc         = str_trim(vessel_length_category),
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    n_vessels         = safe_numeric(total_vessels),
    year              = as.integer(year),
    principal_sub_region = str_trim(principal_sub_region)
  ) |>
  filter(principal_sub_region %in% STUDY_DIVS_FULL) |>
  left_join(country_crosswalk, by = c("country" = "country_fdi")) |>
  filter(flag %in% FOCUS_FLAGS) |>
  mutate(gear_category = map_fishing_technique(fishing_technique)) |>
  filter(gear_category %in% c("Bottom trawl", "Beam trawl",
                              "Midwater trawl", "Scottish seine",
                              "Danish seine")) |>
  group_by(year, flag, vessel_lc) |>
  summarise(n_vessels_fdi = sum(n_vessels, na.rm = TRUE),
            .groups = "drop")

gfw_vessels_channel_lc <- fishing_days_study |>
  mutate(division = gsub("27\\.", "", division)) |>
  filter(gear        == "TRAWLERS",
         division    %in% STUDY_DIVS,
         vessel_flag %in% FOCUS_FLAGS) |>
  group_by(year, vessel_flag, size_class) |>
  summarise(n_vessels_gfw = n_distinct(ssvid), .groups = "drop") |>
  rename(flag = vessel_flag, vessel_lc = size_class)

# Harmonise size class labels between FDI (VL1218) and GFW (CLASS3 etc.)
# Check what GFW size classes look like first
gfw_vessels_channel_lc |> count(vessel_lc)
fdi_vessels_channel_lc |> count(vessel_lc)


# Crosswalk: FDI length class -> GFW GT size class
# Based on typical GT/length relationships for NE Atlantic trawlers
# VL0010 (<10m)   -> S1 <100 GT      (small inshore, never AIS-equipped)
# VL1012 (10-12m) -> S1 <100 GT      (still mostly <100 GT)
# VL1218 (12-18m) -> S1/S2 boundary  (straddles 100 GT — split here as S1)
# VL1824 (18-24m) -> S2 100-300 GT   (typical beam/otter trawlers)
# VL2440 (24-40m) -> S3 300-600 GT   (larger Channel trawlers/seiners)
# VL40XX (>40m)   -> S4/S5           (large pelagic/factory — rare in Channel)

lc_crosswalk <- tribble(
  ~vessel_lc_fdi, ~vessel_lc_gfw,
  "VL0010",       "S1 <100 GT",
  "VL1012",       "S1 <100 GT",
  "VL1218",       "S1 <100 GT",
  "VL1824",       "S2 100-300 GT",
  "VL2440",       "S3 300-600 GT",
  "VL40XX",       "S4 600-1200 GT"
)

# Apply crosswalk to FDI
fdi_lc_harmonised <- fdi_vessels_channel_lc |>
  left_join(lc_crosswalk, by = c("vessel_lc" = "vessel_lc_fdi")) |>
  filter(!is.na(vessel_lc_gfw)) |>
  group_by(year, flag, vessel_lc = vessel_lc_gfw) |>
  summarise(n_vessels = sum(n_vessels_fdi, na.rm = TRUE),
            .groups = "drop") |>
  mutate(source = "FDI")

# GFW side
gfw_lc_harmonised <- gfw_vessels_channel_lc |>
  mutate(vessel_lc = as.character(vessel_lc)) |>
  rename(n_vessels = n_vessels_gfw) |>
  mutate(source = "GFW")

# Combine and plot
bind_rows(fdi_lc_harmonised, gfw_lc_harmonised) |>
  filter(flag %in% c("FRA", "GBR", "NLD", "BEL")) |>
  mutate(
    vessel_lc = factor(vessel_lc,
                       levels = c("S1 <100 GT", "S2 100-300 GT",
                                  "S3 300-600 GT", "S4 600-1200 GT",
                                  "S5 >1200 GT")),
    source = factor(source, levels = c("FDI", "GFW"))
  ) |>
  ggplot(aes(x = year, y = n_vessels,
             colour = source, shape = source)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = source_colours) +
  scale_shape_manual(values  = source_shapes) +
  scale_x_continuous(breaks  = seq(2013, 2024, by = 2)) +
  facet_grid(flag ~ vessel_lc, scales = "free_y") +
  labs(
    title    = "Trawl fleet size by country and size class: GFW vs FDI",
    subtitle = "FDI filtered to principal_sub_region in study area",
    caption  = paste("GT/length crosswalk is approximate.",
                     "S1 (<100 GT) vessels rarely carry AIS — GFW undercounts expected.",
                     "FDI: census. GFW: AIS-visible vessels only."),
    x = "Year", y = "Number of vessels",
    colour = "Source", shape = "Source"
  ) +
  theme_bw() +
  theme(
    strip.text       = element_text(size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    legend.position  = "bottom"
  )





# What is the actual GT distribution per size class in GFW?
fishing_days_study |>
  filter(gear == "TRAWLERS",
         vessel_flag %in% FOCUS_FLAGS) |>
  distinct(ssvid, vessel_flag, size_class, gt) |>
  filter(!is.na(gt)) |>
  group_by(vessel_flag, size_class) |>
  summarise(
    n          = n(),
    gt_min     = min(gt),
    gt_median  = median(gt),
    gt_max     = max(gt),
    .groups    = "drop"
  ) |>
  arrange(vessel_flag, size_class) |>
  print(n = 30)

# And what is the length distribution per FDI size class in GFW vessel registry?
# (if you have lengthM in vessel_meta)
fishing_days_study |>
  filter(gear == "TRAWLERS",
         vessel_flag %in% FOCUS_FLAGS) |>
  distinct(ssvid, vessel_flag, size_class, gt) |>
  filter(!is.na(gt)) |>
  ggplot(aes(x = gt, fill = size_class)) +
  geom_histogram(bins = 40, alpha = 0.7) +
  facet_grid(vessel_flag ~ size_class, scales = "free") +
  scale_x_log10() +
  labs(title = "GT distribution per GFW size class and flag",
       x = "GT (log scale)", y = "N vessels") +
  theme_bw() +
  theme(legend.position = "none")








# How many FDI capacity vessels are genuinely inactive (0 fishing days)?
raw_capacity |>
  mutate(
    country              = str_to_upper(str_trim(country)),
    n_vessels            = safe_numeric(total_vessels),
    total_trips          = safe_numeric(total_trips),
    principal_sub_region = str_trim(principal_sub_region)
  ) |>
  filter(principal_sub_region %in% STUDY_DIVS_FULL,
         country %in% c("FRANCE", "UNITED KINGDOM",
                        "NETHERLANDS", "BELGIUM")) |>
  mutate(
    fishing_technique = str_to_upper(str_trim(fishing_technique)),
    # NA trips = suppressed (confidential) — treat as unknown, not inactive
    status = case_when(
      fishing_technique == "INACTIVE" ~ "inactive",
      is.na(total_trips)              ~ "unknown (suppressed)",
      total_trips == 0                ~ "inactive",
      total_trips > 0                 ~ "active"
    )
  ) |>
  group_by(country, fishing_technique, status) |>
  summarise(n_vessels = sum(n_vessels, na.rm = TRUE),
            .groups = "drop") |>
  group_by(country, fishing_technique) |>
  mutate(
    total     = sum(n_vessels),
    pct       = round(100 * n_vessels / total, 1)
  ) |>
  ungroup() |>
  filter(country %in% c("FRANCE", "UNITED KINGDOM",
                        "NETHERLANDS", "BELGIUM"),
         fishing_technique %in% c("DTS", "TBB", "DFN",
                                  "DRB", "INACTIVE")) |>
  arrange(country, fishing_technique, status) |>
  print(n = 50)
