# ==============================================================================
# CV Vessels — Fishing Days Distribution
# FAO division × French CFP zone × EEZ × year × vessel
# ==============================================================================
#
# SOURCES:
#   fishing_days_france  — from gfw_s8_effort.RData; contains zone_detail
#                          (French CFP sub-zones + offshore/other)
#   events_classified    — from gfw_s7_zones.RData; contains eez column
#                          retained from the Step 5/6 spatial join
#
# ZONE DETAIL values (zone_detail column in fishing_days_france):
#   "0-3NM"             French territorial sea inner band
#   "3-6NM"             French exclusive band
#   "6-12NM"            French 12NM zone outer band
#   "0-12NM (other)"    Coastal waters outside French zones
#   "Offshore (>12NM)"  Beyond 12NM territorial sea
#
# EEZ values: ISO3 codes — GBR, NLD, BEL, FRA, DEU, DNK, NOR, IRL, ESP
#
# Filtered to CV vessels only (matched via vessels_cv MMSI lookup).
#
# OUTPUTS (saved to flyshootdir):
#   CV_vessel_fishing_days_<date>.RData  — fishing_days_cv_full (long table)
#   CV_vessel_fishing_days_<date>.xlsx   — one sheet per grouping
#   Plots to screen / PDF
# ==============================================================================

library(tidyverse)
library(lubridate)
library(writexl)

# ---- Study settings ----------------------------------------------------------
study_years <- 2013:2025    # controls all downstream filters
study_divs  <- c("27.4.C", "27.7.D", "27.7.E")

# ==============================================================================
# STEP 1 — Load required objects if not already in environment
# ==============================================================================

if (!exists("fishing_days_france")) {
  message("Loading fishing_days_france ...")
  fishing_days_france <- local({
    e <- new.env()
    load(file.path(flyshootdir, "gfw_s8_effort.RData"), envir = e)
    e$fishing_days_france
  })
  message("  ", nrow(fishing_days_france), " rows | ",
          n_distinct(fishing_days_france$ssvid), " vessels")
}

if (!exists("events_classified")) {
  message("Loading events_classified ...")
  events_classified <- local({
    e <- new.env()
    load(file.path(flyshootdir, "gfw_s7_zones.RData"), envir = e)
    e$events_classified
  })
  message("  ", nrow(events_classified), " rows")
}

if (!exists("vessel_meta")) {
  vessel_meta <- local({
    e <- new.env()
    load(file.path(flyshootdir, "gfw_s3_vessel_meta.RData"), envir = e)
    e$vessel_meta
  })
}

# ==============================================================================
# STEP 2 — Build EEZ lookup from events_classified
# ==============================================================================
# events_classified retains the eez column from the Step 5/6 spatial join.
# Aggregate to one EEZ per vesselId × date — take the most frequent EEZ
# on each date (handles rare cases where a vessel crosses EEZ boundaries
# within a single day).

message("Building EEZ lookup from events_classified ...")

eez_by_date <- events_classified %>%
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  filter(!is.na(eez)) %>%
  mutate(date = as.Date(start)) %>%
  group_by(ssvid, date) %>%
  # Most frequent EEZ on that date for that vessel
  summarise(
    eez = names(which.max(table(eez))),
    .groups = "drop"
  )

message("  EEZ lookup: ", nrow(eez_by_date), " vessel-date rows")
message("  EEZ values: ",
        paste(sort(unique(eez_by_date$eez)), collapse = ", "))

# ==============================================================================
# STEP 3 — Filter fishing_days_france to CV vessels
# ==============================================================================

cv_mmsi <- vessels_cv$mmsi

fishing_days_cv_zones <- fishing_days_france %>%
  filter(ssvid %in% cv_mmsi) %>%
  filter(year %in% study_years) %>%  
  left_join(
    vessels_cv %>% dplyr::select(vessel, mmsi),
    by = c("ssvid" = "mmsi")
  ) %>%
  filter(!is.na(vessel))

message("CV vessels in fishing_days_france: ",
        n_distinct(fishing_days_cv_zones$vessel))
message("Vessels: ",
        paste(sort(unique(fishing_days_cv_zones$vessel)), collapse = ", "))

# ==============================================================================
# STEP 4 — Derive EEZ fishing days for CV vessels
# ==============================================================================
# EEZ is not in fishing_days_france so we go back to events_classified,
# aggregate to unique fishing dates per vessel × division × eez × year,
# then attach vessel metadata and short vessel codes.

fishing_days_cv_eez <- events_classified %>%
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  filter(ssvid %in% cv_mmsi,
         division %in% study_divs) %>%
  left_join(
    vessels_cv %>% dplyr::select(vessel, mmsi),
    by = c("ssvid" = "mmsi")
  ) %>%
  filter(!is.na(vessel)) %>%
  mutate(
    date    = as.Date(start),
    year    = year(date),
    quarter = quarter(date)
  ) %>%
  filter(year %in% study_years) %>%  
  
  # One fishing day = one distinct date per vessel × division × eez
  distinct(vessel, ssvid, division, eez, year, quarter, date) %>%
  group_by(vessel, ssvid, division, eez, year, quarter) %>%
  summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
  # Annual totals
  group_by(vessel, ssvid, division, eez, year) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  left_join(
    vessel_meta %>%
      distinct(ssvid, vessel_flag, gear, gt, size_class),
    by = "ssvid"
  )

message("CV vessels EEZ table: ", nrow(fishing_days_cv_eez), " rows | ",
        sum(fishing_days_cv_eez$fishing_days), " total days")
message("EEZ values for CV vessels: ",
        paste(sort(unique(fishing_days_cv_eez$eez)), collapse = ", "))

# ==============================================================================
# STEP 5 — Build full combined table (zone_detail + EEZ)
# ==============================================================================
# Aggregate fishing_days_cv_zones to annual level and join EEZ via date lookup.
# Since zone_detail and EEZ are both spatial attributes, we derive them
# together from events_classified for the full cross-tabulation.

fishing_days_cv_full <- events_classified %>%
  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
  filter(ssvid %in% cv_mmsi,
         division %in% study_divs) %>%
  # Attach French zone from events_classified_fr if available
  {
    if (exists("events_classified_fr")) {
      message("  Joining French zone detail from events_classified_fr ...")
      left_join(.,
                events_classified_fr %>%
                  rename_with(~ "ssvid", any_of("vessel_ssvid")) %>%
                  dplyr::select(ssvid,
                                start_fr = start,
                                zone_detail) %>%
                  distinct(),
                by = c("ssvid", "start" = "start_fr"))
    } else {
      message("  events_classified_fr not found — zone_detail will be NA")
      mutate(., zone_detail = NA_character_)
    }
  } %>%
  left_join(
    vessels_cv %>% dplyr::select(vessel, mmsi),
    by = c("ssvid" = "mmsi")
  ) %>%
  filter(!is.na(vessel)) %>%
  mutate(
    date    = as.Date(start),
    year    = year(date),
    quarter = quarter(date),
    zone_detail = coalesce(zone_detail,
                           if_else(zone == "Coastal (0-12NM)",
                                   "0-12NM (other)",
                                   "Offshore (>12NM)"))
  ) %>%
  filter(year %in% study_years) %>%  
  distinct(vessel, ssvid, division, eez, zone_detail, zone,
           year, quarter, date) %>%
  group_by(vessel, ssvid, division, eez, zone_detail, year) %>%
  summarise(fishing_days = n_distinct(date), .groups = "drop") %>%
  left_join(
    vessel_meta %>% distinct(ssvid, vessel_flag, gear, gt, size_class),
    by = "ssvid"
  )

message("Full CV table (division × zone × EEZ × year × vessel): ",
        nrow(fishing_days_cv_full), " rows | ",
        sum(fishing_days_cv_full$fishing_days), " total days")

# Save
save(fishing_days_cv_full, fishing_days_cv_eez, fishing_days_cv_zones,
     file = file.path(flyshootdir,
                      paste0("CV_vessel_fishing_days_",
                             Sys.Date(), ".RData")))
message("Saved CV_vessel_fishing_days_", Sys.Date(), ".RData")

# ==============================================================================
# STEP 6 — Visual settings
# ==============================================================================

zone_order <- c(
  "0-3NM",
  "3-6NM",
  "6-12NM",
  "0-12NM (other)",
  "Offshore (>12NM)"
)

zone_colours <- c(
  "0-3NM"            = "#D62728",
  "3-6NM"            = "#FF7F0E",
  "6-12NM"           = "#FFBB78",
  "0-12NM (other)"   = "#AEC7E8",
  "Offshore (>12NM)" = "#1F77B4"
)

# EEZ colour palette — one colour per flag state
eez_colours <- c(
  "GBR" = "#59A14F",
  "NLD" = "#F28E2B",
  "BEL" = "#4E79A7",
  "FRA" = "#E15759",
  "DEU" = "#76B7B2",
  "DNK" = "#EDC948",
  "NOR" = "#B07AA1",
  "IRL" = "#FF9DA7",
  "ESP" = "#9C755F"
)

all_cv_vessels <- sort(unique(fishing_days_cv_full$vessel))

# ==============================================================================
# STEP 7 — Plots
# ==============================================================================

# ---- Plot 1: Annual fishing days by zone_detail — faceted by vessel × division

fishing_days_cv_full %>%
  mutate(zone_detail = factor(zone_detail,
                              levels = zone_order[zone_order %in%
                                                    unique(zone_detail)])) %>%
  group_by(vessel, division, year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "CFP zone",
       title = "CV vessels — fishing days by French CFP zone",
       subtitle = "Stacked by zone | faceted by vessel and ICES division") +
  facet_grid(vessel ~ division, scales = "free_y")


# ---- Plot 2: Annual fishing days by EEZ — faceted by vessel × division

fishing_days_cv_full %>%
  group_by(vessel, division, eez, year) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(eez = coalesce(eez, "Unknown")) %>%
  ggplot(aes(x = year, y = fishing_days, fill = eez)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = eez_colours, na.value = "grey70") +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "", y = "Fishing days", fill = "EEZ",
       title = "CV vessels — fishing days by EEZ",
       subtitle = "Stacked by EEZ | faceted by vessel and ICES division") +
  facet_grid(vessel ~ division, scales = "free_y")


# ---- Plot 3: Zone × EEZ combination — faceted by vessel, one panel per division

fishing_days_cv_full %>%
  mutate(
    zone_detail = factor(zone_detail,
                         levels = zone_order[zone_order %in%
                                               unique(zone_detail)]),
    eez_zone = paste0(coalesce(eez, "?"), " — ", zone_detail)
  ) %>%
  group_by(vessel, division, eez_zone, year) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = eez_zone)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = study_years) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text  = element_text(size = 8)) +
  labs(x = "", y = "Fishing days", fill = "EEZ — Zone",
       title = "CV vessels — fishing days by EEZ and CFP zone combined",
       subtitle = "Faceted by vessel and ICES division") +
  facet_grid(vessel ~ division, scales = "free_y")


# ---- Plot 4: Per-vessel detail — one page per vessel in a PDF ----------------

pdf(
  file   = file.path(flyshootdir,
                     paste0("CV_vessel_zone_EEZ_", Sys.Date(), ".pdf")),
  width  = 14,
  height = 10
)

# ---- Overview page 1: total fishing days by CFP zone per year ---------------

p_overview_days <- fishing_days_cv_full %>%
  mutate(zone_detail = factor(zone_detail,
                              levels = zone_order[zone_order %in%
                                                    unique(zone_detail)])) %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.text.x     = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  ) +
  labs(
    x        = "",
    y        = "Fishing days",
    fill     = "CFP zone",
    title    = "CV vessels (all combined) — total fishing days by French CFP zone",
    subtitle = paste0("All ICES divisions combined  |  n vessels = ",
                      n_distinct(fishing_days_cv_full$vessel))
  )

print(p_overview_days)

# ---- Overview page 2: percentage by CFP zone per year -----------------------

p_overview_pct <- plot5_data %>%   # reuse plot5_data from earlier
  ggplot(aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_label_repel(
    data          = plot5_data %>% filter(!is.na(label)),
    aes(y = label_y, label = label, colour = zone_detail),
    fill          = "white",
    size          = 3,
    fontface      = "bold",
    label.padding = unit(0.15, "lines"),
    label.size    = 0.3,
    direction     = "y",
    nudge_x       = 0.4,
    segment.size  = 0.3,
    segment.colour = "grey50",
    show.legend   = FALSE
  ) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_colour_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(
    axis.text.x     = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  ) +
  labs(
    x        = "",
    y        = "% of fishing days",
    fill     = "CFP zone",
    title    = "CV vessels (all combined) — proportion of fishing days by French CFP zone",
    subtitle = paste0("All ICES divisions combined  |  Percentages shown for 0-3NM and 3-6NM bands",
                      "  |  n vessels = ", n_distinct(fishing_days_cv_full$vessel))
  )

print(p_overview_pct)


for (v in all_cv_vessels) {

  message("Plotting vessel: ", v)

  df_v <- fishing_days_cv_full %>%
    filter(vessel == v) %>%
    mutate(zone_detail = factor(zone_detail,
                                levels = zone_order[zone_order %in%
                                                      unique(zone_detail)]))

  if (nrow(df_v) == 0) next

  # 4a: stacked by CFP zone
  p_zone <- df_v %>%
    group_by(division, zone_detail, year) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
    theme_bw() +
    geom_col(width = 0.8) +
    scale_fill_manual(values = zone_colours, na.value = "grey70",
                      drop = FALSE) +
    scale_x_continuous(breaks = study_years) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom") +
    labs(x = "", y = "Fishing days", fill = "CFP zone",
         title = paste0(v, " — fishing days by French CFP zone"),
         subtitle = "Faceted by ICES division") +
    facet_wrap(~ division, nrow = 1)

  # 4b: stacked by EEZ
  p_eez <- df_v %>%
    mutate(eez = coalesce(eez, "Unknown")) %>%
    group_by(division, eez, year) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    ggplot(aes(x = year, y = fishing_days, fill = eez)) +
    theme_bw() +
    geom_col(width = 0.8) +
    scale_fill_manual(values = eez_colours, na.value = "grey70",
                      drop = FALSE) +
    scale_x_continuous(breaks = study_years) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom") +
    labs(x = "", y = "Fishing days", fill = "EEZ",
         title = paste0(v, " — fishing days by EEZ"),
         subtitle = "Faceted by ICES division") +
    facet_wrap(~ division, nrow = 1)

  # 4c: zone × EEZ heatmap — year vs zone_detail, coloured by fishing days
  p_heat <- df_v %>%
    mutate(eez = coalesce(eez, "Unknown")) %>%
    group_by(division, eez, zone_detail, year) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    ggplot(aes(x = year, y = zone_detail, fill = fishing_days)) +
    theme_bw() +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = if_else(fishing_days > 0,
                                  as.character(fishing_days), "")),
              size = 2.5, colour = "white", fontface = "bold") +
    scale_fill_gradient(low = "#D6E4F7", high = "#1F77B4",
                        na.value = "grey95") +
    scale_x_continuous(breaks = study_years) +
    scale_y_discrete(limits = rev(zone_order)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom") +
    labs(x = "", y = "", fill = "Fishing days",
         title = paste0(v, " — fishing days heatmap (zone × year)"),
         subtitle = "Faceted by ICES division × EEZ") +
    facet_grid(eez ~ division)

  print(p_zone)
  print(p_eez)
  print(p_heat)
}

dev.off()
message("PDF saved: CV_vessel_zone_EEZ_", Sys.Date(), ".pdf")

# ---- Plot 5: All CV vessels combined — proportion by CFP zone per year -------
# Percentages labelled for the three inner coastal bands


library(ggrepel)

# Build data with correct cumulative positions
plot5_data <- fishing_days_cv_full %>%
  mutate(zone_detail = factor(zone_detail,
                              levels = zone_order[zone_order %in%
                                                    unique(zone_detail)])) %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total = sum(fishing_days),
    pct   = fishing_days / total * 100
  ) %>%
  # Sort with Offshore at bottom (first in stack), inner zones at top
  arrange(year, desc(zone_detail)) %>%
  mutate(
    cum_top = cumsum(pct),
    cum_bot = lag(cum_top, default = 0),
    label_y = (cum_top + cum_bot) / 2
  ) %>%
  ungroup() %>%
  mutate(
    label = if_else(
      zone_detail %in% c("0-3NM", "3-6NM") & pct >= 0.5,
      paste0(round(pct, 1), "%"),
      NA_character_
    )
  )


ggplot(plot5_data, aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_label_repel(
    data = plot5_data %>% filter(!is.na(label)),
    aes(y = label_y, label = label, colour = zone_detail),
    fill          = "white",
    size          = 3,
    fontface      = "bold",
    label.padding = unit(0.15, "lines"),
    label.size    = 0.3,
    direction     = "y",
    nudge_x       = 0.4,
    segment.size  = 0.3,
    segment.colour = "grey50",
    show.legend   = FALSE
  ) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_colour_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = study_years) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(
    axis.text.x     = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    x        = "",
    y        = "% of fishing days",
    fill     = "CFP zone",
    title    = "CV vessels (all combined) — proportion of fishing days by French CFP zone",
    subtitle = paste0("All ICES divisions combined  |  Percentages shown for 0-3NM and 3-6NM bands",
                      "  |  n vessels = ", n_distinct(fishing_days_cv_full$vessel))
  )

# ---- Table: fishing days and percentages by CFP zone per year ---------------

library(writexl)

zone_table <- fishing_days_cv_full %>%
  mutate(zone_detail = factor(zone_detail,
                              levels = zone_order[zone_order %in%
                                                    unique(zone_detail)])) %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total = sum(fishing_days),
    pct   = round(fishing_days / total * 100, 1)
  ) %>%
  ungroup()

# ---- Wide format: fishing days ----
zone_table_days_wide <- zone_table %>%
  dplyr::select(year, zone_detail, fishing_days) %>%
  pivot_wider(names_from = zone_detail, values_from = fishing_days,
              values_fill = 0) %>%
  # Add total column
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(year)

# ---- Wide format: percentages ----
zone_table_pct_wide <- zone_table %>%
  dplyr::select(year, zone_detail, pct) %>%
  pivot_wider(names_from = zone_detail, values_from = pct,
              values_fill = 0) %>%
  # Recalculate total pct (should always be 100)
  mutate(Total = round(rowSums(across(where(is.numeric))), 1)) %>%
  arrange(year)

# ---- Print to console ----
message("\n===== Fishing days by CFP zone and year =====")
print(zone_table_days_wide, n = 20)

message("\n===== Percentage by CFP zone and year =====")
print(zone_table_pct_wide, n = 20)

# ---- Export to Excel: two sheets ----
writexl::write_xlsx(
  list(
    "Fishing days" = zone_table_days_wide,
    "Percentages"  = zone_table_pct_wide
  ),
  path = file.path(flyshootdir,
                   paste0("CV_CFP_zone_table_", Sys.Date(), ".xlsx"))
)
message("Excel saved: CV_CFP_zone_table_", Sys.Date(), ".xlsx")

# ==============================================================================
# STEP 8 — Excel export
# ==============================================================================

writexl::write_xlsx(
  list(
    "Full (div x zone x EEZ x year)" = fishing_days_cv_full %>%
      arrange(vessel, year, division, eez, zone_detail),

    "By zone (annual)"  = fishing_days_cv_full %>%
      group_by(vessel, division, zone_detail, year) %>%
      summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
      arrange(vessel, year, division, zone_detail),

    "By EEZ (annual)"   = fishing_days_cv_eez %>%
      arrange(vessel, year, division, eez),

    "Zone x EEZ (annual)" = fishing_days_cv_full %>%
      mutate(eez = coalesce(eez, "Unknown")) %>%
      group_by(vessel, division, eez, zone_detail, year) %>%
      summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
      arrange(vessel, year, division, eez, zone_detail)
  ),
  path = file.path(flyshootdir,
                   paste0("CV_vessel_fishing_days_", Sys.Date(), ".xlsx"))
)

message("Excel saved: CV_vessel_fishing_days_", Sys.Date(), ".xlsx")
message("Done.")
