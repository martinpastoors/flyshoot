# ==============================================================================
# Full fleet vs CV fleet — Fishing days by year, country and CFP zone
# ==============================================================================
#
# SOURCES:
#   fishing_days_study   — full fleet with zone_detail (French CFP zones),
#                          from Extract_GFW_effort_v8.R (gfw_s7_effort.RData)
#   fishing_days_cv_full — CV vessels only with zone_detail (from distribution
#                          script CV_vessel_fishing_days_distribution_v1.R)
#   vessels_cv           — CV vessel lookup (mmsi → short vessel code)
#
# OUTPUTS:
#   Plots to screen and PDF: Fleet_vs_CV_CFP_zones_<date>.pdf
#
# NOTE: year_start / year_end control the time window throughout
# ==============================================================================

library(tidyverse)
library(lubridate)
library(ggrepel)

year_start <- 2013
year_end   <- 2025

# ---- Helper: load a single named object from an RData file ----
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

# ==============================================================================
# STEP 1 — Load objects if not in environment
# ==============================================================================

if (!exists("fishing_days_study")) {
  message("Loading fishing_days_study ...")
  fishing_days_study <- load_object(
    file.path(flyshootdir, "gfw_s7_effort.RData"), "fishing_days_study")
}

if (!exists("fishing_days_cv_full")) {
  message("Loading fishing_days_cv_full ...")
  cv_files <- list.files(flyshootdir,
                         pattern = "CV_vessel_fishing_days_.*\\.RData",
                         full.names = TRUE)
  if (length(cv_files) == 0)
    stop("fishing_days_cv_full not found. Run CV_vessel_fishing_days_distribution_v1.R first.")
  load(cv_files[length(cv_files)])
  message("  Loaded: ", cv_files[length(cv_files)])
}

# ==============================================================================
# STEP 2 — Prepare full fleet from fishing_days_study
# ==============================================================================

cv_mmsi <- vessels_cv$mmsi

fishing_days_fleet <- fishing_days_study %>%
  filter(year >= year_start, year <= year_end) %>%
  group_by(ssvid, vessel_flag, gear, gt, size_class,
           division, zone_detail, year) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(
    fleet       = if_else(ssvid %in% cv_mmsi, "CV fleet", "Non-CV fleet"),
    vessel_flag = coalesce(vessel_flag, "Unknown")
  )

fishing_days_noncv <- fishing_days_fleet %>%
  filter(fleet == "Non-CV fleet")

message("Non-CV fleet: ",
        n_distinct(fishing_days_noncv$ssvid), " vessels | ",
        sum(fishing_days_noncv$fishing_days), " total days")
message("Flags: ",
        paste(sort(unique(fishing_days_noncv$vessel_flag)), collapse = ", "))

# ==============================================================================
# STEP 3 — Visual settings
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

flag_colours <- c(
  "BEL"     = "#4E79A7",
  "NLD"     = "#F28E2B",
  "GBR"     = "#59A14F",
  "FRA"     = "#E15759",
  "DEU"     = "#76B7B2",
  "DNK"     = "#EDC948",
  "NOR"     = "#B07AA1",
  "IRL"     = "#FF9DA7",
  "Other"   = "#BAB0AC",
  "Unknown" = "#D3D3D3"
)

threshold_pct <- 1

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
                any_of(c("fishing_days", "total_flag_days", "total_days"))) %>%
    dplyr::select(-total_flag_days, -total_days, -flag_pct)
}

# Build stacked pct data with correct label positions for geom_label_repel
make_pct_data <- function(df, group_vars, fill_var,
                          label_zones = c("0-3NM", "3-6NM"),
                          min_pct_label = 0.5) {
  df %>%
    group_by(across(all_of(c(group_vars, fill_var)))) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      total = sum(fishing_days),
      pct   = fishing_days / total * 100
    ) %>%
    arrange(across(all_of(group_vars)), desc(.data[[fill_var]])) %>%
    mutate(
      cum_top = cumsum(pct),
      cum_bot = lag(cum_top, default = 0),
      label_y = (cum_top + cum_bot) / 2
    ) %>%
    ungroup() %>%
    mutate(
      label = if_else(
        .data[[fill_var]] %in% label_zones & pct >= min_pct_label,
        paste0(round(pct, 1), "%"),
        NA_character_
      )
    )
}

# ==============================================================================
# STEP 4 — Plots
# ==============================================================================

pdf(
  file   = file.path(flyshootdir,
                     paste0("Fleet_vs_CV_CFP_zones_", Sys.Date(), ".pdf")),
  width  = 14,
  height = 10
)

# ============================================================
# SECTION A — Non-CV fleet by year, country and CFP zone
# ============================================================

# ---- A1: Total days — stacked by flag, faceted by zone ----
fishing_days_noncv %>%
  group_by(year, vessel_flag, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  ggplot(aes(x = year, y = fishing_days, fill = vessel_flag)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = flag_colours, na.value = "grey70") +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "Flag",
       title    = "Non-CV fleet — total fishing days by year, country and CFP zone",
       subtitle = "Stacked by country | faceted by CFP zone") +
  facet_wrap(~ zone_detail, scales = "free_y", nrow = 2) ->
  p; print(p)

# ---- A2: Total days — stacked by zone, faceted by flag ----
fishing_days_noncv %>%
  group_by(year, vessel_flag, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  filter(vessel_flag != "Other") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "CFP zone",
       title    = "Non-CV fleet — total fishing days by year and CFP zone",
       subtitle = "Stacked by CFP zone | faceted by country") +
  facet_wrap(~ vessel_flag, scales = "free_y") ->
  p; print(p)

# ---- A3: Percentage — stacked by zone, faceted by flag ----
fishing_days_noncv %>%
  group_by(year, vessel_flag, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  apply_flag_other() %>%
  group_by(year, vessel_flag, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  filter(vessel_flag != "Other") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  make_pct_data(group_vars = c("year", "vessel_flag"),
                fill_var   = "zone_detail") %>%
  ggplot(aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_label_repel(
    data = ~ filter(., !is.na(label)),
    aes(y = label_y, label = label, colour = zone_detail),
    fill = "white", size = 2.5, fontface = "bold",
    label.padding = unit(0.12, "lines"), label.size = 0.25,
    direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.colour = "grey50",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_colour_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "% of fishing days", fill = "CFP zone",
       title    = "Non-CV fleet — % fishing days by year and CFP zone",
       subtitle = "Percentages shown for 0-3NM and 3-6NM | faceted by country") +
  facet_wrap(~ vessel_flag, scales = "free_y") ->
  p; print(p)

# ---- A4: All non-CV combined — total ----
fishing_days_noncv %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "CFP zone",
       title    = "Non-CV fleet (all combined) — total fishing days by CFP zone",
       subtitle = paste0("All countries and ICES divisions combined  |  n vessels = ",
                         n_distinct(fishing_days_noncv$ssvid))) ->
  p; print(p)

# ---- A5: All non-CV combined — percentage ----
fishing_days_noncv %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  make_pct_data(group_vars = "year", fill_var = "zone_detail") %>%
  ggplot(aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_label_repel(
    data = ~ filter(., !is.na(label)),
    aes(y = label_y, label = label, colour = zone_detail),
    fill = "white", size = 3, fontface = "bold",
    label.padding = unit(0.15, "lines"), label.size = 0.3,
    direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.colour = "grey50",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_colour_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "% of fishing days", fill = "CFP zone",
       title    = "Non-CV fleet (all combined) — % fishing days by CFP zone",
       subtitle = paste0("All countries and ICES divisions combined  |  n vessels = ",
                         n_distinct(fishing_days_noncv$ssvid))) ->
  p; print(p)


# ============================================================
# SECTION B — CV fleet by year and CFP zone
# ============================================================

# ---- B1: Total — stacked by zone ----
fishing_days_cv_full %>%
  filter(year >= year_start, year <= year_end) %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "CFP zone",
       title    = "CV fleet (all combined) — total fishing days by CFP zone",
       subtitle = paste0("All ICES divisions combined  |  n vessels = ",
                         n_distinct(fishing_days_cv_full$vessel))) ->
  p; print(p)

# ---- B2: Percentage — stacked by zone ----
fishing_days_cv_full %>%
  filter(year >= year_start, year <= year_end) %>%
  group_by(year, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  make_pct_data(group_vars = "year", fill_var = "zone_detail") %>%
  ggplot(aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_label_repel(
    data = ~ filter(., !is.na(label)),
    aes(y = label_y, label = label, colour = zone_detail),
    fill = "white", size = 3, fontface = "bold",
    label.padding = unit(0.15, "lines"), label.size = 0.3,
    direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.colour = "grey50",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_colour_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "% of fishing days", fill = "CFP zone",
       title    = "CV fleet (all combined) — % fishing days by CFP zone",
       subtitle = paste0("All ICES divisions combined  |  Percentages shown for 0-3NM and 3-6NM",
                         "  |  n vessels = ", n_distinct(fishing_days_cv_full$vessel))) ->
  p; print(p)

# ---- B3: Total — faceted by vessel ----
fishing_days_cv_full %>%
  filter(year >= year_start, year <= year_end) %>%
  group_by(year, vessel, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "CFP zone",
       title    = "CV fleet — total fishing days by year, vessel and CFP zone",
       subtitle = "Faceted by vessel") +
  facet_wrap(~ vessel) ->
  p; print(p)

# ---- B4: Percentage — faceted by vessel ----
fishing_days_cv_full %>%
  filter(year >= year_start, year <= year_end) %>%
  group_by(year, vessel, zone_detail) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order)) %>%
  make_pct_data(group_vars = c("year", "vessel"),
                fill_var   = "zone_detail") %>%
  ggplot(aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "% of fishing days", fill = "CFP zone",
       title    = "CV fleet — % fishing days by year, vessel and CFP zone",
       subtitle = "Faceted by vessel") +
  facet_wrap(~ vessel) ->
  p; print(p)


# ============================================================
# SECTION C — Side-by-side: non-CV vs CV fleet
# ============================================================

fleet_combined <- bind_rows(
  fishing_days_noncv %>%
    filter(year >= year_start, year <= year_end) %>%
    group_by(year, zone_detail) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    mutate(fleet = paste0("Non-CV fleet\n(n=",
                          n_distinct(fishing_days_noncv$ssvid), " vessels)")),

  fishing_days_cv_full %>%
    filter(year >= year_start, year <= year_end) %>%
    group_by(year, zone_detail) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    mutate(fleet = paste0("CV fleet\n(n=",
                          n_distinct(fishing_days_cv_full$vessel), " vessels)"))
) %>%
  mutate(zone_detail = factor(zone_detail, levels = zone_order))

# ---- C1: Total — faceted by fleet ----
fleet_combined %>%
  ggplot(aes(x = year, y = fishing_days, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 11)) +
  labs(x = "", y = "Fishing days", fill = "CFP zone",
       title    = "Non-CV fleet vs CV fleet — total fishing days by CFP zone",
       subtitle = "All ICES divisions combined") +
  facet_wrap(~ fleet, scales = "free_y") ->
  p; print(p)

# ---- C2: Percentage — faceted by fleet ----
fleet_combined %>%
  make_pct_data(group_vars = c("year", "fleet"),
                fill_var   = "zone_detail") %>%
  ggplot(aes(x = year, y = pct, fill = zone_detail)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_label_repel(
    data = ~ filter(., !is.na(label)),
    aes(y = label_y, label = label, colour = zone_detail),
    fill = "white", size = 2.5, fontface = "bold",
    label.padding = unit(0.12, "lines"), label.size = 0.25,
    direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.colour = "grey50",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_colour_manual(values = zone_colours, na.value = "grey70", drop = FALSE) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 11)) +
  labs(x = "", y = "% of fishing days", fill = "CFP zone",
       title    = "Non-CV fleet vs CV fleet — % fishing days by CFP zone",
       subtitle = "Percentages shown for 0-3NM and 3-6NM  |  All ICES divisions combined") +
  facet_wrap(~ fleet) ->
  p; print(p)


# ============================================================
# SECTION D — CV contribution to total fleet effort by zone
# ============================================================

# Build combined dataset with CV/non-CV split, all zones
fleet_contribution <- bind_rows(
  fishing_days_noncv %>%
    filter(year >= year_start, year <= year_end) %>%
    group_by(year, zone_detail) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    mutate(fleet = "Non-CV fleet"),
  
  fishing_days_cv_full %>%
    filter(year >= year_start, year <= year_end) %>%
    group_by(year, zone_detail) %>%
    summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
    mutate(fleet = "CV fleet")
) %>%
  mutate(
    zone_detail = factor(zone_detail, levels = zone_order),
    fleet       = factor(fleet, levels = c("Non-CV fleet", "CV fleet"))
  )

fleet_colours <- c("Non-CV fleet" = "#B0C4DE", "CV fleet" = "#E15759")

# ---- D1: Total fishing days — CV stacked on top of non-CV, faceted by zone ----
# Shows absolute contribution; CV sits visibly on top of non-CV baseline

fleet_contribution %>%
  group_by(year, zone_detail, fleet) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = fleet)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  scale_fill_manual(values = fleet_colours) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "",
       title    = "Total fishing effort — CV fleet contribution by CFP zone",
       subtitle = "CV fleet stacked on non-CV fleet | faceted by CFP zone") +
  facet_wrap(~ zone_detail, scales = "free_y", nrow = 2) ->
  p; print(p)


# ---- D2: CV share of total effort (%) per zone per year ----
# The key contribution plot — how much of total effort is CV?

fleet_contribution %>%
  group_by(year, zone_detail) %>%
  mutate(total = sum(fishing_days),
         pct   = fishing_days / total * 100) %>%
  filter(fleet == "CV fleet") %>%
  ggplot(aes(x = year, y = pct, colour = zone_detail, group = zone_detail)) +
  theme_bw() +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.8, size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = zone_colours) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0.05, 0.15))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "CV fleet share of total effort (%)",
       colour = "CFP zone",
       title    = "CV fleet share of total fishing effort by CFP zone",
       subtitle = "% of total fleet fishing days attributed to CV vessels") ->
  p; print(p)


# ---- D3: Same as D2 but faceted by zone for readability ----

fleet_contribution %>%
  group_by(year, zone_detail) %>%
  mutate(total = sum(fishing_days),
         pct   = fishing_days / total * 100) %>%
  filter(fleet == "CV fleet") %>%
  ggplot(aes(x = year, y = pct)) +
  theme_bw() +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_col(aes(fill = zone_detail), width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.4, size = 2.5) +
  scale_fill_manual(values = zone_colours, guide = "none") +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "CV fleet share (%)",
       title    = "CV fleet share of total fishing effort — by CFP zone",
       subtitle = "% of total fleet fishing days attributed to CV vessels") +
  facet_wrap(~ zone_detail, nrow = 2) ->
  p; print(p)


# ---- D4: Dodged bars — CV vs non-CV absolute days, faceted by zone ----
# Direct visual comparison of magnitudes

fleet_contribution %>%
  group_by(year, zone_detail, fleet) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  ggplot(aes(x = year, y = fishing_days, fill = fleet)) +
  theme_bw() +
  geom_col(width = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = fleet_colours) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "",
       title    = "CV fleet vs non-CV fleet — fishing days by CFP zone",
       subtitle = "Side-by-side comparison | faceted by CFP zone") +
  facet_wrap(~ zone_detail, scales = "free_y", nrow = 2) ->
  p; print(p)


# ---- D5: All zones combined — CV share of total effort ----
# Single summary line across all zones

fleet_contribution %>%
  group_by(year, fleet) %>%
  summarise(fishing_days = sum(fishing_days), .groups = "drop") %>%
  group_by(year) %>%
  mutate(total = sum(fishing_days),
         pct   = fishing_days / total * 100) %>%
  ggplot(aes(x = year, y = fishing_days, fill = fleet)) +
  theme_bw() +
  geom_col(width = 0.8, position = "stack") +
  geom_text(
    data = ~ filter(., fleet == "CV fleet"),
    aes(y = total, label = paste0(round(pct, 1), "%")),
    vjust = -0.4, size = 3, fontface = "bold", colour = "#E15759"
  ) +
  scale_fill_manual(values = fleet_colours) +
  scale_x_continuous(breaks = year_start:year_end) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "Fishing days", fill = "",
       title    = "Total fleet fishing effort — CV contribution (all zones combined)",
       subtitle = "CV % of total effort labelled above each bar") ->
  p; print(p)

dev.off()
message("PDF saved: Fleet_vs_CV_CFP_zones_", Sys.Date(), ".pdf")
