# reprocess_elog_parquets.R
# ============================================================================
# Converts existing elog and elog_trek parquets to standardised LSC/BMS:
#
# Existing values found:
#   elog:       size_category = "legal" | "undersized" | NA
#   elog_trek:  size_category = "legal" | NA
#
# Conversion:
#   "legal"      -> "LSC", weight_kg unchanged,           box_count unchanged
#   "undersized" -> "BMS", weight_kg = weight_undersized_kg, box_count = boxes_undersized
#   NA           -> "LSC"  (assume legal if not specified)
#
# Columns weight_undersized_kg and boxes_undersized are dropped afterwards.
# ============================================================================

library(tidyverse)
library(arrow)
library(glue)
library(here)
library(jsonlite)

config <- read_json("config.json")
source(file.path(here(), "R/harmonized/02_storage_functions.R"))

convert_to_lsc_bms <- function(df, type_name) {
  if (nrow(df) == 0) {
    message(glue("  {type_name}: empty, skipping"))
    return(df)
  }

  message(glue("  {type_name}: {nrow(df)} rows, size_category values: ",
               "{paste(sort(unique(df$size_category)), collapse = ', ')}"))

  result <- df %>%
    dplyr::mutate(
      # For undersized rows: move weight_undersized_kg -> weight_kg
      #                      and boxes_undersized     -> box_count
      weight_kg = dplyr::case_when(
        size_category == "undersized" &
          "weight_undersized_kg" %in% names(.) &
          !is.na(weight_undersized_kg)          ~ weight_undersized_kg,
        TRUE                                    ~ weight_kg
      ),
      box_count = dplyr::case_when(
        size_category == "undersized" &
          "boxes_undersized" %in% names(.) &
          !is.na(boxes_undersized)               ~ as.integer(boxes_undersized),
        TRUE                                     ~ box_count
      ),
      # Recode to LSC/BMS standard
      # NA with presentation=="DIS" and a discard_reason = BMS, otherwise LSC
      size_category = dplyr::case_when(
        size_category == "undersized"                                        ~ "BMS",
        size_category == "legal"                                             ~ "LSC",
        is.na(size_category) &
          "presentation"   %in% names(.) & presentation == "DIS" &
          "discard_reason" %in% names(.) & !is.na(discard_reason)           ~ "BMS",
        is.na(size_category)                                                 ~ "LSC",
        TRUE                                                                 ~ size_category
      )
    ) %>%
    dplyr::select(-dplyr::any_of(c("weight_undersized_kg", "boxes_undersized"))) %>%
    dplyr::arrange(vessel, trip_id, date, species_code, size_category)

  n_lsc <- sum(result$size_category == "LSC", na.rm = TRUE)
  n_bms <- sum(result$size_category == "BMS", na.rm = TRUE)
  message(glue("  {type_name}: {n_lsc} LSC + {n_bms} BMS = {nrow(result)} total rows"))
  result
}

# ── elog ─────────────────────────────────────────────────────────────────────
message("Processing elog...")
elog <- tryCatch(load_flyshoot_data("elog"),
                 error = function(e) { message("  Could not load: ", e$message); tibble() })

if (nrow(elog) > 0) {
  elog_new <- convert_to_lsc_bms(elog, "elog")
  save_flyshoot_data(elog_new, "elog",
    date_range = c(min(elog_new$date, na.rm = TRUE),
                   max(elog_new$date, na.rm = TRUE)))
  message("  Saved.\n")
}

# ── elog_trek ─────────────────────────────────────────────────────────────────
message("Processing elog_trek...")
elog_trek <- tryCatch(load_flyshoot_data("elog_trek"),
                      error = function(e) { message("  Could not load: ", e$message); tibble() })

if (nrow(elog_trek) > 0) {
  elog_trek_new <- convert_to_lsc_bms(elog_trek, "elog_trek")
  save_flyshoot_data(elog_trek_new, "elog_trek",
    date_range = c(min(elog_trek_new$date, na.rm = TRUE),
                   max(elog_trek_new$date, na.rm = TRUE)))
  message("  Saved.\n")
}

message("Done. Both parquets now use LSC/BMS size_category structure.")
