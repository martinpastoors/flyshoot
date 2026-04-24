# ==============================================================================
# FIX: weighing_time in elog_trek
# ==============================================================================
#
# PROBLEM
# -------
# elog_trek.weighing_time is empty (NA) for all records because:
#
#   1. Treklijst-based trips (treklijst_full / treklijst_simplified / kisten_pefa):
#      trip_elog_trek is never set.  The individual box measurements from kisten
#      (which DO carry weighing_time) are saved only to the kisten parquet and
#      never promoted to elog_trek.
#
#   2. PEFA-trek based trips:
#      read_pefa_trek() builds elog_trek from aggregated per-haul-species rows;
#      there is no per-box timestamp, so weighing_time is structurally absent.
#      The column is simply never added.
#
# PERVASIVENESS
# -------------
# * All treklijst / kisten_pefa trips  → no kisten rows in elog_trek at all
# * All pefa_trek trips                → elog_trek rows exist but weighing_time = NA
#
# FIX
# ---
# Part A (03_main_workflow.R)
#   After get_catch_from_kisten() populates trip_catches for treklijst /
#   kisten_pefa source types, also route trip_catches through a small helper
#   (kisten_to_elog_trek) that joins haul positions and produces a properly
#   shaped elog_trek tibble, then assigns it to trip_elog_trek.
#
# Part B (01_flyshoot_functions.R — read_pefa_trek)
#   After the catch_data tibble is assembled, impute weighing_time from
#   haul_time (best proxy available) so the column is populated.
#   Rows where haul_time itself is NA get shoot_time as a fallback.
#
# ==============================================================================


# ==============================================================================
# PART A — new helper function (add to 01_flyshoot_functions.R)
# ==============================================================================

#' Promote kisten box records to elog_trek shape, adding haul positions.
#'
#' kisten already has weighing_time from get_catch_from_kisten().
#' elog_trek expects the same columns plus shoot_lat / shoot_lon (from haul).
#' This function joins haul positions and selects the canonical elog_trek cols.
#'
#' @param kisten   tibble returned by get_catch_from_kisten()
#' @param haul     tibble returned by get_haul_from_treklijst() (or NULL)
#' @return tibble shaped like elog_trek, or NULL if kisten is NULL / empty
kisten_to_elog_trek <- function(kisten, haul = NULL) {

  if (is.null(kisten) || nrow(kisten) == 0) return(NULL)

  # Join shoot position from haul if available
  if (!is.null(haul) && nrow(haul) > 0 &&
      all(c("haul_id", "shoot_lat", "shoot_lon") %in% names(haul))) {

    pos <- haul %>%
      dplyr::select(vessel, trip_id, haul_id,
                    shoot_lat, shoot_lon,
                    dplyr::any_of(c("haul_time", "shoot_time",
                                    "ices_rect", "economic_zone", "fao_division")))

    elog_trek <- kisten %>%
      dplyr::left_join(pos, by = c("vessel", "trip_id", "haul_id"))

  } else {
    elog_trek <- kisten %>%
      dplyr::mutate(
        shoot_lat = NA_real_,
        shoot_lon = NA_real_
      )
  }

  # Select canonical elog_trek columns; keep any extras that happen to be present
  lead_cols <- c("vessel", "trip_id", "haul_id", "weighing_time", "date",
                 "shoot_lat", "shoot_lon",
                 "species_code", "species_name_nl", "species_name_en",
                 "presentation", "size_class", "weight_kg", "box_count",
                 "lot_nr")

  elog_trek %>%
    dplyr::select(dplyr::any_of(lead_cols), dplyr::everything()) %>%
    dplyr::arrange(vessel, trip_id, haul_id, weighing_time)
}


# ==============================================================================
# PART A — changes to 03_main_workflow.R
# ==============================================================================
#
# In the treklijst_full branch, AFTER the get_catch_from_kisten() call,
# add one line to also populate trip_elog_trek:
#
# FIND (around line 239):
# ─────────────────────────────────────────────────────────────────────────────
#     if ("kisten" %in% sources) {
#       kisten_file  <- trip_files %>% filter(source == "kisten") %>% pull(file)
#       quietly(
#         trip_catches <- get_catch_from_kisten(kisten_file, local_tz = trip_tz,
#                                               treklijst_path = treklijst_file)
#       )
#     }
# ─────────────────────────────────────────────────────────────────────────────
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────
#     if ("kisten" %in% sources) {
#       kisten_file  <- trip_files %>% filter(source == "kisten") %>% pull(file)
#       quietly(
#         trip_catches <- get_catch_from_kisten(kisten_file, local_tz = trip_tz,
#                                               treklijst_path = treklijst_file)
#       )
#       trip_elog_trek <- kisten_to_elog_trek(trip_catches, haul = trip_hauls)
#     }
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# Do the same in the treklijst_simplified branch.
#
# FIND (around line 272):
# ─────────────────────────────────────────────────────────────────────────────
#     quietly({
#       trip_catches <- get_catch_from_kisten(kisten_file, local_tz = local_tz,
#                                             treklijst_path = treklijst_file)
#       trip_info    <- get_trip_info(treklijst_file)
#     })
# ─────────────────────────────────────────────────────────────────────────────
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────
#     quietly({
#       trip_catches <- get_catch_from_kisten(kisten_file, local_tz = local_tz,
#                                             treklijst_path = treklijst_file)
#       trip_info    <- get_trip_info(treklijst_file)
#     })
#     trip_elog_trek <- kisten_to_elog_trek(trip_catches, haul = trip_hauls)
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# Do the same in the kisten_pefa branch.
#
# FIND (around line 285):
# ─────────────────────────────────────────────────────────────────────────────
#       trip_catches <- get_catch_from_kisten(kisten_file, treklijst_path = trek_file)
# ─────────────────────────────────────────────────────────────────────────────
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────
#       trip_catches   <- get_catch_from_kisten(kisten_file, treklijst_path = trek_file)
#       trip_elog_trek <- kisten_to_elog_trek(trip_catches, haul = trip_hauls)
# ─────────────────────────────────────────────────────────────────────────────


# ==============================================================================
# PART B — changes to 01_flyshoot_functions.R (read_pefa_trek)
# ==============================================================================
#
# After catch_data is assembled (around line 1143), add weighing_time imputed
# from haul_time (best available proxy for pefa_trek).
#
# FIND (around line 1143):
# ─────────────────────────────────────────────────────────────────────────────
#   catch_data <- dplyr::bind_rows(legal_rows, bms_rows) %>%
#     dplyr::arrange(date, haul_id, species_code, size_category)
# ─────────────────────────────────────────────────────────────────────────────
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────
#   catch_data <- dplyr::bind_rows(legal_rows, bms_rows) %>%
#     dplyr::arrange(date, haul_id, species_code, size_category)
#
#   # Impute weighing_time from haul_time where available (best proxy for
#   # PEFA-trek data which has no per-box timestamps). Fall back to shoot_time.
#   # This ensures weighing_time is never entirely NA in elog_trek.
#   if (!"weighing_time" %in% names(catch_data)) {
#     haul_times <- haul_data %>%
#       dplyr::select(vessel, trip_id, haul_id,
#                     dplyr::any_of(c("haul_time", "shoot_time")))
#
#     catch_data <- catch_data %>%
#       dplyr::left_join(haul_times, by = c("vessel", "trip_id", "haul_id")) %>%
#       dplyr::mutate(
#         weighing_time = dplyr::coalesce(
#           dplyr::if_else(!is.na(haul_time),  haul_time,  as.POSIXct(NA)),
#           dplyr::if_else(!is.na(shoot_time), shoot_time, as.POSIXct(NA))
#         )
#       ) %>%
#       dplyr::select(-dplyr::any_of(c("haul_time", "shoot_time")))
#   }
# ─────────────────────────────────────────────────────────────────────────────


# ==============================================================================
# DIAGNOSTIC: check existing elog_trek parquet for weighing_time coverage
# ==============================================================================
# Run this in your R session after loading 02_storage_functions.R to assess
# how pervasive the problem is in the data already saved to parquet.

diagnose_weighing_time <- function() {

  et <- tryCatch(load_flyshoot_data("elog_trek"), error = function(e) NULL)

  if (is.null(et) || nrow(et) == 0) {
    message("elog_trek parquet is empty or could not be loaded.")
    return(invisible(NULL))
  }

  total <- nrow(et)

  if (!"weighing_time" %in% names(et)) {
    message(glue("weighing_time column is ABSENT from elog_trek ({total} rows total)"))
    return(invisible(NULL))
  }

  n_na  <- sum(is.na(et$weighing_time))
  n_ok  <- total - n_na
  pct   <- round(100 * n_na / total, 1)

  message(glue("elog_trek weighing_time coverage:"))
  message(glue("  Total rows : {total}"))
  message(glue("  Populated  : {n_ok} ({round(100*n_ok/total,1)}%)"))
  message(glue("  NA (empty) : {n_na} ({pct}%)"))

  # Breakdown by vessel and source type (pefa_trek vs kisten-derived)
  if ("vessel" %in% names(et)) {
    breakdown <- et %>%
      dplyr::mutate(wt_present = !is.na(weighing_time)) %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarise(
        n_rows      = dplyr::n(),
        n_wt_ok     = sum(wt_present),
        n_wt_na     = sum(!wt_present),
        pct_na      = round(100 * sum(!wt_present) / dplyr::n(), 1),
        .groups     = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(pct_na))

    message("\nBreakdown by vessel:")
    print(breakdown, n = 50)
  }

  invisible(et)
}

# Call it:
# diagnose_weighing_time()
