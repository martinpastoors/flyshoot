# ==============================================================================
# KISTEN HAUL ASSIGNMENT MODULE
# ==============================================================================
# Purpose : Automatically assign haul IDs to Marelec kisten box records
#           by matching weighing sessions to treklijst haul shoot times.
#
# Background:
#   The Marelec scale produces a kisten file with box-level weighings but
#   no haul_id. The haul_id must be inferred from timing: the crew weighs
#   catch N while haul N+1 is fishing, so the weighing session for haul N
#   starts shortly after shoot_dt[N+1] (anchor point, typically ±30 min).
#
# Algorithm:
#   1. Parse kisten file → sorted datetime rows
#   2. Detect weighing sessions by time gaps > GAP_THRESHOLD_MIN (30 min)
#   3. Reconstruct full datetimes from treklijst haul sheet (date propagated
#      forward for hauls that only record a time, not a full datetime)
#   4. If n_sessions == n_hauls: assign 1:1 by rank order
#      Else: temporal matching — each session anchored to shoot_dt[N+1]
#            within TOLERANCE_MIN (60 min); fallback to rank order
#   5. Flag sessions whose offset from anchor > FLAG_MIN (30 min) as REVIEW
#   6. Empty hauls (in treklijst, no kisten session) get haul rows with
#      n_weighings = 0 for downstream awareness
#
# Integration point in pipeline:
#   Called from get_catch_from_kisten() when a treklijst is available.
#   The assigned haul_id replaces the NA haul_id that the raw kisten file
#   would otherwise carry into kisten.parquet.
# ==============================================================================

library(dplyr)
library(readxl)
library(lubridate)
library(glue)
library(purrr)

# ── Parameters ──────────────────────────────────────────────────────────────
KISTEN_GAP_THRESHOLD_MIN <- 30   # min gap between weighings to start new session
KISTEN_TOLERANCE_MIN     <- 60   # max offset from anchor to accept a match
KISTEN_FLAG_MIN          <- 30   # offset beyond which to flag for review

# ==============================================================================
# LOW-LEVEL HELPERS
# ==============================================================================

#' Reconstruct full datetimes from treklijst haul shoot times
#' The treklijst stores a full datetime for the first haul of each day and
#' only a time (HH:MM) for the rest. This function propagates the date forward.
#'
#' @param hauls  data.frame with columns shoot_dt (mix of POSIXct and hms/time)
#'               as returned by read_excel — column is class "POSIXct" when
#'               a full date+time exists, class "difftime" / numeric hms when
#'               only a time was entered.
#' @return POSIXct vector in UTC
reconstruct_shoot_datetimes <- function(hauls) {
  # The 'bereken tijd begin uitzetten' column comes out of read_excel as:
  #   POSIXct  when Excel cell has a full date+time
  #   hms / numeric fraction-of-day  when only time was entered
  # We need to walk forward and attach the correct date to time-only rows.

  raw <- hauls[["shoot_datetime"]]
  n   <- length(raw)
  out <- vector("list", n)

  current_date <- NA

  for (i in seq_len(n)) {
    v <- raw[[i]]

    if (inherits(v, "POSIXct") && !is.na(v)) {
      # Full datetime — extract date for propagation
      current_date <- as.Date(v)
      out[[i]]     <- v

    } else if (is.numeric(v) && !is.na(v) && !is.na(current_date)) {
      # Excel fraction-of-day (read_excel returns numeric for time-only cells)
      # Convert fraction to hms and combine with current_date
      hms_str <- format(as.POSIXct(v * 86400, origin = "1970-01-01", tz = "UTC"),
                        "%H:%M:%S")
      dt <- as.POSIXct(paste(current_date, hms_str), tz = "UTC")

      # If time goes backwards by > 2 h, assume day has rolled over
      if (!is.null(out[[i - 1]]) && !is.na(out[[i - 1]])) {
        prev_dt <- out[[i - 1]]
        if (dt < prev_dt - hours(2)) {
          current_date <- current_date + days(1)
          dt <- as.POSIXct(paste(current_date, hms_str), tz = "UTC")
        }
      }
      out[[i]] <- dt

    } else {
      out[[i]] <- as.POSIXct(NA)
    }
  }

  do.call(c, out)
}


#' Detect weighing sessions in sorted kisten data
#' A new session starts whenever the gap to the previous row exceeds the
#' threshold (default 30 min). The first row is always session 1.
#'
#' @param kisten  data.frame with column weighing_time (POSIXct, sorted asc)
#' @param gap_min numeric, minimum gap in minutes to trigger new session
#' @return kisten with an added integer column session_id
detect_kisten_sessions <- function(kisten,
                                   gap_min = KISTEN_GAP_THRESHOLD_MIN) {
  kisten %>%
    arrange(weighing_time) %>%
    mutate(
      gap_min    = as.numeric(difftime(weighing_time,
                                       lag(weighing_time),
                                       units = "mins")),
      session_id = cumsum(replace_na(gap_min > gap_min, TRUE)) + 1L
    )
}


#' Match weighing sessions to treklijst hauls
#' Returns a named integer vector: names = session_id, values = haul_id.
#'
#' @param sessions   data.frame: session_id, session_start (POSIXct)
#' @param haul_ids   integer vector of haul numbers (from treklijst, ordered)
#' @param shoot_dts  POSIXct vector of haul shoot datetimes (same length/order)
#' @param tolerance  numeric, tolerance in minutes for temporal matching
match_sessions_to_hauls <- function(sessions,
                                    haul_ids,
                                    shoot_dts,
                                    tolerance = KISTEN_TOLERANCE_MIN) {
  n_s <- nrow(sessions)
  n_h <- length(haul_ids)

  # ── Case 1: Perfect count match → assign by rank ──────────────────────────
  if (n_s == n_h) {
    mapping <- setNames(haul_ids, sessions$session_id)
    message(glue("    Haul assignment: perfect 1:1 rank match ",
                 "({n_s} sessions = {n_h} hauls)"))
    return(mapping)
  }

  # ── Case 2: Temporal matching ─────────────────────────────────────────────
  message(glue("    Haul assignment: temporal matching ",
               "({n_s} sessions ≠ {n_h} hauls — using anchor proximity)"))

  mapping  <- integer(n_s)
  used_idx <- logical(n_h)

  for (i in seq_len(n_s)) {
    ss        <- sessions$session_start[[i]]
    best_idx  <- NA_integer_
    best_diff <- Inf

    for (j in seq_len(n_h)) {
      if (used_idx[j]) next

      # Anchor = shoot time of the NEXT haul (j+1)
      anchor <- if (j < n_h) shoot_dts[[j + 1]] else NA
      if (is.na(anchor)) next

      diff_min <- as.numeric(difftime(ss, anchor, units = "mins"))

      if (abs(diff_min) <= tolerance && abs(diff_min) < abs(best_diff)) {
        best_idx  <- j
        best_diff <- diff_min
      }
    }

    if (is.na(best_idx)) {
      # Fallback: assign to earliest unmatched haul
      best_idx <- which(!used_idx)[1]
      message(glue("    ⚠ Session {sessions$session_id[[i]]} ",
                   "({format(ss, '%m-%d %H:%M')}) — no anchor match, ",
                   "fallback to haul {haul_ids[[best_idx]]}"))
    }

    mapping[[i]]      <- haul_ids[[best_idx]]
    used_idx[[best_idx]] <- TRUE
  }

  setNames(mapping, sessions$session_id)
}


# ==============================================================================
# MAIN EXPORTED FUNCTION
# ==============================================================================

#' Assign haul IDs to kisten rows using treklijst shoot times
#'
#' This is the core function integrating the kisten auto-assignment algorithm
#' into the flyshoot pipeline.  It is called from get_catch_from_kisten()
#' when a treklijst file is available alongside the kisten file.
#'
#' @param kisten     data.frame as returned by get_catch_from_kisten() *before*
#'                   haul_id assignment; must have column weighing_time (POSIXct)
#' @param treklijst  data.frame: the Haul sheet of the treklijst, as returned by
#'                   read_excel(); required columns: haul, shoot_datetime,
#'                   catch_kg (Totale vangst kg berekend)
#' @param local_tz   character IANA timezone string for local->UTC conversion
#'                   (default "Europe/Amsterdam")
#' @param gap_min    numeric, gap threshold in minutes (default 30)
#' @param tolerance  numeric, anchor-match tolerance in minutes (default 60)
#' @param flag_min   numeric, flag threshold in minutes (default 30)
#'
#' @return list with two elements:
#'   $kisten  — original kisten data.frame with haul_id, session_id, gap_min
#'              columns filled; also a logical column haul_flag (TRUE = review)
#'   $summary — one row per treklijst haul: haul_id, n_weighings,
#'              weighing_start, weighing_end, weighed_kg, trek_catch_kg,
#'              anchor_offset_min, haul_flag, is_empty_haul
assign_haul_ids_from_treklijst <- function(kisten,
                                           treklijst,
                                           local_tz   = "Europe/Amsterdam",
                                           gap_min    = KISTEN_GAP_THRESHOLD_MIN,
                                           tolerance  = KISTEN_TOLERANCE_MIN,
                                           flag_min   = KISTEN_FLAG_MIN) {

  # ── 1. Validate inputs ──────────────────────────────────────────────────────
  stopifnot("weighing_time" %in% names(kisten))
  stopifnot(all(c("haul", "shoot_datetime", "catch_kg") %in% names(treklijst)))

  # ── 2. Filter treklijst to hauls that have a shoot time ────────────────────
  trek <- treklijst %>%
    filter(!is.na(haul), !is.na(shoot_datetime)) %>%
    mutate(haul = as.integer(haul)) %>%
    arrange(haul)

  trek$shoot_dt <- reconstruct_shoot_datetimes(trek)

  # Drop hauls where datetime reconstruction failed
  trek <- trek %>% filter(!is.na(shoot_dt))

  haul_ids  <- trek$haul
  shoot_dts <- trek$shoot_dt
  catch_kgs <- trek$catch_kg

  message(glue("    Treklijst: {nrow(trek)} hauls with valid shoot times, ",
               "{nrow(treklijst) - nrow(trek)} dropped (no shoot time)"))

  # ── 3. Sort kisten and detect weighing sessions ─────────────────────────────
  kisten <- kisten %>%
    arrange(weighing_time) %>%
    mutate(
      time_gap_min = as.numeric(difftime(weighing_time,
                                         lag(weighing_time), units = "mins")),
      session_id   = cumsum(replace_na(time_gap_min > gap_min, TRUE)) + 1L
    )

  sessions <- kisten %>%
    group_by(session_id) %>%
    summarise(
      session_start = min(weighing_time),
      session_end   = max(weighing_time),
      n_rows        = n(),
      .groups       = "drop"
    )

  message(glue("    Kisten: {nrow(kisten)} box records, ",
               "{nrow(sessions)} weighing sessions detected ",
               "(gap threshold = {gap_min} min)"))

  # ── 4. Match sessions → hauls ───────────────────────────────────────────────
  mapping <- match_sessions_to_hauls(sessions, haul_ids, shoot_dts, tolerance)

  sessions <- sessions %>%
    mutate(haul_id = mapping[as.character(session_id)])

  # ── 5. Compute anchor offsets and flags ────────────────────────────────────
  sessions <- sessions %>%
    left_join(
      tibble(haul_id = haul_ids,
             haul_idx = seq_along(haul_ids),
             shoot_dt = shoot_dts),
      by = "haul_id"
    ) %>%
    mutate(
      # Anchor = shoot_dt of haul N+1
      anchor_dt = shoot_dts[pmin(haul_idx + 1L, length(shoot_dts))],
      anchor_offset_min = as.numeric(difftime(session_start, anchor_dt,
                                              units = "mins")),
      # Overnight sessions: large negative offsets are expected, not errors
      haul_flag = !is.na(anchor_offset_min) &
                  abs(anchor_offset_min) > flag_min &
                  anchor_offset_min > -300
    )

  # ── 6. Attach haul_id and flag back to individual kisten rows ──────────────
  kisten <- kisten %>%
    left_join(sessions %>% select(session_id, haul_id, haul_flag),
              by = "session_id")

  n_flagged <- kisten %>% distinct(session_id, haul_flag) %>%
    filter(haul_flag) %>% nrow()
  if (n_flagged > 0) {
    message(glue("    ⚠ {n_flagged} session(s) flagged for review ",
                 "(offset > {flag_min} min from anchor)"))
  }

  # ── 7. Build haul summary (one row per treklijst haul) ─────────────────────
  kisten_summary <- kisten %>%
    group_by(haul_id) %>%
    summarise(
      n_weighings    = n(),
      weighing_start = min(weighing_time),
      weighing_end   = max(weighing_time),
      weighed_kg     = sum(weight_kg, na.rm = TRUE),
      haul_flag      = any(haul_flag, na.rm = TRUE),
      .groups        = "drop"
    )

  summary <- trek %>%
    select(haul_id = haul, shoot_dt, trek_catch_kg = catch_kg) %>%
    left_join(kisten_summary, by = "haul_id") %>%
    left_join(sessions %>% select(haul_id, anchor_offset_min),
              by = "haul_id") %>%
    mutate(
      is_empty_haul = is.na(n_weighings) | n_weighings == 0,
      n_weighings   = replace_na(n_weighings, 0L),
      weighed_kg    = replace_na(weighed_kg, 0),
      haul_flag     = replace_na(haul_flag, FALSE)
    ) %>%
    arrange(haul_id)

  n_empty <- sum(summary$is_empty_haul)
  message(glue("    Assignment complete: {nrow(summary) - n_empty} hauls with ",
               "weighings, {n_empty} empty hauls (treklijst only)"))

  list(kisten = kisten, summary = summary)
}


# ==============================================================================
# PIPELINE INTEGRATION HELPER
# ==============================================================================

#' Read treklijst Haul sheet for kisten haul assignment
#'
#' Thin wrapper used by get_catch_from_kisten() to load the treklijst that
#' accompanies the kisten file. Returns NULL (with a message) if the file
#' does not exist or the Haul sheet is missing, so the caller can fall back
#' gracefully to haul_id = NA.
#'
#' @param treklijst_path  path to the treklijst .xlsx file
#' @return data.frame with columns haul, shoot_datetime, catch_kg — or NULL
read_treklijst_for_kisten <- function(treklijst_path) {
  if (is.null(treklijst_path) || is.na(treklijst_path) ||
      !file.exists(treklijst_path)) {
    message("    No treklijst found alongside kisten file — haul_id will be NA")
    return(NULL)
  }

  sheets <- tryCatch(excel_sheets(treklijst_path), error = function(e) character(0))
  if (!"Haul" %in% sheets) {
    message(glue("    Treklijst '{basename(treklijst_path)}' has no 'Haul' sheet ",
                 "— haul_id will be NA"))
    return(NULL)
  }

  haul_sheet <- read_excel(treklijst_path, sheet = "Haul", col_types = "list")

  # Standardise column names (handle multi-line headers from Excel)
  names(haul_sheet) <- names(haul_sheet) %>%
    tolower() %>%
    gsub("\n", " ", .) %>%
    trimws() %>%
    gsub("\\s+", "_", .)

  # Identify the shoot-time column (the computed/calculated version)
  shoot_col <- grep("bereken.*begin.*uitzetten|bereken.*shoot.*begin",
                    names(haul_sheet), value = TRUE)[1]
  catch_col <- grep("totale_vangst|total_catch|vangst.*kg",
                    names(haul_sheet), value = TRUE)[1]

  if (is.na(shoot_col)) {
    message("    Could not find 'bereken tijd begin uitzetten' in treklijst — haul_id will be NA")
    return(NULL)
  }

  haul_sheet %>%
    filter(!is.na(haul)) %>%
    transmute(
      haul          = as.integer(haul),
      shoot_datetime = .data[[shoot_col]],
      catch_kg      = if (!is.na(catch_col)) as.numeric(.data[[catch_col]]) else NA_real_
    )
}
