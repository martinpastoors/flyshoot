# =============================================================================
# parse_ers.R  —  Agiltech / TurboCatch ERS V3 XML parser
# =============================================================================
# Message types handled:
#   DEP  Departure from port
#   COE  Entry into fishing zone
#   FAR  Fishing Activity Report (haul catches)
#   EOF  End of Fishing (last haul done)
#   COX  Exit from fishing zone (cumulative catch on board)
#   PNO  Prior Notification of landing
#   COR  Correction to a previous message (unwrapped transparently)
#   RTP  Return to Port
#   LAN  Landing Declaration (actual landed catch)
#
# Usage:
#   source("parse_ers.R")
#   data <- parse_ers_folder("path/to/xml/files")
#   View(data$trips)
#   View(catch_by_species(data))
#   View(haul_detail(data))
# =============================================================================

library(xml2)
library(dplyr)
library(purrr)
library(tibble)

# ── low-level helpers ─────────────────────────────────────────────────────────

attr_safe <- function(node, name) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
  v <- xml_attr(node, name)
  if (is.null(v) || is.na(v) || v == "") NA_character_ else v
}

find_node <- function(parent, tag, ns) {
  n <- xml_find_first(parent, paste0(".//d1:", tag), ns)
  if (inherits(n, "xml_missing")) n <- xml_find_first(parent, paste0(".//", tag))
  n
}

find_nodes <- function(parent, tag, ns) {
  nn <- xml_find_all(parent, paste0(".//d1:", tag), ns)
  if (length(nn) == 0) nn <- xml_find_all(parent, paste0(".//", tag))
  nn
}

# ── species rows (shared by FAR, COX, PNO, LAN) ──────────────────────────────

parse_species <- function(parent, ns) {
  spe_nodes <- find_nodes(parent, "SPE", ns)
  if (length(spe_nodes) == 0) return(tibble())

  map_dfr(spe_nodes, function(spe) {
    ras <- find_node(spe, "RAS", ns)
    pro <- find_node(spe, "PRO", ns)
    tibble(
      species      = attr_safe(spe, "SN"),
      weight_kg    = as.numeric(attr_safe(spe, "WT")),
      weight_live  = as.numeric(attr_safe(spe, "WL")),
      measure      = attr_safe(spe, "MM"),
      fao_area     = attr_safe(ras, "FA"),
      ices_subarea = attr_safe(ras, "SA"),
      ices_rect    = attr_safe(ras, "SR"),
      eez          = attr_safe(ras, "EZ"),
      presentation = attr_safe(pro, "PR"),
      freshness    = attr_safe(pro, "PS"),
      conv_factor  = as.numeric(attr_safe(pro, "CF")),
      n_boxes      = as.integer(attr_safe(pro, "NN")),
      box_weight_kg = as.numeric(attr_safe(pro, "AW"))
    )
  })
}

# ── single-file parser ────────────────────────────────────────────────────────

parse_ers_file <- function(filepath) {
  doc <- tryCatch(read_xml(filepath),
                  error = function(e) { message("SKIP: ", basename(filepath)); NULL })
  if (is.null(doc)) return(NULL)

  ns       <- xml_ns(doc)
  ops_node <- xml_root(doc)

  # --- OPS-level metadata
  msg_ref  <- attr_safe(ops_node, "ON")
  msg_date <- attr_safe(ops_node, "OD")
  msg_time <- attr_safe(ops_node, "OT")
  software <- attr_safe(ops_node, "EVL")

  # --- Detect COR wrapper
  cor_node     <- find_node(ops_node, "COR", ns)
  is_cor       <- !inherits(cor_node, "xml_missing")
  corrects_msg <- if (is_cor) attr_safe(cor_node, "RN") else NA_character_
  cor_reason   <- if (is_cor) attr_safe(cor_node, "RE") else NA_character_

  # --- Navigate to LOG
  ers_node <- find_node(ops_node, "ERS", ns)
  log_node <- find_node(ers_node, "LOG", ns)
  ers_ref  <- attr_safe(ers_node, "RN")

  # --- Vessel metadata
  vessel_id   <- attr_safe(log_node, "XR")
  vessel_rc   <- attr_safe(log_node, "RC")
  vessel_name <- attr_safe(log_node, "NA")
  flag        <- attr_safe(log_node, "FS")

  # --- Trip ID from ELOG
  elog_node <- find_node(log_node, "ELOG", ns)
  trip_id   <- attr_safe(elog_node, "TN")

  # Helper: build the standard metadata columns for every output row
  meta <- function() {
    tibble(
      trip_id      = trip_id,
      ers_ref      = ers_ref,
      vessel_id    = vessel_id,
      vessel_rc    = vessel_rc,
      vessel_name  = vessel_name,
      flag         = flag,
      msg_ref      = msg_ref,
      msg_date     = msg_date,
      msg_time     = msg_time,
      is_correction = is_cor,
      corrects_msg = corrects_msg,
      cor_reason   = cor_reason,
      source_file  = basename(filepath)
    )
  }

  results <- list(events = list(), far = list(), pno = list(), lan = list())

  # ── DEP ──────────────────────────────────────────────────────────────────
  dep <- find_node(log_node, "DEP", ns)
  if (!inherits(dep, "xml_missing")) {
    gea_nodes <- find_nodes(dep, "GEA", ns)
    gears <- if (length(gea_nodes) > 0)
      paste(map_chr(gea_nodes, ~ paste0(attr_safe(.x, "GE"), "/",
                                         attr_safe(.x, "ME"), "mm")),
            collapse = "; ")
    else NA_character_

    results$events <- c(results$events, list(
      bind_cols(meta(), tibble(
        msg_type       = "DEP",
        event_date     = attr_safe(dep, "DA"),
        event_time     = attr_safe(dep, "TI"),
        port           = attr_safe(dep, "PO"),
        activity       = attr_safe(dep, "AA"),
        gears_declared = gears,
        lat = NA_real_, lon = NA_real_,
        ices_rect = NA_character_, eez = NA_character_
      ))
    ))
  }

  # ── COE ──────────────────────────────────────────────────────────────────
  coe <- find_node(log_node, "COE", ns)
  if (!inherits(coe, "xml_missing")) {
    pos <- find_node(coe, "POS", ns)
    ras <- find_node(coe, "RAS", ns)
    results$events <- c(results$events, list(
      bind_cols(meta(), tibble(
        msg_type       = "COE",
        event_date     = attr_safe(coe, "DA"),
        event_time     = attr_safe(coe, "TI"),
        port           = NA_character_,
        activity       = NA_character_,
        gears_declared = NA_character_,
        lat            = as.numeric(attr_safe(pos, "LT")),
        lon            = as.numeric(attr_safe(pos, "LG")),
        ices_rect      = attr_safe(ras, "SR"),
        eez            = attr_safe(ras, "EZ")
      ))
    ))
  }

  # ── EOF ──────────────────────────────────────────────────────────────────
  eof <- find_node(log_node, "EOF", ns)
  if (!inherits(eof, "xml_missing")) {
    results$events <- c(results$events, list(
      bind_cols(meta(), tibble(
        msg_type       = "EOF",
        event_date     = attr_safe(eof, "DA"),
        event_time     = attr_safe(eof, "TI"),
        port           = NA_character_,
        activity       = NA_character_,
        gears_declared = NA_character_,
        lat = NA_real_, lon = NA_real_,
        ices_rect = NA_character_, eez = NA_character_
      ))
    ))
  }

  # ── COX ──────────────────────────────────────────────────────────────────
  cox <- find_node(log_node, "COX", ns)
  if (!inherits(cox, "xml_missing")) {
    pos <- find_node(cox, "POS", ns)
    ras <- find_node(cox, "RAS", ns)
    cox_catches <- parse_species(cox, ns)
    results$events <- c(results$events, list(
      bind_cols(meta(), tibble(
        msg_type        = "COX",
        event_date      = attr_safe(cox, "DA"),
        event_time      = attr_safe(cox, "TI"),
        port            = NA_character_,
        activity        = NA_character_,
        gears_declared  = NA_character_,
        lat             = as.numeric(attr_safe(pos, "LT")),
        lon             = as.numeric(attr_safe(pos, "LG")),
        ices_rect       = attr_safe(ras, "SR"),
        eez             = attr_safe(ras, "EZ"),
        total_onboard_kg = sum(cox_catches$weight_kg, na.rm = TRUE)
      ))
    ))
  }

  # ── RTP ──────────────────────────────────────────────────────────────────
  rtp <- find_node(log_node, "RTP", ns)
  if (!inherits(rtp, "xml_missing")) {
    results$events <- c(results$events, list(
      bind_cols(meta(), tibble(
        msg_type       = "RTP",
        event_date     = attr_safe(rtp, "DA"),
        event_time     = attr_safe(rtp, "TI"),
        port           = attr_safe(rtp, "PO"),
        activity       = attr_safe(rtp, "RE"),
        gears_declared = NA_character_,
        lat = NA_real_, lon = NA_real_,
        ices_rect = NA_character_, eez = NA_character_
      ))
    ))
  }

  # ── FAR ──────────────────────────────────────────────────────────────────
  far_nodes <- find_nodes(log_node, "FAR", ns)
  for (far in far_nodes) {
    pos     <- find_node(far, "POS", ns)
    gea     <- find_node(far, "GEA", ns)
    catches <- parse_species(far, ns)
    if (nrow(catches) == 0) next

    results$far <- c(results$far, list(
      bind_cols(
        meta(),
        tibble(
          msg_type     = "FAR",
          haul_date    = attr_safe(far, "DA"),
          haul_time    = attr_safe(far, "TI"),
          lat          = as.numeric(attr_safe(pos, "LT")),
          lon          = as.numeric(attr_safe(pos, "LG")),
          gear_type    = attr_safe(gea, "GE"),
          mesh_mm      = as.numeric(attr_safe(gea, "ME")),
          n_shots      = as.integer(attr_safe(gea, "FO")),
          duration_min = as.numeric(attr_safe(gea, "DU")),
          haul_depth_m = as.numeric(attr_safe(gea, "FD"))
        ),
        catches
      )
    ))
  }

  # ── PNO ──────────────────────────────────────────────────────────────────
  pno <- find_node(log_node, "PNO", ns)
  if (!inherits(pno, "xml_missing")) {
    pos     <- find_node(pno, "POS", ns)
    catches <- parse_species(pno, ns)
    if (nrow(catches) > 0) {
      results$pno <- c(results$pno, list(
        bind_cols(
          meta(),
          tibble(
            msg_type        = "PNO",
            landing_date    = attr_safe(pno, "DA"),
            landing_time    = attr_safe(pno, "TI"),
            predicted_date  = attr_safe(pno, "PD"),
            predicted_time  = attr_safe(pno, "PT"),
            port            = attr_safe(pno, "PO"),
            trip_start_date = attr_safe(pno, "DS"),
            lat             = as.numeric(attr_safe(pos, "LT")),
            lon             = as.numeric(attr_safe(pos, "LG"))
          ),
          catches
        )
      ))
    }
  }

  # ── LAN ──────────────────────────────────────────────────────────────────
  lan <- find_node(log_node, "LAN", ns)
  if (!inherits(lan, "xml_missing")) {
    catches <- parse_species(lan, ns)
    if (nrow(catches) > 0) {
      results$lan <- c(results$lan, list(
        bind_cols(
          meta(),
          tibble(
            msg_type     = "LAN",
            landing_date = attr_safe(lan, "DA"),
            landing_time = attr_safe(lan, "TI"),
            port         = attr_safe(lan, "PO"),
            buyer        = attr_safe(lan, "TS")
          ),
          catches
        )
      ))
    }
  }

  results
}

# ── folder parser ─────────────────────────────────────────────────────────────

#' Parse all ERS XML files in a folder into tidy data frames
#'
#' @param folder_path  Path to directory containing XML files
#' @param recursive    Also search sub-folders (default FALSE)
#' @return Named list: $events, $far, $pno, $lan, $trips
parse_ers_folder <- function(folder_path, recursive = FALSE) {
  files <- list.files(folder_path, pattern = "\\.xml$",
                      full.names = TRUE, recursive = recursive)
  if (length(files) == 0) { message("No XML files found."); return(NULL) }

  message("Parsing ", length(files), " files...")
  raw <- map(files, parse_ers_file)

  out <- list(
    events = bind_rows(map(raw, ~ bind_rows(.x$events))),
    far    = bind_rows(map(raw, ~ bind_rows(.x$far))),
    pno    = bind_rows(map(raw, ~ bind_rows(.x$pno))),
    lan    = bind_rows(map(raw, ~ bind_rows(.x$lan)))
  )

  out$trips <- make_trip_summary(out)

  n_trips <- dplyr::n_distinct(out$events$trip_id, na.rm = TRUE)
  message("Done.  Trips found: ", n_trips)
  out
}

# ── trip summary ──────────────────────────────────────────────────────────────

make_trip_summary <- function(data) {
  dep_info <- data$events %>%
    filter(msg_type == "DEP") %>%
    select(trip_id, vessel_id, vessel_name, flag,
           dep_date = event_date, dep_time = event_time,
           dep_port = port, gears_declared) %>%
    distinct()

  rtp_info <- data$events %>%
    filter(msg_type == "RTP") %>%
    select(trip_id, rtp_date = event_date, rtp_time = event_time,
           rtp_port = port) %>%
    distinct()

  lan_totals <- data$lan %>%
    filter(!is_correction) %>%
    group_by(trip_id) %>%
    summarise(
      landed_kg_total  = sum(weight_kg, na.rm = TRUE),
      n_species_landed = n_distinct(species),
      landing_date     = first(landing_date),
      landing_port     = first(port),
      .groups = "drop"
    )

  far_totals <- data$far %>%
    filter(!is_correction) %>%
    group_by(trip_id) %>%
    summarise(
      far_kg_total  = sum(weight_kg, na.rm = TRUE),
      n_hauls       = n_distinct(paste(haul_date, haul_time)),
      n_shots_total = sum(n_shots, na.rm = TRUE),
      fishing_areas = paste(sort(unique(na.omit(ices_rect))), collapse = ", "),
      .groups = "drop"
    )

  dep_info %>%
    left_join(rtp_info,   by = "trip_id") %>%
    left_join(lan_totals, by = "trip_id") %>%
    left_join(far_totals, by = "trip_id") %>%
    arrange(dep_date)
}

# ── convenience summaries ─────────────────────────────────────────────────────

#' Species totals per trip from LAN messages (most authoritative)
#' Use source = "far" for haul-level catch, source = "pno" for pre-landing
catch_by_species <- function(data, source = "lan") {
  data[[source]] %>%
    filter(!is_correction) %>%
    group_by(trip_id, vessel_id, species, ices_rect) %>%
    summarise(total_kg = sum(weight_kg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(trip_id, desc(total_kg))
}

#' Haul-by-haul summary from FAR messages
haul_detail <- function(data) {
  data$far %>%
    filter(!is_correction) %>%
    group_by(trip_id, vessel_id, haul_date, haul_time,
             lat, lon, ices_rect, gear_type, mesh_mm,
             n_shots, duration_min) %>%
    summarise(
      total_kg     = sum(weight_kg, na.rm = TRUE),
      n_species    = n_distinct(species),
      species_list = paste(sort(unique(species)), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(haul_date, haul_time)
}

# =============================================================================
# USAGE EXAMPLE
# =============================================================================
# source("parse_ers.R")

# data <- parse_ers_folder("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch ExportMessagesERS_EVLinden/sorted/CC545762")

# View(data$trips)                        # one row per trip
# View(catch_by_species(data))            # landed kg by trip / species
# View(haul_detail(data))                 # haul-by-haul from FAR
# View(data$far  %>% filter(!is_correction))  # all FAR species rows
# View(data$pno  %>% filter( is_correction))  # corrected PNO messages

# write.csv(data$trips,             "trips.csv",   row.names = FALSE)
# write.csv(catch_by_species(data), "catches.csv", row.names = FALSE)

