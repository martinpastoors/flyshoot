# =============================================================================
# parse_ers.R  —  Agiltech / TurboCatch ERS V3 XML parser
# =============================================================================
# Message types handled:
#   DEP  Departure from port
#   COE  Entry into fishing zone
#   FAR  Fishing Activity Report (haul catches)
#   EOF  End of Fishing (last haul done)
#   COX  Exit from fishing zone (cumulative catch on board)
#   PNO  Prior Notification of landing (predicted landing + catch)
#   COR  Correction to a previous message  (wrapped around ERS/PNO etc.)
#   RTP  Return to Port
#   LAN  Landing Declaration (actual landed catch)
#
# Key design decisions:
#   • COR messages are unwrapped transparently; the corrected message type
#     is parsed normally, and a flag `is_correction` + `corrects_msg` are added.
#   • The trip identifier (ELOG TN) links all messages of one trip together.
#   • parse_ers_folder() returns a named list of tidy data frames:
#       $events   — one row per message (DEP, COE, COX, EOF, RTP)
#       $far      — haul-level catches
#       $pno      — pre-landing catch notifications
#       $lan      — actual landing declarations
#       $trips    — trip-level summary (derived)
# =============================================================================

library(xml2)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)

# ── low-level helpers ─────────────────────────────────────────────────────────

attr_safe <- function(node, name) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
  v <- xml_attr(node, name)
  if (is.null(v) || is.na(v) || v == "") NA_character_ else v
}

find_node <- function(parent, tag, ns) {
  # Try namespace-qualified first, fall back to unqualified
  n <- xml_find_first(parent, paste0(".//d1:", tag), ns)
  if (inherits(n, "xml_missing")) n <- xml_find_first(parent, paste0(".//", tag))
  n
}

find_nodes <- function(parent, tag, ns) {
  nn <- xml_find_all(parent, paste0(".//d1:", tag), ns)
  if (length(nn) == 0) nn <- xml_find_all(parent, paste0(".//", tag))
  nn
}

# ── vessel & OPS metadata ─────────────────────────────────────────────────────

parse_ops_meta <- function(ops_node) {
  list(
    msg_ref   = attr_safe(ops_node, "ON"),
    msg_date  = attr_safe(ops_node, "OD"),
    msg_time  = attr_safe(ops_node, "OT"),
    software  = attr_safe(ops_node, "EVL")
  )
}

parse_vessel_meta <- function(log_node) {
  list(
    vessel_id   = attr_safe(log_node, "XR"),   # external reg — use as vessel key
    vessel_rc   = attr_safe(log_node, "RC"),
    vessel_name = attr_safe(log_node, "NA"),
    flag        = attr_safe(log_node, "FS")
  )
}

parse_trip_id <- function(log_node, ns) {
  elog <- find_node(log_node, "ELOG", ns)
  attr_safe(elog, "TN")
}

# ── species parser (shared by FAR, COX, PNO, LAN) ────────────────────────────

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
      measure      = attr_safe(spe, "MM"),   # WGH=weighed, EST=estimated
      gear         = attr_safe(spe, "GE"),
      fao_area     = attr_safe(ras, "FA"),
      ices_subarea = attr_safe(ras, "SA"),
      ices_rect    = attr_safe(ras, "SR"),
      eez          = attr_safe(ras, "EZ"),
      presentation = attr_safe(pro, "PR"),   # WHL, GUT, …
      freshness    = attr_safe(pro, "PS"),
      conv_factor  = as.numeric(attr_safe(pro, "CF")),
      n_boxes      = as.integer(attr_safe(pro, "NN")),
      box_weight   = as.numeric(attr_safe(pro, "AW"))
    )
  })
}

# ── per-message-type parsers ──────────────────────────────────────────────────

parse_dep <- function(node, ns, base) {
  gea_nodes <- find_nodes(node, "GEA", ns)
  gears <- if (length(gea_nodes) > 0) {
    paste(map_chr(gea_nodes, ~ paste0(
      attr_safe(.x, "GE"), "/", attr_safe(.x, "ME"), "mm"
    )), collapse = "; ")
  } else NA_character_

  bind_cols(as_tibble(base), tibble(
    msg_type   = "DEP",
    event_date = attr_safe(node, "DA"),
    event_time = attr_safe(node, "TI"),
    port       = attr_safe(node, "PO"),
    activity   = attr_safe(node, "AA"),   # FSH = fishing
    gears_declared = gears,
    lat = NA_real_, lon = NA_real_
  ))
}

parse_coe <- function(node, ns, base) {
  pos <- find_node(node, "POS", ns)
  ras <- find_node(node, "RAS", ns)
  bind_cols(as_tibble(base), tibble(
    msg_type   = "COE",
    event_date = attr_safe(node, "DA"),
    event_time = attr_safe(node, "TI"),
    port       = NA_character_,
    activity   = NA_character_,
    gears_declared = NA_character_,
    lat        = as.numeric(attr_safe(pos, "LT")),
    lon        = as.numeric(attr_safe(pos, "LG")),
    ices_rect  = attr_safe(ras, "SR"),
    eez        = attr_safe(ras, "EZ")
  ))
}

parse_eof <- function(node, ns, base) {
  bind_cols(as_tibble(base), tibble(
    msg_type   = "EOF",
    event_date = attr_safe(node, "DA"),
    event_time = attr_safe(node, "TI"),
    port       = NA_character_, activity = NA_character_,
    gears_declared = NA_character_,
    lat = NA_real_, lon = NA_real_
  ))
}

parse_cox <- function(node, ns, base) {
  pos <- find_node(node, "POS", ns)
  ras <- find_node(node, "RAS", ns)
  catches <- parse_species(node, ns)

  event_row <- bind_cols(as_tibble(base), tibble(
    msg_type   = "COX",
    event_date = attr_safe(node, "DA"),
    event_time = attr_safe(node, "TI"),
    port       = NA_character_, activity = NA_character_,
    gears_declared = NA_character_,
    lat        = as.numeric(attr_safe(pos, "LT")),
    lon        = as.numeric(attr_safe(pos, "LG")),
    ices_rect  = attr_safe(ras, "SR"),
    eez        = attr_safe(ras, "EZ"),
    total_catch_kg = sum(catches$weight_kg, na.rm = TRUE)
  ))
  list(event = event_row, catches = catches)
}

parse_rtp <- function(node, ns, base) {
  bind_cols(as_tibble(base), tibble(
    msg_type   = "RTP",
    event_date = attr_safe(node, "DA"),
    event_time = attr_safe(node, "TI"),
    port       = attr_safe(node, "PO"),
    activity   = attr_safe(node, "RE"),   # LAN = landing
    gears_declared = NA_character_,
    lat = NA_real_, lon = NA_real_
  ))
}

parse_far_msg <- function(node, ns, base) {
  pos <- find_node(node, "POS", ns)
  gea <- find_node(node, "GEA", ns)
  catches <- parse_species(node, ns)
  if (nrow(catches) == 0) return(NULL)

  bind_cols(
    as_tibble(base),
    tibble(
      msg_type     = "FAR",
      haul_date    = attr_safe(node, "DA"),
      haul_time    = attr_safe(node, "TI"),
      is_correction = base$is_correction,
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
}

parse_pno_msg <- function(node, ns, base) {
  pos <- find_node(node, "POS", ns)
  catches <- parse_species(node, ns)
  if (nrow(catches) == 0) return(NULL)

  bind_cols(
    as_tibble(base),
    tibble(
      msg_type         = "PNO",
      landing_date     = attr_safe(node, "DA"),
      landing_time     = attr_safe(node, "TI"),
      predicted_date   = attr_safe(node, "PD"),
      predicted_time   = attr_safe(node, "PT"),
      port             = attr_safe(node, "PO"),
      trip_start_date  = attr_safe(node, "DS"),
      is_correction    = base$is_correction,
      corrects_msg     = base$corrects_msg,
      lat              = as.numeric(attr_safe(pos, "LT")),
      lon              = as.numeric(attr_safe(pos, "LG"))
    ),
    catches
  )
}

parse_lan_msg <- function(node, ns, base) {
  catches <- parse_species(node, ns)
  if (nrow(catches) == 0) return(NULL)

  bind_cols(
    as_tibble(base),
    tibble(
      msg_type     = "LAN",
      landing_date = attr_safe(node, "DA"),
      landing_time = attr_safe(node, "TI"),
      port         = attr_safe(node, "PO"),
      buyer        = attr_safe(node, "TS")
    ),
    catches
  )
}

# ── single file parser ────────────────────────────────────────────────────────

parse_ers_file <- function(filepath) {
  doc <- tryCatch(read_xml(filepath),
                  error = function(e) { message("SKIP: ", basename(filepath)); NULL })
  if (is.null(doc)) return(NULL)

  ns       <- xml_ns(doc)
  ops_node <- xml_root(doc)
  ops_meta <- parse_ops_meta(ops_node)

  # Detect COR wrapper
  cor_node     <- xml_find_first(ops_node, ".//d1:COR", ns)
  if (inherits(cor_node, "xml_missing")) cor_node <- xml_find_first(ops_node, ".//COR")
  is_cor       <- !inherits(cor_node, "xml_missing")
  corrects_msg <- if (is_cor) attr_safe(cor_node, "RN") else NA_character_
  cor_reason   <- if (is_cor) attr_safe(cor_node, "RE") else NA_character_

  # Navigate to ERS > LOG
  ers_node <- find_node(ops_node, "ERS", ns)
  log_node <- find_node(ers_node, "LOG", ns)

  vessel_meta <- parse_vessel_meta(log_node)
  trip_id     <- parse_trip_id(log_node, ns)
  ers_ref     <- attr_safe(ers_node, "RN")

  base <- c(ops_meta, vessel_meta,
            list(trip_id      = trip_id,
                 ers_ref      = ers_ref,
                 is_correction = is_cor,
                 corrects_msg = corrects_msg,
                 cor_reason   = cor_reason,
                 source_file  = basename(filepath)))

  results <- list(events = list(), far = list(), pno = list(), lan = list())

  # -- DEP
  dep <- find_node(log_node, "DEP", ns)
  if (!inherits(dep, "xml_missing"))
    results$events <- c(results$events, list(parse_dep(dep, ns, base)))

  # -- COE
  coe <- find_node(log_node, "COE", ns)
  if (!inherits(coe, "xml_missing"))
    results$events <- c(results$events, list(parse_coe(coe, ns, base)))

  # -- EOF
  eof <- find_node(log_node, "EOF", ns)
  if (!inherits(eof, "xml_missing"))
    results$events <- c(results$events, list(parse_eof(eof, ns, base)))

  # -- COX
  cox <- find_node(log_node, "COX", ns)
  if (!inherits(cox, "xml_missing")) {
    cox_parsed <- parse_cox(cox, ns, base)
    results$events <- c(results$events, list(cox_parsed$event))
  }

  # -- RTP
  rtp <- find_node(log_node, "RTP", ns)
  if (!inherits(rtp, "xml_missing"))
    results$events <- c(results$events, list(parse_rtp(rtp, ns, base)))

  # -- FAR (can be multiple per file, though rare)
  far_nodes <- find_nodes(log_node, "FAR", ns)
  for (far in far_nodes) {
    r <- parse_far_msg(far, ns, base)
    if (!is.null(r)) results$far <- c(results$far, list(r))
  }

  # -- PNO
  pno <- find_node(log_node, "PNO", ns)
  if (!inherits(pno, "xml_missing")) {
    r <- parse_pno_msg(pno, ns, base)
    if (!is.null(r)) results$pno <- c(results$pno, list(r))
  }

  # -- LAN
  lan <- find_node(log_node, "LAN", ns)
  if (!inherits(lan, "xml_missing")) {
    r <- parse_lan_msg(lan, ns, base)
    if (!is.null(r)) results$lan <- c(results$lan, list(r))
  }

  results
}

# ── folder parser ─────────────────────────────────────────────────────────────

#' Parse all ERS XML files in a folder into tidy data frames
#'
#' @param folder_path Path to directory of XML files
#' @param recursive   Also search sub-folders (default FALSE)
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

  # Trip summary — derived from events + LAN
  out$trips <- make_trip_summary(out)

  message("Done.  Trips found: ", n_distinct(out$events$trip_id, na.rm = TRUE))
  out
}

# ── trip summary ──────────────────────────────────────────────────────────────

make_trip_summary <- function(data) {
  # Departure info
  dep_info <- data$events %>%
    filter(msg_type == "DEP") %>%
    select(trip_id, vessel_id, vessel_name, flag,
           dep_date = event_date, dep_time = event_time,
           dep_port = port, gears_declared) %>%
    distinct()

  # Return to port info
  rtp_info <- data$events %>%
    filter(msg_type == "RTP") %>%
    select(trip_id, rtp_date = event_date, rtp_time = event_time,
           rtp_port = port) %>%
    distinct()

  # Landing totals from LAN messages (most authoritative)
  lan_totals <- data$lan %>%
    group_by(trip_id) %>%
    summarise(
      landed_kg_total  = sum(weight_kg, na.rm = TRUE),
      n_species_landed = n_distinct(species),
      landing_date     = first(landing_date),
      landing_port     = first(port),
      .groups = "drop"
    )

  # FAR totals
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

#' Catch totals by trip, species (from LAN — most authoritative)
catch_by_species <- function(data, source = "lan") {
  df <- data[[source]]
  df %>%
    filter(!is_correction | is.na(is_correction)) %>%
    group_by(trip_id, vessel_id, species, ices_rect) %>%
    summarise(total_kg = sum(weight_kg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(trip_id, desc(total_kg))
}

#' Haul-by-haul detail from FAR messages
haul_detail <- function(data) {
  data$far %>%
    filter(!is_correction | is.na(is_correction)) %>%
    group_by(trip_id, vessel_id, haul_date, haul_time, lat, lon,
             ices_rect, gear_type, mesh_mm, n_shots, duration_min) %>%
    summarise(total_kg    = sum(weight_kg, na.rm = TRUE),
              n_species   = n_distinct(species),
              species_list = paste(sort(unique(species)), collapse = ", "),
              .groups = "drop") %>%
    arrange(haul_date, haul_time)
}

# =============================================================================
# USAGE EXAMPLE (uncomment to run)
# =============================================================================
# source("parse_ers.R")
#
data <- parse_ers_folder("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch ExportMessagesERS_EVLinden/sorted/CC545762")

# # Trip overview
# View(data$trips)
#
# # Species totals per trip (from landing declarations)
# View(catch_by_species(data, source = "lan"))
#
# # Haul-by-haul detail
# View(haul_detail(data))
#
# # All FAR catches (at-sea, non-corrected)
# View(data$far %>% filter(!is_correction))
#
# # Check corrections — what was corrected and what replaced it
# View(data$pno %>% filter(is_correction))
#
# # Export
# write.csv(data$trips,            "trips.csv",   row.names = FALSE)
# write.csv(catch_by_species(data), "catches.csv", row.names = FALSE)
# =============================================================================
