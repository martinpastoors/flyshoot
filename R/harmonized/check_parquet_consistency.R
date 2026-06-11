# ==============================================================================
# check_parquet_consistency()
# ------------------------------------------------------------------------------
# Validates all flyshoot parquet files for:
#   1. Required columns present (per Poseidat / pipeline schema)
#   2. No unexpected column names (typos, legacy names)
#   3. Correct column types
#   4. Cross-file referential consistency
#      - every trip_id in haul/elog/elog_trek exists in trip
#      - every vessel in haul/elog/elog_trek exists in trip
#   5. No duplicate primary keys
#
# Usage:
#   source(file.path(here(), "R/harmonized/01_flyshoot_functions.R"))
#   check_parquet_consistency()                     # uses default paths
#   check_parquet_consistency(parquet_dir = "path") # custom dir
#   check_parquet_consistency(stop_on_error = TRUE) # hard stop on failure
#
# Returns invisibly: a list of all issues found (length 0 = all good).
# ==============================================================================

check_parquet_consistency <- function(
    parquet_dir    = file.path(
      "C:/Users/MartinPastoors/Martin Pastoors",
      "FLYSHOOT - General/data/raw"
    ),
    stop_on_error  = FALSE,
    verbose        = TRUE
) {

  library(arrow)
  library(dplyr)
  library(purrr)
  library(glue)

  issues <- list()   # accumulate all problems here

  # ── helpers ────────────────────────────────────────────────────────────────

  ok  <- function(msg) if (verbose) message("  \u2713 ", msg)
  warn <- function(key, msg) {
    issues[[key]] <<- msg
    message("  \u26a0  ", msg)
  }
  section <- function(title) {
    if (verbose) message("\n", strrep("\u2500", 60), "\n  ", title)
  }

  # ── schemas ─────────────────────────────────────────────────────────────────
  # Column specs per parquet type.
  # Format: list(required = c(...), optional = c(...), types = list(col = "type"))
  # Types use arrow/R terminology: "character", "double", "integer",
  # "Date", "POSIXct", "logical"

  # Metadata columns added by save_flyshoot_data() to every parquet.
  # Listed here once and appended to every schema's optional list below.
  META_COLS <- c("save_date", "save_timestamp")

  schemas <- list(

    trip = list(
      required = c("vessel", "trip_id", "departure_date", "arrival_date"),
      optional = c(
        # ports & people
        "departure_port", "arrival_port", "landing_port",
        "auction_port", "auction_date",
        "captain", "skipper", "vessel_nr",
        # trip identifiers
        "trip_nr", "trip_nr_elog", "trip_type", "trip_status",
        # gear
        "gear_type", "mesh_size_mm", "gears_declared",
        # FAR / effort summaries
        "n_hauls", "far_regime", "far_kg_total",
        "n_far_msgs", "avg_shots_per_far",
        # spatial / temporal
        "timezone", "fishing_areas", "total_distance",
        "data_source",
        META_COLS
      ),
      types = list(
        vessel         = "character",
        trip_id        = "character",
        departure_date = "Date",
        arrival_date   = "Date",
        n_hauls        = "integer"
      ),
      pk = c("vessel", "trip_id")
    ),

    haul = list(
      required = c(
        "vessel", "trip_id", "haul_id", "date",
        "shoot_lat", "shoot_lon"
      ),
      optional = c(
        # haul geometry
        "haul_lat", "haul_lon",
        "shoot_time", "haul_time",
        "shoot_end_time", "next_haul_time",
        "fishing_time_hours",
        # gear
        "gear_type", "mesh_size", "mesh_size_mm",
        "vertical_opening_m", "cable_length_m",
        "cable_thickness_mm", "groundrope_length_m",
        "escape_panel",
        # environment
        "wind_direction", "wind_force_bft", "water_depth",
        # spatial zones
        "area", "division", "rect", "economiczone",
        "fao_area", "fao_subarea", "fao_division",
        "ices_rect", "ices_area", "economic_zone",
        # trip context carried on haul
        "trip_nr", "skipper",
        "departure_date", "departure_port",
        "arrival_date", "arrival_port",
        "auction_date", "auction_port",
        "timezone", "trip_status",
        # catch summaries (haul-level aggregates, not per-species rows)
        "total_catch_kg", "marketable_catch_kg",
        "n_species", "n_shots",
        # admin
        "comments", "data_source",
        META_COLS
      ),
      types = list(
        vessel             = "character",
        trip_id            = "character",
        haul_id            = "character",
        date               = "Date",
        shoot_lat          = "double",
        shoot_lon          = "double",
        fishing_time_hours = "double"
      ),
      pk = c("vessel", "trip_id", "haul_id")
    ),

    elog = list(
      required = c(
        "vessel", "trip_id", "haul_id", "date",
        "species_code", "weight_kg"
      ),
      optional = c(
        # catch detail
        "presentation", "preservation", "freshness",
        "size_category", "conversion_factor",
        "box_count", "box_number", "discard_reason",
        # position (elog sometimes carries shoot position)
        "shoot_lat", "shoot_lon",
        # spatial zones
        "fao_division", "ices_rect", "economic_zone",
        # gear
        "gear_type", "mesh_size_mm",
        # trip context
        "trip_nr", "trip_nr_elog", "trip_status",
        "skipper", "vessel_nr",
        "departure_date", "departure_port",
        "arrival_date", "arrival_port",
        "landing_date", "landing_port",
        "auction_date", "auction_port",
        "data_source",
        META_COLS
      ),
      types = list(
        vessel       = "character",
        trip_id      = "character",
        haul_id      = "character",
        date         = "Date",
        species_code = "character",
        weight_kg    = "double"
      ),
      pk = c("vessel", "trip_id", "haul_id", "species_code")
    ),

    elog_trek = list(
      required = c(
        "vessel", "trip_id", "haul_id", "date",
        "shoot_lat", "shoot_lon",
        "species_code", "weight_kg"
      ),
      optional = c(
        # haul geometry
        "haul_lat", "haul_lon",
        "shoot_time", "haul_time",
        "fishing_time_hours",
        # gear / spatial
        "mesh_size", "mesh_size_mm", "water_depth", "gear_type",
        "area", "division", "rect", "economiczone",
        "fao_division", "ices_rect", "economic_zone",
        # catch detail
        "box_count", "box_number",
        "presentation", "preservation", "freshness",
        "size_category", "size_class", "conversion_factor",
        "loss_grams", "boxes_undersized", "discard_reason",
        # weighing / auction detail (turbocatch)
        "weighing_time", "lot_nr", "session_id",
        "time_gap_min", "haul_flag",
        "species_name_nl", "species_name_en",
        # trip context
        "trip_nr", "trip_nr_elog", "trip_status",
        "skipper", "vessel_nr",
        "departure_date", "departure_port",
        "arrival_date", "arrival_port",
        "auction_date", "auction_port",
        "data_source",
        META_COLS
      ),
      types = list(
        vessel             = "character",
        trip_id            = "character",
        haul_id            = "character",
        date               = "Date",
        shoot_lat          = "double",
        shoot_lon          = "double",
        species_code       = "character",
        weight_kg          = "double",
        fishing_time_hours = "double"
      ),
      # size_category added to PK because one haul+species can have
      # both "legal" and "undersized" rows
      pk = c("vessel", "trip_id", "haul_id", "species_code", "size_category")
    ),

    kisten = list(
      required = c(
        "vessel", "trip_id", "haul_id",
        "species_code", "weight_kg"
      ),
      optional = c(
        "date",
        # lot / box detail
        "lot_nr", "box_number", "box_count", "time_hhmm",
        "size_category", "size_class", "presentation",
        # species labels
        "species_name_nl", "species_name_en",
        # weighing / auction (turbocatch)
        "weighing_time", "session_id",
        "time_gap_min", "haul_flag",
        "auction_date", "landing_date",
        "data_source",
        META_COLS
      ),
      types = list(
        vessel       = "character",
        trip_id      = "character",
        haul_id      = "character",
        species_code = "character",
        weight_kg    = "double"
      ),
      # lot_nr added to PK: one haul+species can have multiple lots
      pk = c("vessel", "trip_id", "haul_id", "species_code", "lot_nr")
    ),

    vessel_movement = list(
      required = c(
        "vessel", "date",
        "lat", "lon"           # actual column names in the parquet
      ),
      optional = c(
        "trip_id", "trip_nr",
        "event_type",          # e.g. "DEP", "ARR", "fishing"
        "haul_id",
        "port",
        "speed", "heading",
        "distance", "source",
        "data_source",
        META_COLS
      ),
      types = list(
        vessel = "character",
        date   = "Date",
        lat    = "double",
        lon    = "double"
      ),
      pk = c("vessel", "date", "lat", "lon")
    )
  )

  # ── load parquet files ──────────────────────────────────────────────────────
  section("LOADING PARQUET FILES")

  # Find parquet for each type.
  # The pipeline stores files as:
  #   {parquet_dir}/{type}/{type}.parquet   (current, fixed-name)
  # with possible fallback to dated names:
  #   {parquet_dir}/{type}/{type}_*.parquet (legacy)
  # Also supports a flat layout: {parquet_dir}/{type}.parquet
  find_parquet <- function(type) {
    candidates <- c(
      # preferred: subfolder with fixed name
      file.path(parquet_dir, type, paste0(type, ".parquet")),
      # flat layout (e.g. turbocatch output dir)
      file.path(parquet_dir, paste0(type, ".parquet"))
    )
    # also pick up any dated variants inside the subfolder
    dated <- list.files(
      file.path(parquet_dir, type),
      pattern = paste0("^", type, ".*\\.parquet$"),
      full.names = TRUE
    )
    all_files <- c(candidates, dated)
    all_files <- all_files[file.exists(all_files)]
    if (length(all_files) == 0) return(NULL)
    # prefer the fixed-name file if present, otherwise most recently modified
    fixed <- all_files[basename(all_files) == paste0(type, ".parquet")]
    if (length(fixed) > 0) return(fixed[1])
    all_files[which.max(file.mtime(all_files))]
  }

  loaded <- list()
  for (type in names(schemas)) {
    path <- find_parquet(type)
    if (is.null(path)) {
      warn(glue("missing_file_{type}"),
           glue("{type}.parquet not found in {parquet_dir}"))
      loaded[[type]] <- NULL
    } else {
      loaded[[type]] <- tryCatch(
        arrow::read_parquet(path),
        error = function(e) {
          warn(glue("read_error_{type}"),
               glue("Could not read {basename(path)}: {e$message}"))
          NULL
        }
      )
      if (!is.null(loaded[[type]]))
        ok(glue("{type}: {basename(path)}  ({nrow(loaded[[type]])} rows, {ncol(loaded[[type]])} cols)"))
    }
  }

  # ── per-file checks ─────────────────────────────────────────────────────────
  for (type in names(schemas)) {
    df <- loaded[[type]]
    if (is.null(df)) next

    schema <- schemas[[type]]
    section(glue("CHECKING  {toupper(type)}"))
    cols <- names(df)

    # 1. required columns
    missing_req <- setdiff(schema$required, cols)
    if (length(missing_req)) {
      warn(glue("{type}_missing_required"),
           glue("{type}: missing required columns: {paste(missing_req, collapse=', ')}"))
    } else {
      ok(glue("All {length(schema$required)} required columns present"))
    }

    # 2. unexpected columns (not in required + optional)
    known <- c(schema$required, schema$optional)
    unexpected <- setdiff(cols, known)
    if (length(unexpected)) {
      warn(glue("{type}_unexpected_cols"),
           glue("{type}: unexpected columns (check for typos/legacy names): {paste(unexpected, collapse=', ')}"))
    } else {
      ok("No unexpected column names")
    }

    # 3. column types
    type_issues <- character(0)
    for (col in intersect(names(schema$types), cols)) {
      expected_type <- schema$types[[col]]
      actual_type   <- class(df[[col]])[1]
      # map arrow/R types to comparable labels
      compat <- switch(expected_type,
        "character" = actual_type %in% c("character"),
        "double"    = actual_type %in% c("numeric", "double"),
        "integer"   = actual_type %in% c("integer", "numeric"),
        "Date"      = actual_type %in% c("Date"),
        "POSIXct"   = actual_type %in% c("POSIXct", "POSIXt"),
        "logical"   = actual_type %in% c("logical"),
        TRUE
      )
      if (!compat)
        type_issues <- c(type_issues,
          glue("{col}: expected {expected_type}, got {actual_type}"))
    }
    if (length(type_issues)) {
      warn(glue("{type}_type_mismatch"),
           glue("{type} column type issues:\n    {paste(type_issues, collapse='\n    ')}"))
    } else {
      ok("Column types correct")
    }

    # 4. duplicate primary keys
    pk_cols <- intersect(schema$pk, cols)
    if (length(pk_cols) == length(schema$pk)) {
      dupes <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(pk_cols))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup()
      if (nrow(dupes)) {
        warn(glue("{type}_duplicate_pk"),
             glue("{type}: {nrow(dupes)} rows with duplicate primary key ({paste(pk_cols, collapse=', ')})"))
      } else {
        ok(glue("No duplicate primary keys ({paste(pk_cols, collapse=' + ')})"))
      }
    }

    # 5. NA check on required columns
    for (col in intersect(schema$required, cols)) {
      n_na <- sum(is.na(df[[col]]))
      if (n_na > 0)
        warn(glue("{type}_na_{col}"),
             glue("{type}: {n_na} NA values in required column '{col}'"))
    }
    n_req_with_nas <- sum(purrr::map_int(intersect(schema$required, cols),
                                  ~sum(is.na(df[[.x]]))) > 0)
    if (n_req_with_nas == 0)
      ok("No NAs in required columns")

  } # end per-file loop

  # ── cross-file consistency ──────────────────────────────────────────────────
  section("CROSS-FILE CONSISTENCY")

  trip <- loaded$trip
  if (!is.null(trip) && "trip_id" %in% names(trip)) {

    valid_trips   <- unique(trip$trip_id)
    valid_vessels <- if ("vessel" %in% names(trip)) unique(trip$vessel) else NULL

    for (type in c("haul", "elog", "elog_trek", "kisten")) {
      df <- loaded[[type]]
      if (is.null(df)) next

      # trip_id referential integrity
      if ("trip_id" %in% names(df)) {
        orphan_trips <- setdiff(unique(df$trip_id), valid_trips)
        if (length(orphan_trips)) {
          warn(glue("{type}_orphan_trip_id"),
               glue("{type}: {length(orphan_trips)} trip_id(s) not found in trip.parquet: ",
                    "{paste(head(orphan_trips, 5), collapse=', ')}",
                    if (length(orphan_trips) > 5) glue(" ... (+{length(orphan_trips)-5} more)") else ""))
        } else {
          ok(glue("{type}: all trip_ids present in trip.parquet"))
        }
      }

      # vessel referential integrity
      if (!is.null(valid_vessels) && "vessel" %in% names(df)) {
        orphan_vessels <- setdiff(unique(df$vessel), valid_vessels)
        if (length(orphan_vessels)) {
          warn(glue("{type}_orphan_vessel"),
               glue("{type}: vessels not found in trip.parquet: ",
                    "{paste(orphan_vessels, collapse=', ')}"))
        } else {
          ok(glue("{type}: all vessels present in trip.parquet"))
        }
      }
    }
  }

  # haul <-> elog haul_id consistency
  haul <- loaded$haul
  elog <- loaded$elog
  if (!is.null(haul) && !is.null(elog) &&
      "haul_id" %in% names(haul) && "haul_id" %in% names(elog)) {

    haul_keys  <- unique(paste(haul$vessel, haul$trip_id, haul$haul_id))
    elog_keys  <- unique(paste(elog$vessel, elog$trip_id, elog$haul_id))

    elog_orphans <- setdiff(elog_keys, haul_keys)
    if (length(elog_orphans)) {
      warn("elog_haul_mismatch",
           glue("elog: {length(elog_orphans)} haul_id combinations not found in haul.parquet: ",
                "{paste(head(elog_orphans, 3), collapse=', ')}",
                if (length(elog_orphans) > 3) glue(" ... (+{length(elog_orphans)-3} more)") else ""))
    } else {
      ok("elog: all haul_ids matched in haul.parquet")
    }
  }

  # haul <-> kisten haul_id consistency
  kisten <- loaded$kisten
  if (!is.null(haul) && !is.null(kisten) &&
      "haul_id" %in% names(haul) && "haul_id" %in% names(kisten)) {

    haul_keys   <- unique(paste(haul$vessel, haul$trip_id, haul$haul_id))
    kisten_keys <- unique(paste(kisten$vessel, kisten$trip_id, kisten$haul_id))

    kisten_orphans <- setdiff(kisten_keys, haul_keys)
    if (length(kisten_orphans)) {
      warn("kisten_haul_mismatch",
           glue("kisten: {length(kisten_orphans)} haul_id combinations not found in haul.parquet: ",
                "{paste(head(kisten_orphans, 3), collapse=', ')}",
                if (length(kisten_orphans) > 3) glue(" ... (+{length(kisten_orphans)-3} more)") else ""))
    } else {
      ok("kisten: all haul_ids matched in haul.parquet")
    }
  }

  # ── summary ─────────────────────────────────────────────────────────────────
  section("SUMMARY")
  n_issues <- length(issues)
  if (n_issues == 0) {
    message("\n  \u2705  All checks passed. Parquet files are consistent.\n")
  } else {
    message(glue("\n  \u274c  {n_issues} issue(s) found:\n"))
    for (i in seq_along(issues)) {
      message(glue("  [{i}] {issues[[i]]}"))
    }
    message()
    if (stop_on_error)
      stop(glue("{n_issues} consistency issue(s) found — see messages above."))
  }

  invisible(issues)
}
