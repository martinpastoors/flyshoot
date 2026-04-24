# ==============================================================================
# PIPELINE INTEGRATION PATCHES
# ==============================================================================
# This file contains the specific changes to make to the existing pipeline files:
#   01_flyshoot_functions.R   — get_catch_from_kisten() update
#   03_main_workflow.R        — detect_data_source() + treklijst_kisten branch
#   00_setup.R                — kisten RData conversion (haul_id assignment)
#
# Each patch is shown as a FIND/REPLACE with clear delimiters.
# ==============================================================================


# ==============================================================================
# PATCH 1: 01_flyshoot_functions.R
# ==============================================================================
# 
# Change: get_catch_from_kisten() gains a treklijst_path argument.
#         When provided, it calls assign_haul_ids_from_treklijst() to fill
#         haul_id from treklijst shoot times instead of leaving it NA.
#
# FIND this block (the function signature + first few lines):
# ─────────────────────────────────────────────────────────────────────────────
# get_catch_from_kisten <- function(file_path, local_tz = "Europe/Amsterdam") {
# ─────────────────────────────────────────────────────────────────────────────
#
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────

get_catch_from_kisten <- function(file_path,
                                  local_tz       = "Europe/Amsterdam",
                                  treklijst_path = NULL) {

  message(glue("  Processing kisten file: {basename(file_path)}"))

  ids       <- extract_vessel_trip(file_path)
  vessel_id <- ids$vessel
  trip_id   <- ids$trip_id

  if (is.na(vessel_id) || is.na(trip_id)) {
    stop(glue("Could not extract vessel/trip from filename: {basename(file_path)}"))
  }

  # ── 1. Find header row (Marelec prepends metadata before the data table) ──
  preview <- read_excel(file_path, sheet = "1", col_names = FALSE,
                        col_types = "text", n_max = 20)
  skip_n  <- which(apply(preview, 1, function(r)
    any(grepl("lotnummer", r, ignore.case = TRUE))))[1] - 1L
  if (is.na(skip_n)) skip_n <- 5L

  # ── 2. Read data table ────────────────────────────────────────────────────
  raw <- read_excel(file_path, sheet = "1", skip = skip_n,
                    col_names = TRUE, col_types = "text",
                    .name_repair = ~make.names(., unique = TRUE)) %>%
    lowcase() %>%
    filter(!is.na(lotnummer), lotnummer != "")

  message(glue("    Read {nrow(raw)} box records"))

  # ── 3. Apply schema ───────────────────────────────────────────────────────
  catch_data <- apply_schema(raw, marelec_kisten_schema)

  # ── 4. Enrich: datetime, species, size_class ──────────────────────────────
  species_parsed <- parse_marelec_species(catch_data$species_raw)

  catch_data <- catch_data %>%
    mutate(
      vessel    = vessel_id,
      trip_id   = trip_id,
      box_count = 1L,

      weighing_time = as.POSIXct(
        paste(as.character(date), time_hhmm),
        format = "%Y-%m-%d %H:%M:%S",
        tz = local_tz
      ) %>% with_tz("UTC"),

      size_class = suppressWarnings(
        as.integer(trimws(str_remove(size_class,
                                     regex("klasse", ignore_case = TRUE))))
      ),

      species_code    = species_parsed$species_code,
      species_name_nl = species_parsed$species_name_nl,
      species_name_en = species_parsed$species_name_en,
      presentation    = species_parsed$presentation
    )

  # ── 5. AUTO HAUL ASSIGNMENT from treklijst (new) ─────────────────────────
  # Source kisten_haul_assignment.R must be loaded before this function runs.
  # When treklijst_path is supplied, haul_id is assigned automatically.
  # Otherwise haul_id stays NA (raw Marelec file has no haul column).

  if (!is.null(treklijst_path) && !is.na(treklijst_path)) {
    trek_haul_sheet <- read_treklijst_for_kisten(treklijst_path)

    if (!is.null(trek_haul_sheet)) {
      result     <- assign_haul_ids_from_treklijst(
        kisten       = catch_data,
        treklijst    = trek_haul_sheet,
        local_tz     = local_tz
      )
      catch_data <- result$kisten

      # Attach summary as attribute for use by the calling workflow
      attr(catch_data, "haul_assignment_summary") <- result$summary

    } else {
      catch_data <- catch_data %>% mutate(haul_id = NA_integer_)
    }
  } else {
    catch_data <- catch_data %>% mutate(haul_id = NA_integer_)
  }

  # ── 6. Final select ───────────────────────────────────────────────────────
  catch_data <- catch_data %>%
    select(vessel, trip_id, haul_id, weighing_time, date,
           species_code, species_name_nl, species_name_en, presentation,
           size_class, weight_kg, box_count,
           lot_nr,
           any_of(c("session_id", "time_gap_min", "haul_flag")))

  message(glue("    Processed {nrow(catch_data)} box records across ",
               "{n_distinct(catch_data$haul_id, na.rm = TRUE)} hauls"))

  catch_data
}

# ─────────────────────────────────────────────────────────────────────────────
# END PATCH 1
# ─────────────────────────────────────────────────────────────────────────────


# ==============================================================================
# PATCH 2: 03_main_workflow.R  — detect_data_source()
# ==============================================================================
#
# The kisten file now needs to be detected alongside the treklijst so the
# pipeline knows when to run the auto-assignment.  The detection logic
# already handles "treklijst_simplified" (treklijst + kisten) and
# "kisten_pefa" (kisten + pefa).  No change needed in detect_data_source()
# itself — the treklijst_simplified and kisten_pefa branches already pick
# up both files. The change is in how the kisten branch passes the
# treklijst path.
#
# FIND this block in the treklijst_simplified branch:
# ─────────────────────────────────────────────────────────────────────────────
#   } else if (data_source == "treklijst_simplified") {
#     treklijst_file <- trip_files %>% filter(source == "treklijst") %>% pull(file)
#     kisten_file    <- trip_files %>% filter(source == "kisten")    %>% pull(file)
#
#     trip_hauls   <- get_haul_from_treklijst(treklijst_file)
#     trip_catches <- get_catch_from_kisten(kisten_file)
#     trip_info    <- get_trip_info(treklijst_file)
# ─────────────────────────────────────────────────────────────────────────────
#
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────

  } else if (data_source == "treklijst_simplified") {
    treklijst_file <- trip_files %>% filter(source == "treklijst") %>% pull(file)
    kisten_file    <- trip_files %>% filter(source == "kisten")    %>% pull(file)

    trip_hauls   <- get_haul_from_treklijst(treklijst_file)
    local_tz     <- if (!is.null(trip_hauls) && "timezone" %in% names(trip_hauls))
                      first(trip_hauls$timezone) else "Europe/Amsterdam"

    # Pass treklijst path so get_catch_from_kisten() can auto-assign haul IDs
    trip_catches <- get_catch_from_kisten(kisten_file,
                                          local_tz       = local_tz,
                                          treklijst_path = treklijst_file)
    trip_info    <- get_trip_info(treklijst_file)

# ─────────────────────────────────────────────────────────────────────────────
# (Note: the closing brace and next else-if are unchanged)
# ─────────────────────────────────────────────────────────────────────────────
#
# ── Also update the kisten_pefa branch: ──────────────────────────────────────
#
# FIND:
# ─────────────────────────────────────────────────────────────────────────────
#   } else if (data_source == "kisten_pefa") {
#     pefa_file   <- trip_files %>% filter(source == "pefa")   %>% pull(file)
#     kisten_file <- trip_files %>% filter(source == "kisten") %>% pull(file)
#
#     trip_hauls   <- get_haul_from_kisten_pefa(pefa_file, kisten_file)
#     trip_catches <- get_catch_from_kisten(kisten_file)
#     trip_info    <- get_trip_info(pefa_file)
# ─────────────────────────────────────────────────────────────────────────────
#
# REPLACE WITH:
# ─────────────────────────────────────────────────────────────────────────────

  } else if (data_source == "kisten_pefa") {
    pefa_file   <- trip_files %>% filter(source == "pefa")   %>% pull(file)
    kisten_file <- trip_files %>% filter(source == "kisten") %>% pull(file)

    trip_hauls <- get_haul_from_kisten_pefa(pefa_file, kisten_file)

    # If a treklijst is also present for this trip, use it for haul assignment
    trek_file <- if ("treklijst" %in% trip_files$source)
                   trip_files %>% filter(source == "treklijst") %>% pull(file)
                 else NULL

    trip_catches <- get_catch_from_kisten(kisten_file,
                                          treklijst_path = trek_file)
    trip_info    <- get_trip_info(pefa_file)

    if ("elog" %in% sources) {
      elog_file <- trip_files %>% filter(source == "elog") %>% pull(file)
      elog_result <- get_data_from_elog(elog_file)
      trip_info   <- elog_result$trip
    }

# ─────────────────────────────────────────────────────────────────────────────
# END PATCH 2
# ─────────────────────────────────────────────────────────────────────────────


# ==============================================================================
# PATCH 3: 00_setup.R — kisten RData → parquet conversion
# ==============================================================================
#
# The one-time migration path also processes kisten from legacy RData.
# Those files already have a haul column (from prior manual processing).
# No algorithmic change needed there — the haul_id is used as-is.
#
# However, add a note confirming that for NEW kisten files processed through
# the live pipeline (via get_catch_from_kisten), the haul_id comes from
# assign_haul_ids_from_treklijst, not from the raw file.
# No code change needed in 00_setup.R.
# ─────────────────────────────────────────────────────────────────────────────


# ==============================================================================
# PATCH 4: 00_setup.R or 03_main_workflow.R — source the new module
# ==============================================================================
#
# Add this line near the top of whichever file sources the other R modules,
# immediately after the line that sources 01_flyshoot_functions.R:
# ─────────────────────────────────────────────────────────────────────────────

source("kisten_haul_assignment.R")   # auto haul-ID assignment for kisten files

# ─────────────────────────────────────────────────────────────────────────────
# END PATCH 4
# ─────────────────────────────────────────────────────────────────────────────


# ==============================================================================
# PATCH 5: file_patterns in config  (config.json / 00_setup.R)
# ==============================================================================
#
# The file detection uses pattern matching on filenames.  The kisten pattern
# already exists.  No change needed.  For reference, the relevant section:
#
#   file_patterns = list(
#     treklijst = "treklijst",
#     kisten    = "kisten",          # ← already present
#     pefa      = "elog pefa",
#     pefa_trek = "elog_pefa_per_trek",
#     mcatch    = "elog mcatch"
#   )
#
# The filename convention for the example file is:
#   SCH99_2026_283_kisten-reis-283-trek-26.xlsx
# which contains "kisten" → correctly detected.
# ─────────────────────────────────────────────────────────────────────────────
