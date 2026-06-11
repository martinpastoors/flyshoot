# ==============================================================================
# FLYSHOOT Processing Functions
# ==============================================================================
# Core processing functions following Poseidat conventions
# Handles: manual treklijst, PEFA, kisten, elog data
# Last updated: 2026-04-19 — time-based haul assignment, kisten_haul_assignment.R retired
message(paste0("01_flyshoot_functions.R loaded — saved: ",
               format(file.mtime(file.path(here::here(), "R/harmonized", "01_flyshoot_functions.R")),
                      "%Y-%m-%d %H:%M:%S")))
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(glue)

# ==============================================================================
# HELPER: lowcase — normalize column names to snake_case
# ==============================================================================

lowcase <- function(df) {
  names(df) <- gsub("[[:space:]]+", "", names(df))        # remove whitespace/newlines
  names(df) <- gsub("[^[:alnum:]_]", "", names(df))       # remove special chars
  names(df) <- tolower(names(df))
  df
}

# lowcase <- function(df) {
#   names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|_+|\\(|\\)","",.) 
#   df
# }

# ==============================================================================
# SCHEMA: Column definitions for the Haul sheet
#
# Maps the lowcased/cleaned Excel column names to:
#   - poseidat_name : standardized output column name
#   - target_type   : desired R type after coercion
#   - excel_format  : how Excel stores this (for ambiguous cases)
#
# target_type values:
#   "character"     - keep as text
#   "integer"       - as.integer()
#   "double"        - as.numeric()
#   "date"          - Excel serial number or date string -> Date
#   "time_hhmm"     - Time entered as integer HHMM (e.g. 735 = 07:35)
#   "datetime"      - Excel datetime serial or datetime string -> POSIXct
#   "proportion"    - Excel percentage stored as 0-1 fraction -> numeric
#   "logical_yn"    - Y/N text -> logical
# ==============================================================================

treklijst_haul_schema <- tribble(
  ~excel_col,                        ~poseidat_name,          ~target_type,
  
  # --- Identifiers (trip-level, repeated per row) ---
  "vessel",                          "vessel",                "character",
  "reis",                            "trip_nr",               "character",
  "skipper",                         "skipper",               "character",
  "dateembarkedutcdate",             "departure_date",        "date",
  "portofembarkation",               "departure_port",        "character",
  "datedisembarkedutcdate",          "arrival_date",          "date",
  "portofdisembarkation",            "arrival_port",          "character",
  "trip",                            "trip_id",               "character",
  "timezone",                        "timezone",              "character",
  
  # --- Haul identifiers ---
  "haul",                            "record_nr",             "integer",
  "date",                            "date",                  "date",
  
  # --- Times (entered as HHMM integers — used only to build shoot_time/haul_time
  #     via the berekend formula columns below; dropped from final output) ---
  "tijdbeginuitzetten",              "shoot_time_hhmm",       "time_hhmm",
  "tijdeindeuitzetten",              "shoot_end_time_hhmm",   "time_hhmm",
  "tijdeindehalen",                  "haul_time_hhmm",        "time_hhmm",
  
  # --- Calculated times (Excel datetimes from formulas) ---
  "berekentijdbeginuitzetten",       "shoot_time",            "datetime",
  "berekentijdeindeuitzetten",       "shoot_end_time",        "datetime",
  "berekentijdeindehalen",           "haul_time",             "datetime",
  
  # --- Positions (entered as DDMM integers — used only as fallback to compute
  #     shoot_lat/shoot_lon when plot_lat/plot_lon are absent; dropped from
  #     final output after decimal-degree conversion) ---
  "shootlat",                        "shoot_lat_ddmm",        "integer",
  "shootns",                         "shoot_ns",              "character",
  "shootlong",                       "shoot_lon_dddmm",       "integer",
  "shootew",                         "shoot_ew",              "character",
  
  # --- Calculated positions (decimal degrees from formulas) ---
  "plot_lat",                        "shoot_lat",             "double",
  "plot_lon",                        "shoot_lon",             "double",
  
  # --- Environmental ---
  "waterdepth",                      "water_depth",           "integer",
  "winddirection",                   "wind_direction",        "character",
  "windforcebft",                    "wind_force_bft",        "integer",
  
  # --- Catch ---
  "totalevangstkgberekend",          "total_catch_kg",        "double",
  "marktwaardigeviskg",              "marketable_catch_kg",   "double",
  # catch_height_cm, box_type, photo_box, bycatch_pct removed — not used downstream
  
  # --- Gear ---
  "gear",                            "gear_type",             "character",
  "meshsize",                        "mesh_size_mm",          "integer",
  "vertopening",                     "vertical_opening_m",    "integer",
  "cablelengthm",                    "cable_length_m",        "integer",
  "cablethicknessmm",                "cable_thickness_mm",    "integer",
  "lengthgroundropem",               "groundrope_length_m",   "double",
  "escapepanelyn",                   "escape_panel",          "logical_yn",
  
  # --- Spatial (calculated by Excel formula — dropped post-schema in favour
  #     of add_spatial_attributes() which is the consistent source of truth) ---
  "rect",                            "ices_rect",             "character",
  "area",                            "ices_area",             "character",
  
  # --- Other ---
  "comments",                        "comments",              "character"
)

# ==============================================================================
# SCHEMA: Column definitions for the Marelec kisten file
#
# Marelec box-weighing systems export one row per box with a variable-length
# header block before the data table. The data table starts at the row where
# column 1 contains "Lotnummer".
#
# Excel column names (lowcased) -> poseidat_name -> target_type
# ==============================================================================

marelec_kisten_schema <- tribble(
  ~excel_col,    ~poseidat_name,   ~target_type,
  "lotnummer",   "lot_nr",         "integer",
  "datum",       "date",           "date",
  "tijd",        "time_hhmm",      "time_hhmmss",  # Marelec text "HH:MM:SS"
  "time",        "time_hhmm",      "time_hhmmss",  # English variant
  "tijdstip",    "time_hhmm",      "time_hhmmss",  # Dutch variant
  "weegmoment",  "time_hhmm",      "time_hhmmss",  # "weighing moment"
  "soorten",     "species_raw",    "character",    # Dutch: parsed into species_code + presentation
  "species",     "species_raw",    "character",    # English variant
  "maat",        "size_class",     "character",    # Dutch: "KLASSE 1"
  "size",        "size_class",     "character",    # English variant
  "gewicht",     "weight_kg",      "weight_kg",    # "20.3 kg" -> strip unit -> double
  "haul",        "haul_id",        "integer"       # pre-assigned haul_id from manually processed files
)
# ==============================================================================

#' Parse a single Excel text value to its target type
#' @param x character vector (from read_excel with col_types = "text")
#' @param target_type one of the schema target types
#' @return coerced vector of appropriate type
coerce_column <- function(x, target_type) {
  
  # Treat Excel error values as NA
  x <- ifelse(x %in% c("#N/A", "#REF!", "#VALUE!", "#DIV/0!", "#NAME?", ""), 
              NA_character_, x)
  
  switch(target_type,
         
         "character" = x,
         
         "integer" = as.integer(suppressWarnings(as.numeric(x))),
         
         "double" = suppressWarnings(as.numeric(x)),
         
         "weight_kg" = {
           # Marelec exports weight as "20.3 kg" — strip the unit suffix
           suppressWarnings(as.numeric(trimws(gsub("kg", "", x, ignore.case = TRUE))))
         },
         
         "date" = {
           # Three possible formats coming from read_excel with col_types="text":
           #   1. Excel serial number (numeric string): "46000"     -> origin 1899-12-30
           #   2. ISO date string:                      "2026-03-19"
           #   3. Marelec DD/MM/YYYY text string:       "19/03/2026"
           #
           # IMPORTANT: bare as.Date() without a format argument must NOT be used
           # on ambiguous strings. "16/03/2026" matches R's %Y/%m/%d trial,
           # reading year=16 and truncating "2026" to day=20 -> "0016-03-20".
           # Instead, detect the format explicitly from the string pattern.
           num <- suppressWarnings(as.numeric(x))

           # Branch 1: numeric serial number
           from_serial <- suppressWarnings(
             as.Date(ifelse(!is.na(num), num, NA_real_),
                     origin = "1899-12-30")
           )

           # Branch 2: ISO format "YYYY-MM-DD" — only applied to strings that
           # actually match the pattern, never to DD/MM/YYYY strings
           is_iso   <- !is.na(x) & is.na(num) &
                       grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(x))
           from_iso <- suppressWarnings(
             as.Date(ifelse(is_iso, x, NA_character_), format = "%Y-%m-%d")
           )

           # Branch 3: Marelec "DD/MM/YYYY" — only applied to strings matching
           # that exact pattern
           is_dmy   <- !is.na(x) & is.na(num) &
                       grepl("^\\d{2}/\\d{2}/\\d{4}$", trimws(x))
           from_dmy <- suppressWarnings(
             as.Date(ifelse(is_dmy, x, NA_character_), format = "%d/%m/%Y")
           )

           dplyr::coalesce(from_serial, from_iso, from_dmy)
         },
         
         "time_hhmm" = {
           # Times entered as integers: 735 -> 07:35, 1015 -> 10:15, 850 -> 08:50
           num <- suppressWarnings(as.integer(as.numeric(x)))
           hours   <- num %/% 100L
           minutes <- num %% 100L
           ifelse(!is.na(num), 
                  sprintf("%02d:%02d", hours, minutes),
                  NA_character_)
         },
         
         "time_hhmmss" = {
           # Marelec exports time as plain text "HH:MM:SS".
           # Validate the format and pass through; non-matching values -> NA.
           ifelse(grepl("^\\d{2}:\\d{2}:\\d{2}$", trimws(x)),
                  trimws(x),
                  NA_character_)
         },
         
         "datetime" = {
           # Excel stores datetimes as serial numbers (days + fraction since 1899-12-30).
           # A serial of exactly 0 means an empty/zero cell — treat as NA.
           # Fractions between 0 and 1 are TIME-ONLY values (no date component);
           # these are left as-is here (yielding a 1899-12-30 placeholder) and
           # corrected post-schema in get_haul_from_treklijst() by combining with
           # the date column from the same row.
           # NOTE: values are in the vessel's LOCAL time; UTC conversion is applied
           # afterwards in get_haul_from_treklijst() using the 'timezone' column.
           #
           # We avoid ifelse() here because it evaluates both branches on the full
           # vector, causing as.POSIXct() to choke on serial-number strings in the
           # text-parse branch, and vice versa.
           num <- suppressWarnings(as.numeric(x))
           num[!is.na(num) & num == 0] <- NA_real_
           
           result <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
           
           # Branch 1: numeric serial (full datetime or time-only fraction) -> POSIXct
           is_num <- !is.na(num)
           if (any(is_num)) {
             result[is_num] <- as.POSIXct(
               num[is_num] * 86400, origin = "1899-12-30", tz = "UTC"
             )
           }
           
           # Branch 2: text string -> POSIXct (try common Excel export formats)
           is_txt <- !is_num & !is.na(x)
           if (any(is_txt)) {
             parsed <- suppressWarnings(
               lubridate::parse_date_time(
                 x[is_txt],
                 orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM", "ymd"),
                 tz = "UTC", quiet = TRUE
               )
             )
             result[is_txt] <- parsed
           }
           
           result
         },
         
         "proportion" = {
           # Percentages stored as 0-1 proportions; convert to 0-100 
           num <- suppressWarnings(as.numeric(x))
           ifelse(!is.na(num) & num <= 1, num * 100, num)
         },
         
         "logical_yn" = {
           toupper(str_trim(x)) == "Y"
         },
         
         # fallback
         x
  )
}

# df <- raw; schema <- treklijst_haul_schema

#' Apply schema coercion to an entire dataframe read as text
#' @param df dataframe with all character columns (from read_excel col_types="text")
#' @param schema the treklijst_haul_schema tribble
#' @return dataframe with renamed and typed columns
apply_schema <- function(df, schema) {
  
  available_cols <- names(df)
  matched   <- character(0)
  unmatched <- character(0)
  
  result <- tibble(.rows = nrow(df))

  # i <- 1  
  for (i in seq_len(nrow(schema))) {
    ecol <- schema$excel_col[i]
    pname <- schema$poseidat_name[i]
    ttype <- schema$target_type[i]
    
    if (ecol %in% available_cols) {
      result[[pname]] <- coerce_column(df[[ecol]], ttype)
      matched <- c(matched, glue("{ecol} -> {pname} ({ttype})"))
    } else {
      unmatched <- c(unmatched, ecol)
    }
  }
  
  # Report unmapped Excel columns (present in file but not in schema)
  extra_cols <- setdiff(available_cols, schema$excel_col)
  
  message(glue("    Schema mapping: {length(matched)} matched, ",
               "{length(unmatched)} schema cols missing from file, ",
               "{length(extra_cols)} extra cols in file"))
  
  if (length(extra_cols) > 0) {
    message(glue("    Extra (unmapped) columns: {paste(extra_cols, collapse=', ')}"))
  }
  if (length(unmatched) > 0) {
    message(glue("    Missing from file: {paste(unmatched, collapse=', ')}"))
  }
  
  result
}

# ==============================================================================
# POSITION HELPERS: Convert DDMM integer positions to decimal degrees
# ==============================================================================

#' Convert DDMM integer (e.g. 5039) to decimal degrees (50.65)
ddmm_to_decimal <- function(ddmm, hemisphere = "N") {
  ddmm <- as.integer(ddmm)
  degrees <- ddmm %/% 100L
  minutes <- ddmm %% 100L
  dd <- degrees + minutes / 60
  
  # Apply sign for S/W
  sign <- ifelse(toupper(hemisphere) %in% c("S", "W"), -1, 1)
  dd * sign
}

# ==============================================================================
# TIME IMPUTATION HELPER
# ==============================================================================

#' Impute missing times based on a reference time and offset
#' @param target the time to impute if NA
#' @param reference the known time to derive from
#' @param offset_minutes offset in minutes (positive = after reference)
#' @return POSIXct vector with imputed values where target was NA
impute_time <- function(target, reference, offset_minutes) {
  as.POSIXct(
    ifelse(is.na(target) & !is.na(reference),
           reference + offset_minutes * 60,
           target),
    origin = "1970-01-01", tz = "UTC"
  )
}

# ==============================================================================
# TIMEZONE RESOLVER
# ==============================================================================

#' Resolve a timezone string to a valid IANA timezone name.
#' Handles both proper IANA names ("Europe/Amsterdam") and fixed-offset
#' strings ("UTC+1", "UTC+2"). Returns "UTC" with a warning if unrecognised.
#' Note: POSIX Etc/GMT sign convention is opposite to common usage
#' (Etc/GMT-1 = UTC+1), so the sign is flipped when building Etc/GMT zones.
#' @param tz_str character string timezone
#' @return valid IANA timezone string
resolve_tz <- function(tz_str) {
  tz_str <- trimws(tz_str)
  if (tz_str %in% OlsonNames()) return(tz_str)
  m <- regmatches(tz_str, regexpr("UTC([+-])(\\d{1,2})", tz_str, perl = TRUE))
  if (length(m) == 1) {
    parts    <- regmatches(m, regexec("UTC([+-])(\\d{1,2})", m, perl = TRUE))[[1]]
    sign     <- parts[2]
    hours    <- as.integer(parts[3])
    etc_sign <- if (sign == "+") "-" else "+"
    etc_zone <- paste0("Etc/GMT", etc_sign, hours)
    if (etc_zone %in% OlsonNames()) return(etc_zone)
  }
  warning(glue("Unrecognised timezone '{tz_str}', defaulting to UTC"))
  "UTC"
}

# ==============================================================================
# MARELEC SPECIES PARSER
# ==============================================================================
#
# Marelec species strings use mixed separators and mixed case. Three patterns:
#   "DUTCH NAME (CODE)"    e.g. "WIJTING (WHG)", "MUL (MUR)"
#   "Dutch name/CODE"      e.g. "Wijting/WHG", "Kabeljauw/COD"
#   "DUTCH NAME"           e.g. "WIJTING", "KABELJAUW" (no code)
#
# CODE may be a 2-4 letter FAO species code OR a presentation code (e.g. DICHT).
# We resolve using a canonical lookup table built from all observed species strings.
# ==============================================================================

# Presentation codes: internal Marelec processing/state codes, not FAO species
MARELEC_PRESENTATION_CODES <- c(
  "DICHT",  # whole / ongeopend
  "OPEN",   # open
  "GUT",    # gutted
  "HGT",    # gutted (variant)
  "FILET",  # fillet
  "IK",     # iced
  "VERS",   # fresh
  "SS"      # sub-standard / second sort (e.g. "BOT ss")
)

# Non-standard codes that appear in Marelec exports but are not valid FAO codes.
# These are remapped to the correct FAO code before lookup.
MARELEC_CODE_REMAP <- c(
  "ZON" = "JOD",   # Zonnevis/ZON -> John Dory (JOD)
  "JAX" = "HOM",   # Horsmakreel (JAX) -> Atlantic horse mackerel (HOM)
  "SQC" = "SQR"    # Inktvis (SQC) -> common squid (SQR), unless you want nei
)

# Canonical lookup: normalised Dutch name -> FAO 3-letter code + English name
# Built from all observed species strings in kisten_existing.
# Key = uppercased Dutch name (spaces normalised); value = FAO code + EN name.
MARELEC_SPECIES_LOOKUP <- tribble(
  ~dutch_name_upper,          ~fao_code,  ~species_name_en,
  "BLONDE ROG",               "RJH",      "Blonde ray",
  "BOT",                      "FLE",      "European flounder",
  "CONGERAAL",                "COE",      "European conger",
  "DIKLIP HARDER",            "MLR",      "Thicklip grey mullet",
  "DIKLIPHARDER",             "MLR",      "Thicklip grey mullet",
  "DIVERSEN",                 "DPX",      "Miscellaneous",
  "DORADE",                   "BRB",      "Bogue",
  "DORADE ROYAL",             "SBR",      "Red seabream",
  "DUNLIP HARDER",            "MGC",      "Thinlip grey mullet",
  "DUNLIPHARDER",             "MGC",      "Thinlip grey mullet",
  "ENGELSE POON",             "GUR",      "Grey gurnard",
  "FRANSE POON",              "GUR",      "Grey gurnard",
  "GEWONE PIJLINKTVIS",       "SQR",      "Common squid",
  "GLADDE HAAI",              "SMD",      "Starry smooth-hound",
  "GLADDE ROG",               "RJH",      "Blonde ray",
  "GOUDBRASEM",               "SBG",      "Gilthead seabream",
  "GRAUWE POON",              "GUG",      "Tub gurnard",
  "GRIET",                    "BLL",      "Brill",
  "GROTE PIETERMAN",          "WEG",      "Greater weever",
  "GRT PIETER MAN",           "WEG",      "Greater weever",
  "HAAI",                     "SMD",      "Smooth-hound",
  "HARING",                   "HER",      "Atlantic herring",
  "HEEK",                     "HKE",      "European hake",
  "HEILBOT",                  "HAL",      "Atlantic halibut",
  "HONDSHAAI",                "SYC",      "Lesser spotted dogfish",
  "HONDSTONG / WITJE",        "WIT",      "Witch flounder",
  "HORSMAKREEL",              "HOM",      "Atlantic horse mackerel",
  "HORSMAKREEL (JAX)",        "HOM",      "Atlantic horse mackerel",
  "INKTVIS",                  "SQR",      "Common squid",
  "INKTVIS (SQC)",            "SQC",      "Squid nei",
  "INKTVIS (ZEEKAT)",         "CTC",      "Common cuttlefish",
  "KABELJAUW",                "COD",      "Atlantic cod",
  "KOOLVIS",                  "POK",      "Saithe",
  "KOOLVIS (WITTE)",          "POL",      "Pollack",
  "KOOLVIS (ZWARTE)",         "POK",      "Saithe",
  "KRAB",                     "CRE",      "Edible crab",
  "KRAB; NOORDZEEKRAB",       "CRE",      "Edible crab",
  "KREEFT",                   "LBE",      "European lobster",
  "KUIT",                     "ROE",      "Fish roe",
  "LENG",                     "LIN",      "Ling",
  "MAKREEL",                  "MAC",      "Atlantic mackerel",
  "MUL",                      "MUR",      "Red mullet",
  "MULLETS NEI",               "MUL",      "Mullets nei",
  "OCTOPUS",                  "OCC",      "Common octopus",
  "PIETERMAN",                "WEG",      "Greater weever",
  "PILCHARDS",                "PIL",      "European pilchard",
  "PILSTER",                  "PIL",      "European pilchard",
  "RODE MUL",                 "MUR",      "Red mullet",
  "RODE POON",                "GUU",      "Red gurnard",
  "RODE ZEEBRASEM",           "SBR",      "Red seabream",
  "ROG",                      "RJC",      "Thornback ray",
  "ROZE ZEEBRASEM",           "SBR",      "Red seabream",
  "SARDIEN",                  "PIL",      "European pilchard",
  "SCHAR",                    "DAB",      "Dab",
  "SCHARRETONG",              "MEG",      "Megrim",
  "SCHARTONG",                "MEG",      "Megrim",
  "SCHELVIS",                 "HAD",      "Haddock",
  "SCHOL",                    "PLE",      "European plaice",
  "SPINKRAB",                 "SCR",      "Spinous spider crab",
  "SPINOUS SPIDER CRAB",      "SCR",      "Spinous spider crab",
  "ST JACOBSSCHELP",          "SCE",      "Great scallop",
  "ST. JAKOBS SCHELP",        "SCE",      "Great scallop",
  "STEENBOLK",                "BIB",      "Bib",
  "STEKEL ROG",               "RJC",      "Thornback ray",
  "STEKELROG",                "RJC",      "Thornback ray",
  "TARBOT",                   "TUR",      "Turbot",
  "TONG",                     "SOL",      "Common sole",
  "TONG SCHAR",               "LEM",      "Lemon sole",
  "TONGSCHAR",                "LEM",      "Lemon sole",
  "WIJTING",                  "WHG",      "Whiting",
  "WULK",                     "WHE",      "Common whelk",
  "ZEEBAARS",                 "BSS",      "European seabass",
  "ZEEBRASEM",                "SBR",      "Red seabream",
  "ZEEDUIVEL",                "MON",      "Anglerfish",
  "ZEEDUIVEL (ANF)",          "ANF",      "Anglerfish nei",
  "ZEEKARPER",                "BRB",      "Bogue",
  "ZEEKAT",                   "CTC",      "Common cuttlefish",
  "ZEEPALING",                "COE",      "European conger",
  "ZEEWOLF",                  "CAA",      "Atlantic wolffish",
  "ZONNEVIS",                 "JOD",      "John Dory",
  "ZONNEVIS/ZON",             "JOD",      "John Dory",
  "ZONNEVIS ZON",             "JOD",      "John Dory"
)

#' Parse a vector of Marelec species strings into species_code, species_name_nl,
#' species_name_en, and presentation.
#'
#' Fully vectorised — operates on the entire vector at once via mutate + left_join.
#'
#' Handles all separator styles:
#'   "DUTCH NAME (CODE)"        split on " ("
#'   "Dutch name/CODE"          split on "/"
#'   "DUTCH NAME SUFFIX"        space-separated presentation suffix (e.g. "BOT ss")
#'   "DUTCH NAME"               no code present
#'
#' @param x character vector of raw Marelec species strings
#' @return tibble with columns: species_code, species_name_nl, species_name_en, presentation
parse_marelec_species <- function(x) {
  
  lu <- MARELEC_SPECIES_LOOKUP
  known_fao  <- unique(lu$fao_code)
  pres_codes <- MARELEC_PRESENTATION_CODES
  
  # Build lookup vectors for fast named-vector lookup
  lu_code <- setNames(lu$fao_code,        lu$dutch_name_upper)
  lu_en   <- setNames(lu$species_name_en, lu$dutch_name_upper)
  # Also allow lookup by FAO code for English name
  lu_en_by_fao <- setNames(lu$species_name_en, lu$fao_code)
  
  tibble(raw = x) %>%
    mutate(
      raw = trimws(raw),
      
      # ------------------------------------------------------------------
      # Step 1: Normalise NA / DIVERSEN entries up front
      # ------------------------------------------------------------------
      is_na_entry = is.na(raw) | raw == "" | toupper(raw) %in% c("NA", "NA/DIVERSEN"),
      
      # ------------------------------------------------------------------
      # Step 2: Detect separator style and extract dutch + code parts
      # ------------------------------------------------------------------
      has_slash  = !is_na_entry & grepl("/",    raw, fixed = TRUE),
      has_paren  = !is_na_entry & !has_slash & grepl("(", raw, fixed = TRUE),
      has_neither = !is_na_entry & !has_slash & !has_paren,
      
      # Slash style: "Dutch name/CODE"
      dutch_slash = if_else(has_slash, trimws(str_extract(raw, "^[^/]+")),         NA_character_),
      code_slash  = if_else(has_slash, trimws(str_extract(raw, "(?<=/)[^/]+$")),   NA_character_),
      
      # Paren style: "DUTCH NAME (CODE)"
      dutch_paren = if_else(has_paren, trimws(str_remove(raw, "\\s*\\([^)]+\\)")), NA_character_),
      code_paren  = if_else(has_paren, trimws(str_match(raw, "\\(([^)]+)\\)")[,2]),NA_character_),
      
      # Space style: "BOT ss" — last word is a presentation code
      last_word      = if_else(has_neither, toupper(word(raw, -1)),                NA_character_),
      is_pres_suffix = has_neither & last_word %in% pres_codes,
      dutch_space    = if_else(is_pres_suffix,
                               trimws(str_remove(raw, paste0("\\s+\\S+$"))),
                               if_else(has_neither, raw, NA_character_)),
      code_space     = if_else(is_pres_suffix, last_word, NA_character_),
      
      # Combine into single dutch / code columns
      dutch = case_when(
        is_na_entry ~ NA_character_,
        has_slash   ~ dutch_slash,
        has_paren   ~ dutch_paren,
        TRUE        ~ dutch_space
      ),
      code = toupper(trimws(case_when(
        is_na_entry ~ NA_character_,
        has_slash   ~ code_slash,
        has_paren   ~ code_paren,
        TRUE        ~ code_space
      ))),
      dutch_upper = toupper(trimws(dutch)),
      
      # Remap non-standard codes to their correct FAO equivalents
      code = if_else(!is.na(code) & code %in% names(MARELEC_CODE_REMAP),
                     MARELEC_CODE_REMAP[code],
                     code),
      
      # ------------------------------------------------------------------
      # Step 3: Classify the code
      # ------------------------------------------------------------------
      code_is_fao  = !is.na(code) & code != "" & code %in% known_fao,
      code_is_pres = !is.na(code) & code != "" & !code_is_fao & code %in% pres_codes,
      
      # ------------------------------------------------------------------
      # Step 4: Resolve species_code and presentation
      # ------------------------------------------------------------------
      species_code = case_when(
        is_na_entry  ~ "DPX",
        code_is_fao  ~ code,
        code_is_pres ~ lu_code[dutch_upper],     # look up from Dutch name
        !is.na(code) & code != "" ~ code,         # unknown code: pass through as-is
        TRUE         ~ lu_code[dutch_upper]        # no code: look up Dutch name
      ),
      presentation = case_when(
        is_na_entry  ~ NA_character_,
        code_is_pres ~ code,
        TRUE         ~ NA_character_
      ),
      
      # ------------------------------------------------------------------
      # Step 5: English name — prefer Dutch-name lookup, fall back to FAO lookup
      # ------------------------------------------------------------------
      species_name_en = case_when(
        is_na_entry ~ "Miscellaneous",
        !is.na(lu_en[dutch_upper]) ~ lu_en[dutch_upper],
        !is.na(species_code) ~ lu_en_by_fao[species_code],
        .default = NA_character_
      ),
      
      # Dutch name
      species_name_nl = if_else(is_na_entry, "DIVERSEN", dutch_upper),
      
      # ------------------------------------------------------------------
      # Step 6: Fallback — if still no species_code, use first word of Dutch name
      # ------------------------------------------------------------------
      species_code = if_else(
        is.na(species_code),
        toupper(word(dutch_upper, 1)),
        species_code
      )
    ) %>%
    # Warn once per unknown species (not per row — avoids thousands of messages)
    { 
      unknown <- filter(., !species_code %in% c(known_fao, "DPX")) %>%
        distinct(raw, species_code)
      if (nrow(unknown) > 0) {
        walk2(unknown$raw, unknown$species_code, ~
          message(glue("    ⚠ Unknown species: '{.x}' -> fallback code '{.y}'")))
      }
      .
    } %>%
    select(species_code, species_name_nl, species_name_en, presentation)
}


# ==============================================================================
# Load configuration
# ==============================================================================
if (file.exists("config.json")) {
  config <- jsonlite::read_json("config.json")
} else {
  stop("Configuration file not found. Please run 00_setup.R first.")
}


# ==============================================================================
# DATA SOURCE DETECTION ----
# ==============================================================================

#' Detect data source type from available files
#' @param trip_files dataframe with columns: vessel, trip, source, file
#' @return character string indicating data source type
detect_data_source_type <- function(trip_files) {
  
  sources <- trip_files$source
  
  # Priority order for data source detection
  
  # 1. Full manual treklijst (most detailed)
  if ("treklijst" %in% sources) {
    treklijst_file <- trip_files %>% filter(source == "treklijst") %>% pull(file)
    if (!grepl("simplified", basename(treklijst_file), ignore.case = TRUE)) {
      return("treklijst_full")
    }
  }
  
  # 2. Automated PEFA per trek
  if ("pefa_trek" %in% sources) {
    return("pefa_trek")
  }
  
  # 3. Simplified treklijst + kisten
  if ("treklijst" %in% sources && "kisten" %in% sources) {
    return("treklijst_simplified")
  }
  
  # 4. Kisten + PEFA positions
  if ("kisten" %in% sources && "pefa" %in% sources) {
    return("kisten_pefa")
  }
  
  # 5. PEFA elog only
  if ("pefa" %in% sources) {
    return("pefa_only")
  }
  
  # 6. Standalone elog file
  if ("elog" %in% sources) {
    # If elog_trek (per-haul detail) is also present, use combined mode
    if ("elog_trek" %in% sources) {
      return("elog_by_haul")
    }
    return("elog_only")
  }
  
  # 7. Kisten only
  if ("kisten" %in% sources) {
    return("kisten_only")
  }
  
  # 8. mcatch
  if ("mcatch" %in% sources) {
    return("mcatch")
  }

  # 9. TurboCatch ERS XML folder
  if ("turbocatch" %in% sources) {
    return("turbocatch")
  }
  
  stop("Unable to determine data source type from available files")
}

# ==============================================================================
# HAUL PROCESSING FUNCTIONS ----
# ==============================================================================

# file_path <- treklijst_file

#' Get haul data from manual treklijst
#' @param file_path path to treklijst Excel file
#' @return tibble with haul data in standardized format
get_haul_from_treklijst <- function(file_path) {
  
  message(glue("  Processing manual treklijst: {basename(file_path)}"))
  
  # Extract vessel and trip identifiers
  ids <- extract_vessel_trip(file_path)
  vessel_id <- ids$vessel
  trip_id   <- ids$trip_id
  
  if (is.na(vessel_id) || is.na(trip_id)) {
    stop(glue("Could not extract vessel/trip from filename: {basename(file_path)}"))
  }
  
  # ------------------------------------------------------------------
  # 1. Read all columns as text (avoids readxl type-guessing problems)
  # ------------------------------------------------------------------
  raw <- read_excel(
    file_path,
    sheet      = "Haul",
    col_types  = "text",
    col_names  = TRUE,
    .name_repair = ~make.names(., unique = TRUE)
  ) %>%
    lowcase() %>%
    # ------------------------------------------------------------------
    # Keep only rows where the skipper entered a shoot-start time.
    # tijdbeginuitzetten is filled for every real haul without exception.
    # date is NOT a safe filter: skippers only write it on the first haul
    # of each day, and the 50 blank template rows at the bottom of the
    # sheet all get date filled forward by na.locf(), making a post-fill
    # date filter unreliable.
    # ------------------------------------------------------------------
    # Keep only real haul rows. Primary filter: shoot-start time filled.
    # Fallback: if shoot time column is entirely absent or all-NA (simplified
    # treklijst format), filter on non-NA date instead.
    { if ("tijdbeginuitzetten" %in% names(.) &&
          sum(!is.na(.$tijdbeginuitzetten)) > 0) {
        dplyr::filter(., !is.na(tijdbeginuitzetten))
      } else if ("berekentijdbeginuitzetten" %in% names(.) &&
                 sum(!is.na(.$berekentijdbeginuitzetten)) > 0) {
        dplyr::filter(., !is.na(berekentijdbeginuitzetten))
      } else {
        dplyr::filter(., !is.na(date))
      }
    } %>%
    # ------------------------------------------------------------------
    # Forward-fill columns that fishers only enter when something changes.
    # Order matters: date must be filled before any downstream date use.
    # Catch-specific and time columns are haul-unique and NOT filled.
    # ------------------------------------------------------------------
    mutate(across(
      any_of(c(
        # Trip-level identifiers (same for every haul in a trip)
        "vessel", "reis", "skipper",
        "dateembarkedutcdate", "portofembarkation",
        "datedisembarkedutcdate", "portofdisembarkation",
        "trip", "timezone",
        # Date: skippers fill only the first haul of each day;
        # all subsequent hauls inherit the same date via na.locf()
        "date",
        # Shoot position (raw DDMM entries)
        "shootlat", "shootns", "shootlong", "shootew",
        # Shoot position (Excel-calculated decimal degrees)
        "plot_lat", "plot_lon",
        # Environmental (written when conditions change)
        "waterdepth", "winddirection", "windforcebft",
        # Gear configuration (written when gear changes)
        "gear", "meshsize", "vertopening",
        "cablelengthm", "cablethicknessmm", "lengthgroundropem",
        "escapepanelyn",
        # Spatial (ICES rect/area follow position)
        "rect", "area"
      )),
      ~zoo::na.locf(., na.rm = FALSE)
    ))
  
  # ------------------------------------------------------------------
  # 2. Apply schema: rename to poseidat_name + coerce to target types
  #    Dates, times, datetimes, positions, proportions are all handled
  #    inside apply_schema / coerce_column.
  # ------------------------------------------------------------------
  haul_data <- apply_schema(raw, treklijst_haul_schema)
  
  # ------------------------------------------------------------------
  # 2b. Fix time-only datetime columns
  #     berekentijdeindehalen (and potentially others) may be stored as
  #     pure time fractions in Excel (0 < serial < 1), yielding a
  #     1899-12-30 placeholder date. Correct by replacing the date
  #     component with the actual haul date from the same row.
  # ------------------------------------------------------------------
  time_only_cols <- c("shoot_time", "shoot_end_time", "haul_time")
  epoch <- as.Date("1899-12-30")
  
  if ("date" %in% names(haul_data)) {
    haul_data <- haul_data %>%
      mutate(across(
        any_of(time_only_cols),
        ~{
          is_placeholder <- !is.na(.x) & as.Date(.x) == epoch
          if (any(is_placeholder)) {
            # Extract just the time-of-day seconds from the placeholder
            time_secs <- as.numeric(.x[is_placeholder]) %% 86400
            # Combine with the actual date for that row
            fixed <- as.POSIXct(
              as.numeric(as.POSIXct(date[is_placeholder], tz = "UTC")) + time_secs,
              origin = "1970-01-01", tz = "UTC"
            )
            .x[is_placeholder] <- fixed
          }
          .x
        }
      ))
  }
  
  # ------------------------------------------------------------------
  # 2c. Convert datetime columns from local vessel time to UTC.
  #     The treklijst timezone field may be an IANA name ("Europe/Amsterdam")
  #     or a fixed-offset string ("UTC+1", "UTC+2"). Both are handled.
  #     coerce_column() labels everything UTC as a neutral container;
  #     here we re-interpret those values as local and shift to true UTC.
  # ------------------------------------------------------------------
  datetime_cols <- c("shoot_time", "shoot_end_time", "haul_time")
  
  tz_raw <- if ("timezone" %in% names(haul_data) && !all(is.na(haul_data$timezone))) {
    first(na.omit(haul_data$timezone))
  } else {
    "UTC"
  }
  
  tz_value <- resolve_tz(tz_raw)
  
  if (tz_value != "UTC") {
    message(glue("    Converting datetime columns from {tz_raw} ({tz_value}) to UTC"))
    haul_data <- haul_data %>%
      mutate(across(
        any_of(datetime_cols),
        ~lubridate::with_tz(lubridate::force_tz(.x, tzone = tz_value), tzone = "UTC")
      ))
  }
  
  # ------------------------------------------------------------------
  # 3. Post-schema enrichment (only things the schema cannot do)
  # ------------------------------------------------------------------
  haul_data <- haul_data %>%
    
    mutate(
      # Identifiers -------------------------------------------------------
      vessel  = vessel_id,
      trip_id = trip_id,
      haul_id = row_number(),
      
      # Decimal-degree positions ------------------------------------------
      # Prefer the Excel-calculated plot_lat/plot_lon (already decimal).
      # Fall back to converting the raw DDMM integer columns when the
      # calculated columns are absent or all-NA.
      shoot_lat = {
        if ("shoot_lat" %in% names(.) && !all(is.na(shoot_lat))) {
          shoot_lat
        } else if (all(c("shoot_lat_ddmm", "shoot_ns") %in% names(.))) {
          ddmm_to_decimal(shoot_lat_ddmm, shoot_ns)
        } else {
          NA_real_
        }
      },
      shoot_lon = {
        if ("shoot_lon" %in% names(.) && !all(is.na(shoot_lon))) {
          shoot_lon
        } else if (all(c("shoot_lon_dddmm", "shoot_ew") %in% names(.))) {
          ddmm_to_decimal(shoot_lon_dddmm, shoot_ew)
        } else {
          NA_real_
        }
      },
      
      # Haul positions (not in schema; fall back to shoot position) -------
      haul_lat = NA_real_,
      haul_lon = NA_real_,
      
      # Haul end time imputation -----------------------------------------
      # tijdeindehalen is often left blank by the skipper. When the
      # schema-computed haul_time is NA, fall back to shoot_time + 80 min
      # (the typical gear-in-water duration for this vessel: ~60 min tow
      # + ~20 min hauling). impute_time() leaves non-NA values untouched.
      haul_time = impute_time(haul_time, shoot_time, offset_minutes = 80),

      # Fishing duration --------------------------------------------------
      # Now safe to compute: haul_time is never NA when shoot_time is known.
      fishing_time_hours = as.numeric(
        difftime(haul_time, shoot_time, units = "hours")
      ),
      
      # Gear type — use value from schema; fall back to "FLY" only if absent
      gear_type = if ("gear_type" %in% names(.) && !all(is.na(gear_type))) {
        gear_type
      } else {
        "FLY"
      }
    )
  
  # ------------------------------------------------------------------
  # 4. Drop intermediate columns used only for calculation, then
  #    set canonical column order (all others retained via everything())
  # ------------------------------------------------------------------
  haul_data <- haul_data %>%
    select(-any_of(c(
      # Raw HHMM time entries — redundant with shoot_time/haul_time POSIXct
      "shoot_time_hhmm", "shoot_end_time_hhmm", "haul_time_hhmm",
      # Raw DDMM position entries — redundant with shoot_lat/shoot_lon decimal
      "shoot_lat_ddmm", "shoot_ns", "shoot_lon_dddmm", "shoot_ew",
      # Removed catch columns
      "catch_height_cm", "bycatch_pct", "box_type", "photo_box"
    )))
  
  # Haul output: geometry and effort columns only — no catch-level columns.
  haul_cols <- c(
    "vessel", "trip_id", "haul_id", "date",
    "shoot_lat", "shoot_lon", "shoot_time",
    "shoot_end_time", "next_haul_time",
    "haul_lat", "haul_lon", "haul_time",
    "fishing_time_hours",
    "gear_type", "mesh_size_mm",
    "vertical_opening_m", "cable_length_m",
    "cable_thickness_mm", "groundrope_length_m",
    "escape_panel",
    "water_depth", "wind_direction", "wind_force_bft",
    "total_catch_kg", "marketable_catch_kg",  # treklijst haul totals — OK at haul level
    "fao_division", "ices_rect", "economic_zone",
    "trip_nr", "skipper", "timezone",
    "departure_date", "departure_port",
    "arrival_date", "arrival_port",
    "n_shots", "comments", "data_source"
  )

  haul_data <- haul_data %>%
    select(any_of(haul_cols))
  
  message(glue("    Processed {nrow(haul_data)} hauls"))
  
  return(haul_data)
}

#' Get haul data from PEFA per trek
#' @param file_path path to PEFA per trek Excel file
#' @param elog_data optional tibble (vessel, date, trip_id) from elog parquet
#' @return tibble with haul data
get_haul_from_pefa_trek <- function(file_path, elog_data = NULL) {
  read_pefa_trek(file_path, elog_data = elog_data)$haul
}

#' Get catch data from PEFA per trek
#' @param file_path path to PEFA per trek Excel file
#' @param elog_data optional tibble (vessel, date, trip_id) from elog parquet
#' @return tibble with catch data (one row per species per haul)
get_catch_from_pefa_trek <- function(file_path, elog_data = NULL) {
  read_pefa_trek(file_path, elog_data = elog_data)$catch
}

#' Internal: read and parse a PEFA per trek file into haul + catch tibbles.
#' Called by get_haul_from_pefa_trek() and get_catch_from_pefa_trek() so that
#' the file is only read once and the haul renumbering is applied consistently
#' to both outputs.
#' @param file_path path to PEFA per trek Excel file
#' @return list with $haul (one row per haul) and $catch (one row per species per haul)
read_pefa_trek <- function(file_path, elog_data = NULL) {
  
  message(glue("  Processing PEFA per trek: {basename(file_path)}"))
  
  ids <- extract_vessel_trip(file_path)
  vessel_id <- ids$vessel
  
  if (is.na(vessel_id)) {
    stop(glue("Could not extract vessel from filename: {basename(file_path)}"))
  }
  
  # Read data
  pefa_raw <- read_excel(
    file_path,
    sheet = 1,
    col_names = TRUE,
    .name_repair = ~make.names(., unique = TRUE)
  ) %>%
    rename_with(tolower)
  
  message(glue("    Original columns: {paste(names(pefa_raw), collapse = ', ')}"))

  # ── File validity check ───────────────────────────────────────────────────
  # Catch problems from incomplete downloads early rather than processing
  # silently and getting wrong results.
  date_col <- intersect(c("catch_date", "date"), names(pefa_raw))
  rect_col <- intersect(c("ices_rectangle", "ices_rect"), names(pefa_raw))

  file_issues <- character(0)

  if (nrow(pefa_raw) < 5)
    file_issues <- c(file_issues,
      glue("only {nrow(pefa_raw)} data rows — likely incomplete download"))

  if (length(date_col) > 0) {
    dates <- suppressWarnings(as.Date(pefa_raw[[date_col[1]]]))
    dates <- dates[!is.na(dates)]
    n_days <- length(unique(dates))
    if (n_days == 0)
      file_issues <- c(file_issues, "no valid dates found in file")
  }

  if (length(rect_col) > 0 && length(date_col) > 0) {
    dates <- suppressWarnings(as.Date(pefa_raw[[date_col[1]]]))
    n_unique_hauls <- length(unique(paste(
      format(dates, "%Y%m%d"),
      pefa_raw[[rect_col[1]]]
    )))
    if (n_unique_hauls < 2 && nrow(pefa_raw) > 10)
      file_issues <- c(file_issues,
        glue("only {n_unique_hauls} unique date×rectangle combination(s) ",
             "for {nrow(pefa_raw)} rows — file may be a partial download"))
  }

  # A genuine pefa_per_trek file must have a haul/trek identifier column
  haul_col <- intersect(c("haul", "trek", "haul_id", "haulid", "trekid", "haalnr",
                           "haalnummer", "trek_nr", "trek_id", "haulnr", "haulnumber"),
                        names(pefa_raw))
  if (length(haul_col) == 0)
    file_issues <- c(file_issues,
      glue("no haul/trek identifier column found — ",
           "this appears to be a plain elog pefa file, not a per-trek file. ",
           "Expected one of: haul, trek, haul_id, trekid, haalnr"))

  if (length(file_issues) > 0) {
    msg <- paste0(
      "  \u26a0 File validation failed for ", basename(file_path), ":\n",
      paste0("    - ", file_issues, collapse = "\n"), "\n",
      "  Skipping this file. Re-download and try again."
    )
    message(msg)
    stop(glue("Invalid pefa_trek file: {paste(file_issues, collapse='; ')}"),
         call. = FALSE)
  }
  # ── End validity check ────────────────────────────────────────────────────
  
  # pefa_data <- map_pefa_columns(pefa_raw)
  # Remove session-end marker rows before column mapping
  species_col_raw <- intersect(c("species", "soorten"), names(pefa_raw))
  if (length(species_col_raw) > 0) {
    n_before <- nrow(pefa_raw)
    pefa_raw <- pefa_raw %>%
      dplyr::filter(!toupper(trimws(.data[[species_col_raw[1]]])) %in%
                      c("EINDE", "EINDE DAG"))
    n_removed <- n_before - nrow(pefa_raw)
    if (n_removed > 0)
      message(glue("    Removed {n_removed} EINDE/EINDE DAG marker rows"))
  }

  pefa_data <- map_columns(pefa_raw)
  
  message(glue("    Mapped columns: {paste(names(pefa_data), collapse = ', ')}"))
  
  # Trip ID — priority order:
  #   1. 'trip_identifier' column in file (e-logbook trip number — most reliable)
  #   2. 'trip' column in file
  #   3. elog lookup by vessel + date range (avoids date-concatenated ids)
  #   4. filename-derived id (last resort)
  if ("trip_identifier" %in% names(pefa_data) &&
      !all(is.na(pefa_data$trip_identifier))) {
    trip_id <- as.character(first(na.omit(pefa_data$trip_identifier))) %>% str_trim()
    message(glue("    trip_id: {trip_id} (from trip_identifier column)"))

  } else if ("trip" %in% names(pefa_data)) {
    trip_id <- as.character(pefa_data$trip[1]) %>% str_trim()
    message(glue("    trip_id: {trip_id} (from trip column)"))

  } else if (!is.null(elog_data) && nrow(elog_data) > 0) {
    file_dates <- as.Date(pefa_data$date[!is.na(pefa_data$date)])
    if (length(file_dates) > 0) {
      date_min <- min(file_dates)
      date_max <- max(file_dates)
      elog_match <- elog_data %>%
        dplyr::filter(vessel == vessel_id,
                      date >= date_min, date <= date_max) %>%
        dplyr::count(trip_id, name = "n") %>%
        dplyr::arrange(dplyr::desc(n))
      if (nrow(elog_match) > 0) {
        trip_id <- elog_match$trip_id[1]
        message(glue("    trip_id: {trip_id} (from elog lookup {date_min}–{date_max})"))
      } else {
        trip_id <- ids$trip_id
        message(glue("    trip_id: {trip_id} (filename — no elog match for {date_min}–{date_max})"))
      }
    } else {
      trip_id <- ids$trip_id
      message(glue("    trip_id: {trip_id} (filename — no dates in file)"))
    }

  } else {
    trip_id <- ids$trip_id
    if (is.na(trip_id)) {
      stop(glue("Could not extract trip from file or filename: {basename(file_path)}"))
    }
    message(glue("    trip_id: {trip_id} (filename — no elog_data provided)"))
  }
  
  # ------------------------------------------------------------------
  # 1. Full dataset with all columns typed
  #    Rename haul_id -> haul_id_orig immediately so it never collides
  #    with the renumbered haul_id added later by the join.
  # ------------------------------------------------------------------
  full_data <- pefa_data %>%
    rename(haul_id_orig = any_of("haul_id")) %>%
    mutate(
      vessel  = vessel_id,
      trip_id = trip_id,
      date    = if ("date" %in% names(.)) as.Date(date) else NA_Date_,

      # When no haul_id column exists, derive haul from date + position
      # so that each unique (date, ices_rectangle) combination becomes a haul.
      # This is the standard PEFA elog format where catch is reported per day/rect.
      haul_id_orig = if ("haul_id_orig" %in% names(.)) {
        as.integer(haul_id_orig)
      } else {
        # Build a surrogate haul key from date + ICES rectangle (or just date)
        rect_col <- intersect(c("ices_rectangle", "ices_rect"), names(.))
        if (length(rect_col) > 0) {
          as.integer(factor(paste(format(date, "%Y%m%d"),
                                  .data[[rect_col[1]]],
                                  sep = "_")))
        } else {
          as.integer(factor(format(date, "%Y%m%d")))
        }
      },
      shoot_lat    = if ("shoot_lat" %in% names(.)) as.numeric(shoot_lat) else NA_real_,
      shoot_lon    = if ("shoot_lon" %in% names(.)) as.numeric(shoot_lon) else NA_real_,
      haul_lat     = if ("haul_lat" %in% names(.)) as.numeric(haul_lat)
                     else if ("shoot_lat" %in% names(.)) as.numeric(shoot_lat)
                     else NA_real_,
      haul_lon     = if ("haul_lon" %in% names(.)) as.numeric(haul_lon)
                     else if ("shoot_lon" %in% names(.)) as.numeric(shoot_lon)
                     else NA_real_,
      shoot_time   = if ("shoot_time" %in% names(.))
                       as.POSIXct(paste(format(date, "%Y-%m-%d"), shoot_time),
                                  format = "%Y-%m-%d %H:%M:%S")
                     else as.POSIXct(NA),
      haul_time    = if ("haul_time" %in% names(.))
                       as.POSIXct(paste(format(date, "%Y-%m-%d"), haul_time),
                                  format = "%Y-%m-%d %H:%M:%S")
                     else as.POSIXct(NA),
      fishing_time_hours = as.numeric(difftime(haul_time, shoot_time, units = "hours")),
      gear_type    = if ("gear_type" %in% names(.) && !all(is.na(gear_type)))
                       gear_type else NA_character_,
      species_code = if ("species" %in% names(.)) toupper(species) else NA_character_,
      weight_kg    = if ("catch_kg" %in% names(.)) as.numeric(catch_kg) else NA_real_,
      box_count    = if ("box_number" %in% names(.)) as.integer(box_number) else NA_integer_
    )

  # ------------------------------------------------------------------
  # Filter erroneous positions: lat == 0 AND lon == 0 means the GPS had
  # no fix (PEFA records (0, 0) as a sentinel for "no position").
  # Setting these to NA keeps the haul row intact for catch accounting
  # while preventing a spurious spatial join to the Gulf of Guinea.
  # We also guard against implausibly large absolute values (>90 lat,
  # >180 lon) which indicate a unit or format error.
  # ------------------------------------------------------------------
  n_zero <- full_data %>%
    dplyr::filter(!is.na(shoot_lat) & !is.na(shoot_lon) &
                  shoot_lat == 0 & shoot_lon == 0) %>%
    nrow()

  n_implausible <- full_data %>%
    dplyr::filter(
      (!is.na(shoot_lat) & abs(shoot_lat) > 90) |
      (!is.na(shoot_lon) & abs(shoot_lon) > 180)
    ) %>%
    nrow()

  if (n_zero > 0 || n_implausible > 0) {
    full_data <- full_data %>%
      dplyr::mutate(
        shoot_lat = dplyr::case_when(
          !is.na(shoot_lat) & !is.na(shoot_lon) &
            shoot_lat == 0 & shoot_lon == 0          ~ NA_real_,
          !is.na(shoot_lat) & abs(shoot_lat) > 90    ~ NA_real_,
          TRUE                                        ~ shoot_lat
        ),
        shoot_lon = dplyr::case_when(
          !is.na(shoot_lat) & !is.na(shoot_lon) &
            shoot_lat == 0 & shoot_lon == 0          ~ NA_real_,
          !is.na(shoot_lon) & abs(shoot_lon) > 180   ~ NA_real_,
          TRUE                                        ~ shoot_lon
        ),
        haul_lat = dplyr::case_when(
          !is.na(haul_lat) & !is.na(haul_lon) &
            haul_lat == 0 & haul_lon == 0            ~ NA_real_,
          !is.na(haul_lat) & abs(haul_lat) > 90      ~ NA_real_,
          TRUE                                        ~ haul_lat
        ),
        haul_lon = dplyr::case_when(
          !is.na(haul_lat) & !is.na(haul_lon) &
            haul_lat == 0 & haul_lon == 0            ~ NA_real_,
          !is.na(haul_lon) & abs(haul_lon) > 180     ~ NA_real_,
          TRUE                                        ~ haul_lon
        )
      )
    if (n_zero > 0)
      message(glue("    ⚠ {n_zero} row(s) with (0, 0) position set to NA ",
                   "(GPS had no fix)"))
    if (n_implausible > 0)
      message(glue("    ⚠ {n_implausible} row(s) with implausible lat/lon set to NA"))
  }
  
  # ------------------------------------------------------------------
  # 2. Build haul renumber key: original haul_id -> sequential haul_id
  #    Sort chronologically, then number 1..N per trip
  # ------------------------------------------------------------------
  haul_key <- full_data %>%
    group_by(vessel, trip_id, haul_id_orig) %>%
    summarise(
      date       = dplyr::first(na.omit(date)),
      shoot_time = dplyr::first(na.omit(shoot_time)),
      .groups = "drop"
    ) %>%
    arrange(vessel, trip_id, date, shoot_time, haul_id_orig) %>%
    group_by(vessel, trip_id) %>%
    mutate(haul_id = row_number()) %>%
    ungroup() %>%
    select(vessel, trip_id, haul_id_orig, haul_id)
  
  # ------------------------------------------------------------------
  # 3. Haul output: one row per haul with renumbered haul_id
  # ------------------------------------------------------------------
  # Haul output: geometry and effort columns only.
  # Catch-level columns (species_code, weight_kg, presentation, etc.) are
  # intentionally excluded here — they belong in the elog_trek/catch output
  # below, not in the haul table. Using everything() would carry through
  # whichever species happened to be slice(1) for that haul, which is
  # meaningless at haul level and creates 70-column haul parquets.
  haul_cols <- c(
    "vessel", "trip_id", "haul_id", "date",
    "shoot_lat", "shoot_lon", "shoot_time",
    "shoot_end_time", "next_haul_time",
    "haul_lat", "haul_lon", "haul_time",
    "fishing_time_hours",
    "gear_type", "mesh_size_mm",
    "vertical_opening_m", "cable_length_m",
    "cable_thickness_mm", "groundrope_length_m",
    "escape_panel",
    "water_depth", "wind_direction", "wind_force_bft",
    "fao_division", "ices_rect", "economic_zone",
    "trip_nr", "skipper",
    "departure_date", "departure_port",
    "arrival_date", "arrival_port",
    "n_shots", "data_source"
  )

  haul_data <- full_data %>%
    group_by(vessel, trip_id, haul_id_orig) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(haul_key, by = c("vessel", "trip_id", "haul_id_orig")) %>%
    select(-haul_id_orig) %>%
    select(any_of(haul_cols))
  
  message(glue("    Processed {nrow(haul_data)} hauls"))
  
  # ------------------------------------------------------------------
  # 4. Catch output: one row per species per haul with renumbered haul_id
  # LSC (legal size) and BMS (below minimum size) stored as separate rows
  # ------------------------------------------------------------------
  base_cols_trek <- c("vessel", "trip_id", "haul_id", "date",
                      "shoot_lat", "shoot_lon", "species_code", "box_count",
                      "gear_type", "presentation", "preservation", "freshness",
                      "conversion_factor", "discard_reason")

  joined <- full_data %>%
    left_join(haul_key, by = c("vessel", "trip_id", "haul_id_orig")) %>%
    select(-haul_id_orig) %>%
    filter(!is.na(species_code))

  legal_rows <- joined %>%
    dplyr::filter(!is.na(weight_kg) & weight_kg > 0) %>%
    dplyr::mutate(size_category = "LSC") %>%
    dplyr::select(dplyr::any_of(c(base_cols_trek, "weight_kg", "size_category")))

  bms_rows <- if ("weight_undersized_kg" %in% names(joined)) {
    joined %>%
      dplyr::filter(!is.na(weight_undersized_kg) & weight_undersized_kg > 0) %>%
      dplyr::mutate(
        size_category = "BMS",
        weight_kg     = weight_undersized_kg,
        box_count     = if ("boxes_undersized" %in% names(.))
                          as.integer(boxes_undersized) else NA_integer_
      ) %>%
      dplyr::select(dplyr::any_of(c(base_cols_trek, "weight_kg", "box_count", "size_category")))
  } else { tibble::tibble() }

  catch_data <- dplyr::bind_rows(legal_rows, bms_rows) %>%
    dplyr::arrange(date, haul_id, species_code, size_category)

  # Impute weighing_time from haul_time (best available proxy for PEFA-trek
  # data, which has no per-box timestamps). Falls back to shoot_time when
  # haul_time is NA. This ensures weighing_time is never entirely NA in
  # elog_trek for pefa_trek trips.
  if (!"weighing_time" %in% names(catch_data)) {
    haul_times <- haul_data %>%
      dplyr::select(vessel, trip_id, haul_id,
                    dplyr::any_of(c("haul_time", "shoot_time")))

    catch_data <- catch_data %>%
      dplyr::left_join(haul_times, by = c("vessel", "trip_id", "haul_id")) %>%
      dplyr::mutate(
        weighing_time = dplyr::coalesce(
          dplyr::if_else(!is.na(haul_time),  haul_time,  as.POSIXct(NA_real_, tz = "UTC")),
          dplyr::if_else(!is.na(shoot_time), shoot_time, as.POSIXct(NA_real_, tz = "UTC"))
        )
      ) %>%
      dplyr::select(-dplyr::any_of(c("haul_time", "shoot_time")))
  }

  message(glue("    Processed {nrow(catch_data)} catch records across {nrow(haul_data)} hauls"))
  
  list(haul = haul_data, catch = catch_data)
}

#' Get haul data from kisten + PEFA positions
#' @param pefa_file path to PEFA elog file
#' @param kisten_file path to kisten file
#' @return tibble with haul data
get_haul_from_kisten_pefa <- function(pefa_file, kisten_file) {
  
  message(glue("  Processing kisten + PEFA positions"))
  
  # Extract vessel from filename
  ids <- extract_vessel_trip(pefa_file)
  vessel_id <- ids$vessel
  
  if (is.na(vessel_id)) {
    stop(glue("Could not extract vessel from filename: {basename(pefa_file)}"))
  }
  
  # Read PEFA positions
  pefa_raw <- read_excel(pefa_file, sheet = 1) %>%
    rename_with(tolower)
  
  # Map columns
  pefa_data <- map_columns(pefa_raw)
  
  # Try to extract trip from PEFA file content first
  if ("trip" %in% names(pefa_data)) {
    trip_id <- as.character(pefa_data$trip[1]) %>% str_trim()
    message(glue("    Using trip {trip_id} from PEFA file content"))
  } else {
    # Fall back to filename-based extraction
    trip_id <- ids$trip_id
    if (is.na(trip_id)) {
      stop(glue("Could not extract trip from file or filename: {basename(pefa_file)}"))
    }
    message(glue("    Using trip {trip_id} from filename (no trip column in PEFA file)"))
  }
  
  # Process PEFA positions
  pefa_data <- pefa_data %>%
    mutate(
      datetime = if ("date" %in% names(.) && "shoot_time" %in% names(.)) {
        as.POSIXct(paste(date, shoot_time), format = "%Y-%m-%d %H:%M:%S")
      } else NA,
      
      lat = if ("shoot_lat" %in% names(.)) {
        as.numeric(shoot_lat)
      } else NA_real_,
      
      lon = if ("shoot_lon" %in% names(.)) {
        as.numeric(shoot_lon)
      } else NA_real_
    )
  
  # Read kisten data
  kisten_raw <- read_excel(kisten_file, sheet = 1) %>%
    rename_with(tolower)
  
  # Map kisten columns
  kisten_data <- map_columns(kisten_raw)
  
  # Process kisten data
  kisten_data <- kisten_data %>%
    mutate(
      date = if ("date" %in% names(.)) as.Date(date) else NA_Date_,
      haul_id = if ("haul_id" %in% names(.)) as.integer(haul_id) else NA_integer_
    ) %>%
    group_by(haul_id) %>%
    summarise(
      date = first(date),
      total_catch_kg = if ("catch_kg" %in% names(.)) {
        sum(catch_kg, na.rm = TRUE)
      } else 0,
      .groups = "drop"
    )
  
  # Create haul data by matching positions to hauls
  # Gear type: read from PEFA data if available, otherwise NA
  gear_from_pefa <- if ("gear_type" %in% names(pefa_data) && 
                        !all(is.na(pefa_data$gear_type))) {
    first(na.omit(pefa_data$gear_type))
  } else {
    NA_character_
  }
  
  haul_data <- kisten_data %>%
    mutate(
      vessel    = vessel_id,
      trip_id   = trip_id,
      gear_type = gear_from_pefa
    ) %>%
    select(vessel, trip_id, haul_id, date, total_catch_kg, gear_type)
  
  return(haul_data)
}

# CATCH PROCESSING FUNCTIONS ----

#' Get catch data from treklijst
#' @param file_path path to treklijst Excel file
#' @return tibble with catch data
get_catch_from_treklijst <- function(file_path) {
  
  message(glue("  Processing catch from treklijst: {basename(file_path)}"))
  
  # Extract vessel and trip identifiers
  ids <- extract_vessel_trip(file_path)
  vessel_id <- ids$vessel
  trip_id <- ids$trip_id
  
  if (is.na(vessel_id) || is.na(trip_id)) {
    stop(glue("Could not extract vessel/trip from filename: {basename(file_path)}"))
  }
  
  # Read catch sheet
  catch_raw <- read_excel(
    file_path,
    sheet = "Kisten",
    col_names = TRUE,
    .name_repair = ~make.names(., unique = TRUE)
  ) %>%
    rename_with(tolower)
  
  # Map columns
  catch_data <- map_columns(catch_raw)
  
  # Process catch data
  # Filter for valid rows - check column existence first
  if ("species" %in% names(catch_data)) {
    catch_data <- catch_data %>%
      filter(!is.na(species) & species != "")
  } else if ("catch_kg" %in% names(catch_data)) {
    catch_data <- catch_data %>%
      filter(!is.na(catch_kg))
  }
  
  catch_data <- catch_data %>%
    mutate(
      vessel = vessel_id,
      trip_id = trip_id,
      
      haul_id = if ("haul_id" %in% names(.)) {
        as.integer(haul_id)
      } else NA_integer_,
      
      species_code = if ("species" %in% names(.)) {
        toupper(species)
      } else NA_character_,
      
      weight_kg = if ("catch_kg" %in% names(.)) {
        as.numeric(catch_kg)
      } else NA_real_,
      
      box_count = if ("box_number" %in% names(.)) {
        as.numeric(box_number)
      } else NA_real_
    ) %>%
    select(vessel, trip_id, haul_id, species_code, weight_kg, box_count, everything())
  
  return(catch_data)
}

# ==============================================================================
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
                                  treklijst_path = NULL,
                                  haul_data      = NULL) {
  
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
  # Find which row contains "Lotnummer" — that row is the header.
  # rowwise string detection using dplyr, no apply needed.
  skip_n <- preview %>%
    dplyr::mutate(
      .row    = dplyr::row_number(),
      .is_hdr = dplyr::if_any(everything(),
                  ~stringr::str_detect(as.character(.), stringr::regex("lotnummer", ignore_case = TRUE)))
    ) %>%
    dplyr::filter(.is_hdr) %>%
    dplyr::pull(.row) %>%
    dplyr::first()
  skip_n <- if (is.na(skip_n)) 5L else as.integer(skip_n) - 1L
  if (is.na(skip_n)) skip_n <- 5L
  
  # ── 2. Read data table ────────────────────────────────────────────────────
  raw <- read_excel(file_path, sheet = "1", skip = skip_n,
                    col_names = TRUE, col_types = "text",
                    .name_repair = ~make.names(., unique = TRUE)) %>%
    lowcase() %>%
    filter(!is.na(lotnummer), lotnummer != "") %>%
    # Marelec exports rows in descending lot order (newest first).
    # Sort ascending so weighing_time increases monotonically, which is
    # required for the time-gap session detection in the haul assignment.
    arrange(as.integer(lotnummer))

  message(glue("    Read {nrow(raw)} box records"))

  # Remove Marelec session-end marker rows before any processing.
  # "EINDE" and "EINDE DAG" are injected by the Marelec system to mark the
  # end of a weighing session — they are not catch records.
  einde_col <- intersect(c("soorten", "species"), names(raw))
  if (length(einde_col) > 0) {
    n_before <- nrow(raw)
    raw <- raw %>%
      dplyr::filter(!toupper(trimws(.data[[einde_col[1]]])) %in%
                      c("EINDE", "EINDE DAG"))
    n_removed <- n_before - nrow(raw)
    if (n_removed > 0)
      message(glue("    Removed {n_removed} EINDE/EINDE DAG marker rows"))
  }

  # ── 3. Apply schema ───────────────────────────────────────────────────────
  catch_data <- apply_schema(raw, marelec_kisten_schema)
  
  # ── 4. Enrich: datetime, species, size_class ──────────────────────────────
  species_parsed <- parse_marelec_species(catch_data$species_raw)
  
  # Resolve the local timezone string to a valid IANA name before use.
  # The treklijst timezone column often contains "UTC+1" / "UTC+2" which
  # as.POSIXct() does not accept; resolve_tz() converts these to Etc/GMT-1 etc.
  tz_resolved <- resolve_tz(local_tz)

  catch_data <- catch_data %>%
    mutate(
      vessel    = vessel_id,
      trip_id   = trip_id,
      box_count = 1L,

      # weighing_time: handle multiple formats:
      #   1. "HH:MM:SS" text time — paste with date and parse
      #   2. Numeric Excel fraction (e.g. 0.385 = 09:14) — convert fraction to seconds
      #   3. Already a POSIXct datetime in date column — use directly
      weighing_time = {
        if ("time_hhmm" %in% names(.) && !all(is.na(time_hhmm))) {
          t <- time_hhmm
          # Check if numeric fraction (Excel time stored as character "0.385...")
          t_num <- suppressWarnings(as.numeric(t))
          if (!all(is.na(t_num))) {
            # Numeric fraction: combine with date
            as.POSIXct(as.numeric(as.Date(date)) * 86400 + t_num * 86400,
                       origin = "1970-01-01", tz = tz_resolved) %>% with_tz("UTC")
          } else {
            # Text "HH:MM:SS": paste with date
            as.POSIXct(paste(as.character(date), t),
                       format = "%Y-%m-%d %H:%M:%S",
                       tz = tz_resolved) %>% with_tz("UTC")
          }
        } else if ("date" %in% names(.) && inherits(date, c("POSIXct","POSIXlt"))) {
          with_tz(as.POSIXct(date, tz = tz_resolved), "UTC")
        } else {
          as.POSIXct(NA)
        }
      },
      
      size_class = suppressWarnings(
        as.integer(trimws(str_remove(size_class,
                                     regex("klasse", ignore_case = TRUE))))
      ),
      
      species_code    = species_parsed$species_code,
      species_name_nl = species_parsed$species_name_nl,
      species_name_en = species_parsed$species_name_en,
      presentation    = species_parsed$presentation
    )
  
  # ── 5. AUTO HAUL ASSIGNMENT from treklijst ───────────────────────────────
  # Auto-detect the treklijst in the same directory as the kisten file.
  # Files share a VESSEL_ISOWEEK prefix (e.g. "SCH99_2026W23").
  # treklijst_path can also be supplied explicitly by the caller.
  if (is.null(treklijst_path) || is.na(treklijst_path)) {
    treklijst_path <- find_treklijst_for_kisten(file_path)
  }

  # Use pre-assigned haul_id only when the raw kisten file explicitly contains
  # a haul column (manually assigned by the skipper or processor).
  # When haul_data is provided and the file has no haul column, always use
  # time-based matching for accuracy.
  has_haul_id <- "haul_id" %in% names(catch_data) &&
                 sum(!is.na(catch_data$haul_id)) > 0 &&
                 "haul" %in% names(raw)   # raw = the Excel data before schema mapping


  if (has_haul_id) {
    message(glue("    haul_id: using manually pre-assigned values from file ",
                 "({sum(!is.na(catch_data$haul_id))}/{nrow(catch_data)} non-NA)"))
  } else {
    # Build trek_haul_sheet: prefer already-processed haul_data (avoids re-reading
    # the Excel file and bypasses the col_types="list" POSIXct parsing issue),
    # fall back to read_treklijst_for_kisten if haul_data not supplied.
    # Determine haul assignment strategy
    use_date_assignment <- FALSE
    trek_haul_sheet <- if (!is.null(haul_data) && nrow(haul_data) > 0 &&
                            all(c("haul_id", "shoot_time", "total_catch_kg") %in%
                                names(haul_data))) {
      hauls_with_time <- haul_data %>% dplyr::filter(!is.na(shoot_time))
      if (nrow(hauls_with_time) > 0) {
        # Normal path: use shoot_time for precise kisten assignment
        hauls_with_time %>%
          dplyr::transmute(
            haul           = as.integer(haul_id),
            shoot_datetime = shoot_time,
            catch_kg       = as.numeric(total_catch_kg)
          )
      } else if ("date" %in% names(haul_data) && sum(!is.na(haul_data$date)) > 0) {
        # Simplified treklijst: flag for date-based assignment below
        use_date_assignment <- TRUE
        NULL
      } else { NULL }
    } else if (!is.null(treklijst_path) && !is.na(treklijst_path)) {
      read_treklijst_for_kisten(treklijst_path)
    } else {
      NULL
    }

    if (use_date_assignment) {
      # Date-based assignment for simplified treklijst:
      # 1. First detect kisten sessions by time gap (same as normal path)
      # 2. Number sessions sequentially as haul_ids
      # 3. Join treklijst position for each session's date
      message("    \u2139 No shoot times \u2014 using session-based haul numbering with date positions")

      # Detect sessions by time gap
      catch_data <- catch_data %>%
        dplyr::arrange(weighing_time) %>%
        dplyr::mutate(
          time_gap_min = as.numeric(difftime(weighing_time,
                                             dplyr::lag(weighing_time),
                                             units = "mins")),
          new_session  = is.na(time_gap_min) |
                         time_gap_min > KISTEN_GAP_THRESHOLD_MIN,
          session_id   = cumsum(new_session)
        )

      n_sessions <- dplyr::n_distinct(catch_data$session_id)
      message(glue("    {n_sessions} weighing sessions detected"))

      # Number sessions sequentially as haul_ids
      session_dates <- catch_data %>%
        dplyr::group_by(session_id) %>%
        dplyr::summarise(
          session_date = as.Date(dplyr::first(weighing_time), tz = "UTC"),
          .groups = "drop"
        ) %>%
        dplyr::arrange(session_id) %>%
        dplyr::mutate(haul_id = dplyr::row_number())

      # Build date position lookup from treklijst
      date_lookup <- haul_data %>%
        dplyr::filter(!is.na(date)) %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(haul_id = dplyr::first(haul_id), .groups = "drop")

      # Join session date to treklijst position via date
      session_pos <- session_dates %>%
        dplyr::left_join(
          haul_data %>%
            dplyr::filter(!is.na(date)) %>%
            dplyr::select(date, shoot_lat, shoot_lon) %>%
            dplyr::distinct(date, .keep_all = TRUE),
          by = c("session_date" = "date")
        )

      # Apply session haul_ids to catch_data
      catch_data <- catch_data %>%
        dplyr::left_join(
          session_dates %>% dplyr::select(session_id, haul_id),
          by = "session_id"
        ) %>%
        dplyr::select(-session_id, -time_gap_min, -new_session)

      message(glue("    Kisten assigned by session+date: ",
                   "{sum(!is.na(catch_data$haul_id))}/{nrow(catch_data)} rows matched"))

      # Store session positions for use in trip_hauls (accessible via attribute)
      attr(catch_data, "session_positions") <- session_pos

    } else if (!is.null(trek_haul_sheet)) {
      result     <- assign_haul_ids_from_treklijst(
        kisten       = catch_data,
        treklijst    = trek_haul_sheet,
        local_tz     = local_tz
      )
      catch_data <- result$kisten
      attr(catch_data, "haul_assignment_summary") <- result$summary
    } else {
      catch_data <- catch_data %>% mutate(haul_id = NA_integer_)
    }
  }
  
  # ── 6. Final select ───────────────────────────────────────────────────────
  catch_data <- catch_data %>%
    dplyr::select(vessel, trip_id, haul_id, weighing_time, date,
                  species_code, species_name_nl, species_name_en, presentation,
                  size_class, weight_kg, box_count, lot_nr,
                  dplyr::any_of("haul_flag")) %>%
    dplyr::select(-dplyr::any_of(c("session_id", "time_gap_min")))
  
  message(glue("    Processed {nrow(catch_data)} box records across ",
               "{n_distinct(catch_data$haul_id, na.rm = TRUE)} hauls"))
  
  catch_data
}

# TRIP PROCESSING FUNCTIONS ----

#' Extract trip information from already-processed haul data (treklijst source)
#'
#' Trip-level fields (vessel, skipper, departure/arrival date+port, timezone)
#' are present in every haul row — we just take the first non-NA value of each.
#' This avoids re-reading the file and correctly uses the schema-typed values.
#'
#' @param haul_data tibble returned by get_haul_from_treklijst()
#' @return single-row tibble with trip information
get_trip_info_from_hauls <- function(haul_data) {
  
  first_val <- function(col) {
    if (col %in% names(haul_data)) first(na.omit(haul_data[[col]])) else NA
  }
  
  tibble(
    vessel         = first_val("vessel"),
    trip_id        = first_val("trip_id"),
    trip_nr        = first_val("trip_nr"),
    skipper        = first_val("skipper"),
    departure_date = as.Date(first_val("departure_date")),
    departure_port = first_val("departure_port"),
    arrival_date   = as.Date(first_val("arrival_date")),
    arrival_port   = first_val("arrival_port"),
    timezone       = first_val("timezone")
  )
}

# ==============================================================================
# ELOG SCHEMA AND READER
# ==============================================================================
#
# The elog file (image 2) is a flat CSV/Excel export from the e-logbook system.
# One row per species per haul. Key columns identified from the example:
#   catch_date, species (FAO code), presentation, preservation, freshness,
#   conversion_factor, weight, boxes, weight_undersized, boxes_undersized,
#   economic_zone, ices_rectangle, fao_zone, longitude, latitude,
#   gear_type, mesh_size, vessel_number, trip_identifier,
#   departure_date, departure_port, arrival_date, arrival_port,
#   auction_date, auction_port
# ==============================================================================

elog_schema <- tribble(
  ~excel_col,           ~poseidat_name,        ~target_type,
  "catch_date",         "date",                "date",
  "species",            "species_code",        "character",
  "presentation",       "presentation",        "character",
  "preservation",       "preservation",        "character",
  "freshness",          "freshness",           "character",
  "conversion_factor",  "conversion_factor",   "double",
  "weight",             "weight_kg",           "double",
  "boxes",              "box_count",           "integer",
  "weight_undersized",  "weight_undersized_kg","double",
  "boxes_undersized",   "boxes_undersized",    "integer",
  "economic_zone",      "economic_zone",       "character",
  "ices_rectangle",     "ices_rect",           "character",
  "icesrectangle",      "ices_rect",           "character",   # lowcase variant
  "fao_zone",           "fao_zone",            "character",
  "faozone",            "fao_zone",            "character",   # lowcase variant
  "longitude",          "shoot_lon",           "double",
  "latitude",           "shoot_lat",           "double",
  "gear_type",          "gear_type",           "character",
  "mesh_size",          "mesh_size_mm",        "integer",
  "vessel_number",      "vessel_nr",           "character",
  "trip_identifier",    "trip_nr_elog",        "character",
  "departure_date",     "departure_date",      "datetime",
  "departure_port",     "departure_port",      "character",
  "arrival_date",       "arrival_date",        "datetime",
  "arrival_port",       "arrival_port",        "character",
  "auction_date",       "auction_date",        "date",
  "auction_port",       "auction_port",        "character",
  "captain",            "skipper",             "character",
  "trip_status",        "trip_status",         "character",
  "discard_reason",     "discard_reason",      "character"
)

#' Read and parse an elog file
#' @param file_path path to elog Excel or CSV file
#' @return list with two tibbles: $catch (one row per species/haul) and
#'         $trip (one row with trip-level information)
get_data_from_elog <- function(file_path) {
  
  message(glue("  Processing elog: {basename(file_path)}"))
  
  ids <- extract_vessel_trip(file_path)
  vessel_id <- ids$vessel
  trip_id   <- ids$trip_id
  
  # ------------------------------------------------------------------
  # 1. Read — handle both Excel and CSV
  # ------------------------------------------------------------------
  ext <- tolower(tools::file_ext(file_path))
  
  raw <- if (ext %in% c("xlsx", "xls")) {
    read_excel(file_path, sheet = 1, col_types = "text",
               .name_repair = ~make.names(., unique = TRUE)) %>%
      lowcase()
  } else {
    read_csv(file_path, col_types = cols(.default = "c"),
             name_repair = ~make.names(., unique = TRUE)) %>%
      lowcase()
  }
  
  message(glue("    Read {nrow(raw)} rows, columns: {paste(names(raw), collapse = ', ')}"))
  
  # ------------------------------------------------------------------
  # 2. Apply schema
  # ------------------------------------------------------------------
  elog_data <- apply_schema(raw, elog_schema)
  
  # ------------------------------------------------------------------
  # 3. Add vessel / trip identifiers
  #    Priority order for trip_id:
  #      1. trip_identifier column in file (e-logbook trip number — most reliable)
  #      2. trip_nr column in file
  #      3. filename-derived id (last resort)
  #    This ensures that when both elog and elog_trek are present, all datasets
  #    get the trip number as recorded in the e-logbook, not a date-range string
  #    constructed from the filename.
  # ------------------------------------------------------------------
  
  # Resolve trip_nr_elog from file content first (used both as trip_id and stored separately).
  # The elog schema maps column "trip_identifier" -> poseidat_name "trip_nr_elog".
  # Diagnostic: show which raw columns were present to help debug missing trip_identifier.
  message(glue("    Raw elog columns: {paste(names(raw), collapse = ', ')}"))
  
  trip_nr_elog_from_file <- if ("trip_nr_elog" %in% names(elog_data) &&
                                 !all(is.na(elog_data$trip_nr_elog))) {
    as.character(first(na.omit(elog_data$trip_nr_elog)))
  } else {
    NA_character_
  }
  
  # Use plain if/else — case_when() is for vectorised operations, not scalar logic
  trip_id_resolved <- if (!is.na(trip_nr_elog_from_file)) {
    trip_nr_elog_from_file
  } else if ("trip_nr" %in% names(elog_data) && !all(is.na(elog_data$trip_nr))) {
    as.character(first(na.omit(elog_data$trip_nr)))
  } else if (!is.na(trip_id)) {
    trip_id
  } else {
    NA_character_
  }
  
  message(glue("    trip_id: {trip_id_resolved} ",
               "({if (!is.na(trip_nr_elog_from_file)) 'from elog trip_identifier' ",
               "else if (!is.na(trip_id)) 'from filename' else 'not found'})"))
  
  elog_data <- elog_data %>%
    mutate(
      vessel       = if (!is.null(vessel_id) && !is.na(vessel_id)) vessel_id
                     else NA_character_,
      trip_id      = trip_id_resolved,
      # trip_nr_elog stores the e-logbook trip identifier separately from trip_id,
      # which may come from the treklijst on other data source types
      trip_nr_elog = trip_nr_elog_from_file
    )
  
  # ------------------------------------------------------------------
  # 4. Split into catch records and trip summary
  # ------------------------------------------------------------------
  # Base columns shared by both legal and undersized rows.
  # Spatial columns (economic_zone, ices_rect, fao_division) are included
  # directly from the elog file — the e-logbook system reports these per catch
  # row and they are more reliable than deriving them from shoot positions.
  base_cols <- c("vessel", "trip_id", "trip_nr_elog", "date",
                 "species_code", "presentation", "preservation",
                 "freshness", "conversion_factor",
                 "shoot_lat", "shoot_lon", "gear_type", "mesh_size_mm",
                 "economic_zone", "ices_rect", "fao_division",
                 "discard_reason")

  legal_rows <- elog_data %>%
    dplyr::filter(!is.na(weight_kg) & weight_kg > 0) %>%
    dplyr::mutate(size_category = "LSC") %>%
    dplyr::select(dplyr::any_of(c(base_cols, "weight_kg", "box_count", "size_category")))

  # Undersized / de-minimis rows — weight_undersized_kg → weight_kg,
  # boxes_undersized → box_count, size_category = "BMS"
  bms_rows <- if ("weight_undersized_kg" %in% names(elog_data)) {
    elog_data %>%
      dplyr::filter(!is.na(weight_undersized_kg) & weight_undersized_kg > 0) %>%
      dplyr::mutate(
        size_category = "BMS",
        weight_kg     = weight_undersized_kg,
        box_count     = if ("boxes_undersized" %in% names(.))
                          as.integer(boxes_undersized) else NA_integer_
      ) %>%
      dplyr::select(dplyr::any_of(c(base_cols, "weight_kg", "box_count", "size_category")))
  } else { tibble::tibble() }

  catch_data <- dplyr::bind_rows(legal_rows, bms_rows) %>%
    dplyr::arrange(date, species_code, size_category)
  
  trip_data <- elog_data %>%
    summarise(
      vessel         = first(na.omit(vessel)),
      trip_id        = first(na.omit(trip_id)),
      trip_nr        = if ("trip_nr" %in% names(.)) first(na.omit(trip_nr)) else NA_character_,
      trip_nr_elog   = if ("trip_nr_elog" %in% names(.)) first(na.omit(trip_nr_elog)) else NA_character_,
      vessel_nr      = first(na.omit(vessel_nr)),
      skipper        = first(na.omit(skipper)),
      trip_status    = first(na.omit(trip_status)),
      departure_date = as.Date(first(na.omit(departure_date))),
      departure_port = first(na.omit(departure_port)),
      arrival_date   = as.Date(first(na.omit(arrival_date))),
      arrival_port   = first(na.omit(arrival_port)),
      auction_date   = first(na.omit(auction_date)),
      auction_port   = first(na.omit(auction_port)),
      gear_type      = first(na.omit(gear_type)),
      mesh_size_mm   = first(na.omit(mesh_size_mm))
    )
  
  message(glue("    {nrow(catch_data)} catch records, trip: ",
               "{trip_data$departure_port} {trip_data$departure_date} -> ",
               "{trip_data$arrival_port} {trip_data$arrival_date}"))
  
  list(catch = catch_data, trip = trip_data)
}

#' Legacy wrapper: get_trip_info() — now routes to the appropriate source
#' @param file_path path to treklijst or elog file
#' @param haul_data optional: already-processed haul tibble (preferred for treklijst)
#' @return single-row tibble with trip information
get_trip_info <- function(file_path, haul_data = NULL) {
  
  ids <- extract_vessel_trip(file_path)
  vessel_id <- ids$vessel
  trip_id   <- ids$trip_id
  
  # Preferred: derive from already-processed haul data
  if (!is.null(haul_data) && nrow(haul_data) > 0) {
    message("    Extracting trip info from haul data")
    return(get_trip_info_from_hauls(haul_data))
  }
  
  # Elog file: use dedicated reader
  if (grepl("elog", basename(file_path), ignore.case = TRUE)) {
    message("    Extracting trip info from elog file")
    elog <- get_data_from_elog(file_path)
    return(elog$trip)
  }
  
  # Final fallback: minimal trip info from filename only
  message(glue("    ⚠ No haul data or elog available — minimal trip info for {basename(file_path)}"))
  tibble(
    vessel         = vessel_id,
    trip_id        = trip_id,
    trip_nr        = NA_character_,
    skipper        = NA_character_,
    departure_date = as.Date(NA),
    departure_port = NA_character_,
    arrival_date   = as.Date(NA),
    arrival_port   = NA_character_,
    timezone       = NA_character_
  )
}

# SPATIAL PROCESSING FUNCTIONS ----

#' Add spatial attributes to haul data
#' @param haul_data tibble with haul data containing lat/lon
#' @param spatial_datasets list of spatial sf objects
#' @return haul_data with added spatial attributes
add_spatial_attributes <- function(haul_data, spatial_datasets = NULL) {
  
  if (is.null(spatial_datasets)) {
    message("⚠ No spatial datasets provided - skipping spatial attributes")
    return(haul_data)
  }
  
  message("  Adding spatial attributes...")

  # ------------------------------------------------------------------
  # Defensive position guard: nullify (0, 0) and implausible coordinates
  # before the spatial join. These slip through from any source (PEFA,
  # treklijst formula errors, elog exports) and would be joined to the
  # Gulf of Guinea, producing silently wrong ICES rect / FAO / EEZ values.
  # ------------------------------------------------------------------
  pos_cols <- intersect(c("shoot_lat", "shoot_lon", "haul_lat", "haul_lon"),
                        names(haul_data))
  if (length(pos_cols) > 0) {
    n_zero_pre <- haul_data %>%
      dplyr::filter(!is.na(shoot_lat) & !is.na(shoot_lon) &
                    shoot_lat == 0 & shoot_lon == 0) %>%
      nrow()
    n_impl_pre <- haul_data %>%
      dplyr::filter((!is.na(shoot_lat) & abs(shoot_lat) > 90) |
                    (!is.na(shoot_lon) & abs(shoot_lon) > 180)) %>%
      nrow()

    if (n_zero_pre > 0 || n_impl_pre > 0) {
      haul_data <- haul_data %>%
        dplyr::mutate(
          shoot_lat = dplyr::case_when(
            !is.na(shoot_lat) & !is.na(shoot_lon) &
              shoot_lat == 0 & shoot_lon == 0       ~ NA_real_,
            !is.na(shoot_lat) & abs(shoot_lat) > 90 ~ NA_real_,
            TRUE                                     ~ shoot_lat
          ),
          shoot_lon = dplyr::case_when(
            !is.na(shoot_lat) & !is.na(shoot_lon) &
              shoot_lat == 0 & shoot_lon == 0       ~ NA_real_,
            !is.na(shoot_lon) & abs(shoot_lon) > 180 ~ NA_real_,
            TRUE                                     ~ shoot_lon
          )
        )
      if ("haul_lat" %in% names(haul_data))
        haul_data <- haul_data %>%
          dplyr::mutate(
            haul_lat = dplyr::case_when(
              !is.na(haul_lat) & !is.na(haul_lon) &
                haul_lat == 0 & haul_lon == 0         ~ NA_real_,
              !is.na(haul_lat) & abs(haul_lat) > 90   ~ NA_real_,
              TRUE                                     ~ haul_lat
            ),
            haul_lon = dplyr::case_when(
              !is.na(haul_lat) & !is.na(haul_lon) &
                haul_lat == 0 & haul_lon == 0         ~ NA_real_,
              !is.na(haul_lon) & abs(haul_lon) > 180  ~ NA_real_,
              TRUE                                     ~ haul_lon
            )
          )
      if (n_zero_pre > 0)
        message(glue("  ⚠ {n_zero_pre} row(s) with (0, 0) position nullified before spatial join"))
      if (n_impl_pre > 0)
        message(glue("  ⚠ {n_impl_pre} row(s) with implausible coordinates nullified before spatial join"))
    }
  }
  
  # ------------------------------------------------------------------
  # Work on UNIQUE positions only — haul_data may have multiple rows
  # per position (e.g. elog: one row per species per haul).
  # Spatial join on all rows would create a many-to-many explosion.
  # We join on unique lat/lon pairs, then left_join back by position.
  # ------------------------------------------------------------------
  unique_pos <- haul_data %>%
    filter(!is.na(shoot_lon) & !is.na(shoot_lat)) %>%
    distinct(shoot_lat, shoot_lon) %>%
    mutate(.pos_id = row_number())
  
  if (nrow(unique_pos) == 0) {
    message("  ⚠ No valid positions found — skipping spatial attributes")
    return(haul_data)
  }
  
  pos_sf <- unique_pos %>%
    st_as_sf(coords = c("shoot_lon", "shoot_lat"), crs = 4326)
  
  # Spatial joins on unique positions
  if ("fao_sf_area" %in% names(spatial_datasets)) {
    pos_sf <- pos_sf %>% st_join(spatial_datasets$fao_sf_area, left = TRUE)
  }
  if ("fao_sf_division" %in% names(spatial_datasets)) {
    pos_sf <- pos_sf %>% st_join(spatial_datasets$fao_sf_division, left = TRUE)
  }
  if ("rect_sf" %in% names(spatial_datasets)) {
    pos_sf <- pos_sf %>% st_join(spatial_datasets$rect_sf, left = TRUE)
  }
  if ("eez_sf" %in% names(spatial_datasets)) {
    pos_sf <- pos_sf %>% st_join(spatial_datasets$eez_sf, left = TRUE)
  }
  
  # Boundary duplicates: keep first match per unique position
  pos_spatial <- pos_sf %>%
    st_drop_geometry() %>%
    group_by(.pos_id) %>%
    slice(1) %>%
    ungroup()
  
  # Identify which columns were added by the spatial joins
  spatial_cols <- setdiff(names(pos_spatial), c(".pos_id", names(unique_pos)))
  
  # Join spatial attributes back to unique_pos, then to haul_data by lat/lon
  pos_lookup <- unique_pos %>%
    left_join(pos_spatial %>% select(.pos_id, all_of(spatial_cols)),
              by = ".pos_id") %>%
    select(-.pos_id)
  
  # Only drop file-sourced spatial columns if the spatial join actually produced
  # results. We define the known file-source equivalents for each computed column:
  #   ices_rect      <- ices_rectangle, ices_rect (treklijst formula), rect
  #   fao_area       <- area
  #   fao_division   <- fao_zone, division
  #   economic_zone  <- economic_zone (same name — would cause .x/.y collision)
  file_spatial_equivalents <- c(
    "ices_rectangle", "ices_area",         # file-sourced equivalents of ices_rect
    "fao_zone",                             # file-sourced equivalent of fao_division
    "area", "division", "rect"              # old non-standard spatial names
  )
  # Also include any spatial_cols that already exist in haul_data under the
  # same name (e.g. economic_zone -> economic_zone would cause .x/.y)
  cols_to_drop <- union(
    file_spatial_equivalents,
    spatial_cols   # same-named cols in haul_data would collide on join
  )
  
  if (length(spatial_cols) > 0) {
    haul_data <- haul_data %>%
      select(-any_of(cols_to_drop))
  }
  
  result <- haul_data %>%
    left_join(pos_lookup, by = c("shoot_lat", "shoot_lon"))
  
  message(glue("  ✓ Spatial attributes added ({length(spatial_cols)} columns: ",
               "{paste(spatial_cols, collapse = ', ')})"))
  
  return(result)
}

# VALIDATION FUNCTIONS ----

#' Validate haul data
#' @param haul_data tibble with haul data
#' @return list with validation results
validate_haul_data <- function(haul_data) {
  
  issues <- list()
  
  # Check for required columns
  required_cols <- c("vessel", "trip_id", "haul_id", "date")
  missing_cols <- setdiff(required_cols, names(haul_data))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }
  
  # Check for missing positions
  missing_positions <- haul_data %>%
    filter(is.na(shoot_lat) | is.na(shoot_lon)) %>%
    nrow()
  if (missing_positions > 0) {
    issues$missing_positions <- glue("{missing_positions} hauls without positions")
  }
  
  # Check for invalid coordinates
  invalid_coords <- haul_data %>%
    filter(
      !is.na(shoot_lat) & !is.na(shoot_lon) &
        (abs(shoot_lat) > 90 | abs(shoot_lon) > 180)
    ) %>%
    nrow()
  if (invalid_coords > 0) {
    issues$invalid_coordinates <- glue("{invalid_coords} hauls with invalid coordinates")
  }
  
  # Check for duplicate haul_ids within trip
  # Skip if haul_id is all NA (e.g. elog data where rows are per species, not per haul)
  if ("haul_id" %in% names(haul_data) && !all(is.na(haul_data$haul_id))) {
    duplicates <- haul_data %>%
      filter(!is.na(haul_id)) %>%
      group_by(vessel, trip_id, haul_id) %>%
      filter(n() > 1) %>%
      nrow()
    if (duplicates > 0) {
      issues$duplicate_hauls <- glue("{duplicates} duplicate haul records")
    }
  }
  
  # Report results
  if (length(issues) == 0) {
    message("✓ Validation passed: no issues found")
    return(list(valid = TRUE, issues = NULL))
  } else {
    message("⚠ Validation warnings:")
    for (issue_name in names(issues)) {
      message(glue("  - {issue_name}: {issues[[issue_name]]}"))
    }
    return(list(valid = FALSE, issues = issues))
  }
}

# HELPER FUNCTIONS ----

#' Map PEFA column names to standardized names
#' @param df dataframe with PEFA data
#' @return dataframe with standardized column names
# map_pefa_columns <- function(df) {
#   
#   # Get current column names (already lowercased)
#   current_names <- names(df)
#   
#   # Define mapping patterns: standard_name -> possible_variations
#   column_mapping <- list(
#     # Trip identifier
#     trip = c("trip", "tripnumber", "tripnr", "trip_id", "tripid", "reis", "reisnummer", "reisnr"),
#     
#     # Haul identifier
#     haul_id = c("trek", "haul", "haul_id", "haulid", "trekid", "haalnr", "haalnummer"),
#     
#     # Date
#     date = c("datum", "date", "day", "dag", "catch_date"),
#     shoot_lon = c("longitude_shoot", "lon_shoot", "shoot_lon", "longitude", "lon", "lengtegraad"),
#     
#     # Positions - hauling (may not exist)
#     haul_lat = c("latitude_haul", "lat_haul", "haul_lat", "latitude_halen", "lat_halen"),
#     haul_lon = c("longitude_haul", "lon_haul", "haul_lon", "longitude_halen", "lon_halen"),
#     
#     # Times
#     shoot_time = c("tijd_shoot", "time_shoot", "shoot_time", "tijdbeginuitzetten", "tijd", "time"),
#     haul_time = c("tijd_haul", "time_haul", "haul_time", "tijdeindehalen", "tijd_halen"),
#     
#     # Catch
#     catch_kg = c("gewicht", "weight", "catch", "vangst", "weight_kg", "catch_kg"),
#     
#     # Species
#     species = c("soort", "species", "vis", "fish"),
#     
#     # Box/number
#     box_number = c("kist", "box", "kistnummer", "boxnumber", "nummer", "number")
#   )
#   
#   # Create rename mapping
#   # For rename(), we need: new_name = old_name
#   rename_map <- c()
#   
#   for (standard_name in names(column_mapping)) {
#     possible_names <- column_mapping[[standard_name]]
#     
#     # Find which variation exists in the data
#     matching_name <- possible_names[possible_names %in% current_names]
#     
#     if (length(matching_name) > 0) {
#       # Use first match
#       # Assign: new_name = old_name (correct order for rename())
#       rename_map[standard_name] <- matching_name[1]
#     }
#   }
#   
#   # Apply renaming
#   if (length(rename_map) > 0) {
#     df <- df %>% rename(!!!rename_map)
#   }
#   
#   return(df)
# }

#' Map column names to standardized names
#' @param df dataframe with data
#' @return dataframe with standardized column names
map_columns <- function(df) {
  
  # Get current column names (already lowercased)
  current_names <- names(df)
  
  # Define mapping patterns: standard_name -> possible_variations
  column_mapping <- list(
    # Trip identifier
    trip = c("trip", "tripnumber", "tripnr", "trip_id", "tripid", "reis", "reisnummer", "reisnr"),
    
    # Haul identifier
    haul_id = c("trek", "haul", "haul_id", "haulid", "trekid", "haalnr", "haalnummer"),
    
    # Date
    date = c("datum", "date", "day", "dag", "catch_date"),
    
    # Positions - shooting
    shoot_lat = c("latitude_shoot", "lat_shoot", "shoot_lat", "latitude", "lat", "breedtegraad", "shootlat"),
    shoot_lon = c("longitude_shoot", "lon_shoot", "shoot_lon", "longitude", "lon", "lengtegraad", "shootlong"),
    
    # Positions - hauling (may not exist)
    haul_lat = c("latitude_haul", "lat_haul", "haul_lat", "latitude_halen", "lat_halen"),
    haul_lon = c("longitude_haul", "lon_haul", "haul_lon", "longitude_halen", "lon_halen"),
    
    # Times
    shoot_time = c("tijd_shoot", "time_shoot", "shoot_time", "tijdbeginuitzetten", "tijd", "time"),
    haul_time = c("tijd_haul", "time_haul", "haul_time", "tijdeindehalen", "tijd_halen"),
    
    # Catch
    catch_kg = c("gewicht", "weight", "catch", "vangst", "weight_kg", "catch_kg"),
    
    # Species
    species = c("soort", "species", "vis", "fish"),
    
    # Box/number
    box_number = c("kist", "box", "kistnummer", "boxnumber", "nummer", "number"),
    
    # Departure date
    departure_date = c("departuredate", "dateembarkedutcdate"),
    
    # Departure port
    departure_port = c("departureport", "portofembarkation"),
    
    # Arrival date
    arrival_date = c("arrivaldate","datedisembarkedutcdate"),
    
    # Arrival port
    arrival_port = c("arrivalport","portofdisembarkation"),
    
    # Captain
    captain = c("captain","skipper")
    
    
  )
  
  # Create rename mapping
  # For rename(), we need: new_name = old_name
  rename_map <- c()
  
  for (standard_name in names(column_mapping)) {
    possible_names <- column_mapping[[standard_name]]
    
    # Find which variation exists in the data
    matching_name <- possible_names[possible_names %in% current_names]
    
    if (length(matching_name) > 0) {
      # Use first match
      # Assign: new_name = old_name (correct order for rename())
      rename_map[standard_name] <- matching_name[1]
    }
  }
  
  # Apply renaming
  if (length(rename_map) > 0) {
    df <- df %>% rename(!!!rename_map)
  }
  
  return(df)
}

#' Extract trip number from PEFA file
#' @param file_path path to PEFA Excel file
#' @return trip number as character, or NA if not found
extract_trip_from_pefa <- function(file_path) {
  
  trip_number <- NA
  
  tryCatch({
    # Read first few rows to find trip number
    # PEFA files typically have trip in first sheet
    pefa_data <- read_excel(file_path, sheet = 1, n_max = 10)
    
    # Convert column names to lowercase for easier matching
    names(pefa_data) <- tolower(names(pefa_data))
    
    # Look for trip identifier in various possible column names
    trip_cols <- c("trip", "tripnumber", "tripnr", "trip_id", "tripid", 
                   "reis", "reisnummer", "reisnr")
    
    # Find which column exists
    trip_col <- trip_cols[trip_cols %in% names(pefa_data)]
    
    if (length(trip_col) > 0) {
      # Extract trip number from first row
      trip_value <- pefa_data[[trip_col[1]]][1]
      
      if (!is.na(trip_value)) {
        # Convert to character and remove any spaces
        trip_number <- as.character(trip_value) %>% str_trim()
      }
    }
    
  }, error = function(e) {
    # If file can't be read, return NA
    # Error will be caught and filename-based trip will be used
  })
  
  return(trip_number)
}

#' Extract vessel and trip identifiers from filename
#' @param file_path path to file or just filename
#' @return list with vessel and trip_id
extract_vessel_trip <- function(file_path) {
  
  filename <- basename(file_path)

  # Normalise: replace space between vessel ID and year/week with underscore
  # so "SCH144 2026W16 ..." and "SCH144_2026W16 ..." are treated identically.
  filename <- str_replace(filename, "^([A-Z]{2,3}[0-9]+) ([0-9]{4}W)", "\\1_\\2")

  # Extract vessel ID (e.g., SCH99, SCH123, UK101)
  vessel <- str_extract(filename, "^[A-Z]{2,3}[0-9]+")
  
  # Extract trip identifier — patterns tried in priority order:
  # 1. ISO week    : _YYYYWWW or " YYYYWWW"  (e.g. SCH65_2026W13, SCH99_2026W23)
  # 2. Date range  : _YYYYMMDD_YYYYMMDD_ (underscore-separated)
  # 3. Date range  : YYYYMMDD-YYYYMMDD   (hyphen-separated, with leading space)
  # 4. Legacy year : _YYYY_
  # NOTE: The old SCH99 "space style" (" YYYY_TTT") is retired.
  #       All SCH99 files now use ISO week naming (SCH99_2026W23 ...) so they
  #       match pattern 1, identical to SCH65 and other vessels.

  trip_id <- NA

  iso_week_match        <- str_extract(filename, "_[0-9]{4}W[0-9]{2}(?=[^0-9]|$)")
  date_range_underscore <- str_extract(filename, "_[0-9]{8}_[0-9]{8}_")
  date_range_hyphen     <- str_extract(filename, " [0-9]{8}-[0-9]{8}")

  if (!is.na(iso_week_match)) {
    trip_id <- str_remove(iso_week_match, "^_")

  } else if (!is.na(date_range_underscore)) {
    trip_id <- str_remove_all(date_range_underscore, "_")

  } else if (!is.na(date_range_hyphen)) {
    trip_id <- str_remove_all(date_range_hyphen, "[ -]")

  } else {
    # Fallback: legacy underscore year pattern
    parts <- str_split(filename, "_")[[1]]
    for (i in seq_along(parts)) {
      if (i > 1 && nchar(parts[i]) == 4 && str_detect(parts[i], "^[0-9]{4}$")) {
        year <- parts[i]
        if (i < length(parts) && str_detect(parts[i+1], "^[0-9]{1,4}$") &&
            nchar(parts[i+1]) <= 4) {
          trip_id <- paste0(year, parts[i+1])
        } else {
          trip_id <- year
        }
        break
      }
    }
  }
  
  return(list(vessel = vessel, trip_id = trip_id))
}

#' Convert Excel time to POSIXct
#' @param excel_time numeric Excel time value
#' @return POSIXct datetime
convert_excel_time <- function(excel_time) {
  if (is.na(excel_time) || !is.numeric(excel_time)) return(NA)
  
  # Excel stores dates as days since 1899-12-30
  as.POSIXct((excel_time - 25569) * 86400, origin = "1970-01-01", tz = "UTC")
}

#' Standardize species codes
#' @param species_codes character vector of species codes
#' @return standardized species codes
standardize_species_codes <- function(species_codes) {
  
  # Convert to uppercase
  codes <- toupper(species_codes)
  
  # Common mappings
  code_map <- c(
    "HER" = "HER",  # Herring
    "HARING" = "HER",
    "MAC" = "MAC",  # Mackerel
    "MAKREEL" = "MAC",
    "PIL" = "PIL",  # Pilchard
    "PILS" = "PIL",
    "SPR" = "SPR",  # Sprat
    "SPROT" = "SPR",
    "ANE" = "ANE",  # Anchovy
    "ANSJOVIS" = "ANE"
  )
  
  # Apply mapping
  result <- codes
  for (i in seq_along(codes)) {
    if (!is.na(codes[i]) && codes[i] %in% names(code_map)) {
      result[i] <- code_map[codes[i]]
    }
  }
  
  return(result)
}

#' Get file inventory for processing
#' @param input_dir directory to scan for files
#' @param file_patterns list of patterns to match
#' @return tibble with file inventory
get_file_inventory <- function(input_dir = NULL, file_patterns = NULL) {
  
  if (is.null(input_dir)) {
    input_dir <- config$tripdata_input
  }
  
  if (is.null(file_patterns)) {
    file_patterns <- config$file_patterns
  }
  
  message(glue("Scanning directory: {input_dir}"))
  
  # Initialize tibble with proper column types
  inventory <- tibble(
    vessel = character(),
    trip = character(),
    source = character(),
    file = character(),
    filename = character(),
    modified = as.POSIXct(character())
  )
  
  for (source_name in names(file_patterns)) {
    pattern <- file_patterns[[source_name]]
    
    files <- list.files(
      path = input_dir,
      pattern = pattern,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(files) > 0) {
      for (file_path in files) {
        filename    <- basename(file_path)
        filename_new <- str_replace(filename, "^([A-Z]{2,3}[0-9]+) ([0-9]{4}W)", "\\1_\\2")

        # Rename file on disk if normalisation changed the filename
        if (filename_new != filename) {
          new_path <- file.path(dirname(file_path), filename_new)
          if (!file.exists(new_path)) {
            file.rename(file_path, new_path)
            message(glue("  ✓ Renamed: {filename} → {filename_new}"))
          } else {
            message(glue("  ⚠ Could not rename {filename}: {filename_new} already exists"))
          }
          file_path <- new_path
          filename  <- filename_new
        } else {
          filename <- filename_new
        }

        # Extract vessel ID
        vessel <- str_extract(filename, "^[A-Z]{2,3}[0-9]+")
        
        # Detect trip identifier based on filename pattern
        trip <- NA
        trip_from_filename <- FALSE  # Track if trip came from filename dates
        
        # Patterns tried in priority order (same as extract_vessel_trip):
        # 1. ISO week    : _YYYYWWW or " YYYYWWW"  (e.g. SCH65_2026W13, SCH99_2026W23)
        # 2. Date range  : _YYYYMMDD_YYYYMMDD_
        # 3. Date range  : YYYYMMDD-YYYYMMDD (with space)
        # 4. Legacy year : _YYYY_
        # NOTE: old SCH99 " YYYY_TTT" space style is retired.
        iso_week_match        <- str_extract(filename, "_[0-9]{4}W[0-9]{2}(?=[^0-9]|$)")
        date_range_underscore <- str_extract(filename, "_[0-9]{8}_[0-9]{8}_")
        date_range_hyphen     <- str_extract(filename, " [0-9]{8}-[0-9]{8}")

        if (!is.na(iso_week_match)) {
          trip <- str_remove(iso_week_match, "^_")

        } else if (!is.na(date_range_underscore)) {
          trip_from_filename <- TRUE
          trip <- str_remove_all(date_range_underscore, "_")

        } else if (!is.na(date_range_hyphen)) {
          trip_from_filename <- TRUE
          trip <- str_remove_all(date_range_hyphen, "[ -]")

        } else {
          # Fallback: legacy underscore year pattern (e.g., _2024_)
          year_match <- str_extract(filename, "_[0-9]{4}_")
          if (!is.na(year_match)) {
            trip <- str_remove_all(year_match, "_")
          }
        }
        
        # If trip came from date range in filename AND this is a PEFA file,
        # try to extract actual trip number from file content
        if (trip_from_filename && source_name %in% c("pefa", "pefa_trek")) {
          trip_from_file <- extract_trip_from_pefa(file_path)
          
          if (!is.na(trip_from_file)) {
            message(glue("  → Using trip {trip_from_file} from file content (was {trip} from filename)"))
            trip <- trip_from_file
          } else {
            message(glue("  → Using trip {trip} from filename (could not read from file)"))
          }
        }
        
        # Only add to inventory if we have both vessel and trip
        if (!is.na(vessel) && !is.na(trip)) {
          inventory <- inventory %>%
            add_row(
              vessel = vessel,
              trip = trip,
              source = source_name,
              file = file_path,
              filename = filename,
              modified = file.mtime(file_path)
            )
        } else {
          message(glue("  ⚠ Could not parse filename: {filename}"))
          if (is.na(vessel)) {
            message("     - No vessel ID found (expected: ABC123...)")
          }
          if (is.na(trip)) {
            message("     - No trip identifier found")
            message("       Expected: '_YYYYWWW' or ' YYYYWWW' or ' YYYY_XXX ' or '_YYYYMMDD_YYYYMMDD_' or 'YYYYMMDD-YYYYMMDD' or '_YYYY_'")
          }
        }
      }
    }
  }
  
  # --- TurboCatch ERS XML: detect vessel subfolders containing .xml files
  # Expected structure: _te verwerken/CC545762/*.xml
  # The folder name IS the vessel ID; trip_id will be resolved from ELOG TN
  # during processing, so a placeholder is used here.
  vessel_xml_dirs <- list.dirs(input_dir, recursive = FALSE, full.names = TRUE)
  for (vessel_dir in vessel_xml_dirs) {
    vessel_id_dir <- basename(vessel_dir)
    if (!grepl("^[A-Z]{2,3}[0-9]+$", vessel_id_dir)) next  # only vessel-ID folders
    xml_files <- list.files(vessel_dir, pattern = "\\.xml$",
                            full.names = TRUE, recursive = TRUE)
    if (length(xml_files) == 0) next

    inventory <- inventory %>%
      add_row(
        vessel   = vessel_id_dir,
        trip     = glue("turbocatch_{vessel_id_dir}"),  # placeholder
        source   = "turbocatch",
        file     = vessel_dir,                           # folder, not individual file
        filename = basename(vessel_dir),
        modified = max(file.mtime(xml_files))
      )
    message(glue("  Detected turbocatch folder: {vessel_id_dir} ({length(xml_files)} XML files)"))
  }

  if (nrow(inventory) > 0) {
    message(glue("\u2713 Found {nrow(inventory)} files to process"))
    inventory <- inventory %>%
      arrange(vessel, trip, source)
  } else {
    message("\u26a0 No files found to process")
  }
  
  return(inventory)
}


#' Convert coordinate string in degrees and minutes to decimal degrees
#' @param x character string representing the coordinate (e.g., "5234" for 52°34')
#' @param direction character indicating hemisphere: "N", "S", "E", or "W"
#' @return numeric decimal degree value (negative for S and W)
parse_coord <- function(x, direction) {
  x <- trimws(x)
  direction <- trimws(toupper(direction))
  
  deg <- as.numeric(substr(x, 1, nchar(x) - 2))
  min <- as.numeric(substr(x, nchar(x) - 1, nchar(x)))
  
  decimal <- deg + min / 60
  
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  
  return(decimal)
}

#' Parse a combined coordinate string to decimal degrees
#' @param x character string with coordinate and direction (e.g., "5234 N" or "31E")
#' @return numeric decimal degree value (negative for S and W)
parse_coord_full <- function(x) {
  x <- trimws(x)
  parts <- strsplit(x, "\\s+")[[1]]
  
  if (length(parts) == 2) {
    coord <- parts[1]
    dir <- parts[2]
  } else {
    dir <- substr(x, nchar(x), nchar(x))
    coord <- substr(x, 1, nchar(x) - 1)
  }
  
  parse_coord(coord, dir)
}


# ==============================================================================
# KISTEN HAUL ASSIGNMENT FUNCTIONS
# ==============================================================================
# These functions assign haul IDs to Marelec kisten box records by matching
# weighing sessions to treklijst haul shoot times.
#
# Core observation: catch from haul N is weighed while haul N+1 is fishing,
# so each weighing session starts shortly after shoot_dt[N+1] (the "anchor").
# Typical anchor offset: -30 to +30 min; overnight hauls are the exception.
# ==============================================================================

KISTEN_GAP_THRESHOLD_MIN <- 30   # gap between weighings to start a new session
KISTEN_TOLERANCE_MIN     <- 60   # max offset from anchor to accept a match
KISTEN_FLAG_MIN          <- 30   # offset beyond which to flag for review
KISTEN_EARLY_START_MIN   <- 10   # how many minutes before haul end a session may start

#' Auto-detect the treklijst file that accompanies a kisten file.
#' Both files share a VESSEL_ISOWEEK prefix in the same directory.
#' e.g. "SCH99_2026W23 kisten-reis-3-trek-32 wk23.xlsx"
#'      "SCH99_2026W23 treklijst wk23.xlsx"
#' Returns NULL (with message) if no treklijst is found.
find_treklijst_for_kisten <- function(kisten_path) {
  dir      <- dirname(kisten_path)
  filename <- basename(kisten_path)

  # Extract the VESSEL_ISOWEEK prefix (everything before "_kisten" or " kisten")
  prefix <- sub("[_ ]kisten.*$", "", filename, ignore.case = TRUE)

  message(glue("    [find_treklijst] kisten dir  : {dir}"))
  message(glue("    [find_treklijst] kisten file : {filename}"))
  message(glue("    [find_treklijst] prefix used : {prefix}"))

  if (nchar(prefix) == 0 || prefix == filename) {
    message("    Could not extract prefix from kisten filename — skipping treklijst auto-detect")
    return(NULL)
  }

  all_in_dir <- list.files(dir, pattern = "treklijst", full.names = TRUE,
                           ignore.case = TRUE)
  message(glue("    [find_treklijst] treklijst files in dir: {length(all_in_dir)}",
               " [{paste(basename(all_in_dir), collapse=', ')}]"))

  candidates <- all_in_dir[grepl(prefix, basename(all_in_dir), fixed = TRUE)]

  if (length(candidates) == 0) {
    message(glue("    No treklijst found for prefix '{prefix}' in {dir} — haul_id will be NA"))
    return(NULL)
  }
  if (length(candidates) > 1) {
    message(glue("    Multiple treklijst files found for '{prefix}' — using first: ",
                 "{basename(candidates[1])}"))
  } else {
    message(glue("    Auto-detected treklijst: {basename(candidates[1])}"))
  }
  candidates[1]
}


#' Read the Haul sheet from a treklijst file for kisten haul assignment.
#' Returns NULL (with message) if the file or Haul sheet is absent.
read_treklijst_for_kisten <- function(treklijst_path) {
  if (is.null(treklijst_path) || is.na(treklijst_path) ||
      !file.exists(treklijst_path)) {
    message("    Treklijst file not found — haul_id will be NA")
    return(NULL)
  }

  sheets <- tryCatch(excel_sheets(treklijst_path), error = function(e) character(0))
  if (!"Haul" %in% sheets) {
    message(glue("    No 'Haul' sheet in '{basename(treklijst_path)}' — haul_id will be NA"))
    return(NULL)
  }

  haul_sheet <- read_excel(treklijst_path, sheet = "Haul", col_types = "list")

  # Normalise multi-line header names
  names(haul_sheet) <- names(haul_sheet) %>%
    gsub("\n", " ", .) %>%
    trimws() %>%
    gsub("[[:space:]]+", "_", .) %>%
    tolower()

  # Identify the computed shoot-start time column
  shoot_col <- grep("bereken.*begin.*uitzetten|bereken.*shoot.*begin",
                    names(haul_sheet), value = TRUE)[1]
  catch_col <- grep("totale_vangst|total_catch|vangst.*kg",
                    names(haul_sheet), value = TRUE)[1]

  if (is.na(shoot_col)) {
    message("    'bereken tijd begin uitzetten' column not found in treklijst — haul_id will be NA")
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


#' Reconstruct full POSIXct shoot datetimes from the treklijst Haul sheet.
#' The treklijst stores a full datetime for the first haul of each day and
#' only a numeric time fraction (Excel serial) for subsequent hauls.
#' This function propagates the date forward row by row.
reconstruct_shoot_datetimes <- function(hauls) {
  # readxl with col_types="list" returns:
  #   Full datetime cells  -> POSIXct with the correct date, e.g. 2026-03-16 10:45
  #   Time-only cells      -> POSIXct with placeholder date 1899-12-30, e.g. 1899-12-30 12:15
  #   Numeric fraction     -> raw 0..1 double (rare, depends on readxl version)
  #
  # We must distinguish full datetimes from time-only placeholders by checking
  # whether the date component is the Excel epoch (1899-12-30 or 1899-12-31).
  EXCEL_EPOCH <- as.Date("1899-12-30")
  EXCEL_EPOCH2 <- as.Date("1899-12-31")  # readxl off-by-one variant

  raw <- hauls[["shoot_datetime"]]
  n   <- length(raw)
  out <- vector("list", n)
  current_date <- NULL

  for (i in seq_len(n)) {
    v <- raw[[i]]

    if (is.null(v) || (length(v) == 1 && is.na(v))) {
      out[[i]] <- as.POSIXct(NA)
      next
    }

    # Distinguish full datetime from time-only placeholder
    is_posixct     <- inherits(v, "POSIXct")
    cell_date      <- if (is_posixct) as.Date(v, tz = "UTC") else NA
    is_placeholder <- is_posixct && !is.na(cell_date) &&
                      (cell_date == EXCEL_EPOCH | cell_date == EXCEL_EPOCH2)
    is_full_dt     <- is_posixct && !is.na(cell_date) && !is_placeholder

    if (is_full_dt) {
      # Full datetime with a real date — use directly and update current_date
      current_date <- cell_date
      out[[i]]     <- v

    } else if ((is_placeholder || is.numeric(v)) && !is.null(current_date)) {
      # Time-only: extract HH:MM:SS and combine with the propagated date
      if (is_placeholder) {
        hms_str <- format(v, "%H:%M:%S", tz = "UTC")
      } else {
        # Raw numeric fraction of day
        hms_str <- format(as.POSIXct(v * 86400, origin = "1899-12-30", tz = "UTC"),
                          "%H:%M:%S")
      }
      dt <- as.POSIXct(paste(as.character(current_date), hms_str),
                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

      # Roll to next day if time goes backwards by more than 2 hours
      prev <- if (i > 1) out[[i-1]] else NULL
      if (!is.null(prev) && length(prev) == 1 && !is.na(prev) && dt < prev - 7200) {
        current_date <- current_date + 1L
        dt <- as.POSIXct(paste(as.character(current_date), hms_str),
                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      }
      out[[i]] <- dt

    } else {
      out[[i]] <- as.POSIXct(NA)
    }
  }
  do.call(c, out)
}


#' Match weighing sessions to treklijst hauls using haul end times.
#'
#' Each session is matched to the haul whose estimated end time is closest
#' to and before the session start. Haul end time = min(shoot + 80min, next shoot).
#' This is more robust than rank-order matching because it handles cases where
#' the treklijst and kisten have different numbers of entries.
#'
#' @param sessions   tibble with session_id, session_start
#' @param haul_ids   integer vector of haul numbers
#' @param shoot_dts  POSIXct vector of shoot datetimes (same length as haul_ids)
#' @param max_gap_min  maximum allowed gap between haul_end and session_start (minutes)
#' @return named integer vector: names = session_id, values = haul_id
match_sessions_to_hauls <- function(sessions, haul_ids, shoot_dts,
                                    max_gap_min   = 90,
                                    early_start_min = KISTEN_EARLY_START_MIN) {
  n_s <- nrow(sessions)
  n_h <- length(haul_ids)

  stopifnot(n_s >= 1, n_h >= 1)

  # Haul end = min(shoot + 80 min, next shoot) — computed as numeric then cast to POSIXct
  shoot_num_m  <- as.numeric(shoot_dts)
  next_num_m   <- c(shoot_num_m[-1], shoot_num_m[n_h] + 80 * 60)
  haul_end_dts <- as.POSIXct(
    pmin(shoot_num_m + 80 * 60, next_num_m, na.rm = TRUE),
    origin = "1970-01-01", tz = "UTC"
  )

  message(glue("    Haul assignment: matching {n_s} sessions to {n_h} hauls by haul end time"))

  # Result vector: session index -> haul_id value (NA_integer_ = unassigned)
  mapping  <- rep(NA_integer_, n_s)
  used_idx <- logical(n_h)

  for (i in seq_len(n_s)) {
    ss       <- sessions$session_start[[i]]
    best_j   <- NA_integer_
    best_gap <- Inf

    for (j in seq_len(n_h)) {
      if (used_idx[j]) next
      he <- haul_end_dts[[j]]
      if (is.na(he)) next
      gap <- as.numeric(difftime(ss, he, units = "mins"))
      if (!is.finite(gap)) next
      if (gap >= -early_start_min && gap <= max_gap_min && gap < best_gap) {
        best_j   <- j
        best_gap <- gap
      }
    }

    # Fallback: no haul ended cleanly within max_gap_min before this session.
    # Only use hauls that ended before the session (positive gap) within 3x the window.
    # If nothing qualifies, skip — this session will remain unassigned.
    if (is.na(best_j)) {
      remaining <- which(!used_idx)
      sid   <- sessions$session_id[[i]]
      stime <- format(ss, "%m-%d %H:%M")
      if (length(remaining) == 0) {
        best_j <- n_h
        message(glue("    \u26a0 Session {sid} ({stime}) — all hauls assigned, ",
                     "appended to last haul {haul_ids[[n_h]]}"))
      } else {
        gaps     <- as.numeric(difftime(ss, haul_end_dts[remaining], units = "mins"))
        # Only positive gaps (haul ended before session) within 3x the window
        eligible <- which(is.finite(gaps) & gaps >= 0 & gaps <= max_gap_min * 3)
        if (length(eligible) > 0) {
          best_j <- remaining[[ eligible[[ which.min(gaps[eligible]) ]] ]]
          message(glue("    \u26a0 Session {sid} ({stime}) — extended fallback to ",
                       "haul {haul_ids[[best_j]]} (gap {round(gaps[eligible[[which.min(gaps[eligible])]]],0)} min)"))
        } else {
          # No suitable haul — skip this session (it will show as unassigned)
          message(glue("    \u26a0 Session {sid} ({stime}) — no haul match, skipped"))
          next
        }
      }
    }

    # Final type-safe guard — should never fire but prevents any crash
    best_j <- as.integer(best_j)[[1]]
    if (is.na(best_j) || best_j < 1L || best_j > n_h) {
      message(glue("    \u26a0 Session {sessions$session_id[[i]]} — assignment failed, skipping"))
      next
    }

    mapping[[i]]   <- haul_ids[[best_j]]
    used_idx[best_j] <- TRUE
  }

  setNames(mapping, sessions$session_id)
}



#' Assign haul IDs to kisten rows using treklijst shoot times.
#' Called from get_catch_from_kisten() when a treklijst is available.
#'
#' @param kisten     data.frame with column weighing_time (POSIXct, sorted asc)
#' @param treklijst  data.frame: Haul sheet with columns haul, shoot_datetime, catch_kg
#' @param local_tz   IANA timezone string
#' @return list: $kisten (with haul_id, session_id, haul_flag) and $summary
assign_haul_ids_from_treklijst <- function(kisten, treklijst,
                                           local_tz  = "Europe/Amsterdam",
                                           gap_min   = KISTEN_GAP_THRESHOLD_MIN,
                                           tolerance = KISTEN_TOLERANCE_MIN,
                                           flag_min  = KISTEN_FLAG_MIN) {

  stopifnot("weighing_time" %in% names(kisten))
  stopifnot(all(c("haul", "shoot_datetime", "catch_kg") %in% names(treklijst)))

  sep <- strrep("-", 60)

  # ── 1. Reconstruct full shoot datetimes ─────────────────────────────────
  trek <- treklijst %>%
    filter(!is.na(haul), !is.na(shoot_datetime)) %>%
    mutate(haul = as.integer(haul)) %>%
    arrange(haul)

  trek$shoot_dt <- reconstruct_shoot_datetimes(trek)
  trek <- trek %>% filter(!is.na(shoot_dt))

  haul_ids  <- trek$haul
  shoot_dts <- trek$shoot_dt
  n_h       <- length(haul_ids)

  dropped <- nrow(treklijst) - nrow(trek)
  message(glue("    Treklijst: {nrow(trek)} hauls with valid shoot times, {dropped} dropped (no shoot time)"))

  # ── 2. Detect weighing sessions in kisten ───────────────────────────────
  kisten <- kisten %>%
    arrange(weighing_time) %>%
    mutate(
      time_gap_min = as.numeric(difftime(weighing_time,
                                         lag(weighing_time), units = "mins")),
      session_id   = cumsum(replace_na(time_gap_min > gap_min, TRUE))
    )

  sessions <- kisten %>%
    group_by(session_id) %>%
    summarise(session_start = min(weighing_time),
              session_end   = max(weighing_time),
              n_rows        = n(), .groups = "drop") %>%
    dplyr::mutate(
      session_start = as.POSIXct(as.numeric(session_start), origin = "1970-01-01", tz = "UTC"),
      session_end   = as.POSIXct(as.numeric(session_end),   origin = "1970-01-01", tz = "UTC")
    )

  n_s <- nrow(sessions)
  message(glue("    Kisten: {nrow(kisten)} box records, {n_s} weighing sessions detected (gap threshold = {gap_min} min)"))

  # ── 3. Match sessions → hauls ────────────────────────────────────────────
  if (n_s == n_h) {
    message(glue("    Haul assignment: {n_s} sessions = {n_h} hauls (direct match)"))
  } else {
    message(glue("    Haul assignment: temporal matching ({n_s} sessions ≠ {n_h} hauls — using anchor proximity)"))
  }

  mapping  <- match_sessions_to_hauls(sessions, haul_ids, shoot_dts, tolerance)
  sessions <- sessions %>%
    mutate(haul_id = mapping[as.character(session_id)])

  # ── 4. Compute anchor offsets and flags ─────────────────────────────────
  haul_anchor_tbl <- tibble::tibble(
    haul_id    = haul_ids,
    haul_idx   = seq_along(haul_ids),
    haul_shoot = shoot_dts,
    anchor_dt  = c(shoot_dts[-1], as.POSIXct(NA))
  ) %>%
    dplyr::mutate(
      haul_shoot  = as.POSIXct(as.numeric(haul_shoot), origin = "1970-01-01", tz = "UTC"),
      anchor_dt   = as.POSIXct(as.numeric(anchor_dt),  origin = "1970-01-01", tz = "UTC"),
      haul_end_dt = as.POSIXct(
        pmin(as.numeric(haul_shoot) + 80 * 60,
             c(as.numeric(shoot_dts[-1]), as.numeric(shoot_dts[n_h]) + 80 * 60),
             na.rm = TRUE),
        origin = "1970-01-01", tz = "UTC"
      )
    )

  sessions <- sessions %>%
    left_join(haul_anchor_tbl, by = "haul_id") %>%
    mutate(
      anchor_offset_min = as.numeric(difftime(
        as.POSIXct(as.numeric(session_start), origin = "1970-01-01", tz = "UTC"),
        as.POSIXct(as.numeric(haul_end_dt),   origin = "1970-01-01", tz = "UTC"),
        units = "mins"
      )),
      haul_flag = !is.na(anchor_offset_min) &
                  (anchor_offset_min < -KISTEN_EARLY_START_MIN | anchor_offset_min > flag_min)
    )

  # ── 5. Attach haul_id and flag to individual kisten rows ─────────────────
  kisten <- kisten %>%
    left_join(sessions %>% select(session_id, haul_id, haul_flag),
              by = "session_id")

  n_flagged <- sessions %>% filter(!is.na(haul_flag) & haul_flag) %>% nrow()
  if (n_flagged > 0)
    message(glue("    ⚠ {n_flagged} session(s) flagged for review (offset > {flag_min} min)"))

  n_assigned <- sessions %>% filter(!is.na(haul_id)) %>% nrow()
  n_empty    <- n_h - n_assigned
  message(glue("    Assignment complete: {n_assigned} hauls with weighings, {n_empty} empty hauls (treklijst only)"))
  message(glue("    Processed {nrow(kisten)} box records across {n_assigned} hauls"))

  # ── 6. Per-haul summary ───────────────────────────────────────────────────
  kisten_summary <- kisten %>%
    group_by(haul_id) %>%
    summarise(n_weighings    = n(),
              weighing_start = min(weighing_time),
              weighing_end   = max(weighing_time),
              weighed_kg     = sum(weight_kg, na.rm = TRUE),
              haul_flag      = any(haul_flag == TRUE, na.rm = TRUE),
              .groups = "drop") %>%
    dplyr::mutate(haul_flag = as.logical(haul_flag))

  summary <- tryCatch({
    trek %>%
      select(haul_id = haul, shoot_dt, trek_catch_kg = catch_kg) %>%
      left_join(kisten_summary, by = "haul_id") %>%
      left_join(
        sessions %>%
          dplyr::filter(!is.na(haul_id)) %>%
          dplyr::group_by(haul_id) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(haul_id, anchor_offset_min),
        by = "haul_id"
      ) %>%
      dplyr::mutate(
        is_empty_haul = is.na(n_weighings) | n_weighings == 0,
        n_weighings   = dplyr::coalesce(as.integer(n_weighings), 0L),
        weighed_kg    = dplyr::coalesce(as.double(weighed_kg),   0.0),
        trek_catch_kg = as.double(trek_catch_kg),
        haul_flag     = dplyr::coalesce(haul_flag, FALSE),
        weight_ratio  = dplyr::if_else(
          !is.na(trek_catch_kg) & trek_catch_kg > 0,
          weighed_kg / trek_catch_kg,
          NA_real_
        ),
        quality_flag  = dplyr::case_when(
          is_empty_haul                                      ~ "empty",
          haul_flag                                          ~ "offset",
          !is.na(weight_ratio) & weight_ratio < 0.50        ~ "weight_low",
          !is.na(weight_ratio) & weight_ratio > 1.50        ~ "weight_high",
          TRUE                                               ~ "ok"
        )
      ) %>%
      dplyr::arrange(haul_id)
  }, error = function(e) {
    message(glue("    ⚠ Summary build failed: {e$message}"))
    trek %>%
      select(haul_id = haul, shoot_dt, trek_catch_kg = catch_kg) %>%
      left_join(kisten_summary %>%
                  dplyr::select(haul_id, n_weighings, weighed_kg, haul_flag),
                by = "haul_id") %>%
      dplyr::mutate(is_empty_haul = is.na(n_weighings) | n_weighings == 0)
  })

  # ── 7. Print quality report ─────────────────────────────────────────────────────
  expected_cols <- c("is_empty_haul", "weight_ratio", "quality_flag")
  missing_cols  <- setdiff(expected_cols, names(summary))
  if (length(missing_cols) == 0) {
    detail_cols <- c("haul_id", "trek_catch_kg", "weighed_kg",
                     "weight_ratio", "anchor_offset_min", "quality_flag")
    summary %>%
      dplyr::filter(quality_flag != "ok") %>%
      dplyr::select(dplyr::any_of(detail_cols)) %>%
      print(n = 100)

    problems <- summary %>% filter(quality_flag != "ok")
    if (nrow(problems) > 0) {
      message(glue("
  ⚠ {nrow(problems)} haul(s) need attention:"))
      problems %>%
        select(dplyr::any_of(detail_cols)) %>%
        dplyr::mutate(
          dplyr::across(dplyr::any_of("trek_catch_kg"),     ~round(as.double(.), 1)),
          dplyr::across(dplyr::any_of("weighed_kg"),        ~round(as.double(.), 1)),
          dplyr::across(dplyr::any_of("weight_ratio"),      ~round(as.double(.), 2)),
          dplyr::across(dplyr::any_of("anchor_offset_min"), ~round(as.double(.), 0))
        ) %>%
        print(n = 100)
    } else {
      message("  ✓ All haul assignments look good.")
    }
  }
  message(sep)

  list(kisten = kisten, summary = summary)
}


# ==============================================================================
# UTILITY: remove_flyshoot_data
# ==============================================================================
#
#' Remove all records for a vessel + date range from every parquet datastore.
#'
#' Use this before reprocessing a trip to ensure clean re-ingestion without
#' duplicate or conflicting trip_ids.
#'
#' @param vessel_id  Vessel identifier string, e.g. "SCH65"
#' @param date_from  Start date (inclusive), "YYYY-MM-DD" or Date
#' @param date_to    End date   (inclusive), "YYYY-MM-DD" or Date
#' @param flyshoot_root  Path to the parquet root directory.
#'                       Defaults to ONEDRIVE_FLYSHOOT env variable.
#' @param dry_run    If TRUE (default), print what would be removed without
#'                   writing anything.
#' @return Invisible named list with row counts per dataset.
#'
#' @examples
#' # Preview
#' remove_flyshoot_data("SCH65", "2026-03-09", "2026-03-12")
#'
#' # Apply
#' remove_flyshoot_data("SCH65", "2026-03-09", "2026-03-12", dry_run = FALSE)
remove_flyshoot_data <- function(
    vessel_id,
    date_from     = NULL,
    date_to       = NULL,
    trip_ids      = NULL,   # optional: filter by specific trip_id(s) instead of / in addition to dates
    flyshoot_root = Sys.getenv("ONEDRIVE_FLYSHOOT"),
    dry_run       = TRUE
) {

  vessel_ids <- as.character(vessel_id)
  sep        <- strrep("=", 60)

  # Build description for header message
  filter_desc <- if (!is.null(trip_ids)) {
    glue("trip_id={paste(trip_ids, collapse=', ')}")
  } else {
    glue("{format(as.Date(date_from))} to {format(as.Date(date_to))}")
  }

  message(sep)
  message("Removing data: vessel=", paste(vessel_ids, collapse = ", "),
          " | ", filter_desc)
  message("dry_run = ", dry_run)
  message(sep)

  if (!nzchar(flyshoot_root))
    stop("flyshoot_root is empty — set ONEDRIVE_FLYSHOOT or pass it directly")

  if (is.null(trip_ids) && (is.null(date_from) || is.null(date_to)))
    stop("Provide either trip_ids or both date_from and date_to")

  date_from <- if (!is.null(date_from)) as.Date(date_from) else NULL
  date_to   <- if (!is.null(date_to))   as.Date(date_to)   else NULL

  # Dataset name -> date column to filter on
  datasets <- list(
    haul            = "date",
    kisten          = "date",
    elog            = "date",
    elog_trek       = "date",
    trip            = "departure_date",
    vessel_movement = "date"
  )

  results <- list()

  for (ds_name in names(datasets)) {

    date_col <- datasets[[ds_name]]
    pq_path  <- file.path(flyshoot_root, ds_name,
                           glue("{ds_name}.parquet"))

    if (!file.exists(pq_path)) {
      message("  [", ds_name, "] SKIP — file not found")
      next
    }

    df       <- arrow::read_parquet(pq_path)
    n_before <- nrow(df)

    # kisten may store date only in weighing_time
    added_date_col <- FALSE
    if (!"date" %in% names(df) && "weighing_time" %in% names(df)) {
      df             <- df %>% dplyr::mutate(date = as.Date(weighing_time))
      if (date_col == "date") added_date_col <- TRUE
    }

    # Build filter: by trip_id if provided, else by date range
    if (!is.null(trip_ids)) {
      if (!"trip_id" %in% names(df)) {
        message("  [", ds_name, "] SKIP — no trip_id column")
        next
      }
      target <- df %>%
        dplyr::filter(vessel %in% vessel_ids, trip_id %in% trip_ids)
    } else {
      if (!date_col %in% names(df)) {
        message("  [", ds_name, "] SKIP — date column '", date_col, "' not found")
        next
      }
      target <- df %>%
        dplyr::filter(
          vessel %in% vessel_ids,
          as.Date(.data[[date_col]]) >= date_from,
          as.Date(.data[[date_col]]) <= date_to
        )
    }
    n_remove <- nrow(target)

    message("  [", ds_name, "] rows before: ", n_before,
            " | to remove: ", n_remove)

    if (n_remove > 0 && "trip_id" %in% names(target)) {
      trips <- paste(sort(unique(target$trip_id)), collapse = ", ")
      message("           trip_ids affected: ", trips)
    }

    if (!dry_run && n_remove > 0) {

      if (added_date_col) df <- dplyr::select(df, -date)

      df_clean <- if (!is.null(trip_ids)) {
        df %>% dplyr::filter(!(vessel %in% vessel_ids & trip_id %in% trip_ids))
      } else {
        df %>% dplyr::filter(!(
          vessel %in% vessel_ids &
          as.Date(.data[[date_col]]) >= date_from &
          as.Date(.data[[date_col]]) <= date_to
        ))
      }

      tmp_path <- paste0(pq_path, ".tmp")
      arrow::write_parquet(df_clean, tmp_path)
      rm(df, df_clean, target)
      gc()
      file.rename(tmp_path, pq_path)
      message("           WRITTEN — rows after: ", n_before - n_remove)
    } else {
      rm(df, target)
      gc()
    }

    results[[ds_name]] <- list(before = n_before, removed = n_remove)
  }

  message(sep)
  message(if (dry_run) "DRY RUN complete — no files modified."
          else "DONE — parquet files updated.")

  invisible(results)
}


# ==============================================================================
# UTILITY: get_added_data_stats
# ==============================================================================
#' Print a summary table of data added in the current processing run.
#'
#' Shows one row per vessel with:
#'   - the source file types that were processed (e.g. "elog; treklijst; kisten")
#'   - the number of records added to each parquet datastore
#'
#' @param hauls,kisten,elogs,elog_treks,trips  The all_new_* lists from the workflow
#' @param inventory  file_inventory tibble (vessel, source columns)
get_added_data_stats <- function(hauls      = list(),
                                 kisten     = list(),
                                 elogs      = list(),
                                 elog_treks = list(),
                                 trips      = list(),
                                 inventory  = tibble::tibble()) {

  sep <- strrep("=", 70)
  message(sep)
  message("DATA ADDED THIS RUN")
  message(sep)

  # File types processed per vessel (from file_inventory)
  sources_per_vessel <- if (nrow(inventory) > 0 &&
                             all(c("vessel", "source") %in% names(inventory))) {
    inventory %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarise(sources = paste(sort(unique(source)), collapse = "; "),
                       .groups = "drop")
  } else {
    tibble::tibble(vessel = character(), sources = character())
  }

  # One row per vessel × data_type
  count_by_vessel <- function(lst, type_label) {
    if (length(lst) == 0) return(tibble::tibble())
    dplyr::bind_rows(lst) %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarise(
        n_records   = dplyr::n(),
        oldest_date = min(as.Date(date), na.rm = TRUE),
        newest_date = max(as.Date(date), na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      dplyr::mutate(data_type = type_label)
  }

  # trip parquet uses departure_date not date
  count_trips <- function(lst) {
    if (length(lst) == 0) return(tibble::tibble())
    dplyr::bind_rows(lst) %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarise(
        n_records   = dplyr::n(),
        oldest_date = min(as.Date(departure_date), na.rm = TRUE),
        newest_date = max(as.Date(departure_date), na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      dplyr::mutate(data_type = "trip")
  }

  stats <- dplyr::bind_rows(
    count_by_vessel(hauls,      "haul"),
    count_by_vessel(kisten,     "kisten"),
    count_by_vessel(elogs,      "elog"),
    count_by_vessel(elog_treks, "elog_trek"),
    count_trips(trips)
  )

  if (nrow(stats) == 0) {
    message("  No new data added.")
    message(sep)
    return(invisible(NULL))
  }

  stats <- stats %>%
    dplyr::left_join(sources_per_vessel, by = "vessel") %>%
    dplyr::select(vessel, sources, data_type, n_records, oldest_date, newest_date) %>%
    dplyr::arrange(vessel, factor(data_type,
                   levels = c("haul", "kisten", "elog", "elog_trek", "trip")))

  print(stats, n = 50)
  message(sep)
  invisible(stats)
}


# ==============================================================================
# DIAGNOSTIC: compare_data_sources
# ==============================================================================
#' Compare catch totals between elog, elog_trek and kisten for a vessel/trip.
#'
#' Prints comparison tables by day, by species and by area (ices_rect /
#' economic_zone). Flags rows where the relative difference exceeds a threshold.
#'
#' @param vessel_id  Vessel identifier, e.g. "SCH135"
#' @param trip_id    Trip identifier, e.g. "2026031600015"
#' @param flyshoot_root  Path to parquet root. Defaults to ONEDRIVE_FLYSHOOT.
#' @param threshold  Relative difference threshold for flagging (default 0.10 = 10%)
#' @param sources    Which sources to compare. Default c("elog","elog_trek","kisten")
compare_data_sources <- function(
    vessel_id,
    trip_id,
    flyshoot_root = Sys.getenv("ONEDRIVE_FLYSHOOT"),
    threshold     = 0.10,
    sources       = c("elog", "elog_trek", "kisten")
) {
  sep  <- strrep("=", 70)
  sep2 <- strrep("-", 70)

  message(sep)
  message(glue("DATA SOURCE COMPARISON  |  {vessel_id}  |  trip {trip_id}"))
  message(sep)

  if (!nzchar(flyshoot_root))
    stop("flyshoot_root is empty — set ONEDRIVE_FLYSHOOT or pass it directly")

  # ── Load requested sources ─────────────────────────────────────────────────
  load_src <- function(type) {
    pq <- file.path(flyshoot_root, type, glue("{type}.parquet"))
    if (!file.exists(pq)) return(NULL)
    tryCatch(
      arrow::read_parquet(pq) %>%
        dplyr::filter(vessel == vessel_id, trip_id == !!trip_id),
      error = function(e) NULL
    )
  }

  dat <- list()
  if ("elog"      %in% sources) dat[["elog"]]      <- load_src("elog")
  if ("elog_trek" %in% sources) dat[["elog_trek"]] <- load_src("elog_trek")
  if ("kisten"    %in% sources) dat[["kisten"]]    <- load_src("kisten")

  # Drop sources with no data
  dat <- Filter(function(x) !is.null(x) && nrow(x) > 0, dat)

  if (length(dat) < 2) {
    message("  Less than 2 sources have data — nothing to compare.")
    return(invisible(NULL))
  }

  src_names <- names(dat)
  message(glue("  Sources with data: {paste(src_names, collapse = ', ')}"))
  for (nm in src_names) {
    message(glue("  {nm}: {nrow(dat[[nm]])} rows, ",
                 "{sum(dat[[nm]]$weight_kg, na.rm=TRUE) |> round()} kg total"))
  }

  # ── Helper: aggregate one source by grouping columns ──────────────────────
  agg <- function(df, grp_cols) {
    df %>%
      dplyr::select(dplyr::any_of(c(grp_cols, "weight_kg"))) %>%
      dplyr::filter(!is.na(weight_kg)) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(grp_cols))) %>%
      dplyr::summarise(kg = round(sum(weight_kg, na.rm = TRUE), 1),
                       .groups = "drop")
  }

  # ── Helper: wide comparison table with flag column ─────────────────────────
  compare_wide <- function(grp_cols, label) {
    message(sep2)
    message(glue("  BY {toupper(label)}"))
    message(sep2)

    # Build one aggregated tibble per source, rename kg to source name,
    # then iteratively full-join all sources together.
    wide_list <- vector("list", length(src_names))
    for (si in seq_along(src_names)) {
      wide_list[[si]] <- agg(dat[[src_names[si]]], grp_cols) %>%
        dplyr::rename(!!src_names[si] := kg)
    }
    wide <- wide_list[[1]]
    if (length(wide_list) > 1) {
      for (si in 2:length(wide_list)) {
        wide <- dplyr::full_join(wide, wide_list[[si]],
                                 by = intersect(names(wide), names(wide_list[[si]])))
      }
    }
    wide <- wide %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(src_names),
                                  ~tidyr::replace_na(., 0))) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(grp_cols)))

    # abs_diff = max - min across all sources per row
    # pct_contrib = abs_diff as % of total absolute difference in this table
    if (length(src_names) >= 2) {
      wide <- wide %>%
        dplyr::mutate(
          max_val  = pmax(!!!dplyr::syms(src_names), na.rm = TRUE),
          min_val  = pmin(!!!dplyr::syms(src_names), na.rm = TRUE),
          abs_diff = round(max_val - min_val, 1)
        ) %>%
        dplyr::select(-max_val, -min_val)

      total_abs_diff <- sum(wide$abs_diff, na.rm = TRUE)
      wide <- wide %>%
        dplyr::mutate(
          pct_contrib = if (total_abs_diff > 0)
            paste0(round(100 * abs_diff / total_abs_diff, 1), "%")
          else
            rep("0%", dplyr::n())
        )
    }

    total_diff <- if ("abs_diff" %in% names(wide)) sum(wide$abs_diff, na.rm = TRUE) else 0

    if (nrow(wide) == 0) {
      message("  (no data)")
    } else {
      # Add totals row: sum numeric source and abs_diff columns
      num_cols  <- intersect(c(src_names, "abs_diff"), names(wide))
      total_row <- wide %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(num_cols), ~sum(., na.rm = TRUE))) %>%
        dplyr::mutate(
          dplyr::across(dplyr::any_of(grp_cols), ~"TOTAAL"),
          pct_contrib = if ("pct_contrib" %in% names(wide)) "100%" else NULL
        )
      wide_print <- dplyr::bind_rows(wide, total_row)
      print(wide_print, n = 100)
      message(glue("\n  Total absolute difference: {round(total_diff, 1)} kg"))
    }
    invisible(wide)
  }

  # ── 1. By day ─────────────────────────────────────────────────────────────
  result_day <- compare_wide("date", "day")

  # ── 2. By species ─────────────────────────────────────────────────────────
  result_sp  <- compare_wide("species_code", "species")

  # ── 3. By area ────────────────────────────────────────────────────────────
  # Use ices_rect if present, else economic_zone
  area_col <- if (any(sapply(dat, function(d) "ices_rect" %in% names(d) &&
                                               sum(!is.na(d$ices_rect)) > 0)))
    "ices_rect" else "economic_zone"

  result_area <- compare_wide(area_col, glue("area ({area_col})"))

  message(sep)
  invisible(list(by_day = result_day, by_species = result_sp, by_area = result_area))
}


# ==============================================================================
# PIPELINE UTILITIES (used by 03_main_workflow.R)
# ==============================================================================

#' Suppress messages unless verbose mode is on.
#' verbose must be defined in the calling environment (set at top of 03_main_workflow.R).
quietly <- function(expr) {
  if (exists("verbose", envir = parent.env(environment()), inherits = TRUE) &&
      isTRUE(get("verbose", envir = parent.env(environment()), inherits = TRUE))) {
    expr
  } else {
    suppressMessages(expr)
  }
}

#' Convert ICES rectangle names to lon/lat midpoints.
#' Implements the exact mapplots::ices.rect formula (Hans Gerritsen, CRAN).
#' rx = paste0(1-based letter index, digit) as integer; lon = rx - 59.5
#' lat = (row_num + 71.5) / 2
#' @param rect character vector of ICES rectangle names (e.g. "32F3", "29E9")
#' @return data.frame with columns lon and lat (midpoints); NA for invalid rects
ices_rect_centroid <- function(rect) {
  rect  <- trimws(toupper(rect))
  valid <- grepl("^[0-9]{2}[A-Z][0-9]$", rect)
  lon   <- rep(NA_real_, length(rect))
  lat   <- rep(NA_real_, length(rect))
  # Exact mapplots::ices.rect formula (Hans Gerritsen)
  # rx = paste0(1-based letter index, digit) as integer; lon = rx - 59.5
  # lat = (row_num + 71.5) / 2
  if (any(valid)) {
    row_num  <- as.integer(substr(rect[valid], 1, 2))
    col_lett <- match(substr(rect[valid], 3, 3), LETTERS)   # 1-based: A=1,E=5,F=6...
    col_dig  <- substr(rect[valid], 4, 4)
    rx       <- as.integer(paste0(col_lett, col_dig))
    lon[valid] <- rx - 59.5
    lat[valid] <- (row_num + 71.5) / 2
  }
  data.frame(lon = lon, lat = lat)
}

#' Look up port coordinates from a places lookup table.
#' @param port_vec character vector of port names or harbour codes
#' @param coord_col "lon" or "lat"
#' @param places data.frame with columns name, lon, lat, and optionally harbour_code
lookup_port_coord <- function(port_vec, coord_col, places) {
  p_up <- toupper(port_vec)
  n_up <- toupper(places$name)
  h_up <- if ("harbour_code" %in% names(places))
             toupper(places$harbour_code) else character(0)
  vapply(p_up, function(p) {
    if (is.na(p)) return(NA_real_)
    idx <- match(p, n_up)
    if (is.na(idx) && length(h_up)) idx <- match(p, h_up)
    if (is.na(idx)) NA_real_ else places[[coord_col]][idx]
  }, numeric(1))
}

# ==============================================================================
# KISTEN -> ELOG_TREK PROMOTION HELPER
# ==============================================================================

#' Promote kisten box records to elog_trek shape, adding haul positions.
#'
#' kisten records produced by get_catch_from_kisten() already carry
#' weighing_time (the Marelec per-box timestamp). elog_trek additionally
#' expects shoot_lat / shoot_lon from the haul table. This helper joins those
#' positions and selects the canonical elog_trek column set so that
#' treklijst-based trips populate elog_trek with real weighing timestamps.
#'
#' @param kisten  tibble returned by get_catch_from_kisten() (or NULL)
#' @param haul    tibble returned by get_haul_from_treklijst() (or NULL)
#' @return tibble shaped like elog_trek, or NULL when kisten is NULL / empty
kisten_to_elog_trek <- function(kisten, haul = NULL) {

  if (is.null(kisten) || nrow(kisten) == 0) return(NULL)

  if (!is.null(haul) && nrow(haul) > 0 &&
      all(c("haul_id", "shoot_lat", "shoot_lon") %in% names(haul))) {

    pos <- haul %>%
      dplyr::select(vessel, trip_id, haul_id,
                    shoot_lat, shoot_lon,
                    dplyr::any_of(c("ices_rect", "economic_zone", "fao_division")))

    elog_trek <- kisten %>%
      dplyr::left_join(pos, by = c("vessel", "trip_id", "haul_id"))

  } else {
    elog_trek <- kisten %>%
      dplyr::mutate(shoot_lat = NA_real_, shoot_lon = NA_real_)
  }

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
# DIAGNOSTIC: weighing_time coverage in elog_trek parquet
# ==============================================================================

#' Report weighing_time completeness in the elog_trek parquet.
#'
#' Prints an overall summary and a per-vessel breakdown. For vessels / trips
#' where weighing_time is NA, also lists the affected dates so you can see
#' which raw files need to be reprocessed.
#'
#' @return invisibly the full elog_trek tibble (for further inspection)
diagnose_weighing_time <- function() {

  et <- tryCatch(load_flyshoot_data("elog_trek"), error = function(e) NULL)

  if (is.null(et) || nrow(et) == 0) {
    message("elog_trek parquet is empty or could not be loaded.")
    return(invisible(NULL))
  }

  total <- nrow(et)

  if (!"weighing_time" %in% names(et)) {
    message(glue("weighing_time column is ABSENT from elog_trek ({total} rows total)."))
    message("Reprocess all raw files to populate it.")
    return(invisible(et))
  }

  n_na <- sum(is.na(et$weighing_time))
  n_ok <- total - n_na

  message(glue("elog_trek weighing_time coverage"))
  message(glue("  Total rows : {total}"))
  message(glue("  Populated  : {n_ok}  ({round(100 * n_ok / total, 1)}%)"))
  message(glue("  NA (empty) : {n_na}  ({round(100 * n_na / total, 1)}%)"))

  # Per-vessel summary
  if ("vessel" %in% names(et)) {
    message("\nBreakdown by vessel:")
    breakdown <- et %>%
      dplyr::mutate(wt_ok = !is.na(weighing_time)) %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarise(
        n_rows   = dplyr::n(),
        n_ok     = sum(wt_ok),
        n_na     = sum(!wt_ok),
        pct_na   = round(100 * sum(!wt_ok) / dplyr::n(), 1),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(pct_na))

    print(breakdown, n = 50)
  }

  # Dates with NA weighing_time — tells you exactly which trips to reprocess
  if (n_na > 0 && "date" %in% names(et)) {
    message("\nDates with NA weighing_time (trips to reprocess):")
    na_dates <- et %>%
      dplyr::filter(is.na(weighing_time)) %>%
      dplyr::group_by(vessel, trip_id, date) %>%
      dplyr::summarise(
        n_na_rows = dplyr::n(),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(vessel, date)

    print(na_dates, n = 200)
  }

  invisible(et)
}




# ==============================================================================
# DIAGNOSTIC: diagnose_parquet_coverage
# ==============================================================================
#' Compare kisten/haul parquet coverage against each other and optionally
#' against old RData files, to understand why discard trend plots look sparse.
#'
#' Run interactively:
#'   diagnose_parquet_coverage("SCH99")
#'   diagnose_parquet_coverage("SCH99",
#'     rdata_haul_path   = "path/to/haul.RData",
#'     rdata_kisten_path = "path/to/kisten.RData")
#'
#' @param vessel_id           Vessel to diagnose
#' @param flyshoot_root       Path to parquet root (default ONEDRIVE_FLYSHOOT env var)
#' @param rdata_haul_path     Optional path to old haul.RData file
#' @param rdata_kisten_path   Optional path to old kisten.RData file
#' @param rdata_haul_obj      Name of haul object inside haul.RData (default "haul")
#' @param rdata_kisten_obj    Name of kisten object inside kisten.RData (default "kisten")
diagnose_parquet_coverage <- function(
    vessel_id,
    flyshoot_root       = Sys.getenv("ONEDRIVE_FLYSHOOT"),
    rdata_haul_path     = NULL,
    rdata_kisten_path   = NULL,
    rdata_haul_obj      = "haul",
    rdata_kisten_obj    = "kisten"
) {
  sep  <- strrep("=", 70)
  sep2 <- strrep("-", 50)
  message(sep)
  message(glue("PARQUET COVERAGE DIAGNOSTICS  |  {vessel_id}"))
  message(sep)

  # ── 1. Load parquet data ──────────────────────────────────────────────────
  haul_pq   <- load_flyshoot_data("haul",   vessel_ids = vessel_id)
  kisten_pq <- load_flyshoot_data("kisten", vessel_ids = vessel_id)

  message(glue("\nParquet haul  : {nrow(haul_pq)} rows | ",
               "{min(haul_pq$date, na.rm=TRUE)} to {max(haul_pq$date, na.rm=TRUE)}"))
  message(glue("Parquet kisten: {nrow(kisten_pq)} rows | ",
               "{min(kisten_pq$date, na.rm=TRUE)} to {max(kisten_pq$date, na.rm=TRUE)}"))

  # ── 2. Weekly coverage: how many haul weeks have matching kisten weeks ────
  haul_weeks <- haul_pq %>%
    dplyr::filter(!is.na(total_catch_kg), total_catch_kg > 0) %>%
    dplyr::mutate(yr = lubridate::year(date), wk = lubridate::isoweek(date)) %>%
    dplyr::distinct(trip_id, yr, wk) %>%
    dplyr::mutate(has_haul = TRUE)

  kisten_weeks <- kisten_pq %>%
    dplyr::filter(!is.na(haul_id)) %>%
    dplyr::mutate(yr = lubridate::year(date), wk = lubridate::isoweek(date)) %>%
    dplyr::distinct(trip_id, yr, wk) %>%
    dplyr::mutate(has_kisten = TRUE)

  coverage <- haul_weeks %>%
    dplyr::full_join(kisten_weeks, by = c("trip_id", "yr", "wk")) %>%
    dplyr::mutate(
      has_haul   = tidyr::replace_na(has_haul,   FALSE),
      has_kisten = tidyr::replace_na(has_kisten, FALSE),
      status = dplyr::case_when(
        has_haul & has_kisten  ~ "both",
        has_haul & !has_kisten ~ "haul only (no kisten)",
        !has_haul & has_kisten ~ "kisten only (no haul)",
        TRUE                   ~ "unknown"
      )
    )

  message(glue("\n{sep2}"))
  message("Coverage by year (trip-weeks with haul data):")
  message(sep2)
  coverage %>%
    dplyr::group_by(yr, status) %>%
    dplyr::summarise(n_weeks = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = status, values_from = n_weeks,
                       values_fill = 0L) %>%
    dplyr::arrange(yr) %>%
    print(n = 20)

  # ── 3. Trip ID format check ───────────────────────────────────────────────
  message(glue("\n{sep2}"))
  message("Trip ID format samples (first 5 per source):")
  message(sep2)
  message("Haul trip_ids  : ",
          paste(head(sort(unique(haul_pq$trip_id)), 5), collapse = ", "))
  message("Kisten trip_ids: ",
          paste(head(sort(unique(kisten_pq$trip_id)), 5), collapse = ", "))

  # Check for format mismatches (e.g. "2023W05" vs "2023052")
  haul_fmt   <- unique(nchar(haul_pq$trip_id))
  kisten_fmt <- unique(nchar(kisten_pq$trip_id))
  if (!all(haul_fmt %in% kisten_fmt))
    message(glue("  \u26a0 trip_id length mismatch: haul={paste(sort(haul_fmt),collapse=',')} ",
                 "kisten={paste(sort(kisten_fmt),collapse=',')}"))

  # ── 4. Optional RData comparison ─────────────────────────────────────────
  has_rdata <- (!is.null(rdata_haul_path)   && file.exists(rdata_haul_path)) ||
               (!is.null(rdata_kisten_path) && file.exists(rdata_kisten_path))

  if (has_rdata) {
    message(glue("\n{sep2}"))
    message("Comparing against RData files:")
    message(sep2)

    # Load haul RData
    if (!is.null(rdata_haul_path) && file.exists(rdata_haul_path)) {
      env_haul <- new.env()
      load(rdata_haul_path, envir = env_haul)
      if (rdata_haul_obj %in% ls(env_haul)) {
        haul_old <- get(rdata_haul_obj, envir = env_haul)
        # Filter to vessel if column exists
        if ("vessel" %in% names(haul_old))
          haul_old <- haul_old %>% dplyr::filter(vessel == vessel_id)
        message(glue("RData haul  : {nrow(haul_old)} rows | ",
                     "{min(haul_old$date, na.rm=TRUE)} to ",
                     "{max(haul_old$date, na.rm=TRUE)}"))
        message(glue("Parquet haul: {nrow(haul_pq)} rows | ",
                     "{min(haul_pq$date, na.rm=TRUE)} to ",
                     "{max(haul_pq$date, na.rm=TRUE)}"))
        # Weeks in RData but not parquet
        missing_haul <- haul_old %>%
          dplyr::mutate(yr = lubridate::year(date),
                        wk = lubridate::isoweek(date)) %>%
          dplyr::distinct(yr, wk) %>%
          dplyr::anti_join(
            haul_pq %>%
              dplyr::mutate(yr = lubridate::year(date),
                            wk = lubridate::isoweek(date)) %>%
              dplyr::distinct(yr, wk),
            by = c("yr", "wk")
          ) %>%
          dplyr::arrange(yr, wk)
        if (nrow(missing_haul) > 0) {
          message(glue("  \u26a0 {nrow(missing_haul)} year-week(s) in RData haul ",
                       "but NOT in parquet:"))
          print(missing_haul, n = 50)
        } else {
          message("  \u2713 All RData haul year-weeks present in parquet")
        }
      } else {
        message(glue("  \u26a0 Object '{rdata_haul_obj}' not found in {basename(rdata_haul_path)}"))
        message(glue("    Available objects: {paste(ls(env_haul), collapse=', ')}"))
      }
    }

    # Load kisten RData
    if (!is.null(rdata_kisten_path) && file.exists(rdata_kisten_path)) {
      env_kisten <- new.env()
      load(rdata_kisten_path, envir = env_kisten)
      if (rdata_kisten_obj %in% ls(env_kisten)) {
        kisten_old <- get(rdata_kisten_obj, envir = env_kisten)
        if ("vessel" %in% names(kisten_old))
          kisten_old <- kisten_old %>% dplyr::filter(vessel == vessel_id)
        # Find the date column — may be named differently in old data
        date_col_old <- intersect(c("date", "datum", "Date"), names(kisten_old))
        message(glue("  Kisten RData columns: {paste(names(kisten_old), collapse=', ')}"))
        if (length(date_col_old) > 0) {
          d_sample <- kisten_old[[date_col_old[1]]][!is.na(kisten_old[[date_col_old[1]]])][1:3]
          message(glue("  Date column '{date_col_old[1]}': class={class(d_sample)[1]}, ",
                       "sample={paste(d_sample, collapse=', ')}"))

          # Robust date parsing — handles DD/MM/YYYY, DD-MM-YYYY, DD-MM-YY, ISO
          d_raw2 <- as.character(kisten_old[[date_col_old[1]]])
          parsed2 <- dplyr::case_when(
            grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", d_raw2) ~
              as.Date(d_raw2, format = "%d/%m/%Y"),
            grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$",  d_raw2) ~
              as.Date(d_raw2, format = "%d/%m/%y"),
            grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$",  d_raw2) ~
              as.Date(d_raw2, format = "%d-%m-%Y"),
            grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$",  d_raw2) ~
              as.Date(sub("^(\\d{1,2}-\\d{1,2}-)(\\d{2})$","\\120\\2", d_raw2),
                      format = "%d-%m-%Y"),
            grepl("^\\d{4}-\\d{2}-\\d{2}", d_raw2) ~
              as.Date(substr(d_raw2, 1, 10)),
            TRUE ~ NA_Date_
          )
          kisten_old$date <- parsed2

          n_na <- sum(is.na(kisten_old$date))
          if (n_na > 0)
            message(glue("  \u26a0 {n_na} rows with unparseable dates (filtered out)"))

          kisten_old <- kisten_old %>% dplyr::filter(!is.na(date))

          # Find trip column
          trip_col_old <- intersect(c("trip_id", "trip", "reis"), names(kisten_old))
          if (length(trip_col_old) > 0 && trip_col_old[1] != "trip_id")
            kisten_old <- kisten_old %>%
              dplyr::rename(trip_id = !!trip_col_old[1])
        } else {
          message("  \u26a0 No date column found. Available: ",
                  paste(names(kisten_old), collapse = ", "))
        }
        message(glue("RData kisten  : {nrow(kisten_old)} rows | ",
                     "{min(kisten_old$date, na.rm=TRUE)} to ",
                     "{max(kisten_old$date, na.rm=TRUE)}"))
        message(glue("Parquet kisten: {nrow(kisten_pq)} rows | ",
                     "{min(kisten_pq$date, na.rm=TRUE)} to ",
                     "{max(kisten_pq$date, na.rm=TRUE)}"))
        # Weeks in RData but not parquet
        missing_kisten <- kisten_old %>%
          dplyr::mutate(yr = lubridate::year(date),
                        wk = lubridate::isoweek(date)) %>%
          dplyr::distinct(yr, wk) %>%
          dplyr::anti_join(
            kisten_pq %>%
              dplyr::mutate(yr = lubridate::year(date),
                            wk = lubridate::isoweek(date)) %>%
              dplyr::distinct(yr, wk),
            by = c("yr", "wk")
          ) %>%
          dplyr::arrange(yr, wk)
        if (nrow(missing_kisten) > 0) {
          message(glue("  ⚠ {nrow(missing_kisten)} year-week(s) in RData kisten ",
                       "but NOT in parquet:"))
          missing_kisten %>%
            dplyr::filter(!is.na(yr), !is.na(wk)) %>%
            as.data.frame() %>%
            print(row.names = FALSE)
        } else {
          message("  ✓ All RData kisten year-weeks present in parquet")
        }

        # Trip-level row count comparison
        message(glue("\n  Trip-level kisten row counts (RData vs parquet):"))
        trip_col_old <- intersect(c("trip_id", "trip", "reis"), names(kisten_old))
        trip_col_pq  <- "trip_id"
        if (length(trip_col_old) > 0) {
          counts_old <- kisten_old %>%
            dplyr::rename(trip_id = !!trip_col_old[1]) %>%
            dplyr::count(trip_id, name = "n_rdata")
          counts_pq <- kisten_pq %>%
            dplyr::count(trip_id, name = "n_parquet")
          trip_diff <- counts_old %>%
            dplyr::full_join(counts_pq, by = "trip_id") %>%
            dplyr::mutate(
              n_rdata   = tidyr::replace_na(n_rdata, 0L),
              n_parquet = tidyr::replace_na(n_parquet, 0L),
              diff      = n_parquet - n_rdata
            ) %>%
            dplyr::filter(diff != 0) %>%
            dplyr::arrange(diff)
          if (nrow(trip_diff) > 0) {
            message(glue("  \u26a0 {nrow(trip_diff)} trip(s) with different row counts ",
                         "(negative diff = missing from parquet):"))
            trip_diff %>%
              dplyr::filter(!is.na(trip_id)) %>%
              as.data.frame() %>%
              print(row.names = FALSE)
          } else {
            message("  \u2713 All trips have matching row counts")
          }
        }
      } else {
        message(glue("  \u26a0 Object '{rdata_kisten_obj}' not found in {basename(rdata_kisten_path)}"))
        message(glue("    Available objects: {paste(ls(env_kisten), collapse=', ')}"))
      }
    }
  }

  message(glue("\n{sep}"))
  invisible(list(haul = haul_pq, kisten = kisten_pq, coverage = coverage))
}


# ==============================================================================
# UTILITY: backfill_kisten_from_rdata
# ==============================================================================
#' Backfill kisten parquet from old RData file.
#' Maps old column names to the current parquet schema and writes missing trips.
#'
#' Run interactively:
#'   backfill_kisten_from_rdata(
#'     rdata_path    = "path/to/kisten.RData",
#'     flyshoot_root = Sys.getenv("ONEDRIVE_FLYSHOOT"),
#'     vessel_id     = "SCH99",   # NULL = all vessels
#'     dry_run       = TRUE       # set FALSE to actually write
#'   )
#'
#' @param rdata_path    Path to kisten.RData file
#' @param flyshoot_root Path to parquet root (default ONEDRIVE_FLYSHOOT env var)
#' @param vessel_id     Vessel to backfill (NULL = all vessels in RData)
#' @param dry_run       If TRUE (default) print what would be written without writing
#' @param rdata_obj     Name of kisten object inside RData (default "kisten")
backfill_kisten_from_rdata <- function(
    rdata_path,
    flyshoot_root = Sys.getenv("ONEDRIVE_FLYSHOOT"),
    vessel_id     = NULL,
    dry_run       = TRUE,
    rdata_obj     = "kisten"
) {
  stopifnot(file.exists(rdata_path))
  sep <- strrep("=", 70)

  message(sep)
  message(glue("KISTEN BACKFILL FROM RDATA{if (dry_run) ' (DRY RUN)' else ''}"))
  message(glue("  Source: {basename(rdata_path)}"))
  message(sep)

  # ── 1. Load old RData ──────────────────────────────────────────────────────
  env_old <- new.env()
  load(rdata_path, envir = env_old)
  if (!rdata_obj %in% ls(env_old)) {
    stop(glue("Object '{rdata_obj}' not found. Available: ",
              "{paste(ls(env_old), collapse=', ')}"))
  }
  kisten_old <- get(rdata_obj, envir = env_old)
  message(glue("  RData rows: {nrow(kisten_old)} | cols: ",
               "{paste(names(kisten_old), collapse=', ')}"))

  if (!is.null(vessel_id))
    kisten_old <- kisten_old %>% dplyr::filter(vessel %in% vessel_id)

  # ── 2. Map old columns to parquet schema ──────────────────────────────────
  # Old: lotnummer, soorten, maat, gewicht, vessel, trip, datetime,
  #      time_diff, haul2, haul, source, species, datum, tijd
  # New: lot_nr, species_code, size_category, weight_kg, vessel, trip_id,
  #      weighing_time, haul_id, date, time_hhmm, session_id, haul_flag

  # Find trip column
  trip_col <- intersect(c("trip_id", "trip", "reis"), names(kisten_old))
  if (length(trip_col) == 0) stop("No trip column found in old kisten data")

  # Find haul column
  haul_col <- intersect(c("haul", "haul_id", "haul2"), names(kisten_old))
  haul_col <- haul_col[1]  # prefer "haul" over "haul2"

  # Find date column and parse it
  date_col <- intersect(c("datum", "date", "Date"), names(kisten_old))
  if (length(date_col) == 0) stop("No date column found in old kisten data")

  # Find datetime column for weighing_time
  dt_col <- intersect(c("datetime", "weighing_time", "weegmoment"), names(kisten_old))

  message(glue("  Column mapping: trip={trip_col[1]}, haul={haul_col}, ",
               "date={date_col[1]}, datetime={if(length(dt_col)>0) dt_col[1] else 'none'}"))

  # ── Parse date column separately ──────────────────────────────────────────
  d_raw <- as.character(kisten_old[[date_col[1]]])
  parsed_dates <- dplyr::case_when(
    grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", d_raw) ~
      as.Date(d_raw, format = "%d/%m/%Y"),
    grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", d_raw) ~
      as.Date(d_raw, format = "%d/%m/%y"),
    grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", d_raw) ~
      as.Date(d_raw, format = "%d-%m-%Y"),
    grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$", d_raw) ~
      as.Date(sub("^(\\d{1,2}-\\d{1,2}-)(\\d{2})$", "\\120\\2", d_raw),
              format = "%d-%m-%Y"),
    grepl("^\\d{4}-\\d{2}-\\d{2}", d_raw) ~
      as.Date(substr(d_raw, 1, 10)),
    TRUE ~ NA_Date_
  )
  n_parsed <- sum(!is.na(parsed_dates))
  message(glue("  Date parsing: {n_parsed}/{length(d_raw)} rows parsed successfully"))
  message(glue("  Sample dates: {paste(head(parsed_dates[!is.na(parsed_dates)], 3), collapse=', ')}"))
  kisten_old$date_parsed <- parsed_dates

  kisten_mapped <- kisten_old %>%
    dplyr::rename(trip_id = !!trip_col[1]) %>%
    dplyr::mutate(
      date = date_parsed,

      # Weighing time from datetime column if available
      weighing_time = if (length(dt_col) > 0)
        as.POSIXct(as.numeric(.data[[dt_col[1]]]),
                   origin = "1970-01-01", tz = "UTC")
      else
        as.POSIXct(as.numeric(as.POSIXct(date, tz = "UTC")),
                   origin = "1970-01-01", tz = "UTC"),

      # Species: prefer "species" (3-letter code), fall back to "soorten"
      species_code = dplyr::coalesce(
        if ("species"  %in% names(.)) as.character(species)  else NA_character_,
        if ("soorten"  %in% names(.)) as.character(soorten)  else NA_character_
      ),

      # Size category from "maat"
      size_category = if ("maat" %in% names(.)) as.character(maat) else NA_character_,

      # Weight
      weight_kg = if ("gewicht" %in% names(.)) as.double(gewicht)
                  else if ("weight_kg" %in% names(.)) as.double(weight_kg)
                  else NA_real_,

      # Haul ID
      haul_id = as.integer(.data[[haul_col]]),

      # Required columns that may be absent
      session_id   = NA_integer_,
      haul_flag    = FALSE,
      lot_nr       = if ("lotnummer" %in% names(.)) as.integer(lotnummer) else NA_integer_,
      time_hhmm    = if ("tijd"      %in% names(.)) as.character(tijd)    else NA_character_,

      # Save metadata
      save_date      = Sys.Date(),
      save_timestamp = Sys.time()
    ) %>%
    dplyr::select(vessel, trip_id, haul_id, date, weighing_time,
                  species_code, size_category, weight_kg,
                  lot_nr, time_hhmm, session_id, haul_flag,
                  save_date, save_timestamp) %>%
    dplyr::filter(!is.na(date), !is.na(vessel), !is.na(trip_id))

  message(glue("  Mapped rows: {nrow(kisten_mapped)} | ",
               "date range: {min(kisten_mapped$date, na.rm=TRUE)} to ",
               "{max(kisten_mapped$date, na.rm=TRUE)}"))

  # ── 3. Find trips missing from parquet ────────────────────────────────────
  kisten_pq <- tryCatch(
    load_flyshoot_data("kisten", vessel_ids = vessel_id),
    error = function(e) tibble::tibble()
  )

  existing_trips <- unique(kisten_pq$trip_id)
  new_trips      <- unique(kisten_mapped$trip_id)
  missing_trips  <- setdiff(new_trips, existing_trips)

  message(glue("\n  Trips in RData:   {length(new_trips)}"))
  message(glue("  Trips in parquet: {length(existing_trips)}"))
  message(glue("  Missing trips:    {length(missing_trips)}"))

  if (length(missing_trips) == 0) {
    message("  \u2713 All trips already present in parquet — nothing to backfill")
    return(invisible(NULL))
  }

  kisten_to_add <- kisten_mapped %>%
    dplyr::filter(trip_id %in% missing_trips)

  message(glue("  Rows to add: {nrow(kisten_to_add)} across {length(missing_trips)} trips"))
  message(glue("  Date range to add: {min(kisten_to_add$date, na.rm=TRUE)} to ",
               "{max(kisten_to_add$date, na.rm=TRUE)}"))

  if (dry_run) {
    message(glue("\n  DRY RUN — no changes written."))
    message("  Re-run with dry_run = FALSE to apply.")
    message(glue("\n  First 10 missing trips:"))
    kisten_to_add %>%
      dplyr::count(trip_id, date = as.Date(date)) %>%
      dplyr::arrange(date) %>%
      head(10) %>%
      print()
    return(invisible(kisten_to_add))
  }

  # ── 4. Combine and save ───────────────────────────────────────────────────
  combined <- dplyr::bind_rows(kisten_pq, kisten_to_add) %>%
    dplyr::arrange(vessel, trip_id, date, weighing_time)

  save_flyshoot_data(combined, "kisten")
  message(glue("  \u2713 Backfill complete: added {nrow(kisten_to_add)} rows, ",
               "total now {nrow(combined)} rows"))

  invisible(kisten_to_add)
}


# ==============================================================================
# DIAGNOSTIC: debug_haul_assignment
# ==============================================================================
#' Diagnose the kisten-to-haul assignment for a specific trip.
#'
#' Runs the same readers as the main workflow, then prints a step-by-step
#' comparison so you can see exactly where sessions and hauls diverge.
#'
#' @param kisten_file     Path to the raw Marelec kisten Excel file
#' @param treklijst_file  Path to the raw treklijst Excel file
#' @param gap_min         Session gap threshold in minutes (default KISTEN_GAP_THRESHOLD_MIN)
debug_haul_assignment <- function(kisten_file,
                                   treklijst_file,
                                   gap_min = KISTEN_GAP_THRESHOLD_MIN) {

  sep  <- strrep("=", 70)
  sep2 <- strrep("-", 70)
  message(sep)
  message("HAUL ASSIGNMENT DIAGNOSTICS")
  message(glue("  Kisten:    {basename(kisten_file)}"))
  message(glue("  Treklijst: {basename(treklijst_file)}"))
  message(glue("  Session gap threshold: {gap_min} min"))
  message(sep)

  # ── Step 1: Read treklijst via production reader ──────────────────────────
  message(sep2)
  message("STEP 1: TREKLIJST HAULS")
  message(sep2)

  hauls <- quietly(get_haul_from_treklijst(treklijst_file))

  if (is.null(hauls) || nrow(hauls) == 0) {
    message("  ✗ No hauls returned from treklijst — cannot continue")
    return(invisible(NULL))
  }

  n_hauls       <- nrow(hauls)
  n_valid_times <- sum(!is.na(hauls$shoot_time))
  message(glue("  Hauls read:                  {n_hauls}"))
  message(glue("  Hauls with valid shoot time: {n_valid_times}"))

  # Compute haul end time: min(shoot_time + 80 min, next haul shoot_time)
  hauls <- hauls %>%
    dplyr::arrange(haul_id) %>%
    dplyr::mutate(
      haul_end_time = pmin(
        shoot_time + lubridate::dminutes(80),
        dplyr::lead(shoot_time, default = shoot_time[dplyr::n()] + lubridate::dminutes(80)),
        na.rm = TRUE
      ),
      haul_duration_min = round(as.numeric(difftime(haul_end_time, shoot_time, units = "mins")), 0)
    )

  hauls %>%
    dplyr::select(dplyr::any_of(c("haul_id", "date", "shoot_time", "haul_end_time",
                                   "haul_duration_min", "total_catch_kg"))) %>%
    dplyr::mutate(
      shoot_time    = format(shoot_time,    "%d/%m %H:%M"),
      haul_end_time = format(haul_end_time, "%d/%m %H:%M")
    ) %>%
    print(n = 50)

  # ── Step 2: Read raw kisten and detect sessions ───────────────────────────
  message(sep2)
  message("STEP 2: KISTEN SESSIONS")
  message(sep2)

  # Read kisten using the same header-skip logic as get_catch_from_kisten:
  # Marelec files have metadata rows before the actual data table; scan the
  # first 20 rows to find where "Lotnummer" appears, then skip to that row.
  kisten_raw <- tryCatch({
    preview <- readxl::read_excel(kisten_file, sheet = "1",
                                   col_names = FALSE, col_types = "text", n_max = 20)
    skip_n <- preview %>%
      dplyr::mutate(
        .row    = dplyr::row_number(),
        .is_hdr = dplyr::if_any(dplyr::everything(),
                    ~stringr::str_detect(as.character(.), stringr::regex("lotnummer", ignore_case = TRUE)))
      ) %>%
      dplyr::filter(.is_hdr) %>%
      dplyr::pull(.row) %>%
      dplyr::first()
    skip_n <- if (is.na(skip_n)) 5L else as.integer(skip_n) - 1L

    readxl::read_excel(kisten_file, sheet = "1", skip = skip_n,
                       col_names = TRUE, col_types = "text",
                       .name_repair = ~make.names(., unique = TRUE)) %>%
      lowcase() %>%
      dplyr::filter(!is.na(lotnummer), lotnummer != "") %>%
      dplyr::arrange(as.integer(lotnummer))
  }, error = function(e) { message("  ✗ Could not read kisten file: ", e$message); NULL })
  if (is.null(kisten_raw)) return(invisible(NULL))

  # Filter EINDE markers
  sp_col <- intersect(c("soorten", "species"), names(kisten_raw))
  if (length(sp_col) > 0)
    kisten_raw <- kisten_raw %>%
      dplyr::filter(!toupper(trimws(.data[[sp_col[1]]])) %in% c("EINDE", "EINDE DAG"))

  message(glue("  Raw records (after EINDE filter): {nrow(kisten_raw)}"))

  # Check for pre-assigned haul column
  haul_col <- intersect(c("haul", "trek", "haul_id"), names(kisten_raw))
  if (length(haul_col) > 0 && sum(!is.na(kisten_raw[[haul_col[1]]])) > 0) {
    pre_vals <- sort(unique(na.omit(as.integer(kisten_raw[[haul_col[1]]]))))
    message(glue("  ℹ Pre-assigned '{haul_col[1]}' column found with {length(pre_vals)} unique values: ",
                 "{paste(head(pre_vals, 10), collapse=', ')}",
                 "{if (length(pre_vals) > 10) '...' else ''}"))
    message("    Automatic session detection would be skipped — pre-assigned values used directly.")
    return(invisible(NULL))
  }

  # Apply schema to get date + time columns
  kisten_schema <- apply_schema(kisten_raw, marelec_kisten_schema)

  # Build weighing_time
  tz_resolved <- resolve_tz(
    if (!is.null(hauls) && "timezone" %in% names(hauls))
      dplyr::first(na.omit(hauls$timezone))
    else "Europe/Amsterdam"
  )

  kisten_timed <- kisten_schema %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate(
      weighing_time = tryCatch(
        as.POSIXct(paste(format(as.Date(date), "%Y-%m-%d"), time_hhmm),
                   format = "%Y-%m-%d %H:%M:%S", tz = tz_resolved),
        error = function(e) as.POSIXct(NA)
      )
    ) %>%
    dplyr::filter(!is.na(weighing_time)) %>%
    dplyr::arrange(weighing_time)

  message(glue("  Records with valid weighing_time: {nrow(kisten_timed)}"))

  if (nrow(kisten_timed) == 0) {
    message("  ✗ No records have valid weighing times — check date/time columns")
    message(glue("    Columns found: {paste(names(kisten_raw), collapse=', ')}"))
    return(invisible(NULL))
  }

  # Detect sessions using same logic as assign_haul_ids_from_treklijst
  sessions <- kisten_timed %>%
    dplyr::mutate(
      time_gap_min = as.numeric(difftime(weighing_time, dplyr::lag(weighing_time),
                                          units = "mins")),
      session_id   = cumsum(tidyr::replace_na(time_gap_min > gap_min, TRUE))
    ) %>%
    dplyr::group_by(session_id) %>%
    dplyr::summarise(
      session_start = min(weighing_time),
      session_end   = max(weighing_time),
      duration_min  = round(as.numeric(difftime(max(weighing_time),
                                                 min(weighing_time), units = "mins")), 1),
      n_weighings   = dplyr::n(),
      total_kg      = round(sum(weight_kg, na.rm = TRUE), 1),
      max_gap_min   = round(max(time_gap_min, na.rm = TRUE), 1),
      .groups       = "drop"
    )

  n_sessions <- nrow(sessions)
  message(glue("  Sessions detected (gap > {gap_min} min): {n_sessions}"))

  sessions %>%
    dplyr::mutate(session_start = format(session_start, "%d/%m %H:%M"),
                  session_end   = format(session_end,   "%d/%m %H:%M")) %>%
    print(n = 60)

  # ── Step 3: Run matcher and show actual assignments ───────────────────────
  message(sep2)
  message("STEP 3: COMPARISON (hauls vs sessions)")
  message(sep2)

  # Run the actual production matcher so the diagnostic reflects real behaviour
  haul_rows  <- hauls %>%
    dplyr::filter(!is.na(shoot_time)) %>%
    dplyr::arrange(haul_id)
  haul_ids_d  <- haul_rows$haul_id
  shoot_dts_d <- haul_rows$shoot_time

  mapping <- match_sessions_to_hauls(sessions, haul_ids_d, shoot_dts_d)

  # Build comparison: one row per haul, with matched session alongside
  sess_matched <- sessions %>%
    dplyr::mutate(haul_id = mapping[as.character(session_id)])

  comparison <- haul_rows %>%
    dplyr::select(haul_id, shoot_time, haul_end_time,
                  dplyr::any_of("total_catch_kg")) %>%
    dplyr::rename(haul_shoot = shoot_time, haul_end = haul_end_time) %>%
    { if ("total_catch_kg" %in% names(.)) dplyr::rename(., trek_kg = total_catch_kg)
      else dplyr::mutate(., trek_kg = NA_real_) } %>%
    dplyr::mutate(haul_shoot_dt = haul_rows$shoot_time,
                  haul_end_dt   = haul_rows$haul_end_time,
                  haul_shoot    = format(haul_shoot_dt, "%d/%m %H:%M"),
                  haul_end      = format(haul_end_dt,   "%d/%m %H:%M")) %>%
    dplyr::left_join(
      sess_matched %>%
        dplyr::select(haul_id, session_id, session_start, n_weighings,
                      session_kg = total_kg),
      by = "haul_id"
    ) %>%
    dplyr::mutate(
      gap_min = round(as.numeric(difftime(session_start, haul_end_dt, units = "mins")), 0),
      session_start = format(session_start, "%d/%m %H:%M"),
      kg_ratio = dplyr::if_else(
        !is.na(trek_kg) & trek_kg > 0 & !is.na(session_kg),
        round(session_kg / trek_kg, 2),
        NA_real_
      ),
      status = dplyr::case_when(
        is.na(session_id)                                      ~ "\u26a0 no session",
        gap_min < 0                                            ~ "\u26a0 weighing before haul end",
        !is.na(kg_ratio) & (kg_ratio < 0.5 | kg_ratio > 2.0) ~ "\u26a0 kg mismatch",
        TRUE                                                   ~ "\u2713"
      )
    ) %>%
    dplyr::select(haul_id, haul_shoot, haul_end, trek_kg,
                  session_id, session_start, gap_min,
                  n_weighings, session_kg, kg_ratio, status)

  print(comparison, n = 80)


  # ── Step 4: Summary and recommendation ────────────────────────────────────
  message(sep2)
  message("SUMMARY")
  message(sep2)

  if (n_sessions == n_valid_times) {
    message(glue("  \u2713 Sessions ({n_sessions}) = hauls ({n_valid_times}) — assignment should work."))
  } else if (n_sessions > n_valid_times) {
    excess <- n_sessions - n_valid_times
    message(glue("  \u26a0 {excess} MORE SESSION(S) THAN HAULS ({n_sessions} vs {n_valid_times})"))
    message("  Sessions with fewest weighings are most likely split hauls:")
    sessions %>%
      dplyr::arrange(n_weighings) %>%
      dplyr::slice_head(n = min(excess + 2, n_sessions)) %>%
      dplyr::mutate(session_start = format(session_start, "%d/%m %H:%M"),
                    session_end   = format(session_end,   "%d/%m %H:%M")) %>%
      dplyr::select(session_id, session_start, session_end, n_weighings,
                    total_kg, max_gap_min) %>%
      print()
    message(glue("  \u2192 If a small session immediately precedes a large one from the same haul,"))
    message(glue("    the Marelec likely paused mid-haul. The gap threshold ({gap_min} min) is"))
    message(glue("    treating this pause as a session boundary."))
    message(glue("  \u2192 You can add the file to get_catch_from_kisten() with a higher gap_min,"))
    message(glue("    or (if systematic) lower KISTEN_GAP_THRESHOLD_MIN in 01_flyshoot_functions.R."))
  } else {
    missing <- n_valid_times - n_sessions
    message(glue("  \u2139 {missing} haul(s) have no kisten sessions — normal for zero-catch hauls."))
  }

  message(sep)
  invisible(list(hauls = haul_rows, sessions = sessions, comparison = comparison))
}

# ==============================================================================
# TURBOCATCH / ERS CONVERTERS
# ==============================================================================
# These functions convert the output of parse_ers_folder() (from parse_ers.R)
# into the standard flyshoot pipeline schema (haul, elog_trek, trip tables).
#
# parse_ers.R must be sourced before these functions are called.
# Add to 00_setup.R:
#   source(file.path(here(), "R/turbocatch", "parse_ers.R"))
#
# Mapping:
#   haul      ← FAR message (one row per haul)
#   elog_trek ← FAR species rows (one row per species per haul)
#   trip      ← DEP + RTP + FAR summary (one row per trip)
#   kisten    ← not applicable (no Marelec box data in ERS)
#   elog      ← not applicable (trip-level LAN not used; FAR is authoritative)
# ==============================================================================

# ── shared haul-ID helper ─────────────────────────────────────────────────────
# One XML file = one FAR message = one haul (per_haul regime, 2026+).
# source_file is the unique key; hauls are numbered by date/time within trip.
.ers_haul_lookup <- function(far_df) {
  far_df %>%
    dplyr::filter(!is_correction) %>%
    dplyr::distinct(trip_id, source_file, haul_date, haul_time, lat, lon) %>%
    dplyr::arrange(trip_id, haul_date, haul_time) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(haul_id = dplyr::row_number()) %>%
    dplyr::ungroup()
}

# ── FAR regime detection ──────────────────────────────────────────────────────
# Two distinct TurboCatch reporting practices exist:
#   "daily"    — one FAR per day covering multiple shots (avg n_shots > 3)
#                Used 2019–2025. Equivalent to elog (daily catch report).
#   "per_haul" — one FAR per haul, 1-2 shots each (avg n_shots <= 3)
#                Used 2026+. Equivalent to elog_trek (haul-level catch).
# Regime is detected per trip from avg_shots_per_far in the trip summary.

.ers_trip_regime <- function(ers_data, trip_id_val) {
  if (!is.null(ers_data$trips) && "far_regime" %in% names(ers_data$trips)) {
    regime <- ers_data$trips %>%
      dplyr::filter(trip_id == trip_id_val) %>%
      dplyr::pull(far_regime)
    if (length(regime) > 0 && !is.na(regime[1])) return(regime[1])
  }
  # Fallback: check avg shots directly from FAR data
  far_trip <- ers_data$far %>%
    dplyr::filter(!is_correction, trip_id == trip_id_val)
  if (nrow(far_trip) == 0) return("no_far")
  avg <- mean(far_trip$n_shots, na.rm = TRUE)
  if_else(avg > 3, "daily", "per_haul")
}

# ── ers_to_haul ───────────────────────────────────────────────────────────────

#' Convert ERS FAR data to pipeline haul format (one row per FAR message).
#' Only meaningful for per_haul regime (2026+) where one FAR = one haul.
#' For daily regime trips, haul positions are not available from ERS.
#' @param ers_data  output of parse_ers_folder()
#' @return tibble matching the haul parquet schema (per_haul trips only)
ers_to_haul <- function(ers_data) {
  if (is.null(ers_data$far) || nrow(ers_data$far) == 0) {
    message("  No FAR records found — haul table will be empty")
    return(tibble::tibble())
  }

  far <- ers_data$far %>% dplyr::filter(!is_correction)

  # Only process per_haul trips for the haul table
  # daily = one FAR per day (2019-2025), no_far = no catch messages
  trip_regimes <- ers_data$trips %>%
    dplyr::select(trip_id, far_regime) %>%
    dplyr::filter(!is.na(far_regime))

  per_haul_trips <- trip_regimes %>%
    dplyr::filter(far_regime == "per_haul") %>%
    dplyr::pull(trip_id)

  if (length(per_haul_trips) == 0) {
    message("  No per_haul trips — no haul-level positions available")
    return(tibble::tibble())
  }

  far_ph <- far %>% dplyr::filter(trip_id %in% per_haul_trips)
  if (nrow(far_ph) == 0) return(tibble::tibble())

  lookup <- .ers_haul_lookup(far_ph)

  far_ph %>%
    dplyr::left_join(lookup,
                     by = c("trip_id", "source_file",
                            "haul_date", "haul_time", "lat", "lon")) %>%
    dplyr::mutate(
      shoot_time         = as.POSIXct(paste(haul_date, haul_time),
                                       format = "%Y-%m-%d %H:%M", tz = "UTC"),
      fishing_time_hours = duration_min / 60,
      data_source        = "turbocatch"
    ) %>%
    dplyr::group_by(trip_id, vessel_id, haul_id, haul_date, haul_time) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      vessel             = vessel_id,
      trip_id            = trip_id,
      trip_nr            = trip_id,
      haul_id            = haul_id,
      date               = as.Date(haul_date),
      shoot_lat          = lat,
      shoot_lon          = lon,
      shoot_time         = shoot_time,
      haul_lat           = NA_real_,
      haul_lon           = NA_real_,
      haul_time          = NA_POSIXct_,
      fishing_time_hours = fishing_time_hours,
      gear_type          = gear_type,
      mesh_size_mm       = as.integer(mesh_mm),
      water_depth        = NA_integer_,
      n_shots            = n_shots,
      ices_rect          = ices_rect,
      fao_division       = fao_area,
      economic_zone      = eez,
      data_source        = "turbocatch"
    )
}

# ── ers_to_elog_trek ──────────────────────────────────────────────────────────

#' Convert ERS FAR species rows to pipeline elog_trek format.
#' Only for per_haul regime (2026+): one FAR = one haul = one row per species.
#' @param ers_data  output of parse_ers_folder()
#' @return tibble matching the elog_trek parquet schema
ers_to_elog_trek <- function(ers_data) {
  if (is.null(ers_data$far) || nrow(ers_data$far) == 0) return(tibble::tibble())

  per_haul_trips <- ers_data$trips %>%
    dplyr::filter(!is.na(far_regime), far_regime == "per_haul") %>%
    dplyr::pull(trip_id)

  if (length(per_haul_trips) == 0) return(tibble::tibble())

  far <- ers_data$far %>%
    dplyr::filter(!is_correction, trip_id %in% per_haul_trips)
  if (nrow(far) == 0) return(tibble::tibble())

  lookup <- .ers_haul_lookup(far)

  far %>%
    dplyr::left_join(lookup,
                     by = c("trip_id", "source_file",
                            "haul_date", "haul_time", "lat", "lon")) %>%
    dplyr::transmute(
      vessel            = vessel_id,
      trip_id           = trip_id,
      trip_nr           = trip_id,
      trip_nr_elog      = trip_id,
      haul_id           = haul_id,
      date              = as.Date(haul_date),
      weighing_time     = as.POSIXct(paste(haul_date, haul_time),
                                      format = "%Y-%m-%d %H:%M", tz = "UTC"),
      species_code      = species,
      presentation      = presentation,
      size_category     = "LSC",
      weight_kg         = weight_kg,
      box_count         = n_boxes,
      conversion_factor = conv_factor,
      shoot_lat         = lat,
      shoot_lon         = lon,
      ices_rect         = ices_rect,
      gear_type         = gear_type,
      mesh_size_mm      = as.integer(mesh_mm),
      data_source       = "turbocatch"
    ) %>%
    dplyr::filter(!is.na(species_code), !is.na(weight_kg))
}

# ── ers_to_elog ───────────────────────────────────────────────────────────────

#' Convert ERS FAR data to pipeline elog format.
#' Only for daily regime (2019-2025): one FAR per day, n_shots = hauls that day.
#' Stored in elog (daily catch report), not elog_trek.
#' @param ers_data  output of parse_ers_folder()
#' @return tibble matching the elog parquet schema
ers_to_elog <- function(ers_data) {
  if (is.null(ers_data$far) || nrow(ers_data$far) == 0) return(tibble::tibble())

  daily_trips <- ers_data$trips %>%
    dplyr::filter(!is.na(far_regime), far_regime == "daily") %>%
    dplyr::pull(trip_id)

  if (length(daily_trips) == 0) return(tibble::tibble())

  far <- ers_data$far %>%
    dplyr::filter(!is_correction, trip_id %in% daily_trips)
  if (nrow(far) == 0) return(tibble::tibble())

  far %>%
    dplyr::transmute(
      vessel            = vessel_id,
      trip_id           = trip_id,
      trip_nr           = trip_id,
      trip_nr_elog      = trip_id,
      date              = as.Date(haul_date),
      species_code      = species,
      presentation      = presentation,
      size_category     = "LSC",          # ERS does not distinguish undersized
      weight_kg         = weight_kg,
      box_count         = n_boxes,
      conversion_factor = conv_factor,
      shoot_lat         = lat,
      shoot_lon         = lon,
      ices_rect         = ices_rect,
      fao_division      = fao_area,
      economic_zone     = eez,
      gear_type         = gear_type,
      mesh_size_mm      = as.integer(mesh_mm),
      n_hauls           = n_shots,         # number of hauls covered by this daily FAR
      data_source       = "turbocatch"
    ) %>%
    dplyr::filter(!is.na(species_code), !is.na(weight_kg))
}

# ── ers_to_trip ───────────────────────────────────────────────────────────────

#' Convert ERS trip summary to pipeline trip format (one row per trip)
#' @param ers_data  output of parse_ers_folder()
#' @return tibble matching the trip parquet schema
ers_to_trip <- function(ers_data) {
  if (is.null(ers_data$trips) || nrow(ers_data$trips) == 0) {
    message("  No complete trips found")
    return(tibble::tibble())
  }

  ers_data$trips %>%
    dplyr::transmute(
      vessel            = vessel_id,
      trip_id           = trip_id,
      trip_nr           = trip_id,
      departure_date    = as.Date(dep_date),
      departure_port    = dep_port,
      arrival_date      = as.Date(rtp_date),
      arrival_port      = rtp_port,
      gears_declared    = gears_declared,
      gear_type         = stringr::str_extract(gears_declared, "^[A-Z]+"),
      far_kg_total      = far_kg_total,
      n_far_msgs        = n_far_msgs,
      n_hauls           = n_hauls,
      avg_shots_per_far = avg_shots_per_far,
      far_regime        = far_regime,
      trip_type         = dplyr::case_when(
        far_regime == "no_far"   ~ "non_fishing",
        far_regime == "daily"    ~ "fishing_daily_far",
        far_regime == "per_haul" ~ "fishing_per_haul",
        TRUE                     ~ NA_character_
      ),
      fishing_areas     = fishing_areas,
      mesh_size_mm      = as.integer(stringr::str_extract(gears_declared, "[0-9]+(?=mm)")),
      data_source       = "turbocatch"
    )
}

# ==============================================================================
# PARQUET COVERAGE
# ==============================================================================

#' Coverage overview for one or more pipeline parquet tables.
#' Shows number of rows (or unique days) per vessel, with years as columns.
#'
#' @param tables       Table name(s), or NULL for all (haul, elog, elog_trek, trip).
#' @param value        "n_obs"  = total row count per vessel/year (default)
#'                     "n_days" = number of distinct days per vessel/year
#' @param pipeline_dir Path to the raw parquet folder (default from config).
#'
#' @return Named list of tibbles (one per table), invisibly.
#'         Each tibble: one row per vessel, one column per year.
#'
#' @examples
#' parquet_coverage()                   # all tables, row counts
#' parquet_coverage(value = "n_days")   # all tables, unique days
#' parquet_coverage("elog")             # one table
#' parquet_coverage(c("elog", "trip"))  # two tables
parquet_coverage <- function(
    tables       = NULL,
    value        = "n_obs",
    pipeline_dir = config$raw_data_path
) {
  .table_date_col <- c(
    haul      = "date",
    elog      = "date",
    elog_trek = "date",
    trip      = "departure_date"
  )

  if (is.null(tables)) tables <- names(.table_date_col)

  results <- list()

  for (tbl in tables) {
    path <- file.path(pipeline_dir, tbl, paste0(tbl, ".parquet"))

    if (!file.exists(path)) {
      message(glue("  {tbl}.parquet not found — skipping"))
      next
    }

    df       <- arrow::read_parquet(path)
    date_col <- .table_date_col[[tbl]]

    if (!date_col %in% names(df)) {
      message(glue("  {tbl}: column '{date_col}' not found — skipping"))
      next
    }

    summary <- df %>%
      dplyr::mutate(
        date = as.Date(.data[[date_col]]),
        year = lubridate::year(date)
      ) %>%
      dplyr::group_by(vessel, year, date) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(vessel, year) %>%
      dplyr::summarise(
        n_obs  = sum(n),
        n_days = dplyr::n_distinct(date),
        .groups = "drop"
      ) %>%
      dplyr::select(vessel, year, val = dplyr::all_of(value)) %>%
      tidyr::pivot_wider(names_from = year, values_from = val) %>%
      dplyr::arrange(vessel) %>%
      # Sort year columns chronologically
      { yr_cols <- sort(as.integer(intersect(names(.), as.character(2010:2035))))
        dplyr::select(., vessel, dplyr::any_of(as.character(yr_cols))) }

    results[[tbl]] <- summary

    message(glue("\n{strrep('=', 60)}"))
    message(glue("TABLE: {tbl}  |  value: {value}"))
    message(strrep("=", 60))
    print(summary, n = Inf)
  }

  invisible(results)
}


# ==============================================================================
# COLUMN STANDARDISATION
# ==============================================================================
#' Rename legacy/duplicate column names to canonical names and drop columns
#' that belong to a different parquet (e.g. catch-level columns on haul).
#'
#' Call this just before save_flyshoot_data() in the pipeline, or once on
#' existing parquets via standardise_all_parquets().
#'
#' Decisions based on glimpse() analysis 2026-06-05:
#'
#'  SPATIAL ZONES
#'    Keep : fao_division, ices_rect, economic_zone
#'    Drop  : area, division, rect, economiczone  (generic names, same data)
#'    Drop  : fao_area, fao_subarea (always NA in haul — not populated)
#'    Drop  : ices_rectangle, fao_zone, ices_area (legacy input names that
#'             leaked through; fao_division is the canonical output name)
#'    Drop  : latitude (duplicate of shoot_lat, always NA in haul)
#'
#'  PEOPLE
#'    Keep : skipper  (populated in elog; captain always NA)
#'    Drop : captain  (elog schema already maps captain -> skipper)
#'
#'  BOX COUNTS
#'    Keep : box_count  (one box per lot/row in kisten and elog_trek — always 1)
#'    Keep : lot_nr     (unique box identifier in kisten/elog_trek)
#'    Drop : boxes      (legacy elog input name, mapped to box_count in schema)
#'    Drop : box_number (legacy pefa name, mapped to box_count in schema)
#'    Note : box_count in haul is always NA — remove from haul
#'
#'  LEGACY CATCH COLS ON HAUL (all NA — belong in elog/kisten only)
#'    Drop from haul: species, species_code, weight_kg, catch_kg, box_count,
#'    record_nr, presentation, preservation, freshness, size, conversion_factor,
#'    loss_grams, boxes, weight_undersized, boxes_undersized, discard_reason
#'
#'  LEGACY IDENTIFIERS ON HAUL (all NA — belong in trip only)
#'    Drop from haul: vessel_number, trip_identifier, captain
#'
#'  MESH SIZE
#'    Keep : mesh_size_mm  (explicit unit in name)
#'    Drop : mesh_size     (ambiguous unit, always NA in haul)
#'
#' @param df       data frame to standardise
#' @param dataset  one of "haul","elog","elog_trek","kisten","trip","vessel_movement"
#' @return standardised data frame
standardise_columns <- function(df, dataset) {

  # ── 1. Universal renames (apply to any dataset that has these columns) ──────
  universal_renames <- c(
    # spatial: canonical name on left, legacy aliases on right
    "fao_division" = "fao_zone",        # fao_zone -> fao_division
    "fao_division" = "faozone",
    "ices_rect"    = "ices_rectangle",  # ices_rectangle -> ices_rect
    "economic_zone"= "economiczone",    # economiczone -> economic_zone
    "skipper"      = "captain",         # captain -> skipper
    "box_count"    = "boxes",           # boxes -> box_count
    "box_count"    = "box_number",      # box_number -> box_count
    "mesh_size_mm" = "mesh_size",       # mesh_size -> mesh_size_mm
    "haul_id"      = "haul",            # elog: haul -> haul_id
    "haul_id"      = "record_nr"        # haul: record_nr -> haul_id (where used)
  )

  for (canonical in unique(names(universal_renames))) {
    aliases <- universal_renames[names(universal_renames) == canonical]
    for (alias in aliases) {
      if (alias %in% names(df) && !canonical %in% names(df)) {
        df <- dplyr::rename(df, !!canonical := !!alias)
      } else if (alias %in% names(df) && canonical %in% names(df)) {
        # Both exist: coalesce into canonical, drop alias
        df[[canonical]] <- dplyr::coalesce(df[[canonical]], df[[alias]])
        df <- dplyr::select(df, -dplyr::all_of(alias))
      }
    }
  }

  # ── 2. Drop redundant spatial columns (always duplicated by canonical names) ─
  spatial_redundant <- c(
    "area", "division", "rect",          # too generic; same as fao/ices equivalents
    "fao_area", "fao_subarea",           # never populated in any parquet
    "ices_area",                         # never populated
    "latitude"                           # duplicate of shoot_lat, always NA
  )
  df <- dplyr::select(df, -dplyr::any_of(spatial_redundant))

  # ── 3. Dataset-specific column drops ─────────────────────────────────────────
  if (dataset == "haul") {
    # These are catch-level columns that belong in elog/kisten, not haul.
    # They appear on haul only because bind_rows() fills NA across sources.
    catch_cols_on_haul <- c(
      "species", "species_code", "weight_kg", "catch_kg",
      "presentation", "preservation", "freshness", "size",
      "conversion_factor", "loss_grams", "boxes_undersized",
      "weight_undersized", "box_count", "record_nr", "discard_reason"
    )
    # Legacy trip-context identifiers that are always NA on haul rows
    legacy_id_on_haul <- c(
      "vessel_number", "trip_identifier"
    )
    df <- dplyr::select(df,
                        -dplyr::any_of(c(catch_cols_on_haul, legacy_id_on_haul)))
  }

  if (dataset == "elog_trek") {
    # boxes_undersized always NA in elog_trek; undersized rows are pivoted to BMS
    df <- dplyr::select(df, -dplyr::any_of(c("boxes_undersized")))
  }

  # ── 4. Ensure haul_id is character ───────────────────────────────────────────
  if ("haul_id" %in% names(df) && !is.character(df$haul_id)) {
    df$haul_id <- as.character(df$haul_id)
  }

  df
}


#' Apply standardise_columns() to all parquets on disk and overwrite in place.
#'
#' Run once after pipeline changes, then the pipeline functions will keep
#' columns clean going forward via standardise_columns() before each save.
#'
#' @param parquet_dir  root raw data directory (contains haul/, elog/, etc.)
#' @param dry_run      if TRUE, report changes without writing
standardise_all_parquets <- function(
    parquet_dir = file.path("C:/Users/MartinPastoors/Martin Pastoors",
                            "FLYSHOOT - General/data/raw"),
    dry_run     = FALSE
) {
  library(arrow)
  library(dplyr)
  library(glue)

  datasets <- c("haul", "elog", "elog_trek", "kisten", "trip", "vessel_movement")

  for (ds in datasets) {
    pq_path <- file.path(parquet_dir, ds, paste0(ds, ".parquet"))
    if (!file.exists(pq_path)) {
      message(glue("  SKIP {ds} — file not found"))
      next
    }

    df_orig <- arrow::read_parquet(pq_path)
    df_new  <- standardise_columns(df_orig, ds)

    dropped  <- setdiff(names(df_orig), names(df_new))
    renamed  <- character(0)  # renames already reflected in df_new column names
    n_before <- ncol(df_orig)
    n_after  <- ncol(df_new)

    message(glue("\n{ds}: {n_before} cols -> {n_after} cols"))
    if (length(dropped) > 0)
      message(glue("  dropped : {paste(dropped, collapse=', ')}"))

    # Check for renames by comparing with original
    orig_names <- names(df_orig)
    new_names  <- names(df_new)
    added <- setdiff(new_names, orig_names)
    if (length(added) > 0)
      message(glue("  renamed -> : {paste(added, collapse=', ')}"))

    if (!dry_run) {
      # Write to a temp file first, then replace the original.
      # arrow::read_parquet() memory-maps the source file, keeping it locked
      # until the object is garbage-collected. Writing directly to pq_path
      # while df_orig is still in scope causes Windows error 1224.
      # Fix: write to sibling .tmp, rm() both frames, gc() to drop the map,
      # then rename .tmp -> pq_path (now unlocked).
      tmp_path <- paste0(pq_path, ".tmp")
      arrow::write_parquet(df_new, tmp_path)
      rm(df_orig, df_new)
      gc()
      file.rename(tmp_path, pq_path)
      message(glue("  Written."))
    } else {
      message(glue("  (dry run — not written)"))
    }
  }

  invisible(NULL)
}
