# ============================================================================
# check_processed_data.R
#
# Shows completeness of recently processed trips in all parquet files.
# Run after 03_main_workflow.R to verify what was saved.
# ============================================================================

library(tidyverse)
library(lubridate)
library(arrow)
library(glue)

project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop("Set FLYSHOOT_PROJECT in .Renviron")
p <- function(...) file.path(project_root, ...)
config <- jsonlite::read_json(p("config.json"))
withr::with_dir(project_root, source(p("R", "harmonized", "02_storage_functions.R")))

# ── PARAMETERS — adjust to the trips just processed ──────────────────────────
check_vessel  <- "SCH99"
check_trips   <- c("2026282", "2026283")   # trip_ids just processed
# ─────────────────────────────────────────────────────────────────────────────

cat(strrep("=", 70), "\n")
cat(glue("DATA COMPLETENESS CHECK: {check_vessel} trips {paste(check_trips, collapse=', ')}\n"))
cat(strrep("=", 70), "\n\n")

# Helper: summarise completeness of a dataset
completeness <- function(df, label) {
  if (nrow(df) == 0) {
    cat(glue("  {label}: ⚠ EMPTY\n\n"))
    return(invisible(NULL))
  }
  cat(glue("  {label}: {nrow(df)} rows × {ncol(df)} cols\n"))

  # Date range
  if ("date" %in% names(df)) {
    cat(glue("    date range: {min(df$date,na.rm=TRUE)} – {max(df$date,na.rm=TRUE)}\n"))
  }

  # trip_id and trip_nr_elog
  for (col in c("trip_id", "trip_nr", "trip_nr_elog")) {
    if (col %in% names(df)) {
      vals <- paste(sort(unique(na.omit(df[[col]]))), collapse=", ")
      n_na <- sum(is.na(df[[col]]))
      cat(glue("    {col}: {vals} ({n_na} NA)\n"))
    }
  }

  # Key numeric columns — show % complete
  key_cols <- c("haul_id", "shoot_lat", "shoot_lon", "weight_kg",
                "species_code", "total_catch_kg", "lat", "lon")
  present <- intersect(key_cols, names(df))
  if (length(present) > 0) {
    cat("    Column completeness:\n")
    for (col in present) {
      n_ok  <- sum(!is.na(df[[col]]))
      pct   <- round(100 * n_ok / nrow(df))
      flag  <- if (pct < 50) "⚠" else if (pct < 100) "~" else "✓"
      cat(glue("      {flag} {col}: {n_ok}/{nrow(df)} ({pct}%)\n"))
    }
  }

  # Show all columns with >0 NA
  other_na <- names(df)[sapply(names(df), function(x) {
    !(x %in% key_cols) && sum(is.na(df[[x]])) > 0 &&
    sum(is.na(df[[x]])) < nrow(df)
  })]
  if (length(other_na) > 0) {
    cat("    Partial NAs in:\n")
    for (col in other_na) {
      n_na <- sum(is.na(df[[col]]))
      cat(glue("      {col}: {n_na}/{nrow(df)} NA\n"))
    }
  }

  # Completely empty columns
  all_na <- names(df)[sapply(names(df), function(x) all(is.na(df[[x]])))]
  if (length(all_na) > 0) {
    cat(glue("    All-NA columns: {paste(all_na, collapse=', ')}\n"))
  }

  cat("\n")
  invisible(df)
}

# ── Check each dataset ────────────────────────────────────────────────────────
for (type in c("elog", "elog_trek", "haul", "kisten", "trip", "vessel_movement")) {
  dat <- tryCatch(load_flyshoot_data(type), error = function(e) {
    cat(glue("  {type}: could not load ({e$message})\n\n")); NULL
  })
  if (is.null(dat) || !"trip_id" %in% names(dat)) next

  filtered <- dplyr::filter(dat, vessel == check_vessel, trip_id %in% check_trips)
  completeness(filtered, type)
}

# ── Position summary for maps ─────────────────────────────────────────────────
cat(strrep("=", 70), "\n")
cat("POSITION DATA SUMMARY (needed for Figure 1)\n")
cat(strrep("=", 70), "\n\n")

for (type in c("elog", "elog_trek", "haul")) {
  dat <- tryCatch(load_flyshoot_data(type), error = function(e) NULL)
  if (is.null(dat)) next
  df <- dplyr::filter(dat, vessel == check_vessel, trip_id %in% check_trips)
  if (nrow(df) == 0) next
  if (!all(c("shoot_lon", "shoot_lat") %in% names(df))) {
    cat(glue("  {type}: no shoot_lon/shoot_lat columns\n"))
    next
  }
  n_pos <- sum(!is.na(df$shoot_lon) & !is.na(df$shoot_lat) &
               df$shoot_lon != 0 & df$shoot_lat != 0)
  cat(glue("  {type}: {n_pos}/{nrow(df)} rows have valid positions\n"))
  if (n_pos > 0) {
    cat(glue("    lon: {round(min(df$shoot_lon,na.rm=TRUE),3)} – ",
             "{round(max(df$shoot_lon,na.rm=TRUE),3)}\n"))
    cat(glue("    lat: {round(min(df$shoot_lat,na.rm=TRUE),3)} – ",
             "{round(max(df$shoot_lat,na.rm=TRUE),3)}\n"))
  }
}

vm <- tryCatch(load_flyshoot_data("vessel_movement"), error = function(e) NULL)
if (!is.null(vm)) {
  df <- dplyr::filter(vm, vessel == check_vessel, trip_id %in% check_trips)
  n_pos <- sum(!is.na(df$lat) & !is.na(df$lon))
  cat(glue("  vessel_movement: {n_pos}/{nrow(df)} rows have valid lat/lon\n"))
}

cat("\n✓ Completeness check done\n")


# Check what's in each dataset for 2026282 and 2026283
for (type in c("elog", "haul", "kisten", "trip", "vessel_movement")) {
  dat <- tryCatch(load_flyshoot_data(type), error = function(e) NULL)
  if (is.null(dat) || !"trip_id" %in% names(dat)) next
  df <- dplyr::filter(dat, vessel == "SCH99", trip_id %in% c("2026282", "2026283"))
  if (nrow(df) > 0)
    cat(glue("{type}: {nrow(df)} rows | trips: {paste(unique(df$trip_id),collapse=',')} | dates: {min(df$date,na.rm=TRUE)} – {max(df$date,na.rm=TRUE)}\n"))
  else
    cat(glue("{type}: EMPTY for 2026282/2026283\n"))
}




# What files were processed?
file_inventory %>% 
  select(vessel, trip, source, filename) %>% 
  print(n = 99)

# What trips were grouped?
trip_groups %>% 
  rowwise() %>% 
  mutate(sources = paste(sources[[1]], collapse=", ")) %>% 
  select(vessel, trip, n_files, sources) %>% 
  print(n = 99)

# Check exact trip values per file
file_inventory %>%
  mutate(trip_raw = trip) %>%
  select(filename, trip_raw, source)


# Run this to test the treklijst directly:
source("C:/GIT/flyshoot/R/harmonized/01_flyshoot_functions.R")
trek_file <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_te verwerken\SCH99 2026_282 treklijst wk 11.xlsx)"
tryCatch(
  get_haul_from_treklijst(trek_file),
  error = function(e) cat("ERROR:", e$message, "\n")
)


kisten_file <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_te verwerken\SCH99 2026_282 kisten-reis-282-trek-30.xlsx)"
trek_file   <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_te verwerken\SCH99 2026_282 treklijst wk 11.xlsx)"

tryCatch(
  get_catch_from_kisten(kisten_file, treklijst_path = trek_file),
  error = function(e) cat("ERROR:", e$message, "\n")
)




source("C:/GIT/flyshoot/R/harmonized/01_flyshoot_functions.R")
source("C:/GIT/flyshoot/R/harmonized/02_storage_functions.R")

vessel_id <- "SCH99"
trip_id   <- "2026282"
trek_file   <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_te verwerken\SCH99 2026_282 treklijst wk 11.xlsx)"
kisten_file <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_te verwerken\SCH99 2026_282 kisten-reis-282-trek-30.xlsx)"
pefa_file   <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_te verwerken\SCH99 2026_282 elog pefa.xlsx)"

# Step 1
cat("Step 1: treklijst hauls\n")
trip_hauls <- get_haul_from_treklijst(trek_file)
cat(glue("  {nrow(trip_hauls)} hauls\n"))

# Step 2
trip_tz <- first(na.omit(trip_hauls$timezone))
cat(glue("Step 2: kisten catches (tz={trip_tz})\n"))
trip_catches <- get_catch_from_kisten(kisten_file, local_tz = trip_tz,
                                      treklijst_path = trek_file)
cat(glue("  {nrow(trip_catches)} catch records\n"))

# Step 3
cat("Step 3: trip info\n")
trip_info <- get_trip_info(trek_file, haul_data = trip_hauls)
cat(glue("  {nrow(trip_info)} trip info rows\n"))

# Step 4 — elog
cat("Step 4: elog\n")
elog_result <- tryCatch(get_data_from_elog(pefa_file),
                        error = function(e) { cat("ERROR:", e$message, "\n"); NULL })
if (!is.null(elog_result)) cat(glue("  {nrow(elog_result$catch)} elog records\n"))

# Step 5 — save attempt
cat("Step 5: bind_rows with existing haul\n")
haul_existing <- load_flyshoot_data("haul")
new_hauls <- trip_hauls
tryCatch(
  dplyr::bind_rows(haul_existing, new_hauls),
  error = function(e) cat("bind_rows ERROR:", e$message, "\n")
)

