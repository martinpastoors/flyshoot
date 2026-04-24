# ============================================================================
# debug_pefa_trek_processing.R
#
# Mirrors read_pefa_trek() step by step so you can inspect the data
# at each transformation stage.
#
# Set pefa_file to the path of your SCH135 elog_trek Excel file.
# ============================================================================

library(tidyverse)
library(readxl)
library(glue)

project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop("Set FLYSHOOT_PROJECT in .Renviron")
p <- function(...) file.path(project_root, ...)
withr::with_dir(project_root, source(p("R", "harmonized", "01_flyshoot_functions.R")))

# ── SET THIS TO YOUR FILE ─────────────────────────────────────────────────────

pefa_file <- r"(C:\Users\MartinPastoors\Martin Pastoors\FLYSHOOT - General\tripdata\_verwerkt\SCH135 20260316-20260319 elog_pefa_per_trek.xlsx)"

# ─────────────────────────────────────────────────────────────────────────────

stopifnot(file.exists(pefa_file))
cat(glue("\nFile: {basename(pefa_file)}\n\n"))

sep <- function(title) cat(paste0("\n", strrep("=",60), "\n", title, "\n", strrep("=",60), "\n"))

# ── STEP 1: Raw read ──────────────────────────────────────────────────────────
sep("STEP 1: Raw Excel read (col_types = text)")
raw <- read_excel(pefa_file, sheet = 1, col_names = TRUE,
                  .name_repair = ~make.names(., unique = TRUE)) %>%
  rename_with(tolower)

cat(glue("Rows: {nrow(raw)} | Cols: {ncol(raw)}\n"))
cat("Column names:\n")
print(names(raw))
cat("\nhead(3):\n")
print(head(raw, 3), width = 200)

# ── STEP 2: map_pefa_columns ──────────────────────────────────────────────────
sep("STEP 2: map_columns()")
pefa_data <- map_columns(raw)

cat(glue("Rows: {nrow(pefa_data)} | Cols: {ncol(pefa_data)}\n"))
cat("Column names after mapping:\n")
print(names(pefa_data))
cat("\nKey columns check:\n")
for (col in c("vessel", "trip_id", "haul_id", "date", "shoot_lat", "shoot_lon",
              "species_code", "weight_kg", "trip")) {
  if (col %in% names(pefa_data)) {
    n_na  <- sum(is.na(pefa_data[[col]]))
    n_ok  <- nrow(pefa_data) - n_na
    sample <- paste(head(unique(pefa_data[[col]][!is.na(pefa_data[[col]])]), 3), collapse=", ")
    cat(glue("  {col}: {n_ok} non-NA / {nrow(pefa_data)} | sample: {sample}\n"))
  } else {
    cat(glue("  {col}: MISSING from mapped data\n"))
  }
}

# ── STEP 3: Trip ID resolution ────────────────────────────────────────────────
sep("STEP 3: Trip ID resolution")
ids <- extract_vessel_trip(pefa_file)
cat(glue("From filename: vessel={ids$vessel}, trip_id={ids$trip_id}\n"))

if ("trip_identifier" %in% names(pefa_data)) {
  trip_id_file <- as.character(pefa_data$trip_identifier[1]) %>% stringr::str_trim()
  cat(glue("From file 'trip' column: {trip_id_file}\n"))
  trip_id <- trip_id_file
} else {
  cat("No 'trip' column in file — using filename-derived id\n")
  trip_id <- ids$trip_id
}
cat(glue("→ Using trip_id: {trip_id}\n"))

vessel_id <- ids$vessel

# ── STEP 4: full_data mutate ──────────────────────────────────────────────────
sep("STEP 4: full_data construction (rename + type coercion)")
full_data <- pefa_data %>%
  rename(haul_id_orig = any_of("haul_id")) %>%
  mutate(
    vessel       = vessel_id,
    trip_id      = trip_id,
    haul_id_orig = if ("haul_id_orig" %in% names(.)) as.integer(haul_id_orig) else NA_integer_,
    date         = if ("date" %in% names(.)) as.Date(date) else as.Date(NA),
    shoot_lat    = if ("shoot_lat" %in% names(.)) as.numeric(shoot_lat) else NA_real_,
    shoot_lon    = if ("shoot_lon" %in% names(.)) as.numeric(shoot_lon) else NA_real_,
    species_code = if ("species" %in% names(.)) toupper(species) else NA_character_,
    weight_kg    = if ("catch_kg" %in% names(.)) as.numeric(catch_kg) else NA_real_,
    box_count    = if ("box_number" %in% names(.)) as.integer(box_number) else NA_integer_
  )

cat(glue("Rows: {nrow(full_data)}\n"))
for (col in c("vessel", "trip_id", "haul_id_orig", "date", "shoot_lat", "shoot_lon",
              "species_code", "weight_kg", "box_count")) {
  if (col %in% names(full_data)) {
    n_na <- sum(is.na(full_data[[col]]))
    n_ok <- nrow(full_data) - n_na
    cat(glue("  {col}: {n_ok} non-NA / {nrow(full_data)}\n"))
    if (n_ok > 0) {
      sample <- paste(head(unique(full_data[[col]][!is.na(full_data[[col]])]), 3), collapse=", ")
      cat(glue("    sample: {sample}\n"))
    }
  }
}

# ── STEP 5: haul_key ─────────────────────────────────────────────────────────
sep("STEP 5: haul_key (renumber hauls)")
haul_key <- full_data %>%
  distinct(vessel, trip_id, haul_id_orig, date,
           shoot_lat, shoot_lon) %>%
  arrange(vessel, trip_id, date) %>%
  group_by(vessel, trip_id) %>%
  mutate(haul_id = row_number()) %>%
  ungroup() %>%
  select(vessel, trip_id, haul_id_orig, haul_id)

cat(glue("{nrow(haul_key)} hauls\n"))
print(haul_key)

# ── STEP 6: catch_data ────────────────────────────────────────────────────────
sep("STEP 6: catch_data (what goes into elog_trek parquet)")
catch_data <- full_data %>%
  left_join(haul_key, by = c("vessel", "trip_id", "haul_id_orig")) %>%
  select(-haul_id_orig) %>%
  select(vessel, trip_id, haul_id, date,
         shoot_lat, shoot_lon,
         species_code, weight_kg, box_count,
         gear_type,
         any_of(c("presentation", "preservation", "freshness",
                  "conversion_factor", "weight_undersized_kg",
                  "boxes_undersized", "discard_reason"))) %>%
  filter(!is.na(species_code))

cat(glue("Rows: {nrow(catch_data)} | Cols: {ncol(catch_data)}\n"))
cat("Columns:\n")
print(names(catch_data))
cat("\nKey column NA counts:\n")
for (col in names(catch_data)) {
  n_na <- sum(is.na(catch_data[[col]]))
  if (n_na > 0) cat(glue("  {col}: {n_na} NA / {nrow(catch_data)}\n"))
}
cat("\nhead(5):\n")
print(head(catch_data, 5), width = 200)

# ── STEP 7: Compare with existing parquet ────────────────────────────────────
sep("STEP 7: Compare with existing elog_trek parquet")
withr::with_dir(project_root, source(p("R", "harmonized", "02_storage_functions.R")))
config <- jsonlite::read_json(p("config.json"))

et_parquet <- tryCatch(
  load_flyshoot_data("elog_trek") %>%
    dplyr::filter(vessel == vessel_id, trip_id == trip_id),
  error = function(e) { cat(glue("Could not load parquet: {e$message}\n")); tibble() }
)

cat(glue("Parquet elog_trek for {vessel_id} / {trip_id}: {nrow(et_parquet)} rows\n"))
if (nrow(et_parquet) > 0) {
  cat("Parquet columns:\n")
  print(names(et_parquet))
  cat("\nParquet NA counts:\n")
  for (col in names(et_parquet)) {
    n_na <- sum(is.na(et_parquet[[col]]))
    if (n_na > 0) cat(glue("  {col}: {n_na} NA / {nrow(et_parquet)}\n"))
  }
  cat("\nParquet head(3):\n")
  print(head(et_parquet, 3), width = 200)
}

cat("\n✓ Debug complete\n")



elog_trek_raw %>% 
  filter(vessel == "SCH135") %>% 
  filter(date >= dmy("10-03-2026")) %>% 
  distinct(vessel, trip_nr, trip_nr_elog) %>% 
  print()

elog_trek_raw %>% 
  filter(date >= dmy("01-01-2026")) %>% 
  distinct(vessel, trip_nr, trip_nr_elog) %>% 
  print(n=999)

elog_raw %>% 
  filter(date >= dmy("01-01-2026")) %>% 
  distinct(vessel, trip_nr, trip_nr_elog) %>% 
  print(n=999)

# Remove the stale date-concatenated trip_id rows and re-save
et_fixed <- load_flyshoot_data("elog_trek") %>%
  dplyr::filter(!(vessel == "SCH135" & trip_id == "2026031620260319"))

save_flyshoot_data(et_fixed, "elog_trek",
                   date_range = c(min(et_fixed$date, na.rm=TRUE),
                                  max(et_fixed$date, na.rm=TRUE)))

for (type in c("elog", "elog_trek", "haul", "kisten", "trip", "vessel_movement")) {
  dat <- tryCatch(load_flyshoot_data(type), error = function(e) NULL)
  if (is.null(dat) || !"trip_id" %in% names(dat)) next
  n_before <- nrow(dat)
  dat_clean <- dplyr::filter(dat, !is.na(trip_id))
  n_removed <- n_before - nrow(dat_clean)
  if (n_removed > 0) {
    save_flyshoot_data(dat_clean, type,
                       date_range = c(min(dat_clean$date, na.rm=TRUE),
                                      max(dat_clean$date, na.rm=TRUE)))
    cat(glue("  {type}: removed {n_removed} NA trip_id rows\n"))
  }
}

# SCH99 missing trips
elog_raw %>% 
  filter(vessel == "SCH99") %>%
  distinct(trip_id, trip_nr, trip_nr_elog) %>%
  arrange(trip_nr) %>%
  tail(10)

# SCH135 most recent trip in elog vs elog_trek
cat("\n=== SCH135 most recent trips ===\n")
elog_raw %>% 
  filter(vessel == "SCH135", date >= dmy("01-03-2026")) %>%
  distinct(trip_id, trip_nr, date) %>% arrange(date) %>% print(n=99)

elog_trek_raw %>% 
  filter(vessel == "SCH135", date >= dmy("01-03-2026")) %>%
  distinct(trip_id, trip_nr, date) %>% arrange(date) %>% print(n=99)

# Check what the stale date-concatenated trip_ids look like now
cat("\n=== Any remaining date-format trip_ids (16 digit) ===\n")
elog_raw %>% 
  filter(nchar(trip_id) == 16, grepl("^[0-9]{16}$", trip_id)) %>%
  distinct(vessel, trip_id) %>% print(n=99)

elog_trek_raw %>% 
  filter(nchar(trip_id) == 16, grepl("^[0-9]{16}$", trip_id)) %>%
  distinct(vessel, trip_id) %>% print(n=99)






# Check which parquets have this trip
for (type in c("elog", "elog_trek", "haul", "kisten", "trip", "vessel_movement")) {
  dat <- tryCatch(load_flyshoot_data(type), error = function(e) NULL)
  if (is.null(dat) || !"trip_id" %in% names(dat)) next
  n <- sum(dat$vessel == "SCH99" & dat$trip_id == "2026030900012", na.rm = TRUE)
  if (n > 0) cat(glue("{type}: {n} rows\n"))
}


# Remove SCH99 trip 2026030900012 from all parquets
for (type in c("elog", "elog_trek", "haul", "kisten", "trip", "vessel_movement")) {
  dat <- tryCatch(load_flyshoot_data(type), error = function(e) NULL)
  if (is.null(dat) || !"trip_id" %in% names(dat)) next
  n_before <- nrow(dat)
  dat_clean <- dplyr::filter(dat, !(vessel == "SCH99" & trip_id == "2026030900012"))
  n_removed <- n_before - nrow(dat_clean)
  if (n_removed > 0) {
    save_flyshoot_data(dat_clean, type,
                       date_range = c(min(dat_clean$date, na.rm = TRUE),
                                      max(dat_clean$date, na.rm = TRUE)))
    cat(glue("  {type}: removed {n_removed} rows\n"))
  }
}

