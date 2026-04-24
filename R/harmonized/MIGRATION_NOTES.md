# FLYSHOOT Tripreport — Migration Notes (March 2026)

## What changed from the December 2025 version

### 1. Data backend: `.RData` → Parquet

| Old (Dec 2025)                                          | New (Mar 2026)                                     |
|---------------------------------------------------------|----------------------------------------------------|
| `load(file.path(onedrive, "elog.RData"))`               | `arrow::read_parquet(file.path(flyshoot_root, "elog", "elog.parquet"))` |
| `load(file.path(onedrive, "haul.RData"))`               | `arrow::read_parquet(file.path(flyshoot_root, "haul", "haul.parquet"))` |
| `load(file.path(onedrive, "kisten.RData"))`             | `arrow::read_parquet(file.path(flyshoot_root, "kisten", "kisten.parquet"))` |
| `load(file.path(onedrive, "elog_trek.RData"))`          | `arrow::read_parquet(file.path(flyshoot_root, "elog_trek", "elog_trek.parquet"))` |
| `load(file.path(onedrive, "trip.RData"))`               | `arrow::read_parquet(file.path(flyshoot_root, "trip", "trip.parquet"))` |

The parquet root is resolved from the `ONEDRIVE_FLYSHOOT` environment variable (add to `.Renviron`).
Fallback: `<USERPROFILE>/Martin Pastoors/FLYSHOOT - General/rdata/`

### 2. Column name changes (Poseidat alignment)

| Dataset  | Old column   | New column      | Notes                                  |
|----------|--------------|-----------------|----------------------------------------|
| all      | `trip`       | `trip_id`       | Primary trip key, character type       |
| elog     | `catchdate`  | `date`          | Date type                              |
| elog     | `faozone`    | `fao_division`  | Spatial                                |
| elog     | `rect`       | `ices_rect`     | Spatial                                |
| elog     | `weight`     | `weight_kg`     | Numeric (kg)                           |
| elog     | `species`    | `species_code`  | FAO 3-alpha code, uppercase            |
| kisten   | `datetime`   | `weighing_time` | POSIXct UTC                            |
| kisten   | `soorten`    | `species_code`  | Parsed from Marelec species string     |
| all      | `lat`/`lon`  | `shoot_lat`/`shoot_lon` | Positional columns (except vessel_movement which retains `lat`/`lon`) |

### 3. Trip selection

Old code derived `settrip` using `elog$catchdate` and `elog$trip`.
New code uses `elog$date` and `elog$trip_id`.

### 4. Environment variable for parquet path

Add to your `.Renviron` (use `usethis::edit_r_environ()`):

```
ONEDRIVE_FLYSHOOT=C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/rdata
```

### 5. File structure (unchanged)

```
<flyshoot_root>/
  haul/haul.parquet
  kisten/kisten.parquet
  elog/elog.parquet
  elog_trek/elog_trek.parquet
  trip/trip.parquet
```

### 6. Render scripts

Both `render_tripreport.R` and `batch_render_tripreports.R`:
- Load elog from parquet (not RData)
- Use `trip_id` (not `trip`) and `date` (not `catchdate`) in filter
- Use `isoweek()` for week number (consistent with parquet data)
- Provide clearer error messages including the resolved parquet root path

### 7. Week numbering

The old code used `week()` (US week, starts Sunday).
The new code uses `isoweek()` (ISO 8601, starts Monday), matching the parquet data.
Output filenames use ISO week numbers.
