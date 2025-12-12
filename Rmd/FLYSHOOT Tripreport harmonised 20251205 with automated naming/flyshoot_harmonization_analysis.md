# FLYSHOOT Tripreport Harmonization Analysis

## Executive Summary

The two RMarkdown files represent different data streams:
- **v1**: Multi-source integration (haul data, kisten/marelec, elog, trip data, GFW)
- **v2**: PEFA-only (electronic logbook data only)

This analysis identifies key differences, inconsistencies, and provides recommendations for creating a unified, maintainable codebase.

---

## Key Structural Differences

### 1. Data Sources

**v1 loads:**
- haul.RData
- kisten.RData
- elog.RData
- elog_trek.RData
- trip.RData
- gfw.RData

**v2 loads:**
- elog.RData
- elog_trek.RData
- eez.df.RData (not in v1)

**Recommendation:**
Create a modular data loading system with flags to control which data sources to load:

```r
# Data source configuration
use_haul_data <- TRUE
use_kisten_data <- TRUE
use_gfw_data <- TRUE
use_elog_data <- TRUE

# Conditional loading
if(use_haul_data) load(file.path(onedrive, "haul.RData"))
if(use_kisten_data) load(file.path(onedrive, "kisten.RData"))
# etc.
```

### 2. Trip Selection Methods

**v1 approach (lines 25-30):**
- Hardcoded vessel and trip IDs
- Manual selection with commented examples

**v2 approach (lines 92-106):**
- Date-based selection
- More flexible and user-friendly
- Automatically derives trip IDs from date range

**Recommendation:**
Implement a dual-mode selection system:

```r
# Trip selection mode
selection_mode <- "date"  # or "manual"

if(selection_mode == "date") {
  # Date-based selection (v2 approach)
  setvessel <- "SCH135"
  startdate <- dmy("01/12/2025")
  enddate <- dmy("04/12/2025")
  
  settrip <- elog %>% 
    filter(vessel %in% setvessel, 
           catchdate >= startdate, 
           catchdate <= enddate) %>% 
    distinct(trip) %>% 
    pull(trip)
    
} else if(selection_mode == "manual") {
  # Manual selection (v1 approach)
  setvessel <- c("Z99", "SCH99")
  settrip <- c("2025264")
}
```

---

## Code Quality Issues & Improvements

### 3. Library Loading

**Inconsistency:**
- v1 loads `patchwork` in setup chunk (line 48)
- v2 loads `patchwork` in a specific chunk (line 964)

**Recommendation:**
Load all required libraries in setup chunk for consistency:

```r
# Standard libraries
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(viridis)
library(pander)
library(captioner)
library(patchwork)  # Always load here
library(icosa)      # Add to both versions
```

### 4. Spatial Data Loading

**Inconsistency:**
- v1 has commented-out spatial data loads (lines 64-67)
- v2 loads eez.df.RData (line 51) which v1 doesn't
- Both load slightly different combinations

**Recommendation:**
Standardize spatial data loading with clear documentation:

```r
# Spatial data files - load consistently
load(file.path(spatialdir, "world_lr_sf.RData"))
load(file.path(spatialdir, "world_mr_sf.RData"))
load(file.path(spatialdir, "world_lr_df.RData"))
load(file.path(spatialdir, "world_mr_df.RData"))

# Optional spatial data (load based on need)
if(file.exists(file.path(spatialdir, "eez.df.RData"))) {
  load(file.path(spatialdir, "eez.df.RData"))
}
```

### 5. Map Extent Calculation

**Major Difference:**

**v1 (lines 172-223):**
- Simple floor/ceiling approach
- Manual aspect ratio adjustments
- Commented-out geosphere calculations
- Hardcoded adjustments based on aspect ratio thresholds

**v2 (lines 124-163):**
- Sophisticated buffering (10% on each side)
- Dynamic aspect ratio correction
- Target aspect ratio approach
- Better handling of edge cases

**Recommendation:**
Adopt v2's superior approach with additional enhancements:

```r
# Function to calculate optimal map extent with buffer
calculate_map_extent <- function(data, lon_col = "lon", lat_col = "lat", 
                                 buffer_pct = 0.1, target_aspect = 1.0) {
  
  # Get data ranges, handling multiple data sources
  lon_range <- range(data[[lon_col]], na.rm = TRUE)
  lat_range <- range(data[[lat_col]], na.rm = TRUE)
  
  # Add buffer
  lon_buffer <- diff(lon_range) * buffer_pct
  lat_buffer <- diff(lat_range) * buffer_pct
  
  xmin <- floor(lon_range[1] - lon_buffer)
  xmax <- ceiling(lon_range[2] + lon_buffer)
  ymin <- floor((lat_range[1] - lat_buffer) * 2) / 2
  ymax <- ceiling((lat_range[2] + lat_buffer) * 2) / 2
  
  # Calculate actual distances using icosa
  xdistance <- icosa::arcdist(p1 = c(xmin, ymin), p2 = c(xmax, ymin))
  ydistance <- icosa::arcdist(p1 = c(xmin, ymin), p2 = c(xmin, ymax))
  data_aspect <- ydistance / xdistance
  
  # Adjust to target aspect ratio
  if (data_aspect > target_aspect) {
    extra_x <- (ydistance / target_aspect - xdistance) / 2
    extra_x_deg <- extra_x / 111  # approximate km to degrees
    xmin <- xmin - extra_x_deg
    xmax <- xmax + extra_x_deg
  } else {
    extra_y <- (xdistance * target_aspect - ydistance) / 2
    extra_y_deg <- extra_y / 111
    ymin <- ymin - extra_y_deg
    ymax <- ymax + extra_y_deg
  }
  
  return(list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
              xdistance = xdistance, ydistance = ydistance, 
              aspect = data_aspect))
}

# Usage
if(nrow(et) > 0) {
  extent <- calculate_map_extent(et, target_aspect = 1.0)
} else if(nrow(e) > 0) {
  extent <- calculate_map_extent(e, target_aspect = 1.0)
}

# Extract values
list2env(extent, envir = .GlobalEnv)
```

---

## Harmonized Structure Recommendations

### 6. Modular Configuration Section

Create a clear configuration section at the top:

```r
# ==========================================================================================================
# CONFIGURATION SECTION
# ==========================================================================================================

# Data source flags
use_haul_data    <- TRUE   # Self-sampling haul data
use_kisten_data  <- TRUE   # Marelec/scale data
use_elog_data    <- TRUE   # Electronic logbook
use_gfw_data     <- FALSE  # Global Fishing Watch data
use_trip_data    <- TRUE   # Trip summary data

# Report options
comparisons      <- FALSE  # Include comparison plots
target_aspect    <- 1.0    # Map aspect ratio (1.0 = square)
buffer_pct       <- 0.1    # Map buffer as percentage (0.1 = 10%)

# Trip selection mode
selection_mode   <- "date" # Options: "date" or "manual"

# Trip selection parameters
if(selection_mode == "date") {
  setvessel  <- "SCH135"
  startdate  <- dmy("01/12/2025")
  enddate    <- dmy("04/12/2025")
} else {
  setvessel  <- c("Z99", "SCH99")
  settrip    <- c("2025264")
}
```

### 7. Data Filtering Strategy

**Issue:** v1 creates objects `h`, `m`, `e`, `et`, `tt`, `g` while v2 only uses `e` and `et`

**Recommendation:**
Create a unified filtering function:

```r
# Universal data filtering function
filter_trip_data <- function(data, vessel_filter, trip_filter = NULL, 
                             date_range = NULL, data_type = NULL) {
  
  result <- data %>% 
    filter(vessel %in% vessel_filter)
  
  if(!is.null(trip_filter)) {
    result <- result %>% filter(trip %in% trip_filter)
  }
  
  if(!is.null(date_range)) {
    date_col <- case_when(
      data_type == "elog" ~ "catchdate",
      data_type == "haul" ~ "haultime",
      data_type == "kisten" ~ "datetime",
      TRUE ~ "date"
    )
    result <- result %>% 
      filter(!!sym(date_col) >= date_range[1],
             !!sym(date_col) <= date_range[2])
  }
  
  # Add data-specific cleaning
  if(data_type == "elog_trek") {
    result <- result %>% 
      mutate(lon = ifelse(lon == 0 & lat == 0, NA, lon),
             lat = ifelse(is.na(lon) & lat == 0, NA, lat))
  }
  
  return(result)
}

# Apply filters conditionally
if(selection_mode == "date") {
  if(use_elog_data) {
    # Derive trips from date range
    settrip <- elog %>% 
      filter(vessel %in% setvessel,
             catchdate >= startdate,
             catchdate <= enddate) %>% 
      distinct(trip) %>% 
      pull(trip)
  }
}

# Filter all data sources
if(use_elog_data) {
  e <- filter_trip_data(elog, setvessel, settrip, data_type = "elog")
  et <- filter_trip_data(elog_trek, setvessel, settrip, data_type = "elog_trek")
}

if(use_haul_data) {
  h <- filter_trip_data(haul, setvessel, settrip, data_type = "haul")
}

if(use_kisten_data) {
  m <- filter_trip_data(kisten, setvessel, settrip, data_type = "kisten") %>% 
    mutate(soorten = tolower(soorten)) %>% 
    distinct()
}
```

---

## Figure Sizing & Layout

### 8. Dynamic Figure Dimensions

**v2 has superior approach (lines 1020-1040):**

**Recommendation:**
Create a reusable function for dynamic figure sizing:

```r
# Function to calculate optimal figure layout
calculate_figure_layout <- function(n_panels, plot_type = "map") {
  
  if(plot_type == "map") {
    # Maps look better in grid layouts
    if(n_panels <= 4) {
      ncol <- 2
      panel_aspect <- 1.0  # square maps
    } else if(n_panels <= 9) {
      ncol <- 3
      panel_aspect <- 1.0
    } else if(n_panels <= 16) {
      ncol <- 4
      panel_aspect <- 1.0
    } else {
      ncol <- 5
      panel_aspect <- 1.0
    }
  } else if(plot_type == "timeseries") {
    # Time series look better wider
    if(n_panels <= 4) {
      ncol <- 2
      panel_aspect <- 1.25  # slightly wider
    } else if(n_panels <= 9) {
      ncol <- 3
      panel_aspect <- 1.25
    } else {
      ncol <- 4
      panel_aspect <- 1.25
    }
  }
  
  nrows <- ceiling(n_panels / ncol)
  fig_width <- knitr::opts_chunk$get("fig.width")
  fig_height <- (fig_width / ncol) * panel_aspect * nrows + 1.5
  fig_height <- min(fig_height, 10)  # cap height
  
  return(list(ncol = ncol, nrows = nrows, 
              fig_height = fig_height, 
              panel_aspect = panel_aspect))
}

# Usage in chunk header
layout <- calculate_figure_layout(nspecies_actual, plot_type = "timeseries")
```

---

## Code Organization Issues

### 9. Commented-Out Code

**Problem:**
Both files contain extensive commented-out code that makes maintenance difficult:
- v1: Lines 110, 140, 177-195, 208-223, 225-241
- Multiple eval=FALSE chunks

**Recommendation:**
- Remove dead code or move to archive file
- Keep only code that's actively being tested
- Use git for version control instead of commented blocks

```r
# If keeping experimental code:
if(FALSE) {  # Better than commenting
  # Experimental code here
  # Can be easily toggled for testing
}
```

### 10. Hardcoded Paths

**Issue:**
Hardcoded paths in v1 and v2:
```r
spatialdir = "C:/Users/MartinPastoors/OneDrive - Martin Pastoors/DATA/RDATA"
```

**Recommendation:**
```r
# Get user-specific spatial directory
get_spatial_dir <- function() {
  user_home <- Sys.getenv("USERPROFILE")  # Windows
  if(user_home == "") user_home <- Sys.getenv("HOME")  # Mac/Linux
  
  # Try different potential locations
  potential_paths <- c(
    file.path(user_home, "OneDrive - Martin Pastoors/DATA/RDATA"),
    file.path(user_home, "OneDrive/DATA/RDATA"),
    "../DATA/RDATA"  # Relative path fallback
  )
  
  for(path in potential_paths) {
    if(dir.exists(path)) return(path)
  }
  
  stop("Could not find spatial data directory. Please set spatialdir manually.")
}

spatialdir <- get_spatial_dir()
```

---

## Species Handling

### 11. Species Code Standardization

**Inconsistency:**
- v2 has: `mutate(species = ifelse(species == "SQC", "SQR", species))`
- This appears in multiple places but not consistently

**Recommendation:**
Create a species standardization function:

```r
# Species code standardization
standardize_species <- function(data, species_col = "species") {
  data %>% 
    mutate(
      !!species_col := case_when(
        !!sym(species_col) == "SQC" ~ "SQR",  # Squid codes
        # Add other standardizations here
        TRUE ~ !!sym(species_col)
      )
    )
}

# Apply consistently
e <- e %>% standardize_species()
et <- et %>% standardize_species()
```

---

## Proposed Unified File Structure

```
FLYSHOOT_tripreport.Rmd (main file)
├── 00_configuration.R           (parameters, flags)
├── 01_data_loading.R            (conditional data loading)
├── 02_data_filtering.R          (filtering functions)
├── 03_spatial_functions.R       (map extent, projections)
├── 04_plotting_functions.R      (reusable plot functions)
└── 05_figure_layouts.R          (dynamic sizing functions)
```

**Main RMarkdown structure:**

```r
# Setup chunk
source("00_configuration.R")
source("01_data_loading.R")
source("02_data_filtering.R")
source("03_spatial_functions.R")
source("04_plotting_functions.R")
source("05_figure_layouts.R")

# Then regular reporting chunks
```

---

## Critical Harmonization Actions

### Priority 1 (High Impact)
1. ✅ Unify trip selection mechanism (date vs manual)
2. ✅ Standardize map extent calculation (adopt v2's approach)
3. ✅ Create modular data loading system
4. ✅ Implement dynamic figure sizing throughout

### Priority 2 (Code Quality)
5. ✅ Remove/archive commented-out code
6. ✅ Standardize library loading
7. ✅ Fix hardcoded paths
8. ✅ Standardize species codes

### Priority 3 (Future Improvements)
9. ⬜ Create reusable plotting functions
10. ⬜ Implement comprehensive error handling
11. ⬜ Add data validation checks
12. ⬜ Create unit tests for key functions

---

## Specific Code Examples for Harmonization

### Example 1: Unified Trip Summary

Both versions create trip summaries differently. Unify as:

```r
# Unified trip summary function
create_trip_summary <- function(data, data_source = "elog_trek") {
  
  date_col <- case_when(
    data_source == "elog_trek" ~ "date",
    data_source == "elog" ~ "catchdate",
    data_source == "haul" ~ "haultime",
    TRUE ~ "date"
  )
  
  haul_col <- case_when(
    data_source %in% c("elog_trek", "elog") ~ "haul",
    data_source == "haul" ~ "haul",
    TRUE ~ "haul"
  )
  
  data %>%
    group_by(vessel, trip) %>%
    summarise(
      nhauls = n_distinct(!!sym(haul_col)),
      startdate = min(!!sym(date_col), na.rm = TRUE),
      enddate = max(!!sym(date_col), na.rm = TRUE),
      weeks = paste(unique(week), collapse = ", "),
      months = paste(unique(month), collapse = ", "),
      divisions = paste(unique(na.omit(faozone)), collapse = "; "),
      rects = paste(unique(na.omit(rect)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(
      ndays = as.numeric(difftime(enddate, startdate, units = "days")) + 1,
      haulsperday = nhauls / ndays
    )
}
```

### Example 2: Unified Catch Summary Plot

Create a reusable function:

```r
# Reusable catch summary plot
plot_catch_summary <- function(data, vessel_name, years = 2023:2025,
                              ncol = 3, plot_type = "bar") {
  
  # Data preparation
  plot_data <- data %>%
    filter(vessel %in% vessel_name, 
           year %in% years,
           species %in% myspecies$species) %>%
    mutate(vessel = ifelse(vessel %in% c("SCH99", "Z99"), 
                          "SCH99/Z99", vessel)) %>%
    mutate(species = factor(species, levels = myspecies$species))
  
  # Create plot
  p <- plot_data %>%
    ggplot(aes(x = week, y = weight)) +
    theme_publication() +
    labs(
      y = "aanvoer (kg)",
      title = paste(vessel_name, "aanvoer per soort en week"),
      fill = ""
    )
  
  if(plot_type == "bar") {
    p <- p + 
      geom_bar(aes(fill = english_species),
               stat = "identity",
               position = position_stack(reverse = TRUE),
               colour = "lightgray",
               linewidth = 0.3) +
      scale_fill_brewer(palette = "Paired")
  } else if(plot_type == "line") {
    p <- p +
      geom_line(aes(colour = english_species, group = english_species)) +
      scale_colour_brewer(palette = "Paired")
  }
  
  p + 
    guides(fill = guide_legend(nrow = 2)) +
    facet_wrap(~year, ncol = ncol)
}
```

---

## Testing & Validation

### Recommended Tests

1. **Data availability tests:**
```r
# Check data availability before processing
check_data_availability <- function() {
  issues <- character()
  
  if(use_haul_data && nrow(h) == 0) {
    issues <- c(issues, "No haul data found")
  }
  if(use_elog_data && nrow(e) == 0) {
    issues <- c(issues, "No elog data found")
  }
  if(use_kisten_data && nrow(m) == 0) {
    issues <- c(issues, "No kisten data found")
  }
  
  if(length(issues) > 0) {
    warning("Data availability issues:\n", 
            paste(issues, collapse = "\n"))
    return(FALSE)
  }
  return(TRUE)
}
```

2. **Spatial extent validation:**
```r
# Validate map extent
validate_extent <- function(xmin, xmax, ymin, ymax) {
  if(xmin >= xmax) stop("Invalid longitude range")
  if(ymin >= ymax) stop("Invalid latitude range")
  if(abs(xmax - xmin) > 180) warning("Longitude range > 180 degrees")
  if(abs(ymax - ymin) > 90) warning("Latitude range > 90 degrees")
}
```

---

## Migration Path

### Phase 1: Immediate Harmonization
1. Create unified configuration section
2. Standardize data loading and filtering
3. Adopt v2's map extent calculation
4. Remove commented code

### Phase 2: Code Quality
1. Extract functions to separate files
2. Implement error handling
3. Add data validation
4. Create reusable plot functions

### Phase 3: Advanced Features
1. Add automated testing
2. Create interactive HTML reports option
3. Implement caching for expensive operations
4. Add multi-vessel comparison mode

---

## Conclusion

The two files serve different purposes but share significant overlap. The recommended approach is:

1. **Create a single master file** with configuration flags to enable/disable features
2. **Adopt v2's superior approaches** for map extent and figure sizing
3. **Keep v1's multi-source capabilities** but make them optional
4. **Extract reusable code** into separate utility files
5. **Maintain backwards compatibility** through configuration flags

This will provide:
- ✅ Single codebase to maintain
- ✅ Flexibility for different data streams
- ✅ Better code organization
- ✅ Easier testing and debugging
- ✅ More generic reusability across vessels

### Next Steps
1. Review this analysis with stakeholders
2. Create unified template file
3. Test with both data streams
4. Document configuration options
5. Archive old versions with git
