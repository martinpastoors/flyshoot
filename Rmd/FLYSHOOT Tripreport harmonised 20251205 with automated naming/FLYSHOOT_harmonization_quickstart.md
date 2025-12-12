# FLYSHOOT Tripreport Harmonized Template - Quick Start Guide

## Overview

The harmonized template combines both v1 and v2 functionality into a single, flexible RMarkdown file that can handle:
- Multiple data sources (haul, kisten, elog, gfw)
- Both manual and date-based trip selection
- Conditional reporting based on data availability

## Quick Start: 3 Steps to Generate Report

### Step 1: Configure Data Sources

At the top of the RMarkdown file, set which data sources you want to use:

```r
# Data source flags
use_haul_data    <- TRUE   # Self-sampling haul data
use_kisten_data  <- TRUE   # Marelec/scale data
use_elog_data    <- TRUE   # Electronic logbook (PEFA)
use_gfw_data     <- FALSE  # Global Fishing Watch
use_trip_data    <- TRUE   # Trip summary data
```

**Typical Configurations:**

**For v1-style reports (multiple sources):**
```r
use_haul_data    <- TRUE
use_kisten_data  <- TRUE
use_elog_data    <- TRUE
use_gfw_data     <- TRUE
use_trip_data    <- TRUE
```

**For v2-style reports (PEFA only):**
```r
use_haul_data    <- FALSE
use_kisten_data  <- FALSE
use_elog_data    <- TRUE
use_gfw_data     <- FALSE
use_trip_data    <- FALSE
```

### Step 2: Choose Trip Selection Method

**Option A: Date-based (recommended for flexibility)**
```r
selection_mode   <- "date"

setvessel  <- "SCH135"
startdate  <- lubridate::dmy("01/12/2025")
enddate    <- lubridate::dmy("04/12/2025")
```

**Option B: Manual (for specific trips)**
```r
selection_mode   <- "manual"

setvessel  <- c("Z99", "SCH99")
settrip    <- c("2025264", "2025265")
```

### Step 3: Customize Report Options

```r
# Report options
comparisons      <- FALSE  # Include comparison plots?
target_aspect    <- 1.0    # Map shape (1.0 = square)
buffer_pct       <- 0.1    # Map buffer (10% around data)
```

Then knit the document!

---

## Configuration Options Explained

### Data Source Flags

| Flag | Description | When to Use |
|------|-------------|-------------|
| `use_haul_data` | Self-sampling haul records | When you have manual haul recordings |
| `use_kisten_data` | Marelec scale data | When landing weights recorded by fish box |
| `use_elog_data` | Electronic logbook (PEFA) | Always (primary data source) |
| `use_gfw_data` | Global Fishing Watch tracks | For vessel tracking analysis |
| `use_trip_data` | Trip summary information | For trip-level summaries |

### Report Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `comparisons` | Boolean | `FALSE` | Show comparison plots between data sources |
| `target_aspect` | Numeric | `1.0` | Map aspect ratio (1.0=square, >1=tall, <1=wide) |
| `buffer_pct` | Numeric | `0.1` | Buffer around data for maps (as percentage) |

### Selection Mode

**"date" mode:**
- Automatically finds all trips within date range
- More flexible for recent trips
- Recommended for routine reporting

**"manual" mode:**
- Explicitly specify vessel and trip IDs
- Better for historical analysis
- Allows multiple vessels/trips combination

---

## Common Use Cases

### Use Case 1: Standard Weekly Report (PEFA only)

```r
# Configuration
use_haul_data    <- FALSE
use_kisten_data  <- FALSE
use_elog_data    <- TRUE
use_gfw_data     <- FALSE
use_trip_data    <- FALSE

selection_mode   <- "date"
setvessel        <- "SCH135"
startdate        <- lubridate::dmy("01/12/2025")
enddate          <- lubridate::dmy("07/12/2025")

comparisons      <- FALSE
```

### Use Case 2: Detailed Analysis with All Data Sources

```r
# Configuration
use_haul_data    <- TRUE
use_kisten_data  <- TRUE
use_elog_data    <- TRUE
use_gfw_data     <- TRUE
use_trip_data    <- TRUE

selection_mode   <- "manual"
setvessel        <- "SCH135"
settrip          <- c("2025350", "2025351")

comparisons      <- TRUE  # Show comparisons between sources
```

### Use Case 3: Multi-Vessel Comparison

```r
# Configuration
use_haul_data    <- FALSE
use_kisten_data  <- FALSE
use_elog_data    <- TRUE
use_gfw_data     <- FALSE
use_trip_data    <- FALSE

selection_mode   <- "date"
setvessel        <- c("SCH135", "SCH144", "SCH65")
startdate        <- lubridate::dmy("01/12/2025")
enddate          <- lubridate::dmy("31/12/2025")

comparisons      <- FALSE
```

### Use Case 4: Historical Trip Analysis

```r
# Configuration
use_haul_data    <- TRUE
use_kisten_data  <- TRUE
use_elog_data    <- TRUE
use_gfw_data     <- FALSE
use_trip_data    <- TRUE

selection_mode   <- "manual"
setvessel        <- "Z99"
settrip          <- c("2023377", "2023378", "2023379")

comparisons      <- TRUE
target_aspect    <- 1.4  # Taller maps for North Sea trips
```

---

## Troubleshooting

### Error: "No data found for vessel(s) and trip(s)"

**Causes:**
1. Wrong vessel name or trip ID
2. Date range doesn't include any trips
3. Required data source not loaded

**Solutions:**
- Check vessel spelling: "SCH135" not "SCH-135"
- Verify trips exist in elog with: `elog %>% filter(vessel == "SCH135") %>% distinct(trip)`
- Ensure at least one data source flag is TRUE
- Check date range includes actual fishing dates

### Warning: "No haul-level information available"

**Cause:** Using elog summary data without elog_trek

**Solution:** This is informational only - report will generate with available data

### Maps Look Strange/Distorted

**Cause:** Aspect ratio not appropriate for fishing area

**Solutions:**
- Adjust `target_aspect`: 
  - North Sea (tall area): `target_aspect <- 1.4`
  - English Channel (wide area): `target_aspect <- 0.7`
  - Default (square): `target_aspect <- 1.0`
- Adjust `buffer_pct` if data is too zoomed in/out

### Plots are Too Small/Large

The template uses dynamic sizing based on number of species/panels. If manual adjustment needed:

```r
# In specific chunk options
```{r fig.height=8, fig.width=10}
```

---

## Tips and Best Practices

### 1. Version Control
Keep old reports by saving with date:
```
FLYSHOOT_tripreport_SCH135_2025W48.docx
```

### 2. Testing Configuration
Test with small date range first:
```r
startdate  <- lubridate::dmy("01/12/2025")
enddate    <- lubridate::dmy("01/12/2025")  # Single day
```

### 3. Data Quality Checks
Enable comparisons when you have multiple data sources:
```r
comparisons <- TRUE
```
This helps identify discrepancies between haul records and elog.

### 4. Performance
For large date ranges (>1 month), consider:
- Disabling GFW data: `use_gfw_data <- FALSE`
- Running overnight for monthly/yearly reports

### 5. Custom Species Lists
The template automatically selects top 10 species. To customize:

```r
# After the setup chunk, add:
myspecies <- e %>%
  filter(species %in% c("COD", "PLE", "SOL", "DAB")) %>%
  group_by(species) %>%
  summarise(weight = sum(weight, na.rm = TRUE)) %>%
  left_join(asfis, by = "species")
```

---

## Advanced: Adding Custom Sections

### Adding a New Plot

```r
```{r echo=FALSE, fig.asp=0.8}
fig_nums(
  name = "myplot",
  level = 1,
  display = FALSE,
  caption = "My custom plot"
)

if(use_elog_data && nrow(e) > 0) {
  e %>%
    # Your plot code here
    ggplot(aes(...)) +
    theme_publication() +
    ...
}
```

_`r fig_nums("myplot")`_
```

### Adding Conditional Content

```r
```{r}
if(use_haul_data && use_kisten_data) {
  # Content only shown when both data sources available
  # e.g., comparison analysis
}
```

---

## Key Improvements Over Original Files

### From v1:
✅ Retained multi-source capability
✅ Kept comparison functionality
✅ Maintained comprehensive analysis

### From v2:
✅ Better map extent calculation
✅ Dynamic figure sizing
✅ Cleaner date-based selection
✅ Improved aspect ratio handling

### New Features:
✅ Modular configuration
✅ Conditional data loading
✅ Better error handling
✅ Reusable functions
✅ Flexible trip selection

---

## Getting Help

### Check Data Availability
```r
# In R console before knitting:
load("path/to/elog.RData")
elog %>% filter(vessel == "SCH135") %>% distinct(trip)
elog %>% filter(vessel == "SCH135", year == 2025) %>% summary()
```

### Debug Mode
Set in console before knitting:
```r
options(error = recover)
```

### Contact
For issues with the template, check:
1. This guide for common solutions
2. Original v1/v2 files for reference
3. FLYSHOOT utils.R for function documentation

---

## File Organization

Recommended structure:
```
project/
├── FLYSHOOT_tripreport_harmonized.Rmd  (main template)
├── report_template.dotx                 (Word template)
├── r/
│   └── FLYSHOOT utils.R                 (utility functions)
├── DATA/
│   └── RDATA/                           (spatial data)
└── reports/                             (generated reports)
    ├── 2025/
    │   ├── SCH135_2025W48.docx
    │   └── SCH144_2025W48.docx
    └── archive/
```

---

## Changelog

**Harmonized Version (Dec 2025):**
- Merged v1 and v2 functionality
- Added configuration system
- Improved map calculations
- Added dynamic figure sizing
- Better error handling
- Comprehensive documentation

**v2 (Aug 2023):**
- PEFA-only focus
- Improved map extent
- Dynamic layouts

**v1 (Jan-Feb 2023):**
- Multi-source integration
- Comparison functionality
- Initial implementation
