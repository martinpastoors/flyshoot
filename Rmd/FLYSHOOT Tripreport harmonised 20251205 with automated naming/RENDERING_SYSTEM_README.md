# FLYSHOOT Tripreport - Dynamic Rendering System

## Overview

This system automatically generates FLYSHOOT tripreports with dynamic filenames and organized directory structures based on vessel, year, and week information.

**Key Features:**
- ✅ Automatic filename generation (e.g., `FLYSHOOT_SCH135_2025W48_2trips.docx`)
- ✅ Organized directory structure (`reports/2025/W48/`)
- ✅ Single vessel or batch processing
- ✅ Automatic trip discovery from date ranges
- ✅ Error handling and summary statistics
- ✅ No manual renaming or organizing required!

## Quick Start

### Option 1: Single Vessel Report

1. Open `render_tripreport.R`
2. Edit the configuration section:
```r
setvessel <- "SCH135"
startdate <- dmy("01/12/2025")
enddate   <- dmy("04/12/2025")
```
3. Run the script: `source("render_tripreport.R")`
4. Your report appears in: `reports/2025/W48/FLYSHOOT_SCH135_2025W48_2trips.docx`

### Option 2: Batch Process Multiple Vessels

1. Open `batch_render_tripreports.R`
2. Edit the vessels list:
```r
vessels <- c("SCH135", "SCH144", "SCH65", "SL9")
startdate <- dmy("01/12/2025")
enddate   <- dmy("07/12/2025")
```
3. Run the script: `source("batch_render_tripreports.R")`
4. All reports generated automatically!

## Files Included

| File | Purpose |
|------|---------|
| `FLYSHOOT_tripreport_harmonized.Rmd` | Main RMarkdown template (updated to accept parameters) |
| `render_tripreport.R` | Render single vessel report |
| `batch_render_tripreports.R` | Render multiple vessel reports |
| `flyshoot_harmonization_analysis.md` | Detailed harmonization analysis |
| `FLYSHOOT_harmonization_quickstart.md` | Quick start guide |

## How It Works

### 1. Filename Generation

The system automatically creates descriptive filenames:

**Format:** `FLYSHOOT_{VESSEL}_{YEAR}W{WEEK}_{N}trip[s].docx`

**Examples:**
- `FLYSHOOT_SCH135_2025W48_1trip.docx` (single trip)
- `FLYSHOOT_SCH144_2025W49_3trips.docx` (multiple trips)
- `FLYSHOOT_Z99_2024W52_2trips.docx` (historical)

### 2. Directory Structure

Reports are organized by year and week:

```
reports/
├── 2024/
│   ├── W50/
│   │   └── FLYSHOOT_SCH135_2024W50_2trips.docx
│   ├── W51/
│   │   ├── FLYSHOOT_SCH135_2024W51_1trip.docx
│   │   └── FLYSHOOT_SCH144_2024W51_2trips.docx
│   └── W52/
│       └── FLYSHOOT_SL9_2024W52_3trips.docx
└── 2025/
    ├── W01/
    └── W48/
        ├── FLYSHOOT_SCH135_2025W48_2trips.docx
        └── FLYSHOOT_SCH65_2025W48_1trip.docx
```

**Benefits:**
- Easy to find reports by time period
- Natural chronological organization
- Prevents file naming conflicts
- Clean, professional structure

### 3. Automatic Trip Discovery

The system automatically finds trips within your date range:

```r
# You specify:
setvessel <- "SCH135"
startdate <- dmy("01/12/2025")
enddate   <- dmy("07/12/2025")

# System finds:
# - Trip IDs: 2025264, 2025265
# - Number of trips: 2
# - Week: 48
# - Generates: FLYSHOOT_SCH135_2025W48_2trips.docx
```

## Detailed Usage

### Single Vessel Rendering (`render_tripreport.R`)

**Configuration Options:**

```r
# Basic configuration
setvessel <- "SCH135"            # Vessel name
startdate <- dmy("01/12/2025")   # Start of period
enddate   <- dmy("04/12/2025")   # End of period

# Optional: Change output base directory
output_base_dir <- "reports"     # Default

# Optional: Use different RMarkdown file
rmd_file <- "FLYSHOOT_tripreport_harmonized.Rmd"
```

**What Happens:**

1. Script loads elog data
2. Finds all trips for vessel in date range
3. Determines year, week, number of trips
4. Creates filename: `FLYSHOOT_SCH135_2025W48_2trips.docx`
5. Creates directory: `reports/2025/W48/`
6. Renders report with parameters
7. Opens report automatically (Windows)
8. Displays summary statistics

**Output Example:**

```
==========================================
RENDER SUMMARY
==========================================
Vessel:       SCH135
Date range:   01/12/2025 to 04/12/2025
Trips found:  2
Output file:  FLYSHOOT_SCH135_2025W48_2trips.docx
Output path:  reports/2025/W48/FLYSHOOT_SCH135_2025W48_2trips.docx
==========================================
```

### Batch Processing (`batch_render_tripreports.R`)

**Configuration Options:**

```r
# Vessel list
vessels <- c("SCH135", "SCH144", "SCH65", "SL9")

# Date range (applied to all vessels)
startdate <- dmy("01/12/2025")
enddate   <- dmy("07/12/2025")

# Processing options
open_reports <- FALSE   # Open each report after rendering?
quiet_mode <- TRUE      # Suppress rendering messages?
```

**Advanced: Process Last Week Automatically**

```r
# Automatically process last week
enddate <- Sys.Date()
startdate <- enddate - days(7)

# Or last month
startdate <- floor_date(Sys.Date() - months(1), "month")
enddate <- ceiling_date(startdate, "month") - days(1)
```

**What Happens:**

1. Loads elog data once
2. For each vessel:
   - Finds trips in date range
   - Skips if no data found
   - Renders report with error handling
   - Tracks statistics
3. Displays comprehensive summary

**Output Example:**

```
==========================================
BATCH PROCESSING SUMMARY
==========================================

Total vessels processed: 4
  ✓ Successful:          3
  ⊘ Skipped (no data):   1
  ✗ Errors:              0

Total time:              45.2s
Average render time:     15.1s
Total output size:       2456 KB

------------------------------------------
DETAILED RESULTS
------------------------------------------

vessel   status    path
-------  --------  ---------------------------------------
SCH135   success   FLYSHOOT_SCH135_2025W48_2trips.docx
SCH144   success   FLYSHOOT_SCH144_2025W48_1trip.docx
SCH65    skipped   -
SL9      success   FLYSHOOT_SL9_2025W49_3trips.docx
```

## Advanced Usage

### Custom Output Location

**For single vessel:**
```r
# In render_tripreport.R
output_base_dir <- "C:/MyReports/Flyshoot"
```

**For batch:**
```r
# In batch_render_tripreports.R
output_base_dir <- "//server/shared/Flyshoot_Reports"
```

### Process Specific Week for All Vessels

```r
# Get week number
target_week <- 48
target_year <- 2025

# Calculate date range for that week
startdate <- lubridate::ymd(sprintf("%d-01-01", target_year)) + 
             weeks(target_week - 1)
enddate <- startdate + days(6)

# Run batch processing
vessels <- c("SCH135", "SCH144", "SCH65", "SL9")
```

### Generate Monthly Summary for One Vessel

```r
# In render_tripreport.R
setvessel <- "SCH135"
startdate <- dmy("01/12/2025")
enddate   <- dmy("31/12/2025")  # Whole month

# Filename will be: FLYSHOOT_SCH135_2025W48_8trips.docx
# (assuming 8 trips across the month, week of first trip)
```

### Automated Weekly Reports (Scheduled Task)

Create a Windows Task Scheduler job or cron job:

**Windows batch file (`run_weekly_reports.bat`):**
```batch
@echo off
cd C:\path\to\project
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" batch_render_tripreports.R
```

Schedule to run every Monday morning at 8 AM.

### Include in Workflow

**Example integration:**
```r
# Your existing data processing script
source("process_trip_data.R")
source("validate_elog.R")

# Generate reports automatically after processing
source("batch_render_tripreports.R")

# Send email notification
send_email(
  to = "fleet@company.com",
  subject = "Weekly Tripreports Generated",
  body = paste("Reports generated:", 
               list.files("reports", pattern = "*.docx$", recursive = TRUE))
)
```

## Troubleshooting

### Issue: "No trips found for vessel"

**Possible causes:**
1. Wrong vessel name
2. Date range doesn't include any trips
3. Data not loaded correctly

**Solutions:**
```r
# Check available vessels
elog %>% distinct(vessel) %>% pull()

# Check trips for your vessel
elog %>% 
  filter(vessel == "SCH135") %>% 
  distinct(trip, catchdate) %>% 
  arrange(catchdate)

# Verify date range
elog %>% 
  filter(vessel == "SCH135", 
         year == 2025) %>% 
  summarise(min = min(catchdate), max = max(catchdate))
```

### Issue: "Error rendering report"

**Possible causes:**
1. RMarkdown file not found
2. Required packages not installed
3. Data files missing
4. Syntax error in RMarkdown

**Solutions:**
```r
# Check RMarkdown file exists
file.exists("FLYSHOOT_tripreport_harmonized.Rmd")

# Check required packages
required <- c("rmarkdown", "tidyverse", "lubridate", 
              "reshape2", "RColorBrewer", "viridis", 
              "pander", "captioner", "patchwork", "icosa")
installed <- required %in% installed.packages()[,"Package"]
if(!all(installed)) install.packages(required[!installed])

# Try rendering manually to see error
rmarkdown::render("FLYSHOOT_tripreport_harmonized.Rmd")
```

### Issue: Directory permission errors

**Solution:**
```r
# Check write permissions
output_test <- "reports/test"
if(!dir.exists(output_test)) {
  dir.create(output_test, recursive = TRUE)
}

# Or specify different output location
output_base_dir <- "C:/Users/YourName/Documents/Reports"
```

### Issue: Reports not opening automatically (Windows)

**Cause:** Shell execution disabled or non-Windows platform

**Solutions:**
```r
# Manually open the report
shell.exec("reports/2025/W48/FLYSHOOT_SCH135_2025W48_2trips.docx")

# Or navigate to folder
shell.exec(dirname(output_path))

# Or disable auto-open
# (Remove the shell.exec line from the script)
```

## Customization

### Change Filename Format

Edit in `render_tripreport.R` or `batch_render_tripreports.R`:

```r
# Current format
output_filename <- sprintf(
  "FLYSHOOT_%s_%dW%02d_%dtrip%s.docx",
  setvessel, trip_info$year, trip_info$week, 
  trip_info$ntrips, ifelse(trip_info$ntrips > 1, "s", "")
)

# Alternative: Include date range
output_filename <- sprintf(
  "FLYSHOOT_%s_%s_to_%s.docx",
  setvessel,
  format(trip_info$startdate, '%Y%m%d'),
  format(trip_info$enddate, '%Y%m%d')
)

# Alternative: Include month
output_filename <- sprintf(
  "FLYSHOOT_%s_%d-%02d_%dtrips.docx",
  setvessel,
  trip_info$year,
  trip_info$month,
  trip_info$ntrips
)
```

### Change Directory Structure

```r
# Current structure: reports/YYYY/WWW/
output_dir <- file.path(
  output_base_dir,
  as.character(trip_info$year),
  sprintf("W%02d", trip_info$week)
)

# Alternative: By year/month
output_dir <- file.path(
  output_base_dir,
  as.character(trip_info$year),
  sprintf("%02d", trip_info$month)
)

# Alternative: By vessel/year
output_dir <- file.path(
  output_base_dir,
  setvessel,
  as.character(trip_info$year)
)

# Alternative: Flat structure with subfolders only for year
output_dir <- file.path(
  output_base_dir,
  as.character(trip_info$year)
)
```

### Add Email Notification

Add to end of `batch_render_tripreports.R`:

```r
# Using mailR package
library(mailR)

if(n_success > 0) {
  
  report_list <- results_df %>%
    filter(status == "success") %>%
    pull(path) %>%
    paste(collapse = "\n  ")
  
  send.mail(
    from = "reports@yourcompany.com",
    to = "fleet@yourcompany.com",
    subject = sprintf("Weekly Tripreports - Week %d", trip_info$week),
    body = sprintf(
      "Generated %d reports:\n\n  %s\n\nTotal time: %ss",
      n_success, report_list, overall_elapsed
    ),
    smtp = list(host.name = "smtp.yourcompany.com", 
                port = 587),
    authenticate = TRUE,
    send = TRUE
  )
}
```

## Best Practices

### 1. Version Control
Keep scripts in git but exclude generated reports:

**.gitignore:**
```
reports/
*.docx
```

### 2. Backup Strategy
- Keep source RMarkdown files in version control
- Archive old reports monthly:
```r
# Archive script
old_reports <- list.files("reports", pattern = "*.docx$", 
                         recursive = TRUE, full.names = TRUE)
old_reports <- old_reports[file.mtime(old_reports) < Sys.Date() - months(3)]
file.copy(old_reports, "archive/")
```

### 3. Testing
Always test with small date range first:
```r
startdate <- dmy("01/12/2025")
enddate   <- dmy("01/12/2025")  # Single day
```

### 4. Documentation
Keep a log of generated reports:
```r
# Add to end of batch script
log_entry <- data.frame(
  timestamp = Sys.time(),
  date_range = sprintf("%s to %s", startdate, enddate),
  vessels = paste(vessels, collapse = ", "),
  success = n_success,
  skipped = n_skipped,
  errors = n_errors
)
write_csv(log_entry, "reports_log.csv", append = TRUE)
```

## System Requirements

- R version 4.0 or higher
- Required R packages (automatically checked):
  - rmarkdown
  - tidyverse
  - lubridate
  - reshape2, RColorBrewer, viridis
  - pander, captioner, patchwork
  - icosa
- MS Word or compatible software (for .docx output)
- Access to OneDrive data location

## Support

### Quick Reference

| Task | Script | Key Setting |
|------|--------|-------------|
| Single vessel report | `render_tripreport.R` | `setvessel`, `startdate`, `enddate` |
| Multiple vessels | `batch_render_tripreports.R` | `vessels`, `startdate`, `enddate` |
| Change output location | Either script | `output_base_dir` |
| Automated weekly | Schedule `batch_render_tripreports.R` | Windows Task Scheduler |

### Common Workflows

**Weekly routine reports:**
1. Edit `batch_render_tripreports.R`
2. Set date range to last week
3. Run script
4. Reports auto-organized by year/week

**Historical analysis:**
1. Edit `render_tripreport.R`
2. Set specific date range
3. Use `selection_mode <- "manual"` if needed
4. Run script

**Quality control:**
1. Set `comparisons <- TRUE` in RMarkdown
2. Run with all data sources enabled
3. Review comparison plots

## Updates and Maintenance

### Updating the Template

When updating `FLYSHOOT_tripreport_harmonized.Rmd`:
1. Keep the parameter section in YAML header
2. Keep the params check in configuration section
3. Test with rendering script before deploying

### Adding New Vessels

Simply add to vessel list:
```r
vessels <- c("SCH135", "SCH144", "SCH65", "SL9", "NEWVESSEL")
```

No other changes needed!

---

**Questions or Issues?**
Refer to:
- `FLYSHOOT_harmonization_quickstart.md` for RMarkdown configuration
- `flyshoot_harmonization_analysis.md` for technical details
- This README for rendering system usage
