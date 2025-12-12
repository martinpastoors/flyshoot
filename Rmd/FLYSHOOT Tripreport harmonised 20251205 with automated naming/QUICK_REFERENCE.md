# FLYSHOOT Tripreport - Quick Reference Card

## 🚀 Most Common Tasks

### Generate Single Report
```r
# 1. Open: render_tripreport.R
# 2. Edit these 3 lines:
setvessel <- "SCH135"
startdate <- dmy("01/12/2025")
enddate   <- dmy("04/12/2025")

# 3. Run:
source("render_tripreport.R")

# Output: reports/2025/W48/FLYSHOOT_SCH135_2025W48_2trips.docx
```

### Generate Multiple Reports
```r
# 1. Open: batch_render_tripreports.R
# 2. Edit:
vessels <- c("SCH135", "SCH144", "SCH65")
startdate <- dmy("01/12/2025")
enddate   <- dmy("07/12/2025")

# 3. Run:
source("batch_render_tripreports.R")
```

### Process Last Week Automatically
```r
# In batch_render_tripreports.R:
enddate <- Sys.Date()
startdate <- enddate - days(7)
```

---

## 📁 What Gets Generated

### Filename Format
`FLYSHOOT_{VESSEL}_{YEAR}W{WEEK}_{N}trip[s].docx`

**Examples:**
- `FLYSHOOT_SCH135_2025W48_1trip.docx`
- `FLYSHOOT_SCH144_2025W48_3trips.docx`

### Directory Structure
```
reports/
└── 2025/
    └── W48/
        ├── FLYSHOOT_SCH135_2025W48_2trips.docx
        └── FLYSHOOT_SCH144_2025W48_1trip.docx
```

---

## ⚙️ RMarkdown Template Configuration

In `FLYSHOOT_tripreport_harmonized.Rmd`:

### Enable/Disable Data Sources
```r
use_haul_data    <- TRUE   # Self-sampling
use_kisten_data  <- TRUE   # Marelec/scale
use_elog_data    <- TRUE   # PEFA (always TRUE)
use_gfw_data     <- FALSE  # Tracking
use_trip_data    <- TRUE   # Trip summaries
```

### Report Options
```r
comparisons   <- FALSE  # Show comparison plots?
target_aspect <- 1.0    # Map shape (1.0 = square)
buffer_pct    <- 0.1    # Map buffer (10%)
```

### Manual Mode (when not using rendering scripts)
```r
selection_mode <- "manual"  # or "date"

# Manual:
setvessel <- c("Z99", "SCH99")
settrip <- c("2025264")

# Date:
setvessel <- "SCH135"
startdate <- dmy("01/12/2025")
enddate <- dmy("04/12/2025")
```

---

## 🔍 Quick Checks

### Check Available Vessels
```r
load("path/to/elog.RData")
elog %>% distinct(vessel) %>% pull()
```

### Check Trips for Vessel
```r
elog %>%
  filter(vessel == "SCH135", year == 2025) %>%
  distinct(trip, catchdate) %>%
  arrange(catchdate)
```

### Check Date Range
```r
elog %>%
  filter(vessel == "SCH135") %>%
  summarise(
    first = min(catchdate),
    last = max(catchdate)
  )
```

---

## 🆘 Quick Fixes

### "No trips found"
- Check vessel spelling: `"SCH135"` not `"SCH-135"`
- Verify date range includes trips
- Run checks above

### "Error rendering"
- File exists? `file.exists("FLYSHOOT_tripreport_harmonized.Rmd")`
- Packages installed? See full docs
- Try manual knit in RStudio first

### "Wrong output location"
```r
# In rendering script:
output_base_dir <- "your/path/here"
```

---

## 📋 File Checklist

Required files in project directory:
- [ ] `FLYSHOOT_tripreport_harmonized.Rmd`
- [ ] `render_tripreport.R`
- [ ] `batch_render_tripreports.R`
- [ ] `r/FLYSHOOT utils.R`
- [ ] `report_template.dotx`

---

## 🎯 Typical Workflows

### Weekly Routine
```r
# Monday morning:
source("batch_render_tripreports.R")
# Done! All vessels processed.
```

### Specific Vessel Deep-Dive
```r
# In FLYSHOOT_tripreport_harmonized.Rmd:
use_haul_data <- TRUE
use_kisten_data <- TRUE
use_elog_data <- TRUE
comparisons <- TRUE

# Then:
source("render_tripreport.R")
```

### Monthly Summary
```r
# In render_tripreport.R:
startdate <- dmy("01/12/2025")
enddate <- dmy("31/12/2025")
source("render_tripreport.R")
```

---

## 📚 Where to Find More Info

| Topic | Document |
|-------|----------|
| Getting started | PACKAGE_SUMMARY.md |
| Rendering system | RENDERING_SYSTEM_README.md |
| Template configuration | FLYSHOOT_harmonization_quickstart.md |
| Technical details | flyshoot_harmonization_analysis.md |

---

## 💡 Pro Tips

1. **Test first**: Use single day for testing
   ```r
   startdate <- dmy("01/12/2025")
   enddate <- dmy("01/12/2025")
   ```

2. **Quiet mode**: Turn on for batch processing
   ```r
   quiet_mode <- TRUE
   ```

3. **Skip auto-open**: When processing many vessels
   ```r
   open_reports <- FALSE
   ```

4. **Check logs**: Review batch processing summary

5. **Version control**: Git commit before major changes

---

## 🎓 Learning Curve

**Beginner:** Just edit vessel/dates in scripts ⭐
**Intermediate:** Configure data sources in template ⭐⭐
**Advanced:** Customize filenames/structure ⭐⭐⭐

Start simple! Most users only need the rendering scripts.

---

**Questions? See the full documentation files!**

**Created:** December 2025 | **Package:** Harmonized v1.0
