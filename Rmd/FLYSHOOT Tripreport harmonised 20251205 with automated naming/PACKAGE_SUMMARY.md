# FLYSHOOT Tripreport Harmonization - Complete Package Summary

## 📦 What You Received

This package contains a complete harmonization solution for your FLYSHOOT tripreport RMarkdown files, including automatic rendering with dynamic filenames and organized directory structures.

---

## 📄 Files Included

### 1. Core Analysis & Documentation

| File | Purpose | Read This If... |
|------|---------|-----------------|
| **flyshoot_harmonization_analysis.md** | Comprehensive technical analysis of differences between v1 and v2 | You want to understand what changed and why |
| **FLYSHOOT_harmonization_quickstart.md** | Quick start guide for the harmonized template | You want to configure the RMarkdown template |
| **RENDERING_SYSTEM_README.md** | Complete guide to the dynamic rendering system | You want to use automatic filename generation |

### 2. RMarkdown Template

| File | Purpose | Use This For... |
|------|---------|-----------------|
| **FLYSHOOT_tripreport_harmonized.Rmd** | Unified RMarkdown template combining v1 and v2 | Generating tripreports (manual knit or via scripts) |

### 3. Rendering Scripts

| File | Purpose | Use This For... |
|------|---------|-----------------|
| **render_tripreport.R** | Single vessel report generator | One-off reports with automatic naming |
| **batch_render_tripreports.R** | Multiple vessel batch processor | Weekly reports for all vessels |

---

## 🚀 Quick Start (Choose Your Path)

### Path A: I Just Want to Generate One Report Right Now

1. Open **render_tripreport.R**
2. Change these three lines:
   ```r
   setvessel <- "SCH135"              # Your vessel
   startdate <- dmy("01/12/2025")     # Start date
   enddate   <- dmy("04/12/2025")     # End date
   ```
3. Run: `source("render_tripreport.R")`
4. Done! Your report appears in `reports/2025/W48/FLYSHOOT_SCH135_2025W48_2trips.docx`

### Path B: I Want to Generate Multiple Reports Weekly

1. Open **batch_render_tripreports.R**
2. Edit the vessels list and dates:
   ```r
   vessels <- c("SCH135", "SCH144", "SCH65")
   startdate <- dmy("01/12/2025")
   enddate   <- dmy("07/12/2025")
   ```
3. Run: `source("batch_render_tripreports.R")`
4. All reports generated with automatic naming and organization!

### Path C: I Want to Understand What Changed First

1. Read **flyshoot_harmonization_analysis.md** (comprehensive)
2. Then read **FLYSHOOT_harmonization_quickstart.md** (practical guide)
3. Review the harmonized template **FLYSHOOT_tripreport_harmonized.Rmd**
4. Try rendering with **render_tripreport.R**

---

## 🎯 What Problems Does This Solve?

### Problem 1: Manual Filename Management ❌
**Before:** 
- Knit document → Save As → Type long filename → Organize into folder
- Easy to make mistakes, inconsistent naming

**After:** ✅
```r
source("render_tripreport.R")
# Automatic: reports/2025/W48/FLYSHOOT_SCH135_2025W48_2trips.docx
```

### Problem 2: Two Separate Code Files ❌
**Before:**
- v1 for multi-source data
- v2 for PEFA-only
- Maintain both separately

**After:** ✅
- One template with flags to enable/disable data sources
- Configuration at the top controls everything

### Problem 3: Inconsistent Code Quality ❌
**Before:**
- Different map calculations in v1 vs v2
- Commented-out code blocks
- Hardcoded paths

**After:** ✅
- Best practices from both versions
- Clean, reusable functions
- Flexible configuration

### Problem 4: Manual Trip Selection ❌
**Before:**
- Look up trip IDs manually
- Update hardcoded values
- Error-prone

**After:** ✅
- Automatic trip discovery from date range
- Manual mode still available when needed

---

## 📊 Feature Comparison

| Feature | Old v1 | Old v2 | New Harmonized | Rendering Scripts |
|---------|--------|--------|----------------|-------------------|
| Multi-source data | ✅ | ❌ | ✅ (optional) | ✅ |
| PEFA-only mode | ❌ | ✅ | ✅ (optional) | ✅ |
| Date-based selection | ❌ | ✅ | ✅ | ✅ |
| Manual trip selection | ✅ | ❌ | ✅ | ❌ |
| Dynamic filename | ❌ | ❌ | ✅ (via scripts) | ✅ |
| Auto directory organization | ❌ | ❌ | ✅ (via scripts) | ✅ |
| Batch processing | ❌ | ❌ | ✅ (via scripts) | ✅ |
| Superior map extent | ❌ | ✅ | ✅ | ✅ |
| Dynamic figure sizing | ❌ | ✅ | ✅ | ✅ |
| Reusable functions | ❌ | ❌ | ✅ | ✅ |

---

## 💡 Usage Examples

### Example 1: Weekly Routine Report
```r
# Monday morning routine
source("batch_render_tripreports.R")
# Generates all reports for last week, automatically named and organized
```

### Example 2: Historical Analysis
```r
# In render_tripreport.R
setvessel <- "SCH135"
startdate <- dmy("01/01/2024")  # Whole year
enddate   <- dmy("31/12/2024")
source("render_tripreport.R")
```

### Example 3: Comparison Analysis
```r
# In FLYSHOOT_tripreport_harmonized.Rmd
use_haul_data   <- TRUE
use_kisten_data <- TRUE
use_elog_data   <- TRUE
comparisons     <- TRUE  # Enable comparison plots

# Then render
source("render_tripreport.R")
```

---

## 📁 Recommended Project Structure

```
your_project/
├── FLYSHOOT_tripreport_harmonized.Rmd  ← Main template
├── render_tripreport.R                 ← Single vessel script
├── batch_render_tripreports.R          ← Batch processing script
├── report_template.dotx                ← Word template
├── r/
│   └── FLYSHOOT utils.R                ← Utility functions
├── DATA/
│   └── RDATA/                          ← Spatial data
└── reports/                            ← Auto-generated
    ├── 2024/
    │   ├── W50/
    │   ├── W51/
    │   └── W52/
    └── 2025/
        ├── W01/
        └── W48/
            ├── FLYSHOOT_SCH135_2025W48_2trips.docx
            ├── FLYSHOOT_SCH144_2025W48_1trip.docx
            └── FLYSHOOT_SCH65_2025W48_3trips.docx
```

---

## 🔧 Setup Checklist

### First Time Setup (5 minutes)

- [ ] Copy all files to your project directory
- [ ] Ensure `r/FLYSHOOT utils.R` exists
- [ ] Ensure `report_template.dotx` exists  
- [ ] Test with small date range:
  ```r
  setvessel <- "SCH135"
  startdate <- dmy("01/12/2025")
  enddate   <- dmy("01/12/2025")  # Single day
  source("render_tripreport.R")
  ```
- [ ] Verify report generated successfully
- [ ] Create `reports/` directory if it doesn't exist

### Optional Enhancements

- [ ] Set up Windows Task Scheduler for weekly automation
- [ ] Configure email notifications
- [ ] Customize filename format (see RENDERING_SYSTEM_README.md)
- [ ] Add vessels to batch processing list

---

## 📚 Documentation Reading Order

1. **Start Here:** This file (you are here!)
2. **For Rendering:** RENDERING_SYSTEM_README.md
3. **For Template Config:** FLYSHOOT_harmonization_quickstart.md
4. **For Technical Details:** flyshoot_harmonization_analysis.md

---

## 🆘 Troubleshooting

### "I get an error when running the script"

**Check:**
1. Do you have all required R packages?
2. Does `FLYSHOOT_tripreport_harmonized.Rmd` exist in the same directory?
3. Can you access your OneDrive data location?
4. Are there trips for the vessel in your date range?

**See:** RENDERING_SYSTEM_README.md → Troubleshooting section

### "The filename or directory structure isn't what I want"

**See:** RENDERING_SYSTEM_README.md → Customization section

### "I want to use the old manual method"

**Do:** Just knit `FLYSHOOT_tripreport_harmonized.Rmd` normally
- Set `selection_mode <- "manual"` in the template
- Configure vessel and trip IDs directly in the RMarkdown

### "I don't understand what changed between v1 and v2"

**Read:** flyshoot_harmonization_analysis.md
- Complete technical analysis
- Side-by-side comparisons
- Rationale for each change

---

## ✨ Key Benefits Summary

1. **Time Savings**: No more manual filename creation and organization
2. **Consistency**: Standardized naming across all reports
3. **Organization**: Automatic chronological filing
4. **Flexibility**: One template, multiple modes
5. **Quality**: Best practices from both versions
6. **Automation**: Ready for scheduled tasks
7. **Error Reduction**: Automatic trip discovery, no manual lookup

---

## 🎓 Learning Path

### Beginner
1. Use the rendering scripts as-is
2. Just change vessel name and dates
3. Generate reports with automatic naming

### Intermediate
1. Read FLYSHOOT_harmonization_quickstart.md
2. Configure data sources in template
3. Customize report options

### Advanced
1. Read flyshoot_harmonization_analysis.md
2. Modify filename formats
3. Customize directory structures
4. Set up automation

---

## 📋 Next Steps

### Immediate (Today)
1. ✅ Place all files in your project directory
2. ✅ Test with one vessel using `render_tripreport.R`
3. ✅ Verify output in `reports/` directory

### This Week
1. ⬜ Try batch processing with `batch_render_tripreports.R`
2. ⬜ Review harmonization analysis
3. ⬜ Customize configurations to your needs

### This Month
1. ⬜ Set up weekly automation (if desired)
2. ⬜ Archive old reports using new system
3. ⬜ Train team members on new workflow

---

## 🤝 Support

If you run into issues:
1. Check the relevant README file
2. Review the troubleshooting sections
3. Test with a minimal example (single day, single vessel)
4. Return with specific error messages for help

---

## 📊 Migration from Old System

### If You're Using v1 or v2 Currently

**Step 1:** Backup your existing files
```r
# Keep old versions
v1_archive/FLYSHOOT_tripreport_v1.Rmd
v2_archive/FLYSHOOT_tripreport_v2.Rmd
```

**Step 2:** Start using harmonized template
- All v1 features still available (set flags to TRUE)
- All v2 features still available (superior code quality maintained)
- New rendering scripts optional but recommended

**Step 3:** Transition period
- Can use both old and new systems simultaneously
- Test new system with recent trips
- Compare outputs for validation

**Step 4:** Full migration
- Once confident, use only harmonized version
- Archive old templates
- Enjoy streamlined workflow!

---

## 🎉 You're All Set!

You now have:
- ✅ A unified, well-documented RMarkdown template
- ✅ Automatic rendering with smart naming
- ✅ Organized directory structure
- ✅ Comprehensive documentation
- ✅ Batch processing capability
- ✅ Flexible configuration options

**Start simple, expand as needed!**

Most users find the rendering scripts immediately useful and gradually explore the advanced configuration options as their needs evolve.

---

**Last Updated:** December 2025
**Package Version:** Harmonized v1.0
