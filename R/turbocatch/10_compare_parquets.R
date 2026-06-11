# =============================================================================
# 10_compare_parquets.R
# Compare turbocatch parquets with the existing flyshoot pipeline parquets.
# Checks: table presence, column names, column types, value ranges, species,
#         vessels, date coverage and catch totals.
#
# Usage:
#   source("10_compare_parquets.R")
#   compare_parquets()
# =============================================================================

library(arrow)
library(dplyr)
library(glue)
library(lubridate)
library(purrr)

TURBOCATCH_DIR <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch"
PIPELINE_DIR   <- "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/data/raw"

compare_parquets <- function(
    tc_dir       = TURBOCATCH_DIR,
    pipeline_dir = PIPELINE_DIR,
    tables       = c("haul", "elog", "elog_trek", "trip")
) {
  message(strrep("=", 65))
  message("TURBOCATCH vs PIPELINE PARQUET COMPARISON")
  message(strrep("=", 65))
  message(glue("  Turbocatch : {tc_dir}"))
  message(glue("  Pipeline   : {pipeline_dir}"))

  for (tbl in tables) {
    message(glue("\n{strrep('-', 65)}"))
    message(glue("TABLE: {tbl}"))
    message(strrep("-", 65))

    tc_path <- file.path(tc_dir,       paste0(tbl, ".parquet"))
    # Pipeline stores parquets in subfolders: raw/elog/elog.parquet
    pl_path <- file.path(pipeline_dir, tbl, paste0(tbl, ".parquet"))

    tc_exists <- file.exists(tc_path)
    pl_exists <- file.exists(pl_path)

    if (!tc_exists) { message(glue("  ✗ turbocatch/{tbl}.parquet NOT FOUND")); next }
    if (!pl_exists) { message(glue("  ✗ pipeline/{tbl}.parquet   NOT FOUND — skipping compare")); next }

    tc <- read_parquet(tc_path)
    pl <- read_parquet(pl_path)

    # ── 1. Row counts ────────────────────────────────────────────────────────
    message(glue("\n  Rows — turbocatch: {nrow(tc)}  |  pipeline: {nrow(pl)}"))

    # ── 2. Column comparison ─────────────────────────────────────────────────
    tc_cols <- names(tc)
    pl_cols <- names(pl)
    in_both   <- intersect(tc_cols, pl_cols)
    tc_only   <- setdiff(tc_cols, pl_cols)
    pl_only   <- setdiff(pl_cols, tc_cols)

    message(glue("\n  Columns in both ({length(in_both)}): ",
                 "{paste(in_both, collapse=', ')}"))
    if (length(tc_only) > 0)
      message(glue("  Turbocatch-only  ({length(tc_only)}): ",
                   "{paste(tc_only, collapse=', ')}"))
    if (length(pl_only) > 0)
      message(glue("  Pipeline-only    ({length(pl_only)}): ",
                   "{paste(pl_only, collapse=', ')}"))

    # ── 3. Type mismatches on shared columns ─────────────────────────────────
    type_issues <- map_dfr(in_both, function(col) {
      tc_type <- class(tc[[col]])[1]
      pl_type <- class(pl[[col]])[1]
      if (tc_type != pl_type)
        tibble(column = col, turbocatch = tc_type, pipeline = pl_type)
      else
        tibble()
    })
    if (nrow(type_issues) > 0) {
      message("\n  ⚠ Type mismatches:")
      print(type_issues, n = Inf)
    } else {
      message("  ✓ All shared columns have matching types")
    }

    # ── 4. Vessels ───────────────────────────────────────────────────────────
    if ("vessel" %in% in_both) {
      tc_vessels <- sort(unique(tc$vessel))
      pl_vessels <- sort(unique(pl$vessel))
      overlap    <- intersect(tc_vessels, pl_vessels)
      message(glue("\n  Vessels in turbocatch : {paste(tc_vessels, collapse=', ')}"))
      message(glue("  Vessels in pipeline   : {paste(head(pl_vessels, 10), collapse=', ')}",
                   "{if(length(pl_vessels)>10) '...' else ''}"))
      message(glue("  Overlap               : {paste(overlap, collapse=', ')}",
                   "{if(length(overlap)==0) ' (none)' else ''}"))
    }

    # ── 5. Date coverage ─────────────────────────────────────────────────────
    date_col <- dplyr::case_when(
      "date"           %in% in_both ~ "date",
      "departure_date" %in% in_both ~ "departure_date",
      "weighing_time"  %in% in_both ~ "weighing_time",
      TRUE                          ~ NA_character_
    )
    if (!is.na(date_col)) {
      tc_dates <- as.Date(tc[[date_col]])
      pl_dates <- as.Date(pl[[date_col]])
      message(glue("\n  Date range turbocatch : {min(tc_dates,na.rm=T)} – {max(tc_dates,na.rm=T)}"))
      message(glue("  Date range pipeline   : {min(pl_dates,na.rm=T)} – {max(pl_dates,na.rm=T)}"))
    }

    # ── 6. Table-specific checks ─────────────────────────────────────────────
    if (tbl %in% c("elog", "elog_trek") && "species_code" %in% in_both) {
      tc_spp <- sort(unique(tc$species_code))
      pl_spp <- sort(unique(pl$species_code))
      tc_only_spp <- setdiff(tc_spp, pl_spp)
      pl_only_spp <- setdiff(pl_spp, tc_spp)

      message(glue("\n  Species — turbocatch: {length(tc_spp)}  |  pipeline: {length(pl_spp)}"))
      if (length(tc_only_spp) > 0)
        message(glue("  In turbocatch only: {paste(tc_only_spp, collapse=', ')}"))
      if (length(pl_only_spp) > 0)
        message(glue("  In pipeline only  : {paste(pl_only_spp, collapse=', ')}"))

      if ("weight_kg" %in% in_both) {
        message(glue("\n  Total catch — turbocatch: ",
                     "{round(sum(tc$weight_kg, na.rm=T)/1000, 1)} t  |  ",
                     "pipeline: {round(sum(pl$weight_kg, na.rm=T)/1000, 1)} t"))

        # Top 10 species by weight — turbocatch
        message("\n  Top 10 species by weight (turbocatch):")
        tc %>%
          group_by(species_code) %>%
          summarise(kg = sum(weight_kg, na.rm=T), .groups="drop") %>%
          arrange(desc(kg)) %>% slice_head(n=10) %>%
          mutate(t = round(kg/1000,1)) %>% select(species_code, t) %>%
          as.data.frame() %>% print(row.names=FALSE)

        # Top 10 species by weight — pipeline (TC vessels only if overlap exists)
        if ("vessel" %in% in_both && length(overlap) > 0) {
          message(glue("  Top 10 species by weight (pipeline, vessels {paste(overlap,collapse='+')}):"))
          pl %>%
            filter(vessel %in% overlap) %>%
            group_by(species_code) %>%
            summarise(kg = sum(weight_kg, na.rm=T), .groups="drop") %>%
            arrange(desc(kg)) %>% slice_head(n=10) %>%
            mutate(t = round(kg/1000,1)) %>% select(species_code, t) %>%
            as.data.frame() %>% print(row.names=FALSE)
        }
      }
    }

    if (tbl == "haul" && "n_shots" %in% tc_cols) {
      message(glue("\n  n_shots (turbocatch) — mean: ",
                   "{round(mean(tc$n_shots, na.rm=T),1)}  |  ",
                   "max: {max(tc$n_shots, na.rm=T)}"))
      if ("fishing_time_hours" %in% in_both) {
        message(glue("  fishing_time_hours — TC mean: ",
                     "{round(mean(tc$fishing_time_hours, na.rm=T),1)}  |  ",
                     "PL mean: {round(mean(pl$fishing_time_hours, na.rm=T),1)}"))
      }
    }

    if (tbl == "trip") {
      if ("far_regime" %in% tc_cols) {
        message("\n  FAR regime (turbocatch):")
        print(as.data.frame(table(tc$far_regime, useNA="ifany")), row.names=FALSE)
      }
      if ("trip_type" %in% tc_cols) {
        message("\n  Trip type (turbocatch):")
        print(as.data.frame(table(tc$trip_type, useNA="ifany")), row.names=FALSE)
      }
    }

    # ── 7. Missing values in key columns ─────────────────────────────────────
    key_cols <- intersect(in_both,
                          c("vessel","trip_id","date","species_code","weight_kg",
                            "haul_id","shoot_lat","shoot_lon","ices_rect","gear_type"))
    if (length(key_cols) > 0) {
      na_summary <- map_dfr(key_cols, function(col) {
        n_na <- sum(is.na(tc[[col]]))
        if (n_na > 0) tibble(column=col, n_na=n_na,
                             pct=round(100*n_na/nrow(tc),1))
        else tibble()
      })
      if (nrow(na_summary) > 0) {
        message("\n  ⚠ Missing values in turbocatch key columns:")
        print(na_summary, n=Inf)
      } else {
        message("  ✓ No missing values in key columns")
      }
    }
  }

  message(glue("\n{strrep('=', 65)}"))
  message("COMPARISON COMPLETE")
  message(strrep("=", 65))
  invisible(NULL)
}

# =============================================================================
# USAGE
# =============================================================================
# source("10_compare_parquets.R")
compare_parquets()
#
# # Compare specific tables only
# compare_parquets(tables = c("elog", "trip"))
# =============================================================================
