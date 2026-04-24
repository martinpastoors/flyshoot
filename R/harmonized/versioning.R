# ============================================================================
# versioning.R
#
# Simple version tracking for FLYSHOOT pipeline files.
# 
# CONCEPT:
#   Each key file has a VERSION comment at the top (e.g. # VERSION: 2026.03.25.1)
#   This script checks whether files in a staging folder (e.g. Downloads) are
#   newer than the GIT versions, and copies them automatically if so.
#
# USAGE:
#   1. When you receive updated files, put them in config$staging_path
#      (default: a "staging" subfolder in the project)
#   2. Source this script — it compares versions and copies newer files
#   3. Optionally add source("R/harmonized/versioning.R") at the top of
#      03_main_workflow.R and batch_render_tripreports.R so it runs automatically
#
# VERSION FORMAT: YYYY.MM.DD.N  (N = revision number within that day, starts at 1)
# ============================================================================

library(glue)

project_root <- Sys.getenv("FLYSHOOT_PROJECT")
if (project_root == "") stop("Set FLYSHOOT_PROJECT in .Renviron")
p <- function(...) file.path(project_root, ...)

# ── Files to track ────────────────────────────────────────────────────────────
tracked_files <- c(
  "R/harmonized/01_flyshoot_functions.R",
  "R/harmonized/02_storage_functions.R",
  "R/harmonized/03_main_workflow.R",
  "R/harmonized/FLYSHOOT_tripreport_harmonized.Rmd",
  "R/harmonized/batch_render_tripreports.R",
  "R/harmonized/render_tripreport.R",
  "R/harmonized/flyshoot_places.csv"
)

# Staging folder — put new files here before running this script
staging_path <- p("R", "harmonized", "_staging")

# ── Helper: extract VERSION from file ─────────────────────────────────────────
get_version <- function(file_path) {
  if (!file.exists(file_path)) return(NA_character_)
  lines <- tryCatch(readLines(file_path, n = 10, warn = FALSE),
                    error = function(e) character(0))
  ver_line <- grep("^#\\s*VERSION:", lines, value = TRUE)[1]
  if (is.na(ver_line)) return(NA_character_)
  trimws(sub("^#\\s*VERSION:\\s*", "", ver_line))
}

version_to_num <- function(v) {
  if (is.na(v)) return(0)
  parts <- as.integer(strsplit(v, "\\.")[[1]])
  if (length(parts) == 4)
    parts[1]*10000000 + parts[2]*100000 + parts[3]*1000 + parts[4]
  else if (length(parts) == 3)
    parts[1]*10000000 + parts[2]*100000 + parts[3]*1000
  else 0
}

# ── Check and update ──────────────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("FLYSHOOT VERSION CHECK\n")
cat(strrep("=", 60), "\n\n")

if (!dir.exists(staging_path)) {
  cat(glue("Staging folder not found: {staging_path}\n"))
  cat("Create it and place updated files there to enable auto-update.\n\n")
} else {
  updated <- 0
  for (rel_path in tracked_files) {
    filename    <- basename(rel_path)
    git_path    <- p(rel_path)
    stage_path  <- file.path(staging_path, filename)

    if (!file.exists(stage_path)) next  # nothing staged for this file

    v_git   <- get_version(git_path)
    v_stage <- get_version(stage_path)
    n_git   <- version_to_num(v_git)
    n_stage <- version_to_num(v_stage)

    if (n_stage > n_git) {
      file.copy(stage_path, git_path, overwrite = TRUE)
      cat(glue("  ✓ Updated: {filename}  ({v_git %||% 'no version'} → {v_stage})\n"))
      updated <- updated + 1
    } else if (n_stage == n_git && !is.na(v_git)) {
      cat(glue("  = Current: {filename}  ({v_git})\n"))
    } else if (is.na(v_stage) && file.exists(stage_path)) {
      # No version tag — fall back to file modification time
      if (file.mtime(stage_path) > file.mtime(git_path) + 60) {
        file.copy(stage_path, git_path, overwrite = TRUE)
        cat(glue("  ✓ Updated: {filename}  (newer mtime, no version tag)\n"))
        updated <- updated + 1
      }
    }
  }

  if (updated == 0) cat("  All files are current — no updates needed.\n")
  cat(glue("\n{updated} file(s) updated.\n\n"))
}

# ── Show current versions ─────────────────────────────────────────────────────
cat(strrep("-", 60), "\n")
cat("Current versions in GIT:\n\n")
for (rel_path in tracked_files) {
  git_path <- p(rel_path)
  v        <- get_version(git_path)
  exists   <- file.exists(git_path)
  cat(glue("  {basename(rel_path)}: {if(exists) v %||% '(no version tag)' else '(missing)'}\n"))
}
cat("\n")

# ── Add VERSION tag to a file ─────────────────────────────────────────────────
#' Add or update a VERSION comment at the top of a file
#' @param file_path path to file
#' @param version version string e.g. "2026.03.25.1" (default: today.1)
stamp_version <- function(file_path,
                          version = paste0(format(Sys.Date(), "%Y.%m.%d"), ".1")) {
  if (!file.exists(file_path)) stop("File not found: ", file_path)
  lines <- readLines(file_path, warn = FALSE)
  ver_line <- paste0("# VERSION: ", version)
  # Replace existing VERSION line or insert after first line
  existing <- grep("^#\\s*VERSION:", lines)
  if (length(existing) > 0) {
    lines[existing[1]] <- ver_line
  } else {
    lines <- c(lines[1], ver_line, lines[-1])
  }
  writeLines(lines, file_path)
  cat(glue("Stamped {basename(file_path)} with version {version}\n"))
}

# ── Usage reminder ─────────────────────────────────────────────────────────────
cat("To stamp a version into a file:\n")
cat('  stamp_version(p("R/harmonized/03_main_workflow.R"), "2026.03.25.1")\n\n')
cat("To auto-run on workflow start, add to top of 03_main_workflow.R:\n")
cat('  source(p("R/harmonized/versioning.R"))\n\n')
