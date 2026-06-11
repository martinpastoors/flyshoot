# =============================================================================
# anonymize_ers.R
# Anonymize Agiltech / TurboCatch ERS V3 XML files before sharing
#
# What it replaces:
#   - Vessel CFR / IR  (International Radio / Community Fleet Register number)
#   - Vessel name (NA attribute)
#   - Vessel call signs (RC, XR attributes)
#   - Master name (MA attribute)
#   - Message numbers (ON, RN attributes) — replaced with sequential IDs
#     so structure is preserved but originals are hidden
#
# What it KEEPS (needed for parsing):
#   - All dates and times
#   - All positions (lat/lon, ICES rectangles)
#   - All species codes and weights
#   - All gear codes
#   - Message types (FAR, PNO, COE, DEP, COX…)
#   - Flag state (FRA etc.) — not sensitive
#
# Usage:
#   source("anonymize_ers.R")
#   anonymize_ers_folder("C:/ERS_exports/2025", "C:/ERS_exports/anonymized")
# =============================================================================

library(xml2)
library(purrr)

# Fields to blank out completely
BLANK_ATTRS <- c(
  "IR",   # CFR / internal registration
  "MA",   # Master / skipper name
  "MD"    # Master date of birth or ID
)

# Fields to replace with a consistent fake per vessel
# (same vessel gets same fake name across all files)
VESSEL_ATTRS <- c(
  "RC",   # Radio call sign
  "XR",   # External registration number
  "NA"    # Vessel name
)

# -- helpers ------------------------------------------------------------------

make_fake_vessel_id <- function() {
  # Returns a function that maps real vessel IDs to fake ones consistently
  registry <- list()
  counter  <- 0L

  function(real_id, prefix = "VESSEL") {
    if (is.null(real_id) || is.na(real_id) || real_id == "") return("UNKNOWN")
    key <- as.character(real_id)
    if (is.null(registry[[key]])) {
      counter  <<- counter + 1L
      registry[[key]] <<- paste0(prefix, sprintf("%02d", counter))
    }
    registry[[key]]
  }
}

# One shared lookup so the same vessel gets the same fake name across files
vessel_lookup <- make_fake_vessel_id()

anonymize_node_attrs <- function(node, ir_fake) {
  # Blank out sensitive personal fields
  for (attr in BLANK_ATTRS) {
    if (!is.na(xml_attr(node, attr)))
      xml_set_attr(node, attr, "REDACTED")
  }

  # Replace vessel identity fields with consistent fake
  for (attr in VESSEL_ATTRS) {
    val <- xml_attr(node, attr)
    if (!is.na(val))
      xml_set_attr(node, attr, paste0(ir_fake, "_", attr))
  }
}

anonymize_file <- function(filepath, output_dir) {
  doc <- tryCatch(
    read_xml(filepath),
    error = function(e) {
      message("  SKIP (cannot read): ", basename(filepath), " — ", e$message)
      return(NULL)
    }
  )
  if (is.null(doc)) return(invisible(NULL))

  # Find all LOG nodes — these carry the vessel identity attributes
  ns   <- xml_ns(doc)
  logs <- xml_find_all(doc, ".//d1:LOG", ns)
  if (length(logs) == 0) logs <- xml_find_all(doc, ".//LOG")

  for (log in logs) {
    real_ir <- xml_attr(log, "IR")
    ir_fake <- vessel_lookup(real_ir, "VESSEL")

    # Replace the CFR/IR with the fake vessel ID
    if (!is.na(real_ir))
      xml_set_attr(log, "IR", ir_fake)

    anonymize_node_attrs(log, ir_fake)
  }

  # Also scrub ON (original message number) in OPS root — optional,
  # keeps the sequential structure but hides the real fleet number
  ops <- xml_root(doc)
  on_val <- xml_attr(ops, "ON")
  if (!is.na(on_val)) {
    # Keep the date prefix, replace the vessel-specific numeric suffix
    scrubbed <- sub("([A-Z]+)(\\d{8})(\\d+)$", "\\1\\2XXXXX", on_val)
    xml_set_attr(ops, "ON", scrubbed)
  }

  # Write output
  out_name <- basename(filepath)
  out_path <- file.path(output_dir, out_name)
  write_xml(doc, out_path, encoding = "UTF-8")
  message("  OK: ", out_name)
  invisible(out_path)
}

# -- main entry point ---------------------------------------------------------

#' Anonymize all ERS XML files in a folder
#' @param input_dir  Folder containing original XML files
#' @param output_dir Folder to write anonymized copies (created if needed)
#' @param recursive  Also process sub-folders? Default FALSE
anonymize_ers_folder <- function(input_dir,
                                  output_dir = file.path(input_dir, "anonymized"),
                                  recursive  = FALSE) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output folder: ", output_dir)
  }

  files <- list.files(input_dir,
                      pattern   = "\\.xml$",
                      full.names = TRUE,
                      recursive  = recursive)

  # Exclude files already in the output dir
  files <- files[!startsWith(normalizePath(files),
                              normalizePath(output_dir))]

  if (length(files) == 0) {
    message("No .xml files found in: ", input_dir)
    return(invisible(NULL))
  }

  message("Anonymizing ", length(files), " files...")
  walk(files, anonymize_file, output_dir = output_dir)
  message("Done. Anonymized files are in: ", output_dir)
}

# =============================================================================
# EXAMPLE
# =============================================================================
# source("anonymize_ers.R")
#
# anonymize_ers_folder(
#   input_dir  = "C:/ERS_exports/2025",
#   output_dir = "C:/ERS_exports/anonymized"
# )
#
# Then share the files from C:/ERS_exports/anonymized
# =============================================================================
