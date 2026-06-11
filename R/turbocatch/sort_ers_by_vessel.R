# =============================================================================
# sort_ers_by_vessel.R
# Sort Agiltech / TurboCatch ERS V3 XML files into per-vessel sub-folders
#
# Each file is read just enough to extract the vessel's external registration
# number (XR attribute on the <LOG> node), which is the most human-readable
# vessel identifier (e.g. "CC12345"). Files are then copied into:
#
#   output_dir/
#     CC12345/
#       OOE20260113007902.xml
#       OOE20260118007901.xml
#     CC67890/
#       OOE20260122007603.xml
#       ...
#
# You can then run anonymize_ers_folder() on each sub-folder separately,
# or on the whole output_dir with recursive = TRUE.
#
# Usage:
#   source("sort_ers_by_vessel.R")
#   sort_ers_by_vessel("C:/ERS_exports/2025", "C:/ERS_exports/sorted")
# =============================================================================

library(xml2)
library(purrr)

#' Extract the vessel identifier from a single ERS XML file.
#' Returns a named list: $xr (external reg), $rc (call sign), $na (name)
get_vessel_id <- function(filepath) {
  doc <- tryCatch(
    read_xml(filepath),
    error = function(e) {
      message("  SKIP (cannot read): ", basename(filepath), " - ", e$message)
      return(NULL)
    }
  )
  if (is.null(doc)) return(NULL)

  ns  <- xml_ns(doc)
  log <- xml_find_first(doc, ".//d1:LOG", ns)
  if (inherits(log, "xml_missing"))
    log <- xml_find_first(doc, ".//LOG")
  if (inherits(log, "xml_missing")) {
    message("  SKIP (no LOG node): ", basename(filepath))
    return(NULL)
  }

  list(
    xr = xml_attr(log, "XR"),   # External registration -- most readable
    rc = xml_attr(log, "RC"),   # Radio call sign
    na = xml_attr(log, "NA")    # Vessel name
  )
}

#' Copy one file into output_dir/<vessel_folder>/
copy_to_vessel_folder <- function(filepath, output_dir, id_field) {
  info <- get_vessel_id(filepath)
  if (is.null(info)) return(invisible(NULL))

  # Pick the best available identifier for the folder name
  folder_name <- info[[id_field]]
  if (is.na(folder_name) || folder_name == "") {
    # Fallback chain: xr -> rc -> na -> UNKNOWN
    folder_name <- Filter(function(x) !is.na(x) && x != "",
                          unlist(info))[1]
    if (length(folder_name) == 0) folder_name <- "UNKNOWN"
  }

  # Sanitize for use as a folder name (remove characters unsafe on Windows/Mac)
  folder_name <- gsub("[/\\\\:*?\"<>|]", "_", folder_name)

  dest_dir <- file.path(output_dir, folder_name)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("  Created folder: ", folder_name)
  }

  dest_file <- file.path(dest_dir, basename(filepath))
  file.copy(filepath, dest_file, overwrite = TRUE)
  message("  ", basename(filepath), "  ->  ", folder_name, "/")

  invisible(dest_file)
}

#' Sort all ERS XML files in input_dir into per-vessel sub-folders
#'
#' @param input_dir   Folder containing the raw XML export files
#' @param output_dir  Where to create the per-vessel sub-folders
#'                    (defaults to a "sorted" sub-folder inside input_dir)
#' @param id_field    Which vessel attribute to use as folder name:
#'                    "xr" (external reg, default), "rc" (call sign),
#'                    or "na" (vessel name)
#' @param recursive   Also search sub-folders of input_dir? Default FALSE
sort_ers_by_vessel <- function(input_dir,
                                output_dir = file.path(input_dir, "sorted"),
                                id_field   = "xr",
                                recursive  = FALSE) {
  files <- list.files(input_dir,
                      pattern    = "\\.xml$",
                      full.names = TRUE,
                      recursive  = recursive)

  # Exclude files already inside the output_dir
  files <- files[!startsWith(normalizePath(files, mustWork = FALSE),
                              normalizePath(output_dir, mustWork = FALSE))]

  if (length(files) == 0) {
    message("No .xml files found in: ", input_dir)
    return(invisible(NULL))
  }

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  message("Sorting ", length(files), " files by vessel (field = '",
          id_field, "')...")
  walk(files, copy_to_vessel_folder,
       output_dir = output_dir,
       id_field   = id_field)

  # Print a summary of what ended up where
  vessel_folders <- list.dirs(output_dir, recursive = FALSE, full.names = FALSE)
  message("\nSummary:")
  for (vf in vessel_folders) {
    n <- length(list.files(file.path(output_dir, vf), pattern = "\\.xml$"))
    message("  ", vf, ":  ", n, " file(s)")
  }
  message("\nDone. Sorted files are in: ", output_dir)
}

# =============================================================================
# EXAMPLE
# =============================================================================
# source("sort_ers_by_vessel.R")
#
# sort_ers_by_vessel(
#   input_dir  = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch",
#   output_dir = "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch/sorted"
# )


# This creates:
#   C:/ERS_exports/sorted/CC12345/   <- vessel A
#   C:/ERS_exports/sorted/CC67890/   <- vessel B
#
# Then anonymize each vessel separately (keeps VESSEL01/VESSEL02 consistent):
  # source(file.path(here::here(), "R/turbocatch/anonymize_ers.R"))
  # anonymize_ers_folder("C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch ExportMessagesERS_EVLinden/sorted/CC545762",
  #                      "C:/Users/MartinPastoors/Martin Pastoors/FLYSHOOT - General/tripdata/turbocatch ExportMessagesERS_EVLinden/anonymized/VesselA")
  # anonymize_ers_folder("C:/ERS_exports/sorted/CC67890",
  #                      "C:/ERS_exports/anonymized/VesselB")
#
# Or anonymize everything in one go:
#   anonymize_ers_folder("C:/ERS_exports/sorted",
#                        "C:/ERS_exports/anonymized",
#                        recursive = TRUE)
# =============================================================================
