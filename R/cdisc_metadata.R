# Package-level cache to avoid re-reading CSV on every call
.metadata_cache <- new.env(parent = emptyenv())

#' Load and Cache CSV Metadata
#'
#' @param filename CSV filename inside inst/extdata/
#' @param key_col Column used to split into a named list (domain or dataset)
#' @param cache_key Name used for the internal cache entry
#' @return Named list of data frames, one per domain/dataset.
#' @noRd
.load_metadata_csv <- function(filename, key_col, cache_key) {
  if (exists(cache_key, envir = .metadata_cache)) {
    return(get(cache_key, envir = .metadata_cache))
  }

  csv_path <- system.file("extdata", filename, package = "clinCompare")
  if (!nzchar(csv_path) || !file.exists(csv_path)) {
    stop("Metadata file not found: ", filename,
         ". Is clinCompare installed correctly?", call. = FALSE)
  }

  raw <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  result <- split(raw[, setdiff(names(raw), key_col)], raw[[key_col]])
  # Drop the split factor so each element is a plain data.frame

  result <- lapply(result, function(df) {
    rownames(df) <- NULL
    df
  })

  assign(cache_key, result, envir = .metadata_cache)
  return(result)
}


#' SDTM Metadata
#'
#' Returns metadata for SDTM domains following CDISC standards.
#' Provides information about required, expected, and permissible variables
#' for each SDTM domain.
#'
#' Variable definitions are based on the published CDISC SDTM Implementation
#' Guide. The canonical machine-readable source is the CDISC Library API
#' (\url{https://www.cdisc.org/cdisc-library}), which requires CDISC
#' membership. The metadata shipped with clinCompare is hand-curated from the
#' published IG specifications and should be cross-referenced with the
#' official CDISC Library for regulatory submissions.
#'
#' @param version Character string specifying the SDTM IG version.
#'   Supported values: "3.4" (default, based on SDTM v2.0),
#'   "3.3" (based on SDTM v1.7). Version "3.3" excludes domains
#'   introduced in v3.4 (GF, CP, BE, BS, SM, TD, TM).
#'
#' @return A named list where keys are SDTM domain codes and values are
#'   data.frames with columns:
#'   \describe{
#'     \item{variable}{Variable name (character)}
#'     \item{label}{Variable label/description (character)}
#'     \item{type}{Data type: "Char" for character or "Num" for numeric}
#'     \item{core}{Importance level: "Req" (Required), "Exp" (Expected),
#'       or "Perm" (Permissible)}
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' sdtm_meta <- get_sdtm_metadata()
#' dm_vars <- sdtm_meta$DM
#' ae_vars <- sdtm_meta$AE
#'
#' # Use SDTM IG 3.3 metadata
#' sdtm_33 <- get_sdtm_metadata(version = "3.3")
#' }
get_sdtm_metadata <- function(version = "3.4") {
  version <- match.arg(version, choices = c("3.4", "3.3"))

  all_domains <- .load_metadata_csv("sdtm_metadata.csv", "domain", "sdtm_all")

  # Drop the min_ig_version helper column from each data.frame
  all_domains <- lapply(all_domains, function(df) {
    df[, c("variable", "label", "type", "core"), drop = FALSE]
  })

  if (version == "3.3") {
    v34_only <- c("GF", "CP", "BE", "BS", "SM", "TD", "TM")
    all_domains[v34_only] <- NULL
  }

  return(all_domains)
}


#' ADaM Metadata
#'
#' Returns metadata for ADaM datasets following CDISC standards.
#' Provides information about required, conditional, and other variables
#' for each ADaM analysis dataset.
#'
#' Variable definitions are based on the published CDISC ADaM Implementation
#' Guide. The canonical machine-readable source is the CDISC Library API
#' (\url{https://www.cdisc.org/cdisc-library}), which requires CDISC
#' membership. The metadata shipped with clinCompare is hand-curated from the
#' published IG specifications.
#'
#' @param version Character string specifying the ADaM IG version.
#'   Supported values: "1.3" (default), "1.2", "1.1".
#'   All versions currently return the same variable definitions;
#'   the version is recorded for provenance tracking.
#'
#' @return A named list where keys are ADaM dataset names and values are
#'   data.frames with columns:
#'   \describe{
#'     \item{variable}{Variable name (character)}
#'     \item{label}{Variable label/description (character)}
#'     \item{type}{Data type: "Char" for character or "Num" for numeric}
#'     \item{core}{Importance level: "Req" (Required), "Cond" (Conditional)}
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' adam_meta <- get_adam_metadata()
#' adsl_vars <- adam_meta$ADSL
#' adae_vars <- adam_meta$ADAE
#' }
get_adam_metadata <- function(version = "1.3") {
  version <- match.arg(version, choices = c("1.3", "1.2", "1.1"))

  .load_metadata_csv("adam_metadata.csv", "dataset", "adam_all")
}


#' Extract CDISC Version from TS Domain
#'
#' @description
#' Reads a Trial Summary (TS) dataset and extracts the CDISC standard version
#' information. Looks for SDTM IG version (TSPARMCD = "SDTIGVER" or "CDISCVER")
#' and ADaM IG version (TSPARMCD = "ADAMIGVR") parameters.
#'
#' @param ts_data A data frame representing a TS (Trial Summary) domain.
#'   Must contain at minimum TSPARMCD and TSVAL columns.
#'
#' @return
#' A list containing:
#' \item{sdtm_ig_version}{Character: SDTM IG version (e.g., "3.4"), or NA}
#' \item{adam_ig_version}{Character: ADaM IG version (e.g., "1.3"), or NA}
#' \item{study_id}{Character: STUDYID from TS if available, or NA}
#' \item{protocol_title}{Character: Protocol title if available, or NA}
#' \item{version_note}{Character: Formatted note string for reports}
#'
#' @export
#' @examples
#' \dontrun{
#' ts <- data.frame(
#'   STUDYID = rep("STUDY001", 3),
#'   TSPARMCD = c("SDTIGVER", "ADAMIGVR", "TITLE"),
#'   TSPARM = c("SDTM IG Version", "ADaM IG Version", "Protocol Title"),
#'   TSVAL = c("3.4", "1.3", "My Phase 3 Trial"),
#'   stringsAsFactors = FALSE
#' )
#' version_info <- extract_cdisc_version(ts)
#' cat(version_info$version_note)
#' }
extract_cdisc_version <- function(ts_data) {
  if (!is.data.frame(ts_data)) {
    stop("ts_data must be a data frame", call. = FALSE)
  }

  # Normalize column names to uppercase for case-insensitive matching
  col_upper <- toupper(colnames(ts_data))

  # Check for required columns
  parmcd_col <- which(col_upper == "TSPARMCD")
  val_col <- which(col_upper == "TSVAL")

  if (length(parmcd_col) == 0 || length(val_col) == 0) {
    warning("TS data must contain TSPARMCD and TSVAL columns", call. = FALSE)
    return(list(
      sdtm_ig_version = NA_character_,
      adam_ig_version = NA_character_,
      study_id = NA_character_,
      protocol_title = NA_character_,
      version_note = ""
    ))
  }

  parmcd_col <- parmcd_col[1]
  val_col <- val_col[1]

  parmcds <- toupper(as.character(ts_data[[parmcd_col]]))
  vals <- as.character(ts_data[[val_col]])

  # Helper to look up a parameter value
  get_ts_val <- function(codes) {
    for (code in codes) {
      idx <- which(parmcds == code)
      if (length(idx) > 0) {
        return(vals[idx[1]])
      }
    }
    return(NA_character_)
  }

  sdtm_ver <- get_ts_val(c("SDTIGVER", "CDISCVER"))
  adam_ver <- get_ts_val(c("ADAMIGVR"))
  study_id <- get_ts_val(c("STUDYID"))
  protocol_title <- get_ts_val(c("TITLE"))

  # Also try STUDYID column directly if not found as a parameter
  studyid_col <- which(col_upper == "STUDYID")
  if (is.na(study_id) && length(studyid_col) > 0 && nrow(ts_data) > 0) {
    study_id <- as.character(ts_data[[studyid_col[1]]][1])
  }

  # Build version note
  parts <- character(0)
  if (!is.na(sdtm_ver)) {
    parts <- c(parts, paste0("SDTM IG ", sdtm_ver))
  }
  if (!is.na(adam_ver)) {
    parts <- c(parts, paste0("ADaM IG ", adam_ver))
  }

  if (length(parts) > 0) {
    version_note <- paste0("CDISC Version (from TS): ", paste(parts, collapse = ", "))
  } else {
    version_note <- ""
  }

  return(list(
    sdtm_ig_version = sdtm_ver,
    adam_ig_version = adam_ver,
    study_id = study_id,
    protocol_title = protocol_title,
    version_note = version_note
  ))
}
