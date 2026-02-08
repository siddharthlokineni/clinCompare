#' Batch Compare CDISC Datasets Across Submission Directories
#'
#' @description
#' Scans two directories for matching dataset files, runs [cdisc_compare()]
#' on each pair, and optionally generates a consolidated Excel report.
#'
#' @param base_dir Path to directory containing base/reference files.
#' @param compare_dir Path to directory containing comparison files.
#' @param format File format to match: "xpt", "sas7bdat", "csv", or "rds".
#'   When NULL (default), auto-detected from the most common file type in base_dir.
#' @param id_vars Optional character vector of ID variables (passed to each comparison).
#'   When NULL, CDISC-standard keys are auto-detected per domain.
#' @param tolerance Numeric tolerance for floating-point comparisons (default 0).
#' @param output_file Optional path to Excel (.xlsx) file for consolidated report.
#'
#' @return Named list of cdisc_compare() results, one per matched domain.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Auto-detects format from directory contents
#'   results <- compare_submission("v1/", "v2/",
#'                                  output_file = "submission_diff.xlsx")
#'
#'   # Explicit format
#'   results <- compare_submission("v1/", "v2/", format = "csv")
#' }
#'
compare_submission <- function(base_dir,
                               compare_dir,
                               format = NULL,
                               id_vars = NULL,
                               tolerance = 0,
                               output_file = NULL) {

  # ============================================================================
  # Validate inputs
  # ============================================================================
  if (!dir.exists(base_dir)) {
    stop("base_dir does not exist: ", base_dir)
  }
  if (!dir.exists(compare_dir)) {
    stop("compare_dir does not exist: ", compare_dir)
  }

  # Auto-detect format if not specified
  if (is.null(format)) {
    supported <- c("xpt", "sas7bdat", "csv", "rds")
    all_files <- list.files(base_dir, ignore.case = TRUE, full.names = FALSE)
    extensions <- tolower(tools::file_ext(all_files))
    ext_counts <- table(extensions[extensions %in% supported])
    if (length(ext_counts) == 0) {
      stop("No supported files (.xpt, .sas7bdat, .csv, .rds) found in base_dir")
    }
    format <- names(which.max(ext_counts))
    message(sprintf("File format auto-detected: .%s (%d files)", format, max(ext_counts)))
  }

  format <- tolower(format)
  if (!format %in% c("xpt", "sas7bdat", "csv", "rds")) {
    stop("format must be one of: 'xpt', 'sas7bdat', 'csv', 'rds'")
  }

  # Build file pattern for the format
  file_pattern <- switch(format,
    "xpt" = "\\.xpt$",
    "sas7bdat" = "\\.sas7bdat$",
    "csv" = "\\.csv$",
    "rds" = "\\.rds$"
  )

  # ============================================================================
  # List files in both directories
  # ============================================================================
  base_files <- list.files(
    path = base_dir,
    pattern = file_pattern,
    ignore.case = TRUE,
    full.names = FALSE
  )
  compare_files <- list.files(
    path = compare_dir,
    pattern = file_pattern,
    ignore.case = TRUE,
    full.names = FALSE
  )

  # Extract basenames without extension for matching (case-insensitive)
  base_names <- tolower(tools::file_path_sans_ext(base_files))
  compare_names <- tolower(tools::file_path_sans_ext(compare_files))

  # Find matched pairs
  matched_names <- intersect(base_names, compare_names)

  if (length(matched_names) == 0) {
    message("No matching files found between directories.")
    return(invisible(list()))
  }

  cat(sprintf("Found %d matching file pair(s): %s\n",
              length(matched_names), paste(matched_names, collapse = ", ")))

  # Report unmatched files
  unmatched_base <- setdiff(base_names, compare_names)
  unmatched_compare <- setdiff(compare_names, base_names)

  if (length(unmatched_base) > 0) {
    message("Files only in base_dir: ", paste(unmatched_base, collapse = ", "))
  }
  if (length(unmatched_compare) > 0) {
    message("Files only in compare_dir: ", paste(unmatched_compare, collapse = ", "))
  }

  # ============================================================================
  # Run cdisc_compare() on each matched pair
  # ============================================================================
  results <- list()
  summary_data <- data.frame(
    Domain = character(),
    Base_Rows = integer(),
    Compare_Rows = integer(),
    Attr_Diffs = integer(),
    Value_Diffs = integer(),
    CDISC_Errors = integer(),
    Verdict = character(),
    stringsAsFactors = FALSE
  )

  for (matched_name in matched_names) {
    # Find the actual file (preserving original case)
    idx_base <- which(tolower(tools::file_path_sans_ext(base_files)) == matched_name)
    idx_compare <- which(tolower(tools::file_path_sans_ext(compare_files)) == matched_name)

    base_file <- base_files[idx_base]
    compare_file <- compare_files[idx_compare]

    base_path <- file.path(base_dir, base_file)
    compare_path <- file.path(compare_dir, compare_file)

    # Run comparison with error handling
    result <- tryCatch(
      {
        cdisc_compare(
          df1 = base_path,
          df2 = compare_path,
          id_vars = id_vars,
          tolerance = tolerance
        )
      },
      error = function(e) {
        message("Error comparing ", matched_name, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(result)) {
      results[[matched_name]] <- result

      # ========================================================================
      # Extract metrics for summary table
      # ========================================================================
      base_rows <- result$nrow_df1
      compare_rows <- result$nrow_df2

      # Count attribute differences (metadata_comparison)
      attr_diffs <- 0
      if (!is.null(result$metadata_comparison)) {
        meta <- result$metadata_comparison
        # Count variables with any attribute mismatch
        mismatch_cols <- intersect(
          c("type_mismatches", "label_mismatches", "format_mismatches",
            "length_mismatches", "decimals_mismatches"),
          names(meta)
        )
        if (length(mismatch_cols) > 0 && nrow(meta) > 0) {
          attr_diffs <- sum(
            rowSums(meta[, mismatch_cols, drop = FALSE], na.rm = TRUE) > 0,
            na.rm = TRUE
          )
        }
      }

      # Count value differences
      value_diffs <- 0
      if (!is.null(result$observation_comparison)) {
        if (!is.null(result$observation_comparison$discrepancies)) {
          value_diffs <- sum(result$observation_comparison$discrepancies, na.rm = TRUE)
        } else if (!is.null(result$observation_comparison$status)) {
          if (result$observation_comparison$status == "Skipped") {
            value_diffs <- NA_integer_  # Mark as skipped
          }
        }
      }

      # Count CDISC errors
      cdisc_errors <- 0
      if (!is.null(result$cdisc_validation_df1)) {
        cdisc_errors <- cdisc_errors +
          sum(result$cdisc_validation_df1$severity == "ERROR", na.rm = TRUE)
      }
      if (!is.null(result$cdisc_validation_df2)) {
        cdisc_errors <- cdisc_errors +
          sum(result$cdisc_validation_df2$severity == "ERROR", na.rm = TRUE)
      }

      # Determine verdict
      verdict <- "Match"
      if (!is.na(value_diffs) && value_diffs > 0) {
        verdict <- sprintf("%d value diffs", value_diffs)
      } else if (is.na(value_diffs)) {
        verdict <- "Comparison Skipped"
      }
      if (attr_diffs > 0) {
        if (verdict == "Match") {
          verdict <- sprintf("%d attr diffs", attr_diffs)
        } else {
          verdict <- sprintf("%s, %d attr diffs", verdict, attr_diffs)
        }
      }
      if (cdisc_errors > 0) {
        if (verdict == "Match") {
          verdict <- sprintf("%d CDISC errors", cdisc_errors)
        } else {
          verdict <- sprintf("%s, %d CDISC errors", verdict, cdisc_errors)
        }
      }

      # Add row to summary
      summary_data <- rbind(
        summary_data,
        data.frame(
          Domain = matched_name,
          Base_Rows = base_rows,
          Compare_Rows = compare_rows,
          Attr_Diffs = attr_diffs,
          Value_Diffs = value_diffs,
          CDISC_Errors = cdisc_errors,
          Verdict = verdict,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # ============================================================================
  # Print summary table to console
  # ============================================================================
  message("\n=== Submission Comparison Summary ===\n")

  # Build formatted table with aligned columns
  cat(sprintf(
    "%-20s %10s %12s %10s %11s %13s %s\n",
    "Domain",
    "Base Rows",
    "Compare Rows",
    "Attr Diffs",
    "Value Diffs",
    "CDISC Errors",
    "Verdict"
  ))
  cat(strrep("-", 100), "\n")

  for (i in seq_len(nrow(summary_data))) {
    row <- summary_data[i, ]
    value_diffs_str <- if (is.na(row$Value_Diffs)) "NA" else as.character(row$Value_Diffs)
    cat(sprintf(
      "%-20s %10d %12d %10d %11s %13d %s\n",
      row$Domain,
      row$Base_Rows,
      row$Compare_Rows,
      row$Attr_Diffs,
      value_diffs_str,
      row$CDISC_Errors,
      row$Verdict
    ))
  }
  cat("\n")

  # ============================================================================
  # Generate Excel report if requested
  # ============================================================================
  if (!is.null(output_file)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      message("openxlsx package not available. Skipping Excel report generation.")
    } else {
      tryCatch(
        {
          # Create workbook
          wb <- openxlsx::createWorkbook()

          # Add Summary sheet
          openxlsx::addWorksheet(wb, "Summary")
          openxlsx::writeData(wb, sheet = "Summary", x = summary_data)

          # Add sheets for each domain with value differences
          for (domain_name in names(results)) {
            result <- results[[domain_name]]

            # Get value differences if available
            if (!is.null(result$observation_comparison)) {
              if (!is.null(result$observation_comparison$status) &&
                  result$observation_comparison$status == "Skipped") {
                # Skip this sheet
                next
              }

              diff_data <- get_all_differences(result)
              if (!is.null(diff_data) && nrow(diff_data) > 0) {
                sheet_name <- substr(domain_name, 1, 31) # Excel sheet name limit
                openxlsx::addWorksheet(wb, sheet_name)
                openxlsx::writeData(wb, sheet = sheet_name, x = diff_data)
              }
            }
          }

          # Save workbook
          openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
          message("Excel report saved to: ", output_file)
        },
        error = function(e) {
          message("Error generating Excel report: ", conditionMessage(e))
        }
      )
    }
  }

  # Return results invisibly but accessible
  invisible(results)
}
