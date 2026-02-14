#' Export Comparison Report to File
#'
#' @description
#' Exports a dataset or CDISC comparison result to a file in multiple formats.
#' Automatically detects format from file extension (.html, .txt, .xlsx).
#'
#' @param result A list from [compare_datasets()] or [cdisc_compare()].
#' @param file Character string specifying the output file path.
#'   File extension determines format: .html, .txt, or .xlsx.
#' @param format Character string specifying output format: "html", "text", or "excel".
#'   If NULL (default), format is auto-detected from file extension.
#'
#' @return
#' Invisibly returns the input `result` (useful for piping).
#'
#' @details
#' Supported formats:
#' - **HTML** (.html): Self-contained HTML report with styling and interactive charts.
#' - **Text** (.txt): Plain text report suitable for console review.
#' - **Excel** (.xlsx): Multi-sheet workbook with tabbed data:
#'   - "Summary": Dataset dimensions, domain, standard, matching type, tolerance
#'   - "Variable Diffs": Metadata attribute differences
#'   - "Value Diffs": Unified diff data frame from [get_all_differences()]
#'   - "CDISC Validation": Combined validation results (for CDISC comparisons only)
#'
#' The result object can be either a `dataset_comparison` (from [compare_datasets()])
#' or `cdisc_comparison` (from [cdisc_compare()]). All features are supported for both.
#'
#' @export
#' @examples
#' \donttest{
#' # Create sample datasets
#' df1 <- data.frame(
#'   ID = c(1, 2, 3),
#'   NAME = c("Alice", "Bob", "Charlie"),
#'   AGE = c(25, 30, 35)
#' )
#'
#' df2 <- data.frame(
#'   ID = c(1, 2, 3),
#'   NAME = c("Alice", "Bob", "Charles"),
#'   AGE = c(25, 30, 36)
#' )
#'
#' # Compare datasets
#' result <- compare_datasets(df1, df2)
#'
#' # Export to different formats (write to tempdir)
#' export_report(result, file.path(tempdir(), "report.html"))
#' export_report(result, file.path(tempdir(), "report.txt"))
#'
#' # Explicit format specification
#' export_report(result, file.path(tempdir(), "report.xlsx"), format = "excel")
#' }
export_report <- function(result, file, format = NULL) {
  if (!is.list(result)) {
    stop("result must be a list from compare_datasets() or cdisc_compare()", call. = FALSE)
  }

  if (!is.character(file) || length(file) != 1 || nchar(file) == 0) {
    stop("file must be a non-empty character string", call. = FALSE)
  }

  # Auto-detect format from file extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
      "html" = "html",
      "htm"  = "html",
      "txt"  = "text",
      "xlsx" = "excel",
      "xls"  = "excel",
      stop(sprintf("Cannot detect format from extension '.%s'. Use format = 'html', 'text', or 'excel'.", ext),
           call. = FALSE)
    )
  }

  format <- match.arg(format, choices = c("html", "text", "excel"))

  is_cdisc <- inherits(result, "cdisc_comparison") ||
    !is.null(result$cdisc_validation) ||
    !is.null(result$domain)

  if (format == "html") {
    export_html_report(result, file, is_cdisc)
  } else if (format == "text") {
    export_text_report(result, file, is_cdisc)
  } else if (format == "excel") {
    export_excel_report(result, file, is_cdisc)
  }

  message(sprintf("Report written to: %s", file))
  invisible(result)
}


# ---- Internal helpers -------------------------------------------------------

.null_default <- function(x, y) if (is.null(x)) y else x

#' @keywords internal
export_html_report <- function(result, file, is_cdisc) {
  if (is_cdisc) {
    report_html <- generate_html_report(result)
  } else {
    report_html <- generate_dataset_html_report(result)
  }
  writeLines(report_html, file)
}

#' @keywords internal
export_text_report <- function(result, file, is_cdisc) {
  if (is_cdisc) {
    report_text <- generate_text_report(result)
  } else {
    report_text <- generate_dataset_text_report(result)
  }
  writeLines(report_text, file)
}

#' @keywords internal
export_excel_report <- function(result, file, is_cdisc) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required for Excel export. Install it with: install.packages('openxlsx')",
         call. = FALSE)
  }

  wb <- openxlsx::createWorkbook()

  # --- Summary sheet ---
  openxlsx::addWorksheet(wb, "Summary")
  summary_data <- build_summary_df(result, is_cdisc)
  openxlsx::writeData(wb, "Summary", summary_data)

  # --- Variable Diffs sheet ---
  openxlsx::addWorksheet(wb, "Variable Diffs")
  var_diffs <- build_variable_diffs_df(result)
  openxlsx::writeData(wb, "Variable Diffs", var_diffs)

  # --- Value Diffs sheet ---
  openxlsx::addWorksheet(wb, "Value Diffs")
  value_diffs <- tryCatch(get_all_differences(result), error = function(e) {
    data.frame(Note = "No value-level differences available")
  })
  openxlsx::writeData(wb, "Value Diffs", value_diffs)

  # --- CDISC Validation sheet (if applicable) ---
  if (is_cdisc && !is.null(result$cdisc_validation)) {
    openxlsx::addWorksheet(wb, "CDISC Validation")
    validation_df <- build_validation_df(result)
    openxlsx::writeData(wb, "CDISC Validation", validation_df)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}


# ---- Dataset (non-CDISC) report generators ----------------------------------

#' @keywords internal
generate_dataset_text_report <- function(result) {
  lines <- character()
  lines <- c(lines, "clinCompare: Dataset Comparison Report")
  lines <- c(lines, paste(rep("=", 50), collapse = ""))
  lines <- c(lines, "")

  # Dimensions
  if (!is.null(result$dimension_comparison)) {
    dc <- result$dimension_comparison
    nrow1 <- .null_default(dc$nrow_df1, "?")
    ncol1 <- .null_default(dc$ncol_df1, "?")
    nrow2 <- .null_default(dc$nrow_df2, "?")
    ncol2 <- .null_default(dc$ncol_df2, "?")
    lines <- c(lines, sprintf("Base:    %s rows x %s cols", nrow1, ncol1))
    lines <- c(lines, sprintf("Compare: %s rows x %s cols", nrow2, ncol2))
    lines <- c(lines, "")
  }

  # Variable comparison
  if (!is.null(result$variable_comparison)) {
    if (length(result$extra_in_df1) > 0) {
      lines <- c(lines, sprintf("Columns only in base: %s",
                                 paste(result$extra_in_df1, collapse = ", ")))
    }
    if (length(result$extra_in_df2) > 0) {
      lines <- c(lines, sprintf("Columns only in compare: %s",
                                 paste(result$extra_in_df2, collapse = ", ")))
    }
    lines <- c(lines, "")
  }

  # Type mismatches
  if (!is.null(result$type_mismatches) && is.data.frame(result$type_mismatches) &&
      nrow(result$type_mismatches) > 0) {
    lines <- c(lines, "Type Mismatches:")
    lines <- c(lines, paste(utils::capture.output(print(result$type_mismatches)), collapse = "\n"))
    lines <- c(lines, "")
  }

  # Observation comparison
  if (!is.null(result$observation_comparison)) {
    oc <- result$observation_comparison
    if (!is.null(oc$discrepancies)) {
      disc <- oc$discrepancies
      total_diffs <- sum(disc, na.rm = TRUE)
      lines <- c(lines, sprintf("Total value differences: %d", total_diffs))
      if (total_diffs > 0) {
        diff_cols <- names(disc[disc > 0])
        for (col in diff_cols) {
          lines <- c(lines, sprintf("  %s: %d difference(s)", col, disc[col]))
        }
      }
      lines <- c(lines, "")
    }
  }

  paste(lines, collapse = "\n")
}

#' @keywords internal
generate_dataset_html_report <- function(result) {
  text_report <- generate_dataset_text_report(result)
  html_lines <- c(
    "<!DOCTYPE html>",
    "<html><head><meta charset='UTF-8'>",
    "<title>clinCompare Report</title>",
    "<style>",
    "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
    "  max-width: 900px; margin: 40px auto; padding: 20px; color: #333; }",
    "h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }",
    "pre { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;",
    "  padding: 16px; overflow-x: auto; font-size: 14px; line-height: 1.5; }",
    ".meta { color: #6c757d; font-size: 0.9em; margin-bottom: 20px; }",
    "</style></head><body>",
    "<h1>clinCompare: Dataset Comparison Report</h1>",
    sprintf("<p class='meta'>Generated: %s</p>", Sys.time()),
    "<pre>",
    gsub("<", "&lt;", gsub(">", "&gt;", text_report)),
    "</pre>",
    "</body></html>"
  )
  paste(html_lines, collapse = "\n")
}


# ---- Excel helper functions --------------------------------------------------

#' @keywords internal
build_summary_df <- function(result, is_cdisc) {
  rows <- list()

  if (is_cdisc) {
    rows$Domain <- .null_default(result$domain, "N/A")
    rows$Standard <- .null_default(result$standard, "N/A")
    rows$`Matching Type` <- .null_default(result$matching_type, "N/A")
  }

  if (!is.null(result$dimension_comparison)) {
    dc <- result$dimension_comparison
    rows$`Base Rows` <- .null_default(dc$nrow_df1, NA)
    rows$`Base Cols` <- .null_default(dc$ncol_df1, NA)
    rows$`Compare Rows` <- .null_default(dc$nrow_df2, NA)
    rows$`Compare Cols` <- .null_default(dc$ncol_df2, NA)
  }

  if (!is.null(result$observation_comparison$discrepancies)) {
    rows$`Total Value Differences` <- sum(result$observation_comparison$discrepancies, na.rm = TRUE)
  }

  if (!is.null(result$tolerance)) {
    rows$Tolerance <- result$tolerance
  }

  data.frame(
    Field = names(rows),
    Value = as.character(unlist(rows)),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
build_variable_diffs_df <- function(result) {
  if (!is.null(result$type_mismatches) && is.data.frame(result$type_mismatches) &&
      nrow(result$type_mismatches) > 0) {
    return(result$type_mismatches)
  }

  if (!is.null(result$attribute_diffs) && is.data.frame(result$attribute_diffs) &&
      nrow(result$attribute_diffs) > 0) {
    return(result$attribute_diffs)
  }

  data.frame(Note = "No variable-level differences found", stringsAsFactors = FALSE)
}

#' @keywords internal
build_validation_df <- function(result) {
  val <- result$cdisc_validation

  if (is.data.frame(val)) {
    return(val)
  }

  # If validation is a list with df1/df2 results, combine them
  frames <- list()
  if (!is.null(val$df1) && is.data.frame(val$df1)) {
    df1_val <- val$df1
    df1_val$Dataset <- "Base"
    frames <- c(frames, list(df1_val))
  }
  if (!is.null(val$df2) && is.data.frame(val$df2)) {
    df2_val <- val$df2
    df2_val$Dataset <- "Compare"
    frames <- c(frames, list(df2_val))
  }

  if (length(frames) > 0) {
    return(do.call(rbind, frames))
  }

  data.frame(Note = "No CDISC validation data available", stringsAsFactors = FALSE)
}
