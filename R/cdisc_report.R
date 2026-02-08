#' Generate CDISC Validation Report
#'
#' @description
#' Generates a formatted report from the results of [cdisc_compare()]. Supports
#' text console output with optional save to file (.txt), and styled HTML reports.
#'
#' @param cdisc_results A list output from [cdisc_compare()].
#' @param output_format Character string: "text" (default) for console output
#'   or "html" for HTML report.
#' @param file_name Optional character string specifying the output file path.
#'   For "text" format, the report is printed to console and also saved to
#'   file if provided (e.g. "report.txt"). For "html" format, defaults to
#'   "cdisc_report.html" if not provided.
#'
#' @return Invisibly returns the input `cdisc_results` (useful for piping).
#'
#' @details
#' The report includes:
#' - Dataset Comparison Summary
#' - Unified Comparison Table (attribute + value differences in one table)
#' - Unmatched Rows (when id_vars were used)
#' - CDISC Compliance for each dataset
#' - CDISC Conformance Comparison
#' - Comparison Dashboard (HTML only)
#'
#' For text output, formatting uses a clean tabular layout. Use
#' \code{file_name} to save to a .txt file for easy sharing and review.
#' For HTML output, a self-contained report is generated with color-coded
#' badges for difference types and severity levels.
#'
#' @keywords internal
#' @examples
#' \donttest{
#' # Create sample datasets
#' dm1 <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = c("SUBJ001", "SUBJ002"),
#'   DMSEQ = c(1, 1),
#'   RACE = c("WHITE", "BLACK OR AFRICAN AMERICAN")
#' )
#'
#' dm2 <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = c("SUBJ001", "SUBJ003"),
#'   DMSEQ = c(1, 1),
#'   RACE = c("WHITE", "ASIAN")
#' )
#'
#' result <- cdisc_compare(dm1, dm2, domain = "DM")
#'
#' # Generate text report to console
#' generate_cdisc_report(result, output_format = "text")
#'
#' # Save text report to .txt file
#' generate_cdisc_report(result, output_format = "text", file_name = "report.txt")
#'
#' # Generate HTML report to file
#' generate_cdisc_report(result, output_format = "html", file_name = "report.html")
#' }
generate_cdisc_report <- function(cdisc_results, output_format = "text",
                                   file_name = NULL) {
  if (!is.list(cdisc_results)) {
    stop("cdisc_results must be a list from cdisc_compare()", call. = FALSE)
  }

  if (!output_format %in% c("text", "html")) {
    stop("output_format must be either 'text' or 'html'", call. = FALSE)
  }

  if (output_format == "text") {
    report_text <- generate_text_report(cdisc_results)

    cat(report_text)

    if (!is.null(file_name)) {
      writeLines(report_text, file_name)
      message(sprintf("Report written to: %s", file_name))
    }
  } else {
    # HTML output
    report_html <- generate_html_report(cdisc_results)

    if (is.null(file_name)) {
      file_name <- "cdisc_report.html"
    }

    writeLines(report_html, file_name)
    message(sprintf("HTML report written to: %s", file_name))
  }

  invisible(cdisc_results)
}


#' Print CDISC Validation Results
#'
#' @description
#' Pretty-prints CDISC validation results to the console with a summary and grouped
#' output by category. Displays counts of errors, warnings, and info messages.
#'
#' @param validation_result A data frame from [validate_cdisc()].
#'
#' @return Invisibly returns the input `validation_result` (useful for piping).
#'
#' @details
#' Output includes:
#' - Summary counts of errors, warnings, and info messages
#' - Issues grouped by category
#' - Each issue displayed with its variable name and message
#'
#' @keywords internal
#' @examples
#' \donttest{
#' # Validate a dataset
#' dm <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = c("SUBJ001", "SUBJ002"),
#'   DMSEQ = c(1, 1),
#'   RACE = c("WHITE", "BLACK OR AFRICAN AMERICAN")
#' )
#'
#' validation_result <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
#' print_cdisc_validation(validation_result)
#' }
print_cdisc_validation <- function(validation_result) {
  if (!is.data.frame(validation_result)) {
    stop("validation_result must be a data frame from validate_cdisc()", call. = FALSE)
  }

  message("")
  message(paste0("=", strrep("=", 77)))
  message("CDISC VALIDATION RESULTS")
  message(paste0("=", strrep("=", 77)))
  message("")

  if (nrow(validation_result) == 0) {
    message("No validation issues found.")
    message("")
    return(invisible(validation_result))
  }

  # Count by severity
  severity_counts <- table(validation_result$severity)
  message(sprintf(
    "Summary: %d error(s), %d warning(s), %d info message(s)",
    if ("ERROR" %in% names(severity_counts)) severity_counts[["ERROR"]] else 0L,
    if ("WARNING" %in% names(severity_counts)) severity_counts[["WARNING"]] else 0L,
    if ("INFO" %in% names(severity_counts)) severity_counts[["INFO"]] else 0L
  ))
  message("")

  # Group by category
  categories <- unique(validation_result$category)
  for (cat in categories) {
    cat_rows <- validation_result[validation_result$category == cat, ]
    message(sprintf("%s (%d):", cat, nrow(cat_rows)))

    # Sort by severity (ERROR > WARNING > INFO)
    severity_order <- c("ERROR" = 1, "WARNING" = 2, "INFO" = 3)
    cat_rows <- cat_rows[order(severity_order[cat_rows$severity]), ]

    for (i in seq_len(nrow(cat_rows))) {
      message(sprintf(
        "  [%s] %s: %s",
        cat_rows$severity[i],
        cat_rows$variable[i],
        cat_rows$message[i]
      ))
    }
    message("")
  }

  message(paste0("=", strrep("=", 77)))
  message("")

  invisible(validation_result)
}


#' Generate Text Report
#'
#' @description
#' Internal function to generate a formatted text report from CDISC comparison results.
#' Displays the unified comparison table (attributes + values), followed by
#' CDISC validation information.
#'
#' @param cdisc_results List from [cdisc_compare()].
#'
#' @return Character string containing the formatted text report.
#'
#' @keywords internal
generate_text_report <- function(cdisc_results) {
  lines <- character()

  # Determine domain and standard for title
  domain <- if (!is.null(cdisc_results$domain)) cdisc_results$domain else "UNKNOWN"
  standard <- if (!is.null(cdisc_results$standard)) cdisc_results$standard else "CDISC"

  # Title
  lines <- c(lines, "")
  lines <- c(lines, paste0("=", strrep("=", 77)))
  lines <- c(lines, sprintf("  clinCompare - %s %s Domain Comparison Report", standard, domain))
  lines <- c(lines, paste0("=", strrep("=", 77)))

  # CDISC Version note from TS domain (if available)
  if (!is.null(cdisc_results$cdisc_version) &&
      nzchar(cdisc_results$cdisc_version$version_note)) {
    lines <- c(lines, sprintf("  %s", cdisc_results$cdisc_version$version_note))
  }
  lines <- c(lines, "")

  # DATA SET SUMMARY
  lines <- c(lines, "  DATA SET SUMMARY")
  lines <- c(lines, paste0("  ", strrep("-", 50)))

  df1_vars <- if (!is.null(cdisc_results$ncol_df1)) cdisc_results$ncol_df1 else 0
  df1_obs  <- if (!is.null(cdisc_results$nrow_df1)) cdisc_results$nrow_df1 else 0
  df2_vars <- if (!is.null(cdisc_results$ncol_df2)) cdisc_results$ncol_df2 else 0
  df2_obs  <- if (!is.null(cdisc_results$nrow_df2)) cdisc_results$nrow_df2 else 0

  lines <- c(lines, "  Dataset              Variables  Observations")
  lines <- c(lines, paste0("  ", strrep("-", 50)))
  lines <- c(lines, sprintf("  Base (df1)               %3d           %3d", df1_vars, df1_obs))
  lines <- c(lines, sprintf("  Compare (df2)            %3d           %3d", df2_vars, df2_obs))
  lines <- c(lines, paste0("  ", strrep("-", 50)))
  lines <- c(lines, "")

  # VARIABLES SUMMARY
  lines <- c(lines, "  VARIABLES SUMMARY")
  lines <- c(lines, paste0("  -", strrep("-", 45)))

  common_count <- 0
  extra_in_df1 <- character()
  extra_in_df2 <- character()

  if (!is.null(cdisc_results$variable_comparison)) {
    var_comp <- cdisc_results$variable_comparison
    if (is.list(var_comp) && !is.null(var_comp$details)) {
      details <- var_comp$details
      common_count <- if (!is.null(details$common_columns)) length(details$common_columns) else 0
      extra_in_df1 <- if (!is.null(details$extra_in_df1)) details$extra_in_df1 else character()
      extra_in_df2 <- if (!is.null(details$extra_in_df2)) details$extra_in_df2 else character()
    }
  }

  lines <- c(lines, sprintf("  Common variables:          %d", common_count))
  lines <- c(lines, sprintf("  Variables only in Base:     %d", length(extra_in_df1)))
  if (length(extra_in_df1) > 0) {
    lines <- c(lines, sprintf("    %s", paste(extra_in_df1, collapse = ", ")))
  }
  lines <- c(lines, sprintf("  Variables only in Compare:  %d", length(extra_in_df2)))
  if (length(extra_in_df2) > 0) {
    lines <- c(lines, sprintf("    %s", paste(extra_in_df2, collapse = ", ")))
  }
  lines <- c(lines, paste0("  -", strrep("-", 45)))
  lines <- c(lines, "")

  # ID VARIABLES (if used)
  if (!is.null(cdisc_results$id_vars)) {
    lines <- c(lines, sprintf("  ID Variables (keys): %s",
      paste(cdisc_results$id_vars, collapse = ", ")))
    lines <- c(lines, "")
  }

  # UNIFIED COMPARISON TABLE (attribute + value differences combined)
  unified <- cdisc_results$unified_comparison
  meta <- cdisc_results$metadata_comparison
  if (!is.null(unified) && nrow(unified) > 0) {
    lines <- c(lines, "  COMPARISON DETAILS (Attributes + Values)")
    lines <- c(lines, paste0("  ", strrep("=", 74)))
    lines <- c(lines, sprintf("  %-15s %-8s %-18s %-17s %-17s",
      "Variable", "Type", "Row / Key", "Base", "Compare"))
    lines <- c(lines, paste0("  ", strrep("-", 74)))

    for (i in seq_len(nrow(unified))) {
      var_name <- unified$variable[i]
      dtype <- unified$diff_type[i]
      rk <- unified$row_or_key[i]
      bv <- unified$base_value[i]
      cv <- unified$compare_value[i]
      # Truncate long values for text display
      if (nchar(bv) > 17) bv <- paste0(substr(bv, 1, 14), "...")
      if (nchar(cv) > 17) cv <- paste0(substr(cv, 1, 14), "...")
      if (nchar(rk) > 18) rk <- paste0(substr(rk, 1, 15), "...")
      lines <- c(lines, sprintf("  %-15s %-8s %-18s %-17s %-17s",
        var_name, dtype, rk, bv, cv))
    }

    lines <- c(lines, paste0("  ", strrep("-", 74)))

    # Summary counts — check if observation comparison was skipped
    n_attr <- sum(unified$diff_type != "Value")
    n_val <- sum(unified$diff_type == "Value")
    obs_comp_tmp <- cdisc_results$observation_comparison
    obs_was_skipped <- !is.null(obs_comp_tmp) && is.list(obs_comp_tmp) &&
      (!is.null(obs_comp_tmp$status) || !is.null(obs_comp_tmp$message))
    if (obs_was_skipped && n_val == 0) {
      lines <- c(lines, sprintf(
        "  Summary: %d attribute difference(s); value comparison not performed (see note below)",
        n_attr))
    } else {
      lines <- c(lines, sprintf("  Summary: %d attribute difference(s), %d value difference(s)",
        n_attr, n_val))
    }
    lines <- c(lines, paste0("  ", strrep("=", 74)))
    lines <- c(lines, "")
  } else {
    lines <- c(lines, "  COMPARISON DETAILS (Attributes + Values)")
    lines <- c(lines, paste0("  ", strrep("=", 74)))
    lines <- c(lines, "  No attribute or value differences found. Datasets match.")
    lines <- c(lines, paste0("  ", strrep("=", 74)))
    lines <- c(lines, "")
  }

  # Column order note (from metadata)
  if (!is.null(meta)) {
    if (!meta$order_match) {
      lines <- c(lines, "  COLUMN ORDERING: DIFFERS between Base and Compare")
      lines <- c(lines, sprintf("    Base:    %s", paste(meta$order_df1, collapse = ", ")))
      lines <- c(lines, sprintf("    Compare: %s", paste(meta$order_df2, collapse = ", ")))
      lines <- c(lines, "")
    }
  }

  # UNMATCHED ROWS (when id_vars are used)
  unmatched <- cdisc_results$unmatched_rows
  if (!is.null(unmatched)) {
    has_unmatched <- (!is.null(unmatched$df1_only) && nrow(unmatched$df1_only) > 0) ||
                     (!is.null(unmatched$df2_only) && nrow(unmatched$df2_only) > 0)
    if (has_unmatched) {
      lines <- c(lines, "  UNMATCHED ROWS")
      lines <- c(lines, paste0("  -", strrep("-", 59)))
      if (!is.null(unmatched$df1_only) && nrow(unmatched$df1_only) > 0) {
        lines <- c(lines, sprintf("  Rows only in Base: %d", nrow(unmatched$df1_only)))
        id_vars_used <- cdisc_results$id_vars
        if (!is.null(id_vars_used)) {
          for (r in seq_len(min(nrow(unmatched$df1_only), 10))) {
            key_str <- paste(vapply(id_vars_used, function(v) {
              paste0(v, "=", as.character(unmatched$df1_only[[v]][r]))
            }, character(1)), collapse = ", ")
            lines <- c(lines, sprintf("    %s", key_str))
          }
          if (nrow(unmatched$df1_only) > 10) {
            lines <- c(lines, sprintf("    ... and %d more",
              nrow(unmatched$df1_only) - 10))
          }
        }
      }
      if (!is.null(unmatched$df2_only) && nrow(unmatched$df2_only) > 0) {
        lines <- c(lines, sprintf("  Rows only in Compare: %d", nrow(unmatched$df2_only)))
        id_vars_used <- cdisc_results$id_vars
        if (!is.null(id_vars_used)) {
          for (r in seq_len(min(nrow(unmatched$df2_only), 10))) {
            key_str <- paste(vapply(id_vars_used, function(v) {
              paste0(v, "=", as.character(unmatched$df2_only[[v]][r]))
            }, character(1)), collapse = ", ")
            lines <- c(lines, sprintf("    %s", key_str))
          }
          if (nrow(unmatched$df2_only) > 10) {
            lines <- c(lines, sprintf("    ... and %d more",
              nrow(unmatched$df2_only) - 10))
          }
        }
      }
      lines <- c(lines, paste0("  -", strrep("-", 59)))
      lines <- c(lines, "")
    }
  }

  # Observation-level note when comparison was skipped (no id_vars, unequal rows)
  obs_comp <- cdisc_results$observation_comparison
  obs_has_note <- !is.null(obs_comp) && is.list(obs_comp) &&
    (!is.null(obs_comp$message) ||
     (!is.null(obs_comp$status) && obs_comp$status %in% c("Error", "Skipped")))
  if (obs_has_note && !is.null(obs_comp$message)) {
    lines <- c(lines, sprintf("  NOTE: %s", obs_comp$message))
    lines <- c(lines, "")
  }

  # CDISC VALIDATION SECTION
  lines <- c(lines, "  CDISC VALIDATION SECTION")
  lines <- c(lines, "")

  # CDISC Validation for df1
  val_df1 <- cdisc_results$cdisc_validation_df1
  lines <- c(lines, sprintf("  CDISC %s VALIDATION - Base Dataset (df1)", standard))
  lines <- c(lines, paste0("  -", strrep("-", 45)))
  lines <- c(lines, format_validation_summary(val_df1))
  lines <- c(lines, "")

  # CDISC Validation for df2
  val_df2 <- cdisc_results$cdisc_validation_df2
  lines <- c(lines, sprintf("  CDISC %s VALIDATION - Compare Dataset (df2)", standard))
  lines <- c(lines, paste0("  -", strrep("-", 45)))
  lines <- c(lines, format_validation_summary(val_df2))
  lines <- c(lines, "")

  # CDISC CONFORMANCE COMPARISON
  lines <- c(lines, "  CDISC CONFORMANCE COMPARISON")
  lines <- c(lines, paste0("  -", strrep("-", 45)))

  conform <- cdisc_results$cdisc_conformance_comparison
  if (!is.null(conform) && nrow(conform) > 0) {
    df1_only_count <- sum(conform$df1_only)
    df2_only_count <- sum(conform$df2_only)
    both_count <- sum(conform$both)

    lines <- c(lines, sprintf("  Issues unique to Base: %d", df1_only_count))
    if (df1_only_count > 0) {
      df1_issues <- conform[conform$df1_only, ]
      for (i in seq_len(nrow(df1_issues))) {
        lines <- c(lines, sprintf(
          "    %s - %s",
          df1_issues$variable[i],
          df1_issues$category[i]
        ))
      }
    }
    lines <- c(lines, "")

    lines <- c(lines, sprintf("  Issues unique to Compare: %d", df2_only_count))
    if (df2_only_count > 0) {
      df2_issues <- conform[conform$df2_only, ]
      for (i in seq_len(nrow(df2_issues))) {
        lines <- c(lines, sprintf(
          "    %s - %s",
          df2_issues$variable[i],
          df2_issues$category[i]
        ))
      }
    }
    lines <- c(lines, "")

    lines <- c(lines, sprintf("  Issues common to both: %d", both_count))
  } else {
    lines <- c(lines, "  No CDISC issues found for comparison.")
  }

  lines <- c(lines, "")
  lines <- c(lines, paste0("=", strrep("=", 77)))
  lines <- c(lines, "")

  return(paste(lines, collapse = "\n"))
}


#' Format Validation Summary
#'
#' @description
#' Internal function to format validation results as text. Skips verbose INFO
#' items and focuses on errors and warnings, plus non-standard variable info.
#'
#' @param validation_df Validation results data frame.
#'
#' @return Character vector of formatted lines.
#'
#' @keywords internal
format_validation_summary <- function(validation_df) {
  lines <- character()

  if (is.null(validation_df) || nrow(validation_df) == 0) {
    lines <- c(lines, "  No validation issues found.")
    return(lines)
  }

  # Count by severity (for summary only)
  severity_counts <- table(validation_df$severity)
  error_count <- if ("ERROR" %in% names(severity_counts)) severity_counts[["ERROR"]] else 0L
  warning_count <- if ("WARNING" %in% names(severity_counts)) severity_counts[["WARNING"]] else 0L
  info_count <- if ("INFO" %in% names(severity_counts)) severity_counts[["INFO"]] else 0L

  lines <- c(lines, sprintf(
    "  Errors: %d  |  Warnings: %d  |  Info: %d",
    error_count, warning_count, info_count
  ))
  lines <- c(lines, "")

  # Only show Errors and Warnings, skip verbose Variable Info category items
  for_display <- validation_df[validation_df$severity %in% c("ERROR", "WARNING"), ]

  if (nrow(for_display) > 0) {
    # Group Errors and Warnings separately
    for (severity in c("ERROR", "WARNING")) {
      sev_rows <- for_display[for_display$severity == severity, ]
      if (nrow(sev_rows) > 0) {
        sev_label <- if (severity == "ERROR") "Errors" else "Warnings"
        lines <- c(lines, sprintf("  %s:", sev_label))

        for (i in seq_len(nrow(sev_rows))) {
          lines <- c(lines, sprintf(
            "    %s - %s",
            sev_rows$variable[i],
            sev_rows$message[i]
          ))
        }
        lines <- c(lines, "")
      }
    }
  }

  return(lines)
}


#' Generate HTML Report
#'
#' @description
#' Internal function to generate a self-contained HTML report with styling.
#' Report shows the unified comparison table, CDISC validation, and an interactive dashboard.
#'
#' @param cdisc_results List from [cdisc_compare()].
#'
#' @return Character string containing the HTML report.
#'
#' @keywords internal
generate_html_report <- function(cdisc_results) {
  html_lines <- character()

  # Determine domain and standard for title
  domain <- if (!is.null(cdisc_results$domain)) cdisc_results$domain else "UNKNOWN"
  standard <- if (!is.null(cdisc_results$standard)) cdisc_results$standard else "CDISC"

  # HTML header with styles
  html_lines <- c(html_lines, "<!DOCTYPE html>")
  html_lines <- c(html_lines, "<html>")
  html_lines <- c(html_lines, "<head>")
  html_lines <- c(html_lines, '  <meta charset="UTF-8">')
  html_lines <- c(html_lines, sprintf("  <title>clinCompare - %s %s Domain Report</title>", standard, domain))
  html_lines <- c(html_lines, "  <style>")
  html_lines <- c(html_lines, "    body {")
  html_lines <- c(html_lines, "      font-family: 'Courier New', monospace;")
  html_lines <- c(html_lines, "      margin: 20px;")
  html_lines <- c(html_lines, "      background-color: #f5f5f5;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .container {")
  html_lines <- c(html_lines, "      max-width: 1200px;")
  html_lines <- c(html_lines, "      margin: 0 auto;")
  html_lines <- c(html_lines, "      background-color: white;")
  html_lines <- c(html_lines, "      padding: 30px;")
  html_lines <- c(html_lines, "      border-radius: 8px;")
  html_lines <- c(html_lines, "      box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    h1 {")
  html_lines <- c(html_lines, "      color: #333;")
  html_lines <- c(html_lines, "      border-bottom: 3px solid #0066cc;")
  html_lines <- c(html_lines, "      padding-bottom: 15px;")
  html_lines <- c(html_lines, "      text-align: center;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    h2 {")
  html_lines <- c(html_lines, "      color: #0066cc;")
  html_lines <- c(html_lines, "      margin-top: 35px;")
  html_lines <- c(html_lines, "      border-left: 5px solid #0066cc;")
  html_lines <- c(html_lines, "      padding-left: 12px;")
  html_lines <- c(html_lines, "      font-size: 1.3em;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    table {")
  html_lines <- c(html_lines, "      width: 100%;")
  html_lines <- c(html_lines, "      border-collapse: collapse;")
  html_lines <- c(html_lines, "      margin: 20px 0;")
  html_lines <- c(html_lines, "      box-shadow: 0 1px 3px rgba(0,0,0,0.1);")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    th {")
  html_lines <- c(html_lines, "      background-color: #004d99;")
  html_lines <- c(html_lines, "      color: white;")
  html_lines <- c(html_lines, "      padding: 14px;")
  html_lines <- c(html_lines, "      text-align: left;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "      border-bottom: 2px solid #003366;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    td {")
  html_lines <- c(html_lines, "      padding: 12px 14px;")
  html_lines <- c(html_lines, "      border-bottom: 1px solid #ddd;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    tr:nth-child(even) {")
  html_lines <- c(html_lines, "      background-color: #f9f9f9;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    tr:hover {")
  html_lines <- c(html_lines, "      background-color: #f0f5ff;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .severity-ERROR {")
  html_lines <- c(html_lines, "      background-color: #ffe6e6;")
  html_lines <- c(html_lines, "      color: #cc0000;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .severity-WARNING {")
  html_lines <- c(html_lines, "      background-color: #fff4e6;")
  html_lines <- c(html_lines, "      color: #ff8800;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .severity-INFO {")
  html_lines <- c(html_lines, "      background-color: #e6f2ff;")
  html_lines <- c(html_lines, "      color: #0066cc;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .summary-box {")
  html_lines <- c(html_lines, "      background-color: #f0f8ff;")
  html_lines <- c(html_lines, "      border-left: 5px solid #0066cc;")
  html_lines <- c(html_lines, "      padding: 15px;")
  html_lines <- c(html_lines, "      margin: 20px 0;")
  html_lines <- c(html_lines, "      line-height: 1.6;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .summary-header {")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "      color: #004d99;")
  html_lines <- c(html_lines, "      margin-bottom: 8px;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .diff-highlight {")
  html_lines <- c(html_lines, "      background-color: #fff9e6;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .chart-container {")
  html_lines <- c(html_lines, "      max-width: 600px;")
  html_lines <- c(html_lines, "      margin: 20px auto;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .kpi-row {")
  html_lines <- c(html_lines, "      display: flex;")
  html_lines <- c(html_lines, "      gap: 15px;")
  html_lines <- c(html_lines, "      margin: 20px 0;")
  html_lines <- c(html_lines, "      flex-wrap: wrap;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .kpi-card {")
  html_lines <- c(html_lines, "      flex: 1;")
  html_lines <- c(html_lines, "      min-width: 150px;")
  html_lines <- c(html_lines, "      background: linear-gradient(135deg, #f0f8ff, #e6f2ff);")
  html_lines <- c(html_lines, "      border: 1px solid #cce0ff;")
  html_lines <- c(html_lines, "      border-radius: 8px;")
  html_lines <- c(html_lines, "      padding: 15px;")
  html_lines <- c(html_lines, "      text-align: center;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .kpi-value {")
  html_lines <- c(html_lines, "      font-size: 2em;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "      color: #004d99;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .kpi-label {")
  html_lines <- c(html_lines, "      font-size: 0.85em;")
  html_lines <- c(html_lines, "      color: #666;")
  html_lines <- c(html_lines, "      margin-top: 5px;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .kpi-card.success .kpi-value { color: #27AE60; }")
  html_lines <- c(html_lines, "    .kpi-card.warning .kpi-value { color: #F39C12; }")
  html_lines <- c(html_lines, "    .kpi-card.danger .kpi-value { color: #E74C3C; }")
  html_lines <- c(html_lines, "    .dashboard-grid {")
  html_lines <- c(html_lines, "      display: grid;")
  html_lines <- c(html_lines, "      grid-template-columns: repeat(2, 1fr);")
  html_lines <- c(html_lines, "      gap: 20px;")
  html_lines <- c(html_lines, "      margin: 20px 0;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .chart-box {")
  html_lines <- c(html_lines, "      background: white;")
  html_lines <- c(html_lines, "      border: 1px solid #e0e0e0;")
  html_lines <- c(html_lines, "      border-radius: 8px;")
  html_lines <- c(html_lines, "      padding: 15px;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "  </style>")
  # Inline Chart.js for offline use (no CDN dependency)
  chartjs_path <- system.file("assets", "chart.umd.min.js", package = "clinCompare")
  if (nzchar(chartjs_path) && file.exists(chartjs_path)) {
    chartjs_src <- paste(readLines(chartjs_path, warn = FALSE), collapse = "\n")
    html_lines <- c(html_lines, "  <script>", chartjs_src, "  </script>")
  } else {
    # Fallback to CDN if bundled file not found (e.g., during development)
    html_lines <- c(html_lines, '  <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.min.js"></script>')
  }
  html_lines <- c(html_lines, "</head>")
  html_lines <- c(html_lines, "<body>")
  html_lines <- c(html_lines, '<div class="container">')

  # Title with domain and standard
  html_lines <- c(html_lines, sprintf("<h1>clinCompare - %s %s Domain Comparison Report</h1>", standard, domain))

  # CDISC Version note from TS domain (if available)
  if (!is.null(cdisc_results$cdisc_version) &&
      nzchar(cdisc_results$cdisc_version$version_note)) {
    html_lines <- c(html_lines,
      '<div style="text-align:center;margin:-10px 0 15px 0;color:#555;font-size:0.95em;">',
      sprintf("  <em>%s</em>", cdisc_results$cdisc_version$version_note),
      "</div>"
    )
  }

  # DATA SET SUMMARY
  html_lines <- c(html_lines, "<h2>Data Set Summary</h2>")
  html_lines <- c(html_lines, '<div class="summary-box">')

  common_count <- 0
  extra_in_df1 <- character()
  extra_in_df2 <- character()

  if (!is.null(cdisc_results$variable_comparison)) {
    var_comp <- cdisc_results$variable_comparison
    if (is.list(var_comp) && !is.null(var_comp$details)) {
      details <- var_comp$details
      common_count <- if (!is.null(details$common_columns)) length(details$common_columns) else 0
      extra_in_df1 <- if (!is.null(details$extra_in_df1)) details$extra_in_df1 else character()
      extra_in_df2 <- if (!is.null(details$extra_in_df2)) details$extra_in_df2 else character()
    }
  }

  df1_vars <- if (!is.null(cdisc_results$ncol_df1)) cdisc_results$ncol_df1 else "-"
  df1_obs  <- if (!is.null(cdisc_results$nrow_df1)) cdisc_results$nrow_df1 else "-"
  df2_vars <- if (!is.null(cdisc_results$ncol_df2)) cdisc_results$ncol_df2 else "-"
  df2_obs  <- if (!is.null(cdisc_results$nrow_df2)) cdisc_results$nrow_df2 else "-"

  html_lines <- c(html_lines, "<table>")
  html_lines <- c(html_lines, "<tr><th>Dataset</th><th>Variables</th><th>Observations</th></tr>")
  html_lines <- c(html_lines, sprintf("<tr><td><strong>Base (df1)</strong></td><td>%s</td><td>%s</td></tr>", df1_vars, df1_obs))
  html_lines <- c(html_lines, sprintf("<tr><td><strong>Compare (df2)</strong></td><td>%s</td><td>%s</td></tr>", df2_vars, df2_obs))
  html_lines <- c(html_lines, "</table>")

  html_lines <- c(html_lines, "<div class='summary-header'>Variables Summary:</div>")
  html_lines <- c(html_lines, sprintf("<p>Common variables: <strong>%d</strong></p>", common_count))
  if (length(extra_in_df1) > 0) {
    html_lines <- c(html_lines, sprintf(
      "<p>Variables only in Base: <strong>%d</strong><br/>%s</p>",
      length(extra_in_df1),
      paste(extra_in_df1, collapse = ", ")
    ))
  } else {
    html_lines <- c(html_lines, "<p>Variables only in Base: <strong>0</strong></p>")
  }
  if (length(extra_in_df2) > 0) {
    html_lines <- c(html_lines, sprintf(
      "<p>Variables only in Compare: <strong>%d</strong><br/>%s</p>",
      length(extra_in_df2),
      paste(extra_in_df2, collapse = ", ")
    ))
  } else {
    html_lines <- c(html_lines, "<p>Variables only in Compare: <strong>0</strong></p>")
  }

  html_lines <- c(html_lines, "</div>")

  # ID Variables (if used)
  if (!is.null(cdisc_results$id_vars)) {
    html_lines <- c(html_lines, '<div class="summary-box">')
    html_lines <- c(html_lines, sprintf(
      "<p><strong>ID Variables (keys):</strong> %s</p>",
      paste(cdisc_results$id_vars, collapse = ", ")
    ))
    html_lines <- c(html_lines, "</div>")
  }

  # UNIFIED COMPARISON TABLE (attributes + values combined)
  unified <- cdisc_results$unified_comparison
  meta <- cdisc_results$metadata_comparison

  html_lines <- c(html_lines, "<h2>Comparison Details (Attributes + Values)</h2>")

  if (!is.null(unified) && nrow(unified) > 0) {
    # Color-code diff_type with CSS classes
    html_lines <- c(html_lines, "<style>")
    html_lines <- c(html_lines, "  .diff-type-Type { background-color: #e6f3ff; }")
    html_lines <- c(html_lines, "  .diff-type-Label { background-color: #fff8e6; }")
    html_lines <- c(html_lines, "  .diff-type-Length { background-color: #ffe6e6; }")
    html_lines <- c(html_lines, "  .diff-type-Format { background-color: #e6fff2; }")
    html_lines <- c(html_lines, "  .diff-type-Value { background-color: #fff9e6; font-weight: bold; }")
    html_lines <- c(html_lines, "  .unified-badge {")
    html_lines <- c(html_lines, "    display: inline-block; padding: 2px 8px; border-radius: 4px;")
    html_lines <- c(html_lines, "    font-size: 0.85em; font-weight: bold; color: white;")
    html_lines <- c(html_lines, "  }")
    html_lines <- c(html_lines, "  .badge-Type { background-color: #3498DB; }")
    html_lines <- c(html_lines, "  .badge-Label { background-color: #F39C12; }")
    html_lines <- c(html_lines, "  .badge-Length { background-color: #E74C3C; }")
    html_lines <- c(html_lines, "  .badge-Format { background-color: #1ABC9C; }")
    html_lines <- c(html_lines, "  .badge-Value { background-color: #9B59B6; }")
    html_lines <- c(html_lines, "</style>")

    html_lines <- c(html_lines, "<table>")
    html_lines <- c(html_lines, "<tr>")
    html_lines <- c(html_lines, "<th>Variable</th>")
    html_lines <- c(html_lines, "<th>Difference</th>")
    html_lines <- c(html_lines, "<th>Row / Key</th>")
    html_lines <- c(html_lines, "<th>Base</th>")
    html_lines <- c(html_lines, "<th>Compare</th>")
    html_lines <- c(html_lines, "</tr>")

    for (i in seq_len(nrow(unified))) {
      dtype <- unified$diff_type[i]
      row_class <- sprintf("diff-type-%s", dtype)
      badge_class <- sprintf("badge-%s", dtype)
      html_lines <- c(html_lines, sprintf('<tr class="%s">', row_class))
      html_lines <- c(html_lines, sprintf("<td><strong>%s</strong></td>", unified$variable[i]))
      html_lines <- c(html_lines, sprintf(
        '<td><span class="unified-badge %s">%s</span></td>', badge_class, dtype))
      html_lines <- c(html_lines, sprintf("<td>%s</td>", unified$row_or_key[i]))
      html_lines <- c(html_lines, sprintf("<td>%s</td>", unified$base_value[i]))
      html_lines <- c(html_lines, sprintf("<td>%s</td>", unified$compare_value[i]))
      html_lines <- c(html_lines, "</tr>")
    }

    html_lines <- c(html_lines, "</table>")

    # Summary counts — check if observation comparison was skipped
    n_attr <- sum(unified$diff_type != "Value")
    n_val <- sum(unified$diff_type == "Value")
    obs_comp_tmp <- cdisc_results$observation_comparison
    obs_was_skipped <- !is.null(obs_comp_tmp) && is.list(obs_comp_tmp) &&
      (!is.null(obs_comp_tmp$status) || !is.null(obs_comp_tmp$message))
    html_lines <- c(html_lines, '<div class="summary-box">')
    if (obs_was_skipped && n_val == 0) {
      html_lines <- c(html_lines, sprintf(
        "<p><strong>Summary:</strong> %d attribute difference(s); value comparison not performed (see note below)</p>",
        n_attr
      ))
    } else {
      html_lines <- c(html_lines, sprintf(
        "<p><strong>Summary:</strong> %d attribute difference(s), %d value difference(s)</p>",
        n_attr, n_val
      ))
    }
    html_lines <- c(html_lines, "</div>")
  } else {
    html_lines <- c(html_lines, '<div class="summary-box">')
    html_lines <- c(html_lines, "<p>No attribute or value differences found. Datasets match.</p>")
    html_lines <- c(html_lines, "</div>")
  }

  # Column order note
  if (!is.null(meta)) {
    if (!meta$order_match) {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, "<p><strong>Column Ordering: DIFFERS</strong></p>")
      html_lines <- c(html_lines, sprintf("<p>Base: %s</p>", paste(meta$order_df1, collapse = ", ")))
      html_lines <- c(html_lines, sprintf("<p>Compare: %s</p>", paste(meta$order_df2, collapse = ", ")))
      html_lines <- c(html_lines, "</div>")
    }
  }

  # UNMATCHED ROWS (when id_vars are used)
  unmatched <- cdisc_results$unmatched_rows
  if (!is.null(unmatched)) {
    has_unmatched <- (!is.null(unmatched$df1_only) && nrow(unmatched$df1_only) > 0) ||
                     (!is.null(unmatched$df2_only) && nrow(unmatched$df2_only) > 0)
    if (has_unmatched) {
      html_lines <- c(html_lines, "<h2>Unmatched Rows</h2>")
      id_vars_used <- cdisc_results$id_vars
      if (!is.null(unmatched$df1_only) && nrow(unmatched$df1_only) > 0) {
        html_lines <- c(html_lines, sprintf(
          "<h3>Rows Only in Base (%d)</h3>", nrow(unmatched$df1_only)))
        html_lines <- c(html_lines, "<table>")
        # Header: ID columns only
        html_lines <- c(html_lines, "<tr>")
        if (!is.null(id_vars_used)) {
          for (v in id_vars_used) {
            html_lines <- c(html_lines, sprintf("<th>%s</th>", v))
          }
        }
        html_lines <- c(html_lines, "</tr>")
        show_n <- min(nrow(unmatched$df1_only), 20)
        for (r in seq_len(show_n)) {
          html_lines <- c(html_lines, '<tr class="severity-WARNING">')
          for (v in id_vars_used) {
            html_lines <- c(html_lines, sprintf("<td>%s</td>",
              as.character(unmatched$df1_only[[v]][r])))
          }
          html_lines <- c(html_lines, "</tr>")
        }
        html_lines <- c(html_lines, "</table>")
        if (nrow(unmatched$df1_only) > 20) {
          html_lines <- c(html_lines, sprintf(
            "<p><em>... and %d more rows</em></p>",
            nrow(unmatched$df1_only) - 20))
        }
      }
      if (!is.null(unmatched$df2_only) && nrow(unmatched$df2_only) > 0) {
        html_lines <- c(html_lines, sprintf(
          "<h3>Rows Only in Compare (%d)</h3>", nrow(unmatched$df2_only)))
        html_lines <- c(html_lines, "<table>")
        html_lines <- c(html_lines, "<tr>")
        if (!is.null(id_vars_used)) {
          for (v in id_vars_used) {
            html_lines <- c(html_lines, sprintf("<th>%s</th>", v))
          }
        }
        html_lines <- c(html_lines, "</tr>")
        show_n <- min(nrow(unmatched$df2_only), 20)
        for (r in seq_len(show_n)) {
          html_lines <- c(html_lines, '<tr class="severity-WARNING">')
          for (v in id_vars_used) {
            html_lines <- c(html_lines, sprintf("<td>%s</td>",
              as.character(unmatched$df2_only[[v]][r])))
          }
          html_lines <- c(html_lines, "</tr>")
        }
        html_lines <- c(html_lines, "</table>")
        if (nrow(unmatched$df2_only) > 20) {
          html_lines <- c(html_lines, sprintf(
            "<p><em>... and %d more rows</em></p>",
            nrow(unmatched$df2_only) - 20))
        }
      }
    }
  }

  # Observation-level note when comparison was skipped
  obs_comp <- cdisc_results$observation_comparison
  obs_has_note <- !is.null(obs_comp) && is.list(obs_comp) &&
    (!is.null(obs_comp$message) ||
     (!is.null(obs_comp$status) && obs_comp$status %in% c("Error", "Skipped")))
  if (obs_has_note && !is.null(obs_comp$message)) {
    html_lines <- c(html_lines, '<div class="summary-box">')
    html_lines <- c(html_lines, sprintf("<p><strong>NOTE:</strong> %s</p>", obs_comp$message))
    html_lines <- c(html_lines, "</div>")
  }

  # CDISC VALIDATION SECTION
  html_lines <- c(html_lines, sprintf("<h2>CDISC %s Validation Section</h2>", standard))

  # CDISC Validation for df1
  val_df1 <- cdisc_results$cdisc_validation_df1
  html_lines <- c(html_lines, sprintf("<h2>CDISC %s Validation - Base Dataset (df1)</h2>", standard))
  html_lines <- c(html_lines, format_validation_html(val_df1))

  # CDISC Validation for df2
  val_df2 <- cdisc_results$cdisc_validation_df2
  html_lines <- c(html_lines, sprintf("<h2>CDISC %s Validation - Compare Dataset (df2)</h2>", standard))
  html_lines <- c(html_lines, format_validation_html(val_df2))

  # CDISC CONFORMANCE COMPARISON
  html_lines <- c(html_lines, "<h2>CDISC Conformance Comparison</h2>")

  conform <- cdisc_results$cdisc_conformance_comparison
  if (!is.null(conform) && nrow(conform) > 0) {
    html_lines <- c(html_lines, "<table>")
    html_lines <- c(html_lines, "<tr>")
    html_lines <- c(html_lines, "<th>Variable</th>")
    html_lines <- c(html_lines, "<th>Category</th>")
    html_lines <- c(html_lines, "<th>In Base Only</th>")
    html_lines <- c(html_lines, "<th>In Compare Only</th>")
    html_lines <- c(html_lines, "<th>In Both</th>")
    html_lines <- c(html_lines, "</tr>")

    for (i in seq_len(nrow(conform))) {
      html_lines <- c(html_lines, "<tr>")
      html_lines <- c(html_lines, sprintf("<td>%s</td>", conform$variable[i]))
      html_lines <- c(html_lines, sprintf("<td>%s</td>", conform$category[i]))
      html_lines <- c(html_lines, sprintf(
        "<td>%s</td>",
        if (conform$df1_only[i]) "Yes" else "No"
      ))
      html_lines <- c(html_lines, sprintf(
        "<td>%s</td>",
        if (conform$df2_only[i]) "Yes" else "No"
      ))
      html_lines <- c(html_lines, sprintf(
        "<td>%s</td>",
        if (conform$both[i]) "Yes" else "No"
      ))
      html_lines <- c(html_lines, "</tr>")
    }

    html_lines <- c(html_lines, "</table>")
  } else {
    html_lines <- c(html_lines, '<div class="summary-box">')
    html_lines <- c(html_lines, "<p>No CDISC issues found for comparison.</p>")
    html_lines <- c(html_lines, "</div>")
  }

  # VISUALIZATIONS - Dashboard with KPI cards and 4 charts
  html_lines <- c(html_lines, "<h2>Comparison Dashboard</h2>")

  # --- Compute metrics for KPI cards ---
  kpi_total_vars <- common_count
  kpi_total_diffs <- 0
  kpi_diffs_vars <- 0
  kpi_meta_issues <- 0

  obs_comp_chart <- cdisc_results$observation_comparison
  has_diffs <- (!is.null(obs_comp_chart) && is.list(obs_comp_chart) &&
                !is.null(obs_comp_chart$details) && is.list(obs_comp_chart$details) &&
                length(obs_comp_chart$details) > 0)

  if (has_diffs) {
    for (vn in names(obs_comp_chart$details)) {
      d <- obs_comp_chart$details[[vn]]
      if (is.data.frame(d)) {
        kpi_total_diffs <- kpi_total_diffs + nrow(d)
        if (nrow(d) > 0) kpi_diffs_vars <- kpi_diffs_vars + 1
      }
    }
  }

  meta_chart <- cdisc_results$metadata_comparison
  if (!is.null(meta_chart)) {
    kpi_meta_issues <- nrow(meta_chart$type_mismatches) + nrow(meta_chart$label_mismatches)
    if (!is.null(meta_chart$length_mismatches)) kpi_meta_issues <- kpi_meta_issues + nrow(meta_chart$length_mismatches)
    if (!is.null(meta_chart$format_mismatches)) kpi_meta_issues <- kpi_meta_issues + nrow(meta_chart$format_mismatches)
    if (!meta_chart$order_match) kpi_meta_issues <- kpi_meta_issues + 1
  }

  # Total cells compared for match rate
  nrow_common <- min(
    if (!is.null(cdisc_results$nrow_df1)) cdisc_results$nrow_df1 else 0,
    if (!is.null(cdisc_results$nrow_df2)) cdisc_results$nrow_df2 else 0
  )
  total_cells <- kpi_total_vars * nrow_common
  match_rate <- if (total_cells > 0) {
    round((total_cells - kpi_total_diffs) / total_cells * 100, 1)
  } else {
    100
  }

  # KPI card color classes
  match_class <- if (match_rate >= 99) "success" else if (match_rate >= 95) "warning" else "danger"
  diff_class <- if (kpi_total_diffs == 0) "success" else if (kpi_total_diffs <= 5) "warning" else "danger"
  meta_class <- if (kpi_meta_issues == 0) "success" else if (kpi_meta_issues <= 2) "warning" else "danger"

  # --- KPI Score Cards ---
  html_lines <- c(html_lines, '<div class="kpi-row">')
  html_lines <- c(html_lines, '<div class="kpi-card">')
  html_lines <- c(html_lines, sprintf('<div class="kpi-value">%d</div>', kpi_total_vars))
  html_lines <- c(html_lines, '<div class="kpi-label">Variables Compared</div>')
  html_lines <- c(html_lines, '</div>')
  html_lines <- c(html_lines, sprintf('<div class="kpi-card %s">', match_class))
  html_lines <- c(html_lines, sprintf('<div class="kpi-value">%s%%</div>', match_rate))
  html_lines <- c(html_lines, '<div class="kpi-label">Cell Match Rate</div>')
  html_lines <- c(html_lines, '</div>')
  html_lines <- c(html_lines, sprintf('<div class="kpi-card %s">', diff_class))
  html_lines <- c(html_lines, sprintf('<div class="kpi-value">%d</div>', kpi_total_diffs))
  html_lines <- c(html_lines, '<div class="kpi-label">Value Differences</div>')
  html_lines <- c(html_lines, '</div>')
  html_lines <- c(html_lines, sprintf('<div class="kpi-card %s">', meta_class))
  html_lines <- c(html_lines, sprintf('<div class="kpi-value">%d</div>', kpi_meta_issues))
  html_lines <- c(html_lines, '<div class="kpi-label">Metadata Issues</div>')
  html_lines <- c(html_lines, '</div>')
  html_lines <- c(html_lines, '</div>')

  # --- 2x2 Chart Dashboard Grid ---
  html_lines <- c(html_lines, '<div class="dashboard-grid">')

  # Canvas for Chart 1: Overall Match Rate (Doughnut)
  html_lines <- c(html_lines, '<div class="chart-box">')
  html_lines <- c(html_lines, '<canvas id="matchChart"></canvas>')
  html_lines <- c(html_lines, '</div>')

  # Canvas for Chart 2: Value Discrepancies by Variable (Horizontal Bar)
  html_lines <- c(html_lines, '<div class="chart-box">')
  html_lines <- c(html_lines, '<canvas id="discChart"></canvas>')
  html_lines <- c(html_lines, '</div>')

  # Canvas for Chart 3: Metadata Issues Breakdown
  html_lines <- c(html_lines, '<div class="chart-box">')
  html_lines <- c(html_lines, '<canvas id="metaChart"></canvas>')
  html_lines <- c(html_lines, '</div>')

  # Canvas for Chart 4: CDISC Validation Profile (Stacked Bar)
  html_lines <- c(html_lines, '<div class="chart-box">')
  html_lines <- c(html_lines, '<canvas id="cdiscChart"></canvas>')
  html_lines <- c(html_lines, '</div>')

  html_lines <- c(html_lines, '</div>') # close dashboard-grid

  # --- All Chart.js Scripts ---
  html_lines <- c(html_lines, '<script>')

  # Chart 1: Doughnut - Overall Match Rate
  matching_cells <- total_cells - kpi_total_diffs
  html_lines <- c(html_lines, "new Chart(document.getElementById('matchChart'), {")
  html_lines <- c(html_lines, "  type: 'doughnut',")
  html_lines <- c(html_lines, "  data: {")
  html_lines <- c(html_lines, "    labels: ['Matching Cells', 'Mismatching Cells'],")
  html_lines <- c(html_lines, sprintf(
    "    datasets: [{ data: [%d, %d], backgroundColor: ['#27AE60', '#E74C3C'], borderWidth: 2 }]",
    matching_cells, kpi_total_diffs
  ))
  html_lines <- c(html_lines, "  },")
  html_lines <- c(html_lines, "  options: {")
  html_lines <- c(html_lines, "    responsive: true,")
  html_lines <- c(html_lines, "    plugins: {")
  html_lines <- c(html_lines, sprintf(
    "      title: { display: true, text: 'Overall Match Rate (%s%%)', font: { size: 14 } },",
    match_rate
  ))
  html_lines <- c(html_lines, "      legend: { position: 'bottom' }")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "  }")
  html_lines <- c(html_lines, "});")

  # Chart 2: Horizontal Bar - Value Discrepancies by Variable
  if (has_diffs) {
    chart_vars <- names(obs_comp_chart$details)
    chart_counts <- vapply(obs_comp_chart$details, function(d) {
      if (is.data.frame(d)) nrow(d) else 0L
    }, integer(1))
    keep <- chart_counts > 0
    chart_vars <- chart_vars[keep]
    chart_counts <- chart_counts[keep]
    # Sort descending by count
    ord <- order(chart_counts, decreasing = TRUE)
    chart_vars <- chart_vars[ord]
    chart_counts <- chart_counts[ord]
    labels_json <- paste0("[", paste(sprintf("'%s'", chart_vars), collapse = ","), "]")
    data_json   <- paste0("[", paste(chart_counts, collapse = ","), "]")
    # Gradient: darker red for more diffs, lighter for fewer
    n_bars <- length(chart_vars)
    alpha_vals <- round(seq(1, 0.4, length.out = max(n_bars, 1)), 2)
    bar_colors <- paste0("[", paste(sprintf("'rgba(231,76,60,%s)'", alpha_vals), collapse = ","), "]")
  } else {
    labels_json <- "['No Differences']"
    data_json   <- "[0]"
    bar_colors  <- "['#27AE60']"
  }

  html_lines <- c(html_lines, "new Chart(document.getElementById('discChart'), {")
  html_lines <- c(html_lines, "  type: 'bar',")
  html_lines <- c(html_lines, "  data: {")
  html_lines <- c(html_lines, sprintf("    labels: %s,", labels_json))
  html_lines <- c(html_lines, "    datasets: [{")
  html_lines <- c(html_lines, "      label: 'Value Differences',")
  html_lines <- c(html_lines, sprintf("      data: %s,", data_json))
  html_lines <- c(html_lines, sprintf("      backgroundColor: %s,", bar_colors))
  html_lines <- c(html_lines, "      borderRadius: 4")
  html_lines <- c(html_lines, "    }]")
  html_lines <- c(html_lines, "  },")
  html_lines <- c(html_lines, "  options: {")
  html_lines <- c(html_lines, "    indexAxis: 'y',")
  html_lines <- c(html_lines, "    responsive: true,")
  html_lines <- c(html_lines, "    plugins: {")
  html_lines <- c(html_lines, "      title: { display: true, text: 'Value Discrepancies by Variable', font: { size: 14 } },")
  html_lines <- c(html_lines, "      legend: { display: false }")
  html_lines <- c(html_lines, "    },")
  html_lines <- c(html_lines, "    scales: { x: { beginAtZero: true, ticks: { stepSize: 1 } } }")
  html_lines <- c(html_lines, "  }")
  html_lines <- c(html_lines, "});")

  # Chart 3: Metadata Issues Breakdown
  meta_type_n <- if (!is.null(meta_chart)) nrow(meta_chart$type_mismatches) else 0
  meta_label_n <- if (!is.null(meta_chart)) nrow(meta_chart$label_mismatches) else 0
  meta_length_n <- if (!is.null(meta_chart) && !is.null(meta_chart$length_mismatches)) nrow(meta_chart$length_mismatches) else 0
  meta_format_n <- if (!is.null(meta_chart) && !is.null(meta_chart$format_mismatches)) nrow(meta_chart$format_mismatches) else 0
  meta_order_n <- if (!is.null(meta_chart) && !meta_chart$order_match) 1 else 0

  html_lines <- c(html_lines, "new Chart(document.getElementById('metaChart'), {")
  html_lines <- c(html_lines, "  type: 'bar',")
  html_lines <- c(html_lines, "  data: {")
  html_lines <- c(html_lines, "    labels: ['Types', 'Labels', 'Lengths', 'Formats', 'Col Order'],")
  html_lines <- c(html_lines, sprintf(
    "    datasets: [{ label: 'Issues', data: [%d, %d, %d, %d, %d], backgroundColor: ['#3498DB', '#F39C12', '#E74C3C', '#1ABC9C', '#9B59B6'], borderRadius: 4 }]",
    meta_type_n, meta_label_n, meta_length_n, meta_format_n, meta_order_n
  ))
  html_lines <- c(html_lines, "  },")
  html_lines <- c(html_lines, "  options: {")
  html_lines <- c(html_lines, "    responsive: true,")
  html_lines <- c(html_lines, "    plugins: {")
  html_lines <- c(html_lines, "      title: { display: true, text: 'Metadata Issues Breakdown', font: { size: 14 } },")
  html_lines <- c(html_lines, "      legend: { display: false }")
  html_lines <- c(html_lines, "    },")
  html_lines <- c(html_lines, "    scales: { y: { beginAtZero: true, ticks: { stepSize: 1 } } }")
  html_lines <- c(html_lines, "  }")
  html_lines <- c(html_lines, "});")

  # Chart 4: CDISC Validation - Stacked Bar (Base vs Compare)
  val_df1_chart <- cdisc_results$cdisc_validation_df1
  val_df2_chart <- cdisc_results$cdisc_validation_df2

  if (!is.null(val_df1_chart) && !is.null(val_df2_chart)) {
    sev1 <- table(factor(val_df1_chart$severity, levels = c("ERROR", "WARNING", "INFO")))
    sev2 <- table(factor(val_df2_chart$severity, levels = c("ERROR", "WARNING", "INFO")))

    html_lines <- c(html_lines, "new Chart(document.getElementById('cdiscChart'), {")
    html_lines <- c(html_lines, "  type: 'bar',")
    html_lines <- c(html_lines, "  data: {")
    html_lines <- c(html_lines, "    labels: ['Base (df1)', 'Compare (df2)'],")
    html_lines <- c(html_lines, "    datasets: [")
    html_lines <- c(html_lines, sprintf(
      "      { label: 'Errors', data: [%d, %d], backgroundColor: '#E74C3C' },",
      as.integer(sev1["ERROR"]), as.integer(sev2["ERROR"])
    ))
    html_lines <- c(html_lines, sprintf(
      "      { label: 'Warnings', data: [%d, %d], backgroundColor: '#F39C12' },",
      as.integer(sev1["WARNING"]), as.integer(sev2["WARNING"])
    ))
    html_lines <- c(html_lines, sprintf(
      "      { label: 'Info', data: [%d, %d], backgroundColor: '#3498DB' }",
      as.integer(sev1["INFO"]), as.integer(sev2["INFO"])
    ))
    html_lines <- c(html_lines, "    ]")
    html_lines <- c(html_lines, "  },")
    html_lines <- c(html_lines, "  options: {")
    html_lines <- c(html_lines, "    responsive: true,")
    html_lines <- c(html_lines, "    plugins: {")
    html_lines <- c(html_lines, "      title: { display: true, text: 'CDISC Validation: Base vs Compare', font: { size: 14 } }")
    html_lines <- c(html_lines, "    },")
    html_lines <- c(html_lines, "    scales: {")
    html_lines <- c(html_lines, "      x: { stacked: true },")
    html_lines <- c(html_lines, "      y: { stacked: true, beginAtZero: true, ticks: { stepSize: 1 } }")
    html_lines <- c(html_lines, "    }")
    html_lines <- c(html_lines, "  }")
    html_lines <- c(html_lines, "});")
  }

  html_lines <- c(html_lines, '</script>')

  # Close container and body
  html_lines <- c(html_lines, "</div>")
  html_lines <- c(html_lines, "</body>")
  html_lines <- c(html_lines, "</html>")

  return(paste(html_lines, collapse = "\n"))
}


#' Format Validation Results as HTML
#'
#' @description
#' Internal function to format validation results as an HTML table.
#' Only shows Errors and Warnings; skips verbose Variable Info category items.
#'
#' @param validation_df Validation results data frame.
#'
#' @return Character vector of HTML lines.
#'
#' @keywords internal
format_validation_html <- function(validation_df) {
  lines <- character()

  if (is.null(validation_df) || nrow(validation_df) == 0) {
    lines <- c(lines, '<div class="summary-box">')
    lines <- c(lines, "<p>No validation issues found.</p>")
    lines <- c(lines, "</div>")
    return(lines)
  }

  # Count by severity (for summary only)
  severity_counts <- table(validation_df$severity)
  error_count <- if ("ERROR" %in% names(severity_counts)) severity_counts[["ERROR"]] else 0L
  warning_count <- if ("WARNING" %in% names(severity_counts)) severity_counts[["WARNING"]] else 0L
  info_count <- if ("INFO" %in% names(severity_counts)) severity_counts[["INFO"]] else 0L

  lines <- c(lines, '<div class="summary-box">')
  lines <- c(lines, sprintf(
    "<p><strong>Summary:</strong> Errors: %d | Warnings: %d | Info: %d</p>",
    error_count, warning_count, info_count
  ))
  lines <- c(lines, "</div>")

  # Only show Errors and Warnings, skip verbose Variable Info category items
  for_display <- validation_df[validation_df$severity %in% c("ERROR", "WARNING"), ]

  if (nrow(for_display) > 0) {
    # Create table for Errors and Warnings
    lines <- c(lines, "<table>")
    lines <- c(lines, "<tr>")
    lines <- c(lines, "<th>Severity</th>")
    lines <- c(lines, "<th>Variable</th>")
    lines <- c(lines, "<th>Category</th>")
    lines <- c(lines, "<th>Message</th>")
    lines <- c(lines, "</tr>")

    for (i in seq_len(nrow(for_display))) {
      severity_class <- paste0("severity-", for_display$severity[i])
      lines <- c(lines, "<tr>")
      lines <- c(lines, sprintf(
        '<td class="%s">%s</td>',
        severity_class,
        for_display$severity[i]
      ))
      lines <- c(lines, sprintf("<td>%s</td>", for_display$variable[i]))
      lines <- c(lines, sprintf("<td>%s</td>", for_display$category[i]))
      lines <- c(lines, sprintf("<td>%s</td>", for_display$message[i]))
      lines <- c(lines, "</tr>")
    }

    lines <- c(lines, "</table>")
  } else {
    lines <- c(lines, '<div class="summary-box">')
    lines <- c(lines, "<p>No errors or warnings found.</p>")
    lines <- c(lines, "</div>")
  }

  return(lines)
}


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
#' @return Invisibly returns the input `result` (useful for piping).
#'
#' @details
#' Supported formats:
#' - **HTML** (.html): Self-contained HTML report with styling and interactive charts.
#'   Uses [generate_html_report()] function.
#' - **Text** (.txt): Plain text report suitable for console review.
#'   Uses [generate_text_report()] function.
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
#' # Export to different formats
#' export_report(result, "report.html")
#' export_report(result, "report.txt")
#' export_report(result, "report.xlsx")
#'
#' # Explicit format specification
#' export_report(result, "output.file", format = "excel")
#' }
export_report <- function(result, file, format = NULL) {
  # Validate result object
  if (!is.list(result)) {
    stop("result must be a list from compare_datasets() or cdisc_compare()", call. = FALSE)
  }

  # Auto-detect format from file extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    if (ext == "html") {
      format <- "html"
    } else if (ext == "txt") {
      format <- "text"
    } else if (ext == "xlsx") {
      format <- "excel"
    } else {
      stop(sprintf(
        "Cannot auto-detect format from file extension '.%s'. Specify format explicitly.",
        ext
      ), call. = FALSE)
    }
  }

  # Validate format argument
  if (!format %in% c("html", "text", "excel")) {
    stop("format must be 'html', 'text', or 'excel'", call. = FALSE)
  }

  # Handle HTML format
  if (format == "html") {
    report_html <- generate_html_report(result)
    writeLines(report_html, file)
    message(sprintf("HTML report written to: %s", file))
  }
  # Handle text format
  else if (format == "text") {
    report_text <- generate_text_report(result)
    writeLines(report_text, file)
    message(sprintf("Text report written to: %s", file))
  }
  # Handle Excel format
  else if (format == "excel") {
    # Check if openxlsx is available
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("The 'openxlsx' package is required for Excel export. Install it with: install.packages('openxlsx')",
           call. = FALSE)
    }

    # Create new workbook
    wb <- openxlsx::createWorkbook()

    # --- Summary Sheet ---
    summary_data <- build_summary_sheet(result)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", summary_data)

    # Auto-size columns
    openxlsx::setColWidths(wb, "Summary", cols = 1:2, widths = c(25, 40))

    # --- Variable Diffs Sheet ---
    var_diffs_data <- build_variable_diffs_sheet(result)
    if (nrow(var_diffs_data) > 0) {
      openxlsx::addWorksheet(wb, "Variable Diffs")
      openxlsx::writeData(wb, "Variable Diffs", var_diffs_data)
      openxlsx::setColWidths(wb, "Variable Diffs", cols = seq_len(ncol(var_diffs_data)), widths = "auto")
    }

    # --- Value Diffs Sheet ---
    value_diffs_data <- get_all_differences(result)
    if (!is.null(value_diffs_data) && nrow(value_diffs_data) > 0) {
      openxlsx::addWorksheet(wb, "Value Diffs")
      openxlsx::writeData(wb, "Value Diffs", value_diffs_data)
      openxlsx::setColWidths(wb, "Value Diffs", cols = seq_len(ncol(value_diffs_data)), widths = "auto")
    }

    # --- CDISC Validation Sheet (only for cdisc_comparison) ---
    if (inherits(result, "cdisc_comparison")) {
      cdisc_val_data <- build_cdisc_validation_sheet(result)
      if (nrow(cdisc_val_data) > 0) {
        openxlsx::addWorksheet(wb, "CDISC Validation")
        openxlsx::writeData(wb, "CDISC Validation", cdisc_val_data)
        openxlsx::setColWidths(wb, "CDISC Validation", cols = seq_len(ncol(cdisc_val_data)), widths = "auto")
      }
    }

    # Save workbook
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    message(sprintf("Excel report written to: %s", file))
  }

  invisible(result)
}


#' Build Summary Sheet for Excel Export
#'
#' @description
#' Internal helper to construct the Summary sheet data frame for Excel export.
#'
#' @param result Comparison result list.
#'
#' @return Data frame with Item and Value columns.
#'
#' @keywords internal
build_summary_sheet <- function(result) {
  summary_list <- list()
  idx <- 1

  # Dataset dimensions
  summary_list[[idx]] <- data.frame(Item = "Base Dataset (df1) - Rows", Value = result$nrow_df1 %||% 0)
  idx <- idx + 1
  summary_list[[idx]] <- data.frame(Item = "Base Dataset (df1) - Columns", Value = result$ncol_df1 %||% 0)
  idx <- idx + 1
  summary_list[[idx]] <- data.frame(Item = "Compare Dataset (df2) - Rows", Value = result$nrow_df2 %||% 0)
  idx <- idx + 1
  summary_list[[idx]] <- data.frame(Item = "Compare Dataset (df2) - Columns", Value = result$ncol_df2 %||% 0)
  idx <- idx + 1

  # Domain and standard (for CDISC comparisons)
  if (!is.null(result$domain)) {
    summary_list[[idx]] <- data.frame(Item = "Domain", Value = result$domain)
    idx <- idx + 1
  }
  if (!is.null(result$standard)) {
    summary_list[[idx]] <- data.frame(Item = "Standard", Value = result$standard)
    idx <- idx + 1
  }

  # Variable counts
  common_cols <- length(result$common_columns %||% character())
  extra_df1 <- length(result$extra_in_df1 %||% character())
  extra_df2 <- length(result$extra_in_df2 %||% character())

  summary_list[[idx]] <- data.frame(Item = "Common Variables", Value = common_cols)
  idx <- idx + 1
  summary_list[[idx]] <- data.frame(Item = "Variables Only in Base", Value = extra_df1)
  idx <- idx + 1
  summary_list[[idx]] <- data.frame(Item = "Variables Only in Compare", Value = extra_df2)
  idx <- idx + 1

  # Matching type
  if (!is.null(result$id_vars)) {
    summary_list[[idx]] <- data.frame(Item = "Matching Type", Value = "ID Variables (Keys)")
    idx <- idx + 1
    summary_list[[idx]] <- data.frame(Item = "ID Variables", Value = paste(result$id_vars, collapse = ", "))
    idx <- idx + 1
  } else {
    summary_list[[idx]] <- data.frame(Item = "Matching Type", Value = "Positional")
    idx <- idx + 1
  }

  # Tolerance
  if (!is.null(result$tolerance)) {
    summary_list[[idx]] <- data.frame(Item = "Tolerance", Value = result$tolerance)
    idx <- idx + 1
  }

  # Combine all rows
  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL
  return(summary_df)
}


#' Build Variable Diffs Sheet for Excel Export
#'
#' @description
#' Internal helper to construct the Variable Diffs sheet data frame for Excel export.
#' Extracts metadata attribute differences (type, label, length, format mismatches).
#'
#' @param result Comparison result list.
#'
#' @return Data frame with variable differences.
#'
#' @keywords internal
build_variable_diffs_sheet <- function(result) {
  # For dataset_comparison, use type_mismatches if available
  if (!is.null(result$type_mismatches) && nrow(result$type_mismatches) > 0) {
    return(result$type_mismatches)
  }

  # For cdisc_comparison, iterate through metadata_comparison lists
  if (!is.null(result$metadata_comparison)) {
    meta <- result$metadata_comparison
    diffs_list <- list()

    # Type mismatches
    if (!is.null(meta$type_mismatches) && nrow(meta$type_mismatches) > 0) {
      diffs_list[[length(diffs_list) + 1]] <- meta$type_mismatches
    }

    # Label mismatches
    if (!is.null(meta$label_mismatches) && nrow(meta$label_mismatches) > 0) {
      diffs_list[[length(diffs_list) + 1]] <- meta$label_mismatches
    }

    # Length mismatches
    if (!is.null(meta$length_mismatches) && nrow(meta$length_mismatches) > 0) {
      diffs_list[[length(diffs_list) + 1]] <- meta$length_mismatches
    }

    # Format mismatches
    if (!is.null(meta$format_mismatches) && nrow(meta$format_mismatches) > 0) {
      diffs_list[[length(diffs_list) + 1]] <- meta$format_mismatches
    }

    # Combine all
    if (length(diffs_list) > 0) {
      return(do.call(rbind, diffs_list))
    }
  }

  # Return empty data frame if no differences found
  return(data.frame())
}


#' Build CDISC Validation Sheet for Excel Export
#'
#' @description
#' Internal helper to construct the CDISC Validation sheet data frame for Excel export.
#' Combines validation results from both datasets with a Dataset column.
#'
#' @param result A cdisc_comparison list.
#'
#' @return Data frame with combined CDISC validation results.
#'
#' @keywords internal
build_cdisc_validation_sheet <- function(result) {
  dfs_to_combine <- list()

  # Validation for df1
  if (!is.null(result$cdisc_validation_df1) && nrow(result$cdisc_validation_df1) > 0) {
    df1_val <- result$cdisc_validation_df1
    df1_val$Dataset <- "Base (df1)"
    dfs_to_combine[[length(dfs_to_combine) + 1]] <- df1_val
  }

  # Validation for df2
  if (!is.null(result$cdisc_validation_df2) && nrow(result$cdisc_validation_df2) > 0) {
    df2_val <- result$cdisc_validation_df2
    df2_val$Dataset <- "Compare (df2)"
    dfs_to_combine[[length(dfs_to_combine) + 1]] <- df2_val
  }

  # Combine all rows
  if (length(dfs_to_combine) > 0) {
    combined <- do.call(rbind, dfs_to_combine)
    rownames(combined) <- NULL
    # Reorder columns to put Dataset first
    col_order <- c("Dataset", setdiff(names(combined), "Dataset"))
    return(combined[, col_order, drop = FALSE])
  }

  # Return empty data frame if no validation data found
  return(data.frame())
}
