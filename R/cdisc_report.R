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
#' @return
#' Invisibly returns the input `cdisc_results` (useful for piping).
#'
#' @details
#' The report includes:
#' - Dataset Comparison Summary
#' - CDISC Compliance for each dataset
#' - CDISC Conformance Comparison
#'
#' For text output, formatting uses a PROC COMPARE-style layout. Use
#' \code{file_name} to save to a .txt file for easy sharing and review.
#' For HTML output, a self-contained report is generated with color-coded severity
#' levels: red for ERROR, orange for WARNING, blue for INFO.
#'
#' @export
#' @examples
#' \dontrun{
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
#' @return
#' Invisibly returns the input (useful for piping).
#'
#' @details
#' Output includes:
#' - Summary counts of errors, warnings, and info messages
#' - Issues grouped by category
#' - Each issue displayed with its variable name and message
#'
#' @export
#' @examples
#' \dontrun{
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
#' Output is styled after SAS PROC COMPARE with comparison details first, followed by
#' CDISC validation information.
#'
#' @param cdisc_results List from [cdisc_compare()].
#'
#' @return
#' Character string containing the formatted text report.
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
  lines <- c(lines, sprintf("  CompareR - %s %s Domain Comparison Report", standard, domain))
  lines <- c(lines, paste0("=", strrep("=", 77)))
  lines <- c(lines, "")

  # DATA SET SUMMARY (proc compare style)
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

  # METADATA COMPARISON
  meta <- cdisc_results$metadata_comparison
  if (!is.null(meta)) {
    lines <- c(lines, "  METADATA COMPARISON")
    lines <- c(lines, paste0("  -", strrep("-", 59)))

    # Type mismatches
    if (nrow(meta$type_mismatches) > 0) {
      lines <- c(lines, "  Variables with Differing Types:")
      lines <- c(lines, sprintf("  %-20s %-15s %-15s", "Variable", "Base", "Compare"))
      lines <- c(lines, paste0("  ", strrep("-", 50)))
      for (i in seq_len(nrow(meta$type_mismatches))) {
        lines <- c(lines, sprintf(
          "  %-20s %-15s %-15s",
          meta$type_mismatches$variable[i],
          meta$type_mismatches$type_base[i],
          meta$type_mismatches$type_compare[i]
        ))
      }
      lines <- c(lines, "")
    } else {
      lines <- c(lines, "  Variable types match on all common variables.")
      lines <- c(lines, "")
    }

    # Label mismatches
    if (nrow(meta$label_mismatches) > 0) {
      lines <- c(lines, "  Variables with Differing Labels:")
      lines <- c(lines, sprintf("  %-15s %-25s %-25s", "Variable", "Base Label", "Compare Label"))
      lines <- c(lines, paste0("  ", strrep("-", 65)))
      for (i in seq_len(nrow(meta$label_mismatches))) {
        bl <- meta$label_mismatches$label_base[i]
        cl <- meta$label_mismatches$label_compare[i]
        bl <- if (nchar(bl) == 0) "(none)" else bl
        cl <- if (nchar(cl) == 0) "(none)" else cl
        lines <- c(lines, sprintf("  %-15s %-25s %-25s",
          meta$label_mismatches$variable[i], bl, cl
        ))
      }
      lines <- c(lines, "")
    } else {
      lines <- c(lines, "  Variable labels match (or none assigned).")
      lines <- c(lines, "")
    }

    # Column order
    if (!meta$order_match) {
      lines <- c(lines, "  Column Ordering: DIFFERS between Base and Compare")
      lines <- c(lines, sprintf("    Base:    %s", paste(meta$order_df1, collapse = ", ")))
      lines <- c(lines, sprintf("    Compare: %s", paste(meta$order_df2, collapse = ", ")))
    } else {
      lines <- c(lines, "  Column Ordering: MATCHES")
    }

    lines <- c(lines, paste0("  -", strrep("-", 59)))
    lines <- c(lines, "")
  }

  # VALUE COMPARISON RESULTS
  lines <- c(lines, "  VALUE COMPARISON RESULTS")
  lines <- c(lines, paste0("  -", strrep("-", 59)))

  obs_comp <- cdisc_results$observation_comparison
  if (!is.null(obs_comp) && is.list(obs_comp)) {
    # Check if it has details with observation differences
    if (!is.null(obs_comp$details) && is.list(obs_comp$details) && length(obs_comp$details) > 0) {
      # Format observation-level differences as a table
      var_name_list <- names(obs_comp$details)

      if (length(var_name_list) > 0) {
        lines <- c(lines, "  Variable    Row   Base Value        Compare Value")
        lines <- c(lines, paste0("  -", strrep("-", 59)))

        total_diffs <- 0
        for (var_name in var_name_list) {
          var_diffs <- obs_comp$details[[var_name]]
          if (is.data.frame(var_diffs) && nrow(var_diffs) > 0) {
            for (j in seq_len(nrow(var_diffs))) {
              row_num <- var_diffs$Row[j]
              val_df1 <- var_diffs$Value_in_df1[j]
              val_df2 <- var_diffs$Value_in_df2[j]
              lines <- c(lines, sprintf(
                "  %-11s %3d   %-17s %-17s",
                var_name, row_num,
                as.character(val_df1), as.character(val_df2)
              ))
              total_diffs <- total_diffs + 1
            }
          }
        }

        lines <- c(lines, paste0("  -", strrep("-", 59)))
        total_vars_with_diffs <- length(var_name_list)
        lines <- c(lines, sprintf("  Total:  %d value(s) differ across %d variable(s)", total_diffs, total_vars_with_diffs))
      } else {
        lines <- c(lines, "  No value differences found.")
      }
    } else if (!is.null(obs_comp$status) && obs_comp$status %in% c("Error", "Skipped")) {
      # Error or skipped case
      lines <- c(lines, sprintf("  NOTE: %s", obs_comp$message))
    } else {
      lines <- c(lines, "  No value differences found. Datasets match on all common observations.")
    }
  }
  lines <- c(lines, "")

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
#' @return
#' Character vector of formatted lines.
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
#' Report follows PROC COMPARE style with comparison details first, then CDISC validation.
#'
#' @param cdisc_results List from [cdisc_compare()].
#'
#' @return
#' Character string containing the HTML report.
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
  html_lines <- c(html_lines, sprintf("  <title>CompareR - %s %s Domain Report</title>", standard, domain))
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
  html_lines <- c(html_lines, '  <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.min.js"></script>')
  html_lines <- c(html_lines, "</head>")
  html_lines <- c(html_lines, "<body>")
  html_lines <- c(html_lines, '<div class="container">')

  # Title with domain and standard
  html_lines <- c(html_lines, sprintf("<h1>CompareR - %s %s Domain Comparison Report</h1>", standard, domain))

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

  # METADATA COMPARISON
  meta <- cdisc_results$metadata_comparison
  if (!is.null(meta)) {
    html_lines <- c(html_lines, "<h2>Metadata Comparison</h2>")

    # Type mismatches
    if (nrow(meta$type_mismatches) > 0) {
      html_lines <- c(html_lines, "<h3>Variables with Differing Types</h3>")
      html_lines <- c(html_lines, "<table>")
      html_lines <- c(html_lines, "<tr><th>Variable</th><th>Base Type</th><th>Compare Type</th></tr>")
      for (i in seq_len(nrow(meta$type_mismatches))) {
        html_lines <- c(html_lines, '<tr class="diff-highlight">')
        html_lines <- c(html_lines, sprintf("<td><strong>%s</strong></td>", meta$type_mismatches$variable[i]))
        html_lines <- c(html_lines, sprintf("<td>%s</td>", meta$type_mismatches$type_base[i]))
        html_lines <- c(html_lines, sprintf("<td>%s</td>", meta$type_mismatches$type_compare[i]))
        html_lines <- c(html_lines, "</tr>")
      }
      html_lines <- c(html_lines, "</table>")
    } else {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, "<p>Variable types match on all common variables.</p>")
      html_lines <- c(html_lines, "</div>")
    }

    # Label mismatches
    if (nrow(meta$label_mismatches) > 0) {
      html_lines <- c(html_lines, "<h3>Variables with Differing Labels</h3>")
      html_lines <- c(html_lines, "<table>")
      html_lines <- c(html_lines, "<tr><th>Variable</th><th>Base Label</th><th>Compare Label</th></tr>")
      for (i in seq_len(nrow(meta$label_mismatches))) {
        bl <- meta$label_mismatches$label_base[i]
        cl <- meta$label_mismatches$label_compare[i]
        bl <- if (nchar(bl) == 0) "<em>(none)</em>" else bl
        cl <- if (nchar(cl) == 0) "<em>(none)</em>" else cl
        html_lines <- c(html_lines, '<tr class="diff-highlight">')
        html_lines <- c(html_lines, sprintf("<td><strong>%s</strong></td>", meta$label_mismatches$variable[i]))
        html_lines <- c(html_lines, sprintf("<td>%s</td>", bl))
        html_lines <- c(html_lines, sprintf("<td>%s</td>", cl))
        html_lines <- c(html_lines, "</tr>")
      }
      html_lines <- c(html_lines, "</table>")
    } else {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, "<p>Variable labels match (or none assigned).</p>")
      html_lines <- c(html_lines, "</div>")
    }

    # Column order
    if (!meta$order_match) {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, "<p><strong>Column Ordering: DIFFERS</strong></p>")
      html_lines <- c(html_lines, sprintf("<p>Base: %s</p>", paste(meta$order_df1, collapse = ", ")))
      html_lines <- c(html_lines, sprintf("<p>Compare: %s</p>", paste(meta$order_df2, collapse = ", ")))
      html_lines <- c(html_lines, "</div>")
    } else {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, "<p>Column Ordering: MATCHES</p>")
      html_lines <- c(html_lines, "</div>")
    }
  }

  # VALUE COMPARISON RESULTS (prominent, featured section)
  html_lines <- c(html_lines, "<h2>Value Comparison Results</h2>")

  obs_comp <- cdisc_results$observation_comparison
  if (!is.null(obs_comp) && is.list(obs_comp)) {
    if (!is.null(obs_comp$details) && is.list(obs_comp$details) && length(obs_comp$details) > 0) {
      var_name_list <- names(obs_comp$details)

      if (length(var_name_list) > 0) {
        html_lines <- c(html_lines, "<table>")
        html_lines <- c(html_lines, "<tr>")
        html_lines <- c(html_lines, "<th>Variable</th>")
        html_lines <- c(html_lines, "<th>Row</th>")
        html_lines <- c(html_lines, "<th>Base Value</th>")
        html_lines <- c(html_lines, "<th>Compare Value</th>")
        html_lines <- c(html_lines, "</tr>")

        total_diffs <- 0
        for (var_name in var_name_list) {
          var_diffs <- obs_comp$details[[var_name]]
          if (is.data.frame(var_diffs) && nrow(var_diffs) > 0) {
            for (j in seq_len(nrow(var_diffs))) {
              row_num <- var_diffs$Row[j]
              val_df1 <- var_diffs$Value_in_df1[j]
              val_df2 <- var_diffs$Value_in_df2[j]
              html_lines <- c(html_lines, '<tr class="diff-highlight">')
              html_lines <- c(html_lines, sprintf("<td><strong>%s</strong></td>", var_name))
              html_lines <- c(html_lines, sprintf("<td>%d</td>", row_num))
              html_lines <- c(html_lines, sprintf("<td>%s</td>", as.character(val_df1)))
              html_lines <- c(html_lines, sprintf("<td>%s</td>", as.character(val_df2)))
              html_lines <- c(html_lines, "</tr>")
              total_diffs <- total_diffs + 1
            }
          }
        }

        html_lines <- c(html_lines, "</table>")
        total_vars_with_diffs <- length(var_name_list)
        html_lines <- c(html_lines, '<div class="summary-box">')
        html_lines <- c(html_lines, sprintf(
          "<p><strong>Total:</strong> %d value(s) differ across %d variable(s)</p>",
          total_diffs, total_vars_with_diffs
        ))
        html_lines <- c(html_lines, "</div>")
      } else {
        html_lines <- c(html_lines, '<div class="summary-box">')
        html_lines <- c(html_lines, "<p>No value differences found.</p>")
        html_lines <- c(html_lines, "</div>")
      }
    } else if (!is.null(obs_comp$status) && obs_comp$status %in% c("Error", "Skipped")) {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, sprintf("<p><strong>NOTE:</strong> %s</p>", obs_comp$message))
      html_lines <- c(html_lines, "</div>")
    } else {
      html_lines <- c(html_lines, '<div class="summary-box">')
      html_lines <- c(html_lines, "<p>No value differences found. Datasets match on all common observations.</p>")
      html_lines <- c(html_lines, "</div>")
    }
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
  meta_order_n <- if (!is.null(meta_chart) && !meta_chart$order_match) 1 else 0

  html_lines <- c(html_lines, "new Chart(document.getElementById('metaChart'), {")
  html_lines <- c(html_lines, "  type: 'bar',")
  html_lines <- c(html_lines, "  data: {")
  html_lines <- c(html_lines, "    labels: ['Type Mismatches', 'Label Mismatches', 'Column Order'],")
  html_lines <- c(html_lines, sprintf(
    "    datasets: [{ label: 'Issues', data: [%d, %d, %d], backgroundColor: ['#3498DB', '#F39C12', '#9B59B6'], borderRadius: 4 }]",
    meta_type_n, meta_label_n, meta_order_n
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
#' @return
#' Character vector of HTML lines.
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
