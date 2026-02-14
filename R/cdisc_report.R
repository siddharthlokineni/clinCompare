#' Generate CDISC Validation Report
#'
#' @description
#' Generates a formatted report from the results of [cdisc_compare()]. Supports both
#' text-based console output and HTML reports with professional styling and color-coding.
#'
#' @param cdisc_results A list output from [cdisc_compare()].
#' @param output_format Character string: either "text" (default) for console output
#'   or "html" for HTML report.
#' @param file_name Optional character string specifying the output file path.
#'   For text format, the report is appended to this file. For HTML format,
#'   must be explicitly provided by the user. If NULL, output is not written to file.
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
#' For text output, formatting uses console-friendly layout.
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
#' # Generate HTML report to file
#' generate_cdisc_report(result, output_format = "html", file_name = file.path(tempdir(), "report.html"))
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
      stop("file_name must be specified for HTML output format", call. = FALSE)
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
#'
#' @param cdisc_results List from [cdisc_compare()].
#'
#' @return
#' Character string containing the formatted text report.
#'
#' @keywords internal
generate_text_report <- function(cdisc_results) {
  lines <- character()

  # Title
  lines <- c(lines, "")
  lines <- c(lines, paste0("=", strrep("=", 77)))
  lines <- c(lines, "CDISC VALIDATION AND COMPARISON REPORT")
  lines <- c(lines, paste0("=", strrep("=", 77)))
  lines <- c(lines, "")

  # Dataset Comparison Summary
  lines <- c(lines, "DATASET COMPARISON SUMMARY")
  lines <- c(lines, paste0("-", strrep("-", 77)))

  if (!is.null(cdisc_results$comparison)) {
    comp <- cdisc_results$comparison
    if (is.data.frame(comp)) {
      lines <- c(lines, sprintf("  Total Issues Found: %d", nrow(comp)))
      if (nrow(comp) > 0) {
        # comparison has columns: Aspect, Description
        # Group by Aspect to show summary
        if ("Aspect" %in% names(comp)) {
          aspect_counts <- table(comp$Aspect)
          for (asp in names(aspect_counts)) {
            lines <- c(lines, sprintf("    %s: %d", asp, aspect_counts[[asp]]))
          }
        }
      }
    }
  }
  lines <- c(lines, "")

  # Variable Comparison
  lines <- c(lines, "VARIABLE COMPARISON")
  lines <- c(lines, paste0("-", strrep("-", 77)))

  if (!is.null(cdisc_results$variable_comparison)) {
    var_comp <- cdisc_results$variable_comparison
    if (is.list(var_comp) && !is.null(var_comp$details)) {
      details <- var_comp$details

      if (!is.null(details$extra_in_df1) && length(details$extra_in_df1) > 0) {
        lines <- c(lines, sprintf("  Variables only in df1 (%d):", length(details$extra_in_df1)))
        lines <- c(lines, sprintf("    %s", paste(details$extra_in_df1, collapse = ", ")))
      }

      if (!is.null(details$extra_in_df2) && length(details$extra_in_df2) > 0) {
        lines <- c(lines, sprintf("  Variables only in df2 (%d):", length(details$extra_in_df2)))
        lines <- c(lines, sprintf("    %s", paste(details$extra_in_df2, collapse = ", ")))
      }

      if (!is.null(details$common_columns) && length(details$common_columns) > 0) {
        lines <- c(lines, sprintf("  Common variables: %d", length(details$common_columns)))
      }
    }
  }
  lines <- c(lines, "")

  # Observation Comparison
  if (!is.null(cdisc_results$observation_comparison)) {
    obs_comp <- cdisc_results$observation_comparison
    lines <- c(lines, "OBSERVATION COMPARISON")
    lines <- c(lines, paste0("-", strrep("-", 77)))

    if (is.list(obs_comp) && !is.null(obs_comp$status)) {
      lines <- c(lines, sprintf("  Status: %s", obs_comp$status))
      if (!is.null(obs_comp$message)) {
        lines <- c(lines, sprintf("  %s", obs_comp$message))
      }
    }
    lines <- c(lines, "")
  }

  # CDISC Validation for df1
  lines <- c(lines, "CDISC VALIDATION RESULTS - DATASET 1")
  lines <- c(lines, paste0("-", strrep("-", 77)))
  lines <- c(lines, format_validation_summary(cdisc_results$cdisc_validation_df1))
  lines <- c(lines, "")

  # CDISC Validation for df2
  lines <- c(lines, "CDISC VALIDATION RESULTS - DATASET 2")
  lines <- c(lines, paste0("-", strrep("-", 77)))
  lines <- c(lines, format_validation_summary(cdisc_results$cdisc_validation_df2))
  lines <- c(lines, "")

  # CDISC Conformance Comparison
  lines <- c(lines, "CDISC CONFORMANCE COMPARISON")
  lines <- c(lines, paste0("-", strrep("-", 77)))

  if (nrow(cdisc_results$cdisc_conformance_comparison) > 0) {
    conform <- cdisc_results$cdisc_conformance_comparison

    df1_only_count <- sum(conform$df1_only)
    df2_only_count <- sum(conform$df2_only)
    both_count <- sum(conform$both)

    lines <- c(lines, sprintf("  Issues unique to df1: %d", df1_only_count))
    if (df1_only_count > 0) {
      df1_issues <- conform[conform$df1_only, ]
      for (i in seq_len(nrow(df1_issues))) {
        lines <- c(lines, sprintf(
          "    - %s: %s",
          df1_issues$variable[i],
          df1_issues$category[i]
        ))
      }
    }

    lines <- c(lines, "")
    lines <- c(lines, sprintf("  Issues unique to df2: %d", df2_only_count))
    if (df2_only_count > 0) {
      df2_issues <- conform[conform$df2_only, ]
      for (i in seq_len(nrow(df2_issues))) {
        lines <- c(lines, sprintf(
          "    - %s: %s",
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
#' Internal function to format validation results as text.
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

  # Count by severity
  severity_counts <- table(validation_df$severity)
  lines <- c(lines, sprintf(
    "  Summary: %d error(s), %d warning(s), %d info message(s)",
    if ("ERROR" %in% names(severity_counts)) severity_counts[["ERROR"]] else 0L,
    if ("WARNING" %in% names(severity_counts)) severity_counts[["WARNING"]] else 0L,
    if ("INFO" %in% names(severity_counts)) severity_counts[["INFO"]] else 0L
  ))
  lines <- c(lines, "")

  # Group by category
  categories <- unique(validation_df$category)
  for (cat in categories) {
    cat_rows <- validation_df[validation_df$category == cat, ]
    lines <- c(lines, sprintf("  %s:", cat))

    for (i in seq_len(nrow(cat_rows))) {
      severity_prefix <- sprintf("[%s]", cat_rows$severity[i])
      lines <- c(lines, sprintf(
        "    %s %s (%s): %s",
        severity_prefix,
        cat_rows$variable[i],
        cat_rows$severity[i],
        cat_rows$message[i]
      ))
    }
    lines <- c(lines, "")
  }

  return(lines)
}


#' Generate HTML Report
#'
#' @description
#' Internal function to generate a self-contained HTML report with styling.
#'
#' @param cdisc_results List from [cdisc_compare()].
#'
#' @return
#' Character string containing the HTML report.
#'
#' @keywords internal
generate_html_report <- function(cdisc_results) {
  html_lines <- character()

  # HTML header with styles
  html_lines <- c(html_lines, "<!DOCTYPE html>")
  html_lines <- c(html_lines, "<html>")
  html_lines <- c(html_lines, "<head>")
  html_lines <- c(html_lines, '  <meta charset="UTF-8">')
  html_lines <- c(html_lines, "  <title>CDISC Validation Report</title>")
  html_lines <- c(html_lines, "  <style>")
  html_lines <- c(html_lines, "    body {")
  html_lines <- c(html_lines, "      font-family: Arial, sans-serif;")
  html_lines <- c(html_lines, "      margin: 20px;")
  html_lines <- c(html_lines, "      background-color: #f5f5f5;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .container {")
  html_lines <- c(html_lines, "      max-width: 1200px;")
  html_lines <- c(html_lines, "      margin: 0 auto;")
  html_lines <- c(html_lines, "      background-color: white;")
  html_lines <- c(html_lines, "      padding: 20px;")
  html_lines <- c(html_lines, "      border-radius: 8px;")
  html_lines <- c(html_lines, "      box-shadow: 0 2px 4px rgba(0,0,0,0.1);")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    h1 {")
  html_lines <- c(html_lines, "      color: #333;")
  html_lines <- c(html_lines, "      border-bottom: 3px solid #0066cc;")
  html_lines <- c(html_lines, "      padding-bottom: 10px;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    h2 {")
  html_lines <- c(html_lines, "      color: #0066cc;")
  html_lines <- c(html_lines, "      margin-top: 30px;")
  html_lines <- c(html_lines, "      border-left: 4px solid #0066cc;")
  html_lines <- c(html_lines, "      padding-left: 10px;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    table {")
  html_lines <- c(html_lines, "      width: 100%;")
  html_lines <- c(html_lines, "      border-collapse: collapse;")
  html_lines <- c(html_lines, "      margin: 15px 0;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    th {")
  html_lines <- c(html_lines, "      background-color: #0066cc;")
  html_lines <- c(html_lines, "      color: white;")
  html_lines <- c(html_lines, "      padding: 12px;")
  html_lines <- c(html_lines, "      text-align: left;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    td {")
  html_lines <- c(html_lines, "      padding: 10px;")
  html_lines <- c(html_lines, "      border-bottom: 1px solid #ddd;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    tr:hover {")
  html_lines <- c(html_lines, "      background-color: #f9f9f9;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .severity-ERROR {")
  html_lines <- c(html_lines, "      background-color: #ffcccc;")
  html_lines <- c(html_lines, "      color: #cc0000;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .severity-WARNING {")
  html_lines <- c(html_lines, "      background-color: #ffe6cc;")
  html_lines <- c(html_lines, "      color: #ff9900;")
  html_lines <- c(html_lines, "      font-weight: bold;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .severity-INFO {")
  html_lines <- c(html_lines, "      background-color: #cce5ff;")
  html_lines <- c(html_lines, "      color: #0066cc;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "    .summary-box {")
  html_lines <- c(html_lines, "      background-color: #f0f8ff;")
  html_lines <- c(html_lines, "      border-left: 4px solid #0066cc;")
  html_lines <- c(html_lines, "      padding: 12px;")
  html_lines <- c(html_lines, "      margin: 15px 0;")
  html_lines <- c(html_lines, "    }")
  html_lines <- c(html_lines, "  </style>")
  html_lines <- c(html_lines, "</head>")
  html_lines <- c(html_lines, "<body>")
  html_lines <- c(html_lines, '<div class="container">')

  # Title
  html_lines <- c(html_lines, "<h1>CDISC Validation and Comparison Report</h1>")

  # Dataset Comparison Summary
  html_lines <- c(html_lines, "<h2>Dataset Comparison Summary</h2>")
  if (!is.null(cdisc_results$comparison) && is.data.frame(cdisc_results$comparison)) {
    comp <- cdisc_results$comparison
    html_lines <- c(html_lines, '<div class="summary-box">')
    html_lines <- c(html_lines, sprintf("<p><strong>Total Issues Found:</strong> %d</p>", nrow(comp)))
    if (nrow(comp) > 0) {
      # Handle Aspect-based grouping instead of severity
      if ("Aspect" %in% names(comp)) {
        aspect_counts <- table(comp$Aspect)
        html_lines <- c(html_lines, "<ul>")
        for (asp in names(aspect_counts)) {
          html_lines <- c(html_lines, sprintf("<li>%s: %d</li>", asp, aspect_counts[[asp]]))
        }
        html_lines <- c(html_lines, "</ul>")
      }
    }
    html_lines <- c(html_lines, "</div>")
  }

  # Variable Comparison
  html_lines <- c(html_lines, "<h2>Variable Comparison</h2>")
  if (!is.null(cdisc_results$variable_comparison)) {
    var_comp <- cdisc_results$variable_comparison
    if (is.list(var_comp) && !is.null(var_comp$details)) {
      details <- var_comp$details
      html_lines <- c(html_lines, '<div class="summary-box">')

      if (!is.null(details$extra_in_df1) && length(details$extra_in_df1) > 0) {
        html_lines <- c(html_lines, sprintf(
          "<p><strong>Variables only in df1 (%d):</strong> %s</p>",
          length(details$extra_in_df1),
          paste(details$extra_in_df1, collapse = ", ")
        ))
      }

      if (!is.null(details$extra_in_df2) && length(details$extra_in_df2) > 0) {
        html_lines <- c(html_lines, sprintf(
          "<p><strong>Variables only in df2 (%d):</strong> %s</p>",
          length(details$extra_in_df2),
          paste(details$extra_in_df2, collapse = ", ")
        ))
      }

      if (!is.null(details$common_columns) && length(details$common_columns) > 0) {
        html_lines <- c(html_lines, sprintf(
          "<p><strong>Common variables:</strong> %d</p>",
          length(details$common_columns)
        ))
      }

      html_lines <- c(html_lines, "</div>")
    }
  }

  # CDISC Validation for df1
  html_lines <- c(html_lines, "<h2>CDISC Validation Results - Dataset 1</h2>")
  html_lines <- c(html_lines, format_validation_html(cdisc_results$cdisc_validation_df1))

  # CDISC Validation for df2
  html_lines <- c(html_lines, "<h2>CDISC Validation Results - Dataset 2</h2>")
  html_lines <- c(html_lines, format_validation_html(cdisc_results$cdisc_validation_df2))

  # CDISC Conformance Comparison
  html_lines <- c(html_lines, "<h2>CDISC Conformance Comparison</h2>")

  if (!is.null(cdisc_results$cdisc_conformance_comparison) &&
      nrow(cdisc_results$cdisc_conformance_comparison) > 0) {
    conform <- cdisc_results$cdisc_conformance_comparison

    html_lines <- c(html_lines, "<table>")
    html_lines <- c(html_lines, "<tr>")
    html_lines <- c(html_lines, "<th>Variable</th>")
    html_lines <- c(html_lines, "<th>Category</th>")
    html_lines <- c(html_lines, "<th>In df1 Only</th>")
    html_lines <- c(html_lines, "<th>In df2 Only</th>")
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

  # Count by severity
  severity_counts <- table(validation_df$severity)
  lines <- c(lines, '<div class="summary-box">')
  lines <- c(lines, sprintf(
    "<p><strong>Summary:</strong> %d error(s), %d warning(s), %d info message(s)</p>",
    if ("ERROR" %in% names(severity_counts)) severity_counts[["ERROR"]] else 0L,
    if ("WARNING" %in% names(severity_counts)) severity_counts[["WARNING"]] else 0L,
    if ("INFO" %in% names(severity_counts)) severity_counts[["INFO"]] else 0L
  ))
  lines <- c(lines, "</div>")

  # Create table
  lines <- c(lines, "<table>")
  lines <- c(lines, "<tr>")
  lines <- c(lines, "<th>Severity</th>")
  lines <- c(lines, "<th>Variable</th>")
  lines <- c(lines, "<th>Category</th>")
  lines <- c(lines, "<th>Message</th>")
  lines <- c(lines, "</tr>")

  for (i in seq_len(nrow(validation_df))) {
    severity_class <- paste0("severity-", validation_df$severity[i])
    lines <- c(lines, "<tr>")
    lines <- c(lines, sprintf(
      '<td class="%s">%s</td>',
      severity_class,
      validation_df$severity[i]
    ))
    lines <- c(lines, sprintf("<td>%s</td>", validation_df$variable[i]))
    lines <- c(lines, sprintf("<td>%s</td>", validation_df$category[i]))
    lines <- c(lines, sprintf("<td>%s</td>", validation_df$message[i]))
    lines <- c(lines, "</tr>")
  }

  lines <- c(lines, "</table>")

  return(lines)
}
