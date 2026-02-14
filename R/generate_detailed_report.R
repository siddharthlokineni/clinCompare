#' Generate a Detailed Report of Dataset Comparison
#'
#' Creates a detailed report outlining all the differences found in the comparison,
#' including variable differences, observation differences, and group-based discrepancies.
#'
#' @param comparison_results A list containing the results of dataset comparisons.
#' @param output_format Format of the output ('text' or 'html').
#' @param file_name Name of the file to save the report to (applicable for 'html' format).
#' @return The detailed report. For 'text', prints to console. For 'html', writes to file.
#' @export
#' @examples
#' \dontrun{
#'   generate_detailed_report(comparison_results, output_format = "text")
#' }

generate_detailed_report <- function(comparison_results, output_format = "text", file_name = NULL) {
  detailed_report <- paste("Detailed Comparison Report\n", "======================\n\n", sep = "")

  # Generate report content
  if (!is.null(comparison_results$VariableDifferences)) {
    detailed_report <- paste0(detailed_report, "Variable Differences:\n")
    detailed_report <- paste0(detailed_report, format(comparison_results$VariableDifferences), "\n\n")
  }

  if (!is.null(comparison_results$ObservationDifferences)) {
    detailed_report <- paste0(detailed_report, "Observation Differences:\n")
    for (col in names(comparison_results$ObservationDifferences)) {
      detailed_report <- paste0(detailed_report, "Column: ", col, "\n")
      detailed_report <- paste0(detailed_report, format(comparison_results$ObservationDifferences[[col]]), "\n\n")
    }
  }

  # Output the report based on the specified format
  if (output_format == "text") {
    cat(detailed_report)
  } else if (output_format == "html") {
    html_content <- paste0(
      "<!DOCTYPE html>\n",
      "<html>\n",
      "<head>\n",
      "<title>Comparison Detailed Report</title>\n",
      "<style>",
      "body { font-family: Arial, sans-serif; margin: 20px; }",
      "h1 { color: #333; }",
      "pre { background-color: #f4f4f4; padding: 10px; border-radius: 5px; overflow-x: auto; }",
      "</style>\n",
      "</head>\n",
      "<body>\n",
      "<h1>Detailed Comparison Report</h1>\n",
      "<pre>", detailed_report, "</pre>\n",
      "</body>\n",
      "</html>"
    )
    if (is.null(file_name)) {
      stop("file_name must be specified for HTML output format", call. = FALSE)
    }
    writeLines(html_content, file_name)
    message("Report saved to ", file_name)
  } else {
    stop("Unsupported output format. Choose 'text' or 'html'.")
  }
}
