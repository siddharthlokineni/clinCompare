#' Generate a Summary Report of Dataset Comparison
#'
#' Provides a summary of the comparison results, highlighting key points such as the number of differing observations and variables.
#'
#' @param comparison_results A list containing the results of dataset comparisons.
#' @param detail_level The level of detail ('high', 'medium', 'low') for the summary.
#' @param output_format Format of the output ('text' or 'html').
#' @param file_name Name of the file to save the report to (applicable for 'html' format).
#' @return The summary report. For 'text', prints to console. For 'html', writes to file.
#' @export
#' @examples
#' \dontrun{
#'   generate_summary_report(comparison_results, detail_level = "high", output_format = "text")
#' }

generate_summary_report <- function(comparison_results, detail_level = "high", output_format = "text", file_name = NULL) {
  summary_report <- paste("Summary Comparison Report\n", "======================\n\n", sep = "")

  # Generate summary based on the detail level
  if (!is.null(comparison_results$VariableDifferences)) {
    num_var_diffs <- length(comparison_results$VariableDifferences)
    summary_report <- paste0(summary_report, "Number of Variable Differences: ", num_var_diffs, "\n")
  }

  if (!is.null(comparison_results$ObservationDifferences)) {
    num_obs_diffs <- sum(sapply(comparison_results$ObservationDifferences, nrow))
    summary_report <- paste0(summary_report, "Total Number of Observation Differences: ", num_obs_diffs, "\n")
  }

  # Output the summary based on the specified format
  if (output_format == "text") {
    cat(summary_report)
  } else if (output_format == "html") {
    html_content <- paste0(
      "<!DOCTYPE html>\n",
      "<html>\n",
      "<head>\n",
      "<title>Comparison Summary Report</title>\n",
      "<style>",
      "body { font-family: Arial, sans-serif; margin: 20px; }",
      "h1 { color: #333; }",
      "pre { background-color: #f4f4f4; padding: 10px; border-radius: 5px; }",
      "</style>\n",
      "</head>\n",
      "<body>\n",
      "<h1>Summary Comparison Report</h1>\n",
      "<pre>", summary_report, "</pre>\n",
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
