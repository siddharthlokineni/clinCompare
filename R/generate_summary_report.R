#' Generate a Summary Report of Dataset Comparison
#'
#' @description
#' Provides a summary of the comparison results, highlighting key points such as
#' the number of differing observations and variables. Accepts output from
#' [compare_datasets()], [cdisc_compare()], or any named list with comparison
#' fields.
#'
#' @param comparison_results A list or data frame containing comparison results.
#'   Typically output from [compare_datasets()] or [cdisc_compare()].
#' @param detail_level The level of detail ('high', 'medium', 'low') for the summary.
#' @param output_format Format of the output ('text' or 'html').
#' @param file_name Name of the file to save the report to (applicable for 'html' format).
#' @return The summary report as a character string (invisibly). For 'text',
#'   also prints to console. For 'html', writes to file.
#' @export
#' @examples
#' \dontrun{
#'   result <- compare_datasets(df1, df2)
#'   generate_summary_report(result)
#'
#'   cdisc_result <- cdisc_compare(df1, df2, domain = "DM", standard = "SDTM")
#'   generate_summary_report(cdisc_result)
#' }

generate_summary_report <- function(comparison_results, detail_level = "high",
                                    output_format = "text",
                                    file_name = "summary_report") {
  summary_report <- paste("Summary Comparison Report\n",
                          "======================\n\n", sep = "")

  # Handle cdisc_compare() output (list with named fields)
  if (is.list(comparison_results) && !is.data.frame(comparison_results)) {
    # Variable comparison
    var_comp <- comparison_results$variable_comparison
    if (!is.null(var_comp)) {
      if (is.data.frame(var_comp)) {
        summary_report <- paste0(
          summary_report,
          "Variable Differences: ", nrow(var_comp), "\n"
        )
      } else if (is.list(var_comp)) {
        n_items <- length(var_comp)
        summary_report <- paste0(
          summary_report,
          "Variable Comparison Items: ", n_items, "\n"
        )
      }
    }

    # Observation comparison
    obs_comp <- comparison_results$observation_comparison
    if (!is.null(obs_comp) && is.list(obs_comp) &&
        !is.null(obs_comp$discrepancies)) {
      total_diffs <- sum(obs_comp$discrepancies, na.rm = TRUE)
      summary_report <- paste0(
        summary_report,
        "Total Observation Differences: ", total_diffs, "\n"
      )
    }

    # Dimensions
    if (!is.null(comparison_results$nrow_df1)) {
      summary_report <- paste0(
        summary_report,
        sprintf(
          "Base: %d rows x %d cols | Compare: %d rows x %d cols\n",
          comparison_results$nrow_df1, comparison_results$ncol_df1,
          comparison_results$nrow_df2, comparison_results$ncol_df2
        )
      )
    }

  } else if (is.data.frame(comparison_results)) {
    # Handle compare_datasets() output (data frame with Aspect/Description)
    summary_report <- paste0(
      summary_report,
      "Comparison aspects found: ", nrow(comparison_results), "\n"
    )
    for (i in seq_len(nrow(comparison_results))) {
      summary_report <- paste0(
        summary_report,
        comparison_results$Aspect[i], ": ",
        comparison_results$Description[i], "\n"
      )
    }
  }

  # Output based on format
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
    writeLines(html_content, paste0(file_name, ".html"))
    message("Report saved to ", file_name, ".html")
  } else {
    stop("Unsupported output format. Choose 'text' or 'html'.")
  }

  invisible(summary_report)
}
