#' Generate a Detailed Report of Dataset Comparison
#'
#' @description
#' Generates a detailed comparison report with observation-level differences,
#' providing a comprehensive view of all discrepancies between two datasets.
#' Accepts output from [compare_datasets()], [cdisc_compare()], or any named
#' list with comparison fields.
#'
#' @param comparison_results A list or data frame containing comparison results.
#'   Typically output from [compare_datasets()] or [cdisc_compare()].
#' @param output_format Format of the output ('text' or 'html').
#' @param file_name Name of the file to save the report to (applicable for 'html' format).
#' @return The detailed report as a character string (invisibly). For 'text',
#'   also prints to console. For 'html', writes to file.
#' @export
#' @examples
#' \dontrun{
#'   result <- compare_datasets(df1, df2)
#'   generate_detailed_report(result)
#'
#'   cdisc_result <- cdisc_compare(df1, df2, domain = "DM", standard = "SDTM")
#'   generate_detailed_report(cdisc_result)
#' }

generate_detailed_report <- function(comparison_results, output_format = "text",
                                     file_name = "detailed_report") {
  detailed_report <- paste("Detailed Comparison Report\n",
                           "======================\n\n", sep = "")

  # Handle cdisc_compare() output (list with named fields)
  if (is.list(comparison_results) && !is.data.frame(comparison_results)) {

    # Variable comparison
    var_comp <- comparison_results$variable_comparison
    if (!is.null(var_comp)) {
      detailed_report <- paste0(detailed_report, "Variable Differences:\n")
      if (is.data.frame(var_comp)) {
        detailed_report <- paste0(
          detailed_report,
          paste(utils::capture.output(print(var_comp)), collapse = "\n"),
          "\n\n"
        )
      } else if (is.list(var_comp)) {
        # Format the nested structure from compare_variables() cleanly
        vc <- if (!is.null(var_comp$details)) var_comp$details else var_comp
        disc <- if (!is.null(var_comp$discrepancies)) var_comp$discrepancies else 0L

        detailed_report <- paste0(detailed_report,
          sprintf("  Discrepancies: %d\n", disc))

        if (!is.null(vc$common_columns) && length(vc$common_columns) > 0) {
          detailed_report <- paste0(detailed_report,
            sprintf("  Common columns (%d): %s\n",
                    length(vc$common_columns),
                    paste(vc$common_columns, collapse = ", ")))
        }
        if (!is.null(vc$extra_in_df1) && length(vc$extra_in_df1) > 0) {
          detailed_report <- paste0(detailed_report,
            sprintf("  Only in Base: %s\n",
                    paste(vc$extra_in_df1, collapse = ", ")))
        }
        if (!is.null(vc$extra_in_df2) && length(vc$extra_in_df2) > 0) {
          detailed_report <- paste0(detailed_report,
            sprintf("  Only in Compare: %s\n",
                    paste(vc$extra_in_df2, collapse = ", ")))
        }
        if (!is.null(vc$data_type_comparisons) && length(vc$data_type_comparisons) > 0) {
          # Show only type mismatches (not all columns)
          mismatches <- Filter(function(x) {
            !identical(x$type_df1, x$type_df2)
          }, vc$data_type_comparisons)
          if (length(mismatches) > 0) {
            detailed_report <- paste0(detailed_report, "  Type mismatches:\n")
            for (mm in mismatches) {
              detailed_report <- paste0(detailed_report,
                sprintf("    %s: %s vs %s\n",
                        mm$column,
                        paste(mm$type_df1, collapse = "/"),
                        paste(mm$type_df2, collapse = "/")))
            }
          }
        }
        detailed_report <- paste0(detailed_report, "\n")
      }
    }

    # Observation comparison
    obs_comp <- comparison_results$observation_comparison
    if (!is.null(obs_comp) && is.list(obs_comp) &&
        !is.null(obs_comp$details) && is.list(obs_comp$details) &&
        length(obs_comp$details) > 0) {
      detailed_report <- paste0(detailed_report, "Observation Differences:\n")
      for (col in names(obs_comp$details)) {
        d <- obs_comp$details[[col]]
        if (is.data.frame(d) && nrow(d) > 0) {
          detailed_report <- paste0(detailed_report, "Column: ", col, "\n")
          detailed_report <- paste0(
            detailed_report,
            paste(utils::capture.output(print(d)), collapse = "\n"),
            "\n\n"
          )
        }
      }
    }

    # Unified comparison (if available)
    unified <- comparison_results$unified_comparison
    if (!is.null(unified) && is.data.frame(unified) && nrow(unified) > 0) {
      detailed_report <- paste0(detailed_report, "Unified Comparison:\n")
      detailed_report <- paste0(
        detailed_report,
        paste(utils::capture.output(print(unified)), collapse = "\n"),
        "\n\n"
      )
    }

  } else if (is.data.frame(comparison_results)) {
    # Handle compare_datasets() output (data frame with Aspect/Description)
    for (i in seq_len(nrow(comparison_results))) {
      detailed_report <- paste0(
        detailed_report,
        comparison_results$Aspect[i], ":\n  ",
        comparison_results$Description[i], "\n\n"
      )
    }
  }

  # Output based on format
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
    writeLines(html_content, paste0(file_name, ".html"))
    message("Report saved to ", file_name, ".html")
  } else {
    stop("Unsupported output format. Choose 'text' or 'html'.")
  }

  invisible(detailed_report)
}
