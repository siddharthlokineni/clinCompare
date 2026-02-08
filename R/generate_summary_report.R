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

  # Handle cdisc_compare() or compare_datasets() output (list with named fields)
  if (is.list(comparison_results) && !is.data.frame(comparison_results)) {
    cr <- comparison_results

    # --- Data Set Summary ---
    if (!is.null(cr$nrow_df1)) {
      summary_report <- paste0(summary_report,
        "DATA SET SUMMARY\n",
        sprintf("  %-20s %10s %12s\n", "Dataset", "Variables", "Observations"),
        sprintf("  %-20s %10d %12d\n", "Base (df1)", cr$ncol_df1, cr$nrow_df1),
        sprintf("  %-20s %10d %12d\n", "Compare (df2)", cr$ncol_df2, cr$nrow_df2),
        "\n")
    }

    # --- Variable Summary ---
    var_comp <- cr$variable_comparison
    common_cols <- if (!is.null(cr$common_columns)) cr$common_columns else NULL
    extra_df1 <- NULL
    extra_df2 <- NULL
    n_type_mismatch <- 0L

    if (!is.null(var_comp) && is.list(var_comp)) {
      vc <- if (!is.null(var_comp$details)) var_comp$details else var_comp
      if (!is.null(vc$common_columns)) common_cols <- vc$common_columns
      if (!is.null(vc$extra_in_df1)) extra_df1 <- vc$extra_in_df1
      if (!is.null(vc$extra_in_df2)) extra_df2 <- vc$extra_in_df2
      if (!is.null(vc$data_type_comparisons)) {
        n_type_mismatch <- sum(vapply(vc$data_type_comparisons, function(x) {
          !identical(x$type_df1, x$type_df2)
        }, logical(1)))
      }
    }
    # Fall back to dataset_comparison fields
    if (is.null(common_cols) && !is.null(cr$common_columns)) {
      common_cols <- cr$common_columns
    }
    if (is.null(extra_df1) && !is.null(cr$extra_in_df1)) extra_df1 <- cr$extra_in_df1
    if (is.null(extra_df2) && !is.null(cr$extra_in_df2)) extra_df2 <- cr$extra_in_df2
    if (n_type_mismatch == 0 && !is.null(cr$type_mismatches) && is.data.frame(cr$type_mismatches)) {
      n_type_mismatch <- nrow(cr$type_mismatches)
    }

    n_common <- if (!is.null(common_cols)) length(common_cols) else 0L
    n_extra1 <- if (!is.null(extra_df1)) length(extra_df1) else 0L
    n_extra2 <- if (!is.null(extra_df2)) length(extra_df2) else 0L

    summary_report <- paste0(summary_report,
      "VARIABLE SUMMARY\n",
      sprintf("  Common variables:          %d\n", n_common),
      sprintf("  Variables only in Base:    %d\n", n_extra1),
      sprintf("  Variables only in Compare: %d\n", n_extra2),
      sprintf("  Type mismatches:           %d\n", n_type_mismatch),
      "\n")

    # --- Observation Summary ---
    obs_comp <- cr$observation_comparison
    obs_skipped <- !is.null(obs_comp$message) || !is.null(obs_comp$status)

    if (obs_skipped) {
      obs_msg <- if (!is.null(obs_comp$message)) obs_comp$message else "Observation comparison skipped."
      summary_report <- paste0(summary_report,
        "OBSERVATION SUMMARY\n",
        sprintf("  %s\n", obs_msg),
        "\n")
    } else if (!is.null(obs_comp$discrepancies)) {
      total_val_diffs <- sum(obs_comp$discrepancies, na.rm = TRUE)
      cols_with_diffs <- sum(obs_comp$discrepancies > 0)
      n_total <- if (!is.null(cr$nrow_df1)) cr$nrow_df1 else 0L

      if (total_val_diffs > 0 && length(obs_comp$details) > 0) {
        unique_rows <- unique(unlist(lapply(obs_comp$details, function(d) {
          if (is.data.frame(d)) d$Row else integer(0)
        })))
        n_diff_obs <- length(unique_rows)
      } else {
        n_diff_obs <- 0L
      }

      summary_report <- paste0(summary_report,
        "OBSERVATION SUMMARY\n",
        sprintf("  Observations compared:     %d\n", n_total),
        sprintf("  Observations with diffs:   %d", n_diff_obs))
      if (n_total > 0 && n_diff_obs > 0) {
        summary_report <- paste0(summary_report,
          sprintf(" (%.1f%%)", n_diff_obs / n_total * 100))
      }
      summary_report <- paste0(summary_report, "\n",
        sprintf("  Total value differences:   %d across %d column(s)\n", total_val_diffs, cols_with_diffs))

      # Numeric diff statistics for each column with differences
      if (length(obs_comp$details) > 0) {
        for (col_name in names(obs_comp$details)) {
          d <- obs_comp$details[[col_name]]
          if (!is.data.frame(d) || nrow(d) == 0) next
          if (is.numeric(d$Value_in_df1) && is.numeric(d$Value_in_df2)) {
            abs_diffs <- abs(d$Value_in_df1 - d$Value_in_df2)
            summary_report <- paste0(summary_report,
              sprintf("    %-20s %d diffs | Max Abs Diff: %g | Mean Abs Diff: %g\n",
                      col_name, nrow(d), max(abs_diffs), round(mean(abs_diffs), 4)))
          } else {
            summary_report <- paste0(summary_report,
              sprintf("    %-20s %d diffs\n", col_name, nrow(d)))
          }
        }
      }
      summary_report <- paste0(summary_report, "\n")
    }

    # --- Unmatched rows (key-based) ---
    if (!is.null(cr$unmatched_rows)) {
      n1 <- if (!is.null(cr$unmatched_rows$df1_only)) nrow(cr$unmatched_rows$df1_only) else 0L
      n2 <- if (!is.null(cr$unmatched_rows$df2_only)) nrow(cr$unmatched_rows$df2_only) else 0L
      if (n1 > 0 || n2 > 0) {
        summary_report <- paste0(summary_report,
          "UNMATCHED ROWS\n",
          sprintf("  Only in Base:    %d\n", n1),
          sprintf("  Only in Compare: %d\n", n2),
          "\n")
      }
    }

    # --- CDISC summary (if available) ---
    if (!is.null(cr$domain) && !is.na(cr$domain)) {
      n_err <- sum(cr$cdisc_validation_df1$severity == "ERROR") +
        sum(cr$cdisc_validation_df2$severity == "ERROR")
      n_warn <- sum(cr$cdisc_validation_df1$severity == "WARNING") +
        sum(cr$cdisc_validation_df2$severity == "WARNING")
      verdict <- if (n_err == 0) "PASS" else "FAIL"
      summary_report <- paste0(summary_report,
        sprintf("CDISC VERDICT: %s (%d errors, %d warnings)\n", verdict, n_err, n_warn))
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
