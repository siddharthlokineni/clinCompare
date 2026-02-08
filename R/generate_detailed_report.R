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
#' @keywords internal
#' @examples
#' \donttest{
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

    # Observation comparison -- all variables, every difference
    obs_comp <- comparison_results$observation_comparison
    id_details <- obs_comp$id_details  # key-based matching ID columns
    n_total_obs <- comparison_results$nrow_df1
    tol_used <- comparison_results$tolerance

    # Handle skipped comparison
    if (!is.null(obs_comp$message) || !is.null(obs_comp$status)) {
      obs_msg <- if (!is.null(obs_comp$message)) obs_comp$message else "Observation comparison skipped."
      detailed_report <- paste0(detailed_report,
        "Observation Differences:\n",
        sprintf("  %s\n\n", obs_msg))
    } else if (!is.null(obs_comp) && is.list(obs_comp) &&
        !is.null(obs_comp$details) && is.list(obs_comp$details) &&
        length(obs_comp$details) > 0) {

      # --- Variable Summary Table (all columns with diffs) ---
      counts <- obs_comp$discrepancies[obs_comp$discrepancies > 0]
      counts <- sort(counts, decreasing = TRUE)

      detailed_report <- paste0(detailed_report,
        "OBSERVATION DIFFERENCES -- ALL VARIABLES\n",
        strrep("=", 80), "\n\n")

      # Tolerance note
      if (!is.null(tol_used) && tol_used > 0) {
        detailed_report <- paste0(detailed_report,
          sprintf("  Numeric tolerance (CRITERION): %g\n\n", tol_used))
      }

      # Context line
      total_diffs <- sum(obs_comp$discrepancies, na.rm = TRUE)
      cols_affected <- sum(obs_comp$discrepancies > 0)
      if (!is.null(n_total_obs) && n_total_obs > 0) {
        unique_rows <- unique(unlist(lapply(obs_comp$details, function(d) {
          if (is.data.frame(d)) d$Row else integer(0)
        })))
        pct <- round(length(unique_rows) / n_total_obs * 100, 1)
        detailed_report <- paste0(detailed_report,
          sprintf("  Total: %d value difference(s) across %d column(s); %d of %d obs (%.1f%%) differ\n\n",
                  total_diffs, cols_affected, length(unique_rows), n_total_obs, pct))
      }

      # Summary table header (with N Obs column and right-aligned CHAR dots)
      n_obs_str <- if (!is.null(n_total_obs) && n_total_obs > 0) {
        sprintf("%d", n_total_obs)
      } else {
        "?"
      }

      detailed_report <- paste0(detailed_report,
        sprintf("  %-20s %-6s %7s %8s %12s %12s %12s\n",
                "Variable", "Type", "N Obs", "N Diffs", "Max Diff", "Max % Diff", "RMS Diff"),
        "  ", strrep("-", 80), "\n")

      for (var_name in names(counts)) {
        d <- obs_comp$details[[var_name]]
        if (!is.data.frame(d) || nrow(d) == 0) next
        is_num <- is.numeric(d$Value_in_df1) && is.numeric(d$Value_in_df2)
        var_type <- if (is_num) "NUM" else "CHAR"
        if (is_num) {
          abs_diffs <- abs(d$Value_in_df1 - d$Value_in_df2)
          pct_diffs <- ifelse(d$Value_in_df1 != 0,
            abs((d$Value_in_df1 - d$Value_in_df2) / d$Value_in_df1) * 100, NA_real_)
          max_d <- max(abs_diffs, na.rm = TRUE)
          max_p <- if (any(!is.na(pct_diffs))) max(pct_diffs, na.rm = TRUE) else NA_real_
          rms_d <- sqrt(mean((d$Value_in_df1 - d$Value_in_df2)^2, na.rm = TRUE))
          detailed_report <- paste0(detailed_report,
            sprintf("  %-20s %-6s %7s %8d %12g %11.2f%% %12g\n",
                    var_name, var_type, n_obs_str, counts[var_name], max_d,
                    if (!is.na(max_p)) max_p else 0, rms_d))
        } else {
          detailed_report <- paste0(detailed_report,
            sprintf("  %-20s %-6s %7s %8d %12s %12s %12s\n",
                    var_name, var_type, n_obs_str, counts[var_name],
                    sprintf("%12s", "."), sprintf("%12s", "."), sprintf("%12s", ".")))
        }
      }
      detailed_report <- paste0(detailed_report, "\n")

      # --- Per-Variable Detailed Rows ---
      for (var_name in names(counts)) {
        d <- obs_comp$details[[var_name]]
        if (!is.data.frame(d) || nrow(d) == 0) next

        is_num <- is.numeric(d$Value_in_df1) && is.numeric(d$Value_in_df2)
        detailed_report <- paste0(detailed_report,
          strrep("-", 80), "\n",
          sprintf("Variable: %s  (%d difference(s))\n", var_name, nrow(d)),
          strrep("-", 80), "\n")

        # Build display table
        show_d <- d

        # Add Diff and PctDiff for numeric
        if (is_num) {
          show_d$Diff <- round(d$Value_in_df1 - d$Value_in_df2, 6)
          show_d$PctDiff <- round(ifelse(d$Value_in_df1 != 0,
            abs((d$Value_in_df1 - d$Value_in_df2) / d$Value_in_df1) * 100,
            NA_real_), 2)
        }

        # Prepend ID columns if key-based
        if (!is.null(id_details) && var_name %in% names(id_details)) {
          id_df <- id_details[[var_name]]
          if (is.data.frame(id_df) && nrow(id_df) == nrow(show_d)) {
            show_d <- cbind(id_df, show_d[, setdiff(names(show_d), "Row"), drop = FALSE])
          }
        }

        detailed_report <- paste0(detailed_report,
          paste(utils::capture.output(print(show_d, row.names = FALSE, right = FALSE)),
                collapse = "\n"),
          "\n")

        # Numeric summary stats per variable
        if (is_num) {
          abs_diffs <- abs(d$Value_in_df1 - d$Value_in_df2)
          pct_diffs <- ifelse(d$Value_in_df1 != 0,
            abs((d$Value_in_df1 - d$Value_in_df2) / d$Value_in_df1) * 100, NA_real_)
          rms_val <- sqrt(mean((d$Value_in_df1 - d$Value_in_df2)^2, na.rm = TRUE))
          detailed_report <- paste0(detailed_report,
            sprintf("  Max Abs Diff: %g | Mean Abs Diff: %g | RMS Diff: %g",
                    max(abs_diffs, na.rm = TRUE), mean(abs_diffs, na.rm = TRUE), rms_val))
          valid_pct <- pct_diffs[!is.na(pct_diffs)]
          if (length(valid_pct) > 0) {
            detailed_report <- paste0(detailed_report,
              sprintf(" | Max Pct Diff: %.2f%%", max(valid_pct, na.rm = TRUE)))
          }
          detailed_report <- paste0(detailed_report, "\n")
        }
        detailed_report <- paste0(detailed_report, "\n")
      }
    } else if (!is.null(obs_comp) && !is.null(obs_comp$discrepancies) &&
               sum(obs_comp$discrepancies, na.rm = TRUE) == 0) {
      detailed_report <- paste0(detailed_report,
        "Observation Differences:\n  All compared values are equal.\n\n")
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
