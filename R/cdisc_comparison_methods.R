#' Print CDISC Comparison Results
#'
#' @description
#' Prints a concise summary of CDISC comparison results. Shows dataset
#' dimensions, domain, number of differences, and a pass/fail verdict
#' based on CDISC validation errors.
#'
#' @param x A cdisc_comparison object returned by [cdisc_compare()].
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns x.
#' @export
print.cdisc_comparison <- function(x, ...) {
  cat("\n")
  cat(strrep("=", 50), "\n")
  cat("  clinCompare: CDISC Comparison Results\n")
  cat(strrep("=", 50), "\n\n")

  # Domain info
  if (!is.na(x$domain) && !is.na(x$standard)) {
    cat(sprintf("  Domain:              %s (%s)\n", x$domain, x$standard))
  } else {
    cat("  Domain:              Not detected (general comparison)\n")
  }

  # Dimensions
  cat(sprintf("  Base dataset:        %d rows x %d columns\n", x$nrow_df1, x$ncol_df1))
  cat(sprintf("  Compare dataset:     %d rows x %d columns\n", x$nrow_df2, x$ncol_df2))

  # ID vars
  if (!is.null(x$id_vars)) {
    cat(sprintf("  Matching:            key-based (%s)\n", paste(x$id_vars, collapse = ", ")))
  } else {
    cat("  Matching:            positional\n")
  }

  # Tolerance
  if (!is.null(x$tolerance) && x$tolerance > 0) {
    cat(sprintf("  Tolerance:           %g\n", x$tolerance))
  }

  cat("\n")

  # Unified differences
  obs_skipped <- !is.null(x$observation_comparison$message) ||
    !is.null(x$observation_comparison$status)
  if (!is.null(x$unified_comparison) && nrow(x$unified_comparison) > 0) {
    n_attr <- sum(x$unified_comparison$diff_type != "Value")
    n_val <- sum(x$unified_comparison$diff_type == "Value")
    if (obs_skipped && n_val == 0) {
      cat(sprintf("  Differences:         %d attribute (values not compared, see note below)\n", n_attr))
    } else {
      cat(sprintf("  Differences:         %d attribute, %d value\n", n_attr, n_val))
    }
  } else if (obs_skipped) {
    cat("  Differences:         not fully assessed (see note below)\n")
  } else {
    cat("  Differences:         0\n")
  }

  # Unmatched rows (key-based matching)
  if (!is.null(x$unmatched_rows)) {
    n_only1 <- if (!is.null(x$unmatched_rows$df1_only)) nrow(x$unmatched_rows$df1_only) else 0L
    n_only2 <- if (!is.null(x$unmatched_rows$df2_only)) nrow(x$unmatched_rows$df2_only) else 0L
    if (n_only1 > 0 || n_only2 > 0) {
      cat(sprintf("  Unmatched rows:      %d in base only, %d in compare only\n", n_only1, n_only2))
    }
  }

  cat("\n")

  # Observation-level differences
  n_total <- x$nrow_df1
  if (!is.null(x$unmatched_rows) && !is.null(x$unmatched_rows$df1_only)) {
    n_total <- n_total - nrow(x$unmatched_rows$df1_only)
  }
  .print_observation_diffs(x$observation_comparison, n = 30, n_total_obs = n_total)

  # CDISC verdict
  if (!is.na(x$domain) && !is.na(x$standard)) {
    n_err1 <- sum(x$cdisc_validation_df1$severity == "ERROR")
    n_err2 <- sum(x$cdisc_validation_df2$severity == "ERROR")
    n_warn1 <- sum(x$cdisc_validation_df1$severity == "WARNING")
    n_warn2 <- sum(x$cdisc_validation_df2$severity == "WARNING")
    total_err <- n_err1 + n_err2
    total_warn <- n_warn1 + n_warn2
    verdict <- if (total_err == 0) "PASS" else "FAIL"
    cat(sprintf("  CDISC Compliance:    %s (%d errors, %d warnings)\n", verdict, total_err, total_warn))
  }

  # Version info
  if (!is.null(x$cdisc_version) && nzchar(x$cdisc_version$version_note)) {
    cat("  ", x$cdisc_version$version_note, "\n")
  }

  cat(strrep("=", 50), "\n")

  # Smart context-aware suggestions
  has_obs_diffs <- !is.null(x$observation_comparison$discrepancies) &&
    sum(x$observation_comparison$discrepancies, na.rm = TRUE) > 0
  tips <- character()

  if (has_obs_diffs) {
    tips <- c(tips, "get_all_differences(result) -- extract all diffs as a data frame")
    tips <- c(tips, "export_report(result, \"report.html\") -- save as HTML report")
    tips <- c(tips, "export_report(result, \"report.xlsx\") -- save as Excel workbook")
  }

  # CDISC validation tips based on error/warning counts
  if (!is.na(x$domain) && !is.na(x$standard)) {
    n_err1 <- sum(x$cdisc_validation_df1$severity == "ERROR")
    n_err2 <- sum(x$cdisc_validation_df2$severity == "ERROR")
    n_warn1 <- sum(x$cdisc_validation_df1$severity == "WARNING")
    n_warn2 <- sum(x$cdisc_validation_df2$severity == "WARNING")

    if (n_err1 + n_warn1 > 0) {
      tips <- c(tips, "print_cdisc_validation(result$cdisc_validation_df1) -- base dataset issues")
    }
    if (n_err2 + n_warn2 > 0) {
      tips <- c(tips, "print_cdisc_validation(result$cdisc_validation_df2) -- compare dataset issues")
    }
    if (n_err1 + n_err2 > 0) {
      tips <- c(tips, "generate_cdisc_report(result) -- full CDISC compliance report")
    }
  }

  # Unmatched rows
  n_unmatched <- 0L
  if (!is.null(x$unmatched_rows)) {
    n_unmatched <- (if (!is.null(x$unmatched_rows$df1_only)) nrow(x$unmatched_rows$df1_only) else 0L) +
      (if (!is.null(x$unmatched_rows$df2_only)) nrow(x$unmatched_rows$df2_only) else 0L)
  }
  if (n_unmatched > 0) {
    tips <- c(tips, "result$unmatched_rows -- see rows that didn't match by key")
  }

  # Data-driven tolerance suggestion for numeric diffs
  if (has_obs_diffs && !is.null(x$tolerance) && x$tolerance == 0) {
    obs_inner <- x$observation_comparison
    all_abs_diffs <- numeric(0)
    for (d in obs_inner$details) {
      if (is.data.frame(d) && is.numeric(d$Value_in_df1) && is.numeric(d$Value_in_df2)) {
        all_abs_diffs <- c(all_abs_diffs, abs(d$Value_in_df1 - d$Value_in_df2))
      }
    }
    if (length(all_abs_diffs) > 0) {
      max_abs <- max(all_abs_diffs, na.rm = TRUE)
      if (max_abs > 0) {
        # Suggest tolerance just above the largest diff so next comparison will match
        suggested <- signif(max_abs * 1.01, 2)
        if (suggested <= max_abs) suggested <- max_abs + .Machine$double.eps^0.5
        hint <- if (max_abs < 0.01) ", likely rounding" else ""
        tips <- c(tips, sprintf(
          "cdisc_compare(..., tolerance = %g) -- largest numeric diff is %g%s",
          suggested, max_abs, hint))
      }
    }
  }

  # No diffs at all -- minimal suggestions
  if (!has_obs_diffs && n_unmatched == 0) {
    tips <- c(tips, "export_report(result, \"report.txt\") -- save confirmation to file")
  }

  if (length(tips) > 0) {
    cat("\n  Try next:\n")
    for (tip in tips) {
      cat(sprintf("    %s\n", tip))
    }
  }

  invisible(x)
}


#' Summarize CDISC Comparison Results
#'
#' @description
#' Returns a concise one-row data frame summarizing the comparison:
#' domain, standard, row/col counts, number of differences, and
#' CDISC error/warning counts.
#'
#' @param object A cdisc_comparison object returned by [cdisc_compare()].
#' @param ... Additional arguments (ignored).
#'
#' @return A one-row data frame with summary metrics.
#' @export
summary.cdisc_comparison <- function(object, ...) {
  n_unified <- if (!is.null(object$unified_comparison)) {
    nrow(object$unified_comparison)
  } else {
    0L
  }

  n_errors <- 0L
  n_warnings <- 0L
  if (!is.na(object$domain) && !is.na(object$standard)) {
    n_errors <- sum(object$cdisc_validation_df1$severity == "ERROR") +
      sum(object$cdisc_validation_df2$severity == "ERROR")
    n_warnings <- sum(object$cdisc_validation_df1$severity == "WARNING") +
      sum(object$cdisc_validation_df2$severity == "WARNING")
  }

  data.frame(
    domain = if (is.na(object$domain)) "" else object$domain,
    standard = if (is.na(object$standard)) "" else object$standard,
    rows_base = object$nrow_df1,
    rows_compare = object$nrow_df2,
    cols_base = object$ncol_df1,
    cols_compare = object$ncol_df2,
    total_differences = n_unified,
    cdisc_errors = n_errors,
    cdisc_warnings = n_warnings,
    verdict = if (n_errors == 0) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
}
