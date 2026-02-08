#' Compare Two Datasets
#'
#' @description
#' Compares two datasets at three levels in a single call:
#'
#' \enumerate{
#'   \item \strong{Dataset level} — dimensions, column overlap, missing-value
#'     totals.
#'   \item \strong{Variable level} — column name discrepancies and data-type
#'     mismatches (delegates to [compare_variables()]).
#'   \item \strong{Observation level} — row-by-row value differences on common
#'     columns (delegates to [compare_observations()]). Skipped gracefully when
#'     row counts differ.
#' }
#'
#' The return value is a list with class \code{"dataset_comparison"}, which has
#' a tidy [print()] method. The same object is accepted by
#' [generate_summary_report()], [generate_detailed_report()], and
#' [compare_by_group()].
#'
#' @param df1 A data frame (the \emph{base} dataset).
#' @param df2 A data frame (the \emph{compare} dataset).
#' @param tolerance Numeric tolerance value for floating-point comparisons (default 0).
#'   When tolerance > 0, numeric values are considered equal if their absolute
#'   difference is within the tolerance threshold. Character and factor columns
#'   always use exact matching regardless of tolerance.
#'
#' @return A \code{dataset_comparison} list containing:
#'   \item{nrow_df1, ncol_df1}{Dimensions of df1.}
#'   \item{nrow_df2, ncol_df2}{Dimensions of df2.}
#'   \item{common_columns}{Character vector of columns present in both.}
#'   \item{extra_in_df1}{Columns only in df1.}
#'   \item{extra_in_df2}{Columns only in df2.}
#'   \item{type_mismatches}{Data frame of columns whose class differs
#'     (columns: \code{column}, \code{type_df1}, \code{type_df2}), or
#'     \code{NULL} if none.}
#'   \item{missing_values}{Data frame summarising NA counts per column per
#'     dataset (columns: \code{column}, \code{na_df1}, \code{na_df2}), or
#'     \code{NULL} if no missingness.}
#'   \item{variable_comparison}{Output of [compare_variables()].}
#'   \item{observation_comparison}{Output of [compare_observations()], or a
#'     list with a \code{message} element when row counts differ.}
#'
#' @export
#' @examples
#' \dontrun{
#' df1 <- data.frame(id = 1:3, val = c(10, 20, 30))
#' df2 <- data.frame(id = 1:3, val = c(10, 25, 30))
#' result <- compare_datasets(df1, df2)
#' result                              # tidy one-screen summary
#' result$observation_comparison       # drill into value diffs
#' }
compare_datasets <- function(df1, df2, tolerance = 0) {
  if (is.null(df1) || is.null(df2)) {
    stop("One or both datasets are null.")
  }

  # --- Dataset-level ---
  common_cols <- intersect(names(df1), names(df2))
  extra_df1   <- setdiff(names(df1), names(df2))
  extra_df2   <- setdiff(names(df2), names(df1))

  # Type mismatches on common columns
  type_rows <- lapply(common_cols, function(col) {
    t1 <- class(df1[[col]])[1L]
    t2 <- class(df2[[col]])[1L]
    if (t1 != t2) {
      data.frame(column = col, type_df1 = t1, type_df2 = t2,
                 stringsAsFactors = FALSE)
    }
  })
  type_mismatches <- do.call(rbind, Filter(Negate(is.null), type_rows))

  # Missing-value summary
  na_rows <- lapply(common_cols, function(col) {
    n1 <- sum(is.na(df1[[col]]))
    n2 <- sum(is.na(df2[[col]]))
    if (n1 > 0 || n2 > 0) {
      data.frame(column = col, na_df1 = n1, na_df2 = n2,
                 stringsAsFactors = FALSE)
    }
  })
  missing_values <- do.call(rbind, Filter(Negate(is.null), na_rows))

  # --- Variable-level ---
  variable_comparison <- compare_variables(df1, df2)

  # --- Observation-level (graceful when rows differ) ---
  if (nrow(df1) == nrow(df2) && length(common_cols) > 0) {
    observation_comparison <- compare_observations(df1, df2, tolerance = tolerance)
  } else {
    reason <- if (nrow(df1) != nrow(df2)) {
      sprintf("Row counts differ (%d vs %d); positional comparison skipped.",
              nrow(df1), nrow(df2))
    } else {
      "No common columns; observation comparison skipped."
    }
    observation_comparison <- list(
      discrepancies = integer(0),
      details = list(),
      message = reason
    )
  }

  result <- list(
    nrow_df1               = nrow(df1),
    ncol_df1               = ncol(df1),
    nrow_df2               = nrow(df2),
    ncol_df2               = ncol(df2),
    common_columns         = common_cols,
    extra_in_df1           = extra_df1,
    extra_in_df2           = extra_df2,
    type_mismatches        = type_mismatches,
    missing_values         = missing_values,
    variable_comparison    = variable_comparison,
    observation_comparison = observation_comparison,
    tolerance              = tolerance
  )
  class(result) <- "dataset_comparison"
  result
}


#' Print Dataset Comparison Results
#'
#' @param x A \code{dataset_comparison} object from [compare_datasets()].
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.dataset_comparison <- function(x, ...) {
  cat("clinCompare: Dataset Comparison\n")
  cat(strrep("-", 40), "\n")

  # Dimensions
  cat(sprintf("Base:    %d rows x %d cols\n", x$nrow_df1, x$ncol_df1))
  cat(sprintf("Compare: %d rows x %d cols\n", x$nrow_df2, x$ncol_df2))

  # Columns
  cat(sprintf("Columns: %d common", length(x$common_columns)))
  if (length(x$extra_in_df1) > 0 || length(x$extra_in_df2) > 0) {
    cat(sprintf(", %d only in base, %d only in compare",
                length(x$extra_in_df1), length(x$extra_in_df2)))
  }
  cat("\n")

  # Type mismatches
  if (!is.null(x$type_mismatches) && nrow(x$type_mismatches) > 0) {
    cat(sprintf("Type mismatches: %d\n", nrow(x$type_mismatches)))
  }

  # Missing values
  if (!is.null(x$missing_values) && nrow(x$missing_values) > 0) {
    cat(sprintf("Columns with NAs: %d\n", nrow(x$missing_values)))
  }

  # Tolerance
  if (!is.null(x$tolerance) && x$tolerance > 0) {
    cat(sprintf("Tolerance (CRITERION): %g\n", x$tolerance))
  }

  # Observation differences
  obs <- x$observation_comparison
  # Pass actual row count for correct denominator (not max(Row) from diffs)
  .print_observation_diffs(obs, n = 30, n_total_obs = x$nrow_df1)

  cat(strrep("-", 40), "\n")
  invisible(x)
}


#' Print Observation-Level Differences (Internal Helper)
#'
#' @description
#' Shared helper used by both \code{print.dataset_comparison} and
#' \code{print.cdisc_comparison}. Prints a summary line, names the first
#' variable with differences, and shows up to \code{n} rows of that
#' variable's differing observations.
#'
#' @param obs Observation comparison list (with \code{discrepancies},
#'   \code{details}, and optionally \code{id_details} and \code{message}).
#' @param n Maximum number of differing rows to display (default 30).
#' @param id_details Optional named list of ID detail data frames
#'   (from key-based comparison).
#'
#' @return Called for side effects (prints to console). Returns NULL invisibly.
#' @keywords internal
.print_observation_diffs <- function(obs, n = 30, id_details = NULL, n_total_obs = NULL) {
  if (is.null(obs)) return(invisible(NULL))

  # If observation comparison was skipped, print the reason
  if (!is.null(obs$message)) {
    cat(obs$message, "\n")
    return(invisible(NULL))
  }

  if (is.null(obs$discrepancies)) return(invisible(NULL))

  total <- sum(obs$discrepancies, na.rm = TRUE)
  cols_affected <- sum(obs$discrepancies > 0)
  n_compared <- length(obs$discrepancies)  # number of columns compared

  if (!is.null(n_total_obs) && n_total_obs > 0 && total > 0 && length(obs$details) > 0) {
    # Count unique rows (observations) with at least one difference
    unique_rows <- unique(unlist(lapply(obs$details, function(d) {
      if (is.data.frame(d)) d$Row else integer(0)
    })))
    pct <- round(length(unique_rows) / n_total_obs * 100, 1)
    cat(sprintf("Value differences: %d across %d of %d column(s); %d of %d obs (%.1f%%) differ\n",
                total, cols_affected, n_compared, length(unique_rows), n_total_obs, pct))
  } else {
    cat(sprintf("Value differences: %d across %d column(s)\n", total, cols_affected))
  }

  if (total == 0 || length(obs$details) == 0) return(invisible(NULL))

  # Use id_details from obs itself if not passed separately
  if (is.null(id_details) && !is.null(obs$id_details)) {
    id_details <- obs$id_details
  }

  # Find variables with differences (sorted by count, descending)
  counts <- obs$discrepancies[obs$discrepancies > 0]
  counts <- sort(counts, decreasing = TRUE)

  # Build PROC COMPARE-style summary table for ALL variables with differences
  # (includes N Obs, N Diffs, Max Diff, Max % Diff, RMS Diff per variable)
  cat(sprintf("\nVariable Summary of Differences:\n"))

  # Prepare table data
  summary_rows <- list()
  for (var_name in names(counts)) {
    var_data <- obs$details[[var_name]]
    if (!is.data.frame(var_data)) next

    n_diffs <- counts[var_name]

    # Determine type
    v1 <- var_data$Value_in_df1
    is_numeric <- is.numeric(v1)
    var_type <- if (is_numeric) "NUM" else "CHAR"

    # Compute statistics for numeric variables
    if (is_numeric) {
      v2 <- var_data$Value_in_df2

      # Max absolute difference
      abs_diffs <- abs(v1 - v2)
      max_diff <- max(abs_diffs, na.rm = TRUE)

      # Max percent difference
      pct_diffs <- ifelse(v1 != 0, abs((v1 - v2) / v1) * 100, NA)
      max_pct <- max(pct_diffs, na.rm = TRUE)

      # RMS difference
      rms <- sqrt(mean((v1 - v2)^2, na.rm = TRUE))

      summary_rows[[var_name]] <- data.frame(
        Variable = var_name,
        Type = var_type,
        N_Diffs = n_diffs,
        Max_Diff = max_diff,
        Max_Pct_Diff = max_pct,
        RMS_Diff = rms,
        stringsAsFactors = FALSE
      )
    } else {
      # Character/factor variables: no numeric statistics
      summary_rows[[var_name]] <- data.frame(
        Variable = var_name,
        Type = var_type,
        N_Diffs = n_diffs,
        Max_Diff = NA,
        Max_Pct_Diff = NA,
        RMS_Diff = NA,
        stringsAsFactors = FALSE
      )
    }
  }

  # Bind rows and print formatted table
  if (length(summary_rows) > 0) {
    summary_df <- do.call(rbind, summary_rows)
    rownames(summary_df) <- NULL

    # N Obs compared (total observations for context)
    n_obs_str <- if (!is.null(n_total_obs) && n_total_obs > 0) {
      sprintf("%d", n_total_obs)
    } else {
      "?"
    }

    # Header row with right-aligned numeric columns
    cat(sprintf("  %-20s %-6s %7s %8s %10s %10s %10s\n",
                "Variable", "Type", "N Obs", "N Diffs", "Max Diff", "Max % Diff", "RMS Diff"))
    cat(sprintf("  %s\n", strrep("-", 76)))

    for (i in seq_len(nrow(summary_df))) {
      row <- summary_df[i, ]
      max_diff_str <- if (is.na(row$Max_Diff)) sprintf("%10s", ".") else sprintf("%10.4g", row$Max_Diff)
      max_pct_str  <- if (is.na(row$Max_Pct_Diff)) sprintf("%10s", ".") else sprintf("%9.2f%%", row$Max_Pct_Diff)
      rms_str      <- if (is.na(row$RMS_Diff)) sprintf("%10s", ".") else sprintf("%10.4g", row$RMS_Diff)

      cat(sprintf("  %-20s %-6s %7s %8d %s %s %s\n",
                  row$Variable, row$Type, n_obs_str, row$N_Diffs,
                  max_diff_str, max_pct_str, rms_str))
    }
  }

  # Show first variable's rows
  first_var <- names(counts)[1L]
  diffs_df <- obs$details[[first_var]]
  if (is.null(diffs_df) || !is.data.frame(diffs_df) || nrow(diffs_df) == 0) {
    return(invisible(NULL))
  }

  show_n <- min(nrow(diffs_df), n)
  cat(sprintf("\nFirst %d observation(s) differing in '%s':\n", show_n, first_var))

  # Build display table
  display <- diffs_df[seq_len(show_n), , drop = FALSE]

  # Check if values are numeric to add Diff and PctDiff columns
  v1_all <- diffs_df$Value_in_df1
  v2_all <- diffs_df$Value_in_df2
  is_num <- is.numeric(v1_all) && is.numeric(v2_all)

  if (is_num) {
    abs_diff <- v1_all - v2_all
    pct_diff <- ifelse(v1_all != 0, abs(abs_diff / v1_all) * 100, NA_real_)

    display$Diff    <- round(abs_diff[seq_len(show_n)], 4)
    display$PctDiff <- round(pct_diff[seq_len(show_n)], 2)
  }

  # Prepend ID columns if available (key-based matching)
  # and drop the meaningless "Row" column since keys identify the record
  if (!is.null(id_details) && first_var %in% names(id_details)) {
    id_df <- id_details[[first_var]]
    if (is.data.frame(id_df) && nrow(id_df) >= show_n) {
      display <- cbind(id_df[seq_len(show_n), , drop = FALSE],
                        display[, setdiff(names(display), "Row"), drop = FALSE])
    }
  }

  # Print as aligned table
  print(display, row.names = FALSE, right = FALSE)

  if (nrow(diffs_df) > n) {
    cat(sprintf("... %d more row(s) not shown. Access via $observation_comparison$details$%s\n",
                nrow(diffs_df) - n, first_var))
  }

  # Print numeric summary statistics (like SAS PROC COMPARE)
  if (is_num) {
    cat(sprintf("\nDifference statistics for '%s' (%d differences):\n", first_var, length(abs_diff)))
    cat(sprintf("  Max Abs Diff:  %g\n", max(abs(abs_diff), na.rm = TRUE)))
    cat(sprintf("  Mean Abs Diff: %g\n", mean(abs(abs_diff), na.rm = TRUE)))
    valid_pct <- pct_diff[!is.na(pct_diff)]
    if (length(valid_pct) > 0) {
      cat(sprintf("  Max Pct Diff:  %.2f%%\n", max(valid_pct, na.rm = TRUE)))
      cat(sprintf("  Mean Pct Diff: %.2f%%\n", mean(valid_pct, na.rm = TRUE)))
    }
  }

  invisible(NULL)
}


#' Extract All Differences as a Unified Data Frame
#'
#' @description
#' Converts per-variable observation differences into a single long-format
#' data frame suitable for filtering with dplyr, writing to CSV, or
#' programmatic analysis. This is the R equivalent of SAS PROC COMPARE's
#' \code{OUT=} dataset with \code{_TYPE_} and \code{_DIF_} variables.
#'
#' Accepts output from [compare_datasets()], [cdisc_compare()], or any list
#' containing an \code{observation_comparison} element with the standard
#' \code{discrepancies} / \code{details} / \code{id_details} structure.
#'
#' @param comparison_results A \code{dataset_comparison} or
#'   \code{cdisc_comparison} object, or any list with an
#'   \code{observation_comparison} element.
#'
#' @return A data frame with one row per differing cell. Columns:
#'   \describe{
#'     \item{Variable}{Character: column name where the difference was found.}
#'     \item{Row}{Integer: row index in df1 (positional matching).}
#'     \item{Base}{The value in df1 (base dataset).}
#'     \item{Compare}{The value in df2 (compare dataset).}
#'     \item{Diff}{Numeric: Base - Compare (NA for character columns).}
#'     \item{PctDiff}{Numeric: absolute percentage difference relative to
#'       Base (NA when Base is 0 or column is character).}
#'   }
#'   When key-based matching was used (id_vars), the ID columns are
#'   prepended to the left of the data frame.
#'
#'   Returns an empty data frame with the expected columns when no
#'   differences exist or observation comparison was skipped.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- compare_datasets(df1, df2)
#' diffs <- get_all_differences(result)
#' head(diffs)
#'
#' # Filter to a specific variable
#' dplyr::filter(diffs, Variable == "AVAL")
#'
#' # Write to CSV
#' write.csv(diffs, "all_diffs.csv", row.names = FALSE)
#' }
get_all_differences <- function(comparison_results) {
  obs <- comparison_results$observation_comparison

  # Empty scaffold
  empty_df <- data.frame(
    Variable = character(0),
    Row      = integer(0),
    Base     = character(0),
    Compare  = character(0),
    Diff     = numeric(0),
    PctDiff  = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(obs)) return(empty_df)
  if (!is.null(obs$message) || !is.null(obs$status)) return(empty_df)
  if (is.null(obs$details) || length(obs$details) == 0) return(empty_df)

  id_details <- obs$id_details
  has_id_cols <- !is.null(id_details) && length(id_details) > 0
  all_rows <- list()

  for (var_name in names(obs$details)) {
    d <- obs$details[[var_name]]
    if (!is.data.frame(d) || nrow(d) == 0) next

    is_num <- is.numeric(d$Value_in_df1) && is.numeric(d$Value_in_df2)

    row_df <- data.frame(
      Variable = rep(var_name, nrow(d)),
      Row      = d$Row,
      Base     = as.character(d$Value_in_df1),
      Compare  = as.character(d$Value_in_df2),
      stringsAsFactors = FALSE
    )

    if (is_num) {
      row_df$Diff <- d$Value_in_df1 - d$Value_in_df2
      row_df$PctDiff <- ifelse(d$Value_in_df1 != 0,
        abs((d$Value_in_df1 - d$Value_in_df2) / d$Value_in_df1) * 100,
        NA_real_)
    } else {
      row_df$Diff    <- NA_real_
      row_df$PctDiff <- NA_real_
    }

    # Prepend ID columns if available (key-based matching)
    # and drop the meaningless Row column since keys identify the record
    if (has_id_cols && var_name %in% names(id_details)) {
      id_df <- id_details[[var_name]]
      if (is.data.frame(id_df) && nrow(id_df) == nrow(row_df)) {
        row_df <- cbind(id_df, row_df[, setdiff(names(row_df), "Row"), drop = FALSE])
      }
    }

    all_rows[[length(all_rows) + 1L]] <- row_df
  }

  if (length(all_rows) == 0) return(empty_df)

  # Combine — rbind with fill for ID columns that may differ across variables
  all_cols <- unique(unlist(lapply(all_rows, names)))
  unified <- do.call(rbind, lapply(all_rows, function(r) {
    missing_cols <- setdiff(all_cols, names(r))
    for (mc in missing_cols) r[[mc]] <- NA
    r[, all_cols, drop = FALSE]
  }))

  rownames(unified) <- NULL
  unified
}
