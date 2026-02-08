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
compare_datasets <- function(df1, df2) {
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
    observation_comparison <- compare_observations(df1, df2)
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
    observation_comparison = observation_comparison
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

  # Observation differences
  obs <- x$observation_comparison
  .print_observation_diffs(obs, n = 30)

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
.print_observation_diffs <- function(obs, n = 30, id_details = NULL) {
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

  # Try to determine total observations compared (for % context)
  # The max Row value in any details data frame gives us the number of rows compared
  n_obs <- 0L
  if (length(obs$details) > 0) {
    all_rows <- unlist(lapply(obs$details, function(d) if (is.data.frame(d)) max(d$Row) else 0L))
    n_obs <- max(all_rows, na.rm = TRUE)
  }
  if (n_obs > 0 && total > 0) {
    # Count unique rows with at least one difference
    unique_rows <- unique(unlist(lapply(obs$details, function(d) if (is.data.frame(d)) d$Row else integer(0))))
    pct <- round(length(unique_rows) / n_obs * 100, 1)
    cat(sprintf("Value differences: %d across %d of %d column(s); %d of %d obs (%.1f%%) differ\n",
                total, cols_affected, n_compared, length(unique_rows), n_obs, pct))
  } else {
    cat(sprintf("Value differences: %d across %d column(s)\n", total, cols_affected))
  }

  if (total == 0 || length(obs$details) == 0) return(invisible(NULL))

  # Use id_details from obs itself if not passed separately
  if (is.null(id_details) && !is.null(obs$id_details)) {
    id_details <- obs$id_details
  }

  # Find the first variable with differences (sorted by count, descending)
  counts <- obs$discrepancies[obs$discrepancies > 0]
  counts <- sort(counts, decreasing = TRUE)

  cat(sprintf("\nTop differing columns:\n"))
  show_top <- min(length(counts), 5L)
  for (i in seq_len(show_top)) {
    cat(sprintf("  %-20s %d difference(s)\n", names(counts)[i], counts[i]))
  }
  if (length(counts) > 5L) {
    cat(sprintf("  ... and %d more column(s)\n", length(counts) - 5L))
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
