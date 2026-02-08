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
  if (!is.null(obs$message)) {
    cat(obs$message, "\n")
  } else if (!is.null(obs$discrepancies)) {
    total <- sum(obs$discrepancies, na.rm = TRUE)
    cols_affected <- sum(obs$discrepancies > 0)
    cat(sprintf("Value differences: %d across %d column(s)\n",
                total, cols_affected))
  }

  cat(strrep("-", 40), "\n")
  invisible(x)
}
