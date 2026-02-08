#' Compare Observations of Two Datasets
#'
#' @description
#' Performs row-by-row comparison of two datasets on common columns, identifying
#' specific value differences at the cell level. Returns discrepancy counts and details
#' showing which rows differ and how their values diverge.
#'
#' @param df1 A data frame representing the first dataset.
#' @param df2 A data frame representing the second dataset.
#' @param tolerance Numeric tolerance value for floating-point comparisons (default 0).
#'   When tolerance > 0, numeric values are considered equal if their absolute
#'   difference is within the tolerance threshold. Character and factor columns
#'   always use exact matching regardless of tolerance.
#' @return A list containing discrepancy counts and details of row differences.
#' @export
#' @examples
#' \donttest{
#'   compare_observations(df1, df2)
#'   compare_observations(df1, df2, tolerance = 0.00001)
#' }

compare_observations <- function(df1, df2, tolerance = 0) {
  # Validate tolerance
  if (!is.numeric(tolerance) || length(tolerance) != 1 || is.na(tolerance) || tolerance < 0 || is.infinite(tolerance)) {
    stop("tolerance must be a single non-negative finite number", call. = FALSE)
  }

  if (nrow(df1) != nrow(df2)) {
    stop("The datasets have different numbers of rows.")
  }

  common_cols <- intersect(names(df1), names(df2))

  discrepancy_counts <- integer(length = length(common_cols))
  names(discrepancy_counts) <- common_cols
  row_differences <- list()

  for (col in common_cols) {
    if (is.factor(df1[[col]]) || is.factor(df2[[col]])) {
      # Convert factors to characters to compare
      df1_col <- as.character(df1[[col]])
      df2_col <- as.character(df2[[col]])
      is_numeric_col <- FALSE
    } else {
      df1_col <- df1[[col]]
      df2_col <- df2[[col]]
      is_numeric_col <- is.numeric(df1_col) && is.numeric(df2_col)
    }

    # Handle NA comparisons explicitly:
    # NA vs NA = match, NA vs value = difference, value vs NA = difference
    both_na <- is.na(df1_col) & is.na(df2_col)
    either_na <- is.na(df1_col) | is.na(df2_col)
    na_mismatch <- either_na & !both_na

    # For numeric columns, detect Inf-Inf as a difference (Inf - Inf = NaN, not a match)
    if (is_numeric_col && tolerance > 0) {
      raw_diff <- abs(df1_col - df2_col)
      value_mismatch <- !either_na & (raw_diff > tolerance | is.nan(raw_diff))
    } else if (is_numeric_col) {
      inf_mismatch <- !either_na & (is.infinite(df1_col) | is.infinite(df2_col)) &
                      is.nan(df1_col - df2_col)
      value_mismatch <- !either_na & (df1_col != df2_col) | inf_mismatch
    } else {
      value_mismatch <- !either_na & (df1_col != df2_col)
    }

    differences <- which(na_mismatch | value_mismatch)
    discrepancy_counts[col] <- length(differences)

    if (length(differences) > 0) {
      row_differences[[col]] <- data.frame(
        Row = differences,
        Value_in_df1 = df1_col[differences],
        Value_in_df2 = df2_col[differences]
      )
    }
  }

  list(discrepancies = discrepancy_counts, details = row_differences)
}
