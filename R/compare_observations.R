#' Compare Observations of Two Datasets
#'
#' @description
#' Performs row-by-row comparison of two datasets on common columns, identifying
#' specific value differences at the cell level. Returns discrepancy counts and details
#' showing which rows differ and how their values diverge.
#'
#' @param df1 A data frame representing the first dataset.
#' @param df2 A data frame representing the second dataset.
#' @return A list containing discrepancy counts and details of row differences.
#' @export
#' @examples
#' \dontrun{
#'   compare_observations(df1, df2)
#' }

compare_observations <- function(df1, df2) {
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
    } else {
      df1_col <- df1[[col]]
      df2_col <- df2[[col]]
    }

    differences <- which(df1_col != df2_col)
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
