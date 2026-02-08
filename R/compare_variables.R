#' Compare Variables of Two Datasets
#'
#' @description
#' Compares the structural attributes of two datasets including column names,
#' data types, and variable ordering. Identifies common columns and reports columns
#' that exist in only one dataset.
#'
#' @param df1 A data frame representing the first dataset.
#' @param df2 A data frame representing the second dataset.
#' @return A list containing variable comparison details and discrepancy count.
#' @export
#' @examples
#' \donttest{
#'   compare_variables(df1, df2)
#' }

compare_variables <- function(df1, df2) {
  # Initialize a list to hold the results
  variable_comparisons <- list()

  # Compare column names
  variable_comparisons$common_columns <- intersect(names(df1), names(df2))
  variable_comparisons$extra_in_df1 <- setdiff(names(df1), names(df2))
  variable_comparisons$extra_in_df2 <- setdiff(names(df2), names(df1))

  # Compare data types for common columns
  common_cols <- variable_comparisons$common_columns
  data_type_comparisons <- lapply(common_cols, function(col) {
    list(column = col,
         type_df1 = class(df1[[col]]),
         type_df2 = class(df2[[col]]))
  })

  # Add data type comparisons to the results
  variable_comparisons$data_type_comparisons <- data_type_comparisons

  # Calculate the discrepancy count
  discrepancy_count <- length(variable_comparisons$extra_in_df1) + length(variable_comparisons$extra_in_df2)

  # Return the results with discrepancy count
  list(discrepancies = discrepancy_count, details = variable_comparisons)
}
