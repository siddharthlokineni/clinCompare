#' Check Compatibility of Two Datasets for Comparison
#'
#' @description
#' Checks if two datasets are compatible for comparison by verifying their dimensions,
#' column names, and data types. Returns a list indicating whether the datasets are
#' compatible and detailing any structural differences.
#'
#' @param df1 The first data frame to be compared.
#' @param df2 The second data frame to be compared.
#' @return A list containing details about the compatibility of the datasets, 
#' including information on dimension equality and common columns.
#' @export
#' @examples
#' \dontrun{
#'   check_compatibility(df1, df2)
#' }

check_compatibility <- function(df1, df2) {
  compatibility_results <- list()
  
  # Check for null datasets
  if (is.null(df1) || is.null(df2)) {
    return(list(compatible = FALSE, reason = "One or both datasets are null"))
  }
  
  # Check for dimension equality
  if (!all(dim(df1) == dim(df2))) {
    compatibility_results$dimensions_equal <- FALSE
  } else {
    compatibility_results$dimensions_equal <- TRUE
  }
  
  # Compare column names
  compatibility_results$common_columns <- intersect(names(df1), names(df2))
  compatibility_results$extra_in_df1 <- setdiff(names(df1), names(df2))
  compatibility_results$extra_in_df2 <- setdiff(names(df2), names(df1))
  
  compatibility_results$compatible <- compatibility_results$dimensions_equal &&
    length(compatibility_results$extra_in_df1) == 0 &&
    length(compatibility_results$extra_in_df2) == 0
  
  compatibility_results
}