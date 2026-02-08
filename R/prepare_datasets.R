#' Prepare Datasets for Comparison
#'
#' @description
#' Prepares two datasets for comparison by optionally sorting by specified columns
#' and filtering rows based on a condition.
#'
#' @param df1 First dataset to be prepared.
#' @param df2 Second dataset to be prepared.
#' @param sort_columns Columns to sort the datasets by.
#' @param filter_criteria Criteria for filtering the datasets.
#' @return A list containing two prepared datasets.
#' @importFrom dplyr arrange filter
#' @importFrom rlang syms parse_expr
#' @export
#' @examples
#' \donttest{
#'   df1 <- data.frame(id = c(3, 1, 2), score = c(70, 90, 80))
#'   df2 <- data.frame(id = c(2, 3, 1), score = c(80, 75, 90))
#'   prepare_datasets(df1, df2, sort_columns = "id", filter_criteria = "score > 75")
#' }

prepare_datasets <- function(df1, df2, sort_columns = NULL, filter_criteria = NULL) {
  if (!is.null(sort_columns)) {
    if (all(sort_columns %in% names(df1)) && all(sort_columns %in% names(df2))) {
      df1 <- dplyr::arrange(df1, !!!rlang::syms(sort_columns))
      df2 <- dplyr::arrange(df2, !!!rlang::syms(sort_columns))
    } else {
      warning("Some sorting columns are not present in the datasets.")
    }
  }

  if (!is.null(filter_criteria)) {
    if (ncol(df1) > 0) {
      df1 <- dplyr::filter(df1, !!rlang::parse_expr(filter_criteria))
    }
    if (ncol(df2) > 0) {
      df2 <- dplyr::filter(df2, !!rlang::parse_expr(filter_criteria))
    }
  }

  list(df1 = df1, df2 = df2)
}
