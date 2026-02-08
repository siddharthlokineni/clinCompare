#' Clean Dataset
#'
#' @description
#' Removes duplicate rows, standardizes column names and text values to uppercase
#' or lowercase, and performs basic data cleaning on a data frame.
#'
#' @param df A data frame to be cleaned.
#' @param variables Optional; a vector of variable names to specifically clean.
#' If NULL, applies cleaning to all variables.
#' @param remove_duplicates Logical; whether to remove duplicate rows.
#' @param convert_to_case Optional; convert character variables to "lower" or "upper" case.
#' @return A cleaned data frame.
#' @export
#' @examples
#' \donttest{
#'   clean_dataset(df, variables = c("var1", "var2"),
#'     remove_duplicates = TRUE, convert_to_case = "lower")
#' }

clean_dataset <- function(df, variables = NULL, remove_duplicates = TRUE, convert_to_case = NULL) {
  # If no specific variables are specified, apply to all columns
  if (is.null(variables)) {
    variables <- names(df)
  }

  # Remove duplicate rows (once, before variable-level operations)
  if (remove_duplicates) {
    df <- df[!duplicated(df), , drop = FALSE]
  }

  # Apply case conversion to specified character variables
  if (!is.null(convert_to_case)) {
    for (var in variables) {
      if (var %in% names(df) && is.character(df[[var]])) {
        if (tolower(convert_to_case) == "lower") {
          df[[var]] <- tolower(df[[var]])
        } else if (tolower(convert_to_case) == "upper") {
          df[[var]] <- toupper(df[[var]])
        } else {
          warning("Invalid 'convert_to_case' value. Use 'lower' or 'upper'.")
        }
      } else if (!var %in% names(df)) {
        warning(paste("Variable", var, "not found in the dataset."))
      }
    }
  }

  df
}
