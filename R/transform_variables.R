#' Transform Variables in a Dataset
#'
#' @description
#' Applies mathematical or logical transformations to specified columns in a data frame
#' based on a named list of transformation functions.
#'
#' @param df A data frame containing the variables to be transformed.
#' @param transformations A list of functions for transforming the variables.
#' The names of the list should correspond to the variable names in the dataset.
#' @return A data frame with transformed variables.
#' @keywords internal

transform_variables <- function(df, transformations) {
  for (var in names(transformations)) {
    if (var %in% names(df)) {
      # Applying the transformation
      transform_function <- transformations[[var]]
      df[[var]] <- transform_function(df[[var]])
    } else {
      warning(paste("Variable", var, "not found in the dataset."))
    }
  }
  df
}