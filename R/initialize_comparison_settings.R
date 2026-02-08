#' Package-Level Settings Environment
#'
#' @description
#' Internal environment used to store package settings without modifying
#' global options.
#'
#' @keywords internal
.clincompare_env <- new.env(parent = emptyenv())
.clincompare_env$tolerance <- 0
.clincompare_env$missing_value_handling <- "ignore"

#' Initialize Settings for Data Comparison
#'
#' @description
#' Initializes default settings for dataset comparison including tolerance and other
#' parameters stored in a package environment.
#'
#' @param tolerance Default tolerance level for numeric comparisons.
#' @param missing_value_method Default method for handling missing values in data comparison.
#' @return Invisible \code{NULL}. Called for its side effect of updating package settings.
#' @keywords internal
#' @examples
#' \donttest{
#'   initialize_comparison_settings(tolerance = 0.01, missing_value_method = "exclude")
#' }
initialize_comparison_settings <- function(tolerance = 0, missing_value_method = "ignore") {
  .clincompare_env$tolerance <- tolerance
  .clincompare_env$missing_value_handling <- missing_value_method
  message("Comparison settings initialized. Tolerance: ", tolerance, ", Missing Value Handling: ", missing_value_method)
  invisible(NULL)
}
