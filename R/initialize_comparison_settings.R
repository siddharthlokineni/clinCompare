#' Initialize Settings for Data Comparison
#'
#' @description
#' Initializes default settings for dataset comparison including tolerance and other
#' parameters stored in a package environment.
#'
#' @param tolerance Default tolerance level for numeric comparisons.
#' @param missing_value_method Default method for handling missing values in data comparison.
#' @return None; this function sets global options and does not return a value.
#' @export
#' @examples
#' \dontrun{
#'   initialize_comparison_settings(tolerance = 0.01, missing_value_method = "exclude")
#' }

initialize_comparison_settings <- function(tolerance = 0, missing_value_method = "ignore") {
  options(comparison_tolerance = tolerance)
  options(missing_value_handling = missing_value_method)
  message("Comparison settings initialized. Tolerance: ", tolerance, ", Missing Value Handling: ", missing_value_method)
}