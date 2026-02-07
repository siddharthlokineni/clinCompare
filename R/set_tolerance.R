#' Set Tolerance Level for Comparisons
#'
#' @description
#' Sets the numeric tolerance for floating-point comparisons, allowing small differences
#' within the tolerance to be treated as equal.
#'
#' @param tolerance A non-negative numeric value specifying the tolerance level.
#' @return None; this function sets an option and does not return a value.
#' @export
#' @examples
#' \dontrun{
#' set_tolerance(0.001)
#' }

set_tolerance <- function(tolerance = 0) {
  if (!is.numeric(tolerance) || tolerance < 0) {
    stop("Tolerance must be a non-negative numeric value.")
  }
  options(comparison_tolerance = tolerance)
  message("Tolerance set to ", tolerance)
}

#' Get Tolerance Level for Comparisons
#'
#' Retrieves the currently set tolerance level for numeric comparisons.
#'
#' @return The current tolerance level as a numeric value.
#' @keywords internal
#' @examples
#' \dontrun{
#'   get_tolerance()
#' }

get_tolerance <- function() {
  return(getOption("comparison_tolerance", default = 0))
}
