#' Set Tolerance Level for Comparisons
#'
#' @description
#' Sets the numeric tolerance for floating-point comparisons, allowing small differences
#' within the tolerance to be treated as equal.
#'
#' @param tolerance A non-negative numeric value specifying the tolerance level.
#' @return Invisible \code{NULL}. Called for its side effect of updating the tolerance setting.
#' @keywords internal
set_tolerance <- function(tolerance = 0) {
  if (!is.numeric(tolerance) || tolerance < 0) {
    stop("Tolerance must be a non-negative numeric value.")
  }
  .clincompare_env$tolerance <- tolerance
  message("Tolerance set to ", tolerance)
  invisible(NULL)
}

#' Get Tolerance Level for Comparisons
#'
#' Retrieves the currently set tolerance level for numeric comparisons.
#'
#' @return The current tolerance level as a numeric value.
#' @keywords internal
get_tolerance <- function() {
  .clincompare_env$tolerance
}
