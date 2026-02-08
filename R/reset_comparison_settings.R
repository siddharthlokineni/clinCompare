#' Reset Comparison Settings to Defaults
#'
#' @description
#' Resets all comparison settings back to their defaults, clearing any custom tolerance
#' or other parameters.
#'
#' @return Invisible \code{NULL}. Called for its side effect of resetting package settings.
#' @keywords internal
reset_comparison_settings <- function() {
  .clincompare_env$tolerance <- 0
  .clincompare_env$missing_value_handling <- "ignore"
  message("Comparison settings have been reset to default values.")
  invisible(NULL)
}
