#' Reset Comparison Settings to Defaults
#'
#' @description
#' Resets all comparison settings back to their defaults, clearing any custom tolerance
#' or other parameters.
#'
#' @return None; this function resets global options and does not return a value.
#' @export
#' @examples
#' \dontrun{
#'   reset_comparison_settings()
#' }

reset_comparison_settings <- function() {
  options(comparison_tolerance = 0)
  options(missing_value_handling = "ignore")
  message("Comparison settings have been reset to default values.")
}