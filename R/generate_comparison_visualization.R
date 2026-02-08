#' Generate Visualization for Data Comparison
#'
#' @description
#' Creates a ggplot2 bar chart visualization showing the number of discrepancies
#' per variable from comparison results. Provides a clear visual summary of data
#' differences across variables in the datasets being compared.
#'
#' @param comparison_results A list or data frame containing the results of dataset comparisons.
#' @return A plot object visualizing the comparison results.
#' @importFrom rlang .data
#' @keywords internal
#' @examples
#' \donttest{
#'   generate_comparison_visualization(comparison_results)
#' }

generate_comparison_visualization <- function(comparison_results) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function. Please install it with: install.packages('ggplot2')")
  }

  # Validate input
  if (is.null(comparison_results) || !is.data.frame(comparison_results)) {
    stop("comparison_results must be a non-null data frame.")
  }
  if (nrow(comparison_results) == 0) {
    warning("Empty dataset provided.")
    return(ggplot2::ggplot())  # Return an empty ggplot object
  }

  # Check for required columns and suggest a default plot if missing
  required_columns <- c("Variable", "Discrepancies")
  if (!all(required_columns %in% names(comparison_results))) {
    warning("Data does not contain the required columns: Variable, Discrepancies. Attempting a default plot with available columns.")
    if (length(names(comparison_results)) >= 2) {
      comparison_results <- comparison_results[, 1:2]
      names(comparison_results) <- required_columns
    } else {
      warning("Not enough columns in comparison_results to create a default plot.")
      return(ggplot2::ggplot())
    }
  }

  # Generate the plot using aes() with .data pronoun
  ggplot2::ggplot(data = comparison_results,
                  ggplot2::aes(x = .data[["Variable"]], y = .data[["Discrepancies"]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Discrepancies per Variable", x = "Variable", y = "Count of Discrepancies")
}
