library(testthat)
library(ggplot2)

# Begin tests
test_that("Returns ggplot object with valid data", {
  comparison_results <- data.frame(Variable = c("Var1", "Var2"), Discrepancies = c(10, 5))
  plot <- generate_comparison_visualization(comparison_results)
  expect_true(ggplot2::is_ggplot(plot))
})

test_that("Handles empty data input with a warning", {
  empty_data <- data.frame(Variable = character(), Discrepancies = numeric())
  
  # Check for warning with empty data
  expect_warning(generate_comparison_visualization(empty_data), "Empty dataset provided.")
})

test_that("Handles incorrect data structure with a warning", {
  incorrect_data_warning <- data.frame(OtherColumn = c("A", "B"))
  
  # Check if the function issues a warning with incorrect data structure
  expect_warning(generate_comparison_visualization(incorrect_data_warning),
                 "Data does not contain the required columns")
})

test_that("Handles different types of variables in the data", {
  # Creating a dataframe with different types of variables
  comparison_results <- data.frame(
    Variable = c("Var1", "Var2", 3, TRUE, NA), 
    Discrepancies = c(10, 5, 3, 2, 1)
  )
  
  plot <- generate_comparison_visualization(comparison_results)
  expect_true(ggplot2::is_ggplot(plot))
  
})

test_that("Plot contains geom_bar", {
  comparison_results <- data.frame(
    Variable = c("Var1", "Var2", "Var3"), 
    Discrepancies = c(10, 5, 3)
  )
  
  plot <- generate_comparison_visualization(comparison_results)
  expect_true(ggplot2::is_ggplot(plot))
  
  # Check for GeomBar layer
  geom_bar_present <- any(sapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomBar")
  }))
  
  expect_true(geom_bar_present)
})

