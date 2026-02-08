test_that("generate_comparison_visualization returns ggplot", {
  skip_if_not_installed("ggplot2")
  comp_data <- data.frame(
    Variable = c("x", "y", "z"),
    Discrepancies = c(3, 0, 1),
    stringsAsFactors = FALSE
  )
  result <- generate_comparison_visualization(comp_data)
  expect_s3_class(result, "gg")
})

test_that("generate_comparison_visualization errors on NULL input", {
  expect_error(generate_comparison_visualization(NULL))
})

test_that("generate_comparison_visualization handles empty data", {
  skip_if_not_installed("ggplot2")
  empty_df <- data.frame(Variable = character(0), Discrepancies = numeric(0),
                         stringsAsFactors = FALSE)
  expect_warning(generate_comparison_visualization(empty_df))
})
