test_that("generate_detailed_report works with compare_datasets output", {
  df1 <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  df2 <- data.frame(x = c(1, 2, 4), stringsAsFactors = FALSE)
  comp <- compare_datasets(df1, df2)
  # comp is now a list with class "dataset_comparison"
  expect_true(is.list(comp))
  expect_true("variable_comparison" %in% names(comp))
  expect_output(generate_detailed_report(comp), "Detailed Comparison Report")
})

test_that("generate_detailed_report works with cdisc_compare list output", {
  result_list <- list(
    VariableDifferences = list(common = c("x", "y")),
    ObservationDifferences = list(
      y = data.frame(Row = 3, Value_in_df1 = "c", Value_in_df2 = "d",
                     stringsAsFactors = FALSE)
    )
  )
  expect_output(generate_detailed_report(result_list), "Observation Differences")
})
