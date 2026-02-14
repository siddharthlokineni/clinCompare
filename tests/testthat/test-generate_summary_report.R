test_that("generate_summary_report works with compare_datasets output", {
  df1 <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  df2 <- data.frame(x = c(1, 2, 4), stringsAsFactors = FALSE)
  comp <- compare_datasets(df1, df2)
  expect_output(generate_summary_report(comp), "Summary Comparison Report")
})

test_that("generate_summary_report works with cdisc_compare list output", {
  result_list <- list(
    VariableDifferences = list("x", "y"),
    ObservationDifferences = list(
      y = data.frame(Row = 3, Value_in_df1 = "c", Value_in_df2 = "d",
                     stringsAsFactors = FALSE)
    )
  )
  expect_output(generate_summary_report(result_list), "Total Number of Observation Differences:\\s+1")
})
