test_that("generate_detailed_report works with compare_datasets output", {
  df1 <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  df2 <- data.frame(x = c(1, 2, 4), stringsAsFactors = FALSE)
  comp <- compare_datasets(df1, df2)
  expect_output(generate_detailed_report(comp), "Detailed Comparison Report")
})

test_that("generate_detailed_report works with cdisc_compare list output", {
  result_list <- list(
    variable_comparison = list(common = c("x", "y")),
    observation_comparison = list(
      details = list(y = data.frame(Row = 3, Value_in_df1 = "c", Value_in_df2 = "d",
                                     stringsAsFactors = FALSE)),
      discrepancies = c(x = 0, y = 1)
    ),
    unified_comparison = data.frame(
      variable = "y", diff_type = "Value", row_or_key = "Row 3",
      base_value = "c", compare_value = "d", stringsAsFactors = FALSE
    )
  )
  expect_output(generate_detailed_report(result_list), "Observation Differences")
})
