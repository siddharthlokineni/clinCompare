test_that("generate_summary_report works with compare_datasets output", {
  df1 <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  df2 <- data.frame(x = 1:3, y = c("a", "b", "d"), stringsAsFactors = FALSE)
  comp <- compare_datasets(df1, df2)
  expect_output(generate_summary_report(comp), "Summary Comparison Report")
})

test_that("generate_summary_report works with cdisc_compare list output", {
  result_list <- list(
    variable_comparison = data.frame(var = c("x", "y"), stringsAsFactors = FALSE),
    observation_comparison = list(discrepancies = c(x = 0, y = 1)),
    nrow_df1 = 3L, ncol_df1 = 2L, nrow_df2 = 3L, ncol_df2 = 2L
  )
  expect_output(generate_summary_report(result_list), "Total Observation Differences: 1")
})
