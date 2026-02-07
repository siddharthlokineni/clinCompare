test_that("compare_variables identifies common columns", {
  df1 <- data.frame(a = 1, b = 2, c = 3)
  df2 <- data.frame(a = 1, b = 2, d = 4)
  result <- compare_variables(df1, df2)
  expect_true("a" %in% result$details$common_columns)
  expect_true("b" %in% result$details$common_columns)
})

test_that("compare_variables finds extra columns", {
  df1 <- data.frame(a = 1, b = 2, c = 3)
  df2 <- data.frame(a = 1, b = 2, d = 4)
  result <- compare_variables(df1, df2)
  expect_true("c" %in% result$details$extra_in_df1)
  expect_true("d" %in% result$details$extra_in_df2)
  expect_equal(result$discrepancies, 2)  # c in df1 + d in df2
})

test_that("compare_variables detects type differences", {
  df1 <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1.5, b = "x", stringsAsFactors = FALSE)
  result <- compare_variables(df1, df2)
  type_comps <- result$details$data_type_comparisons
  expect_true(length(type_comps) > 0)
})

test_that("compare_variables works with identical datasets", {
  df <- data.frame(a = 1, b = "x", stringsAsFactors = FALSE)
  result <- compare_variables(df, df)
  expect_equal(result$discrepancies, 0)
  expect_equal(length(result$details$extra_in_df1), 0)
  expect_equal(length(result$details$extra_in_df2), 0)
})

