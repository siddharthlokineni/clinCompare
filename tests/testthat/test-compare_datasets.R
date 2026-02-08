test_that("compare_datasets returns object with class 'dataset_comparison'", {
  df1 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  expect_s3_class(result, "dataset_comparison")
  expect_true(is.list(result))
})

test_that("identical datasets have zero observation discrepancies", {
  df1 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  obs <- result$observation_comparison
  expect_true(is.list(obs))
  expect_equal(sum(obs$discrepancies), 0L)
})

test_that("different row counts trigger graceful skip with message", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1:5)
  result <- compare_datasets(df1, df2)
  obs <- result$observation_comparison
  expect_true(!is.null(obs$message))
  expect_true(grepl("Row counts differ", obs$message))
})

test_that("extra columns detected correctly", {
  df1 <- data.frame(a = 1, b = 2, d = 4)
  df2 <- data.frame(a = 1, c = 3)
  result <- compare_datasets(df1, df2)
  expect_true(all(c("b", "d") %in% result$extra_in_df1))
  expect_true("c" %in% result$extra_in_df2)
  expect_equal(result$common_columns, "a")
})

test_that("type mismatches are detected", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = c("1", "2", "3"), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  expect_true(!is.null(result$type_mismatches))
  expect_true("a" %in% result$type_mismatches$column)
})

test_that("value differences detected at observation level", {
  df1 <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = c(1, 2, 99), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  obs <- result$observation_comparison
  expect_true(sum(obs$discrepancies) > 0)
  expect_true("a" %in% names(obs$details))
})

test_that("NULL inputs produce errors", {
  expect_error(compare_datasets(NULL, data.frame(a = 1)))
  expect_error(compare_datasets(data.frame(a = 1), NULL))
})

test_that("print method works without error", {
  df1 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  expect_output(print(result), "clinCompare")
})

test_that("return structure has all expected elements", {
  df1 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  expected <- c("nrow_df1", "ncol_df1", "nrow_df2", "ncol_df2",
                "common_columns", "extra_in_df1", "extra_in_df2",
                "type_mismatches", "missing_values",
                "variable_comparison", "observation_comparison")
  expect_true(all(expected %in% names(result)))
  expect_equal(result$nrow_df1, 3L)
  expect_equal(result$ncol_df1, 2L)
})

test_that("missing values are summarised", {
  df1 <- data.frame(a = c(1, NA, 3), stringsAsFactors = FALSE)
  df2 <- data.frame(a = c(1, 2, 3), stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  expect_true(!is.null(result$missing_values))
  expect_true("a" %in% result$missing_values$column)
  expect_equal(result$missing_values$na_df1[1], 1L)
  expect_equal(result$missing_values$na_df2[1], 0L)
})

test_that("tolerance validation rejects bad inputs", {
  df <- data.frame(a = 1:3)
  expect_error(compare_datasets(df, df, tolerance = -1), "non-negative finite")
  expect_error(compare_datasets(df, df, tolerance = NaN), "non-negative finite")
  expect_error(compare_datasets(df, df, tolerance = Inf), "non-negative finite")
})

test_that("vars parameter filters observation comparison", {
  df1 <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = c(1, 2, 99), b = c("x", "y", "w"), stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2, vars = c("a"))
  obs <- result$observation_comparison
  # Only "a" should show diffs, not "b"
  expect_true("a" %in% names(obs$discrepancies))
  expect_equal(sum(obs$discrepancies), 1L)
  # But structural comparison covers all columns
  expect_equal(length(result$common_columns), 2L)
})

test_that("vars with no matching columns produces warning", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1:3)
  expect_warning(compare_datasets(df1, df2, vars = c("nonexistent")), "None of the specified")
})
