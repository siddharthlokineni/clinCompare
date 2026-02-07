library(testthat)
library(dplyr)


# Sample Data Frame with Missing Values
sample_df <- data.frame(
  A = c(1, NA, 3, 4),
  B = c(NA, 2, 3, 4),
  C = c(1, 2, 3, NA)
)

# Begin tests
test_that("Exclude method removes rows with NAs", {
  result_df <- handle_missing_values(sample_df, method = "exclude")
  expect_equal(nrow(result_df), 1)
})

test_that("Replace method replaces NAs with specified value", {
  result_df <- handle_missing_values(sample_df, method = "replace", replace_with = 0)
  expect_true(all(!is.na(result_df)))
  expect_equal(result_df$A[2], 0)
})

test_that("Replace method without replace_with throws an error", {
  expect_error(handle_missing_values(sample_df, method = "replace"))
})

test_that("Mean method replaces NAs with mean of the column", {
  result_df <- handle_missing_values(sample_df, method = "mean")
  expect_equal(result_df$A[2], mean(c(1, 3, 4), na.rm = TRUE))
})

test_that("Median method replaces NAs with median of the column", {
  result_df <- handle_missing_values(sample_df, method = "median")
  expect_equal(result_df$A[2], median(c(1, 3, 4), na.rm = TRUE))
})

test_that("Flag method adds a missing_flag column", {
  result_df <- handle_missing_values(sample_df, method = "flag")
  expect_true("missing_flag" %in% colnames(result_df))
  expect_equal(result_df$missing_flag[1], TRUE)
})

test_that("Invalid method throws an error", {
  expect_error(handle_missing_values(sample_df, method = "invalid_method"))
})