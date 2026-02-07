library(testthat)


# Begin tests
test_that("Incompatible if one or both dataframes are null", {
  df_null <- NULL
  df <- data.frame(a = 1, b = 2)
  expect_equal(check_compatibility(df_null, df)$compatible, FALSE)
  expect_equal(check_compatibility(df, df_null)$compatible, FALSE)
  expect_equal(check_compatibility(df_null, df_null)$compatible, FALSE)
})

test_that("Incompatible if dimensions are different", {
  df1 <- data.frame(a = 1:3, b = 2:4)
  df2 <- data.frame(a = 1:4, b = 2:5, c = 3:6)
  expect_equal(check_compatibility(df1, df2)$compatible, FALSE)
})

test_that("Compatible if dimensions and columns are the same", {
  df1 <- data.frame(a = 1:3, b = 2:4)
  df2 <- data.frame(a = 4:6, b = 5:7)
  expect_equal(check_compatibility(df1, df2)$compatible, TRUE)
})

test_that("Incompatible if same dimensions but different columns", {
  df1 <- data.frame(a = 1:3, b = 2:4)
  df2 <- data.frame(c = 4:6, b = 5:7)
  expect_equal(check_compatibility(df1, df2)$compatible, FALSE)
})

test_that("Identifies extra columns in either dataframe", {
  df1 <- data.frame(a = 1:3, b = 2:4, c = 3:5)
  df2 <- data.frame(a = 4:6, b = 5:7)
  result <- check_compatibility(df1, df2)
  expect_equal(result$compatible, FALSE)
  expect_length(result$extra_in_df1, 1)
  expect_length(result$extra_in_df2, 0)
})

