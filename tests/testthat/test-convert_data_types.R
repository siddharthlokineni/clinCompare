library(testthat)

# Begin tests
test_that("Convert all columns to character", {
  df <- data.frame(a = 1:3, b = 4:6)
  converted_df <- convert_data_types(df, "character")
  expect_true(all(sapply(converted_df, class) == "character"))
})

test_that("Convert all columns to numeric", {
  df <- data.frame(a = as.character(1:3), b = as.character(4:6))
  converted_df <- convert_data_types(df, "numeric")
  expect_true(all(sapply(converted_df, class) == "numeric"))
})

test_that("Handle non-numeric data for numeric conversion", {
  df <- data.frame(a = c("1", "2", "x"), b = c("4", "5", "6"))
  expect_warning(converted_df <- convert_data_types(df, "numeric"))
  # Additional checks can be added depending on how you want to handle non-convertible data
})

test_that("Dataframe structure is preserved after conversion", {
  df <- data.frame(a = 1:3, b = 4:6)
  converted_df <- convert_data_types(df, "character")
  expect_equal(dim(df), dim(converted_df))
})

