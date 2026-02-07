library(testthat)

test_that("Transformations are correctly applied", {
  df <- data.frame(a = 1:3, b = 4:6)
  transformations <- list(a = function(x) x * 2, b = function(x) x + 1)
  transformed_df <- transform_variables(df, transformations)
  expect_equal(transformed_df$a, c(2, 4, 6))
  expect_equal(transformed_df$b, c(5, 6, 7))
})

test_that("Warning for non-existent columns", {
  df <- data.frame(a = 1:3)
  transformations <- list(a = function(x) x * 2, c = function(x) x + 1)
  expect_warning(transform_variables(df, transformations))
})

test_that("Dataframe structure is preserved after transformations", {
  df <- data.frame(a = 1:3, b = 4:6)
  transformations <- list(a = function(x) x * 2)
  transformed_df <- transform_variables(df, transformations)
  expect_equal(dim(df), dim(transformed_df))
})

test_that("Multiple transformations are correctly applied", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  transformations <- list(a = function(x) x * 2, c = function(x) x - 1)
  transformed_df <- transform_variables(df, transformations)
  expect_equal(transformed_df$a, c(2, 4, 6))
  expect_equal(transformed_df$c, c(6, 7, 8))
})

