library(testthat)

test_that("Both inputs are provided", {
  variable_diffs_test <- "Variable differences data"
  observation_diffs_test <- "Observation differences data"
  expected_output <- list(variable_differences = variable_diffs_test, observation_differences = observation_diffs_test)
  
  expect_equal(report_differences(variable_diffs_test, observation_diffs_test), expected_output)
})

test_that("Only variable_diffs is provided", {
  variable_diffs_test <- "Variable differences data"
  expected_output <- list(variable_differences = variable_diffs_test)
  
  expect_equal(report_differences(variable_diffs_test, NULL), expected_output)
})

test_that("Only observation_diffs is provided", {
  observation_diffs_test <- "Observation differences data"
  expected_output <- list(observation_differences = observation_diffs_test)
  
  expect_equal(report_differences(NULL, observation_diffs_test), expected_output)
})

test_that("Neither input is provided", {
  expected_output <- list()
  
  expect_equal(report_differences(NULL, NULL), expected_output)
})