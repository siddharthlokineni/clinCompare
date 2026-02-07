library(testthat)

# Begin tests
test_that("Tolerance is correctly set and retrieved", {
  set_tolerance(0.1)
  expect_equal(get_tolerance(), 0.1)
})

test_that("Error on invalid tolerance values", {
  expect_error(set_tolerance(-1))
  expect_error(set_tolerance("a"))
})

test_that("Default tolerance value when not set", {
  set_tolerance(0) # Resetting to default
  expect_equal(get_tolerance(), 0)
})

test_that("Tolerance set to zero", {
  set_tolerance(0)
  expect_equal(get_tolerance(), 0)
})

test_that("Tolerance set to a high value", {
  set_tolerance(100)
  expect_equal(get_tolerance(), 100)
})

test_that("Default tolerance when no argument is passed", {
  set_tolerance()
  expect_equal(get_tolerance(), 0)
})

test_that("Correct message output when setting tolerance", {
  expect_message(set_tolerance(0.5), "Tolerance set to 0.5")
})

test_that("Resetting tolerance to default", {
  set_tolerance(10)
  set_tolerance(0) # Reset to default
  expect_equal(get_tolerance(), 0)
})