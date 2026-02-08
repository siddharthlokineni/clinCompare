test_that("clean_dataset removes duplicates", {
  df <- data.frame(x = c(1, 1, 2), y = c("a", "a", "b"), stringsAsFactors = FALSE)
  result <- clean_dataset(df, remove_duplicates = TRUE)
  expect_equal(nrow(result), 2)
})

test_that("clean_dataset converts to lowercase", {
  df <- data.frame(x = c("Hello", "WORLD"), stringsAsFactors = FALSE)
  result <- clean_dataset(df, remove_duplicates = FALSE, convert_to_case = "lower")
  expect_equal(result$x, c("hello", "world"))
})

test_that("clean_dataset converts to uppercase", {
  df <- data.frame(x = c("Hello", "world"), stringsAsFactors = FALSE)
  result <- clean_dataset(df, remove_duplicates = FALSE, convert_to_case = "upper")
  expect_equal(result$x, c("HELLO", "WORLD"))
})

test_that("clean_dataset handles specific variables", {
  df <- data.frame(x = c("Hello"), y = c("World"), stringsAsFactors = FALSE)
  result <- clean_dataset(df, variables = "x", remove_duplicates = FALSE, convert_to_case = "lower")
  expect_equal(result$x, "hello")
  expect_equal(result$y, "World")
})

test_that("clean_dataset warns on missing variable", {
  df <- data.frame(x = c(1, 2), stringsAsFactors = FALSE)
  expect_warning(
    clean_dataset(df, variables = "z", remove_duplicates = FALSE, convert_to_case = "lower"),
    "not found"
  )
})
