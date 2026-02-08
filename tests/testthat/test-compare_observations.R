test_that("compare_observations detects value differences", {
  df1 <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = c(1, 5, 3), b = c("x", "y", "w"), stringsAsFactors = FALSE)
  result <- compare_observations(df1, df2)
  expect_equal(result$discrepancies[["a"]], 1)
  expect_equal(result$discrepancies[["b"]], 1)
})

test_that("compare_observations works with identical data", {
  df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  result <- compare_observations(df, df)
  expect_true(all(result$discrepancies == 0))
  expect_equal(length(result$details), 0)
})

test_that("compare_observations handles factors", {
  df1 <- data.frame(a = factor(c("x", "y")), stringsAsFactors = TRUE)
  df2 <- data.frame(a = factor(c("x", "z")), stringsAsFactors = TRUE)
  result <- compare_observations(df1, df2)
  expect_equal(result$discrepancies[["a"]], 1)
})

test_that("compare_observations errors on mismatched rows", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1:5)
  expect_error(compare_observations(df1, df2), "different numbers of rows")
})

test_that("compare_observations shows row details", {
  df1 <- data.frame(a = c(1, 2, 3))
  df2 <- data.frame(a = c(1, 9, 3))
  result <- compare_observations(df1, df2)
  expect_true("a" %in% names(result$details))
  expect_equal(result$details$a$Row, 2)
  expect_equal(result$details$a$Value_in_df1, 2)
  expect_equal(result$details$a$Value_in_df2, 9)
})

test_that("tolerance validation rejects bad inputs", {
  df <- data.frame(a = 1:3)
  expect_error(compare_observations(df, df, tolerance = -1), "non-negative finite")
  expect_error(compare_observations(df, df, tolerance = NaN), "non-negative finite")
  expect_error(compare_observations(df, df, tolerance = Inf), "non-negative finite")
  expect_error(compare_observations(df, df, tolerance = NA), "non-negative finite")
  expect_error(compare_observations(df, df, tolerance = "0.01"), "non-negative finite")
})

test_that("Inf - Inf detected as difference at tolerance = 0", {
  df1 <- data.frame(val = c(1.0, Inf, -Inf, Inf))
  df2 <- data.frame(val = c(1.0, Inf, -Inf, -Inf))
  result <- compare_observations(df1, df2, tolerance = 0)
  # Inf vs Inf = NaN diff (flagged), -Inf vs -Inf = NaN diff (flagged), Inf vs -Inf (real diff)
  expect_equal(result$discrepancies[["val"]], 3L)
})

test_that("Inf - Inf detected as difference at tolerance > 0", {
  df1 <- data.frame(val = c(1.0, Inf, -Inf, Inf))
  df2 <- data.frame(val = c(1.0, Inf, -Inf, -Inf))
  result <- compare_observations(df1, df2, tolerance = 0.01)
  expect_equal(result$discrepancies[["val"]], 3L)
})

test_that("NA handling: NA vs NA matches, NA vs value differs", {
  df1 <- data.frame(val = c(NA, NA, 1))
  df2 <- data.frame(val = c(NA, 1, NA))
  result <- compare_observations(df1, df2)
  # Row 1: NA vs NA = match. Row 2: NA vs 1 = diff. Row 3: 1 vs NA = diff.
  expect_equal(result$discrepancies[["val"]], 2L)
})
