test_that("compare_datasets returns data frame", {
  df1 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  result <- compare_datasets(df1, df2)
  expect_s3_class(result, "data.frame")
  expect_true("Aspect" %in% names(result))
  expect_true("Description" %in% names(result))
})

test_that("compare_datasets detects dimension differences", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1:5)
  result <- compare_datasets(df1, df2)
  dim_row <- result[result$Aspect == "Dimensions", ]
  expect_true(grepl("different", dim_row$Description))
})

test_that("compare_datasets detects same dimensions", {
  df1 <- data.frame(a = 1:3, b = 1:3)
  df2 <- data.frame(a = 4:6, b = 4:6)
  result <- compare_datasets(df1, df2)
  dim_row <- result[result$Aspect == "Dimensions", ]
  expect_true(grepl("same", dim_row$Description))
})

test_that("compare_datasets errors on null input", {
  expect_error(compare_datasets(NULL, data.frame(a = 1)))
  expect_error(compare_datasets(data.frame(a = 1), NULL))
})

test_that("compare_datasets detects extra columns", {
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(a = 1, c = 3)
  result <- compare_datasets(df1, df2)
  col_row <- result[result$Aspect == "Column Names", ]
  expect_true(grepl("b", col_row$Description))
  expect_true(grepl("c", col_row$Description))
})

