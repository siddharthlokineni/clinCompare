library(testthat)

# Sample Data Frames for Testing
df1 <- data.frame(A = c(3, 1, 2), B = c("x", "y", "z"))
df2 <- data.frame(A = c(2, 3, 1), B = c("z", "x", "y"))

# Begin tests
test_that("Dataframes are correctly sorted", {
  sorted_datasets <- prepare_datasets(df1, df2, sort_columns = "A")
  expect_equal(sorted_datasets$df1$A, c(1, 2, 3))
  expect_equal(sorted_datasets$df2$A, c(1, 2, 3))
})

test_that("Warning for non-existent sort columns", {
  expect_warning(prepare_datasets(df1, df2, sort_columns = "C"))
})

test_that("Dataframes are correctly filtered", {
  filtered_datasets <- prepare_datasets(df1, df2, filter_criteria = "A > 1")
  expect_equal(nrow(filtered_datasets$df1), 2)
  expect_equal(nrow(filtered_datasets$df2), 2)
})

test_that("Correct handling of sorting and filtering", {
  df1 <- data.frame(A = c(3, 1, 2), B = c("x", "y", "z"))
  df2 <- data.frame(A = c(2, 3, 1), B = c("z", "x", "y"))
  
  # Assuming that the datasets are sorted by column 'B' in ascending order
  datasets <- prepare_datasets(df1, df2, sort_columns = "B", filter_criteria = "A > 1")
  expect_equal(datasets$df1$B, c("x", "z"))
  expect_equal(datasets$df2$B, c("x", "z"))
})


test_that("Behavior with null arguments", {
  datasets <- prepare_datasets(df1, df2)
  expect_equal(dim(datasets$df1), dim(df1))
  expect_equal(dim(datasets$df2), dim(df2))
})

test_that("Correct sorting with multiple columns", {
  df1 <- data.frame(A = c(2, 1, 1), B = c(2, 2, 1))
  df2 <- data.frame(A = c(1, 1, 2), B = c(1, 2, 2))
  
  datasets <- prepare_datasets(df1, df2, sort_columns = c("A", "B"))
  
  # Resetting row names for comparison
  expected_df1 <- data.frame(A = c(1, 1, 2), B = c(1, 2, 2), row.names = NULL)
  actual_df1 <- datasets$df1
  rownames(actual_df1) <- NULL
  
  expect_equal(actual_df1, expected_df1)
})

test_that("Dataframes are correctly filtered with complex criteria", {
  df1 <- data.frame(A = c(3, 2, 1), B = c(2, 3, 4))
  df2 <- data.frame(A = c(1, 2, 4), B = c(4, 2, 1)) # This should result in one row matching the criteria
  
  # Debugging: Print df2 before and after filtering
  print("df2 before filtering:")
  print(df2)
  
  datasets <- prepare_datasets(df1, df2, filter_criteria = "(A > 1 & B <= 3)")
  
  print("df2 after filtering:")
  print(datasets$df2)
  
  expect_equal(nrow(datasets$df1), 2)
  expect_equal(nrow(datasets$df2), 2)
})


test_that("No changes when criteria don't apply", {
  df_sorted <- data.frame(A = c(1, 2, 3), B = c("x", "y", "z"))
  datasets <- prepare_datasets(df_sorted, df_sorted, sort_columns = "A", filter_criteria = "A <= 3")
  expect_equal(datasets$df1, df_sorted)
  expect_equal(datasets$df2, df_sorted)
})

test_that("Data integrity is maintained post processing", {
  df3 <- data.frame(A = c(3, 2, 1), C = c("one", "two", "three"))
  datasets <- prepare_datasets(df1, df3, sort_columns = "A")
  # After sorting, values should be the same set (just reordered)
  expect_true(setequal(df1$A, datasets$df1$A))
  expect_true(setequal(df3$C, datasets$df2$C))
})

test_that("Handling empty dataframes", {
  df_empty <- data.frame()
  datasets <- prepare_datasets(df_empty, df_empty, sort_columns = "A", filter_criteria = "A > 1")
  expect_equal(dim(datasets$df1), c(0, 0))
  expect_equal(dim(datasets$df2), c(0, 0))
})
