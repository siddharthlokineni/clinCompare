library(testthat)
library(dplyr)


# Sample Data Frames
df1 <- data.frame(Group = c("A", "B", "C"), Value = 1:3)
df2 <- data.frame(Group = c("B", "C", "D"), Value = 4:6)

# Begin tests
test_that("Grouping variables are present in both datasets", {
  expect_silent(compare_by_group(df1, df2, "Group"))
})

test_that("Error when grouping variables are not in both datasets", {
  expect_error(compare_by_group(df1, df2, "NonexistentGroup"))
})

test_that("Correct groups are compared", {
  result <- compare_by_group(df1, df2, "Group")
  expect_equal(length(result), length(unique(c(df1$Group, df2$Group))))
  # Additional checks can be added based on the implementation of compare_datasets
})