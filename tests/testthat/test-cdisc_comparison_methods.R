test_that("print.cdisc_comparison produces output", {
  mock_result <- list(
    domain = "DM", standard = "SDTM",
    nrow_df1 = 10L, ncol_df1 = 5L, nrow_df2 = 10L, ncol_df2 = 5L,
    id_vars = NULL,
    unified_comparison = data.frame(
      variable = "AGE", diff_type = "Value", row_or_key = "Row 1",
      base_value = "45", compare_value = "46", stringsAsFactors = FALSE
    ),
    cdisc_validation_df1 = data.frame(severity = c("ERROR", "WARNING"),
                                       category = c("Missing", "Type"),
                                       variable = c("RFSTDTC", "AGE"),
                                       message = c("m1", "m2"),
                                       stringsAsFactors = FALSE),
    cdisc_validation_df2 = data.frame(severity = c("WARNING"),
                                       category = c("Type"),
                                       variable = c("AGE"),
                                       message = c("m2"),
                                       stringsAsFactors = FALSE),
    cdisc_version = NULL
  )
  class(mock_result) <- "cdisc_comparison"
  expect_output(print(mock_result), "CDISC Comparison Results")
  expect_output(print(mock_result), "FAIL")
})

test_that("summary.cdisc_comparison returns data frame", {
  mock_result <- list(
    domain = "DM", standard = "SDTM",
    nrow_df1 = 10L, ncol_df1 = 5L, nrow_df2 = 10L, ncol_df2 = 5L,
    unified_comparison = data.frame(
      variable = character(0), diff_type = character(0),
      row_or_key = character(0), base_value = character(0),
      compare_value = character(0), stringsAsFactors = FALSE
    ),
    cdisc_validation_df1 = data.frame(severity = character(0), category = character(0),
                                       variable = character(0), message = character(0),
                                       stringsAsFactors = FALSE),
    cdisc_validation_df2 = data.frame(severity = character(0), category = character(0),
                                       variable = character(0), message = character(0),
                                       stringsAsFactors = FALSE)
  )
  class(mock_result) <- "cdisc_comparison"
  s <- summary(mock_result)
  expect_s3_class(s, "data.frame")
  expect_equal(s$verdict, "PASS")
  expect_equal(s$total_differences, 0)
})
