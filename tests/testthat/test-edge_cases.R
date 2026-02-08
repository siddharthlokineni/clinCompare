# Edge case tests for cdisc_compare safety features

test_that("WHERE clause validation catches bad inputs", {
  df <- data.frame(STUDYID = "S1", DOMAIN = "DM", USUBJID = "U1", SEX = "M",
                   stringsAsFactors = FALSE)
  expect_error(
    cdisc_compare(df, df, domain = "DM", standard = "SDTM", where = ""),
    "non-empty"
  )
  expect_error(
    cdisc_compare(df, df, domain = "DM", standard = "SDTM", where = 42),
    "non-empty character"
  )
  expect_error(
    cdisc_compare(df, df, domain = "DM", standard = "SDTM", where = "SEX ==== 'M'"),
    "Invalid WHERE|WHERE filter failed"
  )
})

test_that("'+' id_vars bare plus errors", {
  df <- data.frame(STUDYID = "S1", DOMAIN = "DM", USUBJID = "U1",
                   stringsAsFactors = FALSE)
  expect_error(
    cdisc_compare(df, df, id_vars = c("+")),
    "at least one additional"
  )
})

test_that("cdisc_compare tolerance validation", {
  df <- data.frame(STUDYID = "S1", DOMAIN = "DM", USUBJID = "U1",
                   stringsAsFactors = FALSE)
  expect_error(
    cdisc_compare(df, df, domain = "DM", standard = "SDTM", tolerance = -5),
    "non-negative finite"
  )
})
