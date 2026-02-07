test_that("detect_cdisc_domain identifies SDTM DM", {
  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M", AGE = 45, AGEU = "YEARS",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  result <- detect_cdisc_domain(dm)
  expect_equal(result$standard, "SDTM")
  expect_equal(result$domain, "DM")
  expect_true(result$confidence > 0.5)
})

test_that("detect_cdisc_domain identifies ADaM ADSL", {
  adsl <- data.frame(
    STUDYID = "STUDY01", USUBJID = "SUBJ01", SUBJID = "01",
    SITEID = "SITE01", AGE = 45, AGEU = "YEARS", SEX = "M",
    RACE = "WHITE", ARM = "Treatment", ARMCD = "TRT",
    ACTARM = "Treatment", ACTARMCD = "TRT",
    TRT01P = "Drug A", TRT01PN = 1, TRT01A = "Drug A", TRT01AN = 1,
    TRTSDT = 22000, TRTEDT = 22100,
    RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SAFFL = "Y", ITTFL = "Y",
    stringsAsFactors = FALSE
  )
  result <- detect_cdisc_domain(adsl)
  expect_equal(result$standard, "ADaM")
  expect_equal(result$domain, "ADSL")
})

test_that("detect_cdisc_domain returns Unknown for non-CDISC data", {
  df <- data.frame(x = 1:5, y = letters[1:5], z = rnorm(5))
  result <- detect_cdisc_domain(df)
  expect_equal(result$standard, "Unknown")
  expect_true(is.na(result$domain))
})

test_that("validate_cdisc detects missing required SDTM variables", {
  # DM dataset missing SEX (required) and ETHNIC (expected)
  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  result <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
  expect_s3_class(result, "data.frame")

  # Should have errors for missing required variables
  errors <- result[result$severity == "ERROR", ]
  expect_true(nrow(errors) > 0)
  expect_true("SEX" %in% errors$variable)

  # Should have warnings for missing expected variables
  warnings_df <- result[result$severity == "WARNING", ]
  expect_true(nrow(warnings_df) > 0)
})

test_that("validate_sdtm detects type mismatches", {
  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M",
    AGE = "45",  # Should be numeric!
    AGEU = "YEARS",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  result <- validate_sdtm(dm, "DM")
  type_issues <- result[result$category == "Type Mismatch", ]
  expect_true(nrow(type_issues) > 0)
  expect_true("AGE" %in% type_issues$variable)
})

test_that("validate_cdisc detects non-standard variables", {
  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M", AGE = 45, AGEU = "YEARS",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    MY_CUSTOM_VAR = "custom",  # Non-standard!
    stringsAsFactors = FALSE
  )
  result <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
  non_std <- result[result$category == "Non-Standard Variable", ]
  expect_true("MY_CUSTOM_VAR" %in% non_std$variable)
})

test_that("validate_adam validates ADSL correctly", {
  adsl <- data.frame(
    STUDYID = "STUDY01", USUBJID = "SUBJ01", SUBJID = "01",
    SITEID = "SITE01", AGE = 45, AGEU = "YEARS", SEX = "M",
    RACE = "WHITE", ARM = "Treatment", ARMCD = "TRT",
    ACTARM = "Treatment", ACTARMCD = "TRT",
    TRT01P = "Drug A", TRT01PN = 1, TRT01A = "Drug A", TRT01AN = 1,
    TRTSDT = 22000, TRTEDT = 22100,
    RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SAFFL = "Y", ITTFL = "Y",
    stringsAsFactors = FALSE
  )
  result <- validate_adam(adsl, "ADSL")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("category", "variable", "message", "severity") %in% names(result)))
})

test_that("validate_cdisc errors on bad inputs", {
  expect_error(validate_cdisc(NULL))
  expect_error(validate_cdisc(data.frame(), domain = "DM", standard = "INVALID"))
})

test_that("cdisc_compare returns comprehensive results", {
  dm1 <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M", AGE = 45, AGEU = "YEARS",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  dm2 <- dm1
  dm2$AGE <- 46  # Small difference

  result <- cdisc_compare(dm1, dm2, domain = "DM", standard = "SDTM")
  expect_type(result, "list")
  expect_true("comparison" %in% names(result))
  expect_true("variable_comparison" %in% names(result))
  expect_true("cdisc_validation_df1" %in% names(result))
  expect_true("cdisc_validation_df2" %in% names(result))
  expect_true("cdisc_conformance_comparison" %in% names(result))
})
