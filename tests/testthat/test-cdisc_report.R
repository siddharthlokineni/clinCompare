test_that("generate_cdisc_report works with text format", {
  dm1 <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M", AGE = 45, AGEU = "YEARS",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  dm2 <- dm1
  dm2$AGE <- 46

  results <- cdisc_compare(dm1, dm2, domain = "DM", standard = "SDTM")
  expect_no_error(
    capture.output(generate_cdisc_report(results, output_format = "text"))
  )
})

test_that("generate_cdisc_report creates HTML file", {
  dm1 <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M", AGE = 45, AGEU = "YEARS",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  dm2 <- dm1
  results <- cdisc_compare(dm1, dm2, domain = "DM", standard = "SDTM")

  tmp_file <- tempfile(fileext = ".html")
  generate_cdisc_report(results, output_format = "html", file_name = tmp_file)
  expect_true(file.exists(tmp_file))
  content <- readLines(tmp_file)
  expect_true(any(grepl("<html>", content, ignore.case = TRUE)))
  unlink(tmp_file)
})

test_that("print_cdisc_validation produces output", {
  dm <- data.frame(
    STUDYID = "STUDY01", DOMAIN = "DM", USUBJID = "SUBJ01",
    SUBJID = "01", RFSTDTC = "2024-01-01", RFENDTC = "2024-06-01",
    SITEID = "SITE01", SEX = "M",
    ARMCD = "TRT", ARM = "Treatment", COUNTRY = "USA",
    ACTARMCD = "TRT", ACTARM = "Treatment",
    stringsAsFactors = FALSE
  )
  validation <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
  expect_message(print_cdisc_validation(validation))
})
