test_that("get_sdtm_metadata returns correct structure", {
  meta <- get_sdtm_metadata()
  expect_type(meta, "list")
  expect_true(length(meta) >= 10)

  # Check known domains exist
  expect_true("DM" %in% names(meta))
  expect_true("AE" %in% names(meta))
  expect_true("LB" %in% names(meta))
  expect_true("VS" %in% names(meta))
  expect_true("EX" %in% names(meta))

  # Check DM structure
  dm <- meta[["DM"]]
  expect_s3_class(dm, "data.frame")
  expect_true(all(c("variable", "label", "type", "core") %in% names(dm)))

  # Check DM has key required variables
  expect_true("STUDYID" %in% dm$variable)
  expect_true("USUBJID" %in% dm$variable)
  expect_true("SEX" %in% dm$variable)

  # Check core values are valid
  expect_true(all(dm$core %in% c("Req", "Exp", "Perm")))

  # Check type values are valid
  expect_true(all(dm$type %in% c("Char", "Num")))
})

test_that("get_adam_metadata returns correct structure", {
  meta <- get_adam_metadata()
  expect_type(meta, "list")
  expect_true(length(meta) >= 5)

  # Check known datasets exist
  expect_true("ADSL" %in% names(meta))
  expect_true("ADAE" %in% names(meta))
  expect_true("ADLB" %in% names(meta))
  expect_true("ADTTE" %in% names(meta))

  # Check ADSL structure
  adsl <- meta[["ADSL"]]
  expect_s3_class(adsl, "data.frame")
  expect_true(all(c("variable", "label", "type", "core") %in% names(adsl)))

  # Check ADSL has key required variables
  expect_true("STUDYID" %in% adsl$variable)
  expect_true("USUBJID" %in% adsl$variable)
  expect_true("TRT01P" %in% adsl$variable)
  expect_true("SAFFL" %in% adsl$variable)

  # Check core values are valid
  expect_true(all(adsl$core %in% c("Req", "Cond")))
})

test_that("SDTM metadata has no duplicate variables within domains", {
  meta <- get_sdtm_metadata()
  for (domain in names(meta)) {
    vars <- meta[[domain]]$variable
    expect_equal(length(vars), length(unique(vars)),
                 info = paste("Duplicate variables in domain:", domain))
  }
})

test_that("ADaM metadata has no duplicate variables within datasets", {
  meta <- get_adam_metadata()
  for (ds in names(meta)) {
    vars <- meta[[ds]]$variable
    expect_equal(length(vars), length(unique(vars)),
                 info = paste("Duplicate variables in dataset:", ds))
  }
})
