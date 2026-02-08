#!/usr/bin/env Rscript
# =============================================================================
# clinCompare — Pharmaverse Integration Test
# =============================================================================
# Tests clinCompare against real-world CDISC datasets from the pharmaverse
# ecosystem (pharmaversesdtm + pharmaverseadam). These are publicly available
# CDISC-compliant datasets modeled on realistic clinical trial data.
#
# Prerequisites:
#   install.packages("pharmaversesdtm")
#   install.packages("pharmaverseadam")
#
# Usage:
#   devtools::load_all("~/Desktop/cowork/clinCompare")
#   source("inst/testdata/test_pharmaverse.R")
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat("  clinCompare — Pharmaverse Integration Test\n")
cat("  Real-world CDISC data from pharmaversesdtm + pharmaverseadam\n")
cat("==============================================================\n\n")

# --- Check pharmaverse packages are available -----------------------------
if (!requireNamespace("pharmaversesdtm", quietly = TRUE)) {
  stop("pharmaversesdtm not installed. Run: install.packages('pharmaversesdtm')")
}
if (!requireNamespace("pharmaverseadam", quietly = TRUE)) {
  stop("pharmaverseadam not installed. Run: install.packages('pharmaverseadam')")
}

# --- Load SDTM data -------------------------------------------------------
cat("Loading pharmaverse SDTM datasets...\n")
dm_pharma   <- as.data.frame(pharmaversesdtm::dm)
ae_pharma   <- as.data.frame(pharmaversesdtm::ae)
lb_pharma   <- as.data.frame(pharmaversesdtm::lb)
vs_pharma   <- as.data.frame(pharmaversesdtm::vs)
ex_pharma   <- as.data.frame(pharmaversesdtm::ex)
cat(sprintf("  DM: %d rows x %d cols\n", nrow(dm_pharma), ncol(dm_pharma)))
cat(sprintf("  AE: %d rows x %d cols\n", nrow(ae_pharma), ncol(ae_pharma)))
cat(sprintf("  LB: %d rows x %d cols\n", nrow(lb_pharma), ncol(lb_pharma)))
cat(sprintf("  VS: %d rows x %d cols\n", nrow(vs_pharma), ncol(vs_pharma)))
cat(sprintf("  EX: %d rows x %d cols\n", nrow(ex_pharma), ncol(ex_pharma)))

# --- Load ADaM data -------------------------------------------------------
cat("\nLoading pharmaverse ADaM datasets...\n")
adsl_pharma <- as.data.frame(pharmaverseadam::adsl)
adae_pharma <- as.data.frame(pharmaverseadam::adae)
adlb_pharma <- as.data.frame(pharmaverseadam::adlb)
cat(sprintf("  ADSL: %d rows x %d cols\n", nrow(adsl_pharma), ncol(adsl_pharma)))
cat(sprintf("  ADAE: %d rows x %d cols\n", nrow(adae_pharma), ncol(adae_pharma)))
cat(sprintf("  ADLB: %d rows x %d cols\n", nrow(adlb_pharma), ncol(adlb_pharma)))


# =============================================================================
# PHARMA TEST 1: Domain auto-detection on real-world data
# =============================================================================
cat("\n\n--- PHARMA TEST 1: Domain auto-detection on real-world data ----------\n")
pharma_datasets <- list(
  DM = dm_pharma,
  AE = ae_pharma,
  LB = lb_pharma,
  VS = vs_pharma,
  EX = ex_pharma,
  ADSL = adsl_pharma,
  ADAE = adae_pharma,
  ADLB = adlb_pharma
)

for (name in names(pharma_datasets)) {
  detection <- tryCatch(
    detect_cdisc_domain(pharma_datasets[[name]]),
    warning = function(w) {
      detect_result <- suppressWarnings(detect_cdisc_domain(pharma_datasets[[name]]))
      attr(detect_result, "warning") <- conditionMessage(w)
      detect_result
    }
  )
  conf <- if (!is.null(attr(detection, "confidence"))) {
    round(attr(detection, "confidence") * 100)
  } else {
    NA
  }
  std <- if (!is.null(attr(detection, "standard"))) {
    attr(detection, "standard")
  } else {
    "?"
  }
  warn <- if (!is.null(attr(detection, "warning"))) " [ambiguous]" else ""
  correct <- (detection == name)
  status <- if (correct) "OK" else sprintf("MISMATCH (got %s)", detection)
  cat(sprintf("  %-6s -> Detected: %-6s (%s) | Confidence: %3d%% | %s%s\n",
              name, detection, std, conf, status, warn))
}


# =============================================================================
# PHARMA TEST 2: Compare real DM with simulated edits
# =============================================================================
cat("\n\n--- PHARMA TEST 2: DM — compare with simulated corrections ----------\n")

# Create a "corrected" version of DM with realistic edits
dm_corrected <- dm_pharma
set.seed(42)
n_dm <- nrow(dm_corrected)

# 1. Correct some RACE values (common data correction scenario)
race_edits <- sample(seq_len(n_dm), 5)
dm_corrected$RACE[race_edits] <- "MULTIPLE"

# 2. Add a new column (ETHNIC was missing, now added)
if (!"ETHNIC" %in% names(dm_corrected)) {
  dm_corrected$ETHNIC <- sample(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"),
                                 n_dm, replace = TRUE, prob = c(0.15, 0.85))
}

# 3. Add a new subject (new enrollment)
new_subj <- dm_corrected[1, , drop = FALSE]
new_subj$USUBJID <- "AB12345-NEW-SUBJ-999"
new_subj$SUBJID <- "999"
dm_corrected <- rbind(dm_corrected, new_subj)

result <- cdisc_compare(dm_pharma, dm_corrected, domain = "DM", standard = "SDTM")
print(result)

# Key-based comparison for real analysis
cat("\n--- PHARMA TEST 2b: DM — key-based matching by USUBJID ---------------\n")
result_keyed <- cdisc_compare(dm_pharma, dm_corrected, domain = "DM",
                               standard = "SDTM", id_vars = "USUBJID")
print(result_keyed)


# =============================================================================
# PHARMA TEST 3: Tolerance stress test with floating-point rounding noise
# =============================================================================
cat("\n\n--- PHARMA TEST 3: Tolerance stress test (floating-point noise) ------\n")
cat("  Scenario: Simulate SAS→R numeric conversion rounding artifacts\n")
cat("  (SAS stores 8-byte doubles; R reads may differ at ~1e-10 precision)\n\n")

# Create two versions of LB that differ ONLY in floating-point noise
lb_base <- lb_pharma
lb_noisy <- lb_pharma

# Add realistic floating-point noise to LBSTRESN (the primary numeric result)
set.seed(123)
if ("LBSTRESN" %in% names(lb_noisy)) {
  n_lb <- nrow(lb_noisy)
  numeric_rows <- which(!is.na(lb_noisy$LBSTRESN))

  # Tier 1: Ultra-small noise (machine epsilon level, ~1e-14)
  tier1 <- sample(numeric_rows, min(200, length(numeric_rows)))
  lb_noisy$LBSTRESN[tier1] <- lb_noisy$LBSTRESN[tier1] +
    runif(length(tier1), -1e-13, 1e-13)

  # Tier 2: SAS→R rounding noise (~1e-10)
  tier2 <- sample(setdiff(numeric_rows, tier1), min(100, length(numeric_rows) - 200))
  lb_noisy$LBSTRESN[tier2] <- lb_noisy$LBSTRESN[tier2] +
    runif(length(tier2), -1e-9, 1e-9)

  # Tier 3: Small but real diffs (~0.01–0.05) — should always flag
  tier3 <- sample(setdiff(numeric_rows, c(tier1, tier2)),
                  min(10, length(numeric_rows) - 300))
  lb_noisy$LBSTRESN[tier3] <- lb_noisy$LBSTRESN[tier3] +
    runif(length(tier3), 0.01, 0.05)

  cat(sprintf("  Noise injected: %d tier-1 (~1e-14), %d tier-2 (~1e-10), %d tier-3 (>0.01)\n",
              length(tier1), length(tier2), length(tier3)))
}

# Without tolerance — catches everything
result_no_tol <- compare_datasets(lb_base, lb_noisy, tolerance = 0)
n_no_tol <- sum(result_no_tol$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Tolerance = 0:      %d differences (machine epsilon noise included)\n", n_no_tol))

# With tolerance = 1e-12 — filters out machine epsilon, keeps SAS rounding + real diffs
result_tol_12 <- compare_datasets(lb_base, lb_noisy, tolerance = 1e-12)
n_tol_12 <- sum(result_tol_12$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Tolerance = 1e-12:  %d differences (machine epsilon filtered)\n", n_tol_12))

# With tolerance = 1e-8 — filters out SAS rounding noise, keeps real diffs
result_tol_8 <- compare_datasets(lb_base, lb_noisy, tolerance = 1e-8)
n_tol_8 <- sum(result_tol_8$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Tolerance = 1e-8:   %d differences (SAS rounding noise filtered)\n", n_tol_8))

# With tolerance = 0.001 — only shows substantial diffs
result_tol_3 <- compare_datasets(lb_base, lb_noisy, tolerance = 0.001)
n_tol_3 <- sum(result_tol_3$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Tolerance = 0.001:  %d differences (only real changes)\n", n_tol_3))

cat(sprintf("\n  Impact: Tolerance=1e-8 eliminated %d false positives from SAS→R noise!\n",
            n_no_tol - n_tol_8))


# =============================================================================
# PHARMA TEST 4: ADLB comparison with key-based matching + tolerance
# =============================================================================
cat("\n\n--- PHARMA TEST 4: ADLB — key-based matching + tolerance -------------\n")

adlb_modified <- adlb_pharma
set.seed(456)
if ("AVAL" %in% names(adlb_modified)) {
  n_adlb <- nrow(adlb_modified)
  aval_rows <- which(!is.na(adlb_modified$AVAL))

  # Add both noise and real changes
  noise_rows <- sample(aval_rows, min(50, length(aval_rows)))
  adlb_modified$AVAL[noise_rows] <- adlb_modified$AVAL[noise_rows] +
    runif(length(noise_rows), -1e-10, 1e-10)

  real_rows <- sample(setdiff(aval_rows, noise_rows), min(15, length(aval_rows) - 50))
  adlb_modified$AVAL[real_rows] <- adlb_modified$AVAL[real_rows] *
    (1 + runif(length(real_rows), -0.03, 0.03))

  cat(sprintf("  Injected: %d rounding-noise rows + %d real-change rows\n",
              length(noise_rows), length(real_rows)))
}

# Determine which id_vars are available
potential_keys <- c("USUBJID", "PARAMCD", "AVISIT", "AVISITN", "ADT", "ADTM")
available_keys <- intersect(potential_keys, names(adlb_pharma))
cat(sprintf("  Available keys: %s\n", paste(available_keys, collapse = ", ")))

# Use USUBJID + PARAMCD + AVISIT for matching (standard ADaM keys)
id_cols <- intersect(c("USUBJID", "PARAMCD", "AVISIT"), available_keys)
if (length(id_cols) >= 2) {
  cat(sprintf("  Using id_vars: %s\n\n", paste(id_cols, collapse = ", ")))

  cat("  Without tolerance:\n")
  result_adlb_no_tol <- cdisc_compare(adlb_pharma, adlb_modified,
                                       domain = "ADLB", standard = "ADaM",
                                       id_vars = id_cols, tolerance = 0)
  n_no <- sum(result_adlb_no_tol$observation_comparison$discrepancies, na.rm = TRUE)
  cat(sprintf("    Differences: %d\n", n_no))

  cat("  With tolerance = 1e-8:\n")
  result_adlb_tol <- cdisc_compare(adlb_pharma, adlb_modified,
                                    domain = "ADLB", standard = "ADaM",
                                    id_vars = id_cols, tolerance = 1e-8)
  n_tol <- sum(result_adlb_tol$observation_comparison$discrepancies, na.rm = TRUE)
  cat(sprintf("    Differences: %d (eliminated %d false positives)\n",
              n_tol, n_no - n_tol))
  print(result_adlb_tol)
} else {
  cat("  Skipping key-based test (insufficient key columns)\n")
}


# =============================================================================
# PHARMA TEST 5: LISTALL detailed report on AE comparison
# =============================================================================
cat("\n\n--- PHARMA TEST 5: LISTALL detailed report (AE) ---------------------\n")

ae_modified <- ae_pharma
set.seed(789)
n_ae <- nrow(ae_modified)

# Modify multiple AE columns to exercise LISTALL with multiple variables
if ("AESEV" %in% names(ae_modified)) {
  sev_rows <- sample(seq_len(n_ae), min(8, n_ae))
  ae_modified$AESEV[sev_rows] <- sample(c("MILD", "MODERATE", "SEVERE"),
                                         length(sev_rows), replace = TRUE)
}
if ("AEREL" %in% names(ae_modified)) {
  rel_rows <- sample(seq_len(n_ae), min(5, n_ae))
  ae_modified$AEREL[rel_rows] <- sample(c("RELATED", "NOT RELATED", "POSSIBLY RELATED"),
                                         length(rel_rows), replace = TRUE)
}
if ("AEOUT" %in% names(ae_modified)) {
  out_rows <- sample(seq_len(n_ae), min(3, n_ae))
  ae_modified$AEOUT[out_rows] <- "RECOVERED/RESOLVED"
}

ae_id_vars <- intersect(c("USUBJID", "AESEQ"), names(ae_pharma))
if (length(ae_id_vars) >= 1) {
  ae_result <- cdisc_compare(ae_pharma, ae_modified, domain = "AE",
                              standard = "SDTM", id_vars = ae_id_vars)

  # Show the LISTALL detailed report
  cat("\n  --- generate_detailed_report() LISTALL output: ---\n\n")
  generate_detailed_report(ae_result)
}


# =============================================================================
# PHARMA TEST 6: get_all_differences() — unified output
# =============================================================================
cat("\n\n--- PHARMA TEST 6: get_all_differences() on AE ---------------------\n")
if (exists("ae_result")) {
  all_diffs <- get_all_differences(ae_result)
  cat(sprintf("  Unified diff table: %d rows x %d cols\n", nrow(all_diffs), ncol(all_diffs)))
  cat(sprintf("  Columns: %s\n", paste(names(all_diffs), collapse = ", ")))
  cat(sprintf("  Variables with diffs: %s\n",
              paste(unique(all_diffs$Variable), collapse = ", ")))
  cat(sprintf("  Has 'Row' column: %s (should be FALSE for key-based)\n",
              "Row" %in% names(all_diffs)))
  if (nrow(all_diffs) > 0) {
    cat("\n  First 15 rows:\n")
    print(head(all_diffs, 15), row.names = FALSE)
  }
}


# =============================================================================
# PHARMA TEST 7: CDISC validation on real pharmaverse data
# =============================================================================
cat("\n\n--- PHARMA TEST 7: CDISC validation on pharmaverse SDTM DM ---------\n")
val_dm <- validate_cdisc(dm_pharma, domain = "DM", standard = "SDTM")
n_err <- sum(val_dm$severity == "ERROR")
n_warn <- sum(val_dm$severity == "WARNING")
n_info <- sum(val_dm$severity == "INFO")
cat(sprintf("  Errors: %d | Warnings: %d | Info: %d\n", n_err, n_warn, n_info))
if (n_err > 0) {
  cat("  ERRORS:\n")
  for (i in which(val_dm$severity == "ERROR")) {
    cat(sprintf("    %s: %s\n", val_dm$variable[i], val_dm$message[i]))
  }
}
if (n_warn > 0) {
  cat("  WARNINGS:\n")
  for (i in which(val_dm$severity == "WARNING")) {
    cat(sprintf("    %s: %s\n", val_dm$variable[i], val_dm$message[i]))
  }
}


# =============================================================================
# PHARMA TEST 8: Summary report on real-world comparison
# =============================================================================
cat("\n\n--- PHARMA TEST 8: Summary report (LB tolerance comparison) ---------\n")
if (exists("result_tol_8")) {
  generate_summary_report(result_tol_8)
}


# =============================================================================
# PHARMA TEST 9: VS comparison with explicit domain
# =============================================================================
cat("\n\n--- PHARMA TEST 9: VS vital signs comparison -------------------------\n")
vs_modified <- vs_pharma
set.seed(321)
if ("VSSTRESN" %in% names(vs_modified)) {
  vs_rows <- which(!is.na(vs_modified$VSSTRESN))
  edit_rows <- sample(vs_rows, min(20, length(vs_rows)))
  vs_modified$VSSTRESN[edit_rows] <- vs_modified$VSSTRESN[edit_rows] *
    (1 + runif(length(edit_rows), -0.02, 0.02))
}

vs_id_vars <- intersect(c("USUBJID", "VSSEQ"), names(vs_pharma))
if (length(vs_id_vars) >= 1) {
  vs_result <- cdisc_compare(vs_pharma, vs_modified, domain = "VS",
                              standard = "SDTM", id_vars = vs_id_vars)
  print(vs_result)
}


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n==============================================================\n")
cat("  PHARMAVERSE INTEGRATION TESTS COMPLETE\n")
cat("==============================================================\n")
cat("  Data sources: pharmaversesdtm, pharmaverseadam\n")
cat("  Features tested:\n")
cat("    - Domain auto-detection on real CDISC data\n")
cat("    - Key-based matching (USUBJID, AESEQ, PARAMCD, etc.)\n")
cat("    - Tolerance at 4 levels (0, 1e-12, 1e-8, 0.001)\n")
cat("    - Floating-point noise elimination (SAS→R artifact)\n")
cat("    - LISTALL detailed report with multiple variables\n")
cat("    - get_all_differences() unified output (Row dropped for key-based)\n")
cat("    - CDISC validation on real-world SDTM DM\n")
cat("    - Summary report on tolerance comparison\n")
cat("    - VS vital signs comparison with numeric diffs\n")
cat("==============================================================\n\n")
