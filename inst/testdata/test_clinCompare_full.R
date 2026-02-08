#!/usr/bin/env Rscript
# =============================================================================
# clinCompare — Full Integration Test
# =============================================================================
# Tests every major function against realistic synthetic SDTM and ADaM data.
# Run after: devtools::load_all() or library(clinCompare)
#
# Usage:
#   devtools::load_all("~/Desktop/cowork/clinCompare")
#   source("inst/testdata/test_clinCompare_full.R")
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat("  clinCompare — Full Integration Test\n")
cat("  Study: CLIN-2025-042 | 500 subjects | 5 sites | 3 arms\n")
cat("==============================================================\n\n")

# --- Load test data ---------------------------------------------------------
data_dir <- system.file("testdata", package = "clinCompare")
if (!nzchar(data_dir)) {
  # Fallback for devtools::load_all() (inst/ not installed yet)
  data_dir <- file.path(getwd(), "inst", "testdata")
}
stopifnot(dir.exists(data_dir))

cat("Loading datasets from:", data_dir, "\n\n")

dm_v1   <- read.csv(file.path(data_dir, "dm_v1.csv"),   stringsAsFactors = FALSE)
dm_v2   <- read.csv(file.path(data_dir, "dm_v2.csv"),   stringsAsFactors = FALSE)
ae_v1   <- read.csv(file.path(data_dir, "ae_v1.csv"),   stringsAsFactors = FALSE)
ae_v2   <- read.csv(file.path(data_dir, "ae_v2.csv"),   stringsAsFactors = FALSE)
lb_v1   <- read.csv(file.path(data_dir, "lb_v1.csv"),   stringsAsFactors = FALSE)
lb_v2   <- read.csv(file.path(data_dir, "lb_v2.csv"),   stringsAsFactors = FALSE)
vs_v1   <- read.csv(file.path(data_dir, "vs_v1.csv"),   stringsAsFactors = FALSE)
vs_v2   <- read.csv(file.path(data_dir, "vs_v2.csv"),   stringsAsFactors = FALSE)
ex_v1   <- read.csv(file.path(data_dir, "ex_v1.csv"),   stringsAsFactors = FALSE)
ex_v2   <- read.csv(file.path(data_dir, "ex_v2.csv"),   stringsAsFactors = FALSE)
adsl_v1 <- read.csv(file.path(data_dir, "adsl_v1.csv"), stringsAsFactors = FALSE)
adsl_v2 <- read.csv(file.path(data_dir, "adsl_v2.csv"), stringsAsFactors = FALSE)
adae_v1 <- read.csv(file.path(data_dir, "adae_v1.csv"), stringsAsFactors = FALSE)
adae_v2 <- read.csv(file.path(data_dir, "adae_v2.csv"), stringsAsFactors = FALSE)
adlb_v1 <- read.csv(file.path(data_dir, "adlb_v1.csv"), stringsAsFactors = FALSE)
adlb_v2 <- read.csv(file.path(data_dir, "adlb_v2.csv"), stringsAsFactors = FALSE)

cat(sprintf("  DM:   %d vs %d subjects\n", nrow(dm_v1), nrow(dm_v2)))
cat(sprintf("  AE:   %d vs %d records\n",  nrow(ae_v1), nrow(ae_v2)))
cat(sprintf("  LB:   %d vs %d records\n",  nrow(lb_v1), nrow(lb_v2)))
cat(sprintf("  VS:   %d vs %d records\n",  nrow(vs_v1), nrow(vs_v2)))
cat(sprintf("  EX:   %d vs %d records\n",  nrow(ex_v1), nrow(ex_v2)))
cat(sprintf("  ADSL: %d vs %d subjects\n", nrow(adsl_v1), nrow(adsl_v2)))
cat(sprintf("  ADAE: %d vs %d records\n",  nrow(adae_v1), nrow(adae_v2)))
cat(sprintf("  ADLB: %d vs %d records\n",  nrow(adlb_v1), nrow(adlb_v2)))


# =============================================================================
# TEST 1: compare_datasets() — three-level comparison
# =============================================================================
cat("\n\n--- TEST 1: compare_datasets() on DM (different row counts) ---------\n")
dm_result <- compare_datasets(dm_v1, dm_v2)
print(dm_result)

cat("\nDrill-down — columns only in v2:\n")
cat("  ", paste(dm_result$extra_in_df2, collapse = ", "), "\n")

cat("Observation differences per column:\n")
obs <- dm_result$observation_comparison
if (!is.null(obs$discrepancies)) {
  changed <- obs$discrepancies[obs$discrepancies > 0]
  for (nm in names(changed)) {
    cat(sprintf("  %s: %d rows differ\n", nm, changed[nm]))
  }
}

cat("\n--- TEST 1b: compare_datasets() on LB (16,000 rows) ----------------\n")
lb_result <- compare_datasets(lb_v1, lb_v2)
print(lb_result)

cat("\n--- TEST 1c: compare_datasets() with vars parameter ------------------\n")
lb_vars_result <- compare_datasets(lb_v1, lb_v2, vars = c("LBSTRESN", "LBNRIND"))
cat(sprintf("  Vars requested: LBSTRESN, LBNRIND\n"))
cat(sprintf("  Value differences: %d (should match TEST 1b since those are the only differing cols)\n",
            sum(lb_vars_result$observation_comparison$discrepancies)))
# Structural comparison should still show all columns
cat(sprintf("  Common cols reported: %d (should be 14 — all columns)\n",
            length(lb_vars_result$common_columns)))


# =============================================================================
# TEST 2: compare_variables() and compare_observations() standalone
# =============================================================================
cat("\n\n--- TEST 2: compare_variables() on AE --------------------------------\n")
ae_var <- compare_variables(ae_v1, ae_v2)
cat(sprintf("  Column discrepancies: %d\n", ae_var$discrepancies))
cat(sprintf("  Common columns: %s\n", paste(ae_var$details$common_columns, collapse = ", ")))

cat("\n--- TEST 2b: compare_observations() on EX (same rows) ---------------\n")
ex_obs <- compare_observations(ex_v1, ex_v2)
total_diffs <- sum(ex_obs$discrepancies)
cat(sprintf("  Total value differences: %d\n", total_diffs))
if (total_diffs > 0) {
  changed_cols <- names(ex_obs$discrepancies[ex_obs$discrepancies > 0])
  cat(sprintf("  Columns with diffs: %s\n", paste(changed_cols, collapse = ", ")))
  # Show first few diffs from first changed column
  first_col <- changed_cols[1]
  cat(sprintf("  Sample diffs in %s:\n", first_col))
  print(head(ex_obs$details[[first_col]], 5))
}


# =============================================================================
# TEST 3: detect_cdisc_domain() — auto-detection
# =============================================================================
cat("\n\n--- TEST 3: detect_cdisc_domain() ------------------------------------\n")
cat("  Note: Auto-detection uses column-name matching. Some domains share\n")
cat("  similar columns (e.g. VS/SV, ADAE/ADCM) and may trigger ambiguity\n")
cat("  warnings. Always specify domain explicitly for reliable results.\n\n")
domains_to_test <- list(DM = dm_v1, AE = ae_v1, LB = lb_v1, VS = vs_v1,
                        EX = ex_v1, ADSL = adsl_v1, ADAE = adae_v1, ADLB = adlb_v1)

for (nm in names(domains_to_test)) {
  det <- tryCatch(
    detect_cdisc_domain(domains_to_test[[nm]]),
    warning = function(w) {
      # Capture but continue — ambiguity warnings are expected
      result <- suppressWarnings(detect_cdisc_domain(domains_to_test[[nm]]))
      result$ambiguity_warning <- conditionMessage(w)
      result
    }
  )
  status <- if (!is.null(det$ambiguity_warning)) " [ambiguous]" else ""
  cat(sprintf("  %-6s -> Detected: %-6s (%s) | Confidence: %.0f%%%s\n",
              nm, det$domain, det$standard, det$confidence * 100, status))
}


# =============================================================================
# TEST 4: validate_cdisc() — single-dataset validation
# =============================================================================
cat("\n\n--- TEST 4: validate_cdisc() on DM v1 --------------------------------\n")
dm_val <- validate_cdisc(dm_v1, domain = "DM", standard = "SDTM")
n_err  <- sum(dm_val$severity == "ERROR")
n_warn <- sum(dm_val$severity == "WARNING")
n_info <- sum(dm_val$severity == "INFO")
cat(sprintf("  Errors: %d | Warnings: %d | Info: %d\n", n_err, n_warn, n_info))
if (n_err > 0) {
  cat("  Missing required variables:\n")
  errs <- dm_val[dm_val$severity == "ERROR", ]
  for (i in seq_len(nrow(errs))) {
    cat(sprintf("    - %s\n", errs$variable[i]))
  }
}

cat("\n--- TEST 4b: validate_cdisc() on ADSL v1 -----------------------------\n")
adsl_val <- validate_cdisc(adsl_v1, domain = "ADSL", standard = "ADaM")
n_err2  <- sum(adsl_val$severity == "ERROR")
n_warn2 <- sum(adsl_val$severity == "WARNING")
cat(sprintf("  Errors: %d | Warnings: %d\n", n_err2, n_warn2))


# =============================================================================
# TEST 5: cdisc_compare() — flagship comparison + CDISC validation
# =============================================================================
cat("\n\n--- TEST 5: cdisc_compare() on DM (auto ID vars) --------------------\n")
dm_cdisc <- cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM")
print(dm_cdisc)

cat("\n--- TEST 5b: cdisc_compare() on AE with id_vars ----------------------\n")
ae_cdisc <- cdisc_compare(ae_v1, ae_v2, domain = "AE", standard = "SDTM",
                           id_vars = c("USUBJID", "AESEQ"))
print(ae_cdisc)

cat("\nUnmatched rows:\n")
if (!is.null(ae_cdisc$unmatched_rows)) {
  cat(sprintf("  Only in v1: %d rows\n", nrow(ae_cdisc$unmatched_rows$df1_only)))
  cat(sprintf("  Only in v2: %d rows\n", nrow(ae_cdisc$unmatched_rows$df2_only)))
}

cat("\n--- TEST 5c: cdisc_compare() on LB (16,000 rows, auto ID vars) ------\n")
t0 <- Sys.time()
lb_cdisc <- cdisc_compare(lb_v1, lb_v2, domain = "LB", standard = "SDTM")
elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
print(lb_cdisc)
cat(sprintf("  Completed in %.2f seconds\n", elapsed))

cat("\n--- TEST 5d: cdisc_compare() on ADSL (ADaM) --------------------------\n")
adsl_cdisc <- cdisc_compare(adsl_v1, adsl_v2, domain = "ADSL", standard = "ADaM")
print(adsl_cdisc)

cat("\n--- TEST 5e: cdisc_compare() on ADLB with id_vars --------------------\n")
adlb_cdisc <- cdisc_compare(adlb_v1, adlb_v2, domain = "ADLB", standard = "ADaM",
                             id_vars = c("USUBJID", "PARAMCD", "AVISITN"))
print(adlb_cdisc)

cat("\n--- TEST 5f: cdisc_compare() on VS (explicit domain) -----------------\n")
vs_cdisc <- cdisc_compare(vs_v1, vs_v2, domain = "VS", standard = "SDTM")
print(vs_cdisc)

cat("\n--- TEST 5g: cdisc_compare() on ADAE with id_vars ---------------------\n")
adae_cdisc <- cdisc_compare(adae_v1, adae_v2, domain = "ADAE", standard = "ADaM",
                             id_vars = c("USUBJID", "AESEQ"))
print(adae_cdisc)


# =============================================================================
# TEST 6: summary() S3 method
# =============================================================================
cat("\n\n--- TEST 6: summary() on cdisc_compare result ------------------------\n")
dm_summary <- summary(dm_cdisc)
print(dm_summary)


# =============================================================================
# TEST 7: Data preparation functions
# =============================================================================
cat("\n\n--- TEST 7: clean_dataset() ------------------------------------------\n")
# Add some duplicates and messy case
dm_messy <- rbind(dm_v1, dm_v1[1:5, ])  # add 5 duplicate rows
dm_clean <- clean_dataset(dm_messy, remove_duplicates = TRUE)
cat(sprintf("  Before: %d rows | After: %d rows (removed %d duplicates)\n",
            nrow(dm_messy), nrow(dm_clean), nrow(dm_messy) - nrow(dm_clean)))

cat("\n--- TEST 7b: prepare_datasets() --------------------------------------\n")
prepped <- prepare_datasets(dm_v1, dm_v2, sort_columns = "USUBJID")
cat(sprintf("  Prepared df1: %d rows | df2: %d rows\n",
            nrow(prepped$df1), nrow(prepped$df2)))


# =============================================================================
# TEST 8: compare_by_group()
# =============================================================================
cat("\n\n--- TEST 8: compare_by_group() on DM by SITEID -----------------------\n")
# Use only v1 rows that exist in v2 for positional comparison
common_subj <- intersect(dm_v1$USUBJID, dm_v2$USUBJID)
dm1_common <- dm_v1[dm_v1$USUBJID %in% common_subj, ]
dm2_common <- dm_v2[dm_v2$USUBJID %in% common_subj, ]
# Keep only columns in both
common_cols <- intersect(names(dm1_common), names(dm2_common))
dm1_common <- dm1_common[, common_cols]
dm2_common <- dm2_common[, common_cols]

by_site <- compare_by_group(dm1_common, dm2_common, group_vars = "SITEID")
cat(sprintf("  Sites compared: %d\n", length(by_site)))
for (site in names(by_site)) {
  r <- by_site[[site]]
  if (!is.null(r)) {
    obs <- r$observation_comparison
    n_diff <- if (!is.null(obs$discrepancies)) sum(obs$discrepancies) else 0
    cat(sprintf("    %s: %d value differences\n", site, n_diff))
  }
}


# =============================================================================
# TEST 9: Report generation
# =============================================================================
cat("\n\n--- TEST 9: generate_summary_report() --------------------------------\n")
generate_summary_report(dm_cdisc)

cat("\n--- TEST 9b: generate_detailed_report() on DM -------------------------\n")
generate_detailed_report(dm_cdisc)

cat("\n--- TEST 9b2: generate_detailed_report() Full Detail on AE (with diffs) -\n")
ae_for_listall <- cdisc_compare(ae_v1, ae_v2, domain = "AE",
                                 id_vars = c("STUDYID", "USUBJID", "AESEQ"))
generate_detailed_report(ae_for_listall)

cat("\n--- TEST 9c: generate_cdisc_report() — HTML --------------------------\n")
html_file <- file.path(tempdir(), "dm_comparison_test.html")
generate_cdisc_report(dm_cdisc, output_format = "html", file_name = html_file)
cat(sprintf("  HTML report: %s (%.0f KB)\n", html_file,
            file.size(html_file) / 1024))

cat("\n--- TEST 9d: generate_cdisc_report() — text --------------------------\n")
txt_file <- file.path(tempdir(), "dm_comparison_test")
generate_cdisc_report(dm_cdisc, output_format = "text", file_name = txt_file)


# =============================================================================
# TEST 10: extract_cdisc_version() from TS domain
# =============================================================================
cat("\n\n--- TEST 10: extract_cdisc_version() ---------------------------------\n")
ts_data <- data.frame(
  STUDYID  = rep("CLIN-2025-042", 4),
  TSPARMCD = c("SDTIGVER", "ADAMIGVR", "TITLE", "SPONSOR"),
  TSPARM   = c("SDTM IG Version", "ADaM IG Version",
                "Protocol Title", "Study Sponsor"),
  TSVAL    = c("3.4", "1.3",
                "A Phase 3 Study of Treatment A vs Placebo",
                "Pharma Corp"),
  stringsAsFactors = FALSE
)
version_info <- extract_cdisc_version(ts_data)
cat(sprintf("  SDTM IG: %s | ADaM IG: %s\n",
            version_info$sdtm_ig_version, version_info$adam_ig_version))
cat(sprintf("  Note: %s\n", version_info$version_note))


# =============================================================================
# TEST 11: Numeric tolerance (CRITERION) — stress test
# =============================================================================
cat("\n\n--- TEST 11: Numeric tolerance (CRITERION) stress test ---------------\n")
cat("  Scenario: Inject floating-point rounding noise into LB to simulate\n")
cat("  SAS->R numeric conversion artifacts, then filter with tolerance.\n\n")

# Create noisy copy of lb_v1 with tiered floating-point noise
lb_noisy <- lb_v1
set.seed(2025)
if ("LBSTRESN" %in% names(lb_noisy)) {
  numeric_idx <- which(!is.na(lb_noisy$LBSTRESN))
  n_numeric <- length(numeric_idx)

  # Tier 1: Machine epsilon noise (~1e-14) — 500 rows
  tier1 <- sample(numeric_idx, min(500, n_numeric))
  lb_noisy$LBSTRESN[tier1] <- lb_noisy$LBSTRESN[tier1] +
    runif(length(tier1), -1e-13, 1e-13)

  # Tier 2: SAS rounding noise (~1e-9) — 200 rows
  tier2 <- sample(setdiff(numeric_idx, tier1), min(200, n_numeric - 500))
  lb_noisy$LBSTRESN[tier2] <- lb_noisy$LBSTRESN[tier2] +
    runif(length(tier2), -1e-8, 1e-8)

  # Tier 3: Real changes (0.01–5.0) — 20 rows (should always flag)
  tier3 <- sample(setdiff(numeric_idx, c(tier1, tier2)),
                  min(20, n_numeric - 700))
  lb_noisy$LBSTRESN[tier3] <- lb_noisy$LBSTRESN[tier3] +
    runif(length(tier3), 0.01, 5.0)

  cat(sprintf("  Noise injected: %d tier-1 (~1e-14), %d tier-2 (~1e-9), %d tier-3 (>0.01)\n\n",
              length(tier1), length(tier2), length(tier3)))
}

cat("--- TEST 11a: tolerance = 0 (catches ALL noise) ----------------------\n")
r0 <- compare_datasets(lb_v1, lb_noisy, tolerance = 0)
n0 <- sum(r0$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Differences: %d\n", n0))

cat("\n--- TEST 11b: tolerance = 1e-12 (filters machine epsilon) -----------\n")
r12 <- compare_datasets(lb_v1, lb_noisy, tolerance = 1e-12)
n12 <- sum(r12$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Differences: %d (eliminated %d machine-epsilon diffs)\n", n12, n0 - n12))

cat("\n--- TEST 11c: tolerance = 1e-7 (filters SAS rounding) ---------------\n")
r7 <- compare_datasets(lb_v1, lb_noisy, tolerance = 1e-7)
n7 <- sum(r7$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Differences: %d (eliminated %d SAS rounding diffs)\n", n7, n0 - n7))

cat("\n--- TEST 11d: tolerance = 0.005 (only real changes) -----------------\n")
r3 <- compare_datasets(lb_v1, lb_noisy, tolerance = 0.005)
n3 <- sum(r3$observation_comparison$discrepancies, na.rm = TRUE)
cat(sprintf("  Differences: %d (only genuine data changes remain)\n", n3))
print(r3)

cat(sprintf("\n  SUMMARY: tolerance=1e-7 eliminated %d false positives from SAS->R noise!\n", n0 - n7))

# Also test via cdisc_compare with id_vars
cat("\n--- TEST 11e: cdisc_compare() with tolerance + id_vars ---------------\n")
lb_cdisc_tol <- cdisc_compare(lb_v1, lb_noisy, domain = "LB",
                               id_vars = c("STUDYID", "USUBJID", "LBSEQ"),
                               tolerance = 1e-7)
cat(sprintf("  cdisc_compare(tolerance=1e-7): %d diffs\n",
            sum(lb_cdisc_tol$observation_comparison$discrepancies, na.rm = TRUE)))


# =============================================================================
# TEST 12: get_all_differences() — Unified output data frame
# =============================================================================
cat("\n\n--- TEST 12: get_all_differences() -----------------------------------\n")

# From cdisc_compare result
cat("\n--- TEST 12a: Unified diffs from AE comparison -----------------------\n")
ae_result_for_diffs <- cdisc_compare(ae_v1, ae_v2, domain = "AE",
                                      id_vars = c("STUDYID", "USUBJID", "AESEQ"))
all_diffs <- get_all_differences(ae_result_for_diffs)
cat(sprintf("  Total rows in unified diff table: %d\n", nrow(all_diffs)))
cat(sprintf("  Columns: %s\n", paste(names(all_diffs), collapse = ", ")))
cat(sprintf("  Variables with diffs: %s\n",
            paste(unique(all_diffs$Variable), collapse = ", ")))
cat(sprintf("  Has 'Row' column: %s (should be FALSE for key-based)\n",
            "Row" %in% names(all_diffs)))
if (nrow(all_diffs) > 0) {
  cat("  First 10 rows:\n")
  print(head(all_diffs, 10), row.names = FALSE)
}

# Positional comparison should keep Row column
cat("\n--- TEST 12b: Unified diffs from positional compare (keeps Row) ------\n")
ds_diffs_pos <- get_all_differences(compare_datasets(lb_v1, lb_v2))
cat(sprintf("  Has 'Row' column: %s (should be TRUE for positional)\n",
            "Row" %in% names(ds_diffs_pos)))
cat(sprintf("  Total rows: %d\n", nrow(ds_diffs_pos)))

# DM with auto ID vars — key-based matching finds differences despite row mismatch
cat("\n--- TEST 12c: Unified diffs from key-based DM compare ----------------\n")
dm_result_keyed <- cdisc_compare(dm_v1, dm_v2, domain = "DM")
dm_diffs <- get_all_differences(dm_result_keyed)
cat(sprintf("  Rows: %d (key-based matching via auto ID vars)\n", nrow(dm_diffs)))
cat(sprintf("  Has 'Row' column: %s (should be FALSE for key-based)\n",
            "Row" %in% names(dm_diffs)))


# =============================================================================
# EDGE CASE / SAFETY TESTS (from director-level review)
# =============================================================================

cat("\n--- TEST 13: Tolerance validation ------------------------------------\n")
# 13a: Negative tolerance should error
err_neg <- tryCatch(compare_datasets(dm_v1, dm_v1, tolerance = -1), error = function(e) e$message)
cat(sprintf("  Negative tolerance: %s\n", if (grepl("non-negative", err_neg)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_neg)))

# 13b: NaN tolerance should error
err_nan <- tryCatch(compare_datasets(dm_v1, dm_v1, tolerance = NaN), error = function(e) e$message)
cat(sprintf("  NaN tolerance:      %s\n", if (grepl("non-negative", err_nan)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_nan)))

# 13c: Inf tolerance should error
err_inf <- tryCatch(compare_datasets(dm_v1, dm_v1, tolerance = Inf), error = function(e) e$message)
cat(sprintf("  Inf tolerance:      %s\n", if (grepl("non-negative", err_inf)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_inf)))

# 13d: NA tolerance should error
err_na <- tryCatch(compare_datasets(dm_v1, dm_v1, tolerance = NA), error = function(e) e$message)
cat(sprintf("  NA tolerance:       %s\n", if (grepl("non-negative", err_na)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_na)))

# 13e: Character tolerance should error
err_chr <- tryCatch(compare_datasets(dm_v1, dm_v1, tolerance = "0.01"), error = function(e) e$message)
cat(sprintf("  Character tolerance: %s\n", if (grepl("non-negative", err_chr)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_chr)))

# 13f: cdisc_compare tolerance validation
err_cdisc <- tryCatch(cdisc_compare(dm_v1, dm_v1, domain = "DM", standard = "SDTM", tolerance = -5), error = function(e) e$message)
cat(sprintf("  cdisc_compare(-5):  %s\n", if (grepl("non-negative", err_cdisc)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_cdisc)))


cat("\n--- TEST 14: Inf - Inf handling (NaN detection) ---------------------\n")
# Two datasets with Inf values — should be flagged as differences, not silently matched
inf_df1 <- data.frame(id = 1:4, val = c(1.0, Inf, -Inf, Inf))
inf_df2 <- data.frame(id = 1:4, val = c(1.0, Inf, -Inf, -Inf))
# Row 1: same (1.0 vs 1.0), Row 2: Inf vs Inf (NaN diff), Row 3: -Inf vs -Inf (NaN diff), Row 4: Inf vs -Inf (real diff)
inf_result_0 <- compare_observations(inf_df1, inf_df2, tolerance = 0)
inf_diffs_0 <- inf_result_0$discrepancies["val"]
cat(sprintf("  tolerance=0:   Inf diffs: %d (should be 3)\n", inf_diffs_0))

inf_result_t <- compare_observations(inf_df1, inf_df2, tolerance = 0.01)
inf_diffs_t <- inf_result_t$discrepancies["val"]
cat(sprintf("  tolerance=0.01: Inf diffs: %d (should be 3)\n", inf_diffs_t))

cat(sprintf("  Status: %s\n", if (inf_diffs_0 == 3 && inf_diffs_t == 3) "PASS" else "FAIL"))


cat("\n--- TEST 15: Duplicate key warning -----------------------------------\n")
# Create data with duplicate USUBJID keys
dup_df1 <- data.frame(
  STUDYID = rep("STUDY", 4),
  DOMAIN = rep("AE", 4),
  USUBJID = c("SUBJ01", "SUBJ01", "SUBJ02", "SUBJ02"),
  AETERM = c("Headache", "Nausea", "Fever", "Cough"),
  stringsAsFactors = FALSE
)
dup_df2 <- data.frame(
  STUDYID = rep("STUDY", 4),
  DOMAIN = rep("AE", 4),
  USUBJID = c("SUBJ01", "SUBJ01", "SUBJ02", "SUBJ02"),
  AETERM = c("Headache", "Nausea", "Fever", "Cough"),
  stringsAsFactors = FALSE
)
# Use cdisc_compare with id_vars that will produce duplicates
dup_warnings <- tryCatch({
  w <- character(0)
  withCallingHandlers(
    cdisc_compare(dup_df1, dup_df2, domain = "AE", standard = "SDTM",
                  id_vars = c("USUBJID")),
    warning = function(wn) { w <<- c(w, wn$message); invokeRestart("muffleWarning") }
  )
  w
}, error = function(e) paste("ERROR:", e$message))
has_dup_warn <- any(grepl("duplicate key", dup_warnings, ignore.case = TRUE))
cat(sprintf("  Duplicate key warning issued: %s\n", has_dup_warn))
cat(sprintf("  Status: %s\n", if (has_dup_warn) "PASS" else "FAIL"))


cat("\n--- TEST 16: WHERE clause validation ---------------------------------\n")
# 16a: Invalid expression (syntax error)
err_where1 <- tryCatch(
  cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM", where = "SEX ==== 'M'"),
  error = function(e) e$message
)
cat(sprintf("  Bad syntax:     %s\n", if (grepl("Invalid WHERE|WHERE filter failed", err_where1)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_where1)))

# 16b: Non-existent column
err_where2 <- tryCatch(
  cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM", where = "NONEXIST == 'X'"),
  error = function(e) e$message
)
cat(sprintf("  Bad column:     %s\n", if (grepl("WHERE filter failed|not found|object", err_where2)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_where2)))

# 16c: Empty string
err_where3 <- tryCatch(
  cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM", where = ""),
  error = function(e) e$message
)
cat(sprintf("  Empty string:   %s\n", if (grepl("non-empty", err_where3)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_where3)))

# 16d: Non-character
err_where4 <- tryCatch(
  cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM", where = 42),
  error = function(e) e$message
)
cat(sprintf("  Numeric input:  %s\n", if (grepl("non-empty character", err_where4)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_where4)))


cat("\n--- TEST 17: '+' id_vars edge cases ----------------------------------\n")
# 17a: Just "+" with no variables — should error
err_plus1 <- tryCatch(
  cdisc_compare(dm_v1, dm_v2, id_vars = c("+")),
  error = function(e) e$message
)
cat(sprintf("  Bare '+' only:  %s\n", if (grepl("at least one additional", err_plus1)) "BLOCKED (correct)" else paste("UNEXPECTED:", err_plus1)))

# 17b: "+" with valid extra variable — should work
plus_result <- tryCatch({
  suppressMessages(cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM",
                                  id_vars = c("+", "RACE")))
}, error = function(e) paste("ERROR:", e$message))
cat(sprintf("  '+' with extra: %s\n", if (is.list(plus_result) && !is.null(plus_result$observation_comparison)) "PASS" else paste("FAIL:", plus_result)))


cat("\n--- TEST 18: NA in key variables (sentinel test) ---------------------\n")
# Two datasets where NA and the string "NA" should NOT match
na_df1 <- data.frame(
  STUDYID = c("S1", "S1"),
  USUBJID = c(NA, "NA"),
  VAL = c(10, 20),
  stringsAsFactors = FALSE
)
na_df2 <- data.frame(
  STUDYID = c("S1", "S1"),
  USUBJID = c(NA, "NA"),
  VAL = c(99, 20),
  stringsAsFactors = FALSE
)
# Row 1: NA key -> should match on NA sentinel, find VAL diff (10 vs 99)
# Row 2: "NA" key -> should match on "NA" string, find no VAL diff (20 vs 20)
# Without sentinel fix, both rows would get the same key "S1||NA" causing collisions
na_warnings <- character(0)
na_result <- tryCatch({
  withCallingHandlers(
    cdisc_compare(na_df1, na_df2, domain = "DM", standard = "SDTM",
                  id_vars = c("STUDYID", "USUBJID")),
    warning = function(wn) { na_warnings <<- c(na_warnings, wn$message); invokeRestart("muffleWarning") },
    message = function(m) invokeRestart("muffleMessage")
  )
}, error = function(e) paste("ERROR:", e$message))

if (is.list(na_result)) {
  obs <- na_result$observation_comparison
  n_val_diffs <- if (!is.null(obs$discrepancies)) sum(obs$discrepancies, na.rm = TRUE) else 0
  cat(sprintf("  VAL diffs found: %d (should be 1 — only the NA-keyed row)\n", n_val_diffs))
  cat(sprintf("  Status: %s\n", if (n_val_diffs == 1) "PASS" else "FAIL"))
} else {
  cat(sprintf("  ERROR: %s\n", na_result))
}


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n==============================================================\n")
cat("  ALL TESTS COMPLETE\n")
cat("==============================================================\n")
cat(sprintf("  Datasets tested:     16 files (10 SDTM + 6 ADaM)\n"))
cat(sprintf("  Total records:       ~67,000\n"))
cat(sprintf("  Functions exercised:  compare_datasets, compare_variables,\n"))
cat(sprintf("    compare_observations, detect_cdisc_domain, validate_cdisc,\n"))
cat(sprintf("    cdisc_compare, summary, clean_dataset, prepare_datasets,\n"))
cat(sprintf("    compare_by_group, generate_summary_report,\n"))
cat(sprintf("    generate_detailed_report, generate_cdisc_report,\n"))
cat(sprintf("    extract_cdisc_version, get_all_differences\n"))
cat(sprintf("  New features:  tolerance (CRITERION), full detail report,\n"))
cat(sprintf("    unified output data frame, all-variable summary table\n"))
cat(sprintf("  Safety tests:  tolerance validation, Inf-Inf handling,\n"))
cat(sprintf("    duplicate key warnings, WHERE validation, '+' edge cases,\n"))
cat(sprintf("    NA key sentinel handling\n"))
cat("==============================================================\n\n")
