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
cat("\n\n--- TEST 1: compare_datasets() on DM (same row count) ---------------\n")
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
cat("\n\n--- TEST 5: cdisc_compare() on DM (positional) ----------------------\n")
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

cat("\n--- TEST 5c: cdisc_compare() on LB (16,000 rows, positional) --------\n")
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

cat("\n--- TEST 9b: generate_detailed_report() ------------------------------\n")
generate_detailed_report(dm_cdisc)

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
cat(sprintf("    extract_cdisc_version\n"))
cat("==============================================================\n\n")
