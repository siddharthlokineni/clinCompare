# clinCompare

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/clinCompare)](https://CRAN.R-project.org/package=clinCompare)
[![R-CMD-check](https://github.com/siddharthlokineni/clinCompare/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siddharthlokineni/clinCompare/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Compare any two data frames at the dataset, variable, and observation level in a single call.** For clinical trial data, an optional CDISC validation layer checks SDTM and ADaM conformance automatically.

## Installation

```r
# Install from CRAN (when available)
install.packages("clinCompare")

# Development version
devtools::install_github("siddharthlokineni/clinCompare")
```

## 1. Compare Any Two Data Frames

`compare_datasets()` is the main entry point. One call gives you all three comparison levels: structural overview, variable-level discrepancies, and row-by-row value differences.

```r
library(clinCompare)

# Two versions of a study dataset — SUBJ02's age changed, SUBJ03's sex changed
baseline <- data.frame(
  USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
  AGE     = c(45, 52, 38),
  SEX     = c("M", "F", "M"),
  stringsAsFactors = FALSE
)

updated <- data.frame(
  USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
  AGE     = c(45, 53, 38),
  SEX     = c("M", "F", "F"),
  stringsAsFactors = FALSE
)

result <- compare_datasets(baseline, updated)
result
#> clinCompare: Dataset Comparison
#> ----------------------------------------
#> Base:    3 rows x 3 cols
#> Compare: 3 rows x 3 cols
#> Columns: 3 common
#> Value differences: 2 across 2 column(s)
#> ----------------------------------------
```

### Drill into the details

The result is a structured list. Pull out whichever level you need:

```r
# Which columns differ, and where?
result$observation_comparison$discrepancies
#> USUBJID     AGE     SEX
#>       0       1       1

# Exact row-level diffs for AGE
result$observation_comparison$details$AGE
#>   Row Value_in_df1 Value_in_df2
#> 1   2           52           53

# Columns only in one dataset?
result$extra_in_df1   # character(0) — none
result$extra_in_df2   # character(0) — none

# Type mismatches?
result$type_mismatches  # NULL — all types match
```

### When datasets have different structures

`compare_datasets()` handles mismatched dimensions gracefully — no errors, just clear reporting:

```r
# Different columns and different row counts
v1 <- data.frame(ID = 1:3, score = c(80, 90, 70))
v2 <- data.frame(ID = 1:5, score = c(80, 90, 70, 85, 60), grade = c("B", "A", "C", "B", "D"))

result <- compare_datasets(v1, v2)
result
#> clinCompare: Dataset Comparison
#> ----------------------------------------
#> Base:    3 rows x 2 cols
#> Compare: 5 rows x 3 cols
#> Columns: 2 common, 0 only in base, 1 only in compare
#> Row counts differ (3 vs 5); positional comparison skipped.
#> ----------------------------------------

result$extra_in_df2
#> [1] "grade"
```

## 2. Prepare and Clean Before Comparing

Real data is messy. Prepare your datasets first:

```r
# Remove duplicates and standardize column name case
cleaned <- clean_dataset(raw_data, remove_duplicates = TRUE, convert_to_case = "upper")

# Handle missing values (exclude, replace with 0, mean, median, or flag)
filled <- handle_missing_values(cleaned, method = "median")

# Sort and filter both datasets the same way before comparing
prepped <- prepare_datasets(df1, df2, sort_columns = "USUBJID",
                            filter_criteria = "AGE > 18")

# Then compare the prepared versions
result <- compare_datasets(prepped$df1, prepped$df2)
```

## 3. Group-Wise Comparison

Compare within subgroups — useful for multi-site or multi-arm studies:

```r
by_site <- compare_by_group(df1, df2, group_vars = "SITEID")
# Returns a named list: one comparison result per site
by_site$SITE01
by_site$SITE02
```

## 4. CDISC Validation (Clinical Trial Data)

This is what makes clinCompare different. When you're working with SDTM or ADaM data, clinCompare validates both datasets against CDISC standards **while** comparing them.

### Validate a single dataset

```r
# Build a realistic SDTM DM domain
dm <- data.frame(
  STUDYID  = rep("CLIN-001", 3),
  DOMAIN   = rep("DM", 3),
  USUBJID  = c("CLIN-001-001", "CLIN-001-002", "CLIN-001-003"),
  SUBJID   = c("001", "002", "003"),
  RFSTDTC  = c("2025-01-15", "2025-01-16", "2025-02-01"),
  RFENDTC  = c("2025-07-15", "2025-07-16", "2025-08-01"),
  SITEID   = rep("SITE01", 3),
  SEX      = c("M", "F", "M"),
  AGE      = c(45, 52, 38),
  AGEU     = rep("YEARS", 3),
  RACE     = c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN"),
  ARMCD    = rep("TRT01", 3),
  ARM      = rep("Treatment A", 3),
  COUNTRY  = rep("USA", 3),
  ACTARMCD = rep("TRT01", 3),
  ACTARM   = rep("Treatment A", 3),
  stringsAsFactors = FALSE
)

# Validate against SDTM DM specs
validation <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
print_cdisc_validation(validation)
#> === CDISC Validation Results ===
#> ERROR: Missing Required Variable: ETHNIC
#> WARNING: Missing Expected Variable: RFXSTDTC
#> WARNING: Missing Expected Variable: RFXENDTC
#> ...
```

### Compare two dataset versions with CDISC checks

This is the flagship workflow — compare interim vs. final data, catch both data differences and CDISC issues in one step:

```r
# Simulate interim → final: one subject's race was corrected, a new subject added
dm_interim <- dm  # the dataset from above

dm_final <- dm
dm_final$RACE[2] <- "WHITE"                               # correction
dm_final$ETHNIC <- rep("NOT HISPANIC OR LATINO", 3)       # added missing variable
dm_final <- rbind(dm_final, data.frame(                    # new subject
  STUDYID = "CLIN-001", DOMAIN = "DM",
  USUBJID = "CLIN-001-004", SUBJID = "004",
  RFSTDTC = "2025-03-01", RFENDTC = "2025-09-01",
  SITEID = "SITE02", SEX = "F", AGE = 29, AGEU = "YEARS",
  RACE = "ASIAN", ARMCD = "TRT01", ARM = "Treatment A",
  COUNTRY = "USA", ACTARMCD = "TRT01", ACTARM = "Treatment A",
  ETHNIC = "NOT HISPANIC OR LATINO",
  stringsAsFactors = FALSE
))

# Compare with CDISC validation — specify domain explicitly for reliability
result <- cdisc_compare(dm_interim, dm_final,
                        domain = "DM", standard = "SDTM")
result
#> clinCompare: CDISC Comparison Results
#> ----------------------------------------
#> Domain: DM (SDTM)
#> Base:    3 rows x 16 cols
#> Compare: 4 rows x 17 cols
#> Matching: positional
#> Differences: 2 attribute, 1 value
#> CDISC: FAIL (1 errors, 2 warnings)
#> ----------------------------------------

# Generate an interactive HTML report with dashboard
generate_cdisc_report(result,
                      output_format = "html",
                      file_name = "dm_interim_vs_final")
# Opens a self-contained HTML file with KPI cards and Chart.js visualizations
```

### Key-based matching

When row order differs between datasets, match by subject ID instead of position:

```r
result <- cdisc_compare(
  dm_interim, dm_final,
  domain   = "DM",
  standard = "SDTM",
  id_vars  = c("USUBJID")
)
# Rows matched by USUBJID; unmatched subjects reported separately
result$unmatched_rows$df2_only   # CLIN-001-004 (new in final)
```

### ADaM validation works the same way

```r
adsl_v1 <- read.csv("adsl_draft.csv")
adsl_v2 <- read.csv("adsl_final.csv")

result <- cdisc_compare(adsl_v1, adsl_v2,
                        domain = "ADSL", standard = "ADaM",
                        id_vars = "USUBJID")

generate_cdisc_report(result, output_format = "html",
                      file_name = "adsl_comparison")
```

## 5. Reports

Three report formats cover different needs:

```r
# Quick console summary
generate_summary_report(result)

# Full observation-level details
generate_detailed_report(result)

# Interactive HTML with dashboard (works offline — Chart.js is bundled)
generate_cdisc_report(result, output_format = "html", file_name = "report")

# Plain text file for audit trails
generate_cdisc_report(result, output_format = "text", file_name = "report")
```

## CDISC Coverage

clinCompare ships with hand-curated metadata for **51 SDTM domains** (IG 3.4, with 3.3 support) and **14 ADaM datasets** (IG 1.3, with 1.2/1.1 provenance tracking). The metadata lives in `inst/extdata/` as CSV files — easy to review, diff, and contribute to.

<details>
<summary>Full domain list (click to expand)</summary>

**SDTM** (domains marked * were introduced in IG 3.4):
AE, AG*, BE*, BS*, CE, CM, CO, CP*, DA, DD, DM, DS, DV, EC, EG, EX, FA, GF*, HO, IE, IS, LB, MB, MH, MI, ML*, MS, PC, PE, PP, PR, QS, RELREC, RS, SC, SE, SM*, SS, SU, SUPPQUAL, SV, TA, TD*, TE, TI, TM*, TR, TS, TU, TV, VS

**ADaM**:
ADAE, ADCM, ADEG, ADEFF, ADEX, ADLB, ADMH, ADPC, ADPP, ADRS, ADSL, ADTR, ADTTE, ADVS

</details>

The canonical machine-readable source is the [CDISC Library API](https://www.cdisc.org/cdisc-library) (requires CDISC membership). For regulatory submissions, always cross-reference with the official CDISC Library.

**Disclaimer:** clinCompare is a quality-assurance and exploratory analysis tool. It is not a substitute for official CDISC compliance validation software (e.g., Pinnacle 21). For regulatory submissions, always cross-reference with your organization's validated tools.

## Requirements

R >= 3.5.0, dplyr >= 1.0.0, rlang >= 0.4.0, tidyr >= 1.0.0. Optional: ggplot2 >= 3.0.0 (for visualizations), knitr/rmarkdown (for vignettes).

## Contributing

Contributions welcome — especially CDISC metadata corrections. The metadata is in `inst/extdata/sdtm_metadata.csv` and `inst/extdata/adam_metadata.csv`, so domain experts can review and submit PRs without writing R code. See the [GitHub repository](https://github.com/siddharthlokineni/clinCompare).

## License

MIT License. See [LICENSE](LICENSE.md) for details.

**Author:** Siddharth Lokineni ([GitHub](https://github.com/siddharthlokineni))
