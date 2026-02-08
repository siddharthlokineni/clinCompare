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

## Quick Start

### Compare two data frames

```r
library(clinCompare)

baseline <- data.frame(
  USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
  AGE     = c(45, 52, 38),
  SEX     = c("M", "F", "M"),
  RACE    = c("WHITE", "WHITE", "ASIAN"),
  stringsAsFactors = FALSE
)

updated <- data.frame(
  USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
  AGE     = c(45, 53, 38),
  SEX     = c("M", "F", "F"),
  RACE    = c("WHITE", "WHITE", "ASIAN"),
  stringsAsFactors = FALSE
)

result <- compare_datasets(baseline, updated)
result
#> clinCompare: Dataset Comparison
#> ----------------------------------------
#> Base:    3 rows x 4 cols
#> Compare: 3 rows x 4 cols
#> Columns: 4 common
#> Value differences: 2 across 2 of 4 column(s); 2 of 3 obs (66.7%) differ
#>
#> Variable Summary of Differences:
#>   Variable             Type     N Obs  N Diffs   Max Diff Max % Diff   RMS Diff
#>   ----------------------------------------------------------------------------
#>   AGE                  NUM          3        1          1      1.92%          1
#>   SEX                  CHAR         3        1          .          .          .
#>
#> First 1 observation(s) differing in 'AGE':
#>  Row Value_in_df1 Value_in_df2 Diff PctDiff
#>    2           52           53   -1    1.92
#> ----------------------------------------
```

The result is a structured list you can drill into:

```r
# Per-column difference counts
result$observation_comparison$discrepancies
#> USUBJID     AGE     SEX    RACE
#>       0       1       1       0

# Exact row-level diffs for SEX
result$observation_comparison$details$SEX
#>   Row Value_in_df1 Value_in_df2
#> 1   3            M            F

# Columns only in one dataset
result$extra_in_df1   # character(0)
result$extra_in_df2   # character(0)

# Type mismatches between datasets
result$type_mismatches  # NULL, all types match
```

### When row counts or columns differ

```r
v1 <- data.frame(ID = 1:3, score = c(80, 90, 70))
v2 <- data.frame(ID = 1:5, score = c(80, 90, 70, 85, 60),
                 grade = c("B", "A", "C", "B", "D"))

compare_datasets(v1, v2)
#> clinCompare: Dataset Comparison
#> ----------------------------------------
#> Base:    3 rows x 2 cols
#> Compare: 5 rows x 3 cols
#> Columns: 2 common, 0 only in base, 1 only in compare
#> Row counts differ (3 vs 5); positional comparison skipped.
#> ----------------------------------------
```

Row-level comparison is skipped when dimensions differ. To compare these datasets, use `cdisc_compare()` with `id_vars` for key-based matching instead.

## CDISC Comparison (Clinical Trial Data)

`cdisc_compare()` is the flagship function. It compares two datasets, auto-detects CDISC domain and key variables, performs key-based row matching, and validates against CDISC standards.

### Basic usage with auto-detection

```r
# Domain, standard, and key variables are all auto-detected
result <- cdisc_compare(dm_v1, dm_v2)
#> ID variables auto-detected for SDTM DM: STUDYID, USUBJID

result
#> clinCompare: CDISC Comparison Results
#> ----------------------------------------
#> Domain: DM (SDTM)
#> Base:    306 rows x 26 cols
#> Compare: 307 rows x 26 cols
#> Matching: key-based (STUDYID, USUBJID)
#> Differences: 1 attribute, 5 value
#> Unmatched rows: 0 in base only, 1 in compare only
#> Value differences: 5 across 1 of 24 column(s); 5 of 306 obs (1.6%) differ
#>
#> Variable Summary of Differences:
#>   Variable             Type     N Obs  N Diffs   Max Diff Max % Diff   RMS Diff
#>   ----------------------------------------------------------------------------
#>   RACE                 CHAR       306        5          .          .          .
#>
#> First 5 observation(s) differing in 'RACE':
#>  STUDYID      USUBJID     Value_in_df1              Value_in_df2
#>  CDISCPILOT01 01-701-1440 WHITE                     MULTIPLE
#>  CDISCPILOT01 01-704-1010 WHITE                     MULTIPLE
#>  CDISCPILOT01 01-708-1297 WHITE                     MULTIPLE
#>  CDISCPILOT01 01-708-1353 BLACK OR AFRICAN AMERICAN MULTIPLE
#>  CDISCPILOT01 01-711-1284 WHITE                     MULTIPLE
#> CDISC: PASS (0 errors, 0 warnings)
#> ----------------------------------------
```

### Load from file paths

Domain is auto-detected from the filename:

```r
result <- cdisc_compare("data/dm_v1.xpt", "data/dm_v2.xpt")
#> ID variables auto-detected for SDTM DM: STUDYID, USUBJID

result <- cdisc_compare("data/ae_v1.csv", "data/ae_v2.csv")
#> ID variables auto-detected for SDTM AE: STUDYID, USUBJID, AESEQ

# Supported formats: .xpt, .sas7bdat, .csv, .rds
```

### Specify domain explicitly

When auto-detection is uncertain, specify the domain for reliable results:

```r
result <- cdisc_compare(ae_v1, ae_v2, domain = "AE", standard = "SDTM")
#> ID variables auto-detected for SDTM AE: STUDYID, USUBJID, AESEQ

result
#> clinCompare: CDISC Comparison Results
#> ----------------------------------------
#> Domain: AE (SDTM)
#> Base:    1495 rows x 14 cols
#> Compare: 1545 rows x 14 cols
#> Matching: key-based (STUDYID, USUBJID, AESEQ)
#> Differences: 0 attribute, 21 value
#> Unmatched rows: 0 in base only, 50 in compare only
#> Value differences: 21 across 2 of 12 column(s); 21 of 1495 obs (1.4%) differ
#>
#> Variable Summary of Differences:
#>   Variable             Type     N Obs  N Diffs   Max Diff Max % Diff   RMS Diff
#>   ----------------------------------------------------------------------------
#>   AESEV                CHAR      1495       13          .          .          .
#>   AEREL                CHAR      1495        8          .          .          .
#> CDISC: PASS (0 errors, 0 warnings)
#> ----------------------------------------
```

### ADaM datasets work the same way

```r
result <- cdisc_compare(adlb_v1, adlb_v2, domain = "ADLB", standard = "ADaM")
#> ID variables auto-detected for ADaM ADLB: STUDYID, USUBJID, PARAMCD, AVISIT
```

## ID Variable Control

clinCompare auto-detects CDISC key variables for each domain. You have three modes of control:

```r
# 1. Auto-detect (default): keys come from CDISC standards
result <- cdisc_compare(dm_v1, dm_v2, domain = "DM")
#> ID variables auto-detected for SDTM DM: STUDYID, USUBJID

# 2. Append to defaults: "+" adds your variables on top of the standard keys
result <- cdisc_compare(ae_v1, ae_v2, domain = "AE",
                        id_vars = c("+", "AETOXGR"))
#> ID variables auto-detected for SDTM AE: STUDYID, USUBJID, AESEQ, AETOXGR

# 3. Override completely: your vector replaces auto-detection entirely
result <- cdisc_compare(lb_v1, lb_v2, domain = "LB",
                        id_vars = c("USUBJID", "LBSEQ"))
```

## Numeric Tolerance

Floating-point noise in derived endpoints (AVAL, CHG, BASE) can produce hundreds of false positives. Use the `tolerance` parameter to set an acceptable threshold:

```r
# Without tolerance: 117 value differences (most are rounding noise)
result <- cdisc_compare(adlb_v1, adlb_v2, domain = "ADLB")

# With tolerance: only real changes remain
result <- cdisc_compare(adlb_v1, adlb_v2, domain = "ADLB", tolerance = 1e-8)

# Also works with compare_datasets()
result <- compare_datasets(df1, df2, tolerance = 0.001)
```

Tolerance is validated on input. Negative, NaN, Inf, or non-numeric values are rejected with a clear error.

## Compare Specific Variables

Focus on the columns you care about using `vars`:

```r
# Only compare derived columns in a 115-column ADLB
result <- cdisc_compare(adlb_v1, adlb_v2, domain = "ADLB",
                        vars = c("AVAL", "CHG", "BASE", "PCHG"))

# Structural comparison (extra columns, type mismatches) still covers all columns.
# Only value-level differences are filtered to the requested vars.
```

## Filter Rows Before Comparing

Apply a filter to both datasets using `where`:

```r
# Compare only male subjects
result <- cdisc_compare(dm_v1, dm_v2, domain = "DM",
                        where = "SEX == 'M'")

# Compare a single lab parameter with tolerance
result <- cdisc_compare(adlb_v1, adlb_v2, domain = "ADLB",
                        vars = c("AVAL", "BASE", "CHG"),
                        where = "PARAMCD == 'ALT'",
                        tolerance = 1e-8)

# Compare only severe adverse events
result <- cdisc_compare(ae_v1, ae_v2, domain = "AE",
                        where = "AESEV == 'SEVERE'")
```

The `where` expression is validated before execution. Invalid syntax or references to non-existent columns produce clear error messages.

## CDISC Validation

### Validate a single dataset

```r
validation <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
#> === CDISC Validation Results ===
#> ERROR: Missing Required Variable: ETHNIC
#> WARNING: Missing Expected Variable: RFXSTDTC
#> WARNING: Missing Expected Variable: RFXENDTC
#> INFO: Variable present: STUDYID (Required)
#> ...
```

### Compare and validate in one step

`cdisc_compare()` runs validation automatically alongside the comparison:

```r
result <- cdisc_compare(dm_interim, dm_final, domain = "DM", standard = "SDTM")
#> CDISC: FAIL (1 errors, 2 warnings)

# Access the validation details
result$cdisc_validation
```

## Unified Output Data Frame

Extract all differences into a single long-format data frame for programmatic analysis:

```r
result <- cdisc_compare(ae_v1, ae_v2, domain = "AE")
diffs <- get_all_differences(result)

head(diffs)
#>   STUDYID       USUBJID AESEQ Variable   Base     Compare  Diff PctDiff
#> 1 STUDY01  STUDY01-001      3   AESEV    MILD     MODERATE   NA      NA
#> 2 STUDY01  STUDY01-007     12   AESTDTC  2025...  2025...     NA      NA

# Filter, analyze, or export
library(dplyr)
diffs %>% filter(Variable == "AESEV") %>% count(Base, Compare)

write.csv(diffs, "all_diffs.csv", row.names = FALSE)
```

When key-based matching was used, the ID columns (STUDYID, USUBJID, etc.) are prepended automatically. When positional matching was used, a Row column identifies the row index.

## Export Reports

`export_report()` auto-detects format from the file extension:

```r
result <- cdisc_compare(dm_v1, dm_v2, domain = "DM")

# HTML: interactive dashboard with KPI cards and Chart.js visualizations
export_report(result, "dm_comparison.html")

# Text: plain text for audit trails or email
export_report(result, "dm_comparison.txt")

# Excel: multi-tab workbook (Summary, Variable Diffs, Value Diffs, CDISC Validation)
export_report(result, "dm_comparison.xlsx")  # requires openxlsx
```

## Batch Compare an Entire Submission

Point `compare_submission()` at two directories, one for each version of the submission. The function scans both folders, matches files by name (case-insensitive), and runs `cdisc_compare()` on every matched pair. Files that exist in only one directory are reported but skipped. Domain, standard, and key variables are all auto-detected per file, just like a single `cdisc_compare()` call.

```r
# Each argument is a path to a directory containing dataset files
results <- compare_submission(
  base_dir    = "submission_v1/",
  compare_dir = "submission_v2/",
  output_file = "submission_diff.xlsx"   # optional consolidated Excel report
)
#> File format auto-detected: .xpt (8 files)
#> Found 8 matching file pair(s): ae, cm, dm, ds, ex, lb, mh, vs
#> Files only in compare_dir: suppae
#> ID variables auto-detected for SDTM DM: STUDYID, USUBJID
#> ID variables auto-detected for SDTM AE: STUDYID, USUBJID, AESEQ
#> ...
#>
#> Domain   Base Rows  Compare Rows  Attr Diffs  Value Diffs  CDISC Errors  Verdict
#> ------   ---------  ------------  ----------  -----------  ------------  -------
#> DM             500           503           1            5             0  5 value diffs
#> AE            1495          1545           0           21             0  21 value diffs
#> LB           16000         16000           0          117             0  117 value diffs
#> ...

# Drill into any domain
results$DM
results$AE$unmatched_rows$df2_only
```

The file format is auto-detected from the most common extension in `base_dir`. You can also set it explicitly with `format = "xpt"`, `"sas7bdat"`, `"csv"`, or `"rds"`. The optional `tolerance` and `id_vars` parameters are passed to every comparison in the batch.

```r
# Explicit format and tolerance across all domains
results <- compare_submission("v1/", "v2/", format = "csv", tolerance = 1e-8)
```

## Prepare and Clean

```r
# Remove duplicates and standardize text to uppercase
cleaned <- clean_dataset(raw_data, remove_duplicates = TRUE, convert_to_case = "upper")

# Sort and filter both datasets identically before comparing
prepped <- prepare_datasets(df1, df2,
                            sort_columns = "USUBJID",
                            filter_criteria = "AGE > 18")

result <- compare_datasets(prepped$df1, prepped$df2)
```

## Group-Wise Comparison

Compare within subgroups. Useful for multi-site or multi-arm studies:

```r
by_site <- compare_by_group(df1, df2, group_vars = "SITEID")

# Returns a named list with one comparison result per group
by_site$SITE01
by_site$SITE02
```

## Domain Detection

clinCompare auto-detects CDISC domains using three signals:

1. **Column matching**: Each candidate domain is scored by how many of its expected columns appear in the data.
2. **ADaM indicator columns**: Columns like TRT01P, SAFFL, AVAL, PARAMCD, and AVISIT boost ADaM scores when 3 or more are present.
3. **Filename hint**: When loading from a file path, the filename (e.g., `adlb.xpt`) gives a strong boost to the matching domain.

```r
detect_cdisc_domain(adlb_data)
#> $domain
#> [1] "ADLB"
#> $standard
#> [1] "ADaM"
#> $confidence
#> [1] 0.84
```

## CDISC Coverage

clinCompare ships with hand-curated metadata for **51 SDTM domains** (IG 3.4, with 3.3 support) and **14 ADaM datasets** (IG 1.3, with 1.2/1.1 provenance tracking). The metadata lives in `inst/extdata/` as CSV files, making it easy to review, diff, and contribute to.

<details>
<summary>Full domain list (click to expand)</summary>

**SDTM** (domains marked * were introduced in IG 3.4):
AE, AG*, BE*, BS*, CE, CM, CO, CP*, DA, DD, DM, DS, DV, EC, EG, EX, FA, GF*, HO, IE, IS, LB, MB, MH, MI, ML*, MS, PC, PE, PP, PR, QS, RELREC, RS, SC, SE, SM*, SS, SU, SUPPQUAL, SV, TA, TD*, TE, TI, TM*, TR, TS, TU, TV, VS

**ADaM**:
ADAE, ADCM, ADEG, ADEFF, ADEX, ADLB, ADMH, ADPC, ADPP, ADRS, ADSL, ADTR, ADTTE, ADVS

</details>

The canonical machine-readable source is the [CDISC Library API](https://www.cdisc.org/cdisc-library) (requires CDISC membership). For regulatory submissions, always cross-reference with the official CDISC Library.

**Disclaimer:** clinCompare is a quality-assurance and exploratory analysis tool. It is not a substitute for official CDISC compliance validation software (e.g., Pinnacle 21). For regulatory submissions, always cross-reference with your organization's validated tools.

## Exported Functions

| Function | Purpose |
|---|---|
| `compare_datasets()` | Compare any two data frames (dataset, variable, and observation level) |
| `compare_variables()` | Compare column names, types, and structure |
| `compare_observations()` | Row-by-row value comparison on common columns |
| `compare_by_group()` | Compare within subgroups (e.g., by site or treatment arm) |
| `cdisc_compare()` | Compare with CDISC validation, auto-detected keys, and domain detection |
| `compare_submission()` | Batch compare all datasets across two directories |
| `validate_cdisc()` | Validate a single dataset against CDISC SDTM/ADaM standards |
| `detect_cdisc_domain()` | Auto-detect the CDISC domain and standard of a dataset |
| `export_report()` | Export comparison results to HTML, text, or Excel |
| `get_all_differences()` | Extract all differences as a unified long-format data frame |
| `clean_dataset()` | Remove duplicates, standardize case |
| `prepare_datasets()` | Sort and filter two datasets before comparison |

## Requirements

R >= 3.5.0, dplyr >= 1.0.0, rlang >= 0.4.0, tidyr >= 1.0.0, haven >= 2.0.0. Optional: openxlsx >= 4.0.0 (for Excel report export), ggplot2 >= 3.0.0 (for visualizations).

## Contributing

Contributions welcome, especially CDISC metadata corrections. The metadata lives in `inst/extdata/sdtm_metadata.csv` and `inst/extdata/adam_metadata.csv`, so domain experts can review and submit PRs without writing R code. See the [GitHub repository](https://github.com/siddharthlokineni/clinCompare).

## License

MIT License. See [LICENSE](LICENSE.md) for details.

**Author:** Siddharth Lokineni ([GitHub](https://github.com/siddharthlokineni))
