# CompareR

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/CompareR)](https://CRAN.R-project.org/package=CompareR)
[![R-CMD-check](https://github.com/siddharthlokineni/CompareR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siddharthlokineni/CompareR/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

CompareR is a comprehensive R package for comparing clinical trial datasets, inspired by **SAS PROC COMPARE**. It goes beyond simple dataset comparison by integrating **CDISC (Clinical Data Interchange Standards Consortium)** validation for both **SDTM** and **ADaM** datasets.

When comparing SDTM or ADaM datasets, CompareR automatically:
- Detects the CDISC domain or analysis dataset type
- Validates variable names, types, and completeness against CDISC standards
- Compares metadata attributes (variable types, labels, column ordering)
- Flags missing required variables, type mismatches, and non-standard variables
- Generates detailed comparison reports in text (.txt) or self-contained HTML with an interactive Chart.js dashboard

## Installation

```r
# Install from CRAN (when available)
install.packages("CompareR")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("siddharthlokineni/CompareR")
```

## Quick Start

### Basic Dataset Comparison

```r
library(CompareR)

# Create sample datasets
df1 <- data.frame(
  USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
  AGE = c(45, 52, 38),
  SEX = c("M", "F", "M"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  USUBJID = c("SUBJ01", "SUBJ02", "SUBJ03"),
  AGE = c(45, 53, 38),
  SEX = c("M", "F", "F"),
  stringsAsFactors = FALSE
)

# High-level comparison
results <- compare_datasets(df1, df2)
print(results)

# Variable-level comparison
var_diff <- compare_variables(df1, df2)

# Observation-level comparison
obs_diff <- compare_observations(df1, df2)
```

### CDISC Validation (The Key Feature)

```r
# Create a sample SDTM DM dataset
dm_dataset <- data.frame(
  STUDYID = rep("STUDY01", 3),
  DOMAIN = rep("DM", 3),
  USUBJID = paste0("SUBJ0", 1:3),
  SUBJID = paste0("0", 1:3),
  RFSTDTC = c("2024-01-15", "2024-01-16", "2024-01-17"),
  RFENDTC = c("2024-06-15", "2024-06-16", "2024-06-17"),
  SITEID = rep("SITE01", 3),
  SEX = c("M", "F", "M"),
  AGE = c(45, 52, 38),
  AGEU = rep("YEARS", 3),
  RACE = c("WHITE", "BLACK", "ASIAN"),
  ARMCD = rep("TRT", 3),
  ARM = rep("Treatment", 3),
  COUNTRY = rep("USA", 3),
  ACTARMCD = rep("TRT", 3),
  ACTARM = rep("Treatment", 3),
  stringsAsFactors = FALSE
)

# Auto-detect the CDISC domain
detection <- detect_cdisc_domain(dm_dataset)
print(detection)
# $standard: "SDTM"
# $domain: "DM"
# $confidence: 0.85

# Validate against CDISC standards
validation <- validate_cdisc(dm_dataset)
print_cdisc_validation(validation)
# Shows: Missing Expected variables (ETHNIC, RFXSTDTC, RFXENDTC)
# Shows: Variable info for all present variables
```

### Compare Datasets with CDISC Compliance

```r
# Compare two versions of a DM dataset with CDISC checks
results <- cdisc_compare(dm_v1, dm_v2, domain = "DM", standard = "SDTM")

# Generate an HTML report with interactive dashboard
# Includes KPI cards (match rate, differences, metadata issues)
# and 4 Chart.js visualizations (doughnut, bar, metadata, stacked)
generate_cdisc_report(results, output_format = "html",
                      file_name = "dm_comparison.html")

# Or a PROC COMPARE-style text report (saves as .txt)
generate_cdisc_report(results, output_format = "text",
                      file_name = "dm_comparison.txt")
```

### ADaM Dataset Validation

```r
# Validate an ADSL dataset
adsl_validation <- validate_cdisc(adsl_data, domain = "ADSL", standard = "ADaM")
print_cdisc_validation(adsl_validation)
```

## CDISC Standards Supported

### SDTM Domains (Study Data Tabulation Model)

| Domain | Description |
|--------|-------------|
| DM | Demographics |
| AE | Adverse Events |
| LB | Laboratory Test Results |
| VS | Vital Signs |
| EX | Exposure |
| CM | Concomitant Medications |
| MH | Medical History |
| DS | Disposition |
| SV | Subject Visits |
| TA | Trial Arms |
| TE | Trial Elements |

### ADaM Datasets (Analysis Data Model)

| Dataset | Description |
|---------|-------------|
| ADSL | Subject-Level Analysis |
| ADAE | Adverse Events Analysis |
| ADLB | Laboratory Analysis |
| ADTTE | Time-to-Event Analysis |
| ADEFF | Efficacy Analysis |

## All Functions

### Core Comparison Functions

- **`compare_datasets(df1, df2)`** - High-level comparison of two datasets returning dimensions, variable names, data types, and missing value analysis
- **`compare_variables(df1, df2)`** - Compare variable names, data types, and structure between datasets
- **`compare_observations(df1, df2)`** - Row-wise value comparison to identify differences in data
- **`compare_by_group(df1, df2, group_vars)`** - Compare datasets separately by grouping variables
- **`check_compatibility(df1, df2)`** - Check if datasets are compatible for comparison

### CDISC Validation Functions

- **`cdisc_compare(df1, df2, domain, standard)`** - Compare datasets with CDISC validation, metadata comparison (types, labels, column ordering), and standards checking
- **`validate_cdisc(df, domain, standard)`** - Validate a single dataset against CDISC standards
- **`validate_sdtm(df, domain)`** - Validate SDTM domain (DM, AE, LB, VS, EX, CM, MH, DS, SV, TA, TE)
- **`validate_adam(df, dataset_name)`** - Validate ADaM dataset (ADSL, ADAE, ADLB, ADTTE, ADEFF)
- **`detect_cdisc_domain(df)`** - Auto-detect CDISC domain or ADaM dataset type with confidence scoring
- **`print_cdisc_validation(validation_result)`** - Pretty-print CDISC validation results

### Data Preparation Functions

- **`prepare_datasets(df1, df2, sort_by, filter_condition)`** - Sort and filter datasets before comparison
- **`clean_dataset(df)`** - Remove duplicates, standardize column name case, and clean data
- **`handle_missing_values(df, method)`** - Handle missing values using various strategies (exclude, replace, mean, median, flag)
- **`convert_data_types(df, type_mapping)`** - Convert variable types to match specifications
- **`transform_variables(df, transformations)`** - Apply mathematical or logical transformations to variables

### Settings and Configuration

- **`initialize_comparison_settings()`** - Initialize default comparison settings and parameters
- **`set_tolerance(tolerance_value)`** - Set numeric comparison tolerance for floating-point differences
- **`reset_comparison_settings()`** - Reset comparison settings to defaults

### Reporting and Visualization Functions

- **`generate_cdisc_report(results, output_format, file_name)`** - Generate CDISC comparison reports: text format produces a PROC COMPARE-style report (console + optional .txt file); HTML format produces a self-contained report with styling, KPI score cards, and an interactive Chart.js dashboard with 4 visualizations (match rate doughnut, discrepancies bar, metadata breakdown, CDISC validation stacked bar)
- **`generate_summary_report(results)`** - Generate a high-level summary of dataset differences
- **`generate_detailed_report(results, include_observations)`** - Generate detailed comparison reports with optional observation-level details
- **`report_differences(df1, df2)`** - Create a formatted report of all differences found
- **`generate_comparison_visualization(results)`** - Create ggplot2 bar chart visualization of discrepancies per variable

## SAS PROC COMPARE Equivalence

| SAS PROC COMPARE | CompareR Equivalent |
|------------------|-------------------|
| `PROC COMPARE BASE= COMPARE=` | `compare_datasets(df1, df2)` |
| `PROC COMPARE ... METHOD=ABSOLUTE CRITERION=0.001` | `set_tolerance(0.001)` then `compare_observations()` |
| Variable-level differences | `compare_variables(df1, df2)` |
| Observation-level differences | `compare_observations(df1, df2)` |
| BY-group processing | `compare_by_group(df1, df2, group_vars)` |
| *No SAS equivalent* | `cdisc_compare()` with built-in CDISC validation |
| `PROC COMPARE ... LISTALL` | `generate_detailed_report()` |
| `PROC COMPARE ... PRINTALL` | `generate_comparison_visualization()` |

## Use Cases

### Regulatory Submission QA

CompareR is invaluable for quality assurance in regulatory submissions:

```r
# Validate interim vs. final versions of SDTM data
interim_dm <- read.csv("dm_interim.csv")
final_dm <- read.csv("dm_final.csv")

# Check CDISC compliance
interim_check <- validate_cdisc(interim_dm, domain = "DM", standard = "SDTM")
final_check <- validate_cdisc(final_dm, domain = "DM", standard = "SDTM")

# Compare versions
comparison <- cdisc_compare(interim_dm, final_dm, domain = "DM", standard = "SDTM")
generate_cdisc_report(comparison, output_format = "html")
```

### Data Integration and Validation

```r
# Validate data from multiple sources before integration
source1_data <- read.csv("source1.csv")
source2_data <- read.csv("source2.csv")

# Check compatibility
is_compatible <- check_compatibility(source1_data, source2_data)

# Find discrepancies
vars_diff <- compare_variables(source1_data, source2_data)
obs_diff <- compare_observations(source1_data, source2_data)
```

### Data Cleaning and Standardization

```r
# Prepare data by removing duplicates and standardizing
cleaned_data <- clean_dataset(raw_data)

# Handle missing values appropriately
prepared_data <- handle_missing_values(cleaned_data, method = "exclude")

# Compare original vs. cleaned
comparison <- compare_datasets(raw_data, prepared_data)
```

## Features

- **SAS PROC COMPARE Compatibility**: Functions designed to mirror SAS PROC COMPARE behavior for easy transition
- **CDISC Standards Integration**: Automatic validation against SDTM and ADaM specifications
- **Multiple Comparison Levels**: Compare at dataset, variable, and observation levels
- **Metadata Comparison**: Compare variable types, labels (attr-based), and column ordering between datasets
- **Interactive HTML Dashboard**: Self-contained HTML reports with Chart.js KPI score cards and 4-chart visualization dashboard (match rate doughnut, discrepancies by variable, metadata breakdown, CDISC validation profile)
- **PROC COMPARE-Style Text Reports**: Text output in a familiar SAS-style layout, saved as .txt files for easy sharing
- **Tolerance-Based Comparisons**: Handle floating-point precision issues with configurable tolerance
- **Group-Wise Comparison**: Compare within subgroups defined by specified variables
- **Missing Value Analysis**: Comprehensive missing data detection and handling
- **Data Type Checking**: Identify and report data type mismatches
- **Auto-Detection**: Automatically identify CDISC domains and ADaM datasets

## Requirements

- R >= 3.5.0
- dplyr >= 1.0.0
- rlang >= 0.4.0
- tidyr >= 1.0.0

### Optional

- ggplot2 >= 3.0.0 (for standalone visualizations)
- knitr and rmarkdown (for vignette building)
- Chart.js 4.4.1+ (loaded via CDN in HTML reports, no local installation needed)

## Documentation

Comprehensive documentation is available through:

```r
# View package documentation
?CompareR

# View function help
?compare_datasets
?validate_cdisc
?detect_cdisc_domain

# View vignettes
vignette("Using CompareR")
```

## Examples

### Example 1: Basic Comparison

```r
library(CompareR)

# Create two sample datasets
df1 <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 35, 40, 45),
  Score = c(85.5, 90.0, 78.5, 92.0, 88.5)
)

df2 <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charles", "David", "Eve"),
  Age = c(25, 30, 36, 40, 45),
  Score = c(85.5, 90.0, 78.5, 92.0, 88.5)
)

# Compare datasets
results <- compare_datasets(df1, df2)
print(results)

# Get detailed observation differences
obs_diff <- compare_observations(df1, df2)
print(obs_diff)
```

### Example 2: CDISC Validation

```r
# Load a dataset
my_data <- read.csv("my_study_data.csv")

# Detect if it's SDTM or ADaM
detection <- detect_cdisc_domain(my_data)
print(paste("Detected as:", detection$standard, detection$domain))

# Validate against standards
validation <- validate_cdisc(my_data)
print_cdisc_validation(validation)

# Generate compliance report
generate_cdisc_report(validation, output_format = "html",
                      file_name = "validation_report.html")
```

### Example 3: Tolerance-Based Comparison

```r
# Create datasets with slight numeric differences
df1 <- data.frame(x = c(1.0001, 2.0001, 3.0001))
df2 <- data.frame(x = c(1.0000, 2.0000, 3.0000))

# Set tolerance
set_tolerance(0.001)

# Compare with tolerance
results <- compare_observations(df1, df2)
print(results)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request to the [GitHub repository](https://github.com/siddharthlokineni/CompareR).

When contributing, please:
- Follow the existing code style
- Write tests for new functionality
- Update documentation and examples
- Ensure R CMD check passes without errors or warnings

## Citation

If you use CompareR in your work, please cite it as:

```r
citation("CompareR")
```

## License

MIT License. See [LICENSE](LICENSE.md) for details.

## Author

Developed by Siddharth Lokineni

- Email: sidhu871@gmail.com
- GitHub: [@siddharthlokineni](https://github.com/siddharthlokineni)

## Related Packages

- **[haven](https://haven.tidyverse.org/)** - Read SAS, SPSS, and Stata files
- **[tidyverse](https://www.tidyverse.org/)** - Data manipulation and visualization
- **[janitor](https://sfirke.github.io/janitor/)** - Data cleaning utilities
- **[assertthat](https://github.com/hadley/assertthat)** - Data validation

## References

- [CDISC SDTM Standards](https://www.cdisc.org/standards/foundational/sdtm)
- [CDISC ADaM Standards](https://www.cdisc.org/standards/foundational/adam)
- [SAS PROC COMPARE Documentation](https://documentation.sas.com/doc/en/pgmsascdc/v_025/proc/p0q8b9h25yywjdn15pqp1y1vkp67.htm)

## Support

For issues, feature requests, or bug reports, please visit the [GitHub Issues page](https://github.com/siddharthlokineni/CompareR/issues).

---

**Last Updated**: 2026
**Package Version**: 1.0.0
