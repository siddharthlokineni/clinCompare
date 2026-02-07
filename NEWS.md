# CompareR 1.0.0

* Initial CRAN release.
* Core dataset comparison functions: `compare_datasets()`, `compare_variables()`,
  `compare_observations()`, `compare_by_group()`, `check_compatibility()`.
* CDISC validation for SDTM domains (DM, AE, LB, VS, EX, CM, MH, DS, SV, TA, TE)
  and ADaM datasets (ADSL, ADAE, ADLB, ADTTE, ADEFF).
* Auto-detection of CDISC domains via `detect_cdisc_domain()`.
* Metadata comparison for variable types, labels, and column ordering.
* `cdisc_compare()` for combined comparison and validation workflow.
* Text and HTML report generation via `generate_cdisc_report()`, including
  interactive Chart.js dashboard visualizations in HTML output.
* Data preparation utilities: `prepare_datasets()`, `clean_dataset()`,
  `handle_missing_values()`, `convert_data_types()`, `transform_variables()`.
* Tolerance-based numeric comparison via `set_tolerance()`.
