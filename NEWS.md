# clinCompare 1.0.0

* Initial CRAN release.
* Core dataset comparison functions work on any data frames:
  `compare_datasets()`, `compare_variables()`, `compare_observations()`,
  `compare_by_group()`, `check_compatibility()`.
* CDISC validation for 51 SDTM domains (SDTM IG 3.4/3.3) and 14 ADaM
  datasets (ADaM IG 1.3/1.2/1.1), including SUPPQUAL template and RELREC.
* Version-aware metadata: `get_sdtm_metadata(version)` and
  `get_adam_metadata(version)` accept IG version parameters.
* `extract_cdisc_version()` reads TS domain for CDISC version annotation.
* Auto-detection of CDISC domains via `detect_cdisc_domain()`.
* Metadata comparison for variable types, labels, lengths, formats, and column ordering.
* Z-score outlier detection on numeric columns (opt-in via
  `detect_outliers = TRUE` in `cdisc_compare()`).
* ID variable support for key-based row matching via `id_vars` parameter.
* Unified comparison table combining attribute and value differences.
* `cdisc_compare()` for combined comparison and validation workflow,
  with optional `ts_data` parameter for version tracking.
* Text and HTML report generation via `generate_cdisc_report()`, including
  interactive Chart.js dashboard visualizations in HTML output.
* Data preparation utilities: `prepare_datasets()`, `clean_dataset()`,
  `handle_missing_values()`, `convert_data_types()`, `transform_variables()`.
* Tolerance-based numeric comparison via `set_tolerance()`.
