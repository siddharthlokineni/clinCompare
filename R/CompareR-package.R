#' CompareR: Dataset Comparison with CDISC Validation
#'
#' A comprehensive toolkit for comparing clinical trial datasets,
#' inspired by SAS PROC COMPARE. Provides functions for dataset comparison
#' including variable-level and observation-level differences, data type
#' checking, and missing value analysis. Uniquely integrates CDISC validation
#' for SDTM and ADaM datasets.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{compare_datasets}}}{High-level comparison of two datasets}
#'   \item{\code{\link{compare_variables}}}{Compare variable names and types}
#'   \item{\code{\link{compare_observations}}}{Row-wise value comparison}
#'   \item{\code{\link{cdisc_compare}}}{Compare datasets with CDISC validation}
#'   \item{\code{\link{validate_cdisc}}}{Validate a dataset against CDISC standards}
#'   \item{\code{\link{detect_cdisc_domain}}}{Auto-detect CDISC domain or ADaM dataset}
#' }
#'
#' @section CDISC Standards Supported:
#' \describe{
#'   \item{SDTM}{DM, AE, LB, VS, EX, CM, MH, DS, SV, TA, TE domains}
#'   \item{ADaM}{ADSL, ADAE, ADLB, ADTTE, ADEFF datasets}
#' }
#'
#' @docType package
#' @name CompareR-package
#' @aliases CompareR
"_PACKAGE"
