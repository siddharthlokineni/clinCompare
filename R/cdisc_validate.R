#' Detect CDISC Domain Type
#'
#' @description
#' Detects whether a data frame looks like an SDTM domain or ADaM dataset by comparing
#' column names against known CDISC standards. Calculates a confidence score based on
#' the percentage of expected variables present.
#'
#' @param df A data frame to analyze.
#'
#' @return
#' A list containing:
#' \item{standard}{Character: "SDTM", "ADaM", or "Unknown"}
#' \item{domain}{Character: domain code (e.g., "DM", "AE") or dataset name (e.g., "ADSL"), or NA}
#' \item{confidence}{Numeric between 0 and 1 indicating match quality}
#' \item{message}{Character: human-readable explanation}
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a sample SDTM DM domain
#' dm <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = "SUBJ001",
#'   SUBJID = "001",
#'   DMSEQ = 1,
#'   RACE = "WHITE",
#'   ETHNIC = "NOT HISPANIC OR LATINO",
#'   ARMCD = "ARM01",
#'   ARM = "Treatment A",
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- detect_cdisc_domain(dm)
#' print(result)
#' }
detect_cdisc_domain <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame", call. = FALSE)
  }

  if (nrow(df) == 0) {
    return(list(
      standard = "Unknown",
      domain = NA,
      confidence = 0,
      message = "Cannot detect domain from an empty data frame"
    ))
  }

  df_cols <- tolower(colnames(df))
  sdtm_meta <- get_sdtm_metadata()
  adam_meta <- get_adam_metadata()

  results <- list()

  # Check SDTM domains
  for (domain in names(sdtm_meta)) {
    meta_vars <- sdtm_meta[[domain]]
    meta_vars_lower <- tolower(meta_vars$variable)
    required_vars <- tolower(meta_vars$variable[
      toupper(meta_vars$core) == "REQ"
    ])

    if (length(required_vars) > 0) {
      matches <- sum(required_vars %in% df_cols)
      confidence <- matches / length(required_vars)
    } else {
      confidence <- 0
    }

    results[[paste0("SDTM_", domain)]] <- list(
      standard = "SDTM",
      domain = domain,
      confidence = confidence
    )
  }

  # Check ADaM datasets
  for (dataset in names(adam_meta)) {
    meta_vars <- adam_meta[[dataset]]
    meta_vars_lower <- tolower(meta_vars$variable)
    required_vars <- tolower(meta_vars$variable[
      toupper(meta_vars$core) == "REQ"
    ])

    if (length(required_vars) > 0) {
      matches <- sum(required_vars %in% df_cols)
      confidence <- matches / length(required_vars)
    } else {
      confidence <- 0
    }

    results[[paste0("ADAM_", dataset)]] <- list(
      standard = "ADaM",
      domain = dataset,
      confidence = confidence
    )
  }

  # Find best match
  best_match <- NULL
  best_confidence <- 0.5  # threshold: >= 50% required variables

  for (name in names(results)) {
    if (results[[name]]$confidence > best_confidence) {
      best_match <- results[[name]]
      best_confidence <- results[[name]]$confidence
    }
  }

  if (!is.null(best_match)) {
    message <- sprintf(
      "%s domain '%s' detected with %.1f%% confidence (%.0f%% of required variables present)",
      best_match$standard,
      best_match$domain,
      best_match$confidence * 100,
      best_match$confidence * 100
    )
    return(list(
      standard = best_match$standard,
      domain = best_match$domain,
      confidence = best_match$confidence,
      message = message
    ))
  } else {
    return(list(
      standard = "Unknown",
      domain = NA,
      confidence = 0,
      message = "Could not confidently match data frame to any known CDISC domain or dataset"
    ))
  }
}


#' Validate CDISC Compliance
#'
#' @description
#' Main validation entry point that checks whether a data frame conforms to CDISC standards.
#' If domain and standard are not provided, they are automatically detected via
#' [detect_cdisc_domain()]. Dispatches to [validate_sdtm()] or [validate_adam()] as appropriate.
#'
#' @param df A data frame to validate.
#' @param domain Optional character string specifying the CDISC domain code
#'   (e.g., "DM", "AE") or ADaM dataset name (e.g., "ADSL", "ADAE"). If NULL, auto-detected.
#' @param standard Optional character string: "SDTM" or "ADaM". If NULL, auto-detected.
#'
#' @return
#' A data frame with columns:
#' \item{category}{Character: type of validation issue ("Missing Required Variable",
#'   "Missing Expected Variable", "Type Mismatch", "Non-Standard Variable", "Variable Info")}
#' \item{variable}{Character: variable name}
#' \item{message}{Character: description of the issue}
#' \item{severity}{Character: "ERROR", "WARNING", or "INFO"}
#'
#' @export
#' @examples
#' \dontrun{
#' # Auto-detect domain
#' dm <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = "SUBJ001",
#'   DMSEQ = 1,
#'   RACE = "WHITE",
#'   stringsAsFactors = FALSE
#' )
#' results <- validate_cdisc(dm)
#' print(results)
#'
#' # Validate with explicit domain specification
#' results <- validate_cdisc(dm, domain = "DM", standard = "SDTM")
#' }
validate_cdisc <- function(df, domain = NULL, standard = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame", call. = FALSE)
  }

  # Auto-detect if not provided
  if (is.null(domain) || is.null(standard)) {
    detection <- detect_cdisc_domain(df)

    if (detection$standard == "Unknown") {
      warning("Could not automatically detect CDISC standard. Returning empty validation.",
        call. = FALSE
      )
      return(data.frame(
        category = character(0),
        variable = character(0),
        message = character(0),
        severity = character(0),
        stringsAsFactors = FALSE
      ))
    }

    if (is.null(standard)) {
      standard <- detection$standard
    }
    if (is.null(domain)) {
      domain <- detection$domain
    }
  }

  # Validate inputs
  if (!is.character(standard) || !(standard %in% c("SDTM", "ADaM"))) {
    stop("standard must be either 'SDTM' or 'ADaM'", call. = FALSE)
  }

  if (!is.character(domain)) {
    stop("domain must be a character string", call. = FALSE)
  }

  # Dispatch to appropriate validator
  if (standard == "SDTM") {
    return(validate_sdtm(df, domain))
  } else {
    return(validate_adam(df, domain))
  }
}


#' Validate SDTM Compliance
#'
#' @description
#' Validates a data frame against a specific SDTM domain specification. Checks for
#' missing required/expected variables, data type mismatches, and non-standard variables.
#'
#' @param df A data frame to validate.
#' @param domain Character string specifying the SDTM domain code (e.g., "DM", "AE", "VS").
#'
#' @return
#' A data frame with validation results containing columns:
#' \item{category}{Character: validation issue type}
#' \item{variable}{Character: variable name}
#' \item{message}{Character: issue description}
#' \item{severity}{Character: "ERROR", "WARNING", or "INFO"}
#'
#' @details
#' Severity levels:
#' - ERROR: Required variable is missing
#' - WARNING: Expected variable is missing or data type mismatch detected
#' - INFO: Non-standard variable present or variable information
#'
#' @export
#' @examples
#' \dontrun{
#' # Validate a sample SDTM DM domain
#' dm <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = "SUBJ001",
#'   DMSEQ = 1,
#'   RACE = "WHITE",
#'   stringsAsFactors = FALSE
#' )
#' results <- validate_sdtm(dm, domain = "DM")
#' print(results)
#' }
validate_sdtm <- function(df, domain) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame", call. = FALSE)
  }

  if (!is.character(domain) || length(domain) != 1) {
    stop("domain must be a single character string", call. = FALSE)
  }

  sdtm_meta <- get_sdtm_metadata()
  domain <- toupper(domain)

  if (!domain %in% names(sdtm_meta)) {
    stop(sprintf("Domain '%s' not found in SDTM metadata", domain), call. = FALSE)
  }

  # meta_vars IS the data.frame directly, not a nested object
  meta_vars <- sdtm_meta[[domain]]
  df_cols <- colnames(df)
  df_cols_lower <- tolower(df_cols)

  results <- list()

  # Check for missing required variables
  required_idx <- toupper(meta_vars$core) == "REQ"
  if (any(required_idx)) {
    required_vars <- meta_vars[required_idx, , drop = FALSE]
    for (i in seq_len(nrow(required_vars))) {
      var_name <- required_vars$variable[i]
      var_lower <- tolower(var_name)

      if (!var_lower %in% df_cols_lower) {
        results[[paste0("missing_req_", var_name)]] <- data.frame(
          category = "Missing Required Variable",
          variable = var_name,
          message = sprintf(
            "Required variable '%s' is missing from %s domain",
            var_name, domain
          ),
          severity = "ERROR",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Check for missing expected variables
  expected_idx <- toupper(meta_vars$core) == "EXP"
  if (any(expected_idx)) {
    expected_vars <- meta_vars[expected_idx, , drop = FALSE]
    for (i in seq_len(nrow(expected_vars))) {
      var_name <- expected_vars$variable[i]
      var_lower <- tolower(var_name)

      if (!var_lower %in% df_cols_lower) {
        results[[paste0("missing_exp_", var_name)]] <- data.frame(
          category = "Missing Expected Variable",
          variable = var_name,
          message = sprintf(
            "Expected variable '%s' is not present in %s domain",
            var_name, domain
          ),
          severity = "WARNING",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Check data types of present variables
  for (i in seq_len(nrow(meta_vars))) {
    var_name <- meta_vars$variable[i]
    var_lower <- tolower(var_name)

    # Find matching column in df
    col_idx <- which(tolower(df_cols) == var_lower)

    if (length(col_idx) > 0) {
      actual_col <- df_cols[col_idx[1]]
      expected_type <- meta_vars$type[i]
      actual_col_data <- df[[actual_col]]

      # Check type compatibility
      is_numeric <- is.numeric(actual_col_data) && !is.logical(actual_col_data)
      is_character <- is.character(actual_col_data)

      type_match <- FALSE
      if (toupper(expected_type) == "NUM" && is_numeric) {
        type_match <- TRUE
      } else if (toupper(expected_type) == "CHAR" && is_character) {
        type_match <- TRUE
      }

      if (!type_match) {
        actual_type <- if (is_numeric) "Num" else if (is_character) "Char" else "Other"
        results[[paste0("type_mismatch_", var_name)]] <- data.frame(
          category = "Type Mismatch",
          variable = actual_col,
          message = sprintf(
            "Variable '%s' has type '%s' but SDTM expects '%s'",
            var_name, actual_type, expected_type
          ),
          severity = "WARNING",
          stringsAsFactors = FALSE
        )
      }

      # Add variable info for present variables
      results[[paste0("var_info_", var_name)]] <- data.frame(
        category = "Variable Info",
        variable = actual_col,
        message = sprintf("SDTM variable '%s': %s", var_name, meta_vars$label[i]),
        severity = "INFO",
        stringsAsFactors = FALSE
      )
    }
  }

  # Check for non-standard variables (in df but not in metadata)
  meta_vars_lower <- tolower(meta_vars$variable)
  for (col in df_cols) {
    col_lower <- tolower(col)
    if (!col_lower %in% meta_vars_lower) {
      results[[paste0("nonstand_", col)]] <- data.frame(
        category = "Non-Standard Variable",
        variable = col,
        message = sprintf(
          "Variable '%s' is not part of the %s SDTM domain specification",
          col, domain
        ),
        severity = "INFO",
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine results
  if (length(results) == 0) {
    return(data.frame(
      category = character(0),
      variable = character(0),
      message = character(0),
      severity = character(0),
      stringsAsFactors = FALSE
    ))
  }

  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
}


#' Validate ADaM Compliance
#'
#' @description
#' Validates a data frame against a specific ADaM dataset specification. Similar to
#' [validate_sdtm()] but uses ADaM metadata and treats Conditional variables differently.
#'
#' @param df A data frame to validate.
#' @param domain Character string specifying the ADaM dataset name (e.g., "ADSL", "ADAE").
#'
#' @return
#' A data frame with validation results containing columns:
#' \item{category}{Character: validation issue type}
#' \item{variable}{Character: variable name}
#' \item{message}{Character: issue description}
#' \item{severity}{Character: "ERROR", "WARNING", or "INFO"}
#'
#' @details
#' Severity levels:
#' - ERROR: Required variable is missing
#' - WARNING: Data type mismatch detected
#' - INFO: Conditional variable missing, non-standard variable, or variable information
#'
#' @export
#' @examples
#' \dontrun{
#' # Validate a sample ADaM ADSL dataset
#' adsl <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = "SUBJ001",
#'   SITEID = "SITE01",
#'   ARM = "Treatment A",
#'   stringsAsFactors = FALSE
#' )
#' results <- validate_adam(adsl, domain = "ADSL")
#' print(results)
#' }
validate_adam <- function(df, domain) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame", call. = FALSE)
  }

  if (!is.character(domain) || length(domain) != 1) {
    stop("domain must be a single character string", call. = FALSE)
  }

  adam_meta <- get_adam_metadata()
  domain <- toupper(domain)

  if (!domain %in% names(adam_meta)) {
    stop(sprintf("Dataset '%s' not found in ADaM metadata", domain), call. = FALSE)
  }

  # meta_vars IS the data.frame directly, not a nested object
  meta_vars <- adam_meta[[domain]]
  df_cols <- colnames(df)
  df_cols_lower <- tolower(df_cols)

  results <- list()

  # Check for missing required variables
  required_idx <- toupper(meta_vars$core) == "REQ"
  if (any(required_idx)) {
    required_vars <- meta_vars[required_idx, , drop = FALSE]
    for (i in seq_len(nrow(required_vars))) {
      var_name <- required_vars$variable[i]
      var_lower <- tolower(var_name)

      if (!var_lower %in% df_cols_lower) {
        results[[paste0("missing_req_", var_name)]] <- data.frame(
          category = "Missing Required Variable",
          variable = var_name,
          message = sprintf(
            "Required variable '%s' is missing from %s dataset",
            var_name, domain
          ),
          severity = "ERROR",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Check for missing conditional variables (with INFO severity)
  cond_idx <- toupper(meta_vars$core) == "COND"
  if (any(cond_idx)) {
    cond_vars <- meta_vars[cond_idx, , drop = FALSE]
    for (i in seq_len(nrow(cond_vars))) {
      var_name <- cond_vars$variable[i]
      var_lower <- tolower(var_name)

      if (!var_lower %in% df_cols_lower) {
        results[[paste0("missing_cond_", var_name)]] <- data.frame(
          category = "Missing Conditional Variable",
          variable = var_name,
          message = sprintf(
            "Conditional variable '%s' is not present in %s dataset",
            var_name, domain
          ),
          severity = "INFO",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Check data types of present variables
  for (i in seq_len(nrow(meta_vars))) {
    var_name <- meta_vars$variable[i]
    var_lower <- tolower(var_name)

    # Find matching column in df
    col_idx <- which(tolower(df_cols) == var_lower)

    if (length(col_idx) > 0) {
      actual_col <- df_cols[col_idx[1]]
      expected_type <- meta_vars$type[i]
      actual_col_data <- df[[actual_col]]

      # Check type compatibility
      is_numeric <- is.numeric(actual_col_data) && !is.logical(actual_col_data)
      is_character <- is.character(actual_col_data)

      type_match <- FALSE
      if (toupper(expected_type) == "NUM" && is_numeric) {
        type_match <- TRUE
      } else if (toupper(expected_type) == "CHAR" && is_character) {
        type_match <- TRUE
      }

      if (!type_match) {
        actual_type <- if (is_numeric) "Num" else if (is_character) "Char" else "Other"
        results[[paste0("type_mismatch_", var_name)]] <- data.frame(
          category = "Type Mismatch",
          variable = actual_col,
          message = sprintf(
            "Variable '%s' has type '%s' but ADaM expects '%s'",
            var_name, actual_type, expected_type
          ),
          severity = "WARNING",
          stringsAsFactors = FALSE
        )
      }

      # Add variable info for present variables
      results[[paste0("var_info_", var_name)]] <- data.frame(
        category = "Variable Info",
        variable = actual_col,
        message = sprintf("ADaM variable '%s': %s", var_name, meta_vars$label[i]),
        severity = "INFO",
        stringsAsFactors = FALSE
      )
    }
  }

  # Check for non-standard variables (in df but not in metadata)
  meta_vars_lower <- tolower(meta_vars$variable)
  for (col in df_cols) {
    col_lower <- tolower(col)
    if (!col_lower %in% meta_vars_lower) {
      results[[paste0("nonstand_", col)]] <- data.frame(
        category = "Non-Standard Variable",
        variable = col,
        message = sprintf(
          "Variable '%s' is not part of the %s ADaM dataset specification",
          col, domain
        ),
        severity = "INFO",
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine results
  if (length(results) == 0) {
    return(data.frame(
      category = character(0),
      variable = character(0),
      message = character(0),
      severity = character(0),
      stringsAsFactors = FALSE
    ))
  }

  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
}


#' Compare Two Datasets with CDISC Validation
#'
#' @description
#' Flagship function that compares two datasets AND runs CDISC validation on both.
#' Combines dataset comparison with CDISC conformance analysis to provide comprehensive
#' insights into both differences and regulatory compliance.
#'
#' @param df1 First data frame to compare.
#' @param df2 Second data frame to compare.
#' @param domain Optional character string specifying the CDISC domain code or dataset name.
#'   If NULL, auto-detected from df1.
#' @param standard Optional character string: "SDTM" or "ADaM". If NULL, auto-detected from df1.
#'
#' @return
#' A list containing:
#' \item{comparison}{Result of [compare_datasets()] function}
#' \item{variable_comparison}{Result of [compare_variables()] function}
#' \item{observation_comparison}{Result of [compare_observations()] if dimensions match,
#'   otherwise NULL with explanatory message}
#' \item{cdisc_validation_df1}{CDISC validation results for df1}
#' \item{cdisc_validation_df2}{CDISC validation results for df2}
#' \item{cdisc_conformance_comparison}{Data frame showing which CDISC issues are unique
#'   to df1, unique to df2, or common to both}
#'
#' @export
#' @examples
#' \dontrun{
#' # Create sample SDTM DM domains
#' dm1 <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = c("SUBJ001", "SUBJ002"),
#'   DMSEQ = c(1, 1),
#'   RACE = c("WHITE", "BLACK OR AFRICAN AMERICAN"),
#'   stringsAsFactors = FALSE
#' )
#'
#' dm2 <- data.frame(
#'   STUDYID = "STUDY001",
#'   USUBJID = c("SUBJ001", "SUBJ003"),
#'   DMSEQ = c(1, 1),
#'   RACE = c("WHITE", "ASIAN"),
#'   ETHNIC = c("NOT HISPANIC", "NOT HISPANIC"),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- cdisc_compare(dm1, dm2, domain = "DM", standard = "SDTM")
#' names(result)
#' }
cdisc_compare <- function(df1, df2, domain = NULL, standard = NULL) {
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("Both inputs must be data frames", call. = FALSE)
  }

  # Auto-detect from df1 if not provided
  if (is.null(domain) || is.null(standard)) {
    detection <- detect_cdisc_domain(df1)

    if (is.null(standard)) {
      standard <- if (detection$standard == "Unknown") NA else detection$standard
    }
    if (is.null(domain)) {
      domain <- if (is.na(detection$domain)) NA else detection$domain
    }
  }

  # Run dataset comparison
  comparison <- compare_datasets(df1, df2)
  variable_comparison <- compare_variables(df1, df2)

  # Run observation comparison if dimensions match and common columns exist
  observation_comparison <- NULL
  # Identify common columns (case-insensitive match)
  df1_cols_lower <- tolower(colnames(df1))
  df2_cols_lower <- tolower(colnames(df2))
  common_cols <- df1_cols_lower[df1_cols_lower %in% df2_cols_lower]

  if (nrow(df1) != nrow(df2)) {
    observation_comparison <- list(
      status = "Skipped",
      message = sprintf(
        "Observation comparison skipped: row counts differ (df1: %d, df2: %d)",
        nrow(df1), nrow(df2)
      )
    )
  } else if (length(common_cols) == 0) {
    observation_comparison <- list(
      status = "Skipped",
      message = "Observation comparison skipped: no common columns found"
    )
  } else {
    # Get actual column names from df1 and df2 for common columns
    df1_common_idx <- which(df1_cols_lower %in% common_cols)
    df2_common_idx <- which(df2_cols_lower %in% common_cols)

    df1_common <- df1[, df1_common_idx, drop = FALSE]
    df2_common <- df2[, df2_common_idx, drop = FALSE]

    # Use try-catch for compare_observations in case of errors
    observation_comparison <- tryCatch({
      compare_observations(df1_common, df2_common)
    }, error = function(e) {
      list(
        status = "Error",
        message = sprintf("Observation comparison failed: %s", conditionMessage(e))
      )
    })
  }

  # Build metadata comparison for common columns
  metadata_comparison <- build_metadata_comparison(df1, df2)

  # Run CDISC validation if domain and standard are available
  if (!is.na(domain) && !is.na(standard)) {
    val_df1 <- validate_cdisc(df1, domain = domain, standard = standard)
    val_df2 <- validate_cdisc(df2, domain = domain, standard = standard)

    # Create conformance comparison
    conform_comparison <- create_conformance_comparison(val_df1, val_df2)
  } else {
    val_df1 <- data.frame(
      category = character(0),
      variable = character(0),
      message = character(0),
      severity = character(0),
      stringsAsFactors = FALSE
    )
    val_df2 <- data.frame(
      category = character(0),
      variable = character(0),
      message = character(0),
      severity = character(0),
      stringsAsFactors = FALSE
    )
    conform_comparison <- data.frame(
      category = character(0),
      variable = character(0),
      df1_only = logical(0),
      df2_only = logical(0),
      both = logical(0),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    domain = domain,
    standard = standard,
    nrow_df1 = nrow(df1),
    ncol_df1 = ncol(df1),
    nrow_df2 = nrow(df2),
    ncol_df2 = ncol(df2),
    comparison = comparison,
    variable_comparison = variable_comparison,
    metadata_comparison = metadata_comparison,
    observation_comparison = observation_comparison,
    cdisc_validation_df1 = val_df1,
    cdisc_validation_df2 = val_df2,
    cdisc_conformance_comparison = conform_comparison
  ))
}


#' Create CDISC Conformance Comparison
#'
#' @description
#' Internal function to compare CDISC validation results from two datasets
#' and identify which issues are unique to each or common to both.
#'
#' @param val_df1 Validation result data frame from df1.
#' @param val_df2 Validation result data frame from df2.
#'
#' @return
#' A data frame showing CDISC issue distribution across datasets, with columns:
#' \item{category}{Character: validation issue category}
#' \item{variable}{Character: variable name}
#' \item{df1_only}{Logical: TRUE if issue only appears in df1}
#' \item{df2_only}{Logical: TRUE if issue only appears in df2}
#' \item{both}{Logical: TRUE if issue appears in both datasets}
#'
#' @keywords internal
create_conformance_comparison <- function(val_df1, val_df2) {
  if (nrow(val_df1) == 0 && nrow(val_df2) == 0) {
    return(data.frame(
      category = character(0),
      variable = character(0),
      df1_only = logical(0),
      df2_only = logical(0),
      both = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  # Create combined key for comparison
  val_df1$key <- paste0(val_df1$category, "|", val_df1$variable, "|", val_df1$severity)
  val_df2$key <- paste0(val_df2$category, "|", val_df2$variable, "|", val_df2$severity)

  all_keys <- union(val_df1$key, val_df2$key)

  comparison_list <- list()
  for (key in all_keys) {
    in_df1 <- key %in% val_df1$key
    in_df2 <- key %in% val_df2$key

    # Get the row info from whichever dataframe has it
    if (in_df1) {
      idx <- which(val_df1$key == key)[1]
      category <- val_df1$category[idx]
      variable <- val_df1$variable[idx]
    } else {
      idx <- which(val_df2$key == key)[1]
      category <- val_df2$category[idx]
      variable <- val_df2$variable[idx]
    }

    comparison_list[[key]] <- data.frame(
      category = category,
      variable = variable,
      df1_only = in_df1 && !in_df2,
      df2_only = !in_df1 && in_df2,
      both = in_df1 && in_df2,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, comparison_list)
  rownames(result) <- NULL
  return(result)
}


#' Build Metadata Comparison
#'
#' @description
#' Internal function to compare metadata attributes (types, labels, column
#' order) between two datasets. Modeled after SAS PROC COMPARE metadata
#' sections.
#'
#' @param df1 First data frame (base).
#' @param df2 Second data frame (compare).
#'
#' @return
#' A list with:
#' \item{type_mismatches}{Data frame of variables with differing R classes}
#' \item{label_mismatches}{Data frame of variables with differing labels}
#' \item{order_match}{Logical: TRUE if common column ordering matches}
#' \item{order_df1}{Character: column order in df1 for common columns}
#' \item{order_df2}{Character: column order in df2 for common columns}
#'
#' @keywords internal
build_metadata_comparison <- function(df1, df2) {
  common_cols <- intersect(names(df1), names(df2))

  # --- Type comparison ---
  type_rows <- list()
  for (col in common_cols) {
    t1 <- paste(class(df1[[col]]), collapse = "/")
    t2 <- paste(class(df2[[col]]), collapse = "/")
    if (t1 != t2) {
      type_rows[[col]] <- data.frame(
        variable = col,
        type_base = t1,
        type_compare = t2,
        stringsAsFactors = FALSE
      )
    }
  }
  type_mismatches <- if (length(type_rows) > 0) {
    do.call(rbind, type_rows)
  } else {
    data.frame(
      variable = character(0),
      type_base = character(0),
      type_compare = character(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(type_mismatches) <- NULL

  # --- Label comparison ---
  label_rows <- list()
  for (col in common_cols) {
    l1 <- attr(df1[[col]], "label")
    l2 <- attr(df2[[col]], "label")
    l1 <- if (is.null(l1)) "" else as.character(l1)
    l2 <- if (is.null(l2)) "" else as.character(l2)
    if (l1 != l2) {
      label_rows[[col]] <- data.frame(
        variable = col,
        label_base = l1,
        label_compare = l2,
        stringsAsFactors = FALSE
      )
    }
  }
  label_mismatches <- if (length(label_rows) > 0) {
    do.call(rbind, label_rows)
  } else {
    data.frame(
      variable = character(0),
      label_base = character(0),
      label_compare = character(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(label_mismatches) <- NULL

  # --- Column order comparison ---
  df1_common_order <- names(df1)[names(df1) %in% common_cols]
  df2_common_order <- names(df2)[names(df2) %in% common_cols]
  order_match <- identical(df1_common_order, df2_common_order)

  list(
    type_mismatches = type_mismatches,
    label_mismatches = label_mismatches,
    order_match = order_match,
    order_df1 = df1_common_order,
    order_df2 = df2_common_order
  )
}
