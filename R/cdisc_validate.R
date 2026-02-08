#' Detect CDISC Domain Type
#'
#' @description
#' Detects whether a data frame looks like an SDTM domain or ADaM dataset by comparing
#' column names against known CDISC standards. Calculates a confidence score based on
#' the percentage of expected variables present.
#'
#' Auto-detection is a convenience for exploratory use. For anything important --
#' validation reports, regulatory submissions, scripted pipelines -- always pass
#' \code{domain} and \code{standard} explicitly. Datasets with common columns
#' (STUDYID, USUBJID, etc.) can match multiple domains, and a warning is issued
#' when the top two candidates score within 10 percentage points of each other.
#'
#' @param df A data frame to analyze.
#' @param name_hint Optional character string with the dataset name (e.g., "DM",
#'   "ADLB", or a filename like "adlb.xpt"). When provided and it matches a known
#'   CDISC domain, that candidate receives a strong confidence boost. This makes
#'   detection much more accurate when the filename is available.
#'
#' @return A list containing:
#' \item{standard}{Character: "SDTM", "ADaM", or "Unknown"}
#' \item{domain}{Character: domain code (e.g., "DM", "AE") or dataset name (e.g., "ADSL"), or NA}
#' \item{confidence}{Numeric between 0 and 1 indicating match quality}
#' \item{message}{Character: human-readable explanation}
#'
#' @export
#' @examples
#' \donttest{
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
detect_cdisc_domain <- function(df, name_hint = NULL) {
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
  n_df_cols <- length(df_cols)
  sdtm_meta <- get_sdtm_metadata()
  adam_meta <- get_adam_metadata()

  results <- list()

  # Score each candidate using two signals:
  #   recall    = fraction of domain's REQ vars found in the data  (does it fit?)
  #   coverage  = fraction of the data's columns that appear in the domain spec (does it explain the data?)
  # Final score = 0.5 * recall + 0.5 * coverage
  # This prevents small domains (few REQ vars) from winning just because they are easy to satisfy.

  .score_domain <- function(meta_vars, df_cols, n_df_cols, domain_code) {
    all_vars_lower <- tolower(meta_vars$variable)
    required_vars <- tolower(meta_vars$variable[toupper(meta_vars$core) == "REQ"])

    if (length(required_vars) == 0) {
      return(list(recall = 0, coverage = 0, prefix_bonus = 0, score = 0))
    }

    recall <- sum(required_vars %in% df_cols) / length(required_vars)
    coverage <- if (n_df_cols > 0) sum(df_cols %in% all_vars_lower) / n_df_cols else 0

    # Prefix bonus: CDISC variable naming convention uses domain prefix
    # (e.g. LBSEQ, LBTESTCD for LB; EGSEQ, EGTESTCD for EG; VSSEQ for VS).
    # For ADaM datasets like ADLB, strip the "AD" prefix to get "LB".
    # Count how many data columns start with the domain's prefix -- this
    # breaks ties between domains that share the same generic variables.
    prefix <- tolower(domain_code)
    if (nchar(prefix) > 2 && startsWith(prefix, "ad")) {
      prefix <- substring(prefix, 3)  # ADLB -> lb, ADAE -> ae
    }
    prefix_hits <- sum(startsWith(df_cols, prefix) & nchar(df_cols) > nchar(prefix))
    prefix_bonus <- if (n_df_cols > 0) prefix_hits / n_df_cols else 0

    # Final score: recall (40%) + coverage (40%) + prefix (20%)
    score <- 0.4 * recall + 0.4 * coverage + 0.2 * prefix_bonus

    # Specificity: count of domain-specific variables that appear in the data.
    # Domain-specific vars are those whose names contain the domain prefix
    # (e.g. LBSEQ, LBTESTCD for LB; EGSEQ for EG).
    # Even a single domain-specific column hit adds a small bonus (0.01)
    # to break ties between domains with otherwise identical scores.
    domain_specific <- all_vars_lower[startsWith(all_vars_lower, prefix) &
                                       nchar(all_vars_lower) > nchar(prefix)]
    specificity <- sum(df_cols %in% domain_specific)
    if (specificity > 0) {
      score <- score + 0.01 * specificity
    }

    # Size tiebreaker: when two domains are otherwise tied, prefer the one
    # with a larger metadata specification. Larger specs are more specific
    # (e.g. ADLB has 31 vars vs ADEG has 28) and more likely to be the
    # intended domain.  Bonus is tiny (0.001 per var) to avoid overriding
    # the main signals.
    score <- score + 0.001 * length(all_vars_lower)

    list(recall = recall, coverage = coverage, prefix_bonus = prefix_bonus, score = score)
  }

  # Check SDTM domains
  for (domain in names(sdtm_meta)) {
    sc <- .score_domain(sdtm_meta[[domain]], df_cols, n_df_cols, domain)
    results[[paste0("SDTM_", domain)]] <- list(
      standard = "SDTM",
      domain = domain,
      confidence = sc$score,
      recall = sc$recall,
      coverage = sc$coverage
    )
  }

  # Check ADaM datasets
  for (dataset in names(adam_meta)) {
    sc <- .score_domain(adam_meta[[dataset]], df_cols, n_df_cols, dataset)
    results[[paste0("ADAM_", dataset)]] <- list(
      standard = "ADaM",
      domain = dataset,
      confidence = sc$score,
      recall = sc$recall,
      coverage = sc$coverage
    )
  }

  # ADaM indicator boost: certain columns only appear in ADaM datasets.
  # If present, boost all ADaM candidates to avoid misdetecting as SDTM.
  adam_indicator_cols <- c("trt01p", "trt01a", "saffl", "ittfl", "efffl",
                           "trtsdt", "trtedt", "base", "chg", "aval",
                           "avalc", "param", "paramcd", "avisit", "avisitn",
                           "ady", "astdt", "aendt", "ablfl", "anl01fl")
  adam_hits <- sum(df_cols %in% adam_indicator_cols)

  if (adam_hits >= 3) {
    # Strong ADaM signal -- boost all ADaM candidates.
    # Scale: 3 hits -> 0.10, 5 hits -> 0.20, 8+ hits -> 0.30 (cap)
    boost <- 0.10 + 0.04 * min(adam_hits - 3, 5)  # 0.10 to 0.30
    for (nm in names(results)) {
      if (results[[nm]]$standard == "ADaM") {
        results[[nm]]$confidence <- results[[nm]]$confidence + boost
      }
    }
  }

  # Name hint boost: if a dataset name or filename was provided, and it matches

  # a known domain, give that specific candidate a decisive boost.
  # e.g., name_hint = "ADLB" or "adlb.xpt" -> boost ADAM_ADLB
  if (!is.null(name_hint) && nchar(name_hint) > 0) {
    hint <- toupper(tools::file_path_sans_ext(basename(name_hint)))
    # Check both SDTM and ADaM candidates
    sdtm_key <- paste0("SDTM_", hint)
    adam_key <- paste0("ADAM_", hint)
    if (sdtm_key %in% names(results)) {
      results[[sdtm_key]]$confidence <- results[[sdtm_key]]$confidence + 0.50
    }
    if (adam_key %in% names(results)) {
      results[[adam_key]]$confidence <- results[[adam_key]]$confidence + 0.50
    }
  }

  # Rank all candidates by confidence, descending
  conf_scores <- vapply(results, function(x) x$confidence, numeric(1))
  ranked <- order(conf_scores, decreasing = TRUE)

  threshold <- 0.5
  if (length(ranked) == 0 || conf_scores[ranked[1]] <= threshold) {
    return(list(
      standard = "Unknown",
      domain = NA,
      confidence = 0,
      message = "Could not confidently match data frame to any known CDISC domain or dataset"
    ))
  }

  best <- results[[ranked[1]]]
  runner_up_conf <- if (length(ranked) > 1) conf_scores[ranked[2]] else 0

  # Warn when top two candidates score within 10 percentage points
  ambiguous <- (runner_up_conf > threshold) &&
    ((best$confidence - runner_up_conf) < 0.10)

  if (ambiguous) {
    runner_up <- results[[ranked[2]]]
    warning(
      sprintf(
        paste0(
          "Ambiguous domain detection: '%s' (%.0f%%) vs '%s' (%.0f%%). ",
          "Specify `domain` and `standard` explicitly for reliable results."
        ),
        best$domain, min(best$confidence, 1.0) * 100,
        runner_up$domain, min(runner_up_conf, 1.0) * 100
      ),
      call. = FALSE
    )
  }

  # Cap displayed confidence at 1.0 -- the internal score can exceed 1.0

  # due to tiebreaker bonuses, but that's only used for ranking.
  display_conf <- min(best$confidence, 1.0)
  display_recall <- min(if (!is.null(best$recall)) best$recall else display_conf, 1.0)
  display_coverage <- min(if (!is.null(best$coverage)) best$coverage else 0, 1.0)

  msg <- sprintf(
    "%s domain '%s' detected with %.0f%% confidence (%.0f%% required vars present, %.0f%% of columns explained)",
    best$standard, best$domain, display_conf * 100,
    display_recall * 100, display_coverage * 100
  )

  return(list(
    standard = best$standard,
    domain = best$domain,
    confidence = display_conf,
    message = msg
  ))
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
#' @return A data frame with columns:
#' \item{category}{Character: type of validation issue ("Missing Required Variable",
#'   "Missing Expected Variable", "Type Mismatch", "Non-Standard Variable", "Variable Info")}
#' \item{variable}{Character: variable name}
#' \item{message}{Character: description of the issue}
#' \item{severity}{Character: "ERROR", "WARNING", or "INFO"}
#'
#' @export
#' @examples
#' \donttest{
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


#' Internal CDISC Validation Worker
#'
#' @description
#' Internal function that performs the core validation logic for both SDTM and ADaM standards.
#' This helper function is called by [validate_sdtm()] and [validate_adam()] to avoid
#' code duplication. Checks for missing required/secondary variables, data type mismatches,
#' and non-standard variables.
#'
#' @param df A data frame to validate.
#' @param domain Character string specifying the domain/dataset code (e.g., "DM", "ADSL").
#' @param metadata Named list from [get_sdtm_metadata()] or [get_adam_metadata()].
#' @param standard_name Character: "SDTM" or "ADaM" (used in message text).
#' @param secondary_core Character: "EXP" for SDTM or "COND" for ADaM.
#'   Specifies which core type represents secondary variables.
#'
#' @return A data frame with validation results containing columns:
#' \item{category}{Character: validation issue type}
#' \item{variable}{Character: variable name}
#' \item{message}{Character: issue description}
#' \item{severity}{Character: "ERROR", "WARNING", or "INFO"}
#'
#' @details
#' Severity levels:
#' - ERROR: Required variable is missing
#' - WARNING (SDTM) or INFO (ADaM): Secondary variable missing or type mismatch
#' - INFO: Non-standard variable or variable information
#'
#' @noRd
#' @keywords internal
.validate_cdisc_internal <- function(df, domain, metadata, standard_name,
                                     secondary_core = "EXP") {
  # meta_vars IS the data.frame directly, not a nested object
  meta_vars <- metadata[[domain]]
  df_cols <- colnames(df)
  df_cols_lower <- tolower(df_cols)

  results <- list()

  # Determine appropriate labels based on standard
  domain_type_label <- if (standard_name == "SDTM") "domain" else "dataset"
  secondary_category <- if (secondary_core == "EXP") {
    "Missing Expected Variable"
  } else {
    "Missing Conditional Variable"
  }
  secondary_severity <- if (secondary_core == "EXP") "WARNING" else "INFO"

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
            "Required variable '%s' is missing from %s %s",
            var_name, domain, domain_type_label
          ),
          severity = "ERROR",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Check for missing secondary variables (EXP or COND)
  secondary_idx <- toupper(meta_vars$core) == secondary_core
  if (any(secondary_idx)) {
    secondary_vars <- meta_vars[secondary_idx, , drop = FALSE]
    for (i in seq_len(nrow(secondary_vars))) {
      var_name <- secondary_vars$variable[i]
      var_lower <- tolower(var_name)

      if (!var_lower %in% df_cols_lower) {
        results[[paste0("missing_sec_", var_name)]] <- data.frame(
          category = secondary_category,
          variable = var_name,
          message = sprintf(
            "%s variable '%s' is not present in %s %s",
            sub("Missing ", "", secondary_category), var_name, domain, domain_type_label
          ),
          severity = secondary_severity,
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
            "Variable '%s' has type '%s' but %s expects '%s'",
            var_name, actual_type, standard_name, expected_type
          ),
          severity = "WARNING",
          stringsAsFactors = FALSE
        )
      }

      # Add variable info for present variables
      results[[paste0("var_info_", var_name)]] <- data.frame(
        category = "Variable Info",
        variable = actual_col,
        message = sprintf("%s variable '%s': %s", standard_name, var_name, meta_vars$label[i]),
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
          "Variable '%s' is not part of the %s %s %s specification",
          col, domain, standard_name, domain_type_label
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


#' Validate SDTM Compliance
#'
#' @description
#' Validates a data frame against a specific SDTM domain specification. Checks for
#' missing required/expected variables, data type mismatches, and non-standard variables.
#'
#' @param df A data frame to validate.
#' @param domain Character string specifying the SDTM domain code (e.g., "DM", "AE", "VS").
#'
#' @return A data frame with validation results containing columns:
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
#' @keywords internal
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

  .validate_cdisc_internal(df, domain, sdtm_meta, "SDTM", "EXP")
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
#' @return A data frame with validation results containing columns:
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
#' @keywords internal
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

  .validate_cdisc_internal(df, domain, adam_meta, "ADaM", "COND")
}


#' Get Default ID Variables for CDISC Domain
#'
#' @description
#' Returns CDISC-conventional key variables for a given domain/dataset.
#' Used internally by [cdisc_compare()] when \code{id_vars = NULL}.
#'
#' @param domain Character: SDTM domain code or ADaM dataset name.
#' @param standard Character: "SDTM" or "ADaM".
#' @return Character vector of ID variable names, or NULL if unknown.
#' @keywords internal
#' @noRd
get_default_id_vars <- function(domain, standard) {
  domain <- toupper(domain)
  standard <- toupper(standard)

  if (standard == "SDTM") {
    if (domain == "DM") return(c("STUDYID", "USUBJID"))
    seq_var <- paste0(domain, "SEQ")
    return(c("STUDYID", "USUBJID", seq_var))
  }

  if (standard == "ADAM") {
    adam_keys <- list(
      ADSL  = c("STUDYID", "USUBJID"),
      ADAE  = c("STUDYID", "USUBJID", "AESEQ"),
      ADLB  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADVS  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADEG  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADPC  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADPP  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADTTE = c("STUDYID", "USUBJID", "PARAMCD"),
      ADCM  = c("STUDYID", "USUBJID", "CMSEQ"),
      ADMH  = c("STUDYID", "USUBJID", "MHSEQ"),
      ADEX  = c("STUDYID", "USUBJID", "EXSEQ"),
      ADRS  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADTR  = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      ADEFF = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
    if (domain %in% names(adam_keys)) return(adam_keys[[domain]])
  }

  NULL
}


#' Load Dataset from File Path
#'
#' @description
#' Internal helper that loads a dataset from file if a path is provided.
#' Supports .xpt, .sas7bdat, .csv, and .rds formats.
#'
#' @param x Data frame or character file path.
#' @param domain Optional domain hint.
#' @return List with \code{data} (data frame) and \code{domain_hint} (character or NULL).
#' @keywords internal
#' @noRd
.load_if_filepath <- function(x, domain = NULL) {
  if (is.data.frame(x)) return(list(data = x, domain_hint = NULL))
  if (!is.character(x) || length(x) != 1) return(list(data = x, domain_hint = NULL))

  ext <- tolower(tools::file_ext(x))
  if (!ext %in% c("xpt", "sas7bdat", "csv", "rds")) {
    return(list(data = x, domain_hint = NULL))
  }

  if (!file.exists(x)) stop(sprintf("File not found: %s", x), call. = FALSE)

  data <- switch(ext,
    xpt      = haven::read_xpt(x),
    sas7bdat = haven::read_sas(x),
    csv      = utils::read.csv(x, stringsAsFactors = FALSE),
    rds      = readRDS(x)
  )

  domain_hint <- toupper(tools::file_path_sans_ext(basename(x)))
  list(data = as.data.frame(data), domain_hint = domain_hint)
}


#' Compare Two Datasets with CDISC Validation
#'
#' @description
#' Flagship function that compares two datasets AND runs CDISC validation on both.
#' Combines dataset comparison with CDISC conformance analysis to provide comprehensive
#' insights into both differences and regulatory compliance.
#'
#' @param df1 First data frame to compare, or a file path (character string
#'   ending in \code{.xpt}, \code{.sas7bdat}, \code{.csv}, or \code{.rds}).
#'   When a file path is provided, the dataset is loaded automatically.
#'   Domain is auto-detected from filename if not specified (e.g.,
#'   \code{"dm.xpt"} sets domain to \code{"DM"}).
#' @param df2 Second data frame to compare, or a file path.
#' @param domain Optional character string specifying the CDISC domain code or dataset name
#'   (e.g., "DM", "AE", "ADSL"). Strongly recommended -- auto-detection can be
#'   ambiguous for datasets with common columns. If NULL, auto-detected from df1.
#' @param standard Optional character string: "SDTM" or "ADaM". If NULL, auto-detected from df1.
#' @param id_vars Optional character vector of ID variable names (e.g.,
#'   \code{c("USUBJID", "VISITNUM")}) used to match rows between datasets.
#'   When provided, rows are joined by these keys instead of matched by position.
#'   Unmatched rows are reported separately. When \code{NULL} (default) and
#'   domain is known, CDISC-standard keys are auto-detected (e.g.,
#'   STUDYID + USUBJID + \<DOMAIN\>SEQ for SDTM). Only variables present in
#'   both datasets are used. To add extra keys on top of the defaults, prefix
#'   with \code{"+"}: e.g., \code{id_vars = c("+", "AETOXGR")} appends AETOXGR
#'   to the standard keys. To override completely, pass without \code{"+"}.
#' @param vars Optional character vector of variable names to compare. Only these columns are included in value comparison. Structural and CDISC validation still covers all columns.
#' @param ts_data Optional data frame of the TS (Trial Summary) domain.
#'   When provided, CDISC standard versions (e.g., SDTM IG 3.4, ADaM IG 1.3)
#'   are extracted and included in the results and reports. If NULL (default),
#'   version information is omitted.
#' @param detect_outliers Logical. When TRUE, runs z-score outlier detection
#'   on numeric columns and includes results in the output. Defaults to FALSE.
#' @param tolerance Numeric tolerance value for floating-point comparisons (default 0).
#'   When tolerance > 0, numeric values are considered equal if their absolute
#'   difference is within the tolerance threshold. Character and factor columns
#'   always use exact matching regardless of tolerance.
#' @param where Optional filter expression as a string (e.g., "AESEV == 'SEVERE'").
#'   Applied to both datasets before comparison. Equivalent to a WHERE clause.
#'
#' @return A list containing:
#' \item{domain}{Character: detected or supplied CDISC domain}
#' \item{standard}{Character: detected or supplied CDISC standard (SDTM/ADaM)}
#' \item{nrow_df1}{Integer: number of rows in df1}
#' \item{ncol_df1}{Integer: number of columns in df1}
#' \item{nrow_df2}{Integer: number of rows in df2}
#' \item{ncol_df2}{Integer: number of columns in df2}
#' \item{id_vars}{Character vector of ID variables used for matching (NULL if
#'   positional matching was used)}
#' \item{comparison}{Result of [compare_datasets()] function}
#' \item{variable_comparison}{Result of [compare_variables()] function}
#' \item{metadata_comparison}{List of metadata differences: type_mismatches,
#'   label_mismatches, length_mismatches, format_mismatches, column ordering}
#' \item{observation_comparison}{Result of [compare_observations()] if dimensions match,
#'   otherwise NULL with explanatory message}
#' \item{unified_comparison}{Data frame combining attribute and value differences
#'   per variable. Columns: variable, attribute, base_value, compare_value,
#'   and optionally id columns and row when value differences exist}
#' \item{unmatched_rows}{List with df1_only and df2_only data frames of rows that
#'   could not be matched by id_vars (NULL when id_vars is not used)}
#' \item{cdisc_validation_df1}{CDISC validation results for df1}
#' \item{cdisc_validation_df2}{CDISC validation results for df2}
#' \item{cdisc_conformance_comparison}{Data frame showing which CDISC issues are unique
#'   to df1, unique to df2, or common to both}
#' \item{outlier_notes}{Data frame of z-score outliers (|z| > 3) found in
#'   numeric columns of either dataset (NULL when detect_outliers is FALSE)}
#' \item{cdisc_version}{List of CDISC version information extracted from TS
#'   domain (NULL when ts_data is not provided). See [extract_cdisc_version()]}
#'
#' @export
#' @examples
#' \donttest{
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
#' # Positional matching (default)
#' result <- cdisc_compare(dm1, dm2, domain = "DM", standard = "SDTM")
#'
#' # Key-based matching by ID variables
#' result <- cdisc_compare(dm1, dm2, domain = "DM", id_vars = c("USUBJID"))
#' names(result)
#' }
cdisc_compare <- function(df1, df2, domain = NULL, standard = NULL,
                          id_vars = NULL, vars = NULL, ts_data = NULL,
                          detect_outliers = FALSE, tolerance = 0, where = NULL) {
  # --- Handle file paths (Phase 2) ---
  loaded1 <- .load_if_filepath(df1)
  if (is.data.frame(loaded1$data)) {
    df1 <- loaded1$data
    if (is.null(domain) && !is.null(loaded1$domain_hint)) {
      domain <- loaded1$domain_hint
    }
  }

  loaded2 <- .load_if_filepath(df2)
  if (is.data.frame(loaded2$data)) {
    df2 <- loaded2$data
  }

  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("Both inputs must be data frames or valid file paths (.xpt, .sas7bdat, .csv, .rds)", call. = FALSE)
  }

  # Apply WHERE filter if specified
  if (!is.null(where)) {
    if (!is.character(where) || length(where) != 1 || nchar(trimws(where)) == 0) {
      stop("where must be a non-empty character string", call. = FALSE)
    }
    where_expr <- tryCatch(
      rlang::parse_expr(where),
      error = function(e) stop(sprintf("Invalid WHERE expression: %s", e$message), call. = FALSE)
    )
    df1 <- tryCatch(
      dplyr::filter(df1, !!where_expr),
      error = function(e) stop(sprintf("WHERE filter failed on base dataset: %s", e$message), call. = FALSE)
    )
    df2 <- tryCatch(
      dplyr::filter(df2, !!where_expr),
      error = function(e) stop(sprintf("WHERE filter failed on compare dataset: %s", e$message), call. = FALSE)
    )
    if (nrow(df1) == 0 && nrow(df2) == 0) {
      warning("WHERE filter returned 0 rows from both datasets", call. = FALSE)
    }
  }

  # Validate tolerance
  if (!is.numeric(tolerance) || length(tolerance) != 1 || is.na(tolerance) || tolerance < 0 || is.infinite(tolerance)) {
    stop("tolerance must be a single non-negative finite number", call. = FALSE)
  }

  # Auto-detect domain/standard from df1 if not provided
  if (is.null(domain) || is.null(standard)) {
    detection <- detect_cdisc_domain(df1, name_hint = loaded1$domain_hint)

    if (is.null(standard)) {
      standard <- if (detection$standard == "Unknown") NA else detection$standard
    }
    if (is.null(domain)) {
      domain <- if (is.na(detection$domain)) NA else detection$domain
    }
  }

  # --- Auto-detect ID variables from CDISC conventions (Phase 1) ---
  # Supports three modes:
  #   id_vars = NULL           -> auto-detect from CDISC defaults
  #   id_vars = c("A", "B")   -> use exactly these variables (override)
  #   id_vars = c("+", "X")   -> auto-detect defaults, then append "X"
  extra_id_vars <- NULL
  if (!is.null(id_vars) && length(id_vars) >= 1 && id_vars[1] == "+") {
    if (length(id_vars) < 2) {
      stop("'+' prefix requires at least one additional variable name, e.g. id_vars = c('+', 'MYVAR')", call. = FALSE)
    }
    extra_id_vars <- id_vars[-1]
    id_vars <- NULL  # trigger auto-detection, then append extras
  }

  if (is.null(id_vars) && !is.na(domain) && !is.na(standard)) {
    candidate_vars <- get_default_id_vars(domain, standard)
    if (!is.null(candidate_vars)) {
      # Append user-specified extras to defaults
      if (!is.null(extra_id_vars)) {
        candidate_vars <- unique(c(candidate_vars, extra_id_vars))
      }
      available_vars <- intersect(candidate_vars, intersect(names(df1), names(df2)))
      if (length(available_vars) >= 2) {
        id_vars <- available_vars
        message(sprintf(
          "ID variables auto-detected for %s %s: %s",
          standard, domain, paste(id_vars, collapse = ", ")
        ))
      }
    }
  }

  # If "+" was used but auto-detection found nothing, fall back to extras alone
  if (is.null(id_vars) && !is.null(extra_id_vars)) {
    available_extras <- intersect(extra_id_vars, intersect(names(df1), names(df2)))
    if (length(available_extras) > 0) {
      id_vars <- available_extras
      warning("Could not auto-detect CDISC key variables; using only the appended variables as keys.", call. = FALSE)
    }
  }

  # Validate id_vars if provided
  if (!is.null(id_vars)) {
    if (!is.character(id_vars) || length(id_vars) == 0) {
      stop("id_vars must be a character vector of column names", call. = FALSE)
    }
    missing_in_df1 <- setdiff(id_vars, names(df1))
    missing_in_df2 <- setdiff(id_vars, names(df2))
    if (length(missing_in_df1) > 0) {
      stop(sprintf("id_vars not found in Base dataset: %s",
                    paste(missing_in_df1, collapse = ", ")), call. = FALSE)
    }
    if (length(missing_in_df2) > 0) {
      stop(sprintf("id_vars not found in Compare dataset: %s",
                    paste(missing_in_df2, collapse = ", ")), call. = FALSE)
    }
  }

  # Run dataset comparison (pass tolerance and vars for consistent observation comparison)
  comparison <- compare_datasets(df1, df2, tolerance = tolerance, vars = vars)
  variable_comparison <- compare_variables(df1, df2)

  # Identify common columns (case-insensitive match)
  df1_cols_lower <- tolower(colnames(df1))
  df2_cols_lower <- tolower(colnames(df2))
  common_cols_lower <- df1_cols_lower[df1_cols_lower %in% df2_cols_lower]

  # Get actual (original-case) common column names from df1
  common_cols <- colnames(df1)[df1_cols_lower %in% common_cols_lower]

  # --- Observation comparison (with id_vars support) ---
  observation_comparison <- NULL
  unmatched_rows <- NULL

  if (!is.null(id_vars) && length(common_cols) > 0) {
    # KEY-BASED matching via id_vars
    obs_result <- compare_observations_by_id(df1, df2, id_vars, common_cols, tolerance = tolerance)
    observation_comparison <- obs_result$observation_comparison
    unmatched_rows <- obs_result$unmatched_rows
  } else if (nrow(df1) != nrow(df2)) {
    observation_comparison <- list(
      status = "Skipped",
      message = sprintf(
        "Observation comparison skipped: row counts differ (df1: %d, df2: %d). Consider using id_vars to match by key.",
        nrow(df1), nrow(df2)
      )
    )
  } else if (length(common_cols) == 0) {
    observation_comparison <- list(
      status = "Skipped",
      message = "Observation comparison skipped: no common columns found"
    )
  } else {
    # Positional matching (original behavior)
    df1_common_idx <- which(df1_cols_lower %in% common_cols_lower)
    df2_common_idx <- which(df2_cols_lower %in% common_cols_lower)
    df1_common <- df1[, df1_common_idx, drop = FALSE]
    df2_common <- df2[, df2_common_idx, drop = FALSE]

    observation_comparison <- tryCatch({
      compare_observations(df1_common, df2_common, tolerance = tolerance)
    }, error = function(e) {
      list(
        status = "Error",
        message = sprintf("Observation comparison failed: %s", conditionMessage(e))
      )
    })
  }

  # Build metadata comparison for common columns
  metadata_comparison <- build_metadata_comparison(df1, df2)

  # Build unified comparison table (attribute + value in one table)
  unified_comparison <- build_unified_comparison(
    metadata_comparison, observation_comparison, id_vars, df1, df2
  )

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

  # Detect outliers using z-score method on numeric columns (opt-in)
  outlier_notes <- NULL
  if (isTRUE(detect_outliers)) {
    outlier_notes <- detect_outliers_zscore(df1, df2)
  }

  # Extract CDISC version from TS domain if provided
  cdisc_version <- NULL
  if (!is.null(ts_data)) {
    cdisc_version <- tryCatch(
      extract_cdisc_version(ts_data),
      error = function(e) NULL,
      warning = function(w) {
        suppressWarnings(extract_cdisc_version(ts_data))
      }
    )
  }

  result <- list(
    domain = domain,
    standard = standard,
    nrow_df1 = nrow(df1),
    ncol_df1 = ncol(df1),
    nrow_df2 = nrow(df2),
    ncol_df2 = ncol(df2),
    id_vars = id_vars,
    tolerance = tolerance,
    comparison = comparison,
    variable_comparison = variable_comparison,
    metadata_comparison = metadata_comparison,
    observation_comparison = observation_comparison,
    unified_comparison = unified_comparison,
    unmatched_rows = unmatched_rows,
    cdisc_validation_df1 = val_df1,
    cdisc_validation_df2 = val_df2,
    cdisc_conformance_comparison = conform_comparison,
    outlier_notes = outlier_notes,
    cdisc_version = cdisc_version
  )
  class(result) <- "cdisc_comparison"
  return(result)
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
#' @return A data frame showing CDISC issue distribution across datasets, with columns:
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
#' Internal function to compare metadata attributes (types, labels, lengths,
#' formats, and column order) between two datasets.
#'
#' @param df1 First data frame (base).
#' @param df2 Second data frame (compare).
#'
#' @return A list with:
#' \item{type_mismatches}{Data frame of variables with differing R classes}
#' \item{label_mismatches}{Data frame of variables with differing labels}
#' \item{length_mismatches}{Data frame of variables with differing lengths
#'   (max character width or haven width attribute)}
#' \item{format_mismatches}{Data frame of variables with differing SAS format
#'   attributes (format.sas or display_format)}
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

  # --- Length comparison (max character width or attr "width") ---
  length_rows <- list()
  for (col in common_cols) {
    w1 <- attr(df1[[col]], "width")
    w2 <- attr(df2[[col]], "width")
    if (is.null(w1)) {
      w1 <- if (is.character(df1[[col]]) && length(df1[[col]]) > 0) {
        max(nchar(as.character(df1[[col]])), na.rm = TRUE)
      } else {
        NA_integer_
      }
    }
    if (is.null(w2)) {
      w2 <- if (is.character(df2[[col]]) && length(df2[[col]]) > 0) {
        max(nchar(as.character(df2[[col]])), na.rm = TRUE)
      } else {
        NA_integer_
      }
    }
    if (!is.na(w1) && !is.na(w2) && w1 != w2) {
      length_rows[[col]] <- data.frame(
        variable = col,
        length_base = as.integer(w1),
        length_compare = as.integer(w2),
        stringsAsFactors = FALSE
      )
    }
  }
  length_mismatches <- if (length(length_rows) > 0) {
    do.call(rbind, length_rows)
  } else {
    data.frame(
      variable = character(0),
      length_base = integer(0),
      length_compare = integer(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(length_mismatches) <- NULL

  # --- Format comparison (SAS format attributes from haven) ---
  format_rows <- list()
  for (col in common_cols) {
    f1 <- attr(df1[[col]], "format.sas")
    if (is.null(f1)) f1 <- attr(df1[[col]], "display_format")
    f2 <- attr(df2[[col]], "format.sas")
    if (is.null(f2)) f2 <- attr(df2[[col]], "display_format")
    f1 <- if (is.null(f1)) "" else as.character(f1)
    f2 <- if (is.null(f2)) "" else as.character(f2)
    if (f1 != f2 && !(f1 == "" && f2 == "")) {
      format_rows[[col]] <- data.frame(
        variable = col,
        format_base = f1,
        format_compare = f2,
        stringsAsFactors = FALSE
      )
    }
  }
  format_mismatches <- if (length(format_rows) > 0) {
    do.call(rbind, format_rows)
  } else {
    data.frame(
      variable = character(0),
      format_base = character(0),
      format_compare = character(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(format_mismatches) <- NULL

  # --- Column order comparison ---
  df1_common_order <- names(df1)[names(df1) %in% common_cols]
  df2_common_order <- names(df2)[names(df2) %in% common_cols]
  order_match <- identical(df1_common_order, df2_common_order)

  list(
    type_mismatches = type_mismatches,
    label_mismatches = label_mismatches,
    length_mismatches = length_mismatches,
    format_mismatches = format_mismatches,
    order_match = order_match,
    order_df1 = df1_common_order,
    order_df2 = df2_common_order
  )
}


#' Compare Observations by ID Variables
#'
#' @description
#' Internal function to match rows between two datasets using specified key
#' variables, then compare values on matched rows. Also identifies unmatched
#' rows in either dataset.
#'
#' @param df1 First data frame (base).
#' @param df2 Second data frame (compare).
#' @param id_vars Character vector of ID column names.
#' @param common_cols Character vector of common column names.
#' @param tolerance Numeric tolerance value for floating-point comparisons (default 0).
#'   When tolerance > 0, numeric values are considered equal if their absolute
#'   difference is within the tolerance threshold. Character and factor columns
#'   always use exact matching regardless of tolerance.
#'
#' @return A list with:
#' \item{observation_comparison}{List with discrepancies and details (same
#'   structure as [compare_observations()] output), plus id_details containing
#'   the ID variable values for each difference}
#' \item{unmatched_rows}{List with df1_only and df2_only data frames}
#'
#' @keywords internal
compare_observations_by_id <- function(df1, df2, id_vars, common_cols, tolerance = 0) {
  # Build composite key for matching
  make_key <- function(df, vars) {
    key_parts <- lapply(vars, function(v) {
      vals <- as.character(df[[v]])
      # Use a sentinel that won't appear in real data to distinguish true NA
      vals[is.na(df[[v]])] <- "\x01NA\x01"
      vals
    })
    do.call(paste, c(key_parts, list(sep = "||")))
  }

  key1 <- make_key(df1, id_vars)
  key2 <- make_key(df2, id_vars)

  matched_in_df1 <- key1 %in% key2
  matched_in_df2 <- key2 %in% key1

  # Unmatched rows
  df1_only <- if (any(!matched_in_df1)) df1[!matched_in_df1, , drop = FALSE] else NULL
  df2_only <- if (any(!matched_in_df2)) df2[!matched_in_df2, , drop = FALSE] else NULL

  unmatched_rows <- list(df1_only = df1_only, df2_only = df2_only)

  # Warn about duplicate keys (match() only uses first occurrence)
  dup1 <- duplicated(key1)
  dup2 <- duplicated(key2)
  if (any(dup1)) {
    n_dup1 <- sum(dup1)
    warning(sprintf("Base dataset has %d duplicate key(s) -- only first occurrence of each will be compared. Consider adding more id_vars.", n_dup1), call. = FALSE)
  }
  if (any(dup2)) {
    n_dup2 <- sum(dup2)
    warning(sprintf("Compare dataset has %d duplicate key(s) -- only first occurrence of each will be compared. Consider adding more id_vars.", n_dup2), call. = FALSE)
  }

  # Compare only matched rows
  matched_keys <- intersect(key1, key2)
  if (length(matched_keys) == 0) {
    observation_comparison <- list(
      status = "Skipped",
      message = "No matching rows found by ID variables"
    )
    return(list(
      observation_comparison = observation_comparison,
      unmatched_rows = unmatched_rows
    ))
  }

  # Pre-compute index mapping ONCE using match() -- O(n) instead of O(n^2)
  idx_map1 <- match(matched_keys, key1)
  idx_map2 <- match(matched_keys, key2)

  # For each matched key, compare values on non-id common columns
  compare_cols <- setdiff(common_cols, id_vars)
  discrepancy_counts <- integer(length(compare_cols))
  names(discrepancy_counts) <- compare_cols
  row_differences <- list()
  id_details <- list()

  # Pre-extract ID values for all matched rows (once, not per-column)
  id_vals_all <- lapply(id_vars, function(v) as.character(df1[[v]][idx_map1]))
  names(id_vals_all) <- id_vars

  for (col in compare_cols) {
    # Vectorized: extract all matched values at once
    vals1 <- df1[[col]][idx_map1]
    vals2 <- df2[[col]][idx_map2]

    # Handle factors
    if (is.factor(vals1) || is.factor(vals2)) {
      vals1 <- as.character(vals1)
      vals2 <- as.character(vals2)
    }

    # NA handling (vectorized)
    both_na <- is.na(vals1) & is.na(vals2)
    either_na <- is.na(vals1) | is.na(vals2)
    na_mismatch <- either_na & !both_na

    # Value comparison (vectorized)
    is_numeric_col <- is.numeric(vals1) && is.numeric(vals2)
    if (is_numeric_col && tolerance > 0) {
      raw_diff <- abs(vals1 - vals2)
      value_mismatch <- !either_na & (raw_diff > tolerance | is.nan(raw_diff))
    } else if (is_numeric_col) {
      inf_mismatch <- !either_na & (is.infinite(vals1) | is.infinite(vals2)) &
                      is.nan(vals1 - vals2)
      value_mismatch <- !either_na & (vals1 != vals2) | inf_mismatch
    } else {
      value_mismatch <- !either_na & (as.character(vals1) != as.character(vals2))
    }

    differences <- which(na_mismatch | value_mismatch)
    discrepancy_counts[col] <- length(differences)

    if (length(differences) > 0) {
      row_differences[[col]] <- data.frame(
        Row = idx_map1[differences],
        Value_in_df1 = vals1[differences],
        Value_in_df2 = vals2[differences],
        stringsAsFactors = FALSE
      )
      # ID values for differing rows
      id_df <- as.data.frame(
        lapply(id_vals_all, function(v) v[differences]),
        stringsAsFactors = FALSE
      )
      names(id_df) <- id_vars
      id_details[[col]] <- id_df
    }
  }

  observation_comparison <- list(
    discrepancies = discrepancy_counts,
    details = row_differences,
    id_details = id_details
  )

  list(
    observation_comparison = observation_comparison,
    unmatched_rows = unmatched_rows
  )
}


#' Build Unified Comparison Table
#'
#' @description
#' Internal function that merges attribute differences (type, label, length,
#' format) and value differences into a single data frame, giving a
#' consolidated per-variable view of all differences.
#'
#' @param meta Metadata comparison list from [build_metadata_comparison()].
#' @param obs_comp Observation comparison list from [compare_observations()]
#'   or [compare_observations_by_id()].
#' @param id_vars Character vector of ID variable names (or NULL).
#' @param df1 First data frame (base), used to retrieve ID values.
#' @param df2 Second data frame (compare).
#'
#' @return A data frame with columns: variable, diff_type, row_or_key,
#'   base_value, compare_value. The diff_type column indicates whether
#'   the row is a Type, Label, Length, Format, or Value difference.
#'
#' @keywords internal
build_unified_comparison <- function(meta, obs_comp, id_vars, df1, df2) {
  rows <- list()

  # --- Attribute differences from metadata ---
  if (!is.null(meta)) {
    # Type mismatches
    if (nrow(meta$type_mismatches) > 0) {
      for (i in seq_len(nrow(meta$type_mismatches))) {
        rows[[length(rows) + 1]] <- data.frame(
          variable = meta$type_mismatches$variable[i],
          diff_type = "Type",
          row_or_key = "--",
          base_value = meta$type_mismatches$type_base[i],
          compare_value = meta$type_mismatches$type_compare[i],
          stringsAsFactors = FALSE
        )
      }
    }

    # Label mismatches
    if (nrow(meta$label_mismatches) > 0) {
      for (i in seq_len(nrow(meta$label_mismatches))) {
        bl <- meta$label_mismatches$label_base[i]
        cl <- meta$label_mismatches$label_compare[i]
        rows[[length(rows) + 1]] <- data.frame(
          variable = meta$label_mismatches$variable[i],
          diff_type = "Label",
          row_or_key = "--",
          base_value = if (nchar(bl) == 0) "(none)" else bl,
          compare_value = if (nchar(cl) == 0) "(none)" else cl,
          stringsAsFactors = FALSE
        )
      }
    }

    # Length mismatches
    if (!is.null(meta$length_mismatches) && nrow(meta$length_mismatches) > 0) {
      for (i in seq_len(nrow(meta$length_mismatches))) {
        rows[[length(rows) + 1]] <- data.frame(
          variable = meta$length_mismatches$variable[i],
          diff_type = "Length",
          row_or_key = "--",
          base_value = as.character(meta$length_mismatches$length_base[i]),
          compare_value = as.character(meta$length_mismatches$length_compare[i]),
          stringsAsFactors = FALSE
        )
      }
    }

    # Format mismatches
    if (!is.null(meta$format_mismatches) && nrow(meta$format_mismatches) > 0) {
      for (i in seq_len(nrow(meta$format_mismatches))) {
        bf <- meta$format_mismatches$format_base[i]
        cf <- meta$format_mismatches$format_compare[i]
        rows[[length(rows) + 1]] <- data.frame(
          variable = meta$format_mismatches$variable[i],
          diff_type = "Format",
          row_or_key = "--",
          base_value = if (nchar(bf) == 0) "(none)" else bf,
          compare_value = if (nchar(cf) == 0) "(none)" else cf,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # --- Value differences from observation comparison ---
  if (!is.null(obs_comp) && is.list(obs_comp) &&
      !is.null(obs_comp$details) && is.list(obs_comp$details) &&
      length(obs_comp$details) > 0) {

    has_id_details <- !is.null(obs_comp$id_details)

    for (var_name in names(obs_comp$details)) {
      var_diffs <- obs_comp$details[[var_name]]
      if (!is.data.frame(var_diffs) || nrow(var_diffs) == 0) next

      for (j in seq_len(nrow(var_diffs))) {
        row_num <- var_diffs$Row[j]
        val1 <- as.character(var_diffs$Value_in_df1[j])
        val2 <- as.character(var_diffs$Value_in_df2[j])

        # Build key label from id_vars or row number
        if (has_id_details && var_name %in% names(obs_comp$id_details)) {
          id_df <- obs_comp$id_details[[var_name]]
          if (j <= nrow(id_df)) {
            key_parts <- vapply(id_vars, function(v) {
              paste0(v, "=", id_df[[v]][j])
            }, character(1))
            key_label <- paste(key_parts, collapse = ", ")
          } else {
            key_label <- sprintf("Row %d", row_num)
          }
        } else if (!is.null(id_vars) && length(id_vars) > 0 &&
                   row_num <= nrow(df1)) {
          key_parts <- vapply(id_vars, function(v) {
            paste0(v, "=", as.character(df1[[v]][row_num]))
          }, character(1))
          key_label <- paste(key_parts, collapse = ", ")
        } else {
          key_label <- sprintf("Row %d", row_num)
        }

        rows[[length(rows) + 1]] <- data.frame(
          variable = var_name,
          diff_type = "Value",
          row_or_key = key_label,
          base_value = val1,
          compare_value = val2,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      variable = character(0),
      diff_type = character(0),
      row_or_key = character(0),
      base_value = character(0),
      compare_value = character(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}


#' Detect Outliers Using Z-Score Method
#'
#' @description
#' Internal function to detect potential outliers in numeric columns of both
#' datasets using the z-score method. Values with |z| > 3 are flagged.
#' Results are returned as advisory notes for the user.
#'
#' @param df1 First data frame (base).
#' @param df2 Second data frame (compare).
#' @param threshold Numeric z-score threshold (default 3).
#'
#' @return A data frame with columns: dataset, variable, row, value, zscore.
#' Empty data frame if no outliers found.
#'
#' @keywords internal
detect_outliers_zscore <- function(df1, df2, threshold = 3) {
  outlier_rows <- list()

  for (ds_label in c("Base", "Compare")) {
    df <- if (ds_label == "Base") df1 else df2

    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    for (col in num_cols) {
      vals <- df[[col]]
      vals_clean <- vals[!is.na(vals)]
      if (length(vals_clean) < 3) next

      col_mean <- mean(vals_clean)
      col_sd <- stats::sd(vals_clean)
      if (is.na(col_sd) || col_sd == 0) next

      z_scores <- (vals - col_mean) / col_sd
      outlier_idx <- which(!is.na(z_scores) & abs(z_scores) > threshold)

      for (idx in outlier_idx) {
        outlier_rows[[length(outlier_rows) + 1]] <- data.frame(
          dataset = ds_label,
          variable = col,
          row = idx,
          value = vals[idx],
          zscore = round(z_scores[idx], 2),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(outlier_rows) > 0) {
    result <- do.call(rbind, outlier_rows)
    rownames(result) <- NULL
    return(result)
  }

  data.frame(
    dataset = character(0),
    variable = character(0),
    row = integer(0),
    value = numeric(0),
    zscore = numeric(0),
    stringsAsFactors = FALSE
  )
}
