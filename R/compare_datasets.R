#' Compare Two Datasets
#'
#' @description
#' Compares two datasets at three levels in a single call:
#'
#' \enumerate{
#'   \item \strong{Dataset level} -- dimensions, column overlap, missing-value
#'     totals.
#'   \item \strong{Variable level} -- column name discrepancies and data-type
#'     mismatches (delegates to [compare_variables()]).
#'   \item \strong{Observation level} -- row-by-row value differences on common
#'     columns (delegates to [compare_observations()]). Skipped gracefully when
#'     row counts differ.
#' }
#'
#' The return value is a list with class \code{"dataset_comparison"}, which has
#' a tidy [print()] method. The same object is accepted by
#' [generate_summary_report()], [generate_detailed_report()], and
#' [compare_by_group()].
#'
#' @param df1 A data frame (the \emph{base} dataset).
#' @param df2 A data frame (the \emph{compare} dataset).
#' @param tolerance Numeric tolerance value for floating-point comparisons (default 0).
#'   When tolerance > 0, numeric values are considered equal if their absolute
#'   difference is within the tolerance threshold. Character and factor columns
#'   always use exact matching regardless of tolerance.
#' @param vars Optional character vector of variable names to compare. When provided, only these columns are included in the observation-level comparison. Structural comparison (extra columns, type mismatches) still covers all columns. Default is NULL (compare all common columns).
#'
#' @return A \code{dataset_comparison} list containing:
#'   \item{nrow_df1, ncol_df1}{Dimensions of df1.}
#'   \item{nrow_df2, ncol_df2}{Dimensions of df2.}
#'   \item{common_columns}{Character vector of columns present in both.}
#'   \item{extra_in_df1}{Columns only in df1.}
#'   \item{extra_in_df2}{Columns only in df2.}
#'   \item{type_mismatches}{Data frame of columns whose class differs
#'     (columns: \code{column}, \code{type_df1}, \code{type_df2}), or
#'     \code{NULL} if none.}
#'   \item{missing_values}{Data frame summarising NA counts per column per
#'     dataset (columns: \code{column}, \code{na_df1}, \code{na_df2}), or
#'     \code{NULL} if no missingness.}
#'   \item{variable_comparison}{Output of [compare_variables()].}
#'   \item{observation_comparison}{Output of [compare_observations()], or a
#'     list with a \code{message} element when row counts differ.}
#'
#' @export
#' @examples
#' \donttest{
#' df1 <- data.frame(id = 1:3, val = c(10, 20, 30))
#' df2 <- data.frame(id = 1:3, val = c(10, 25, 30))
#' result <- compare_datasets(df1, df2)
#' result                              # tidy one-screen summary
#' result$observation_comparison       # drill into value diffs
#' }
compare_datasets <- function(df1, df2, tolerance = 0, vars = NULL) {
  # Validate tolerance
  if (!is.numeric(tolerance) || length(tolerance) != 1 || is.na(tolerance) || tolerance < 0 || is.infinite(tolerance)) {
    stop("tolerance must be a single non-negative finite number", call. = FALSE)
  }

  if (is.null(df1) || is.null(df2)) {
    stop("One or both datasets are null.")
  }

  # --- Dataset-level ---
  common_cols <- intersect(names(df1), names(df2))
  extra_df1   <- setdiff(names(df1), names(df2))
  extra_df2   <- setdiff(names(df2), names(df1))

  # Subset to requested variables if specified
  if (!is.null(vars)) {
    vars <- intersect(vars, common_cols)
    if (length(vars) == 0) {
      warning("None of the specified `vars` are common to both datasets", call. = FALSE)
    }
  } else {
    vars <- common_cols
  }

  # Type mismatches on common columns
  type_rows <- lapply(common_cols, function(col) {
    t1 <- class(df1[[col]])[1L]
    t2 <- class(df2[[col]])[1L]
    if (t1 != t2) {
      data.frame(column = col, type_df1 = t1, type_df2 = t2,
                 stringsAsFactors = FALSE)
    }
  })
  type_mismatches <- do.call(rbind, Filter(Negate(is.null), type_rows))

  # Missing-value summary
  na_rows <- lapply(common_cols, function(col) {
    n1 <- sum(is.na(df1[[col]]))
    n2 <- sum(is.na(df2[[col]]))
    if (n1 > 0 || n2 > 0) {
      data.frame(column = col, na_df1 = n1, na_df2 = n2,
                 stringsAsFactors = FALSE)
    }
  })
  missing_values <- do.call(rbind, Filter(Negate(is.null), na_rows))

  # --- Variable-level ---
  variable_comparison <- compare_variables(df1, df2)

  # --- Observation-level (graceful when rows differ) ---
  if (nrow(df1) == nrow(df2) && length(vars) > 0) {
    # For observation comparison, subset to requested vars
    obs_df1 <- df1[, vars, drop = FALSE]
    obs_df2 <- df2[, vars, drop = FALSE]
    observation_comparison <- compare_observations(obs_df1, obs_df2, tolerance = tolerance)
  } else {
    reason <- if (nrow(df1) != nrow(df2)) {
      sprintf("Row counts differ (%d vs %d); positional comparison skipped.",
              nrow(df1), nrow(df2))
    } else {
      "No variables to compare after filtering by `vars`; observation comparison skipped."
    }
    observation_comparison <- list(
      discrepancies = integer(0),
      details = list(),
      message = reason
    )
  }

  result <- list(
    nrow_df1               = nrow(df1),
    ncol_df1               = ncol(df1),
    nrow_df2               = nrow(df2),
    ncol_df2               = ncol(df2),
    common_columns         = common_cols,
    extra_in_df1           = extra_df1,
    extra_in_df2           = extra_df2,
    type_mismatches        = type_mismatches,
    missing_values         = missing_values,
    variable_comparison    = variable_comparison,
    observation_comparison = observation_comparison,
    tolerance              = tolerance
  )
  class(result) <- "dataset_comparison"
  result
}


#' Print Dataset Comparison Results
#'
#' @param x A \code{dataset_comparison} object from [compare_datasets()].
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.dataset_comparison <- function(x, ...) {
  # Determine overall verdict
  obs <- x$observation_comparison
  has_diffs <- FALSE
  total_diffs <- 0L
  if (!is.null(obs$discrepancies)) {
    total_diffs <- sum(obs$discrepancies, na.rm = TRUE)
    has_diffs <- total_diffs > 0
  }
  has_struct_diffs <- length(x$extra_in_df1) > 0 || length(x$extra_in_df2) > 0 ||
    (!is.null(x$type_mismatches) && nrow(x$type_mismatches) > 0)

  verdict <- if (has_diffs || has_struct_diffs) "DIFFERENCES FOUND" else "MATCH"

  cat("\n")
  cat(strrep("=", 50), "\n")
  cat("  clinCompare: Dataset Comparison\n")
  cat(strrep("=", 50), "\n\n")

  # Overall status
  cat(sprintf("  Status: %s\n\n", verdict))

  # Dimensions
  cat(sprintf("  Base dataset:    %d rows x %d columns\n", x$nrow_df1, x$ncol_df1))
  cat(sprintf("  Compare dataset: %d rows x %d columns\n", x$nrow_df2, x$ncol_df2))
  cat("\n")

  # Columns
  cat(sprintf("  Shared columns:       %d\n", length(x$common_columns)))
  if (length(x$extra_in_df1) > 0) {
    cat(sprintf("  Only in base:         %d (%s)\n",
                length(x$extra_in_df1), paste(x$extra_in_df1, collapse = ", ")))
  }
  if (length(x$extra_in_df2) > 0) {
    cat(sprintf("  Only in compare:      %d (%s)\n",
                length(x$extra_in_df2), paste(x$extra_in_df2, collapse = ", ")))
  }

  # Type mismatches
  if (!is.null(x$type_mismatches) && nrow(x$type_mismatches) > 0) {
    cat(sprintf("  Type mismatches:      %d\n", nrow(x$type_mismatches)))
    for (i in seq_len(nrow(x$type_mismatches))) {
      cat(sprintf("    - %s: %s (base) vs %s (compare)\n",
                  x$type_mismatches$column[i],
                  x$type_mismatches$type_df1[i],
                  x$type_mismatches$type_df2[i]))
    }
  }

  # Missing values
  if (!is.null(x$missing_values) && nrow(x$missing_values) > 0) {
    cat(sprintf("  Columns with NAs:     %d\n", nrow(x$missing_values)))
  }

  # Tolerance
  if (!is.null(x$tolerance) && x$tolerance > 0) {
    cat(sprintf("  Tolerance:            %g\n", x$tolerance))
  }

  cat("\n")

  # Observation differences
  .print_observation_diffs(obs, n = 30, n_total_obs = x$nrow_df1)

  cat(strrep("=", 50), "\n")
  invisible(x)
}


#' Print Observation-Level Differences (Internal Helper)
#'
#' @description
#' Shared helper used by both \code{print.dataset_comparison} and
#' \code{print.cdisc_comparison}. Prints a summary line, a per-variable
#' table, and up to \code{n} rows of the top variable's differing observations.
#'
#' @param obs Observation comparison list (with \code{discrepancies},
#'   \code{details}, and optionally \code{id_details} and \code{message}).
#' @param n Maximum number of differing rows to display (default 30).
#' @param id_details Optional named list of ID detail data frames
#'   (from key-based comparison).
#' @param n_total_obs Total number of observations (for percentage calculation).
#'
#' @return Called for side effects (prints to console). Returns NULL invisibly.
#' @keywords internal
.print_observation_diffs <- function(obs, n = 30, id_details = NULL, n_total_obs = NULL) {
  if (is.null(obs)) return(invisible(NULL))

  # If observation comparison was skipped, print the reason
  if (!is.null(obs$message)) {
    cat("  ", obs$message, "\n")
    return(invisible(NULL))
  }

  if (is.null(obs$discrepancies)) return(invisible(NULL))

  total <- sum(obs$discrepancies, na.rm = TRUE)
  cols_affected <- sum(obs$discrepancies > 0)
  n_compared <- length(obs$discrepancies)

  # Summary line
  cat(strrep("-", 50), "\n")
  cat("  Value Comparison\n")
  cat(strrep("-", 50), "\n")

  if (total == 0) {
    cat("  All values match across ", n_compared, " column(s).\n\n")
    return(invisible(NULL))
  }

  if (!is.null(n_total_obs) && n_total_obs > 0 && length(obs$details) > 0) {
    unique_rows <- unique(unlist(lapply(obs$details, function(d) {
      if (is.data.frame(d)) d$Row else integer(0)
    })))
    pct <- round(length(unique_rows) / n_total_obs * 100, 1)
    cat(sprintf("  %d difference(s) found in %d of %d column(s)\n",
                total, cols_affected, n_compared))
    cat(sprintf("  %d of %d row(s) affected (%.1f%%)\n\n",
                length(unique_rows), n_total_obs, pct))
  } else {
    cat(sprintf("  %d difference(s) found in %d of %d column(s)\n\n",
                total, cols_affected, n_compared))
  }

  if (length(obs$details) == 0) return(invisible(NULL))

  # Use id_details from obs itself if not passed separately
  if (is.null(id_details) && !is.null(obs$id_details)) {
    id_details <- obs$id_details
  }

  # Find variables with differences (sorted by count, descending)
  counts <- obs$discrepancies[obs$discrepancies > 0]
  counts <- sort(counts, decreasing = TRUE)

  # Per-variable summary table
  cat("  Per-Column Summary:\n")

  summary_rows <- list()
  for (var_name in names(counts)) {
    var_data <- obs$details[[var_name]]
    if (!is.data.frame(var_data)) next

    n_diffs <- counts[var_name]
    v1 <- var_data$Value_in_df1
    v2 <- var_data$Value_in_df2
    is_numeric <- is.numeric(v1) && is.numeric(v2)

    if (is_numeric) {
      abs_diffs <- abs(v1 - v2)
      max_diff <- max(abs_diffs, na.rm = TRUE)
      summary_rows[[var_name]] <- data.frame(
        Column = var_name,
        Type = "numeric",
        Differences = n_diffs,
        Largest_Diff = max_diff,
        stringsAsFactors = FALSE
      )
    } else {
      summary_rows[[var_name]] <- data.frame(
        Column = var_name,
        Type = "character",
        Differences = n_diffs,
        Largest_Diff = NA,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(summary_rows) > 0) {
    summary_df <- do.call(rbind, summary_rows)
    rownames(summary_df) <- NULL

    # Print formatted table
    cat(sprintf("  %-20s %-12s %12s %14s\n",
                "Column", "Type", "Differences", "Largest Diff"))
    cat(sprintf("  %s\n", strrep("-", 60)))

    for (i in seq_len(nrow(summary_df))) {
      row <- summary_df[i, ]
      diff_str <- if (is.na(row$Largest_Diff)) {
        sprintf("%14s", "-")
      } else {
        sprintf("%14.4g", row$Largest_Diff)
      }
      cat(sprintf("  %-20s %-12s %12d %s\n",
                  row$Column, row$Type, row$Differences, diff_str))
    }
    cat("\n")
  }

  # Show first variable's rows with cleaner column names
  first_var <- names(counts)[1L]
  diffs_df <- obs$details[[first_var]]
  if (is.null(diffs_df) || !is.data.frame(diffs_df) || nrow(diffs_df) == 0) {
    return(invisible(NULL))
  }

  show_n <- min(nrow(diffs_df), n)
  cat(sprintf("  Sample differences in '%s' (showing %d of %d):\n",
              first_var, show_n, nrow(diffs_df)))

  # Build display table with user-friendly column names
  display <- diffs_df[seq_len(show_n), , drop = FALSE]

  # Rename columns for readability
  names(display)[names(display) == "Value_in_df1"] <- "Base"
  names(display)[names(display) == "Value_in_df2"] <- "Compare"

  # Add Diff column for numeric variables
  v1_all <- diffs_df$Value_in_df1
  v2_all <- diffs_df$Value_in_df2
  is_num <- is.numeric(v1_all) && is.numeric(v2_all)

  if (is_num) {
    abs_diff <- v1_all - v2_all
    display$Diff <- round(abs_diff[seq_len(show_n)], 4)
  }

  # Prepend ID columns if available (key-based matching)
  if (!is.null(id_details) && first_var %in% names(id_details)) {
    id_df <- id_details[[first_var]]
    if (is.data.frame(id_df) && nrow(id_df) >= show_n) {
      display <- cbind(id_df[seq_len(show_n), , drop = FALSE],
                        display[, setdiff(names(display), "Row"), drop = FALSE])
    }
  }

  # Print as aligned table
  print(display, row.names = FALSE, right = FALSE)

  if (nrow(diffs_df) > n) {
    cat(sprintf("  ... %d more row(s). Access via $observation_comparison$details$%s\n",
                nrow(diffs_df) - n, first_var))
  }

  cat("\n")
  invisible(NULL)
}


#' Extract All Differences as a Unified Data Frame
#'
#' @description
#' Converts per-variable observation differences into a single long-format
#' data frame suitable for filtering with dplyr, writing to CSV, or
#' programmatic analysis. This is the R equivalent of SAS PROC COMPARE's
#' \code{OUT=} dataset with \code{_TYPE_} and \code{_DIF_} variables.
#'
#' Accepts output from [compare_datasets()], [cdisc_compare()], or any list
#' containing an \code{observation_comparison} element with the standard
#' \code{discrepancies} / \code{details} / \code{id_details} structure.
#'
#' @param comparison_results A \code{dataset_comparison} or
#'   \code{cdisc_comparison} object, or any list with an
#'   \code{observation_comparison} element.
#'
#' @return A data frame with one row per differing cell. Columns:
#'   \describe{
#'     \item{Variable}{Character: column name where the difference was found.}
#'     \item{Row}{Integer: row index in df1 (positional matching).}
#'     \item{Base}{The value in df1 (base dataset).}
#'     \item{Compare}{The value in df2 (compare dataset).}
#'     \item{Diff}{Numeric: Base - Compare (NA for character columns).}
#'     \item{PctDiff}{Numeric: absolute percentage difference relative to
#'       Base (NA when Base is 0 or column is character).}
#'   }
#'   When key-based matching was used (id_vars), the ID columns are
#'   prepended to the left of the data frame.
#'
#'   Returns an empty data frame with the expected columns when no
#'   differences exist or observation comparison was skipped.
#'
#' @export
#' @examples
#' \donttest{
#' df1 <- data.frame(id = 1:3, value = c(10, 20, 30), name = c("A", "B", "C"))
#' df2 <- data.frame(id = 1:3, value = c(10, 25, 30), name = c("A", "B", "D"))
#' result <- compare_datasets(df1, df2)
#' diffs <- get_all_differences(result)
#' head(diffs)
#' }
get_all_differences <- function(comparison_results) {
  obs <- comparison_results$observation_comparison

  # Empty scaffold
  empty_df <- data.frame(
    Variable = character(0),
    Row      = integer(0),
    Base     = character(0),
    Compare  = character(0),
    Diff     = numeric(0),
    PctDiff  = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(obs)) return(empty_df)
  if (!is.null(obs$message) || !is.null(obs$status)) return(empty_df)
  if (is.null(obs$details) || length(obs$details) == 0) return(empty_df)

  id_details <- obs$id_details
  has_id_cols <- !is.null(id_details) && length(id_details) > 0
  all_rows <- list()

  for (var_name in names(obs$details)) {
    d <- obs$details[[var_name]]
    if (!is.data.frame(d) || nrow(d) == 0) next

    is_num <- is.numeric(d$Value_in_df1) && is.numeric(d$Value_in_df2)

    row_df <- data.frame(
      Variable = rep(var_name, nrow(d)),
      Row      = d$Row,
      Base     = as.character(d$Value_in_df1),
      Compare  = as.character(d$Value_in_df2),
      stringsAsFactors = FALSE
    )

    if (is_num) {
      row_df$Diff <- d$Value_in_df1 - d$Value_in_df2
      row_df$PctDiff <- ifelse(d$Value_in_df1 != 0,
        abs((d$Value_in_df1 - d$Value_in_df2) / d$Value_in_df1) * 100,
        NA_real_)
    } else {
      row_df$Diff    <- NA_real_
      row_df$PctDiff <- NA_real_
    }

    # Prepend ID columns if available (key-based matching)
    # and drop the meaningless Row column since keys identify the record
    if (has_id_cols && var_name %in% names(id_details)) {
      id_df <- id_details[[var_name]]
      if (is.data.frame(id_df) && nrow(id_df) == nrow(row_df)) {
        row_df <- cbind(id_df, row_df[, setdiff(names(row_df), "Row"), drop = FALSE])
      }
    }

    all_rows[[length(all_rows) + 1L]] <- row_df
  }

  if (length(all_rows) == 0) return(empty_df)

  # Combine -- rbind with fill for ID columns that may differ across variables
  all_cols <- unique(unlist(lapply(all_rows, names)))
  unified <- do.call(rbind, lapply(all_rows, function(r) {
    missing_cols <- setdiff(all_cols, names(r))
    for (mc in missing_cols) r[[mc]] <- NA
    r[, all_cols, drop = FALSE]
  }))

  rownames(unified) <- NULL
  unified
}
