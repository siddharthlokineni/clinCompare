#' Converts the data types of specified variables in a dataset.
#'
#' @description
#' Converts columns in a data frame to specified types based on a named list
#' mapping column names to target types. Supports conversion to numeric, character,
#' factor, integer, logical, and other R data types.
#'
#' @param df A data frame containing the variables to be converted.
#' @param conversions A named list where names correspond to variable names
#' in the dataset, and values are the desired data types (e.g., 'numeric', 'factor').
#' @return A data frame with converted variable types.
#' @importFrom methods as
#' @keywords internal

convert_data_types <- function(df, conversions) {
  # Support a single string to convert ALL columns to one type
  if (is.character(conversions) && length(conversions) == 1L && is.null(names(conversions))) {
    target_type <- conversions
    for (var_name in names(df)) {
      current_type <- class(df[[var_name]])[1L]
      if (current_type != target_type) {
        df[[var_name]] <- tryCatch(
          {
            switch(target_type,
              "numeric"   = {
                result <- suppressWarnings(as.numeric(as.character(df[[var_name]])))
                if (any(is.na(result) & !is.na(df[[var_name]]))) {
                  warning("NAs introduced by coercion in column '", var_name, "'")
                }
                result
              },
              "character" = as.character(df[[var_name]]),
              "factor"    = as.factor(df[[var_name]]),
              "integer"   = as.integer(df[[var_name]]),
              "logical"   = as.logical(df[[var_name]]),
              as(df[[var_name]], target_type)
            )
          },
          error = function(e) {
            warning("Could not convert column '", var_name, "' to ", target_type, ": ", e$message)
            df[[var_name]]
          }
        )
      }
    }
    return(df)
  }

  # Named list: per-column conversion
  for (var_name in names(conversions)) {
    target_type <- conversions[[var_name]]

    # Check if the variable exists in the data frame
    if (var_name %in% names(df)) {
      current_type <- class(df[[var_name]])[1L]

      # Only convert if the current type doesn't match the target type
      if (current_type != target_type) {
        df[[var_name]] <- tryCatch(
          as(df[[var_name]], target_type),
          error = function(e) {
            warning("Could not convert column '", var_name, "' to ", target_type, ": ", e$message)
            df[[var_name]]
          }
        )
      }
    }
  }
  df
}
