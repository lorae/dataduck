#' Calculate Weighted Mean, Count, and Percentages for Groups
#'
#' This function calculates the weighted mean, sum of weights, and count of observations
#' for the specified value and weight columns, grouped by the specified columns. 
#' Additionally, it can calculate percentages within specified groups. 
#' The user can control whether to calculate the weighted mean and percentages via 
#' the `mean` and `percent` arguments.
#'
#' @param data A data frame or a database connection object containing the data to be aggregated.
#' @param weight A string specifying the name of the column containing the weights.
#' @param group_by A character vector of column names to group by for the main aggregations.
#' @param mean Logical, whether to calculate the weighted mean. Defaults to `FALSE`.
#' @param value A string specifying the name of the column containing the values to be averaged. 
#'   Required if `mean = TRUE`.
#' @param percent Logical, whether to calculate percentages. Defaults to `FALSE`.
#' @param percent_group_by A character vector specifying the column names to group by for the percentage calculation. 
#'   All elements of `percent_group_by` must be included in `group_by`. Required if `percent = TRUE`.
#'
#' @return A tibble or database connection object containing the group-by columns, sum of weights, 
#'   and optionally the count of observations, weighted mean, and percentages for each group.
#'
#' @export
crosstab <- function(
    data, 
    weight, 
    group_by, 
    value = NULL,  # Optional: for calculating weighted means
    percent_group_by = NULL  # Optional: for calculating percentages
) {
  # Check if we need to calculate weighted mean or percentages
  calculate_mean <- !is.null(value)
  calculate_percent <- !is.null(percent_group_by)
  
  # Message to track the calculation type
  if (calculate_mean) {
    message("Since `value` column has been specified, weighted means are being calculated.")
  }
  if (calculate_percent) {
    if (!all(percent_group_by %in% group_by)) {
      stop("All elements of 'percent_group_by' must be included in 'group_by'.")
    }
    message("Since `percent_group_by` column has been specified, percentages are being calculated.")
  }
  
  # Base summarization: Always calculate weighted_count and count
  if (calculate_mean) {
    result <- data |>
      group_by(!!!syms(group_by)) |>
      summarize(
        weighted_count = sum(!!sym(weight), na.rm = TRUE),
        count = n(),
        total_value_weighted = sum(!!sym(value) * !!sym(weight), na.rm = TRUE),
        .groups = "drop"
        ) 
    } else {
      result <- data |>
        group_by(!!!syms(group_by)) |>
        summarize(
          weighted_count = sum(!!sym(weight), na.rm = TRUE),
          count = n(),
          .groups = "drop"
        )
    }
  
  # Conditionally add the weighted mean
  if (calculate_mean) {
    result <- result |>
      mutate(
        weighted_mean = total_value_weighted / weighted_count
      ) |>
      select(-total_value_weighted)  # Remove the intermediate result
  }
  
  # Conditionally add the percentage within the explicitly defined group
  if (calculate_percent) {
    result <- result |>
      group_by(!!!syms(percent_group_by)) |>
      mutate(
        percent = 100 * (weighted_count / sum(weighted_count, na.rm = TRUE))
      ) |>
      ungroup()  # Ungroup after calculating percentage
  }
  
  # Return the final result
  result |>
    select(!!!syms(group_by), everything())
}








#' Calculate Weighted Mean
#'
#' This function calculates the weighted mean, sum of weights, and count of observations
#' for a given value column, listed for every unique cross-combination of the specified 
#' columns. The function can handle both data frames and database pointers.
#'
#' @param data A data frame or a database connection object. The data containing the 
#'   value, weight, and grouping columns.
#' @param value_column A string specifying the name of the column containing the values
#'   to be averaged.
#' @param weight_column A string specifying the name of the column containing the weights.
#' @param group_by_columns A character vector of column names to group by.
#'
#' @return A tibble or database connection object containing the group-by columns, count of observations, sum of weights,
#'   and the calculated weighted mean for each group.
#'
#' @export
crosstab_mean <- function(
    data, 
    value_column, 
    weight_column, 
    group_by_columns) {
  
  # Use quasiquotation to handle column names passed as strings
  value_col <- sym(value_column)
  weight_col <- sym(weight_column)
  
  # Dynamically reference grouping columns
  group_by_cols <- syms(group_by_columns)
  
  # Calculate the weighted mean, sum of weights, and count of observations
  data %>%
    group_by(!!!group_by_cols) |>
    summarize(
      total_value_weighted = sum(!!value_col * !!weight_col, na.rm = TRUE),
      sum_weights = sum(!!weight_col, na.rm = TRUE),
      count = n()
      # Add weighted variance. Potential resources:
      # https://stats.stackexchange.com/questions/51442/weighted-variance-one-more-time
      # https://influentialpoints.com/Training/two-sample_t-test-principles-properties-assumptions.htm
    ) |>
    mutate(weighted_mean = total_value_weighted / sum_weights) |>
    select(!!!group_by_cols, count, sum_weights, weighted_mean)
}

#' Calculate Count and Weighted Sum for Groups
#'
#' This function calculates the count of observations and the sum of weights for 
#' every unique cross-combination of the specified grouping columns. It handles 
#' both data frames and database pointers, allowing for flexible input types.
#'
#' @param data A data frame or database connection object. The data containing the weight 
#'   and grouping columns.
#' @param weight_column A string specifying the name of the column containing the weights.
#' @param group_by_columns A character vector specifying the column names to group by.
#'
#' @return A tibble or database connection object with the grouped data, containing the group-by columns, 
#'   the count of observations, and the sum of weights for each group.
#'
#' @export
crosstab_count <- function(
    data, 
    weight_column, 
    group_by_columns) {
  
  # Use quasiquotation to handle column names passed as strings
  weight_col <- sym(weight_column)
  
  # Dynamically reference grouping columns
  group_by_cols <- syms(group_by_columns)
  
  # Calculate the weighted mean, sum of weights, and count of observations
  data %>%
    group_by(!!!group_by_cols) |>
    summarize(
      sum_weights = sum(!!weight_col, na.rm = TRUE),
      count = n()
    ) |>
    select(!!!group_by_cols, count, sum_weights)
}


#' Calculate the Difference in Means Between Two Datasets
#'
#' This function calculates the difference in means between two datasets (such as data 
#' from different years), along a specified column. It assumes that the datasets can be 
#' matched by a common column.
#'
#' @param data2000 A data frame or database object representing the first dataset, such as data from the year 2000.
#' @param data2020 A data frame or database object representing the second dataset, such as data from the year 2020.
#' @param match_by A string specifying the column name used to match the two datasets.
#' @param diff_by A string specifying the column along which to calculate differences (e.g., a measure like income or household size).
#' @param keep A character vector of additional column names to keep in the output, along with the difference column. Defaults to NULL.
#'
#' @return A tibble containing the matching column, the calculated difference for `diff_by`,
#'   and any additional columns specified in `keep`.
#'
#' @export
difference_means <- function(
    data2000, 
    data2020,
    match_by,   # The column to match the data from both years along
    diff_by,    # The column along which to calculate differences
    keep = NULL # The columns to keep in addition to the diff_by column
) {
  # TODO: genericize this function further to difference any two data sources, without
  # enforcing the _2000 and _2020 suffixes.
  suffixed_cols <- c(diff_by, keep)
  
  # Rename columns with their year suffixes; Keep only `match_by` and suffixed columns
  data2000 <- data2000 |>
    select({{ match_by }}, all_of(suffixed_cols)) |>
    rename_with(~ paste0(., "_2000"), all_of(suffixed_cols))
  
  # Rename columns with their year suffixes; Keep only `match_by` and suffixed columns
  data2020 <- data2020 |>
    select({{ match_by }}, all_of(suffixed_cols)) |>
    rename_with(~ paste0(., "_2020"), all_of(suffixed_cols))
  
  # Merge data2000 and data2020 by the matching column
  diff <- data2000 |>
    inner_join(data2020, by = as_string(ensym(match_by))) |>
    mutate(
      diff = !!sym(paste0(diff_by, "_2020")) - !!sym(paste0(diff_by, "_2000"))
    ) |>
    # Arrange column order
    select(
      {{ match_by }},
      diff,
      everything()
    )
  
  return(diff)
}