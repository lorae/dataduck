#' Calculate Weighted and Unweighted Counts for Groups
#'
#' This function calculates the weighted count (sum of weights) and the unweighted count
#' of observations for the specified weight column, grouped by the specified columns.
#' Optionally, it can include all combinations of the grouping variables, even if some combinations
#' do not exist in the data, setting the counts to zero for those combinations.
#' Info on replicate weight standard errors: https://usa.ipums.org/usa/repweight.shtml
#'
#' @param data A data frame or a database connection object containing the data to be aggregated.
#' @param weight A string specifying the name of the column containing the weights.
#' @param group_by A character vector of column names to group by.
#' @param every_combo Logical, whether to include all combinations of the grouping variables,
#'   setting counts to zero for combinations not present in the data. Defaults to `FALSE`.
#'
#' @return A tibble or database connection object containing the group-by columns, weighted count,
#'   unweighted count, and replicate weight standard errors for each group.
#'
#' @export
crosstab_count <- function(
    data,
    weight,
    group_by,
    every_combo = FALSE
) {
  
  # Calculate base results using full-sample weight and unweighted count
  result <- data |>
    group_by(across(all_of(group_by))) |>
    summarize(
      weighted_count = sum(!!sym(weight), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  # Conditionally include all combinations of grouping variables
  if (every_combo) {
    result <- result |>
      complete(!!!syms(group_by), fill = list(weighted_count = 0, count = 0, standard_error = NA))
  }
  
  return(result)
}


#' Calculate Weighted Mean for Groups
#'
#' This function calculates the weighted mean for the specified value and weight columns, grouped by the specified columns.
#'
#' @param data A data frame or a database connection object containing the data to be aggregated.
#' @param value A string specifying the name of the column containing the values to be averaged.
#' @param weight A string specifying the name of the column containing the weights.
#' @param group_by A character vector of column names to group by.
#'
#' @return A tibble or database connection object containing the group-by columns and weighted mean for each group.
#'
#' @export
crosstab_mean <- function(
    data, 
    value, 
    weight, 
    group_by,
    every_combo = FALSE
) {
  result <- data |>
    group_by(!!!syms(group_by)) |>
    summarize(
      # Weighted sumproducts
      weighted_sumprod = sum(!!sym(value) * !!sym(weight), na.rm = TRUE),
      # Weighted counts
      weighted_count = sum(!!sym(weight), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) |>
    collect() # must collect in order for following operations to work
  
  result <- result |>
    mutate(weighted_mean = weighted_sumprod / weighted_count) |>
    select(-weighted_sumprod)
  
  # Conditionally include all combinations of grouping variables
  if (every_combo) {
    result <- result |>
      complete(!!!syms(group_by), fill = list(weighted_count = 0, count = 0))
  }
  
  return(result)
}

#' Calculate Percentages Within Groups
#'
#' This function calculates percentages of weighted counts within specified groups.
#'
#' @param data A data frame or a database connection object containing the data to be aggregated.
#' @param weight A string specifying the name of the column containing the weights.
#' @param group_by A character vector of column names to group by for the main counts.
#' @param percent_group_by A character vector of column names to group by for the percentage calculation.
#'   All elements of `percent_group_by` must be included in `group_by`.
#'
#' @return A tibble or database connection object containing the group-by columns and percentages for each group.
#'
#' @export
crosstab_percent <- function(
    data, 
    weight, 
    group_by, 
    percent_group_by,
    every_combo = FALSE
) {
  if (!all(percent_group_by %in% group_by)) {
    stop("All elements of 'percent_group_by' must be included in 'group_by'.")
  }
  
  result <- data |>
    group_by(!!!syms(group_by)) |>
    summarize(
      weighted_count = sum(!!sym(weight), na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  # Conditionally include all combinations of grouping variables
  if (every_combo) {
    # Use complete to fill in missing combinations
    result <- result |>
      complete(!!!syms(group_by), fill = list(weighted_count = 0, count = 0))
  }  
  
  result <- result |>
    collect()
  
  # Now add the percent column
  result <- result |> 
    group_by(!!!syms(percent_group_by)) |>
    mutate(
      # Main percentage
      percent = 100 * (weighted_count / sum(weighted_count, na.rm = TRUE)),
    ) |>
    ungroup()
  
  return(result)
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