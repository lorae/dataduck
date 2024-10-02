weighted_mean <- function(
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




# TODO: genericize this function further to difference any two data sources, without
# enforcing the _2000 and _2020 suffixes.
difference_means <- function(
    data2000, 
    data2020,
    match_by,   # The column to match the data from both years along
    diff_by,    # The column along which to calculate differences
    keep = NULL # The columns to keep in addition to the diff_by column
) {
  
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