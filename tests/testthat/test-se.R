library(dplyr)
library(purrr)

# Input data
input <- tibble(
  per_id = c(1, 2, 3, 4, 5),
  sex = c(1, 0, 1, 1, 0),
  hhsize = c(2, 3, 2, 1, 1),
  wt = c(10, 12, 15, 30, 20),
  repwt1 = c(11, 13, 16, 28, 22),
  repwt2 = c(8, 8, 16, 25, 22),
  repwt3 = c(2, 4, 10, 14, 13),
  repwt4 = c(18, 17, 11, 25, 15)
)

# Two example functions that I want to bootstrap in `bootstrap_replicates()`.
# Note that they must they must have an explicit argument for weight (`wt`) to work
# within `bootstrap_replicates()`
hhsize_by_sex <- function(
    data,
    wt, # string name of weight column in `data`
    hhsize # string name of hhsize column in `data`
    ) {
  result <- data |>
    group_by(sex) |>
    summarize(
      weighted_mean = sum(.data[[hhsize]] * .data[[wt]], na.rm = TRUE)/sum(.data[[wt]], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}

count_by_sex <- function(
    data,
    wt # string name of weight column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      count = n(),
      weighted_count = sum(.data[[wt]]),
      .groups = "drop"
    )
  
  return(result)
}

# Test out the functions
hhsize_by_sex(input, wt = "wt", hhsize = "hhsize")
count_by_sex(input, wt = "wt")


# Calculates results of a target function `f()` and also calculates results of the 
# target function subbing each of the specified `repwt_col` arguments for the 
# `wt` argument within `f()`
bootstrap_replicates <- function(
    data, 
    f, # function producing new columns for standard errors. Must have an argument
    # that is called "wt"
    wt_col = "wt", # string name of weight column in `data`
    repwt_cols = paste0("repwt", 1:4), # Vector of strings of replicate weight columns
    # in `data`
    ... # Any additional arguments needed for function f
    ) {
  main_estimate <- f(data, wt = wt_col, ...)
  replicate_estimates <- map(repwt_cols, function(.x) f(data, wt = .x, ...))
  
  # Return results
  list(
    main_estimate = main_estimate,
    replicate_estimates = replicate_estimates
  )
}


# Initialize the names of the replicate weight columns
repwt_vector <- paste0("repwt", 1:4)

# Apply bootstrap replicates to the data using the two functions
x <- bootstrap_replicates(
  data = input, 
  f = hhsize_by_sex, 
  wt_col = "wt", 
  repwt_cols = repwt_vector, 
  hhsize = "hhsize"
)

y <- bootstrap_replicates(
  data = input, 
  f = count_by_sex, 
  wt_col = "wt", 
  repwt_cols = repwt_vector
)

# Note: this function assumes that all input tibbles from bootstrap are ordered
# identically. If they're not (which might happen after a duckdb operation) then
# this will introduce problems.
calculate_standard_errors <- function(
    bootstrap, # The output of a bootstrap_replicates() function
    constant = 4/80, # See https://usa.ipums.org/usa/repwt.shtml for more info
    se_cols # Vector of string column names to produce standard error on
    ) {
  # Create a list of data frames containing sq_diff columns for each se_col
  sq_diffs <- map(bootstrap$replicate_estimates, function(.x) {
    # Calculate squared differences for each se_col
    .x |>
      mutate(across(all_of(se_cols), 
                    ~ (. - bootstrap$main_estimate[[cur_column()]])^2, 
                    .names = "sq_diff_{.col}"))
  })
  
  se <- sq_diffs |>
    reduce(~ .x + .y) |>
    mutate(across(starts_with("sq_diff_"), 
                  ~ sqrt(. * constant), 
                  .names = "se_{.col}")) |>
    # Rename columns that start with "se_sq_diff_" to start with "se_"
    rename_with(~ sub("^se_sq_diff_", "se_", .), starts_with("se_sq_diff_")) |>
    # Keep only the standard error columns
    select(starts_with("se_"))
  
  output <- bootstrap$main_estimate |>
    bind_cols(se)

  return(output)
}

calculate_standard_errors(x, se_cols = "weighted_mean")

calculate_standard_errors(y, se_cols = c("weighted_count", "count"))
calculate_standard_errors(y, se_cols = c("weighted_count"))
calculate_standard_errors(y, se_cols = "weighted_count")
