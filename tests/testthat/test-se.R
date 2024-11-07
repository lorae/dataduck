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

# This is the result of the function. The bootstrapped replicates are not what
# I expected
bootstrap_replicates(
  data = input, 
  f = hhsize_by_sex, 
  wt_col = "wt", 
  repwt_cols = repwt_vector, 
  hhsize = "hhsize"
)

# This is what the bootstrapped replicates ~should~ be. 
map(repwt_vector, ~ hhsize_by_sex(input, wt = .x, hhsize = "hhsize"))

calculate_standard_errors <- function(
    bootstrap, # The output of a bootstrap_replicates() function
    se_col # String name of column to produce standard error on
    ) {
  # Create a list of each of the bootstrapped data frames minus the main result
  map(bootstrap$replicate_estimates, function(.x) {
    .x |>
      mutate(diff_sq = (.x[[se_col]] - bootstrap$main_estimate[[se_col]])^2)
    })
}
