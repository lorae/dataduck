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

# Two example functions that we'll want to get standard errors for. For these functions
# to work in the se() function, they must have an explicit argument for weight
hhsize_by_sex <- function(
    data,
    wt,
    hhsize
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
    wt
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





calculate_standard_errors <- function(
    data, 
    f, # function producing new columns for standard errors. Must have an argument
    # that is called "wt"
    wt_col = "wt", # The main weight
    repwt_cols = paste0("repwt", 1:4), # The replication weight column names
    result_cols, # The names of columns for which standard errors will be calculated
    ... # Any additional arguments needed for function f
    ) {
  
  # Main estimate computed using primary weights
  main_estimate <- f(data, wt = wt_col, ...)
  
  # # Compute replicate estimates
  replicate_estimates <- map(repwt_cols, ~ f(data, wt = .x, ...))
  
  # # Number of replicates
  # R <- length(rep_weight_cols)
  # 
  # # Calculate deviations
  # deviations <- replicate_estimates - main_estimate
  # 
  # # Variance calculation (modify formula based on replication method)
  # variance <- sum(deviations^2) / (R * (R - 1))
  # 
  # # Standard error
  # standard_error <- sqrt(variance)
  
  # Return results
  list(
    main_estimate = main_estimate,
    replicate_estimates = replicate_estimates#,
    #standard_error = standard_error,
  )
}


calculate_standard_errors(
  data = input, 
  f = hhsize_by_sex, 
  wt_col = "wt", 
  repwt_cols = paste0("repwt", 1:4), 
  hhsize = "hhsize"
  )


