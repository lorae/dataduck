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