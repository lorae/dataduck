#' Bootstrap Results Using Replicate Weights
#'
#' Calculates results of a target function `f()` using a specified weight column,
#' and also calculates results by substituting each of the specified `repwt_cols` 
#' for the `wt` argument within `f()`. This is useful for calculating replicate 
#' estimates required for standard error computation using replicate weights.
#'
#' @param data A tibble or data frame containing the data to be analyzed.
#' @param f A function that produces new columns or summary statistics. 
#'   The function `f` must have an argument named `wt` to specify the weight column.
#' @param wt_col A string indicating the column name to be used as the main weight.
#'   Defaults to `"wt"`.
#' @param repwt_cols A vector of strings indicating the names of replicate weight 
#'   columns in `data`. Defaults to `paste0("repwt", 1:80)`.
#' @param ... Additional arguments passed to the function `f`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{main_estimate}{The result of applying `f()` with the main weight column.}
#'   \item{replicate_estimates}{A list of results from applying `f()` with each 
#'   replicate weight column.}
#' }
bootstrap_replicates <- function(
    data, 
    f, # function producing new columns for standard errors. Must have an argument
    # that is called "wt"
    wt_col = "wt", # string name of weight column in `data`
    repwt_cols = paste0("repwt", 1:4), # Vector of strings of replicate weight columns
    # in `data`
    ... # Any additional arguments needed for function f
) {
  
  extra_args <- list(...)
  is.extra_args <- (length(extra_args) > 0)
  
  # Check if additional arguments are passed
  if (is.extra_args) {
    main_estimate <- f(data, wt = wt_col, ...)
    replicate_estimates <- map(repwt_cols, function(.x) f(data, wt = .x, ...))
  } else {
    main_estimate <- f(data, wt = wt_col)
    replicate_estimates <- map(repwt_cols, function(.x) f(data, wt = .x))
  }
  
  # Return results
  list(
    main_estimate = main_estimate,
    replicate_estimates = replicate_estimates
  )
}

  
#' Calculate Standard Errors Using Bootstrapped Replicate Results
#'
#' Computes standard errors across specified columns using the output of 
#' `bootstrap_replicates()`. The function assumes that all input tibbles 
#' from bootstrap replicates are ordered identically. If they are not, 
#' (which might happen after a duckdb operation) this will introduce issues.
#' 
#' @param bootstrap The output from the `bootstrap_replicates()` function, 
#'   containing the main estimate and replicate estimates.
#' @param constant A constant used in the standard error calculation. For IPUMS data, 
#'   this is typically `4/80`. See \url{https://usa.ipums.org/usa/repwt.shtml} for details.
#' @param se_cols A vector of string column names for which standard errors are to be computed.
#'
#' @return A tibble containing the original main estimate columns along with new 
#'   columns for the calculated standard errors. The standard error columns will 
#'   have the prefix `"se_"`.
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
  
  # Sum the squared differences to calculate the standard errors
  se <- sq_diffs |>
    reduce(~ .x + .y) |>
    # Then multiply these results by the constant and take their square root.
    # Rename to `se_sq_diff_{}`
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