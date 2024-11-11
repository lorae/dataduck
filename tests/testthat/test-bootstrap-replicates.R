library("testthat")
library("dplyr")
library("purrr")
library("rprojroot")

# Set working directory to project root
root <- find_root(is_rstudio_project)
setwd(root)

# Load the dataduck package
devtools::load_all("../dataduck")

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

# Initialize two test functions to run using bootstrap_replicates
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

test_that("bootstrap_replicates produces expected main and bootstrapped results on hhsize_by_sex", {
  output_hhsize <- bootstrap_replicates(
    data = input,
    f = hhsize_by_sex,
    wt_col = "wt",
    repwt_cols = paste0("repwt", 1:4),
    hhsize = "hhsize"
    )
  
  # Compare results
  expect_equal(
    output_hhsize$main_estimate, 
    hhsize_by_sex(input, wt = "wt", hhsize = "hhsize")
  )
  expect_equal(
    output_hhsize$replicate_estimates[[1]], 
    hhsize_by_sex(input, wt = "repwt1", hhsize = "hhsize")
  )
  expect_equal(
    output_hhsize$replicate_estimates[[2]], 
    hhsize_by_sex(input, wt = "repwt2", hhsize = "hhsize")
  )
  expect_equal(
    output_hhsize$replicate_estimates[[3]], 
    hhsize_by_sex(input, wt = "repwt3", hhsize = "hhsize")
  )
  expect_equal(
    output_hhsize$replicate_estimates[[4]], 
    hhsize_by_sex(input, wt = "repwt4", hhsize = "hhsize")
  )
})

test_that("bootstrap_replicates produces expected main and bootstrapped results on count_by_sex", {
  output_count <- bootstrap_replicates(
    data = input,
    f = count_by_sex,
    wt_col = "wt",
    repwt_cols = paste0("repwt", 1:4)
  )
  
  # Compare results for count_by_sex
  expect_equal(
    output_count$main_estimate, 
    count_by_sex(input, wt = "wt")
  )
  expect_equal(
    output_count$replicate_estimates[[1]], 
    count_by_sex(input, wt = "repwt1")
  )
  expect_equal(
    output_count$replicate_estimates[[2]], 
    count_by_sex(input, wt = "repwt2")
  )
  expect_equal(
    output_count$replicate_estimates[[3]], 
    count_by_sex(input, wt = "repwt3")
  )
  expect_equal(
    output_count$replicate_estimates[[4]], 
    count_by_sex(input, wt = "repwt4")
  )
})
