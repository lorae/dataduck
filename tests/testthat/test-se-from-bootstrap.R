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
input_data <- tibble(
  per_id = c(1, 2, 3, 4, 5),
  sex = c(1, 0, 1, 1, 0),
  hhsize = c(2, 3, 2, 1, 1),
  weight = c(10, 12, 15, 30, 20),
  repwt1 = c(11, 13, 16, 28, 22),
  repwt2 = c(8, 8, 16, 25, 22),
  repwt3 = c(2, 4, 10, 14, 13),
  repwt4 = c(18, 17, 11, 25, 15)
)

# Initialize two test functions
hhsize_by_sex <- function(
    data,
    weight, # string name of weight column in `data`
    hhsize # string name of hhsize column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      weighted_mean = sum(.data[[hhsize]] * .data[[weight]], na.rm = TRUE)/sum(.data[[weight]], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}

count_by_sex <- function(
    data,
    weight # string name of weight column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      count = n(),
      weighted_count = sum(.data[[weight]]),
      .groups = "drop"
    )
  
  return(result)
}

# Produce two test bootstrap_replicates input objects
input_bootstrap_count <- bootstrap_replicates(
  data = input_data,
  f = count_by_sex,
  wt_col = "weight",
  repwt_cols = paste0("repwt", 1:4),
  id_cols = "sex"
)

input_bootstrap_hhsize <- bootstrap_replicates(
  data = input_data,
  f = hhsize_by_sex,
  wt_col = "weight",
  repwt_cols = paste0("repwt", 1:4),
  hhsize = "hhsize",
  id_cols = "sex"
)

# Expected results
expected_count <- tibble(
  sex = c(0, 1),
  count = c(2, 3),
  weighted_count = c(32, 55),
  se_weighted_count = c(15.42725, 29.63106)
)

expected_hhsize <- tibble(
  sex = c(0, 1),
  weighted_mean = c(1.75, 1.454545),
  se_weighted_mean = c(0.47193501, 0.09704985)
)

test_that("se_from_bootstrap produces correct results on count_by_sex", {
  output_count <- se_from_bootstrap(
    bootstrap = input_bootstrap_count,
    constant = 1,
    se_cols = c("weighted_count")
  )
  
  # Round decimals to avoid floating point mismatch
  output_count <- output_count |>
    mutate(se_weighted_count = round(se_weighted_count, 5))
  expected_count <- expected_count |>
    mutate(se_weighted_count = round(se_weighted_count, 5))
  
  expect_equal(output_count, expected_count)
})

test_that("se_from_bootstrap produces correct results on hhsize_by_sex", {
  output_hhsize <- se_from_bootstrap(
    bootstrap = input_bootstrap_hhsize,
    constant = 1,
    se_cols = c("weighted_mean")
  )
  
  # Round decimals to avoid floating point mismatch
  output_hhsize <- output_hhsize |>
    mutate(
      se_weighted_mean = round(se_weighted_mean, 4),
      weighted_mean = round(weighted_mean, 4)
      )
  expected_hhsize <- expected_hhsize |>
    mutate(
      se_weighted_mean = round(se_weighted_mean, 4),
      weighted_mean = round(weighted_mean, 4)
      )
  
  expect_equal(output_hhsize, expected_hhsize)
})
