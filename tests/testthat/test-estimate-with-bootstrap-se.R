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
  wt = c(10, 12, 15, 30, 20),
  repwt1 = c(11, 13, 16, 28, 22),
  repwt2 = c(8, 8, 16, 25, 22),
  repwt3 = c(2, 4, 10, 14, 13),
  repwt4 = c(18, 17, 11, 25, 15)
)

# Define the functions
hhsize_by_sex <- function(
    data,
    wt,      # String name of weight column in `data`
    hhsize   # String name of hhsize column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      weighted_mean = sum(.data[[hhsize]] * .data[[wt]], na.rm = TRUE) / sum(.data[[wt]], na.rm = TRUE),
      .groups = "drop"
    )
  return(result)
}

count_by_sex <- function(
    data,
    wt    # String name of weight column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      count = n(),
      weighted_count = sum(.data[[wt]], na.rm = TRUE),
      .groups = "drop"
    )
  return(result)
}

# Expected results
expected_hhsize <- tibble(
  sex = c(0, 1),
  weighted_mean = c(1.75, 1.4545),
  se_weighted_mean = c(0.47193501, 0.09704985)
)

expected_count <- tibble(
  sex = c(0, 1),
  count = c(2, 3),
  weighted_count = c(32, 55),
  se_weighted_count = c(15.42725, 29.63106),
  se_count = c(0, 0)
)

# Unit tests
test_that("estimate_with_bootstrap_se produces correct results for hhsize_by_sex", {
  output_hhsize <- estimate_with_bootstrap_se(
    data = input_data,
    f = hhsize_by_sex,
    wt_col = "wt",
    repwt_cols = paste0("repwt", 1:4),
    constant = 1,   # Using constant = 1 for simplicity
    se_cols = c("weighted_mean"),
    hhsize = "hhsize"
  )
  
  # Round decimals to avoid floating point mismatch
  output_hhsize <- output_hhsize |>
    mutate(
      weighted_mean = round(weighted_mean, 4),
      se_weighted_mean = round(se_weighted_mean, 4)
    )
  expected_hhsize <- expected_hhsize |>
    mutate(
      weighted_mean = round(weighted_mean, 4),
      se_weighted_mean = round(se_weighted_mean, 4)
    )
  
  expect_equal(output_hhsize, expected_hhsize)
})

test_that("estimate_with_bootstrap_se produces correct results for count_by_sex", {
  output_count <- estimate_with_bootstrap_se(
    data = input_data,
    f = count_by_sex,
    wt_col = "wt",
    repwt_cols = paste0("repwt", 1:4),
    constant = 1,   # Using constant = 1 for simplicity
    se_cols = c("weighted_count", "count")
  )
  
  # Round decimals to avoid floating point mismatch
  output_count <- output_count |>
    mutate(
      se_weighted_count = round(se_weighted_count, 5),
      se_count = round(se_count, 5)
    )
  expected_count <- expected_count |>
    mutate(
      se_weighted_count = round(se_weighted_count, 5),
      se_count = round(se_count, 5)
    )

  expect_equal(output_count, expected_count)
})
