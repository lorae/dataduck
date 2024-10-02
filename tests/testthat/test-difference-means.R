# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("rlang")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function being tested
source("src/utils/aggregation-tools.R")

# ----- Step 1: Create test inputs ----- #

# Test tibble inputs
data2000_tb <- tibble(
  CPUMA0010 = c(1001, 1002, 1003),
  weighted_mean = c(2.5, 3.1, 2.9),
  count = c(100, 200, 150),
  other_col = c(5, 10, 15),
  decoy_col_1 = c(20, 30, 40),
  decoy_col_2 = c(1, 2, 4) 
)

data2020_tb <- tibble(
  CPUMA0010 = c(1001, 1002, 1003),
  weighted_mean = c(2.7, 2.8, 3.0),
  count = c(110, 210, 140),
  other_col = c(7, 12, 17), 
  decoy_col_1 = c(25, 35, 45),
  decoy_col_2 = c(3, 1, 6) 
)

# Test database table inputs
test_con <- dbConnect(duckdb::duckdb(), ":memory:")
data2000_db <- copy_to(test_con, data2000_tb, name = "data2000_db", temporary = TRUE)
data2020_db <- copy_to(test_con, data2020_tb, name = "data2020_db", temporary = TRUE)

# ----- Step 2: Define helper functions ----- #

compare_results <- function(output_tb, expected_tb) {
  output_tb <- output_tb |> select(sort(colnames(output_tb))) |> arrange(CPUMA0010)
  expected_tb <- expected_tb |> select(sort(colnames(expected_tb))) |> arrange(CPUMA0010)
  expect_equal(output_tb, expected_tb)
}

# ----- Step 3: Unit tests ----- #

test_that("difference_means produces correct results on tibble inputs", {
  
  expected_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    diff = c(0.2, -0.3, 0.1),   
    weighted_mean_2000 = c(2.5, 3.1, 2.9),
    weighted_mean_2020 = c(2.7, 2.8, 3.0),
    count_2000 = c(100, 200, 150),
    count_2020 = c(110, 210, 140),
    other_col_2000 = c(5, 10, 15),
    other_col_2020 = c(7, 12, 17)
  )
  
  output_tb <- difference_means(
    data2000 = data2000_tb,
    data2020 = data2020_tb,
    match_by = "CPUMA0010",
    diff_by = "weighted_mean",
    keep = c("count", "other_col")
  )
  
  compare_results(output_tb, expected_tb)
  
})

test_that("difference_means produces correct results on tibble inputs when `keep` argument is set to NULL", {
  
  expected_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    diff = c(0.2, -0.3, 0.1),   
    weighted_mean_2000 = c(2.5, 3.1, 2.9),
    weighted_mean_2020 = c(2.7, 2.8, 3.0)
  )
  
  output_tb <- difference_means(
    data2000 = data2000_tb,
    data2020 = data2020_tb,
    match_by = "CPUMA0010",
    diff_by = "weighted_mean",
    keep = NULL
  )
  
  compare_results(output_tb, expected_tb)
  
})

test_that("difference_means produces correct results on database inputs", {
  
  expected_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    diff = c(0.2, -0.3, 0.1),   
    weighted_mean_2000 = c(2.5, 3.1, 2.9),
    weighted_mean_2020 = c(2.7, 2.8, 3.0),
    count_2000 = c(100, 200, 150),
    count_2020 = c(110, 210, 140),
    other_col_2000 = c(5, 10, 15),
    other_col_2020 = c(7, 12, 17)
  )
  
  output_tb <- difference_means(
    data2000 = data2000_db,
    data2020 = data2020_db,
    match_by = "CPUMA0010",
    diff_by = "weighted_mean",
    keep = c("count", "other_col")
  ) |> collect()
  
  compare_results(output_tb, expected_tb)
  
})

test_that("difference_means produces correct results on database inputs when `keep` argument is set to NULL", {
  
  expected_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    diff = c(0.2, -0.3, 0.1),   
    weighted_mean_2000 = c(2.5, 3.1, 2.9),
    weighted_mean_2020 = c(2.7, 2.8, 3.0)
  )
  
  output_tb <- difference_means(
    data2000 = data2000_db,
    data2020 = data2020_db,
    match_by = "CPUMA0010",
    diff_by = "weighted_mean",
    keep = NULL
  ) |> collect()
  
  compare_results(output_tb, expected_tb)
  
})

# ----- Step 4: Clean up ----- #

dbDisconnect(test_con, shutdown = TRUE)


