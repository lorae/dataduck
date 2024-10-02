library("testthat")
library("readr")
library("dplyr")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function being tested
source("src/utils/bucketing-tools.R")

test_that("split_lookup_table correctly splits value and range lookups", {
  
  # Run the function on the test CSV
  lookup_tables <- split_lookup_table("tests/test-data/mock_hhincome_lookup.csv")
  
  # Define the expected value lookup
  expected_value_lookup <- tibble(
    bucket_name = c("N/A"),
    specific_value = c(9999999)
  )
  
  # Define the expected range lookup
  expected_range_lookup <- tibble(
    bucket_name = c("negative", "r000_100k", "r100kplus"),
    lower_bound = c(-Inf, 0, 100000),
    upper_bound = c(0, 100000, 9999999)
  )
  
  # Test that the value lookup matches the expected result
  expect_equal(lookup_tables$value, expected_value_lookup)
  
  # Test that the range lookup matches the expected result
  expect_equal(lookup_tables$range, expected_range_lookup)
})
