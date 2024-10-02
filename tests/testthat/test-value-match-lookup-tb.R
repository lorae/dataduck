library("testthat")
library("dplyr")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

test_that("value_match_lookup works with tibbles in R memory", {
  
  # Create sample data
  data_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6),
    INCOME = c(0, 100000, 40000, 9999999, 50000, 4)
  )
  
  # Create sample lookup table
  lookup_tb <- tibble(
    specific_value = c(9999999, 4),
    bucket_name = c("Top coded", "Social Security")
  )
  
  
  # Run the function
  output_tb <- value_match_lookup(
    data = data_tb,
    lookup = lookup_tb,
    input_column = "INCOME"
  )
  
  # Expected output
  expected_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6),
    INCOME = c(0, 100000, 40000, 9999999, 50000, 4),
    INCOME_bucket = c(NA, NA, NA, "Top coded", NA, "Social Security")
  )
  
  # Assert that the output matches the expected result
  expect_equal(output_tb, expected_tb)
})