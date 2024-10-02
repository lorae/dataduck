library("testthat")
library("dplyr")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

test_that("join_columns works with tibbles in R memory", {
  
  # Create sample data for data1
  data1_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6, 7),
    INCOME = c(100000, 0, 50000, 40000, 10000, 55000, 2000),
    AGE_bucket = c("Over 50", "Under 25", "25-50", "Over 50", "Over 50", "25-50", "Over 50")
  )
  
  # Create sample data for data2
  data2_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6, 7),
    INCOME = c(100000, 0, 50000, 40000, 10000, 55000, 2000),
    AGE_bucket = c(NA, NA, "30 years old", NA, NA, NA, NA)
  )
  
  # Run the function
  output_tb <- join_columns(
    data1 = data1_tb,
    data2 = data2_tb,
    column = "AGE_bucket",
    id = "id"
  )
  
  # Expected result
  expected_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6, 7),
    INCOME = c(100000, 0, 50000, 40000, 10000, 55000, 2000),
    AGE_bucket = c("Over 50", "Under 25", "30 years old", "Over 50", "Over 50", "25-50", "Over 50")
  )
  
  # Assert that the output matches the expected result
  expect_equal(output_tb, expected_tb)
})