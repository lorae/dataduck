library("testthat")
library("readr")
library("rlang")
library("rprojroot")
library("DBI")
library("duckdb")
library("tibble")
library("dplyr")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Unit test for calculate_weighted_mean function
test_that("weighted_mean produces correct results", {
  
  # Step 1: Read the test data from a CSV file
  # Replace 'path/to/test_data.csv' with the actual file path
  test_data <- read_csv("tests/test-data/weighted-mean-inputs.csv")
  
  # Step 2: Read the expected results from a CSV file into a tibble
  # Replace 'path/to/expected.csv' with the actual file path
  expected <- read_csv("tests/test-data/weighted-mean-expected.csv") %>%
    as_tibble()  # Convert to tibble if not already one
  
  # Step 3: Load the test data into a DuckDB database
  con <- dbConnect(duckdb::duckdb(), ":memory:")  # Create an in-memory DuckDB instance
  dbWriteTable(con, "test_data", test_data, overwrite = TRUE)
  
  # Step 4: Query the DuckDB table within the weighted_mean function
  output <- weighted_mean(
    data = tbl(con, "test_data"),  # Use DuckDB table as input
    value_column = "NUMPREC",
    weight_column = "PERWT",
    group_by_columns = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX")
  ) %>% collect()  # Collect the result back into memory as a tibble
  
  # Step 5: Round the weighted_mean to the nearest millionth in both output and expected
  output$weighted_mean <- round(output$weighted_mean, 6)
  expected$weighted_mean <- round(expected$weighted_mean, 6)
  
  # Step 6: Sort both output and expected tibbles by the grouping columns
  output <- output %>%
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket, SEX)
  
  expected <- expected %>%
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket, SEX)
  
  # Step 7: Compare the results using testthat
  expect_equal(output$count, expected$count)
  expect_equal(output$weighted_mean, expected$weighted_mean)
  expect_equal(output$HHINCOME_bucket, expected$HHINCOME_bucket)
  expect_equal(output$AGE_bucket, expected$AGE_bucket)
  expect_equal(output$RACE_ETH_bucket, expected$RACE_ETH_bucket)
  expect_equal(output$SEX, expected$SEX)
  expect_equal(output$sum_weights, expected$sum_weights)
  
  # Step 8: Clean up (disconnect from DuckDB)
  dbDisconnect(con, shutdown = TRUE)
})


