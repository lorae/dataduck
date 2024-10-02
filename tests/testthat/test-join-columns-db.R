library("testthat")
library("dplyr")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function being tested
source("src/utils/bucketing-tools.R")

test_that("join_columns works with DuckDB data table", {
  
  # Create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  
  # Create sample data for data1 and load into DuckDB connection
  data1_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6, 7),
    INCOME = c(100000, 0, 50000, 40000, 10000, 55000, 2000),
    AGE_bucket = c("Over 50", "Under 25", "25-50", "Over 50", "Over 50", "25-50", "Over 50")
  )
  dbWriteTable(con, "data1", data1_tb)
  
  # Create sample data for data2 and load into DuckDB connection
  data2_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6, 7),
    INCOME = c(100000, 0, 50000, 40000, 10000, 55000, 2000),
    AGE_bucket = c(NA, NA, "30 years old", NA, NA, NA, NA)
  )
  dbWriteTable(con, "data2", data2_tb)
  
  # Load tables from DuckDB
  data1_db <- tbl(con, "data1")
  data2_db <- tbl(con, "data2")
  
  # Run the function
  output_db <- join_columns(
    data1 = data1_db,
    data2 = data2_db,
    column = "AGE_bucket",
    id = "id"
  )
  
  # Collect the result from DuckDB
  output_tb <- output_db %>% collect()
  
  # Expected result
  expected_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6, 7),
    INCOME = c(100000, 0, 50000, 40000, 10000, 55000, 2000),
    AGE_bucket = c("Over 50", "Under 25", "30 years old", "Over 50", "Over 50", "25-50", "Over 50")
  )

  # Assert that the output matches the expected result
  expect_equal(output_tb, expected_tb)
  
  # Disconnect from DuckDB
  DBI::dbDisconnect(con)
})