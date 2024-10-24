# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("glue")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Load your package (assuming write_sql_query is part of `dataduck`)
devtools::load_all("../dataduck")

# ----- Step 1: Create test inputs ----- #

# Define the range lookup table (non-empty)
range_test <- tibble::tibble(
  bucket_name = c("Under 16", "16-17", "18-22", "23-29", "30+"),
  lower_bound = c(0, 16, 18, 23, 30),
  upper_bound = c(16, 18, 23, 30, 200)
)

# Define the value lookup table (non-empty)
value_test <- tibble::tibble(
  bucket_name = c("Exactly 25", "Exactly 100"),
  specific_value = c(25, 100)
)

# Create an empty range lookup table
range_test_empty <- tibble::tibble(
  bucket_name = character(),
  lower_bound = numeric(),
  upper_bound = numeric()
)

# Create an empty value lookup table
value_test_empty <- tibble::tibble(
  bucket_name = character(),
  specific_value = numeric()
)

# Define a tibble representing the data to be manipulated
input_tb <- tibble::tibble(
  pers_id = 1:6,
  AGE = c(10, 16, 25, 29, 30, 100)
)

# ----- Step 2: Create DuckDB connection and run tests ----- #

test_that("write_sql_query correctly manipulates DuckDB table", {
  
  # Define the expected output
  expected_tb <- tibble::tibble(
    pers_id = 1:6,
    AGE = c(10, 16, 25, 29, 30, 100),
    AGE_bucket = c("Under 16", "16-17", "Exactly 25", "23-29", "30+", "Exactly 100")
  )
  
  # Set up DuckDB connection and load input tibble in
  test_con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(test_con, "ipums", input_tb)
  
  # Apply a SQL query to add a column to the data table
  sql_query <- write_sql_query(
    range_lookup_table = range_test,
    value_lookup_table = value_test,
    col = "AGE",
    table = "ipums"
  )
  
  dbExecute(test_con, sql_query)
  
  result_tb <- tbl(test_con, "ipums") |>
    collect() |>
    arrange(pers_id)
  
  # Test that the actual result matches the expected result
  expect_equal(result_tb, expected_tb)
  
  # Clean up: disconnect from DuckDB
  dbDisconnect(test_con, shutdown = TRUE)
  
})

test_that("write_sql_query handles empty range lookup table and non-empty value lookup table", {
  
  # Define the expected output
  expected_tb_value_only <- tibble::tibble(
    pers_id = 1:6,
    AGE = c(10, 16, 25, 29, 30, 100),
    AGE_bucket = c("Unknown", "Unknown", "Exactly 25", "Unknown", "Unknown", "Exactly 100")
  )
  
  # Set up DuckDB connection and load input tibble in
  test_con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(test_con, "ipums", input_tb)
  
  # Apply a SQL query with an empty range lookup table and non-empty value lookup table
  sql_query_value_only <- write_sql_query(
    range_lookup_table = range_test_empty,
    value_lookup_table = value_test,
    col = "AGE",
    table = "ipums"
  )
  
  dbExecute(test_con, sql_query_value_only)
  
  result_tb_value_only <- tbl(test_con, "ipums") |>
    collect() |>
    arrange(pers_id)
  
  # Test that the actual result matches the expected result
  expect_equal(result_tb_value_only, expected_tb_value_only)
  
  # Clean up: disconnect from DuckDB
  dbDisconnect(test_con, shutdown = TRUE)
  
})


test_that("write_sql_query handles empty value lookup table and non-empty range lookup table", {
  
  # Define the expected output
  expected_tb_range_only <- tibble::tibble(
    pers_id = 1:6,
    AGE = c(10, 16, 25, 29, 30, 100),
    AGE_bucket = c("Under 16", "16-17", "23-29", "23-29", "30+", "30+")
  )
  
  # Set up DuckDB connection and load input tibble in
  test_con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(test_con, "ipums", input_tb)
  
  # Apply a SQL query with an empty value lookup table and non-empty range lookup table
  sql_query_range_only <- write_sql_query(
    range_lookup_table = range_test,
    value_lookup_table = value_test_empty,
    col = "AGE",
    table = "ipums"
  )
  
  dbExecute(test_con, sql_query_range_only)
  
  result_tb_range_only <- tbl(test_con, "ipums") |>
    collect() |>
    arrange(pers_id)
  
  # Test that the actual result matches the expected result
  expect_equal(result_tb_range_only, expected_tb_range_only)
  
  # Clean up: disconnect from DuckDB
  dbDisconnect(test_con, shutdown = TRUE)
  
})


test_that("write_sql_query throws an error when both lookup tables are empty", {
  
  # Set up DuckDB connection and load input tibble in
  test_con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(test_con, "ipums", input_tb)
  
  # Expect an error when both lookup tables are empty
  expect_error(
    write_sql_query(
      range_lookup_table = range_test_empty,
      value_lookup_table = value_test_empty,
      col = "AGE",
      table = "ipums"
    ),
    regexp = "Cannot write SQL query. Both range_lookup_table and value_lookup_table have no rows."
  )
  
  # Clean up: disconnect from DuckDB
  dbDisconnect(test_con, shutdown = TRUE)
  
})
