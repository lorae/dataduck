library("testthat")
library("dplyr")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

test_that("value_match_lookup works with DuckDB data table", {
  
  # Create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  
  # Create sample data and load into DuckDB connection
  data_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6),
    INCOME = c(0, 100000, 40000, 9999999, 50000, 4)
  )
  dbWriteTable(con, "data", data_tb)
  
  # Create sample lookup table and load into DuckDB connection
  lookup_tb <- tibble(
    specific_value = c(9999999, 4),
    bucket_name = c("Top coded", "Social Security")
  )
  dbWriteTable(con, "lookup", lookup_tb)
  
  # Load tables from DuckDB
  data_db <- tbl(con, "data")
  lookup_db <- tbl(con, "lookup")
  
  # Run the function
  output_db <- value_match_lookup(
    data = data_db,
    lookup = lookup_db,
    input_column = "INCOME"
  )
  
  # Collect the result from DuckDB
  output_tb <- output_db %>% collect()
  
  # Expected output
  expected_tb <- tibble(
    id = c(1, 2, 3, 4, 5, 6),
    INCOME = c(0, 100000, 40000, 9999999, 50000, 4),
    INCOME_bucket = c(NA, NA, NA, "Top coded", NA, "Social Security")
  )
  
  # Assert that the output matches the expected result
  # Arrange so that row orders are identical
  expect_equal(
    output_tb |> arrange(id), 
    expected_tb |> arrange(id)
  )
  
  # Disconnect from DuckDB
  DBI::dbDisconnect(con)
})