library("testthat")
library("dplyr")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

test_that("range_match_lookup works with a DuckDB connection", {
  
  # Create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  
  # Create synthetic data and load into DuckDB
  raw_df <- tibble(
    AGE = c(15, 18, 25, 40, 60)
  )
  dbWriteTable(con, "synth_ipums", raw_df)
  
  # Create the lookup table and load into DuckDB
  age_lookup_df <- tibble(
    bucket_name = c("Under 18", "18-34", "35-49", "Over 50"),
    lower_bound = c(0, 18, 35, 50),
    upper_bound = c(18, 35, 50, 200)
  )
  dbWriteTable(con, "age_lookup", age_lookup_df)
  
  # Load tables from DuckDB
  raw_db <- tbl(con, "synth_ipums")
  age_lookup_db <- tbl(con, "age_lookup")
  
  # Apply the range_match_lookup function
  output_db <- range_match_lookup(
    data = raw_db,
    lookup = age_lookup_db,
    input_column = "AGE"
  )
  
  # Collect the result from DuckDB
  output_df <- output_db %>% collect()
  
  # Expected result
  expected_df <- tibble(
    AGE = c(15, 18, 25, 40, 60),
    AGE_bucket = c("Under 18", "18-34", "18-34", "35-49", "Over 50")
  )
  
  # Assert that the output matches the expected result
  expect_equal(output_df, expected_df)
  
  # Disconnect from DuckDB
  DBI::dbDisconnect(con)
})
