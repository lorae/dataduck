library(testthat)
library(DBI)
library(duckdb)

test_that("AGE bucket column is created correctly", {
  # Connect to a temporary DuckDB database
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  
  # Create a mock dataset similar to your ipums_bucketed table
  data <- data.frame(
    id = 1:6,
    AGE = c(15, 16, 18, 22, 29, 35),
    YEAR = rep(2014, 6),
    SAMPLE = rep(201401, 6),
    SERIAL = c(755807, 755807, 755808, 755809, 755810, 755811)
  )
  
  # Define the expected data frame
  expected <- data.frame(
    id = 1:6,
    AGE = c(15, 16, 18, 22, 29, 35),
    YEAR = rep(2014, 6),
    SAMPLE = rep(201401, 6),
    SERIAL = c(755807, 755807, 755808, 755809, 755810, 755811),
    AGE_bucketed = c("Under 16", "16-17", "18-22", "18-22", "23-29", "30+")
  )
  
  # Write the data to a DuckDB table
  dbWriteTable(con, "ipums_bucketed", data)
  
  # Run the SQL query to create the "AGE_bucketed" column
  query <- "
  WITH age_buckets AS (
      -- Defining the bucket ranges
      SELECT 'Under 16' AS bucket_name, 0 AS lower_bound, 16 AS upper_bound
      UNION ALL
      SELECT '16-17', 16, 18
      UNION ALL
      SELECT '18-22', 18, 23
      UNION ALL
      SELECT '23-29', 23, 30
      UNION ALL
      SELECT '30+', 30, NULL  -- NULL for upper bound as infinity
  )
  
  -- Applying the lookup table to the AGE column
  SELECT 
      data.*, 
      COALESCE(bucket_name, 'Unknown') AS AGE_bucketed
  FROM 
      ipums_bucketed AS data
  LEFT JOIN 
      age_buckets
  ON 
      data.AGE >= age_buckets.lower_bound 
      AND (data.AGE < age_buckets.upper_bound OR age_buckets.upper_bound IS NULL);
  "
  
  # Execute the query
  result <- dbGetQuery(con, query)
  
  # Sort both the result and expected data by id to ensure consistency
  result <- result[order(result$id), ]
  expected <- expected[order(expected$id), ]
  
  # Remove row names from both data frames
  row.names(result) <- NULL
  row.names(expected) <- NULL
  
  print(result)
  print(expected)
  
  # Compare the entire result and expected data frames
  expect_equal(result, expected)
  
  # Disconnect from the DuckDB database
  dbDisconnect(con, shutdown = TRUE)
})
