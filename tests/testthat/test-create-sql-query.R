# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("rlang")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# ----- Step 1: Create test inputs ----- #

# Test tibble inputs
age_lookup_tb <- tibble::tibble(
  bucket_name = c("Under 16", "16-17", "18-22", "23-29", "30+"),
  lower_bound = c(0, 16, 18, 23, 30),
  upper_bound = c(16, 18, 23, 30, 200)
)

hhincome_lookup_tb <- tibble::tibble(
  bucket_name = c("Negative", "Under 10", "10 plus"),
  lower_bound = c(-99, 0, 10),
  upper_bound = c(0, 10, 99)
)

lookup_empty_tb <- tibble::tibble(
  bucket_name = character(),
  lower_bound = logical(),
  upper_bound = logical()
)

# Test database table inputs
test_con <- dbConnect(duckdb::duckdb(), ":memory:")

# ----- Step 2: Define helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 3: Unit tests ----- #

test_that("create_sql_query produces proper query based on age lookup table", {
  
  expected_string <- "
  WITH buckets AS (
    SELECT 'Under 16' AS bucket_name, 0 AS lower_bound, 16 AS upper_bound
    UNION ALL
    SELECT '16-17' AS bucket_name, 16 AS lower_bound, 18 AS upper_bound
    UNION ALL
    SELECT '18-22' AS bucket_name, 18 AS lower_bound, 23 AS upper_bound
    UNION ALL
    SELECT '23-29' AS bucket_name, 23 AS lower_bound, 30 AS upper_bound
    UNION ALL
    SELECT '30+' AS bucket_name, 30 AS lower_bound, 200 AS upper_bound
)
-- Applying the lookup table to the AGE column
SELECT 
    data.*, 
    COALESCE(bucket_name, 'Unknown') AS AGE_bucket
FROM 
    ipums_bucketed AS data
LEFT JOIN 
    buckets
ON 
    data.AGE >= buckets.lower_bound 
    AND (data.AGE < buckets.upper_bound OR buckets.upper_bound IS NULL);
"

output_string <- create_sql_query(
  lookup_table = age_lookup_tb,
  col = "AGE"
)

# Normalize whitespace
normalized_output <- gsub("\\s+", " ", trimws(output_string))
normalized_expected <- gsub("\\s+", " ", trimws(expected_string))

# Compare the normalized strings
expect_equal(normalized_output, normalized_expected)

})

test_that("create_sql_query produces proper query based on household income lookup table", {
  
  expected_string <- "
  WITH buckets AS (
    SELECT 'Negative' AS bucket_name, -99 AS lower_bound, 0 AS upper_bound
    UNION ALL
    SELECT 'Under 10' AS bucket_name, 0 AS lower_bound, 10 AS upper_bound
    UNION ALL
    SELECT '10 plus' AS bucket_name, 10 AS lower_bound, 99 AS upper_bound
)
-- Applying the lookup table to the HHINCOME column
SELECT 
    data.*, 
    COALESCE(bucket_name, 'Unknown') AS HHINCOME_bucket
FROM 
    ipums_bucketed AS data
LEFT JOIN 
    buckets
ON 
    data.HHINCOME >= buckets.lower_bound 
    AND (data.HHINCOME < buckets.upper_bound OR buckets.upper_bound IS NULL);
"

output_string <- create_sql_query(
  lookup_table = hhincome_lookup_tb,
  col = "HHINCOME"
)

# Normalize whitespace
normalized_output <- gsub("\\s+", " ", trimws(output_string))
normalized_expected <- gsub("\\s+", " ", trimws(expected_string))

# Compare the normalized strings
expect_equal(normalized_output, normalized_expected)

})


test_that("create_sql_query produces proper query when given empty lookup table", {
  
  expected_string <- "
SELECT 
    data.*, 
    NULL AS AGE_bucket
FROM 
    ipums_bucketed AS data;
"
  
  output_string <- create_sql_query(
    lookup_table = lookup_empty_tb,
    col = "AGE"
  )
  
  # Normalize whitespace
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})

# ----- Step 4: Clean up ----- #

dbDisconnect(test_con, shutdown = TRUE)
