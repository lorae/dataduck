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
lookup_tb <- tibble::tibble(
  bucket_name = c("Under 16", "16-17", "18-22", "23-29", "30+"),
  lower_bound = c(0, 16, 18, 23, 30),
  upper_bound = c(16, 18, 23, 30, 200)
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

test_that("create_sql_query produces proper query based on lookup table", {
  
  expected_string <- "
  WITH age_buckets AS (
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
    COALESCE(bucket_name, 'Unknown') AS AGE_bucketed
FROM 
    ipums_bucketed AS data
LEFT JOIN 
    age_buckets
ON 
    data.AGE >= age_buckets.lower_bound 
    AND (data.AGE < age_buckets.upper_bound OR age_buckets.upper_bound IS NULL);
"

output_string <- create_sql_query(lookup_table = lookup_tb)

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
    NULL AS AGE_bucketed 
FROM 
    ipums_bucketed AS data;
"
  
  output_string <- create_sql_query(lookup_table = lookup_empty_tb)
  
  # Normalize whitespace
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})

# ----- Step 4: Clean up ----- #

dbDisconnect(test_con, shutdown = TRUE)
