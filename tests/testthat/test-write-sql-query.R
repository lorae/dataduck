# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("glue")
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

# Define the empty range lookup table
range_test_empty <- tibble::tibble(
  bucket_name = character(),
  lower_bound = logical(),
  upper_bound = logical()
)

# Define the empty value lookup table
value_test_empty <- tibble::tibble(
  bucket_name = character(),
  specific_value = logical()
)

# ----- Step 2: Unit test ----- #

test_that("write_sql_query produces correct SQL for nonempty value and range lookup tables", {
  
  expected_string <- "
  ALTER TABLE ipums_bucketed
  ADD AGE_bucket AS (
    CASE
      -- Value-based bucketing logic
      WHEN AGE = 25 THEN 'Exactly 25'
      WHEN AGE = 100 THEN 'Exactly 100'
      -- Range-based bucketing logic
      WHEN AGE >= 0 AND AGE < 16 THEN 'Under 16'
      WHEN AGE >= 16 AND AGE < 18 THEN '16-17'
      WHEN AGE >= 18 AND AGE < 23 THEN '18-22'
      WHEN AGE >= 23 AND AGE < 30 THEN '23-29'
      WHEN AGE >= 30 AND AGE < 200 THEN '30+'
      ELSE 'Unknown'
    END
  );"
  
  output_string <- write_sql_query(
    range_lookup_table = range_test,
    value_lookup_table = value_test,
    col = "AGE",
    table = "ipums_bucketed"
  )
  
  # Normalize whitespace for comparison
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})

test_that("write_sql_query produces correct SQL for nonempty value and empty range lookup tables", {
  
  expected_string <- "
  ALTER TABLE ipums_bucketed
  ADD AGE_bucket AS (
    CASE
      -- Value-based bucketing logic
      WHEN AGE = 25 THEN 'Exactly 25'
      WHEN AGE = 100 THEN 'Exactly 100'
      -- Range-based bucketing logic
      -- None specified
      ELSE 'Unknown'
    END
  );"
  
  output_string <- write_sql_query(
    range_lookup_table = range_test_empty,
    value_lookup_table = value_test,
    col = "AGE",
    table = "ipums_bucketed"
  )
  
  # Normalize whitespace for comparison
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})

test_that("write_sql_query produces correct SQL for empty value and nonempty range lookup tables", {
  
  expected_string <- "
  ALTER TABLE ipums_bucketed
  ADD AGE_bucket AS (
    CASE
      -- Value-based bucketing logic
      -- None specified
      -- Range-based bucketing logic
      WHEN AGE >= 0 AND AGE < 16 THEN 'Under 16'
      WHEN AGE >= 16 AND AGE < 18 THEN '16-17'
      WHEN AGE >= 18 AND AGE < 23 THEN '18-22'
      WHEN AGE >= 23 AND AGE < 30 THEN '23-29'
      WHEN AGE >= 30 AND AGE < 200 THEN '30+'
      ELSE 'Unknown'
    END
  );"
  
  output_string <- write_sql_query(
    range_lookup_table = range_test,
    value_lookup_table = value_test_empty,
    col = "AGE",
    table = "ipums_bucketed"
  )
  
  # Normalize whitespace for comparison
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})

test_that("write_sql_query throws an error when both value and range lookup tables are empty", {
  
  # Expect an error when both lookup tables are empty
  expect_error(
    write_sql_query(
      range_lookup_table = range_test_empty,
      value_lookup_table = value_test_empty,
      col = "AGE",
      table = "ipums_bucketed"
    ),
    regexp = "Cannot write SQL query. Both range_lookup_table and value_lookup_table have no rows."
  )
  
})

