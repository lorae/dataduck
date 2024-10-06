# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("glue")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Load your package (assuming create_range_logic is part of `dataduck`)
devtools::load_all("../dataduck")

# ----- Step 1: Create test inputs ----- #

# Define the AGE lookup table (similar to test_range used in your example)
age_lookup_tb <- tibble::tibble(
  bucket_name = c("Under 16", "16-17", "18-22", "23-29", "30+"),
  lower_bound = c(0, 16, 18, 23, 30),
  upper_bound = c(16, 18, 23, 30, 200)
)

# ----- Step 2: Unit test ----- #

test_that("create_range_logic produces proper SQL fragments based on AGE lookup table", {
  
  expected_string <- "
  WHEN AGE >= 0 AND AGE < 16 THEN 'Under 16'
  WHEN AGE >= 16 AND AGE < 18 THEN '16-17'
  WHEN AGE >= 18 AND AGE < 23 THEN '18-22'
  WHEN AGE >= 23 AND AGE < 30 THEN '23-29'
  WHEN AGE >= 30 AND AGE < 200 THEN '30+'
  "
  
  output_string <- create_range_logic(
    range_lookup_table = age_lookup_tb,
    col = "AGE"
  )
  
  # Normalize whitespace for comparison
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})