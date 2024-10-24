# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("glue")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Load your package (assuming write_sql_fragment_range is part of `dataduck`)
devtools::load_all("../dataduck")

# ----- Step 1: Create test inputs ----- #

# Define the AGE lookup table (similar to test_range used in your example)
age_lookup_tb <- tibble::tibble(
  bucket_name = c("Under 16", "16-17", "18-22", "23-29", "30+"),
  lower_bound = c(0, 16, 18, 23, 30),
  upper_bound = c(16, 18, 23, 30, 200)
)

# Expected SQL fragments for each row
expected_fragments <- c(
  "WHEN AGE >= 0 AND AGE < 16 THEN 'Under 16'",
  "WHEN AGE >= 16 AND AGE < 18 THEN '16-17'",
  "WHEN AGE >= 18 AND AGE < 23 THEN '18-22'",
  "WHEN AGE >= 23 AND AGE < 30 THEN '23-29'",
  "WHEN AGE >= 30 AND AGE < 200 THEN '30+'"
)

# ----- Step 2: Unit test ----- #

test_that("write_sql_fragment_range produces proper SQL fragments for each row of the AGE lookup table", {
  
  # Loop through each row in the lookup table and compare output
  for (i in seq_len(nrow(age_lookup_tb))) {
    output_fragment <- write_sql_fragment_range(i, age_lookup_tb, "AGE")
    
    # Normalize whitespace for comparison
    normalized_output <- gsub("\\s+", " ", trimws(output_fragment))
    normalized_expected <- gsub("\\s+", " ", trimws(expected_fragments[i]))
    
    # Compare the normalized strings
    expect_equal(normalized_output, normalized_expected, 
                 info = glue::glue("Mismatch at row {i}"))
  }
  
})
