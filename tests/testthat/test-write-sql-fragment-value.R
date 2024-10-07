# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("glue")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

devtools::load_all("../dataduck")

# ----- Step 1: Create test inputs ----- #

# Define the HISPAN lookup table
hispan_lookup_tb <- tibble::tibble(
  bucket_name = c("not_hispanic", "hispanic", "hispanic", "hispanic", "hispanic", "N/A"),
  specific_value = c(0, 1, 2, 3, 4, 9)
)

# Expected SQL fragments for each row
expected_fragments <- c(
  "WHEN HISPAN = 0 THEN 'not_hispanic'",
  "WHEN HISPAN = 1 THEN 'hispanic'",
  "WHEN HISPAN = 2 THEN 'hispanic'",
  "WHEN HISPAN = 3 THEN 'hispanic'",
  "WHEN HISPAN = 4 THEN 'hispanic'",
  "WHEN HISPAN = 9 THEN 'N/A'"
)

# ----- Step 2: Unit test ----- #

test_that("write_sql_fragment_value produces proper SQL fragments for each row of the HISPAN lookup table", {
  
  # Loop through each row in the lookup table and compare output
  for (i in seq_len(nrow(hispan_lookup_tb))) {
    output_fragment <- write_sql_fragment_value(i, hispan_lookup_tb, "HISPAN")
    
    # Normalize whitespace for comparison
    normalized_output <- gsub("\\s+", " ", trimws(output_fragment))
    normalized_expected <- gsub("\\s+", " ", trimws(expected_fragments[i]))
    
    # Compare the normalized strings
    expect_equal(normalized_output, normalized_expected, 
                 info = glue::glue("Mismatch at row {i}"))
  }
  
})


