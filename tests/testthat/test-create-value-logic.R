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

# ----- Step 2: Unit test ----- #

test_that("create_value_logic produces proper SQL fragments based on HISPAN lookup table", {
  
  expected_string <- "
  WHEN HISPAN = 0 THEN 'not_hispanic'
  WHEN HISPAN = 1 THEN 'hispanic'
  WHEN HISPAN = 2 THEN 'hispanic'
  WHEN HISPAN = 3 THEN 'hispanic'
  WHEN HISPAN = 4 THEN 'hispanic'
  WHEN HISPAN = 9 THEN 'N/A'
  "
  
  output_string <- create_value_logic(
    value_lookup_table = hispan_lookup_tb,
    col = "HISPAN"
  )
  
  # Normalize whitespace for comparison
  normalized_output <- gsub("\\s+", " ", trimws(output_string))
  normalized_expected <- gsub("\\s+", " ", trimws(expected_string))
  
  # Compare the normalized strings
  expect_equal(normalized_output, normalized_expected)
  
})

