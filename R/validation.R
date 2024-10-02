# Stops pipeline execution of rows are unintentionally dropped.
# Note: input data must be a database
validate_row_counts <- function(
    db, # A database of the data being validated
    expected_count,# The expected number of rows
    step_description
) {
  
  actual_count <- db |>
    summarize(count = n()) |>
    pull(count)
  
  if (actual_count != expected_count) {
    stop(glue::glue(
      "DATA VALIDATION FAILED: Unexpected number of rows found after ",
      "{step_description}: expected {expected_count}, got {actual_count}"
      ))
  }
}

