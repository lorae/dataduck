#' Validate Row Counts in a Database
#'
#' This function checks whether the actual number of rows in a database matches the expected number of rows. 
#' If the actual count does not match the expected count, the function stops the pipeline and raises an error. 
#' It is typically used to ensure data integrity during data processing steps.
#'
#' @param db A database connection or a lazy database reference representing the data being validated.
#' @param expected_count An integer representing the expected number of rows in the database.
#' @param step_description A string providing a description of the pipeline step being validated, which will 
#'   be included in the error message if validation fails.
#'
#' @return If the validation fails (i.e., the actual row count does 
#'   not match the expected count), it raises an error and stops the pipeline execution.
#'   If validation succeeds, it prints a statement.
#'
#' @export
validate_row_counts <- function(
    db, 
    expected_count,
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
  } else {
    print(glue::glue(
      "Data validation successful: {step_description}."
    ))
  }
}


