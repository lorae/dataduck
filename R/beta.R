#' Translate a single line of bucketing logic from a value lookup table into SQL code
#'
#' This function translates one line of a value-based lookup table into a corresponding
#' SQL `CASE WHEN` statement.
#'
#' @param i Integer. The row index of the lookup table.
#' @param lookup_table A tibble or data frame with at least two columns: `bucket_name` 
#' and `specific_value`, which define the bucketing logic.
#' @param col A string. The name of the column in the database that the SQL logic applies to.
#' 
#' @return A string. A single line of SQL `CASE WHEN` code for a value-based condition.
#' @export
write_sql_fragment_value <- function(i, lookup_table, col) {
  
  bucket_name <- lookup_table$bucket_name[i]
  specific_value <- lookup_table$specific_value[i]
  
  # If-else logic used to wrap specific_value in single quotes if it's not a number
  if (is.numeric(specific_value)) {
    fragment <- glue::glue("WHEN {col} = {specific_value} THEN '{bucket_name}'")
  } else {
    fragment <- glue::glue("WHEN {col} = '{specific_value}' THEN '{bucket_name}'")
  }
  
  return(fragment)
}

#' Translate a single line of bucketing logic from a range lookup table into SQL code
#'
#' This function translates one line of a range-based lookup table into a corresponding
#' SQL `CASE WHEN` statement.
#'
#' @param i Integer. The row index of the lookup table.
#' @param lookup_table A tibble or data frame with at least three columns: `bucket_name`, 
#' `lower_bound`, and `upper_bound`, which define the range bucketing logic.
#' @param col A string. The name of the column in the database that the SQL logic applies to.
#' 
#' @return A string. A single line of SQL `CASE WHEN` code for a range-based condition.
#' @export
write_sql_fragment_range <- function(i, lookup_table, col) {
  
  bucket_name <- lookup_table$bucket_name[i]
  lower_bound <- lookup_table$lower_bound[i]
  upper_bound <- lookup_table$upper_bound[i]
  
  fragment <- glue::glue(
    "WHEN {col} >= {lower_bound} AND {col} < {upper_bound} THEN '{bucket_name}'"
  )
  
  return(fragment)
}

#' Create SQL statements to add and populate a new column with bucketing logic
#'
#' This function generates SQL `ALTER TABLE` and `UPDATE` statements to add a new column
#' and populate it based on value-based and range-based bucketing rules provided by
#' `value_lookup_table` and `range_lookup_table`.
#'
#' @param range_lookup_table A tibble or data frame with at least three columns: 
#' `bucket_name`, `lower_bound`, and `upper_bound` to define the range bucketing logic.
#' @param value_lookup_table A tibble or data frame with at least two columns: 
#' `bucket_name` and `specific_value` to define the value-based bucketing logic.
#' @param col A string. The name of the column in the database that the SQL logic applies to.
#' @param table A string. The name of the SQL table where the new column will be added.
#' 
#' @return A string containing the SQL statements to add and update the new column.
#' @export
write_sql_query <- function(
    range_lookup_table,
    value_lookup_table,
    col,   # Must be a string
    table  # Must be a string
) {
  
  # Set the name for the new column based on the input col argument
  new_col <- paste0(col, "_bucket")
  
  # Verify that at least one of the lookup tables is nonempty before proceeding
  if (nrow(value_lookup_table) == 0 & nrow(range_lookup_table) == 0) {
    stop(paste(
      "Cannot write SQL query. Both range_lookup_table and value_lookup_table",
      "have no rows."
    ))
  }
  
  # Generate the code block executing the value-based logic of the SQL query 
  if (nrow(value_lookup_table) == 0) {
    value_logic <- "-- None specified"
  } else {
    # Translate each line of the lookup table into SQL code expressing the 
    # bucketing logic
    value_logic <- lapply(
      seq_len(value_lookup_table |> nrow()),
      write_sql_fragment_value,
      # Arguments for `write_sql_fragment_value()`
      lookup_table = value_lookup_table,
      col = col
    ) |> 
      unlist() |>
      paste(collapse = "\n    ") # newline and spaces for formatting
  }
    
  # Generate the code block executing the range-based logic of the SQL query
  if (nrow(range_lookup_table) == 0) {
    range_logic <- "-- None specified"
  } else {
    # Translate each line of the lookup table into SQL code expressing the 
    # bucketing logic
    range_logic <- lapply(
      seq_len(range_lookup_table |> nrow()),
      write_sql_fragment_range,
      # Arguments for `write_sql_fragment_range()`
      lookup_table = range_lookup_table,
      col = col
    ) |> 
      unlist() |>
      paste(collapse = "\n    ") # newline and spaces for formatting
  }
    
  # Construct the final SQL query, inserting the generated logic into the CASE statement
  query <- glue::glue(
    "ALTER TABLE {table} ADD COLUMN {new_col} TEXT;
    UPDATE {table} SET {new_col} = (
      CASE
        -- Value-based bucketing logic
        {value_logic}
        -- Range-based bucketing logic
        {range_logic}
        ELSE 'Unknown'
      END
    );"
  )
  
  return(query)
}
