# Function which translates a single line of bucketing logic from a value lookup
# table into a single line of SQL code
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

# Function which translates a single line of bucketing logic from a range lookup
# table into a single line of SQL code
write_sql_fragment_range <- function(i, lookup_table, col) {

  bucket_name <- lookup_table$bucket_name[i]
  lower_bound <- lookup_table$lower_bound[i]
  upper_bound <- lookup_table$upper_bound[i]
  
  fragment <- glue::glue(
    "WHEN {col} >= {lower_bound} AND {col} < {upper_bound} THEN '{bucket_name}'"
  )
  
  return(fragment)
}

# Main function to create a full SQL query incorporating both value and range-based logic
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
    "ALTER TABLE {table}
    ADD {new_col} AS (
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
