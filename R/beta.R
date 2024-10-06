create_value_logic <- function(
    value_lookup_table,
    col
) {
  
  # Define a function that writes each line of output
  write_sql_fragment <- function(i) {
    # Read values from lookup table needed to write one line of the query
    bucket_name <- value_lookup_table$bucket_name[i]
    specific_value <- value_lookup_table$specific_value[i]
    
    # Create fragment using these values
    if (is.numeric(specific_value)) {
      fragment <- glue("WHEN {col} = {specific_value} THEN '{bucket_name}'")
    } else {
      # Wrap specific_value in single quotes if it's not a number
      fragment <- glue("WHEN {col} = '{specific_value}' THEN '{bucket_name}'")
    }
    
    return(fragment)
  }
  
  # Use write_sql_fragment() to write one line of SQL for each row of the 
  # lookup table
  sql_fragments <- lapply(
    seq_len(nrow(value_lookup_table)),
    write_sql_fragment
  )
  
  # Combine the fragments into a single string
  paste(unlist(sql_fragments), collapse = "\n")
}



  
create_range_logic <- function(
  range_lookup_table,
  col
) {
  # Define a function that writes each line of output
  write_sql_fragment <- function(i) {
    # Read values from lookup table needed to write one line of the query
    bucket_name <- range_lookup_table$bucket_name[i]
    lower_bound <- range_lookup_table$lower_bound[i]
    upper_bound <- range_lookup_table$upper_bound[i]

    fragment <- glue(
      "WHEN {col} >= {lower_bound} AND {col} < {upper_bound} THEN '{bucket_name}'"
      )
    
    return(fragment)
  }
  
  # Use write_sql_fragment() to write one line of SQL for each row of the 
  # lookup table
  sql_fragments <- lapply(
    seq_len(nrow(range_lookup_table)),
    write_sql_fragment
  )
  
  # Combine the fragments into a single string
  paste(unlist(sql_fragments), collapse = "\n")
  
}

# TODO: rename col to col_name and table to table_name and new_col to
# new_col_name
create_sql_query <- function(
    range_lookup_table,
    value_lookup_table,
    col,   # Must be a string
    table  # Must be a string
) {
  # Set the name for the new column based on the input col argument
  new_col <- paste0(col, "_bucket")
  
  # Generate the value-based logic using create_value_logic()
  value_logic <- create_value_logic(value_lookup_table, col)
  
  # Generate the range-based logic using create_range_logic()
  range_logic <- create_range_logic(range_lookup_table, col)
  
  # Construct the final SQL query, inserting the generated logic into the CASE statement
  query <- glue::glue(
    "ALTER TABLE {table}
    ADD {new_col} AS (
      CASE
        {value_logic}
        {range_logic}
        ELSE 'Unknown'
      END
    );"
  )
  
  return(query)
}
