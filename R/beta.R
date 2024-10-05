create_sql_query <- function(
    lookup_table,
    col # Must be a string
) {
  # Set the name for the new column based on the input col argument
  new_col <- paste0(col, "_bucket")
  
  # If there are no range-based lookup rules...
  if (nrow(lookup_table) == 0) {
    # Generate SQL query that appends a new column (set by col) filled with NAs
    query <- paste0("
SELECT 
    data.*, 
    NULL AS ", new_col, "
FROM 
    ipums_bucketed AS data;")
  } else {
    # If the lookup table has at least one row...
    # Create the SQL query dynamically based on the input lookup table
    lookup_sql <- paste0(
      "WITH buckets AS (\n    ",
      paste(
        apply(lookup_table, 1, function(row) {
          # Use the upper and lower bounds directly from the table
          paste0("SELECT '", row["bucket_name"], "' AS bucket_name, ", 
                 row["lower_bound"], " AS lower_bound, ", 
                 row["upper_bound"], " AS upper_bound")
        }), 
        collapse = "\n    UNION ALL\n    "
      ),
      "\n)\n"
    )
    
    # Build the final SQL query using the dynamically generated WITH clause
    query <- paste0(
      lookup_sql,
      "-- Applying the lookup table to the ", col, " column\n",
      "SELECT\n",
      "    data.*,\n",
      "    COALESCE(bucket_name, 'Unknown') AS ", new_col, "\n",
      "FROM\n",
      "    ipums_bucketed AS data\n",
      "LEFT JOIN\n",
      "    buckets\n",
      "ON\n",
      "    data.", col, " >= buckets.lower_bound\n",
      "    AND (data.", col, " < buckets.upper_bound OR buckets.upper_bound IS NULL);"
    )
  }
  
  return(query)
}
