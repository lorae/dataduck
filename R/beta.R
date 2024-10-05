create_sql_query <- function(
    lookup_table
) {
  # If there are no range-based lookup rules...
  if (nrow(lookup_table) == 0) {
    # Generate SQL query that appends an AGE_bucketed column filled with NAs
    query <- "
SELECT 
    data.*, 
    NULL AS AGE_bucketed 
FROM 
    ipums_bucketed AS data;"
  } else {
    # If the lookup table has at least one row...
    # Create the SQL query dynamically based on the input lookup table
    lookup_sql <- paste0(
      "WITH age_buckets AS (\n    ",
      paste(
        apply(lookup_table, 1, function(row) {
          # Use the upper and lower bounds directly from the table
          paste0("SELECT '", row["bucket_name"], "' AS bucket_name, ", 
                 row["lower_bound"], " AS lower_bound, ", row["upper_bound"], " AS upper_bound")
        }), 
        collapse = "\n    UNION ALL\n    "
      ),
      "\n)\n"
    )
    
    # Build the final SQL query using the dynamically generated WITH clause
    query <- paste0(
      lookup_sql,
      "-- Applying the lookup table to the AGE column\n",
      "SELECT\n",
      "    data.*,\n",
      "    COALESCE(bucket_name, 'Unknown') AS AGE_bucketed\n",
      "FROM\n",
      "    ipums_bucketed AS data\n",
      "LEFT JOIN\n",
      "    age_buckets\n",
      "ON\n",
      "    data.AGE >= age_buckets.lower_bound\n",
      "    AND (data.AGE < age_buckets.upper_bound OR age_buckets.upper_bound IS NULL);"
    )
  }
  
  return(query)
}

