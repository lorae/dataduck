# bucketing-tools.R
#
# This module contains various helper functions meant to create custom buckets 
# of continuous (such as income, age) and categorical (such as race, ethnicity)
# variables.

# Proof-of-concept matching ranges in a lookup table using non-database data
# https://stackoverflow.com/questions/75629990/lookup-table-in-r-by-matching-ranges

# TODO: Roxygen documentation
# A function for bucketing data based on a simple range-based lookup table.
# Returns the input data with an appended column named `output_column`.
# Ranges are inclusive on the bottom end and exclusive on the top end.
# The lookup table must have colnames (bucket_name, lower_bound, upper_bound)
range_match_lookup <- function(
    data, # A dataframe, tibble, or db object containing the data
    lookup, # A dataframe, tibble, or db object containing the lookup table
    input_column, # The name of the column from `data` to be bucketed
    output_column = NULL # optional: the name of the output column. Default: {input_column}_bucket
) {
  # TODO: build in data check on input object types being consistent w/ one another (db,db) or (df, df)
  # TODO: build in check verifying that lookup table ranges don't overlap.
  # That will be fun math problem to solve.
  # TODO: build in check that colnames match. Add warning if any extra columns in lookup table and say
  # that they will be unused, listing the colnames.
  
  # Rename the output_column  to default, if set to null
  if(is.null(output_column)) {
    output_column <- paste0(input_column, "_bucket")
  }
  
  # Check if the lookup table is empty
  if((lookup |> count() |> collect())$n == 0) {
    
    # Add a new column called output_column and assign all NA
    result <- data |>
      mutate(!!sym(output_column) := NA)
    
  } else {
    
  result <- data |>
    # For every unique row of data, a new row is generated combining it with the lookup table
    cross_join(lookup) |>
    # Then only the rows of the lookup table that match the specified data are kept.
    # Note that this logic means that if the lookup table has overlapping ranges 
    # that both match the data, it will produce duplicate entries for the same individual.
    filter(!!sym(input_column) >= lower_bound & !!sym(input_column) < upper_bound) |>
    select(-lower_bound, -upper_bound) |> # Clean up extra columns
    rename(!!sym(output_column) := bucket_name)
  }
  
  return(result)
}


join_columns <- function(
    data1,   # The deprioritized dataset
    data2,   # The prioritized dataset
    column,  # The name of the data column being joined (as a string)
    id       # The name of the column uniquely identifying observations (as a string)
){
  # Join the two datasets on the id column
  result <- data1 |>
    left_join(data2, by = id, suffix = c("_data1", "_data2")) |>
    mutate(
      # Use the value from data2 if it's not NA; otherwise, use the value from data1
      !!sym(column) := coalesce(!!sym(paste0(column, "_data2")), !!sym(paste0(column, "_data1")))
    ) |>
    # Remove the two versions of `column` used for the join, so that only
    # the merged column remains
    select(-!!sym(paste0(column, "_data1")), -!!sym(paste0(column, "_data2"))) |>
    # Remove all `_data2` suffixed columns to delete all other duplicates
    # Note: It would have been equally fine to remove all `_data1` columns instead,
    # since we expect that these two input datasets were exactly identical except
    # for the column called `column`
    select(-ends_with("_data2")) |>
    # Rename the columns ending in "_data1" back to their original names
    rename_with(~ gsub("_data1$", "", .), ends_with("_data1"))
    
  
  return(result)
}


value_match_lookup <- function(
    data, # A dataframe, tibble, or db object containing the data
    lookup, # A dataframe, tibble, or db object containing the lookup table
    input_column, # The name of the column from `data` to be bucketed
    output_column = NULL # optional: the name of the output column. Default: {input_column}_bucket
) {
  # TODO: build in data check on input object types being consistent w/ one another (db,db) or (df, df)
  # TODO: keep only the "specific_value" and "bucket_name"
  
  # Rename the output_column  to default, if set to null
  if(is.null(output_column)) {
    output_column <- paste0(input_column, "_bucket")
  }
  
  result <- data |>
    # For every unique row of data, a new row is generated combining it with the lookup table
    left_join(lookup, by = setNames("specific_value", input_column)) |>
    rename(!!sym(output_column) := bucket_name)
  # Then only the rows of the lookup table that match the specified data are kept.
  # Note that this logic means that if the lookup table has overlapping ranges 
  # that both match the data, it will produce duplicate entries for the same individual.
  
  return(result)
}


# A function that reads a lookup table from a csv file path and splits it into 
# a list with two attributes: 
# $value - a tibble containing a value lookup table. This table can be fed directly
# as the `lookup` argument in value_match_lookup()
# $range - a tibble containing the range lookup table. This table can be fed directly
# as the `lookup` argument in range_match_lookup()
# The source lookup table must have four columns:
# bucket_name
# lower_bound
# upper_bound
# specific_value
split_lookup_table <- function(
    filepath # Path to the .csv file
) {
  # Read the CSV file into a tibble
  lookup_raw <- read_csv(filepath, show_col_types = FALSE)
  
  # Separate rows into range-based and value-based lookups
  range_lookup <- lookup_raw %>%
    filter(!is.na(lower_bound) & !is.na(upper_bound)) %>%
    select(bucket_name, lower_bound, upper_bound)
  
  value_lookup <- lookup_raw %>%
    filter(!is.na(specific_value)) %>%
    select(bucket_name, specific_value)
  
  # Create the processed lookup as a list with two component tibbles: value and range
  lookup_processed <- list(
    value = value_lookup,
    range = range_lookup
  )
  
  # Return the result 
  return(lookup_processed)
}


# Function adding a bucketed column to a database (referenced, optionally,
# through a lazy database reference) using value and range lookup rules specified
# in a lookup table CSV.
append_bucket_column <- function(
    con,             # Database connection
    filepath,        # Path to the lookup table CSV file
    data,            # Lazy database reference (input data)
    input_column,    # The column to be bucketed/matched
    id_column        # The unique ID column
) {
  # Step 1: Split the lookup table into range and value components
  lookup_tb <- split_lookup_table(filepath)
  
  # Step 2: Write the lookup tables to the database
  dbWriteTable(con, paste0(input_column, "_lookup_range"), lookup_tb$range, overwrite = TRUE, temporary = TRUE)
  dbWriteTable(con, paste0(input_column, "_lookup_value"), lookup_tb$value, overwrite = TRUE, temporary = TRUE)
  
  # Step 3: Create lazy references to these tables
  lookup_range_db <- tbl(con, paste0(input_column, "_lookup_range"))
  lookup_value_db <- tbl(con, paste0(input_column, "_lookup_value"))
  
  # Step 4: Perform range and value lookups
  data1_db <- range_match_lookup(
    data = data,
    lookup = lookup_range_db,
    input_column = input_column
  )
  
  data2_db <- value_match_lookup(
    data = data,
    lookup = lookup_value_db,
    input_column = input_column
  )
  
  # Step 5: Join the results
  result <- join_columns(
    data1 = data1_db,
    data2 = data2_db,
    column = paste0(input_column, "_bucket"),
    id = id_column
  )
  
  # Return lazy database reference (result)
  return(result)
}


# The purpose of this function is to take inputs from the RACE_bucket and
# HISPAN_bucket columns as defined in the race_buckets00 and hispan_buckets00
# lookup files and use them to generate a joint race/ethnicity column.
race_eth_bucket <- function(data) {
  
  # Check if the RACE_bucket and HISPAN_bucket columns exist in the data frame or database table
  if (!("RACE_bucket" %in% colnames(data))) {
    stop("Column RACE_bucket not found in the data.")
  }
  if (!("HISPAN_bucket" %in% colnames(data))) {
    stop("Column HISPAN_bucket not found in the data.")
  }
  
  # Create the RACE_ETH_bucket column using dplyr's mutate and case_when
  result <- data %>%
    mutate(
      RACE_ETH_bucket = case_when(
        HISPAN_bucket == "hispanic" ~ "Hispanic",
        RACE_bucket == "black" ~ "Black",
        RACE_bucket == "aapi" ~ "AAPI",
        RACE_bucket == "aian" ~ "AIAN",
        RACE_bucket == "multi" ~ "Multiracial",
        RACE_bucket == "white" ~ "White",
        RACE_bucket == "other" ~ "Other",
        TRUE ~ NA_character_  # For any unmatched cases, set NA
      )
    )
  
  return(result)
}

