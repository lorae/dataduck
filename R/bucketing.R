# bucketing-tools.R
#
# This module contains various helper functions meant to create custom buckets 
# of continuous (such as income, age) and categorical (such as race, ethnicity)
# variables.

# Proof-of-concept matching ranges in a lookup table using non-database data
# https://stackoverflow.com/questions/75629990/lookup-table-in-r-by-matching-ranges

#' Split a Lookup Table into Range-Based and Value-Based Components
#'
#' This function reads a lookup table from a CSV file and splits it into two components: one for
#' value-based lookups and one for range-based lookups.
#'
#' @param filepath The path to the CSV file containing the lookup table.
#'
#' @return A list with two tibbles: `$value` for value-based lookups, and `$range` for range-based lookups.
#'
#' @export
split_lookup_table <- function(
    filepath # Path to the .csv file
) {
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




#' Create a Joint Race/Ethnicity Bucket
#'
#' This function combines the `RACE_bucket` and `HISPAN_bucket` columns to generate a new joint
#' `RACE_ETH_bucket` column.
#'
#' @param data A dataframe or database table with `RACE_bucket` and `HISPAN_bucket` columns.
#'
#' @return A dataframe or database table with an additional `RACE_ETH_bucket` column.
#'
#' @export
race_eth_bucket <- function(data) {
  
  # The purpose of this function is to take inputs from the RACE_bucket and
  # HISPAN_bucket columns as defined in the race_buckets00 and hispan_buckets00
  # lookup files and use them to generate a joint race/ethnicity column.
  
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

