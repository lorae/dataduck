# This module contains functions related to metadata creation and management.

#' Extract a Column as a Vector from a Lookup Table
#'
#' This function extracts a specified column from a lookup table (data frame or 
#' tibble) and returns it as a vector. It is meant to be used to create factor 
#' labels from data, so that graphs and charts displaying categorical data are 
#' ordered in a logical manner.
#'
#' @param lookup_table A data frame or tibble containing the data.
#' @param colname A single character string specifying the name of the column to 
#' extract.
#'
#' @return A vector containing the values from the specified column. If the column
#' is not found or the inputs are invalid, the function will throw an error.
#'
#' @details
#' The function checks whether the provided `lookup_table` is a data frame or tibble,
#' whether `colname` is a single character string, and whether `colname` exists within
#' `lookup_table`. If these checks pass, it extracts the column as a vector using `dplyr::pull`.
#'
#' @export
extract_factor_label <- function(
    lookup_table, # a tibble or data frame
    colname       # a string with the column name
) {
  # Check that lookup_table is a data frame or tibble
  if (!is.data.frame(lookup_table)) {
    stop("'lookup_table' must be a data frame or tibble.")
  }
  
  # Check that colname is a single character string
  if (!is.character(colname) || length(colname) != 1) {
    stop("'colname' must be a single character string.")
  }
  
  # Check that colname exists in lookup_table
  if (!(colname %in% colnames(lookup_table))) {
    stop(paste0("Column '", colname, "' not found in 'lookup_table'."))
  }
  
  # Extract the column as a vector
  factor_label <- lookup_table |> dplyr::pull(!!rlang::sym(colname))
  
  return(factor_label)
}


# placeholder: Function which takes as input a directory. Imports all 
# csvs in that directory and extracts factor labels based on the names.
# saves as a .rda (?rds) file in that directory as a list, with each
# named element in that list representing a vector of factor labels.
# This may be overengineering. I won't make this function right now.
