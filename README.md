# dataduck
An R package providing tools for data bucketing with lookup tables, aggregation, and validation, optimized for performance with databases like DuckDB.

# Unit Testing

Tests are located in the `tests/testthat` folder. To run all tests:
```r
library("devtools")
devtools::test()
```

# Contribution Guidelines

## Variable naming conventions
**1. Data Variables**: In this R package, variables often refer to data that is either:

  - Stored in R memory as a **tibble**.
  - Stored on a DuckDB server as a **pointer to a database table**. 
  
These two data types are handled differently, and failing to distinguish between them may lead to subtle bugs. To prevent this:

  - Variables referring to an **in-memory tibble** are suffixed with `_tb`.
  - Variables referring to an **on-server database table** are suffixed with `_db`.
  
**Example:**

``` r
library(duckdb)
library(dplyr)

# The name of `iris` data represented as a tibble is `iris_tb`
iris_tb <- tibble(iris)

# The name of `iris` data represented as a pointer to a database table is `iris_db`. But the name of the table itself, in the connection, is just `iris`.
con <- dbConnect(duckdb::duckdb(), ":memory:")
dbWriteTable(con, "iris", iris_tb, overwrite = TRUE)
iris_db <- tbl(con, "iris")
dbDisconnect(con)
```

**2. Column name arguments**: Many functions have arguments that are strings representing column names in the data. To clearly indicate that an argument represents a column name, we suffix it with `_col`.

**Example:**

``` r
library(dplyr)

# Define the function
calculate_group_average <- function(
  data, 
  group_col, 
  value_col
  ) {
  data |>
    group_by(.data[[group_col]]) |>
    summarize(average = mean(.data[[value_col]], na.rm = TRUE)) |>
    ungroup()
}

# Example usage
calculate_group_average(
  data = iris_tb,
  group_col = "Species",
  value_col = "Petal.Width"
)
```