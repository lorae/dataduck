library("testthat")
library("readr")
library("rlang")
library("rprojroot")
library("DBI")
library("duckdb")
library("tibble")
library("dplyr")
library("tidyr")

# Set working directory to project root
root <- find_root(is_rstudio_project)
setwd(root)

# Load the dataduck package
devtools::load_all("../dataduck")

# Read test data from pre-computed CSVs
input_tb <- read_csv("tests/test-data/crosstab-mean-inputs.csv")

expected_combo_tb <- tribble(
  ~AGE_bucket,  ~RACE_ETH_bucket, ~weighted_count, ~count, ~standard_error,
  "r00_49",     "white",           65,              2,     1.774823935,
  "r00_49",     "black",           116,             2,     7.529940239,
  "r00_49",     "aapi",            228,             5,     6.659579566,
  "r00_49",     "aian",            0,               0,     NA,
  "r50plus",    "white",           0,               0,     NA,
  "r50plus",    "black",           106,             3,     5.263078947,
  "r50plus",    "aapi",            0,               0,     NA,
  "r50plus",    "aian",            99,              2,     2.121320344
)
expected_tb <- tribble(
  ~AGE_bucket,  ~RACE_ETH_bucket, ~weighted_count, ~count, ~standard_error,
  "r00_49",     "white",           65,              2,     1.774823935,
  "r00_49",     "black",           116,             2,     7.529940239,
  "r00_49",     "aapi",            228,             5,     6.659579566,
  "r50plus",    "black",           106,             3,     5.263078947,
  "r50plus",    "aian",            99,              2,     2.121320344
)


# ----- Unit tests ----- #

test_that("crosstab_count produces correct count results on database, with `every_combo` set to TRUE.", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- crosstab_count(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    repwts = paste0("REPWTP", sprintf("%d", 1:4)),
    every_combo = TRUE
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)

  # Compare results
  expect_equal(output_tb, expected_combo_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_count produces correct count results on database, with `every_combo` set to FALSE.", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- crosstab_count(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    repwts = paste0("REPWTP", sprintf("%d", 1:4)),
    every_combo = FALSE
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_count produces correct count results on tibble, with `every_combo` set to TRUE.", {
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- crosstab_count(
    data = input_tb,
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    repwts = paste0("REPWTP", sprintf("%d", 1:4)),
    every_combo = TRUE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb)

})

test_that("crosstab_count produces correct count results on tibble, with `every_combo` set to FALSE.", {

  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- crosstab_count(
    data = input_tb,
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    repwts = paste0("REPWTP", sprintf("%d", 1:4)),
    every_combo = FALSE
  ) 
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    mutate(standard_error = round(standard_error, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_tb)

})
