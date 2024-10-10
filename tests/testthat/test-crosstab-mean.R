library("testthat")
library("readr")
library("rlang")
library("rprojroot")
library("DBI")
library("duckdb")
library("tibble")
library("dplyr")

# Set working directory to project root
root <- find_root(is_rstudio_project)
setwd(root)

# Load the dataduck package
devtools::load_all("../dataduck")

# Read test data from pre-computed CSVs
input_mean_tb <- read_csv("tests/test-data/crosstab-mean-inputs.csv")
expected_mean_tb <- read_csv("tests/test-data/crosstab-mean-expected.csv")

# input_percent_tb <- read_csv("tests/test-data/crosstab-percent-inputs.csv")
expected_percent_tb <- read_csv("tests/test-data/crosstab-percent-expected.csv")

# ----- Unit tests ----- #

test_that("crosstab produces correct weighted mean results on database", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_mean_tb, overwrite = TRUE)
  
  # Compute weighted mean using DuckDB table
  output_mean_tb <- crosstab(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX"),
    value = "NUMPREC"
  ) |> collect()
  
  # Round and arrange output for comparison
  output_mean_tb <- output_mean_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket, SEX)
  
  expected_mean_tb <- expected_mean_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket, SEX)
  
  # Compare results
  expect_equal(output_mean_tb, expected_mean_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab produces correct weighted mean results on tibble", {
  
  # Compute weighted mean using a tibble
  output_mean_tb <- crosstab(
    data = input_mean_tb,
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX"),
    value = "NUMPREC"
  )
  
  # Round and arrange output for comparison
  output_mean_tb <- output_mean_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket, SEX)
  
  expected_mean_tb <- expected_mean_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket, SEX)
  
  # Compare results
  expect_equal(output_mean_tb, expected_mean_tb)
})

test_that("crosstab produces correct percentage results on database", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_mean_tb, overwrite = TRUE)
  
  # Compute weighted mean using DuckDB table
  output_percent_tb <- crosstab(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX"),
    percent_group_by = c("AGE_bucket", "RACE_ETH_bucket")
  ) |> collect()
  
  # Round and arrange output for comparison
  output_percent_tb <- output_percent_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_percent_tb <- expected_percent_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  print(output_percent_tb)
  print(expected_percent_tb)
  expect_equal(output_percent_tb, expected_percent_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})
