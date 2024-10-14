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

# ----- Unit tests ----- #

test_that("crosstab_mean produces correct weighted mean results on database with every_combo = FALSE", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_mean_tb, overwrite = TRUE)
  
  # Compute weighted mean using DuckDB table
  output_mean_tb <- crosstab_mean(
    data = tbl(con, "input"),
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX"),
    every_combo = FALSE
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
