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

expected_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,
  "r00_49",    "white",          65,              2,      0.1589242054,
  "r00_49",    "black",          116,             2,      0.2836185819,
  "r00_49",    "aapi",           228,             5,      0.5574572127,
  "r50plus",   "black",          106,             3,      0.5170731707,
  "r50plus",   "aian",           99,              2,      0.4829268293
)

# ----- Unit tests ----- #

test_that("crosstab_mean produces correct weighted mean results on database with every_combo = FALSE", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_percent(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("AGE_bucket")
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  print(expected_tb)
  print(output_tb)
  
  # Compare results
  expect_equal(output_tb, expected_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})