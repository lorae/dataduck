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

expected_byage_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,
  "r00_49",    "white",          65,              2,      0.1589242054,
  "r00_49",    "black",          116,             2,      0.2836185819,
  "r00_49",    "aapi",           228,             5,      0.5574572127,
  "r50plus",   "black",          106,             3,      0.5170731707,
  "r50plus",   "aian",           99,              2,      0.4829268293
)

expected_byage_combo_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,
  "r00_49",    "white",          65,              2,      0.1589242054,
  "r00_49",    "black",          116,             2,      0.2836185819,
  "r00_49",    "aapi",           228,             5,      0.5574572127,
  "r00_49",    "aian",           0,               0,      0,
  "r50plus",   "white",          0,               0,      0,
  "r50plus",   "black",          106,             3,      0.5170731707,
  "r50plus",   "aapi",           0,               0,      0,
  "r50plus",   "aian",           99,              2,      0.4829268293
)

expected_byrace_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,
  "r00_49",    "white",          65,              2,      1,
  "r00_49",    "black",          116,             2,      0.5225225225,
  "r00_49",    "aapi",           228,             5,      1,
  "r50plus",   "black",          106,             3,      0.4774774775,
  "r50plus",   "aian",           99,              2,      1
)

# ----- Unit tests ----- #

test_that("crosstab_percent produces correct weighted mean results on database with every_combo = FALSE, grouped by AGE_bucket", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute percentages using DuckDB table
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
  
  expected_byage_tb <- expected_byage_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_byage_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent produces correct weighted mean results on database with every_combo = TRUE, grouped by AGE_bucket", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute percentages using DuckDB table
  output_tb <- crosstab_percent(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("AGE_bucket"),
    every_combo = TRUE
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byage_combo_tb <- expected_byage_combo_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_byage_combo_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent produces correct weighted mean results on database with every_combo = FALSE, grouped by RACE_ETH_bucket", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute percentages using DuckDB table
  output_tb <- crosstab_percent(
    data = tbl(con, "input"),
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("RACE_ETH_bucket")
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byrace_tb <- expected_byrace_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_byrace_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent produces correct weighted mean results on tibble with every_combo = FALSE, grouped by RACE_ETH_bucket", {

  # Compute percentages on tibble input
  output_tb <- crosstab_percent(
    data = input_tb,
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("RACE_ETH_bucket")
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byrace_tb <- expected_byrace_tb |>
    mutate(percent = round(percent, 6)) |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_byrace_tb)

})