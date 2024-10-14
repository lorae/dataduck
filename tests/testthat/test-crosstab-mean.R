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
input_mean_tb <- read_csv("tests/test-data/crosstab-mean-inputs.csv")

expected_tb <- tribble(
  ~HHINCOME_bucket, ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~weighted_mean,
  "r000_100k",      "r00_49",    "white",          65,              2,      2.6,
  "r000_100k",      "r00_49",    "black",          116,             2,      2.612068966,
  "r000_100k",      "r50plus",   "black",          106,             3,      1.877358491,
  "r000_100k",      "r50plus",   "aian",           99,              2,      1.656565657,
  "r100kplus",      "r00_49",    "aapi",           228,             5,      5
)

expected_combo_tb <- tribble(
  ~HHINCOME_bucket, ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~weighted_mean,
  "r000_100k",      "r00_49",    "white",          65,              2,      2.6,
  "r000_100k",      "r00_49",    "black",          116,             2,      2.612068966,
  "r000_100k",      "r00_49",    "aapi",           0,               0,      NA,
  "r000_100k",      "r00_49",    "aian",           0,               0,      NA,
  "r000_100k",      "r50plus",   "white",          0,               0,      NA,
  "r000_100k",      "r50plus",   "black",          106,             3,      1.877358491,
  "r000_100k",      "r50plus",   "aapi",           0,               0,      NA,
  "r000_100k",      "r50plus",   "aian",           99,              2,      1.656565657,
  "r100kplus",      "r00_49",    "white",          0,               0,      NA,
  "r100kplus",      "r00_49",    "black",          0,               0,      NA,
  "r100kplus",      "r00_49",    "aapi",           228,             5,      5,
   "r100kplus",      "r00_49",    "aian",           0,               0,      NA,
  "r100kplus",      "r50plus",   "white",          0,               0,      NA,
  "r100kplus",      "r50plus",   "black",          0,               0,      NA,
  "r100kplus",      "r50plus",   "aapi",           0,               0,      NA,
  "r100kplus",      "r50plus",   "aian",           0,               0,      NA
)


# ----- Unit tests ----- #

test_that("crosstab_mean produces correct weighted mean results on database with every_combo = FALSE", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_mean_tb, overwrite = TRUE)
  
  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_mean(
    data = tbl(con, "input"),
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_mean produces correct weighted mean results on database with every_combo = TRUE", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_mean_tb, overwrite = TRUE)
  
  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_mean(
    data = tbl(con, "input"),
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE
  ) |> collect()
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_mean produces correct weighted mean results on tibble with every_combo = FALSE", {

  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_mean(
    data = input_mean_tb,
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_tb)

})

test_that("crosstab_mean produces correct weighted mean results on tibble with every_combo = TRUE", {

  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_mean(
    data = input_mean_tb,
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    mutate(weighted_mean = round(weighted_mean, 6)) |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb)

})

