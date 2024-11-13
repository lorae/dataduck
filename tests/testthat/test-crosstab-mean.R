library("testthat")
library("readr")
library("rlang")
library("rprojroot")
library("DBI")
library("duckdb")
library("tibble")
library("dplyr")
library("tidyr")
library("stringr")
library("purrr")

# Set working directory to project root
root <- find_root(is_rstudio_project)
setwd(root)

# Load the dataduck package
devtools::load_all("../dataduck")

# Read test data from pre-computed CSVs
input_tb <- read_csv("tests/test-data/crosstab-mean-inputs.csv")

expected_tb <- tribble(
  ~HHINCOME_bucket, ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~weighted_mean, ~mean_standard_error,
  "r000_100k",      "r00_49",    "white",          65,              2,      2.6,            0.021406248,
  "r000_100k",      "r00_49",    "black",          116,             2,      2.612068966,    0.020800555,
  "r000_100k",      "r50plus",   "black",          106,             3,      1.877358491,    0.018993141,
  "r000_100k",      "r50plus",   "aian",           99,              2,      1.656565657,    0.013876558,
  "r100kplus",      "r00_49",    "aapi",           228,             5,      5,              0              
)

expected_combo_tb <- tribble(
  ~HHINCOME_bucket, ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~weighted_mean, ~mean_standard_error,
  "r000_100k",      "r00_49",    "white",          65,              2,      2.6,            0.021406248,
  "r000_100k",      "r00_49",    "black",          116,             2,      2.612068966,    0.020800555,
  "r000_100k",      "r00_49",    "aapi",           0,               0,      NA,             NA,
  "r000_100k",      "r00_49",    "aian",           0,               0,      NA,             NA,
  "r000_100k",      "r50plus",   "white",          0,               0,      NA,             NA,
  "r000_100k",      "r50plus",   "black",          106,             3,      1.877358491,    0.018993141,
  "r000_100k",      "r50plus",   "aapi",           0,               0,      NA,             NA,
  "r000_100k",      "r50plus",   "aian",           99,              2,      1.656565657,    0.013876558,
  "r100kplus",      "r00_49",    "white",          0,               0,      NA,             NA,
  "r100kplus",      "r00_49",    "black",          0,               0,      NA,             NA,
  "r100kplus",      "r00_49",    "aapi",           228,             5,      5,              0,     
  "r100kplus",      "r00_49",    "aian",           0,               0,      NA,             NA,
  "r100kplus",      "r50plus",   "white",          0,               0,      NA,             NA,
  "r100kplus",      "r50plus",   "black",          0,               0,      NA,             NA,
  "r100kplus",      "r50plus",   "aapi",           0,               0,      NA,             NA,
  "r100kplus",      "r50plus",   "aian",           0,               0,      NA,             NA
)


# ----- Unit tests ----- #

test_that("crosstab_mean produces correct weighted mean results on database with every_combo = FALSE", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
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
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    select(-mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_mean with estimate_with_boostrap_se produces correct results on database, with `every_combo` set to FALSE.", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = tbl(con, "input"),
    f = crosstab_mean,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("weighted_mean"),
    id_cols = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE,
    value = "NUMPREC"
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_weighted_mean = mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
  
})

test_that("crosstab_mean produces correct weighted mean results on database with every_combo = TRUE", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
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
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    select(-mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_mean with estimate_with_boostrap_se produces correct results on database, with `every_combo` set to TRUE.", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = tbl(con, "input"),
    f = crosstab_mean,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("weighted_mean"),
    id_cols = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE,
    value = "NUMPREC"
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_weighted_mean = mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
  
})

test_that("crosstab_mean produces correct weighted mean results on tibble with every_combo = FALSE", {

  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_mean(
    data = input_tb,
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    select(-mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)

})

test_that("crosstab_mean with estimate_with_boostrap_se produces correct results on tibble, with `every_combo` set to FALSE.", {
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = input_tb,
    f = crosstab_mean,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("weighted_mean"),
    id_cols = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE,
    value = "NUMPREC"
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_weighted_mean = mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)
  
})

test_that("crosstab_mean produces correct weighted mean results on tibble with every_combo = TRUE", {

  # Compute weighted mean using DuckDB table
  output_tb <- crosstab_mean(
    data = input_tb,
    value = "NUMPREC",
    weight = "PERWT",
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) 
  
  expected_combo_tb <- expected_combo_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    select(-mean_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)

})

test_that("crosstab_mean with estimate_with_boostrap_se produces correct results on tibble, with `every_combo` set to TRUE.", {
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = input_tb,
    f = crosstab_mean,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("weighted_mean"),
    id_cols = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    group_by = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE,
    value = "NUMPREC"
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    arrange(HHINCOME_bucket, AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_weighted_mean = mean_standard_error)

  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)
  
})



