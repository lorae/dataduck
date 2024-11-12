library("testthat")
library("readr")
library("rlang")
library("rprojroot")
library("DBI")
library("duckdb")
library("tibble")
library("dplyr")
library("tidyr")
library("purrr")

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
  
  # Arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)

  expected_combo_tb <- expected_combo_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)

  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)
  
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
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_count_no_se with estimate_with_boostrap_se produces correct count results on database, with `every_combo` set to FALSE.", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = tbl(con, "input"),
    f = crosstab_count_no_se,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    se_cols = c("weighted_count"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    # Temp: rename column name
    rename(se_weighted_count = standard_error)

  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)
  
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
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)

})

test_that("crosstab_count_no_se with estimate_with_boostrap_se produces correct count results on tibble, with `every_combo` set to TRUE.", {
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = input_tb,
    f = crosstab_count_no_se,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("weighted_count"),
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_combo_tb <- expected_combo_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    # Temp: rename column name
    rename(se_weighted_count = standard_error)

  # Compare results
  expect_equal(output_tb, expected_combo_tb, tolerance = 1e-5)
  
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
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)

})

test_that("crosstab_count_no_se with estimate_with_boostrap_se produces correct count results on tibble, with `every_combo` set to FALSE.", {
  
  # Compute weighted and unweighted counts using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = input_tb,
    f = crosstab_count_no_se,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    se_cols = c("weighted_count"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_tb <- expected_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    # Temp: rename column name
    rename(se_weighted_count = standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_tb, tolerance = 1e-5)
  
})