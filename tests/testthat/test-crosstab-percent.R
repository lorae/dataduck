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

expected_byage_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,    ~percent_standard_error,
  "r00_49",    "white",          65,              2,      15.89242054, 0.645717609,
  "r00_49",    "black",          116,             2,      28.36185819, 1.350359405,
  "r00_49",    "aapi",           228,             5,      55.74572127, 1.242531394,
  "r50plus",   "black",          106,             3,      51.70731707, 1.462216867,
  "r50plus",   "aian",           99,              2,      48.29268293, 1.462216867
)

expected_byage_combo_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,    ~percent_standard_error,
  "r00_49",    "white",          65,              2,      15.89242054, 0.645717609,
  "r00_49",    "black",          116,             2,      28.36185819, 1.350359405,
  "r00_49",    "aapi",           228,             5,      55.74572127, 1.242531394,
  "r00_49",    "aian",           0,               0,      0,           NA,   
  "r50plus",   "white",          0,               0,      0,           NA,
  "r50plus",   "black",          106,             3,      51.70731707, 1.462216867,
  "r50plus",   "aapi",           0,               0,      0,           NA,
  "r50plus",   "aian",           99,              2,      48.29268293, 1.462216867
)

expected_byrace_tb <- tribble(
  ~AGE_bucket, ~RACE_ETH_bucket, ~weighted_count, ~count, ~percent,    ~percent_standard_error,
  "r00_49",    "white",          65,              2,      100,         NA,
  "r00_49",    "black",          116,             2,      52.25225225, 2.885295159,
  "r00_49",    "aapi",           228,             5,      100,         NA,
  "r50plus",   "black",          106,             3,      47.74774775, 2.885295159,
  "r50plus",   "aian",           99,              2,      100,         NA
)

# ----- Unit tests ----- #

test_that("crosstab_percent produces correct percent results on database with every_combo = FALSE, grouped by AGE_bucket", {
  
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
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byage_tb <- expected_byage_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    select(-percent_standard_error)

  # Compare results
  expect_equal(output_tb, expected_byage_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent with estimate_with_boostrap_se produces correct percent results on database with every_combo = FALSE, grouped by AGE_bucket", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute percentages using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = tbl(con, "input"),
    f = crosstab_percent,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("percent"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("AGE_bucket"),
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )

  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byage_tb <- expected_byage_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_percent = percent_standard_error) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) 
  
  # Compare results
  expect_equal(output_tb, expected_byage_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent produces correct percent results on database with every_combo = TRUE, grouped by AGE_bucket", {
  
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
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byage_combo_tb <- expected_byage_combo_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    select(-percent_standard_error)
  

  # Compare results
  expect_equal(output_tb, expected_byage_combo_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent with estimate_with_boostrap_se produces correct percent results on database with every_combo = TRUE, grouped by AGE_bucket", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute percentages using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = tbl(con, "input"),
    f = crosstab_percent,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("percent"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("AGE_bucket"),
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = TRUE
  )

  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byage_combo_tb <- expected_byage_combo_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_percent = percent_standard_error) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) 
  
  # Compare results
  expect_equal(output_tb, expected_byage_combo_tb, tolerance = 1e-5)

  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent produces correct percent results on database with every_combo = FALSE, grouped by RACE_ETH_bucket", {
  
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
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byrace_tb <- expected_byrace_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    select(-percent_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_byrace_tb, tolerance = 1e-5)
  
  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent with estimate_with_boostrap_se produces correct percent results on database with every_combo = FALSE, grouped by RACE_ETH_bucket", {
  
  # Create in-memory DuckDB instance and load test input data
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  dbWriteTable(con, "input", input_tb, overwrite = TRUE)
  
  # Compute percentages using DuckDB table
  output_tb <- estimate_with_bootstrap_se(
    data = tbl(con, "input"),
    f = crosstab_percent,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("percent"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("RACE_ETH_bucket"),
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byrace_tb <- expected_byrace_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_percent = percent_standard_error) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) 

  # Compare results
  expect_equal(output_tb, expected_byrace_tb, tolerance = 1e-5)

  dbDisconnect(con, shutdown = TRUE)
})

test_that("crosstab_percent produces correct percent results on tibble with every_combo = FALSE, grouped by RACE_ETH_bucket", {

  # Compute percentages on tibble input
  output_tb <- crosstab_percent(
    data = input_tb,
    weight = "PERWT",
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("RACE_ETH_bucket")
  )
  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byrace_tb <- expected_byrace_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    select(-percent_standard_error)
  
  # Compare results
  expect_equal(output_tb, expected_byrace_tb, tolerance = 1e-5)

})

test_that("crosstab_percent with estimate_with_boostrap_se produces correct percent results on tibble with every_combo = FALSE, grouped by RACE_ETH_bucket", {
  
  # Compute percentages on tibble input
  output_tb <- estimate_with_bootstrap_se(
    data = input_tb,
    f = crosstab_percent,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:4)),
    constant = 4/80,
    se_cols = c("percent"),
    group_by = c("AGE_bucket", "RACE_ETH_bucket"),
    percent_group_by = c("RACE_ETH_bucket"),
    id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
    every_combo = FALSE
  )

  
  # Round and arrange output for comparison
  output_tb <- output_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket)
  
  expected_byrace_tb <- expected_byrace_tb |>
    arrange(AGE_bucket, RACE_ETH_bucket) |>
    # Temp: rename column name
    rename(se_percent = percent_standard_error) |>
    # Temp: replace NAs with 0 in expected output
    mutate(across(everything(), ~ replace_na(.x, 0))) 

  # Compare results
  expect_equal(output_tb, expected_byrace_tb, tolerance = 1e-5)
  
})