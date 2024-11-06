# Input data
library(dplyr)

input <- tibble(
  per_id = c(1, 2, 3, 4, 5),
  sex = c(1, 0, 1, 1, 0),
  hhsize = c(2, 3, 2, 1, 1),
  wt = c(10, 12, 15, 30, 20),
  repwt01 = c(11, 13, 16, 28, 22),
  repwt02 = c(8, 8, 16, 25, 22),
  repwt03 = c(2, 4, 10, 14, 13),
  repwt04 = c(18, 17, 11, 25, 15)
)

# Two example functions that we'll want to get standard errors for. For these functions
# to work in the se() function, they must have an explicit argument for weight
hhsize_by_sex <- function(
    data,
    wt_col,
    hhsize_col
    ) {
  result <- data |>
    group_by(sex) |>
    summarize(
      weighted_mean = sum(.data[[hhsize_col]] * .data[[wt_col]], na.rm = TRUE)/sum(.data[[wt_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}

count_by_sex <- function(
    data,
    wt_col
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      weighted_count = sum(.data[[wt_col]]),
      .groups = "drop"
    )
  
  return(result)
}

# Test out the functions
hhsize_by_sex(input, wt_col = "wt", hhsize_col = "hhsize")
count_by_sex(input, wt_col = "wt")
