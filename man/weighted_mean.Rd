% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation.R
\name{weighted_mean}
\alias{weighted_mean}
\title{Calculate Weighted Mean}
\usage{
weighted_mean(data, value_column, weight_column, group_by_columns)
}
\arguments{
\item{data}{A data frame or a database connection object. The data containing the
value, weight, and grouping columns.}

\item{value_column}{A string specifying the name of the column containing the values
to be averaged.}

\item{weight_column}{A string specifying the name of the column containing the weights.}

\item{group_by_columns}{A character vector of column names to group by.}
}
\value{
A tibble or database connection object containing the group-by columns, count of observations, sum of weights,
and the calculated weighted mean for each group.
}
\description{
This function calculates the weighted mean, sum of weights, and count of observations
for a given value column, grouped by specified columns. The function can handle
both data frames and database pointers.
}
