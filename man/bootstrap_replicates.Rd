% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard-error.R
\name{bootstrap_replicates}
\alias{bootstrap_replicates}
\title{Bootstrap Results Using Replicate Weights}
\usage{
bootstrap_replicates(
  data,
  f,
  wt_col = "weight",
  repwt_cols = paste0("repwt", 1:4),
  id_cols,
  ...
)
}
\arguments{
\item{data}{A tibble or data frame containing the data to be analyzed.}

\item{f}{A function that produces new columns or summary statistics.
The function \code{f} must have an argument named \code{wt_col} to specify the weight column.}

\item{wt_col}{A string indicating the column name to be used as the main weight.
Defaults to \code{"weight"}.}

\item{repwt_cols}{A vector of strings indicating the names of replicate weight
columns in \code{data}. Defaults to \code{paste0("repwt", 1:80)}.}

\item{...}{Additional arguments passed to the function \code{f}.}
}
\value{
A list with two elements:
\describe{
\item{main_estimate}{The result of applying \code{f()} with the main weight column.}
\item{replicate_estimates}{A list of results from applying \code{f()} with each
replicate weight column.}
}
}
\description{
Calculates results of a target function \code{f()} using a specified weight column,
and also calculates results by substituting each of the specified \code{repwt_cols}
for the \code{wt_col} argument within \code{f()}. This is useful for calculating replicate
estimates required for standard error computation using replicate weights.
}
