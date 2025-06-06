% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check_hsi}
\alias{check_hsi}
\title{Check HSI Data}
\usage{
check_hsi(
  x,
  habitat = "Habitat",
  index = "Index",
  by = hsi_by(x[[habitat]]),
  unique = TRUE,
  sorted = unique,
  x_name = substitute(x)
)
}
\arguments{
\item{x}{A vector of habitat values}

\item{habitat}{A string of the name of the column with habitat values.}

\item{index}{A string of the name of the column with index values.}

\item{by}{A number of the increments.}

\item{unique}{A flag indicating whether the habitat values must be unique.}

\item{sorted}{A flag indicating whether the habitat values must be sorted.}

\item{x_name}{A string of the name of object x or NULL.}
}
\value{
An invisible copy of the original object.
}
\description{
A HSI data frame is a data frame with a Habitat column
and Index column.
}
\examples{
check_hsi(hsi_data)
}
