
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hsi

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/poissonconsulting/hsi/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/hsi/actions)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/hsi/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/hsi?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

`hsi` is an R package to manipulate Habitat Suitability (HSI) data
frames. A HSI data frame is a data frame with a Habitat (integer) and
Index (double) column where the Habitat values are an increasing
continous sequence and the index values are between 0 and 1.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/hsi)

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/hsi")

To install the latest development version from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

    # install.packages("drat")
    drat::addRepo("poissonconsulting")
    install.packages("hsi")

## Code of Conduct

Please note that the hsi project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
