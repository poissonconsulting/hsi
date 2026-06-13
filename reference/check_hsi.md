# Check HSI Data

A HSI data frame is a data frame with a Habitat column and Index column.

## Usage

``` r
check_hsi(
  x,
  habitat = "Habitat",
  index = "Index",
  by = hsi_by(x[[habitat]]),
  unique = TRUE,
  sorted = unique,
  x_name = substitute(x)
)
```

## Arguments

- x:

  A vector of habitat values

- habitat:

  A string of the name of the column with habitat values.

- index:

  A string of the name of the column with index values.

- by:

  A number of the increments.

- unique:

  A flag indicating whether the habitat values must be unique.

- sorted:

  A flag indicating whether the habitat values must be sorted.

- x_name:

  A string of the name of object x or NULL.

## Value

An invisible copy of the original object.

## Examples

``` r
check_hsi(hsi_data)
```
