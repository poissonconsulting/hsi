# Check Transect

A transect data frame is a data frame with Distance column and a Habitat
column.

## Usage

``` r
check_transect(
  x,
  distance = "Distance",
  habitat = "Habitat",
  unique = TRUE,
  sorted = unique,
  x_name = substitute(x)
)
```

## Arguments

- x:

  A vector of habitat values

- distance:

  A string of the name of the column with distance values.

- habitat:

  A string of the name of the column with habitat values.

- unique:

  A flag indicating whether the distance values must be unique.

- sorted:

  A flag indicating whether the distance values must be sorted.

- x_name:

  A string of the name of object x or NULL.

## Value

An invisible copy of the original object.

## Examples

``` r
check_transect(trans_data)
```
