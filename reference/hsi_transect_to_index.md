# Transect to HSI Index

Transect to HSI Index

## Usage

``` r
hsi_transect_to_index(
  x,
  distance = "Distance",
  habitat = "Habitat",
  by = 1,
  n = 10^6
)
```

## Arguments

- x:

  A data frame.

- distance:

  A string of the column name with transect distance values.

- habitat:

  A string of the column name with habitat values.

- by:

  A number of the increments.

- n:

  A count of the number of samples to take.

## Value

A hsi data frame

## Examples

``` r
hsi_transect_to_index(trans_data)
#> # A tibble: 4 × 2
#>   Habitat Index
#>     <dbl> <dbl>
#> 1       8 0    
#> 2       9 0.667
#> 3      10 1    
#> 4      11 0    
```
