# Transect Set By

Transect Set By

## Usage

``` r
hsi_transect_set_by(
  x,
  distance = "Distance",
  habitat = "Habitat",
  by = hsi_by(x[[distance]])
)
```

## Arguments

- x:

  A vector of habitat values

- distance:

  A string of the name of the column with distance values.

- habitat:

  A string of the name of the column with habitat values.

- by:

  A number of the increments.

## Value

A HSI data.

## Examples

``` r
hsi_transect_set_by(trans_data)
#> # A tibble: 11 × 2
#>    Distance Habitat
#>       <dbl>   <dbl>
#>  1      1     10   
#>  2      1.5   10.0 
#>  3      2     10.1 
#>  4      2.5   10.2 
#>  5      3     10.2 
#>  6      3.5   10.1 
#>  7      4     10   
#>  8      4.5    9.96
#>  9      5      9.93
#> 10      5.5    9.89
#> 11      6      9.85
```
