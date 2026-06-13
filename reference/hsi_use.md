# HSI Use

HSI Use

## Usage

``` r
hsi_use(pref, avail, habitat = "Habitat", index = "Index")
```

## Arguments

- pref:

  A hsi data frame of preference

- avail:

  A hsi data frame of availability

- habitat:

  A string of the name of the column with habitat values.

- index:

  A string of the name of the column with index values.

## Value

A hsi data frame of use for overlapping habitats.

## Examples

``` r
hsi_use(hsi_data, hsi_data)
#> # A tibble: 10 × 2
#>    Habitat Index
#>      <dbl> <dbl>
#>  1       1  0   
#>  2       2  1   
#>  3       3  1   
#>  4       4  0.25
#>  5       5  0.25
#>  6       6  0.16
#>  7       7  0.09
#>  8       8  0.04
#>  9       9  0.01
#> 10      10  0   
```
