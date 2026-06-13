# HSI Use

HSI Use

## Usage

``` r
hsi_pref(use, avail, habitat = "Habitat", index = "Index")
```

## Arguments

- use:

  A hsi data frame of use

- avail:

  A hsi data frame of availability

- habitat:

  A string of the name of the column with habitat values.

- index:

  A string of the name of the column with index values.

## Value

A hsi data frame of preference for overlapping habitats.

## Examples

``` r
hsi_pref(hsi_data, hsi_data)
#> # A tibble: 8 × 2
#>   Habitat Index
#>     <dbl> <dbl>
#> 1       2     1
#> 2       3     1
#> 3       4     1
#> 4       5     1
#> 5       6     1
#> 6       7     1
#> 7       8     1
#> 8       9     1
```
