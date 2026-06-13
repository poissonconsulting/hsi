# HSI Set By

HSI Set By

## Usage

``` r
hsi_set_by(x, habitat = "Habitat", index = "Index", by = hsi_by(x[[habitat]]))
```

## Arguments

- x:

  A HSI data frame.

- habitat:

  A string of the name of the column with habitat values.

- index:

  A string of the name of the column with index values.

- by:

  A number of the increments.

## Value

A HSI data.

## Examples

``` r
hsi_set_by(hsi_data, by = 2)
#> # A tibble: 6 × 2
#>   Habitat Index
#>     <dbl> <dbl>
#> 1       0   0  
#> 2       2   1  
#> 3       4   0.5
#> 4       6   0.4
#> 5       8   0.2
#> 6      10   0  
```
