# Sample to Index

Sample to Index

## Usage

``` r
hsi_sample_to_index(x, by = hsi_by(x))
```

## Arguments

- x:

  A double vector of habitat values.

- by:

  The increments for the hsi Habitat values.

## Value

A hsi data frame with columns Habitat and Index.

## Examples

``` r
hsi_sample_to_index(runif(100, 1, 2), by = 0.1)
#> # A tibble: 12 × 2
#>    Habitat Index
#>      <dbl> <dbl>
#>  1     0.9 0    
#>  2     1   0.857
#>  3     1.1 0.571
#>  4     1.2 0.786
#>  5     1.3 0.786
#>  6     1.4 1    
#>  7     1.5 0.714
#>  8     1.6 0.714
#>  9     1.7 0.714
#> 10     1.8 0.429
#> 11     1.9 0.571
#> 12     2   0    
```
