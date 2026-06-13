# Transect to Sample

Transect to Sample

## Usage

``` r
hsi_transect_to_sample(x, distance = "Distance", habitat = "Habitat", n = 10^6)
```

## Arguments

- x:

  A data frame.

- distance:

  A string of the column name with transect distance values.

- habitat:

  A string of the column name with habitat values.

- n:

  A count of the number of samples to take.

## Value

A vector of samples.

## Examples

``` r
hsi_transect_to_sample(trans_data, n = 10)
#>  [1] 10.000000 10.055556 10.111111 10.150000 10.116667 10.033333  9.975000
#>  [8]  9.933333  9.891667  9.850000
```
