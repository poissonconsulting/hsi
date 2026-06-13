# HSI Sequence

HSI Sequence

## Usage

``` r
hsi_seq_by(x, by = hsi_by(x))
```

## Arguments

- x:

  A vector of habitat values

- by:

  A number of the increments.

## Value

A vector of the sequence

## Examples

``` r
hsi_seq_by(c(1, 2, 1.5))
#> [1] 0.5 1.0 1.5 2.0 2.5 3.0
```
