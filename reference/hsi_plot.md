# Plot HSI Data

Plot HSI Data

## Usage

``` r
hsi_plot(x, habitat = "Habitat", index = "Index")
```

## Arguments

- x:

  A HSI data frame.

- habitat:

  A string of the name of the column with habitat values.

- index:

  A string of the name of the column with index values.

## Value

A ggplot object

## Examples

``` r
hsi_plot(hsi_data)
#> Warning: Use of `x[[habitat]]` is discouraged.
#> ℹ Use `.data[[habitat]]` instead.
#> Warning: Use of `x[[index]]` is discouraged.
#> ℹ Use `.data[[index]]` instead.
```
