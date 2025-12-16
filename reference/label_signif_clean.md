# Format numeric values with significant digits and minimal trailing zeros

This function formats numeric values to a specified number of
significant digits, avoiding scientific notation and unnecessary
trailing zeros. Useful for plotting labels where readability is
important across varying numeric magnitudes.

## Usage

``` r
label_signif_clean(x, digits = 2)
```

## Arguments

- x:

  Numeric vector of values to be formatted.

- digits:

  Integer. Number of significant digits to retain (default is 2).

## Value

A character vector of formatted numbers.

## Examples

``` r
label_signif_clean(c(0.0001234, 0.0456, 1.23, 12.3, 123.4))
#> [1] "0.00012" "0.046"   "1.2"     "12"      "120"    
```
