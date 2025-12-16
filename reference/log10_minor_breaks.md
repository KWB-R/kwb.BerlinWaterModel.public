# Generate minor tick marks for a log10 axis

Returns a function that computes minor tick positions for a log10-scaled
axis. This is useful in conjunction with `scale_y_log10()` or
`scale_x_log10()` in `ggplot2`, where minor breaks are not added by
default.

## Usage

``` r
log10_minor_breaks(minor_base = 1:9)
```

## Arguments

- minor_base:

  A numeric vector of factors (default is 1:9) to be multiplied by each
  power of 10. This defines which intermediate ticks (e.g., 2, 3,
  ..., 9) are included between each decade (e.g., between 0.1 and 1).

## Value

A function that takes a numeric range and returns a numeric vector of
minor tick positions.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot2::scale_y_log10(
  limits = c(0.01, 100),
  breaks = c(0.01, 0.1, 1, 10, 100),
  minor_breaks = kwb.BerlinWaterModel.public::log10_minor_breaks()
)
} # }
```
