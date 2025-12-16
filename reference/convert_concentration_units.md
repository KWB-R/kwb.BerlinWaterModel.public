# Convert concentration units to kg/m3

Convert concentration units to kg/m3

## Usage

``` r
convert_concentration_units(df, return_inputs = FALSE)
```

## Arguments

- df:

  data frame with "conc\_.\<g\|mg\|mg\|ng\>."

- return_inputs:

  should input data frame also be returned (default: FALSE) or only
  newly calculated columns?

## Value

tibble with "conc\_.kg.m3"

## Examples

``` r
df <- data.frame(
conc_As.ng.L = c(10, 50, 100),  # Nanogramm pro Liter
conc_Zn.ug.L = c(500, 1000, 2000),  # Mikrogramm pro Liter
conc_Fe.mg.L = c(0.1, 0.2, 0.3),    # Milligramm pro Liter
conc_Cu.g.L = c(0.005, 0.01, 0.02)  # Gramm pro Liter
)

convert_concentration_units(df, return_inputs = TRUE)
#>   conc_As.ng.L conc_Zn.ug.L conc_Fe.mg.L conc_Cu.g.L conc_As.mg.m3
#> 1           10          500          0.1       0.005          0.01
#> 2           50         1000          0.2       0.010          0.05
#> 3          100         2000          0.3       0.020          0.10
#>   conc_Zn.mg.m3 conc_Fe.mg.m3 conc_Cu.mg.m3
#> 1           500           100          5000
#> 2          1000           200         10000
#> 3          2000           300         20000
```
