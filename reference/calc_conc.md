# Calculate Concentration

Calculate Concentration

## Usage

``` r
calc_conc(c_in, c_0, Q, V, k, t)
```

## Arguments

- c_in:

  c_in concentration of inflow

- c_0:

  c_0 concentration in section at t = 0

- Q:

  total inflow rate into section (m3/s)

- V:

  volume of section (m3)

- k:

  degradation parameter

- t:

  time in seconds (s)

## Value

concentration of substance at specific time

## Examples

``` r
calc_conc(c_in = 10, c_0 = 0, Q = 40000, V = 300000, k = 0, t = 1)
#> [1] 1.248267
```
