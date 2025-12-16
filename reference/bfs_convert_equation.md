# Bank Filtration Share: convert equation

Bank Filtration Share: convert equation

## Usage

``` r
bfs_convert_equation(
  df,
  col_equation_a = "equation_a",
  col_equation_b = "equation_b"
)
```

## Arguments

- df:

  data frame with bank filtration share equations, defined in sublist
  config\$bfstypes_equations as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- col_equation_a:

  column name of equation parameter a (default: "equation_a")

- col_equation_b:

  column name of equation parameter b (default: "equation_b")

## Value

adds "equation_function" to data frame
