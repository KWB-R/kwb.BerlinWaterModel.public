# Calculate Flow

Calculate Flow

## Usage

``` r
calculate_flow(
  df,
  input,
  shares_timeseries_wide = NULL,
  config,
  use_dynamic = FALSE,
  return_inputs = FALSE,
  debug = TRUE
)
```

## Arguments

- df:

  data frame with model as retrieved by
  [`get_flowpath_table`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/get_flowpath_table.md)

- input:

  input flows as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)
  and sublist "flows"

- shares_timeseries_wide:

  shares timeseries in wide formate, as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)
  and sublist "shares_timeseries" (default: NULL), only used if
  parameter "use_dynamic" is set to TRUE

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- use_dynamic:

  for multiple outputs only: should static shares (as defined in column
  "section_out_share" of "outflows_multiple.csv") be used for separating
  the flow within a section or a function (as defined in column
  "section_out_function" of "outflows_multiple.csv")), (default: FALSE)

- return_inputs:

  should also input data be returned in result dataset (default: FALSE)

- debug:

  print debug messages (default: TRUE)

## Value

tibble with Urban Water Model results
