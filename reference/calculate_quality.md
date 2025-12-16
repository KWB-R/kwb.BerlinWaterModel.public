# Calculate Quality in Section

Calculate Quality in Section

## Usage

``` r
calculate_quality(
  df,
  input,
  shares_timeseries_wide = NULL,
  flows,
  quality = NULL,
  config,
  reverse_flow = FALSE,
  result_type = "list",
  debug = FALSE
)
```

## Arguments

- df:

  data frame with model as retrieved by
  [`get_flowpath_table`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/get_flowpath_table.md)

- input:

  model input data flows as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)
  and sublist "flows"

- shares_timeseries_wide:

  shares timeseries in wide format, as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)
  and sublist "shares_timeseries" (default: NULL), only used if
  parameter "use_dynamic" is set to TRUE

- flows:

  flows as retrieved by
  [`calculate_flows`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/calculate_flows.md)

- quality:

  list with sublist "conc" and "load" for all sections (default: NULL)

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- reverse_flow:

  calculate reverse flow (default: FALSE)

- result_type:

  define how the results should be returned. either a tibble with loads
  and concentrations in long format.(if result_type == "tibble"), a list
  with concentrations in wide format (result_type == "list") or a list
  with loads in wide format (result_type == "load")

- debug:

  print debug messages (default: FALSE)

## Value

tibble with Urban Water Model results with loads and concentrations (if
result_type == "raw"), a list with concentrations (result_type == "conc"
or "load") or a list with loads, default: "list"
