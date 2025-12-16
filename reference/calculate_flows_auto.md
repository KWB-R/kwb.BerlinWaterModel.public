# Calculate Flows Automatically

Calculate Flows Automatically

## Usage

``` r
calculate_flows_auto(
  config,
  input_list,
  network,
  use_dynamic = FALSE,
  debug = FALSE
)
```

## Arguments

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- input_list:

  input_list as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)

- network:

  tibble with water cycle flow network data, as retrieved by
  `{`[`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)`}`

- use_dynamic:

  for multiple outputs only: should static shares (as defined in column
  "section_out_share" of "outflows_multiple.csv") be used for separating
  the flow within a section or a function (as defined in column
  "section_out_function" of "outflows_multiple.csv")), (default: FALSE)

- debug:

  print debug messages (default: FALSE)

## Value

tibble with flows
