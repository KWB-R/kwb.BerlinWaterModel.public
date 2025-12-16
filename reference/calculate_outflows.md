# Calculate Outflows

Calculate Outflows

## Usage

``` r
calculate_outflows(
  list_order,
  input,
  config,
  use_dynamic = TRUE,
  return_inputs
)
```

## Arguments

- list_order:

  list for each order id (corresponding to same level of distance of
  section from selected outflow)

- input:

  input as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- use_dynamic:

  for multiple outputs only: should static shares (as defined in column
  "section_out_share" of "outflows_multiple.csv") be used for separating
  the flow within a section or a function (as defined in column
  "section_out_function" of "outflows_multiple.csv")), (default: TRUE)

- return_inputs:

  should also input data be returned in result dataset (default: FALSE)

## Value

tibble with outflows added to provided dataset
