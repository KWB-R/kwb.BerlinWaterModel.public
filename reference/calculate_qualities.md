# Calculate Qualities

Calculate Qualities

## Usage

``` r
calculate_qualities(
  input_list,
  flows,
  network,
  config,
  reverse_flow = FALSE,
  branchwise = TRUE,
  max_sections = NULL,
  debug = TRUE
)
```

## Arguments

- input_list:

  input_list as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)

- flows:

  flows as retrieved by xxxx

- network:

  tibble with water cycle flow network data, as retrieved by
  `{`[`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)`}`

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- reverse_flow:

  calculate reverse flow (default: FALSE)

- branchwise:

  improved calculation workflow minimising unneeded section calculations
  (default: TRUE)

- debug:

  print debug messages (default: TRUE)

- max_section:

  restrict number of calculated sections in case problem occcur. Provide
  a number \<= number of sections to be calculated. If NULL, all
  sections will be calculated (default: NULL). Only used if reverse_flow
  = FALSE

## Value

returns modelled flows in tibble format
