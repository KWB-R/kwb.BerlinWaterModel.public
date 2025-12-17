# Calculate Qualities Forward

Calculate Qualities Forward

## Usage

``` r
calculate_qualities_forward(
  input_list,
  flows,
  network,
  config,
  max_sections = NULL,
  debug = TRUE
)
```

## Arguments

- input_list:

  input_list as retrieved by
  [`prepare_input`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_input.md)

- flows:

  tibble with (modelled) flows for the network (e.g. precomputed
  upstream by the user)

- network:

  tibble with water cycle flow network data, as retrieved by
  [`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- max_sections:

  restrict number of calculated sections in case problems occur. Provide
  a number \<= number of sections to be calculated. If NULL, all
  sections will be calculated (default: NULL). Only used if reverse_flow
  = FALSE

- debug:

  print debug messages (default: TRUE)

## Value

returns modelled flows in tibble format
