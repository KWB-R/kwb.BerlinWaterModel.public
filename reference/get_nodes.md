# Helper function: get nodes

Helper function: get nodes

## Usage

``` r
get_nodes(network, config)
```

## Arguments

- network:

  tibble with water cycle flow network data, as retrieved by
  `{`[`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)`}`

- config:

  config as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

## Value

tibble with columns name (from_name) and group (source_group)
