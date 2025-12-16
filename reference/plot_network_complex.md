# Plot Network: complex

Plot Network: complex

## Usage

``` r
plot_network_complex(network, config, show_labels = FALSE)
```

## Arguments

- network:

  tibble with water cycle flow network data, as retrieved by
  `{`[`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)`}`

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- show_labels:

  show labels (default: FALSE)

## Value

complex network graph
