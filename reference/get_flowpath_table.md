# Get Flowpath Table

Get Flowpath Table

## Usage

``` r
get_flowpath_table(outflow_id, network, config)
```

## Arguments

- outflow_id:

  id of outflow section

- network:

  tibble with water cycle flow network data, as retrieved by
  `{`[`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)`}`

- config:

  config

## Value

list for each order id containing tibble with all sections of same order
id
