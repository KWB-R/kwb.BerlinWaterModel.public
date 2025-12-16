# Aggregate Flows Monthly

Aggregate Flows Monthly

## Usage

``` r
aggregate_flows_monthly(flows, aggregation_function = median)
```

## Arguments

- flows:

  flows as retrieved by
  [`calculate_flows_auto`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/calculate_flows_auto.md)

- aggregation_function:

  function used for aggregation (default: median)

## Value

tibble with monthly aggregated flows data
