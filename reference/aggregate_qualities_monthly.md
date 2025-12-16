# Aggregate Qualities Monthly

Aggregate Qualities Monthly

## Usage

``` r
aggregate_qualities_monthly(
  qualities,
  aggregation_function = median,
  minimum_tracer_sum = 0.999
)
```

## Arguments

- qualities:

  list as retrieved by `calcualate_qualities`

- aggregation_function:

  function used for aggregation (default: median)

- minimum_tracer_sum:

  minimum tracer sum (default: 0.999 i.e. 99.9 percent) for filtering
  out results for with tracer has not reached almost 100 percent

## Value

list with sublist "conc" with monthly aggregated concentrations data
