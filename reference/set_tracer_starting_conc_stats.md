# Set Tracer Starting Concentrations Statistics

Set Tracer Starting Concentrations Statistics

## Usage

``` r
set_tracer_starting_conc_stats(
  config,
  qualities,
  aggregation_function = median,
  minimum_tracer_sum = 0.999
)
```

## Arguments

- config:

  config list structure as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- qualities:

  qualities list result structure

- aggregation_function:

  function used for data aggregation (default: median)

- minimum_tracer_sum:

  minimum tracer sum (default: 0.999 i.e. 99.9 percent) for filtering
  out results for with tracer has not reached almost 100 percent

## Value

config list with starting concentrations (defined in config\$sections)
based on qualities
