# Add tracers

Add tracers

## Usage

``` r
add_tracers(config)
```

## Arguments

- config:

  config object as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

## Value

adds new columns 'conc_tracer.xxx' for four tracers ('CSO', 'inlet',
'rain_runoff' and 'WWTP') to flows_in_out for tracking sources withint
the water cycle
