# Set Tracer Starting Concentrations To Final Modelling Results

Set Tracer Starting Concentrations To Final Modelling Results

## Usage

``` r
set_tracer_starting_conc(config, qualities)
```

## Arguments

- config:

  config list structure as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- qualities:

  qualities list result structure

## Value

config list with starting concentrations (defined in config\$sections)
based on qualities. The last time step is used as starting concentration
