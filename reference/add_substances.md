# Add substances

Add substances

## Usage

``` r
add_substances(config)
```

## Arguments

- config:

  config as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

## Value

adds new columns 'conc\_\<substance-name.kg.m3' and re-calculates input
concentrations from (ng\|ug\|mg\|g)/L to kg/m3
