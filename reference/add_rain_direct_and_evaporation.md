# Add Direct Rain and Evaporation to flows_in_out config for sections with defined area_m2

Add Direct Rain and Evaporation to flows_in_out config for sections with
defined area_m2

## Usage

``` r
add_rain_direct_and_evaporation(config)
```

## Arguments

- config:

  config object as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

## Value

config with added flows_in_out for sections with defined area_m2, in
case none area_m2 is defined the original config is returned
