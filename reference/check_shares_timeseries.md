# Check Shares of Multiple Outflow Time Series

Check Shares of Multiple Outflow Time Series

## Usage

``` r
check_shares_timeseries(shares_timeseries, config, debug = TRUE)
```

## Arguments

- shares_timeseries:

  shares timeseries dataset (default:
  kwb.BerlinWaterModel.public::shares_timeseries)

- config:

  model network configuration (as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md))

- debug:

  print debug messages (default: TRUE)

## Value

prints debug messages or stop in case of errors

## Examples

``` r
 config <- kwb.BerlinWaterModel.public::config_read()
#> Error in kwb.BerlinWaterModel.public::config_read(): all(valid_config_files %in% filenames_without_extension) is not TRUE
 check_shares_timeseries(
 shares_timeseries = kwb.BerlinWaterModel.public::shares_timeseries,
 config = config
 )
#> Error: object 'config' not found
```
