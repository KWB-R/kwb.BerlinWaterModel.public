# Calculate flow share

Calculate flow share

## Usage

``` r
calculate_flow_share(
  flow,
  shares_timeseries_id,
  shares_timeseries_wide,
  debug = FALSE
)
```

## Arguments

- flow:

  vector with flows (same length as)

- shares_timeseries_id:

  id column in shares_timeseries_wide

- shares_timeseries_wide:

  tibble or data frame with shares time series data in wide format

- debug:

  print debug messages (default: FALSE)

## Value

flow vector corrected with values from shares_timeseries data.frame for
provided shares_timeseries_id
