# Check outflow multiple dynamic functions

Check outflow multiple dynamic functions

## Usage

``` r
check_outflow_multiple_dynamic_functions(
  config,
  q = 1,
  allowed_relative_offset_percent = 0.001
)
```

## Arguments

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- q:

  total section flow used for testing, needs to be a scalar! (default:
  1)

- allowed_relative_offset_percent:

  maximum allowed percental offset for sum of all section outflows
  compared to section inflow (i.e. parameter q), default: 0.0001)

## Value

nothingt if check passes for all outflow_id s with defined functions,
otherwise error
