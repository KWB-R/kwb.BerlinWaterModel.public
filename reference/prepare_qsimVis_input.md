# Preparew QsimVis Input

Preparew QsimVis Input

## Usage

``` r
prepare_qsimVis_input(config, flows, qualities, rounding_digits = 3)
```

## Arguments

- config:

  config list as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- flows:

  flows input data frame in "wide" format

- qualities:

  qualities list input data structure for each section in wide format

- rounding_digits:

  digits used for result data rounding (default: 3)

## Value

data frame with flow data in long format joined with config\$qsimVis
data frame
