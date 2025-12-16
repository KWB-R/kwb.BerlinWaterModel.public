# Get Bank Filtration shares Model input

Get Bank Filtration shares Model input

## Usage

``` r
get_bfshares(
  config,
  ww = kwb.BerlinWaterModel.public::ww,
  temporal_resolution = "days",
  bfshare_dynamic = TRUE
)
```

## Arguments

- config:

  model network configuration (as retrieved by
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md))

- ww:

  waterworks dataset (default: kwb.BerlinWaterModel.public::ww)

- temporal_resolution:

  specify temporal resolution of model input dataset. (default: "days").
  Valid options are: "days" or "hours"

- bfshare_dynamic:

  should dynamic bankfiltration shares be used or the static ones
  contained in column "bank_filtration_share" of config\$flows_in_out

## Value

data framer with flow ids, date/datetime and well gallery metadata,
column "bank_filtration_share" is set depending on bfshare_dynamic ==
TRUE (based on column "bank_filtration_share_dynamic") or FALSE
(bank_filtration_share_static )
