# Extract meta information from a galleries lookup table

Helper to flatten the `meta` entries from the `kpis` list-column in
`ww_galleries_lookup` (or a similar lookup table). Each gallery gets one
row with all meta fields as columns.

## Usage

``` r
get_gallery_kpis_meta(lookup)
```

## Arguments

- lookup:

  A tibble with columns `waterworks`, `gallery` and a list-column `kpis`
  as returned by
  [`compute_gallery_kpis()`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/compute_gallery_kpis.md).

## Value

A tibble with one row per gallery and all meta information expanded into
regular columns (e.g. `temporal_resolution`,
`synthetic_daily_from_month`, `has_daily_extremes`, ...), including
`waterworks` and `gallery`.
