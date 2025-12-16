# Extract yearly KPIs from a galleries lookup table

Helper to flatten the nested `kpis$per_year` entries from
`ww_galleries_lookup` (or a similar lookup table) into a single tibble.

## Usage

``` r
get_gallery_kpis_per_year(lookup)
```

## Arguments

- lookup:

  A tibble with columns `waterworks`, `gallery` and a list-column `kpis`
  as returned by
  [`compute_gallery_kpis()`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/compute_gallery_kpis.md).

## Value

A tibble with yearly KPIs, one row per gallery and year, including the
columns `waterworks` and `gallery`.
