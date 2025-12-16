# Extract aggregated KPIs per gallery from a lookup table

Helper to flatten the nested `kpis$per_gallery` entries from
`ww_galleries_lookup` (or a similar lookup table) into a single tibble.

## Usage

``` r
get_gallery_kpis_per_gallery(lookup)
```

## Arguments

- lookup:

  A tibble with columns `waterworks`, `gallery` and a list-column `kpis`
  as returned by
  [`compute_gallery_kpis()`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/compute_gallery_kpis.md).

## Value

A tibble with one row per gallery, containing all aggregated KPIs from
`kpis$per_gallery` and the identifying columns `waterworks` and
`gallery`.

## Details

This returns one row per gallery with all aggregated KPIs that were
computed over the full observation period (e.g. `MQA`, `NQA`, `HQA`,
`MNQA`, `NNQA`, `MHQA`, `HHQA`, `MQ365`, `HQ365`, `MQ1Ist`, `MQ30`,
...).

Any `waterworks` or `gallery` columns inside `per_gallery` are dropped
to avoid name clashes; the outer ID columns are kept.
