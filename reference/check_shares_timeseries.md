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
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 2 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (4): backflow_id, section_name, section_backflow_out_id, section_backflo...
#> dbl (1): section_backflow_out_share
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 5 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> dbl (7): type, equation_a, equation_b, r2, rmse, Q_min_m3d, Q_max_m3d
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 49 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (2): waterworks, gallery
#> dbl (1): type
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 8 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (2): flow_type, id_base
#> dbl (1): conc_Valsartan.ug.L
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 176 Columns: 15
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (6): Bemerkung, ds_link_id, surface_water, section_id, section_name, CSO_id
#> dbl (9): water_body_km, y-coordinate, x-coordinate, lat_grad, lat_min, lat_s...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 105 Columns: 12
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (8): section_id, flow_id, flow_name, rainstation_id, is_mar, flow_in_out...
#> dbl (4): rain_runoff_coefficient, rain_EZG_area_m2, ABIMO_runoff_m3/a, bank_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 2 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (1): substance_name
#> dbl (2): k, bf_reduction
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 14 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (9): outflow_id, section_name, section_in_id, section_out_id, section_ou...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 43 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (3): section_id, GewaesserId, comment
#> dbl (2): start_km, Km
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 24 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (3): flow_category, flow_id, flow_type
#> dbl (1): scaling_factor
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 36 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (6): section_id, section_name, section_in_id, section_out_id, no_neg_flo...
#> dbl (3): volume_m3, area_m2, branch_id
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
 check_shares_timeseries(
 shares_timeseries = kwb.BerlinWaterModel.public::shares_timeseries,
 config = config
 )
#> Checking multiple outflows (i.e. 'share_Panke_to_BSSKanal', 'share_Panke_to_Nordgraben') time series for section 'Panke' (1/1) ... ok. (0.00 secs) 
```
