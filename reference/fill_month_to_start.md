# Expand monthly end-of-month values to all days or hours of the same month

For each row in `df`, this function assumes that the time column
contains a monthly value at (or near) the end of a month (e.g.
"2002-01-31" or "2002-01-31 23:00:00") and expands it backwards to all
days or all hours from the 1st of that month up to the given timestamp.

## Usage

``` r
fill_month_to_start(
  df,
  date_col = "date",
  datetime_col = "datetime",
  temporal_res = c("auto", "days", "hours"),
  date_min = NULL,
  date_max = NULL
)
```

## Arguments

- df:

  A data frame or tibble.

- date_col:

  Name of the daily date column (default: "date").

- datetime_col:

  Name of the hourly datetime column (default: "datetime").

- temporal_res:

  One of "auto", "days", "hours". If "auto", the presence of `date_col`
  / `datetime_col` decides.

- date_min:

  Optional lower bound of the output time window. For daily mode this is
  interpreted as `Date` (or converted via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)); for hourly mode
  it can be a `POSIXct` or `Date`. If `NULL`, no lower cropping is
  applied.

- date_max:

  Optional upper bound of the output time window. Same interpretation as
  `date_min`. If `NULL`, no upper cropping is applied.

## Value

A tibble where each original row is expanded so that all days (for
"days") or all hours (for "hours") from the 1st of the respective month
up to the original timestamp are present. All other columns are copied
to the created rows. The result is then cropped to
`[date_min, date_max]` if those are supplied. The name of the time
column is preserved ("date" or "datetime").

## Details

If `temporal_res = "auto"` (default), the function chooses daily
expansion when a column `date` is present and hourly expansion when a
column `datetime` is present.

The resulting series can optionally be cropped to a user-defined time
window via `date_min` and `date_max`.
