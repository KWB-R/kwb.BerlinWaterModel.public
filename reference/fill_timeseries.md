# Fill Timeseries based on User Defined Intervall (days, hours, minutes or seconds)

Fill Timeseries based on User Defined Intervall (days, hours, minutes or
seconds)

## Usage

``` r
fill_timeseries(
  df,
  col_datetime = "date",
  temporal_resolution = "days",
  direction = "up"
)
```

## Arguments

- df:

  data frame with data

- col_datetime:

  column of date or datetime

- temporal_resolution:

  select one of ("days", "hours", "minutes", "seconds"), (default:
  "days")

- direction:

  in which direction should the parameter be filled ("up" or "down"),
  default: "up"

## Value

tibble with filled dates. Note that if min(daste) ==
"xxxx-xx-end-of-month-day" the min(date) is set to "xxxx-xx-01", as
monthly measured (originally summed) values were set to the last day of
the month, but are valid for the whole month
