# Compute abstraction metrics per gallery

Computes yearly, monthly and aggregated abstraction metrics for a single
gallery (Brunnengalerie).

## Usage

``` r
compute_gallery_kpis(
  df,
  gallery_col = "gallery",
  date_col = "date",
  q_col = "cbm_per_day",
  temporal_resolution = c("auto", "daily", "monthly"),
  date_min = NULL,
  date_max = NULL
)
```

## Arguments

- df:

  A `data.frame` containing at least gallery ID, date and abstraction
  rates (e.g. in \\m^3/d\\) for a single gallery.

- gallery_col:

  Name of the gallery column (default: `"gallery"`).

- date_col:

  Name of the date column (default: `"date"`). The column must be
  coercible to `Date`.

- q_col:

  Name of the abstraction column (e.g. `"cbm_per_day"`, interpreted as
  \\m^3/d\\).

- temporal_resolution:

  Character string specifying the temporal resolution, one of `"auto"`,
  `"daily"` or `"monthly"`. If set to `"auto"`, the resolution is
  inferred from the median time step between consecutive dates.

- date_min:

  Optional start date of the observation period. If `NULL` (default),
  the minimum of the date column is used.

- date_max:

  Optional end date of the observation period. If `NULL` (default), the
  maximum of the date column is used.

## Value

A named `list` with the following elements:

- per_year:

  A tibble of yearly metrics per gallery and year, including yearly
  volumes (e.g. `QA`, `NQMonat`, `HQMonat`, `Q365`, `Q1Ist`, `Q30`) as
  well as the number of months with data (`n_months_with_data`), the
  number of months in operation (`n_months_in_operation`), the length of
  the observation period in months (`n_months_observation_period`) and
  the corresponding percentages (`n_months_operation_with_data_percent`,
  `n_months_operation_within_observationperiod_percent`).

- per_gallery:

  A tibble of aggregated metrics per gallery over the full observation
  period (e.g. `MQA`, `NQA`, `HQA`, `MNQA`, `NNQA`, `MHQA`, `HHQA`,
  `MQ365`, `HQ365`, `MQ1Ist`, `MQ30`), together with
  `n_months_with_data`, `n_months_in_operation`,
  `n_months_observation_period`, `n_months_operation_with_data_percent`
  and `n_months_operation_within_observationperiod_percent`.

- per_month:

  A tibble of monthly metrics per gallery and calendar month aggregated
  over all years (e.g. `MQMonat`, `NQMonat`, `HQMonat`), plus the number
  of months with data (`n_months_with_data`), the number of months in
  operation (`n_months_in_operation`), the observation-period length in
  months (`n_months_observation_period`) and the corresponding
  percentages (`n_months_operation_with_data_percent`,
  `n_months_operation_within_observationperiod_percent`).

- meta:

  A named list with meta information such as `temporal_resolution`,
  `synthetic_daily_from_month`, `has_daily_extremes`, the vector
  `requires_real_daily_for = c("Q1Ist", "MQ1Ist", "Q30", "MQ30")`, and
  the effective `date_min`, `date_max` and
  `n_months_observation_period`.

## Details

The function automatically detects whether daily values were
synthetically generated from monthly data, e.g. by completing a daily
grid and applying `tidyr::fill(direction = "up")`. In that case, all
volume-based yearly and monthly metrics are computed, but daily extreme
metrics (`Q1Ist`, `MQ1Ist`, `Q30`, `MQ30`) are suppressed and returned
as `NA`, because they would not represent real daily extremes.

Months with `NA` or zero abstraction are excluded from the calculation
of monthly-based KPIs (e.g. NQMonat, HQMonat, MQMonat). Additional size
indicators are provided: `n_months_with_data`, `n_months_in_operation`
and `n_months_observation_period` per gallery, as well as two percentage
indicators describing the share of operational months within the
observed data and within the full observation period.
