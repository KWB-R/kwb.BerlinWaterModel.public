# Calculate Flow Statistics Per Section

Computes discharge statistics from a daily or hourly flow dataset for
each river section. The function supports both daily data (using a
`date` column) and hourly data (using a `datetime` column), depending on
the `"temporal_resolution"` attribute of the input.

## Usage

``` r
calculate_flow_stats(flows)
```

## Arguments

- flows:

  A data frame containing a date or datetime column (depending on
  `attr(flows, "temporal_resolution")`) followed by one column per river
  section with discharge values.

## Value

A list with three elements:

- per_section:

  A tibble with discharge statistics per section (MQ, MNQ_years,
  MHQ_years, NNQ, HHQ).

- per_year:

  A tibble with statistics per section and year (MQ, MNQ_months,
  MHQ_months, NQ, HQ).

- per_month:

  A tibble with statistics per section and month (MQ, MNQ_same_months,
  MHQ_same_months, NQ, HQ).

## Details

The returned statistics include:

- **MQ** – Mean discharge

- **MNQ_years** – Mean of yearly minimum values (yearly low flows)

- **MHQ_years** – Mean of yearly maximum values (yearly high flows)

- **NNQ** – Overall minimum discharge (absolute low flow)

- **HHQ** – Overall maximum discharge (absolute high flow)

Additional tables contain statistics per year and per month, including
monthly mean, monthly minimum, and monthly maximum discharges.
