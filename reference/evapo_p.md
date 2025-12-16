# Dataset: Daily Potential Evaporation from DWD

Daily potential evaporation based on DWD 1x1 km raster data.

## Usage

``` r
evapo_p
```

## Format

A tibble with 7,670 rows and 10 columns:

- file:

  Name of the file from the DWD https/ftp server

- date:

  Date

- year:

  Year

- month:

  Month

- day:

  Day

- mean:

  Mean (mm/d) for n raster cells (defined in n_values)

- sd:

  Standard deviation (mm/d) for n raster cells (defined in n_values)

- min:

  Minimum (mm/d) for n raster cells (defined in n_values)

- max:

  Maximum (mm/d) for n raster cells (defined in n_values)

- n_values:

  Number of selected raster cells

## Source

<https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/evapo_p/DESCRIPTION_gridsgermany_daily_evapo_p_en.pdf>
<https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/evapo_p/>
