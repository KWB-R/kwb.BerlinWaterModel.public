# Configuration: Read

Configuration: Read

## Usage

``` r
config_read(
  config_dir = system.file("extdata/config/network_complete", package =
    "kwb.BerlinWaterModel.public"),
  file_encoding = "UTF-8"
)
```

## Arguments

- config_dir:

  directory with configuration files (default:
  system.file("extdata/config/network_complete", package =
  "kwb.BerlinWaterModel")). It is mandatory that there are three files
  within this folder: "flows_in_out.csv", "outflows_multiple.csv" and
  "sections.csv"

- file_encoding:

  encoding for reading the files (default: "UTF-8")

## Value

list with three sublists "flows_in_out", "outflows_multiple" and
"sections

## Examples

``` r
if (FALSE) config <- kwb.BerlinWaterModel.public::config_read()
config # \dontrun{}
#> Error: object 'config' not found
```
