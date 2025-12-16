#' Dataset: Daily Potential Evaporation from DWD
#'
#' Daily potential evaporation based on DWD 1x1 km raster data.
#'
#' @format A tibble with 7,670 rows and 10 columns:
#' \describe{
#'   \item{file}{Name of the file from the DWD https/ftp server}
#'   \item{date}{Date}
#'   \item{year}{Year}
#'   \item{month}{Month}
#'   \item{day}{Day}
#'   \item{mean}{Mean (mm/d) for n raster cells (defined in n_values)}
#'   \item{sd}{Standard deviation (mm/d) for n raster cells (defined in n_values)}
#'   \item{min}{Minimum (mm/d) for n raster cells (defined in n_values)}
#'   \item{max}{Maximum (mm/d) for n raster cells (defined in n_values)}
#'   \item{n_values}{Number of selected raster cells}
#' }
#' @source \url{https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/evapo_p/DESCRIPTION_gridsgermany_daily_evapo_p_en.pdf}
#'         \url{https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/evapo_p/}
"evapo_p"
