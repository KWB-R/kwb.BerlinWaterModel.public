#' Check Shares of Multiple Outflow Time Series
#'
#' @param shares_timeseries shares timeseries dataset (default: kwb.BerlinWaterModel::shares_timeseries)
#' @param config model network configuration (as retrieved by \code{\link{config_read}})
#' @param debug print debug messages (default: TRUE)
#'
#' @returns prints debug messages or stop in case of errors
#' @export
#'
#' @examples
#'  config <- kwb.BerlinWaterModel::config_read()
#'  check_shares_timeseries(
#'  shares_timeseries = kwb.BerlinWaterModel::shares_timeseries,
#'  config = config
#'  )
#' @importFrom tidyr pivot_wider
#'
check_shares_timeseries <- function(shares_timeseries, config, debug = TRUE) {

  config_txt <- deparse(substitute(config))

if(!"section_out_timeseries_id" %in% names(config$outflows_multiple)) {
  stop(sprintf("Required column 'section_out_timeseries_id' was not found in %s$outflows multiple!\nPlease add it in config file 'outflows_multiple.csv'",
       config_txt))
  }

shares_timeseries_txt <- deparse(substitute(shares_timeseries))

shares_timeseries_wide <- shares_timeseries %>%
    tidyr::pivot_wider(names_from = "id",
                       values_from = "share")


sections_with_shares_time_series <- config$outflows_multiple %>%
  dplyr::filter(!is.na(section_out_timeseries_id))



sections <- unique(sections_with_shares_time_series$section_name)

if(length(sections) > 0) {

for(section in sections) {


section_with_shares_time_series <- sections_with_shares_time_series %>%
  dplyr::filter(section_name == section)


cols <- section_with_shares_time_series$section_out_timeseries_id

ids_in_shares_dataset <- cols %in% names(shares_timeseries_wide)


kwb.utils::catAndRun(sprintf("Checking multiple outflows (i.e. %s) time series for section '%s' (%d/%d)",
                             paste0(sprintf("'%s'",cols), collapse = ", "),
                             section,
                             which(section == sections),
                             length(sections)),
                     expr = {


if(nrow(section_with_shares_time_series) == sum(ids_in_shares_dataset)) {

  if(!all(rowSums(shares_timeseries_wide[,cols]) == 1)) {

    stop(
      sprintf("%d/%d data points for the multiple outflows (ids %s within dataset '%s') of the section '%s' do not sum up to 100%%",
            length(which(rowSums(shares_timeseries_wide[,cols]) != 1)),
            nrow(shares_timeseries_wide),
            paste0(sprintf("'%s'",cols), collapse = ", "),
            shares_timeseries_txt,
            section)
    )

  }


} else {
  if(!all(ids_in_shares_dataset)) {
    stop(sprintf("The following multiple outflow ids are missing in the dataset '%s' for section '%s':\n%s\nPlease add these!",
                 shares_timeseries_txt,
                 section,
                 paste0(cols[!ids_in_shares_dataset],collapse = ", "))
         )
  }

}},
dbg = debug)

}
} else {
 message(sprintf("No section in '%s$outflows_multiple' defined with a 'section_out_timeseries_id', using either 'function' (if available) or 'static' shares for routing of multiple outflows!",
                 config_txt))
}


}
