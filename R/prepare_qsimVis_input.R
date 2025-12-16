#' Preparew QsimVis Input
#'
#' @param config config list as retrieved by \code{\link{config_read}}
#' @param flows flows input data frame in "wide" format
#' @param qualities qualities list input data structure for each section in wide format
#' @param rounding_digits digits used for result data rounding (default: 3)
#' @returns data frame with flow data in long format joined with
#' config$qsimVis data frame
#' @export
#'
#' @importFrom dplyr sym
#' @importFrom lubridate rollback
prepare_qsimVis_input <- function(config, flows, qualities, rounding_digits = 3) {

flows_date_time <- names(flows)[stringr::str_starts(names(flows), pattern = "date")]
qualities_date_time <- names(qualities$conc[[1]])[stringr::str_starts(names(qualities$conc[[1]]), pattern = "date")]

if(flows_date_time != qualities_date_time) {
  stop(sprintf("The flows data contains '%s' but the qualities data contains '%s' as date/time column. Please provide two datasets with identical temporal resolution!",
               flows_date_time,
               qualities_date_time))
}

qualities_date_min <- if(qualities_date_time == "date") {
  as.Date(min(qualities$conc[[1]][[qualities_date_time]]))
} else {
  as.POSIXct(min(qualities$conc[[1]][[qualities_date_time]]), tz = "UTC")
}

qualities_date_max <- if(qualities_date_time == "date") {
  as.Date(max(qualities$conc[[1]][[qualities_date_time]]))
} else {
  as.POSIXct(max(qualities$conc[[1]][[qualities_date_time]]), tz = "UTC")
}

config$qsimVis %>%
  dplyr::left_join(flows %>%
                     dplyr::filter(!!as.symbol(flows_date_time) >= as.Date(qualities_date_min),
                                   !!as.symbol(flows_date_time) <= as.Date(qualities_date_max)) %>%
                     tidyr::pivot_longer(names_to = "section_id",
                                         values_to = "cbm_per_second",
                                         - tidyselect::starts_with("date")) %>%
                   dplyr::left_join(qualities$conc %>%
                                      dplyr::bind_rows(.id = "section_id")) %>%
                   dplyr::mutate(dplyr::across(- tidyselect::all_of(c(flows_date_time, "section_id")), ~ round(.x, digits = rounding_digits))),
                   by = "section_id",
                   relationship = "many-to-many")

}

