#' Set Tracer Starting Concentrations Statistics
#'
#' @param config config list structure as retrieved by \code{\link{config_read}}
#' @param qualities qualities list result structure
#' @param aggregation_function function used for data aggregation (default: median)
#' @param minimum_tracer_sum minimum tracer sum (default: 0.999 i.e. 99.9 percent)
#' for filtering out results for with tracer has not reached almost 100 percent
#' @returns config list with starting concentrations (defined in config$sections)
#' based on qualities
#' @export
#'

set_tracer_starting_conc_stats <- function(config,
                                           qualities,
                                           aggregation_function = median,
                                           minimum_tracer_sum = 0.999) {

  col_date_or_datetime <- ifelse(any(stringr::str_detect(names(qualities$conc[[1]]), "date")),
                                 "date",
                                 "datetime")


x_list <- stats::setNames(lapply(names(qualities$conc), function(section_id) {


x <- qualities$conc[[section_id]]

x_stats <- x %>%
  dplyr::select(tidyselect::starts_with("tracer")) %>%
  dplyr::mutate(tracer_sum = rowSums(.)) %>%
  dplyr::filter(tracer_sum > minimum_tracer_sum) %>%
  dplyr::select(- tracer_sum) %>%
  dplyr::summarise(dplyr::across(.cols = tidyselect::starts_with("tracer"), .fns = median))

tracer_sum <- rowSums(x_stats)


x_stats_fixed <- x_stats / tracer_sum


x_stats_fixed
}), nm = names(qualities$conc))


for(section_id in names(x_list)) {

  idx <- config$sections$section_id == section_id

  for(section_tracer_name in names(x_list[[section_id]])) {
  #message(section_tracer_name)
  config$sections[[sprintf("conc_%s", section_tracer_name)]][idx] <- x_list[[section_id]][[section_tracer_name]]
  }

}

config


}


