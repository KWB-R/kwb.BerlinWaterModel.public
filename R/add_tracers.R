#' Add tracers
#'
#' @param config config object as retrieved by \code{\link{config_read}}
#' @returns adds new columns 'conc_tracer.xxx' for four tracers ('CSO', 'inlet',
#' 'rain_runoff' and 'WWTP') to flows_in_out for tracking sources withint the
#' water cycle
#' @export
#' @importFrom kwb.utils catAndRun
#' @importFrom dplyr if_else
#' @importFrom stringr str_starts
add_tracers <- function(config) {
  stopifnot(c("flow_id", "flow_type") %in% names(config$flows_in_out))
  stopifnot(names(config$flows_in_out$flow_id) %in% "Q_KW")
  stopifnot(names(config$flows_in_out$flow_type) %in% c("CSO", "inlet", "rain_runoff", "rain_direct", "ww_discharge", "rain_runoff"))

  kwb.utils::catAndRun(sprintf("Adding tracers for tracking of the following sources: %s",
                               paste0(c("CSO", "inlet", "rain_runoff",  "rain_direct", "ww_discharge", "WWTP"), collapse = ", ")),
                       expr = {
                         col_tracers <- paste0("conc_tracer.", c("cso", "inlet", "rain_runoff",  "rain_direct", "ww_discharge", "wwtp"))

                         config$flows_in_out <- config$flows_in_out %>%
                           dplyr::mutate(conc_tracer.cso = dplyr::if_else(stringr::str_starts(flow_type, "CSO"),
                                                                          1,
                                                                          0),
                                         conc_tracer.inlet = dplyr::if_else(stringr::str_starts(flow_type, "inlet"),
                                                                            1,
                                                                            0),
                                         conc_tracer.rain_runoff = dplyr::if_else(stringr::str_starts(flow_type, "rain_runoff"),
                                                                           1,
                                                                           0),
                                         conc_tracer.rain_direct = dplyr::if_else(stringr::str_starts(flow_type, "rain_direct"),
                                                                           1,
                                                                           0),
                                         conc_tracer.ww_discharge = dplyr::if_else(stringr::str_starts(flow_type, "ww_discharge"),
                                                                                   1,
                                                                                   0),
                                         conc_tracer.wwtp = dplyr::if_else(stringr::str_starts(flow_id, "Q_KW"),
                                                                           1,
                                                                           0)
                           ) %>%
                           dplyr::mutate(dplyr::across(tidyselect::starts_with("conc"), ~ tidyr::replace_na(.x, 0))) %>%
                           dplyr::arrange(section_id, flow_id)

                         n_col_tracers_exists <- sum(col_tracers %in% names(config$sections))

                         if(n_col_tracers_exists < length(col_tracers)) {
                           message(sprintf("Section starting concentrations missing for: %s\n0 will be assumed as starting concentration for all 'sections'!",
                                           paste0(stringr::str_remove(col_tracers[!col_tracers %in% names(config$sections)], "conc_tracer."), collapse = ", ")))
                           tmp <- matrix(ncol = sum(!col_tracers %in% names(config$sections)),
                                         data = 0,
                                         nrow = nrow(config$sections)) %>%
                             as.data.frame()

                           names(tmp) <- col_tracers[!col_tracers %in% names(config$sections)]

                           config$sections <- config$sections %>%
                           dplyr::bind_cols(tibble::as_tibble(tmp))
                         }
                       })

config

}
