#' Add substances
#'
#' @param config config as retrieved by \code{\link{config_read}}
#' @returns adds new columns 'conc_<substance-name.kg.m3' and re-calculates input
#' concentrations from (ng|ug|mg|g)/L to kg/m3
#' @export
#' @importFrom dplyr across arrange bind_rows filter left_join mutate select
#' @importFrom tidyr replace_na
#'
add_substances <- function(config) {

stopifnot(c("concentrations", "flows_in_out") %in% names(config))
stopifnot(names(config$flows_in_out$flow_type) %in% "WWTP")

if(!is.null(config$concentrations)) {
  stopifnot("id_base" %in% names(config$concentrations))

  config$flows_in_out <- config$flows_in_out %>%
  dplyr::filter(flow_type != "WWTP") %>%
  dplyr::left_join(config$concentrations %>%
                     convert_concentration_units() %>%
                     dplyr::filter(flow_type != "WWTP") %>%
                     dplyr::select(- id_base),
                   by = "flow_type") %>%
  dplyr::bind_rows(config$flows_in_out %>%
                     dplyr::filter(flow_type == "WWTP") %>%
                     dplyr::mutate(id_base = stringr::str_sub(flow_id, 1,8)) %>%
                     dplyr::left_join(config$concentrations %>%
                                        dplyr::filter(.data[["flow_type"]] == "WWTP") %>%
                                        convert_concentration_units(),
                                      by = c("id_base", "flow_type")) %>%
                     dplyr::select(- id_base)) %>%
  dplyr::mutate(dplyr::across(tidyselect::starts_with("conc"), ~ tidyr::replace_na(.x, 0))) %>%
  dplyr::arrange(section_id, flow_id)

  col_substances <- names(config$flows_in_out)[stringr::str_detect(names(config$flows_in_out), "g.m3")]

  n_col_substances_exists <- sum(col_substances %in% names(config$sections))

  config$sections <- if(n_col_substances_exists < length(col_substances)) {
    message(sprintf("Section starting concentrations missing for: %s\n0 will be assumed as starting concentration for all 'sections'!",
                    paste0(stringr::str_remove(col_substances[!col_substances %in% names(config$sections)], "conc_tracer."), collapse = ", ")))
    tmp <- matrix(ncol = sum(!col_substances %in% names(config$sections)),
                  data = 0,
                  nrow = nrow(config$sections)) %>%
      as.data.frame()

    names(tmp) <- col_substances[!col_substances %in% names(config$sections)]

    config$sections %>%
      dplyr::bind_cols(tibble::as_tibble(tmp))
  } else {
    config$sections
  }
} else {
  stop("No 'concentrations' data is provided in the 'config' object. Please provide
       the required data in the 'concentrations.csv' file in the config directory.")
}

config
}
