#' Add Direct Rain and Evaporation to flows_in_out config for sections with defined area_m2
#'
#' @param config config object as retrieved by \code{\link{config_read}}
#' @returns config with added flows_in_out for sections with defined area_m2, in
#' case none area_m2 is defined the original config is returned
#' @export
#' @importFrom tibble tibble
add_rain_direct_and_evaporation <- function(config) {

  sections_with_area <- config$sections %>%
    dplyr::filter(!is.na(area_m2) & area_m2 > 0)

  if(nrow(sections_with_area) > 0) {

    kwb.utils::catAndRun(sprintf("Adding direct rain and potential evaaporation to 'config$flows_in_out' for %d sections (%s)",
                                 nrow(sections_with_area),
                                 paste(sections_with_area$section_id, collapse = ", ")),
                         expr = {


    rain_direct_evaporation <- tibble::tibble(section_id = sections_with_area$section_id,
                   flow_id = sprintf("rain.direct_%s", section_id),
                   flow_name = sprintf("direct rainfall for section id %s", section_id),
                   flow_in_out = "in",
                   flow_type = "rain_direct") %>%
    dplyr::bind_rows(tibble::tibble(section_id = sections_with_area$section_id,
                                    flow_id = sprintf("evapo.p_%s", section_id),
                                    flow_name = sprintf("evaporation for section id %s", section_id),
                                    flow_in_out = "out",
                                    flow_type = "evaporation_potential"))


    config$flows_in_out <- config$flows_in_out %>%
      dplyr::bind_rows(rain_direct_evaporation) %>%
      dplyr::arrange(section_id, flow_id)
                         })


  } else {
    warning("No 'section_id' in 'config/sections.csv' defined with an 'area_m2' > 0. Please define if needed!")
  }

  config

}
