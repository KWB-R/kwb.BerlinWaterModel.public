#' Add scenario
#'
#' @param config config as retrieved by \code{\link{config_read}}
#' @param use_scenario use scenario (default: FALSE)
#' @param debug print debug messages (default: TRUE)
#'
#' @returns list with input parameters required for function \code{\link{prepare_input}}
#' @export

add_scenario <- function(config, use_scenario = FALSE, debug = TRUE) {

if(use_scenario) {
  message("#######################################################################################")
  message("### Use scenario provided in 'config$scenarios (to be modified in config/scenarios.csv)")
  message("#######################################################################################")
  ### scenario definition: reduction of inlet flows based on file config/scenarios.csv
  cso <- if(sum(tolower(config$scenarios$flow_type) == "cso") == 1) {

    scaling_factor <- config$scenarios$scaling_factor[config$scenarios$flow_type == "cso"]

    kwb.utils::catAndRun(messageText = sprintf("%s 'cso' flows by %s %2.1f %%",
                                               ifelse(scaling_factor > 1, "Increasing", ifelse(scaling_factor == 1, "Unchanged", "Decreasing")),
                                               ifelse(scaling_factor > 1, "+", ifelse(scaling_factor == 1, "", "-")),
                                               ifelse(scaling_factor > 1, 100*(scaling_factor - 1), ifelse(scaling_factor == 1, 0, 100*(1-scaling_factor)))),
                         expr = {

                           kwb.BerlinWaterModel::cso %>%
                             dplyr::mutate(cbm_per_second = cbm_per_second * scaling_factor)
                         },
                         dbg = debug)} else {
                           message("No scaling_factor defined in 'config$scenarios' for flow_type == 'cso'. Please define in config/scenarios.csv' if needed. Using dataset 'cso' from R packag kwb.BerlinWaterModel!")
                           kwb.BerlinWaterModel::cso
                         }

  evapo_p <- if(sum(tolower(config$scenarios$flow_type) == "evaporation_potential") == 1) {

    scaling_factor <- config$scenarios$scaling_factor[config$scenarios$flow_type == "evaporation_potential"]

    kwb.utils::catAndRun(messageText = sprintf("%s 'evaporation, potential' flows by %s %2.1f %%",
                                               ifelse(scaling_factor > 1, "Increasing", ifelse(scaling_factor == 1, "Unchanged", "Decreasing")),
                                               ifelse(scaling_factor > 1, "+", ifelse(scaling_factor == 1, "", "-")),
                                               ifelse(scaling_factor > 1, 100*(scaling_factor - 1), ifelse(scaling_factor == 1, 0, 100*(1-scaling_factor)))),
                         expr = {

                           kwb.BerlinWaterModel::evapo_p %>%
                             dplyr::mutate(mean = mean * scaling_factor)
                         },
                         dbg = debug)} else {
                           message("No scaling_factor defined in 'config$scenarios' for flow_type == 'evaporation_potential'. Please define in config/scenarios.csv' if needed. Using dataset 'evapo_p' from R packag kwb.BerlinWaterModel!")
                           kwb.BerlinWaterModel::evapo_p
                         }

  inflows <- if(any(tolower(config$scenarios$flow_type) == "inlet")) {

    inflows_adapted <- kwb.BerlinWaterModel::inflows %>%
      dplyr::left_join(config$scenarios %>%
                         dplyr::select(flow_id, scaling_factor) %>%
                         dplyr::rename(id = flow_id) %>%
                         dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                       by = "id") %>%
      dplyr::filter(!is.na(scaling_factor)) %>%
      dplyr::count(id, scaling_factor) %>%
      dplyr::mutate(label = sprintf("%s 'inlet' flows for flow_id '%s' by %s %2.1f %%",
                                    ifelse(scaling_factor > 1, "Increasing", ifelse(scaling_factor == 1, "Unchanged", "Decreasing")),
                                    id,
                                    ifelse(scaling_factor > 1, "+", ifelse(scaling_factor == 1, "", "-")),
                                    ifelse(scaling_factor > 1, 100*(scaling_factor - 1), ifelse(scaling_factor == 1, 0, 100*(1-scaling_factor)))))


    kwb.utils::catAndRun(messageText = sprintf("Modifying %d 'flow_ids' with flow_type 'inlet':\n%s",
                                               nrow(inflows_adapted),
                                               paste0(inflows_adapted$label, collapse = "\n")),
                         expr = {
                           kwb.BerlinWaterModel::inflows %>%
                             dplyr::left_join(config$scenarios %>%
                                                dplyr::select(flow_id, scaling_factor) %>%
                                                dplyr::rename(id = flow_id) %>%
                                                dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                                              by = "id") %>%
                             dplyr::mutate(cbm_per_second = dplyr::if_else(!is.na(scaling_factor),
                                                                           cbm_per_second * scaling_factor,
                                                                           cbm_per_second)) %>%
                             dplyr::select(- scaling_factor)
                         },
                         dbg = debug)} else {
                           message("No scaling_factor defined in 'config$scenarios' for flow_type == 'inlet'. Please define in config/scenarios.csv' if needed. Using dataset 'inflows' from R packag kwb.BerlinWaterModel!")
                           kwb.BerlinWaterModel::inflows
                         }

  rain <- if(sum(tolower(config$scenarios$flow_type) == "rain") == 1) {

    scaling_factor <- config$scenarios$scaling_factor[config$scenarios$flow_type == "rain"]

    kwb.utils::catAndRun(messageText = sprintf("%s 'rain (runoff, direct)' flows by %s %2.1f %%",
                                               ifelse(scaling_factor > 1, "Increasing", ifelse(scaling_factor == 1, "Unchanged", "Decreasing")),
                                               ifelse(scaling_factor > 1, "+", ifelse(scaling_factor == 1, "", "-")),
                                               ifelse(scaling_factor > 1, 100*(scaling_factor - 1), ifelse(scaling_factor == 1, 0, 100*(1-scaling_factor)))),
                         expr = {

                           kwb.BerlinWaterModel::rain %>%
                             dplyr::mutate(DWD_0433 = DWD_0433 * scaling_factor)
                         },
                         dbg = debug)} else {
                           message("No scaling_factor defined in 'config$scenarios' for flow_type == 'rain'. Please define in config/scenarios.csv' if needed. Using dataset 'rain' from R packag kwb.BerlinWaterModel!")
                           kwb.BerlinWaterModel::rain
                         }

  ww <- if(any(stringr::str_detect(tolower(config$scenarios$flow_type), pattern = "^ww_|_ww$"))) {


    ww_adapted <- kwb.BerlinWaterModel::ww %>%
      dplyr::mutate(flow_category = stringr::str_sub(id, 1,3)) %>%
      dplyr::left_join(config$scenarios %>%
                         dplyr::select(flow_category, scaling_factor) %>%
                         dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                       by = "flow_category") %>%
      dplyr::filter(!is.na(scaling_factor)) %>%
      dplyr::count(id, scaling_factor) %>%
      dplyr::mutate(label = sprintf("%s 'ww' flows for flow_id '%s' by %s %2.1f %%",
                                    ifelse(scaling_factor > 1, "Increasing", ifelse(scaling_factor == 1, "Unchanged", "Decreasing")),
                                    id,
                                    ifelse(scaling_factor > 1, "+", ifelse(scaling_factor == 1, "", "-")),
                                    ifelse(scaling_factor > 1, 100*(scaling_factor - 1), ifelse(scaling_factor == 1, 0, 100*(1-scaling_factor)))))


    kwb.utils::catAndRun(messageText = sprintf("Modifying %d 'flow_ids' with flow_type 'ww_|_ww':\n%s",
                                               nrow(ww_adapted),
                                               paste0(ww_adapted$label, collapse = "\n")),
                         expr = {
                           kwb.BerlinWaterModel::ww %>%
                             dplyr::mutate(flow_category = stringr::str_sub(id, 1,3)) %>%
                             dplyr::left_join(config$scenarios %>%
                                                dplyr::select(flow_category, scaling_factor) %>%
                                                dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                                              by = "flow_category") %>%
                             dplyr::mutate(cbm_per_second = dplyr::if_else(!is.na(scaling_factor),
                                                                           cbm_per_second * scaling_factor,
                                                                           cbm_per_second)) %>%
                             dplyr::select(- scaling_factor, - flow_category)
                         },
                         dbg = debug)} else {
                           message("No scaling_factor defined in 'config$scenarios' for flow_type 'ww_|_ww'. Please define in config/scenarios.csv' if needed. Using dataset 'ww' from R packag kwb.BerlinWaterModel!")
                           kwb.BerlinWaterModel::ww
                         }

  wwtp <- if(any(tolower(config$scenarios$flow_type) == "wwtp")) {

    wwtp_adapted <- kwb.BerlinWaterModel::wwtp %>%
      dplyr::mutate(flow_category = stringr::str_sub(id, 6,8)) %>%
      dplyr::left_join(config$scenarios %>%
                         dplyr::select(flow_category, scaling_factor) %>%
                         dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                       by = "flow_category") %>%
      dplyr::filter(!is.na(scaling_factor)) %>%
      dplyr::count(id, scaling_factor) %>%
      dplyr::mutate(label = sprintf("%s 'wwtp' flows for flow_id '%s' by %s %2.1f %%",
                                    ifelse(scaling_factor > 1, "Increasing", ifelse(scaling_factor == 1, "Unchanged", "Decreasing")),
                                    id,
                                    ifelse(scaling_factor > 1, "+", ifelse(scaling_factor == 1, "", "-")),
                                    ifelse(scaling_factor > 1, 100*(scaling_factor - 1), ifelse(scaling_factor == 1, 0, 100*(1-scaling_factor)))))


    kwb.utils::catAndRun(messageText = sprintf("Modifying %d 'flow_ids' with flow_type 'wwtp':\n%s",
                                               nrow(wwtp_adapted),
                                               paste0(wwtp_adapted$label, collapse = "\n")),
                         expr = {
                           kwb.BerlinWaterModel::wwtp %>%
                             dplyr::mutate(flow_category = stringr::str_sub(id, 6,8)) %>%
                             dplyr::left_join(config$scenarios %>%
                                                dplyr::select(flow_category, scaling_factor) %>%
                                                dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                                              by = "flow_category") %>%
                             dplyr::mutate(cbm_per_second = dplyr::if_else(!is.na(scaling_factor),
                                                                           cbm_per_second * scaling_factor,
                                                                           cbm_per_second)) %>%
                             dplyr::select(- scaling_factor, - flow_category)
                         },
                         dbg = debug)} else {
                           message("No scaling_factor defined in 'config$scenarios' for flow_type == 'wwtp'. Please define in config/scenarios.csv' if needed. Using dataset 'wwtp' from R packag kwb.BerlinWaterModel!")
                           kwb.BerlinWaterModel::wwtp
                         }

} else {
  message("No scenario selected. Using datasets (i.e. 'cso', 'evapo_p', 'inflows', 'rain', 'ww', 'wwtp') provided with R packag 'kwb.BerlinWaterModel'.")
  cso <- kwb.BerlinWaterModel::cso
  evapo_p <- kwb.BerlinWaterModel::evapo_p
  inflows <- kwb.BerlinWaterModel::inflows
  rain <-  kwb.BerlinWaterModel::rain
  ww <- kwb.BerlinWaterModel::ww
  wwtp <- kwb.BerlinWaterModel::wwtp
}

list(cso = cso,
     evapo_p = evapo_p,
     inflows = inflows,
     rain = rain,
     ww = ww,
     wwtp = wwtp)
}
