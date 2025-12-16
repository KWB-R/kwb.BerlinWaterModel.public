#' Calculate Flows Automatically
#'
#' @param config list with config as imported with \code{\link{config_read}}
#' @param input_list input_list as retrieved by \code{\link{prepare_input}}
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#' @param use_dynamic for multiple outputs only: should static shares
#' (as defined in column "section_out_share" of "outflows_multiple.csv") be used
#' for separating the flow within a section or a function (as defined in column
#' "section_out_function" of "outflows_multiple.csv")), (default: FALSE)
#' @param debug print debug messages (default: FALSE)
#'
#' @returns tibble with flows
#' @export
#'
calculate_flows_auto <- function(config, input_list, network, use_dynamic = FALSE, debug = FALSE) {

col_date_or_datetime <- names(input_list$flows)[stringr::str_detect(names(input_list$flows), "date")]

links_by_orderid <- get_flowpath_table(outflow_id = "Out",
                                                             network = network,
                                                             config = config)

stopifnot(all(c("section_out_name", "section_out_share", "section_out_function", "section_out_function_parsed", "section_out_timeseries_id") %in% names(config$outflows_multiple)))

links_by_orderid <- stats::setNames(lapply(seq_len(length(links_by_orderid)), function(i) {
  links_by_orderid[[i]] %>%
    dplyr::left_join(config$outflows_multiple[,c("section_name", "section_out_name", "section_out_share", "section_out_function", "section_out_function_parsed", "section_out_timeseries_id")] %>%
                       dplyr::mutate(section_out_share = as.numeric(section_out_share)),
                     by = c("source" = "section_name", "target" = "section_out_name", "value" = "section_out_share"))
}), nm = names(links_by_orderid))


flows_tmp <- vector("list", length(links_by_orderid) + 1)
input_flows_tmp <- input_list$flows  # Falls input existiert

for (i in seq_along(links_by_orderid)) {

  flows_tmp[[i]] <- calculate_flows(
    df_order = links_by_orderid[[i]],
    input = input_flows_tmp,
    shares_timeseries_wide = input_list$shares_timeseries,
    config = config,
    return_inputs = FALSE,
    use_dynamic =  use_dynamic,
    debug = debug
  )

  input_flows_tmp <- input_flows_tmp %>%
    dplyr::left_join(flows_tmp[[i]], by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
    dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))
}


flows_tmp[[length(links_by_orderid)+1]] <- calculate_outflows(
  list_order = links_by_orderid,
  input = input_flows_tmp,
  config = config,
  use_dynamic = use_dynamic,
  return_inputs = FALSE
)


res <- flows_tmp[[1]]

for (i in seq_along(flows_tmp)-1) {

  x <- if (i == 1) {
    flows_tmp[[1]]
  } else {
    res
  }

  res <-  x %>%
    dplyr::left_join(flows_tmp[[i+1]], by = col_date_or_datetime, suffix = c("", ".annoying_duplicate_column")) %>%
    dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))
}


res_tot <- res[,c(names(res[1]), names(res[-1])[order(names(res[-1]))])]

attr(res_tot, "use_dynamic") <- use_dynamic
attr(res_tot, "bfshare_dynamic") <- attr(input_list, "bfshare_dynamic")
attr(res_tot, "share_wwtp_sch_to_nordgraben_timeseries") <- attr(input_list, "share_wwtp_sch_to_nordgraben_timeseries")
attr(res_tot, "temporal_resolution") <- attr(input_list, "temporal_resolution")

res_tot

}


