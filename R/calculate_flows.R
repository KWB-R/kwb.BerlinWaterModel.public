#' Calculate Flows
#'
#' @param df_order list for each order id (corresponding to same level of distance
#' of section from selected outflow)
#' @param input input flows as retrieved by \code{\link{prepare_input}} and sublist
#' "flows"
#' @param shares_timeseries_wide shares timeseries in wide format, as retrieved
#' by \code{\link{prepare_input}} and sublist "shares_timeseries" (default: NULL),
#' only used if parameter "use_dynamic" is set to TRUE
#' @param config list with config as imported with \code{\link{config_read}}
#' @param use_dynamic for multiple outputs only: should static shares
#' (as defined in column "section_out_share" of "outflows_multiple.csv") be used
#' for separating the flow within a section or a function (as defined in column
#' "section_out_function" of "outflows_multiple.csv")), (default: FALSE)
#' @param return_inputs should also input data be returned in result dataset
#' (default: FALSE)
#' @param debug print debug messages (default: TRUE)
#'
#' @return returns modelled flows in tibble format
#' @export
#' @importFrom dplyr count pull
#' @importFrom stringr str_starts
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
calculate_flows <- function(df_order,
                            input,
                            shares_timeseries_wide = NULL,
                            config,
                            use_dynamic = FALSE,
                            return_inputs = FALSE,
                            debug = TRUE) {

  df_order$source_id <- get_ids_from_names(df_order$source, config = config)

  section_names <- unique(df_order$order_name)

  source_ids <- sapply(section_names, function(section_name) {

    tmp <- df_order %>%
      dplyr::mutate(source_id = get_ids_from_names(df_order$source, config = config)) %>%
      dplyr::filter(order_name == section_name,
                    order_name != source) %>%
      dplyr::count(order_name, source_id) %>%
      dplyr::filter(stringr::str_starts(source_id, "S|H")) %>%
      dplyr::pull(source_id)

    #tmp[!tmp %in% names(input)]
    all(tmp %in% names(input))
  }) %>% unlist()

  if(any(source_ids == FALSE)) {
    stop(sprintf("Section inputs for the following sections are missing:\n%s",
         paste(names(source_ids == FALSE), collapse = ", ")))
  }

  source_ids <- source_ids[order(source_ids, decreasing = TRUE)]

  stopifnot(sum(source_ids) > 0)
  if(sum(source_ids) == 0) {
    stop(sprintf("Inputs for the following sections are missing in the input datset:\n%s",
                 paste0(sprintf("%s: '%s'",
                                get_ids_from_names(names(source_ids)[!source_ids], config),
                        names(source_ids)[!source_ids]), collapse = "\n")
    ))
  }

  source_ids_available <- source_ids[source_ids == TRUE]

  res <- stats::setNames(lapply(names(source_ids_available), function(section_name) {
    kwb.utils::catAndRun(
      messageText = sprintf("Calculating flow for section '%s'", section_name),
      expr = {
        df <- df_order[df_order$order_name == section_name, ]

        calculate_flow(df = df,
                       input = input,
                       shares_timeseries_wide = shares_timeseries_wide,
                       config = config,
                       use_dynamic = use_dynamic,
                       return_inputs = return_inputs,
                       debug = debug)

      },
      dbg = debug
    )
  }), nm = get_ids_from_names(names(source_ids_available), config = config))

  if(sum(source_ids_available) != length(section_names)) {
  source_ids_unavailable <- source_ids_available[source_ids_available == FALSE]

  res <- c(res,
           stats::setNames(lapply(names(source_ids_unavailable), function(section_name) {
             kwb.utils::catAndRun(
               messageText = sprintf("Calculating flow for section '%s'", section_name),
      expr = {
        df <- df_order[df_order$order_name == section_name, ]

        calculate_flow(df = df,
                       input = input,
                       shares_timeseries_wide = shares_timeseries_wide,
                       config = config,
                       return_inputs = return_inputs,
                       debug = debug)

      },
      dbg = debug
    )
  }), nm = get_ids_from_names(names(source_ids_unavailable), config = config)))
}

  res <- res %>%
    combine_and_clean_dfs()

  attr(res, "use_dynamic") <- use_dynamic
  attr(res, "bfshare_dynamic") <- attr(input, "bfshare_dynamic")
  attr(res, "share_wwtp_sch_to_nordgraben_timeseries") <- attr(input, "share_wwtp_sch_to_nordgraben_timeseries")
  attr(res, "temporal_resolution") <- attr(input, "temporal_resolution")
  res
}
