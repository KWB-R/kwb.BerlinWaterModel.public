#' Calculate Qualities Forward
#'
#' @param input_list input_list as retrieved by \code{\link{prepare_input}}
#' @param flows tibble with (modelled) flows for the network (e.g. precomputed
#'   upstream by the user)
#' @param network tibble with water cycle flow network data, as retrieved by
#'   \code{\link{prepare_network}}
#' @param config list with config as imported with \code{\link{config_read}}
#' @param max_sections restrict number of calculated sections in case problems
#'   occur. Provide a number <= number of sections to be calculated. If NULL, all
#'   sections will be calculated (default: NULL). Only used if reverse_flow = FALSE
#' @param debug print debug messages (default: TRUE)
#'
#' @return returns modelled flows in tibble format
#' @export
#' @importFrom dplyr count pull
#' @importFrom stringr str_starts
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
#' @importFrom purrr reduce
calculate_qualities_forward <- function(input_list,
                                        flows,
                                        network,
                                        config,
                                        max_sections = NULL,
                                        debug = TRUE) {

  stopifnot(all(c("section_out_name", "section_out_share", "section_out_function", "section_out_function_parsed", "section_out_timeseries_id") %in% names(config$outflows_multiple)))

  df_orders <- get_flowpath_table(outflow_id = "Out",
                                         network = network,
                                         config = config) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(config$outflows_multiple[,c("section_name", "section_out_name", "section_out_share", "section_out_function", "section_out_function_parsed", "section_out_timeseries_id")] %>%
                       dplyr::mutate(section_out_share = as.numeric(section_out_share)),
                     by = c("source" = "section_name", "target" = "section_out_name", "value" = "section_out_share"))

  df_orders_id <- df_orders %>%
    dplyr::count(order_id, order_name) %>%
    dplyr::arrange(dplyr::desc(order_id), order_name) %>%
    dplyr::select(- n) %>%
    dplyr::mutate(id = 1:dplyr::n())

  df_orders <- df_orders %>%
    dplyr::mutate(source_id = get_ids_from_names(source, config = config),
                  target_id = get_ids_from_names(target, config = config)) %>%
    dplyr::left_join(df_orders_id, by = c("order_id", "order_name")) %>%
    dplyr::relocate(id, .before = "order_id")


  outflow_pattern <- stringr::str_starts(df_orders$target_id, "Out")

  if(sum(outflow_pattern) == 0) {
    stop("No outflow id has been defined. Please define an outflow in the 'flows_in_out.csv'
       configuration file and name it starting with 'Out'. Otherwise it cannot be properly
       detected.")
  }

  outflow_ids <- unique(df_orders$target_id[outflow_pattern])

  outflows_inflows_df <- lapply(seq_along(outflow_ids), function(i) {
    outflow_inflows <- df_orders %>%
      dplyr::filter(target_id == outflow_ids[i]) %>%
      dplyr::mutate(order_id = 0,
                    order_name = target,
                    id = max(id) + i
                    )
  }) %>%
    dplyr::bind_cols()

  df_orders <- df_orders %>%
    dplyr::bind_rows(outflows_inflows_df) %>%
    dplyr::mutate(section_id = get_ids_from_names(order_name, config)) %>%
    dplyr::relocate(section_id, .after = order_name)


  max_sections <- if(is.null(max_sections)) {
    length(unique(df_orders$id))
  } else if (!is.null(max_sections) & max_sections < length(unique(df_orders$id))) {
    max_sections
  }  else {
    message(sprintf("Provided argument 'max_sections' is longer (%d) than provided sections (%d). 'max_sections' is set to %d",
                    max_sections,
                    length(unique(df_orders$id)),
                    length(unique(df_orders$id))))
    length(unique(df_orders$id))
  }

  qualities <- list()
  for(id in unique(df_orders$id)[seq_len(max_sections)]) {
    df_order <- df_orders[df_orders$id == id,]
    s_id <- unique(df_order$section_id)

    message(sprintf("Calculating water quality 'forward' for section %s: %s",
                    unique(df_order$section_id),
                    unique(df_order$order_name)))

  kwb.utils::catAndRun(messageText = sprintf("Calculating water quality for section '%s: %s' (order_level: %d, id: %d)",
                                             get_ids_from_names(unique(df_order$order_name), config),
                                             unique(df_order$order_name),
                                             unique(df_order$order_id),
                                             id),
                       expr = {


  if (id == 1) {
    qualities <- calculate_quality(df = df_order,
                      input = input_list$flows,
                      shares_timeseries_wide = input_list$shares_timeseries,
                      flows = flows,
                      quality = NULL,
                      config = config,
                      result_type = "list",
                      debug = debug)
  } else {
    quality_forward <- calculate_quality(df = df_order,
                                         input = input_list$flows,
                                         shares_timeseries_wide = input_list$shares_timeseries,
                                         flows = flows,
                                         quality = qualities,
                                         config = config,
                                         reverse_flow = FALSE,
                                         result_type = "list",
                                         debug = debug)


    qualities$conc[[s_id]] <- quality_forward$conc[[s_id]]
    qualities$load[[s_id]] <- quality_forward$load[[s_id]]
    qualities$flows[[s_id]] <- quality_forward$flows[[s_id]]

  }
},
newLine = 1,
dbg = debug)
  }

qualities$conc <- qualities$conc[names(qualities$conc)[order(names(qualities$conc))]]
qualities$load <- qualities$load[names(qualities$load)[order(names(qualities$load))]]
qualities$flows <- qualities$flows[names(qualities$flows)[order(names(qualities$flows))]]

qualities
}
