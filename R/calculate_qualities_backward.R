#' Calculate Qualities Backward
#'
#' @param input_list input_list as retrieved by \code{\link{prepare_input}}
#' @param flows flows as retrieved by xxxx
##' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#' @param config list with config as imported with \code{\link{config_read}}
#' @param debug print debug messages (default: TRUE)
#'
#' @return returns modelled flows in tibble format
#' @export
#' @importFrom dplyr count pull
#' @importFrom stringr str_starts
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
calculate_qualities_backward <- function(input_list,
                                         flows,
                                         network,
                                         config,
                                         debug = TRUE) {


  stopifnot(all(c("section_out_name", "section_out_share", "section_out_function", "section_out_function_parsed", "section_out_timeseries_id") %in% names(config$outflows_multiple)))
  stopifnot(all(c("branch_id", "no_neg_flow_corr", "no_transfer_section_up_neg_flow") %in% names(config$sections)))

  col_input_date_or_datetime <- get_date_or_datetime_columns(input_list$flows)
  col_flows_date_or_datetime <- get_date_or_datetime_columns(flows)

  date_or_datetime_col <- if(col_input_date_or_datetime == col_flows_date_or_datetime) {
    if(length(col_input_date_or_datetime)==1) {
      col_input_date_or_datetime
    } else {
      stop("No date/datetime column found in inflow/flows dataset")
    }

  } else {
    input_tempres <- ifelse(col_input_date_or_datetime == "date", "daily", "hours")
    flows_tempres <- ifelse(col_flows_date_or_datetime == "date", "daily", "hours")

    stop(sprintf("The temporal resolution of 'input' (%s) and 'flows' (%s) dataset",
                 input_tempres,
                 flows_tempres)
    )
  }

  diff_time <- diff(input_list[[date_or_datetime_col]], units = "secs")
  period_seconds <- as.vector(c(diff_time, 0))


  flows_reverse <- get_reverse_flows_per_section(flows) %>%
    dplyr::select(- tidyselect::starts_with("cbm_")) %>%
    dplyr::left_join(config$sections %>%
                       dplyr::select(tidyselect::all_of(c("section_id", "no_neg_flow_corr", "no_transfer_section_up_neg_flow"))),
                     by = "section_id") %>%
    dplyr::mutate(section_name = get_names_from_ids(section_id, config)) %>%
    dplyr::relocate(section_name, .after = section_id)

  if(nrow(flows_reverse) == 0) {
    message("No negative flows in sections (provided in 'flows' input dataset) or labelled with 'yes' in column 'no_neg_flow_corr' within 'config/sections.csv'. Fallback to forward quality calculation!")
    calculate_qualities_forward(input_list = input_list,
                                flows = flows,
                                network = network,
                                config = config,
                                max_sections = max_sections,
                                debug = debug)
  } else {


    if(!all(is.numeric(config$sections$branch_id))) {

      tmp_undef_branches <- config$sections[!is.numeric(config$sections$branch_id),]

      stop(sprintf("The following sections (defined in 'config/sections.csv') have undefined 'branch_id's:\n%s",
                   paste0(sprintf("%s: %s",
                                  tmp_undef_branches$section_id,
                                  tmp_undef_branches$section_name),
                          collapse = "\n")
      ))
    }
  }

  df_orders <- get_flowpath_table(outflow_id = "Out",
                                  network = network,
                                  config = config) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(config$outflows_multiple[,c("section_name", "section_out_name", "section_out_share", "section_out_function", "section_out_function_parsed", "section_out_timeseries_id")] %>%
                       dplyr::mutate(section_out_share = as.numeric(section_out_share)),
                     by = c("source" = "section_name", "target" = "section_out_name", "value" = "section_out_share"))

  df_orders_id <- df_orders %>%
    dplyr::left_join(config$sections %>%
                       dplyr::select("section_name", "branch_id", "no_neg_flow_corr", "no_transfer_section_up_neg_flow") %>%
                       dplyr::rename("order_name" = "section_name"),
                     by = "order_name") %>%
    dplyr::count(branch_id, order_id, order_name) %>%
    dplyr::arrange(branch_id, dplyr::desc(order_id), order_name) %>%
    dplyr::select(- n) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::relocate(id, .before = "order_id")

  df_orders <- df_orders %>%
    dplyr::mutate(source_id = get_ids_from_names(source, config = config),
                  target_id = get_ids_from_names(target, config = config)) %>%
    dplyr::left_join(df_orders_id, by = c("order_id", "order_name")) %>%
    dplyr::relocate(c("branch_id", "id", "order_id"), .before = "order_name") %>%
    dplyr::arrange(branch_id, dplyr::desc(order_id), order_name)


  if("backflows_multiple" %in% names(config)) {
    backflows_multiple <- config$backflows_multiple %>%
      dplyr::rename(order_name = section_name,
                    source_id = section_backflow_out_id) %>%
      dplyr::select(- tidyselect::all_of(c("backflow_id", "section_backflow_out_name")))


    df_orders <- df_orders %>%
      dplyr::left_join(backflows_multiple,
                       by = c("order_name", "source_id"))

    backflows_all_sections_defined <- df_orders %>%
      dplyr::filter(source_id %in% get_section_idnames(config)$id,
                    target_id %in% get_section_idnames(config)$id,
                    order_name != source) %>%
      dplyr::group_by(order_name) %>%
      dplyr::summarise(n = dplyr::n(),
                       n_backflows = sum(!is.na(section_backflow_out_share))) %>%
      dplyr::filter(n_backflows > 0)


    if(any(backflows_all_sections_defined$n != backflows_all_sections_defined$n_backflows)) {
      backflows_undefined <- backflows_all_sections_defined %>%
        #dplyr::filter(n != n_backflows)
        stop(sprintf("For the following backflow sections there are undefined upstream sections:\n%s",
                     paste0(sprintf("%s (%s): %d/%d upstream sections",
                                    get_ids_from_names(backflows_undefined$order_name, config),
                                    backflows_undefined$order_name,
                                    backflows_undefined$n_backflows,
                                    backflows_undefined$n),
                            collapse = "\n")
        )
        )

    } else {
      #do nothing as all upstream sections ar defined
    }


  }

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
      ) %>%
      dplyr::mutate(section_id = target_id) %>%
      dplyr::relocate(section_id, .after = order_name)
  }) %>%
    dplyr::bind_cols()

  df_orders <- df_orders %>%
    dplyr::mutate(section_id = get_ids_from_names(order_name, config)) %>%
    dplyr::bind_rows(outflows_inflows_df) %>%
    dplyr::left_join(flows_reverse %>%
                       dplyr::select(-section_id) %>%
                       dplyr::rename(order_name = section_name),
                     by = "order_name") %>%
    dplyr::relocate(section_id, .after = "order_name")


  qualities <- list()

  ############################################################################
  ### 1. Calculate Forward
  ############################################################################

  for(id in unique(df_orders$id)) {
    df_order <- df_orders[df_orders$id == id,]
    s_id <- unique(df_order$section_id)

    message(sprintf("Calculating water quality 'forward' for section %s: %s",
                    get_ids_from_names(unique(df_order$order_name), config),
                    unique(df_order$order_name)))

    kwb.utils::catAndRun(messageText = sprintf("Calculating water quality 'forward' for section '%s: %s' (order_level: %d, id: %d)",
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
                                               reverse_flow = FALSE,
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


  ############################################################################
  ### 2. Calculate for each branch
  ### 2.1 Backward for each branch with flows < 0 & remaining forward
  ### 2.2 Forward for branches without negative flows
  ############################################################################

  ### 2.1 Forward for each branch
  for(b_id in unique(df_orders$branch_id)) {

    df_orders_branch <- df_orders %>%
      dplyr::filter(branch_id == b_id) %>%
      dplyr::arrange(id)

    ### 2.2 Backward for each branch with flows < 0 & remaining forward

    sections_below_zero <- df_orders_branch %>%
      dplyr::filter(n_flow_below_zero > 0,
                    tolower(no_neg_flow_corr) != "yes" | is.na(no_neg_flow_corr))

    upstream_section_ids <- df_orders %>%
      dplyr::filter(section_id %in% sections_below_zero$section_id,
                    source_id != section_id,
                    source_id %in% get_section_idnames(config)$id,
                    tolower(no_transfer_section_up_neg_flow) != "yes" | is.na(no_transfer_section_up_neg_flow)) %>%
      dplyr::count(section_id, source_id) %>%
      dplyr::select(-n) %>%
      dplyr::pull(source_id)


    sections_below_zero_and_upstream <- sections_below_zero %>%
      dplyr::bind_rows(df_orders %>%
                         dplyr::filter(section_id %in% upstream_section_ids)) %>%
      dplyr::count(!!!rlang::syms(c("branch_id", "id", "order_name", "n_flow_below_zero",
                                    sprintf("%s_min", date_or_datetime_col),
                                    sprintf("%s_max", date_or_datetime_col),
                                    "no_neg_flow_corr",
                                    "no_transfer_section_up_neg_flow"))) %>%
      dplyr::select(-n) %>%
      dplyr::rename(section_name = order_name) %>%
      dplyr::mutate(section_id = get_ids_from_names(section_name, config)) %>%
      dplyr::relocate(c(id,section_id), .before = section_name) %>%
      dplyr::arrange(branch_id, dplyr::desc(id))


    if(nrow(sections_below_zero_and_upstream) > 0) {
      for(s_id in sections_below_zero_and_upstream$section_id) {

        message(sprintf("(%d/%d): Calculating water quality 'backward' for section %s (%s) in branch_id %d",
                        which(s_id == sections_below_zero_and_upstream$section_id),
                        nrow(sections_below_zero_and_upstream),
                        get_names_from_ids(s_id, config),
                        s_id,
                        unique(df_orders$branch_id[df_orders$section_id == s_id])))

        df_order <- df_orders %>%
          dplyr::filter(section_id == s_id)


        quality_reverse <- calculate_quality(df = df_order,
                                             input = input_list$flows,
                                             shares_timeseries_wide = input_list$shares_timeseries,
                                             flows = flows,
                                             quality = qualities,
                                             config = config,
                                             reverse_flow = TRUE,
                                             result_type = "list",
                                             debug = debug)

        qualities$conc[[s_id]] <- quality_reverse$conc[[s_id]]
        qualities$load[[s_id]] <- quality_reverse$load[[s_id]]

        if(length(names(quality_reverse$load_neg)) > 0) {

        for(load_neg_section_id in names(quality_reverse$load_neg)) {
          qualities$load_neg[[load_neg_section_id]] <- purrr::reduce(list(qualities$load_neg[[load_neg_section_id]],
                                                                          quality_reverse$load_neg[[load_neg_section_id]]),
                                                                     merge_two_level_lists,
                                                                     prefer = "left"
                                                                     )
        }
        }
        qualities$flows[[s_id]] <- quality_reverse$flows[[s_id]]
      }
    }

      ### 2.2 Forward after backflow correction (for branches with neg. flows)
    branch_ids_with_negative_flows <- unique(df_orders$branch_id[df_orders$n_flow_below_zero > 0 & !is.na(df_orders$n_flow_below_zero)])

      ids_to_recalculate <- if(nrow(sections_below_zero_and_upstream) == 0) {
        NULL
        } else {
        unique(df_orders_branch$id)[!unique(df_orders_branch$id) %in% sections_below_zero_and_upstream$id & unique(df_orders_branch$id) > min(sections_below_zero_and_upstream$id)]
      }

      if(length(ids_to_recalculate) > 0) {
        for(recalc_id in ids_to_recalculate) {

          df_order <- df_orders_branch %>%
            dplyr::filter(id == recalc_id) %>%
            dplyr::arrange(id)

          section_id <- unique(df_order$section_id)

          message(sprintf("Re-Calculating water quality 'forward' for branch_id: %d (neg. flows in branch), section '%s: %s'",
                  unique(df_orders$branch_id[df_orders$section_id == section_id]),
                  get_ids_from_names(unique(df_order$order_name), config),
                  unique(df_order$order_name)))

          kwb.utils::catAndRun(messageText = sprintf("Re-Calculating water quality 'forward' for branch_id: %d (negative flows within branch), section '%s: %s' (order_level: %d, id: %d)",
                                                     unique(df_orders$branch_id[df_orders$section_id == section_id]),
                                                     get_ids_from_names(unique(df_order$order_name), config),
                                                     unique(df_order$order_name),
                                                     unique(df_order$order_id),
                                                     recalc_id),
                               expr = {

                                 quality_recalc <- calculate_quality(df = df_order,
                                                                     input = input_list$flows,
                                                                     shares_timeseries_wide = input_list$shares_timeseries,
                                                                     flows = flows,
                                                                     quality = qualities,
                                                                     config = config,
                                                                     reverse_flow = FALSE,
                                                                     result_type = "list",
                                                                     debug = debug)

                                 qualities$conc[[section_id]] <- quality_recalc$conc[[section_id]]
                                 qualities$load[[section_id]] <- quality_recalc$load[[section_id]]
                                 qualities$flows[[section_id]] <- quality_recalc$flows[[section_id]]
                               },
                               newLine = 1,
                               dbg = debug)


        }
        ### 2.3 Forward after backflow correction (for branches with non neg flows but upstream neg branches)
        } else if (nrow(sections_below_zero_and_upstream) == 0 & any(b_id > branch_ids_with_negative_flows)) {
        for(recalc_id in unique(df_orders_branch$id)[order(unique(df_orders_branch$id))]) {

          df_order <- df_orders_branch %>%
            dplyr::filter(id == recalc_id) %>%
            dplyr::arrange(id)

          section_id <- unique(df_order$section_id)

          message(sprintf("Re-Calculating water quality 'forward' for branch_id: %d (no neg. flows in branch), section '%s: %s'",
                  b_id,
                  get_ids_from_names(unique(df_order$order_name), config),
                  unique(df_order$order_name)))

          kwb.utils::catAndRun(messageText = sprintf("Re-Calculating water quality 'forward' for branch_id: %d (no negative flows within branch), section '%s: %s' (order_level: %d, id: %d)",
                                                     b_id,
                                                     get_ids_from_names(unique(df_order$order_name), config),
                                                     unique(df_order$order_name),
                                                     unique(df_order$order_id),
                                                     recalc_id),
                               expr = {

                                 quality_recalc <- calculate_quality(df = df_order,
                                                                     input = input_list$flows,
                                                                     shares_timeseries_wide = input_list$shares_timeseries,
                                                                     flows = flows,
                                                                     quality = qualities,
                                                                     config = config,
                                                                     reverse_flow = FALSE,
                                                                     result_type = "list",
                                                                     debug = debug)

                                 qualities$conc[[section_id]] <- quality_recalc$conc[[section_id]]
                                 qualities$load[[section_id]] <- quality_recalc$load[[section_id]]
                                 qualities$flows[[section_id]] <- quality_recalc$flows[[section_id]]
                               },
                               newLine = 1,
                               dbg = debug)
      }
      } else {
        # do nothing
      }
  }

  qualities$conc <- qualities$conc[names(qualities$conc)[order(names(qualities$conc))]]
  qualities$load <- qualities$load[names(qualities$load)[order(names(qualities$load))]]
  qualities$load_neg <- qualities$load_neg[names(qualities$load_neg)[order(names(qualities$load_neg))]]
  qualities$flows <- qualities$flows[names(qualities$flows)[order(names(qualities$flows))]]

  qualities
}
