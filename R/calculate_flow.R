#' Calculate Flow
#'
#' @param df data frame with model as retrieved by \code{\link{get_flowpath_table}}
#' @param input input flows as retrieved by \code{\link{prepare_input}} and sublist
#' "flows"
#' @param shares_timeseries_wide shares timeseries in wide formate, as retrieved
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
#' @return tibble with Urban Water Model results
#' @export
#' @importFrom dplyr bind_cols bind_rows filter pull select
#' @importFrom tibble tibble
#'
calculate_flow <- function(df, input, shares_timeseries_wide = NULL, config, use_dynamic = FALSE, return_inputs = FALSE, debug = TRUE) {
  section_name_up <- unique(df$order_name)
  section_id_up <- get_ids_from_names(section_name_up, config = config)

  col_date_or_datetime <- names(input)[stringr::str_detect(names(input), "date")]

  stopifnot(length(unique(df$order_id)) == 1)
  stopifnot(length(unique(section_name_up)) == 1)

  outflows_multiple_idname <- config$outflows_multiple %>%
    dplyr::select(section_out_id, section_out_name) %>%
    dplyr::rename(id =  section_out_id, name = section_out_name)

  sections_without_selected_section <- get_section_idnames(config) %>%
    dplyr::bind_rows(outflows_multiple_idname) %>%
    dplyr::filter(!id %in% section_id_up) %>%
    dplyr::pull(name)

  df_sel_up <- df %>%
    dplyr::filter(!(source == section_name_up & target %in% sections_without_selected_section))

  flow_ids <-  df_sel_up %>%
    dplyr::filter(! (stringr::str_starts(source_id, "H|S|Out") & value != 1)) %>%
    dplyr::select(source, target) %>%
    unlist() %>%
    as.character() %>%
    unique()

  flow_ids <- tibble::tibble(target = get_ids_from_names(flow_ids, config)) %>%
    shorten_ww_flow_id(col_flow_id = "target") %>%
    dplyr::pull(target)

  missing_flow_ids <- !flow_ids[flow_ids != section_id_up] %in% names(input)

  if (sum(missing_flow_ids) > 0) {
    stop(sprintf(
      "The following flow_ids are missing in the input dataset:\n%s",
      paste0(flow_ids[missing_flow_ids], collapse = ", ")
    ))
  }

  df_sel_down <-  df %>%
    dplyr::filter(source == section_name_up & target %in% sections_without_selected_section)

  if(nrow(df_sel_down) > 0) {

    if (nrow(df_sel_down) == 0) {
      message("This is the last section of the water network. No further outflows are defined.")
      df_updown <- df_up
    }

    if (sum(df_sel_down$value) != 1) {
      stop(sprintf(
        "The sum of the outflows does not equal 1 for %s",
        paste0(df_sel_down$target, collapse = ", ")
      ))
    }
  }

  multiple_inflow <- if(use_dynamic) {
    df_sel_up %>%
      dplyr::filter((stringr::str_starts(source_id, "H|S|Out") & (sapply(section_out_function, is.function) | value != 1 | !is.na(section_out_timeseries_id))))
} else {
  df_sel_up %>%
    dplyr::filter((stringr::str_starts(source_id, "H|S|Out") & value != 1))
}

  flow_ids_mar <- config$flows_in_out %>%
    shorten_ww_flow_id() %>%
    dplyr::filter(tolower(is_mar) == "yes") %>%
    dplyr::pull(flow_id)

  # Fix to not consider well galleries tagged with 'yes' in column 'is_mar', i.e. abstraction is set to 0!
  ids <- flow_ids[!(flow_ids %in% c(section_id_up, flow_ids_mar))]

  section_flows <- if(nrow(multiple_inflow) > 0) {
    input[,ids] %>%
      dplyr::bind_cols(lapply(seq_len(nrow(multiple_inflow)), function(i) {
      inflow_id <- get_ids_from_names(multiple_inflow$source[i], config)
      if(use_dynamic) {
        if(!is.null(shares_timeseries_wide) & !is.na(multiple_inflow$section_out_timeseries_id[i]) & multiple_inflow$section_out_timeseries_id[i] %in% names(shares_timeseries_wide)) {
          if(debug)  {
            message(sprintf("use dynamic & time series for flow from section '%s' to '%s'",
                                    multiple_inflow$source[i],
                                    multiple_inflow$target[i]))
            }
          tibble::tibble("{inflow_id}" := (input %>% dplyr::pull(inflow_id)) * shares_timeseries_wide[[multiple_inflow$section_out_timeseries_id[i]]])
        } else if(is.function(multiple_inflow$section_out_function_parsed[[i]])) {
          if(debug)  {
            message(sprintf("use dynamic & function for flow from section '%s' to '%s'",
                            multiple_inflow$source[i],
                            multiple_inflow$target[i]))
          }
          tibble::tibble("{inflow_id}" := multiple_inflow$section_out_function_parsed[[i]](input %>% dplyr::pull(inflow_id)))
        } else {
          if (is.numeric(multiple_inflow$value[i])) {
            if(debug)  {
              message(sprintf("use dynamic & static value for flow from section '%s' to '%s'",
                              multiple_inflow$source[i],
                              multiple_inflow$target[i]))
            }
          tibble::tibble("{inflow_id}" := input %>% dplyr::pull(inflow_id) * multiple_inflow$value[i])
          } else {
            stop(sprintf("No static value defined in 'section_out_share' in config file 'outflows_multiple' for source '%s' to target '%s'. Please set it there!",
                         multiple_inflow$source[i],
                         multiple_inflow$target[i]
            ))
          }
        }
      } else {
        if (is.numeric(multiple_inflow$value[i])) {
          if(debug)  {
            message(sprintf("no dynamic, using static value for flow from section '%s' to '%s'",
                            multiple_inflow$source[i],
                            multiple_inflow$target[i]))
          }
          tibble::tibble("{inflow_id}" := input %>% dplyr::pull(inflow_id) * multiple_inflow$value[i])
        } else {
          stop(sprintf("No static value defined in 'section_out_share' in config file 'outflows_multiple' for source '%s' to target '%s'. Please set it there!",
                       multiple_inflow$source[i],
                       multiple_inflow$target[i]
                       ))
        }
      }

    }) %>%
      dplyr::bind_cols())

  } else {
    input[,ids]
  }


  date_or_datetime_col <- get_date_or_datetime_columns(input)

  stopifnot(length(date_or_datetime_col) > 0)

  flows_up_sum <- rowSums(section_flows, na.rm = TRUE)

  df_up <- if (return_inputs) {
    input[, date_or_datetime_col] %>%
      dplyr::bind_cols(section_flows)
  } else {
    input[, date_or_datetime_col]
  }

  df_up  %>%
    dplyr::bind_cols(tibble::tibble(!!section_id_up := flows_up_sum))

}
