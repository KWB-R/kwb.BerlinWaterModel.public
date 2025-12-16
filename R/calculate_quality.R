#' Calculate Concentration
#'
#' @param c_in c_in concentration of inflow
#' @param c_0  c_0 concentration in section at t = 0
#' @param Q total inflow rate into section (m3/s)
#' @param V volume of section (m3)
#' @param k degradation parameter
#' @param t time in seconds (s)
#'
#' @returns concentration of substance at specific time
#' @export
#' @examples
#' calc_conc(c_in = 10, c_0 = 0, Q = 40000, V = 300000, k = 0, t = 1)
#'
calc_conc <- function(c_in, c_0, Q, V, k, t) {
  c_0_current <- numeric(length = length(t))
  c_0_current[1] <- c_0  # Initialisierung des ersten Wertes

  conc_values <- numeric(length = length(t))

  for (i in seq_along(t)) {
    # Parameterberechnung
    tau <- if (length(Q) > 1) {
      V / Q[i]
    } else {
      V / Q
    }

    alpha <- 1 / tau + k

    c_inf <- if (length(c_in) > 1) {
      (1 / tau * c_in[i]) / alpha
    } else {
      (1 / tau * c_in) / alpha
    }

    if (i > 1) {
      c_0_current[i] <- (c_0_current[i-1] - c_inf) * exp(-alpha * t[i - 1]) + c_inf
    }

    # Konzentration über die Zeit berechnen
    conc_values[i] <- (c_0_current[i] - c_inf) * exp(-alpha * t[i]) + c_inf
  }

  return(conc_values)
}



#' Calculate Quality in Section
#'
#' @param df data frame with model as retrieved by \code{\link{get_flowpath_table}}
#' @param input model input data flows as retrieved by \code{\link{prepare_input}}
#' and sublist "flows"
#' @param shares_timeseries_wide shares timeseries in wide format, as retrieved
#' by \code{\link{prepare_input}} and sublist "shares_timeseries" (default: NULL),
#' only used if parameter "use_dynamic" is set to TRUE
#' @param flows flows as retrieved by \code{\link{calculate_flows}}
#' @param quality list with sublist "conc" and "load" for all sections (default:
#' NULL)
#' @param config list with config as imported with \code{\link{config_read}}
#' @param reverse_flow calculate reverse flow (default: FALSE)
#' @param result_type define how the results should be returned. either a tibble
#' with loads and concentrations in long format.(if result_type == "tibble"), a list
#' with concentrations in wide format (result_type == "list") or a list with loads
#' in wide format (result_type == "load")
#' @param debug print debug messages (default: FALSE)
#' @return tibble with Urban Water Model results with loads and concentrations
#' (if result_type == "raw"), a list with concentrations (result_type ==  "conc" or "load")
#' or a list with loads, default: "list"
#' @export
#' @importFrom dplyr bind_cols bind_rows filter pull select
#' @importFrom tibble tibble
#' @importFrom purrr compact map reduce
#'
calculate_quality <- function(df,
                              input,
                              shares_timeseries_wide = NULL,
                              flows,
                              quality = NULL,
                              config,
                              reverse_flow = FALSE,
                              result_type = "list",
                              debug = FALSE) {


  use_dynamic <- attr(flows, "use_dynamic")

  section_idnames <- get_section_idnames(config)

  col_input_date_or_datetime <- get_date_or_datetime_columns(input)
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

  diff_time <- diff(input[[date_or_datetime_col]], units = "secs")
  period_seconds <- as.vector(c(diff_time, 0))

  model_input <- input %>%
    dplyr::left_join(flows,
                     by = col_date_or_datetime,
                     suffix = c(".annoying_duplicate_column", "")) %>%
    dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

  section_name_up <- unique(df$order_name)
  section_id_up <- get_ids_from_names(section_name_up, config = config)

  get_negflows <- function(section_id_up) {
    flows_neg <- flows[[section_id_up]]
    flows_neg[flows_neg > 0] <- 0
    flows_neg[flows_neg < 0] <- abs(flows_neg[flows_neg < 0] )
    flows_neg
  }

  flows_neg <- get_negflows(section_id_up)

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
    dplyr::filter(target %in% c(section_name_up, config$flows_in_out$flow_name)) %>%
    dplyr::mutate(flow_id = dplyr::if_else(source_id == section_id_up & !(target_id %in% sections_without_selected_section),
                                           target_id,
                                           source_id)) %>%
    dplyr::left_join(config$flows_in_out %>%
                       dplyr::select("flow_id", tidyselect::starts_with("conc")),
                     by = "flow_id"
                     )


  if(nrow(df_sel_up) == 0) {
    stop("There is no external inflow/section flowing to '%s: %s'. At least one inflow/section is required. Please check: 'config$section' or 'config$flows_in_out'")
  }


  df_sel_up <- shorten_ww_flow_id(df = df_sel_up, col_flow_id = "flow_id")


  cols_concentration <- names(df_sel_up)[stringr::str_starts(names(df_sel_up), "conc")]

  if(length(cols_concentration) == 0) {
    stop("No tracer/substance concentrations were provided. Please execute the functions
         add_tracer() and/or add_substances() after importing the configuration with
         config_read()!")
  }

  df_sel_up_in_section <- df_sel_up %>%
    dplyr::filter(source_id %in% section_idnames$id & source_id != section_id_up)

  upstream_section_loads_in <- if(nrow(df_sel_up_in_section) > 0) {
    kwb.utils::catAndRun(sprintf("Calculating upstream section load inflows for section '%s' (%s)",
                                 section_name_up,
                                 section_id_up),
                         expr = {

    if(is.null(quality)) {
      stop(sprintf("No water quality data for %d upstream sections of '%s: %s' were provided!",
                   nrow(df_sel_up_in_section),
                   section_id_up,
                   section_name_up))
    } else {
      load_upstream_total <- tibble::tibble()

      for(flow_id in df_sel_up_in_section$flow_id) {
        df_sel_up_in_section_tmp <- df_sel_up_in_section[df_sel_up_in_section$flow_id == flow_id, ]

        flow_section_cor <- ifelse(model_input[[flow_id]] < 0,
                                   0,
                                   if(use_dynamic) {
                                     if(!is.null(shares_timeseries_wide) & !is.na(df_sel_up_in_section_tmp$section_out_timeseries_id[1]) & df_sel_up_in_section_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide)) {
                                       if(debug)  {
                                         message(sprintf("use dynamic & time series for flow from section '%s' to '%s'",
                                                         df_sel_up_in_section_tmp$source[1],
                                                         df_sel_up_in_section_tmp$target[1]))
                                       }
                                       flow_function_result <- calculate_flow_share(flow = model_input[[flow_id]],
                                                                                    shares_timeseries_id = df_sel_up_in_section_tmp$section_out_timeseries_id[1],
                                                                                    shares_timeseries_wide = shares_timeseries_wide,
                                                                                    debug = debug)
                                       ifelse(flow_function_result < 0,
                                              0,
                                              flow_function_result)
                                     } else if(is.function(df_sel_up_in_section_tmp$section_out_function_parsed[[1]])) {
                                       if(debug)  {
                                         message(sprintf("use_dynamic & valid R function for flow from section '%s' to '%s'",
                                                         df_sel_up_in_section_tmp$source[1],
                                                         df_sel_up_in_section_tmp$target[1]))
                                       }
                                       flow_function_result <- df_sel_up_in_section_tmp$section_out_function_parsed[[1]](model_input[[flow_id]])
                                       ifelse(flow_function_result < 0,
                                              0,
                                              flow_function_result)
                                     } else if(is.numeric(df_sel_up_in_section_tmp$value)) {
                                       if(debug)  {
                                         message(sprintf("use_dynamic but no share time series and no R function but value is numeric for flow from section '%s' to '%s'",
                                                         df_sel_up_in_section_tmp$source[1],
                                                         df_sel_up_in_section_tmp$target[1]))
                                       }
                                         model_input[[flow_id]] * df_sel_up_in_section_tmp$value
                                     } else {
                                       stop(sprintf("No static value defined in 'section_out_share' in config file 'outflows_multiple.csv' for source '%s' to target '%s'. Please set it there!",
                                                    df_sel_up_in_section_tmp$source,
                                                    df_sel_up_in_section_tmp$target))
                                     }
                                     }
                                       else {
                                         if(is.numeric(df_sel_up_in_section_tmp$value)) {
                                           if(debug)  {
                                             message(sprintf("no dynamic, using static value for flow from section '%s' to '%s'",
                                                             df_sel_up_in_section_tmp$source[1],
                                                             df_sel_up_in_section_tmp$target[1]))
                                           }
                                           model_input[[flow_id]] * df_sel_up_in_section_tmp$value
                                         } else {
                                         stop(sprintf("No static value defined in 'section_out_share' in config file 'outflows_multiple' for source '%s' to target '%s'. Please set it there!",
                                                      df_sel_up_in_section_tmp$source,
                                                      df_sel_up_in_section$target
                                         ))
                                         }
                                       })


        load_upstream_total <- if(flow_id == df_sel_up_in_section$flow_id[1]) {

          quality$conc[[flow_id]][-1] * flow_section_cor * period_seconds
        } else {
          load_upstream_total +  quality$conc[[flow_id]][-1] * flow_section_cor * period_seconds
        }
      }

      tibble::as_tibble(load_upstream_total)
      # quality$load[[1]][,1] %>%
      #   dplyr::bind_cols(load_upstream_total)

    }},
    #newLine = 1,
    dbg = debug)

    } else {
      NULL
    }

  df_sel_down <- df %>%
    dplyr::filter(source == section_name_up,
                  target %in% get_section_idnames(config)$name) %>%
    dplyr::mutate(flow_id = target_id) %>%
    dplyr::left_join(config$flows_in_out %>%
                       dplyr::select("flow_id", tidyselect::starts_with("conc")),
                     by = "flow_id"
    )

  if (reverse_flow & "n_flow_below_zero" %in% names(df) == 0) {
    stop(sprintf("Column 'n_flow_below_zero' does not exist in provided '%s' input",
                 deparse(substitute(df)))
         )
  }

  df_sel_down_neg <- if(reverse_flow & sum(!is.na(df_sel_down$n_flow_below_zero)) > 0) {
    df_sel_down %>%
    dplyr::filter(!is.na(n_flow_below_zero))
  } else {
    tibble::tibble()
  }


  downstream_section_loads_in <- if (reverse_flow & nrow(df_sel_down_neg) > 0) {
    kwb.utils::catAndRun(
      sprintf(
        "Calculating downstream section load inflows for section '%s' (%s)",
        section_name_up,
        section_id_up
      ),
      expr = {
        if (is.null(quality)) {
          stop(sprintf(
            "No water quality data for %d downstream sections of '%s: %s' were provided!",
            nrow(df_sel_down_neg),
            section_id_up,
            section_name_up
          ))
        } else if (!all(df_sel_down_neg$target_id %in% names(quality$conc))) {
          missing_downstream_sections <- df_sel_down_neg$target_id[!df_sel_down_neg$target_id %in% names(quality$conc)]
          stop(sprintf(
            "No water quality data for %d downstream section(s) (i.e. %s) were provided!",
            nrow(df_sel_down_neg),
            paste0(
              sprintf(
                "%s: %s",
                missing_downstream_sections,
                get_names_from_ids(missing_downstream_sections, config)
              ),
              collapse = ", "
            )
          ))
        } else {
          load_downstream_total <- tibble::tibble()

          for (flow_id in df_sel_down_neg$flow_id) {
            df_sel_down_neg_tmp <- df_sel_down_neg[df_sel_down_neg$flow_id == flow_id, ]
            flow_section <- ifelse(model_input[[section_id_up]] < 0,
                                   abs(model_input[[section_id_up]]),
                                   0)

            flow_downstream_section_cor <- if (any(flow_section > 0)) {
              if (use_dynamic) {
                if (!is.null(shares_timeseries_wide) &&
                    !is.na(df_sel_down_neg_tmp$section_out_timeseries_id[1]) &&
                    df_sel_down_neg_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide)) {

                  if (debug) message(sprintf(
                    "use dynamic & time series for flow from downstream section '%s' to '%s'",
                    df_sel_down_neg_tmp$target[1],
                    df_sel_down_neg_tmp$source[1]
                  ))

                  flow_function_result <- calculate_flow_share(
                    flow = flow_section,
                    shares_timeseries_id = df_sel_down_neg_tmp$section_out_timeseries_id[1],
                    shares_timeseries_wide = shares_timeseries_wide,
                    debug = debug
                  )
                  ifelse(flow_function_result < 0, 0, flow_function_result)

                } else if (is.function(df_sel_down_neg_tmp$section_out_function_parsed[[1]])) {

                  if (debug) message(sprintf(
                    "use_dynamic & valid R function for flow from downstream section '%s' to '%s'",
                    df_sel_down_neg_tmp$target[1],
                    df_sel_down_neg_tmp$source[1]
                  ))

                  flow_function_result <- df_sel_down_neg_tmp$section_out_function_parsed[[1]](flow_section)
                  ifelse(flow_function_result < 0, 0, flow_function_result)

                } else if (is.numeric(df_sel_down_neg_tmp$value)) {

                  if (debug) message(sprintf(
                    "use_dynamic but no share time series and no R function but value is numeric for flow from downstream section '%s' to '%s'",
                    df_sel_down_neg_tmp$target[1],
                    df_sel_down_neg_tmp$source[1]
                  ))

                  abs(flow_section) * df_sel_down_neg_tmp$value

                } else {
                  stop(sprintf(
                    "No static value defined in 'section_out_share' in config file 'outflows_multiple.csv' for source '%s' to target '%s'. Please set it there!",
                    df_sel_down_neg_tmp$target,
                    df_sel_down_neg_tmp$source
                  ))
                }
              } else {
                if (is.numeric(df_sel_down_neg_tmp$value)) {

                  if (debug) message(sprintf(
                    "no dynamic, using static value for flow from section '%s' to '%s'",
                    df_sel_down_neg_tmp$target[1],
                    df_sel_down_neg_tmp$source[1]
                  ))

                  flow_section * df_sel_down_neg_tmp$value

                } else {
                  stop(sprintf(
                    "No static value defined in 'section_out_share' in config file 'outflows_multiple' for source '%s' to target '%s'. Please set it there!",
                    df_sel_down_neg_tmp$target,
                    df_sel_down_neg_tmp$source
                  ))
                }
              }
            } else {
              NULL
            }

            load_downstream_total <- if (is.null(flow_downstream_section_cor)) {
              NULL
            } else {
              if (flow_id == df_sel_down_neg$flow_id[1]) {
                quality$conc[[flow_id]][-1] * flow_downstream_section_cor * period_seconds
              } else {
                load_downstream_total + quality$conc[[flow_id]][-1] * flow_downstream_section_cor * period_seconds
              } %>%
                tibble::as_tibble()
            }
          }
          load_downstream_total
        }
      },
      dbg = debug)
    } else {
    NULL
  }


  downstream_negflow_section_loads_in <- if(is.null(quality$load_neg[[section_id_up]])) {
    NULL
  } else {
    section_ids <- names(quality$load_neg[[section_id_up]])

    # Sum of all matrices for section ids
    x <- section_ids %>%
      purrr::map(~ quality$load_neg[[section_id_up]][[.x]]) %>%     # extract list of matrices
      purrr::reduce(`+`)                           # element wise addition for all matrices

    if(is.list(x)) {
      x %>%
        dplyr::bind_cols()
    } else {
      x
    }

  }

  ### Adapt for config/backflows_multiple
  downstream_negflow_section_flows_in <- if(is.null(quality$load_neg[[section_id_up]])) {
    0
  } else {

    section_ids <- names(quality$load_neg[[section_id_up]])

    lapply(section_ids, function(s_id) {

      backflows_multiple <- config$backflows_multiple %>%
        dplyr::mutate(backflow_id = stringr::str_remove(backflow_id, pattern = ".*_")) %>%
        dplyr::filter(section_backflow_out_id == section_id_up,
                      backflow_id == s_id) %>%
        dplyr::select(backflow_id, section_backflow_out_share) %>%
        dplyr::rename(share = section_backflow_out_share)


      share_flow <- if(nrow(backflows_multiple) == 1) {
        backflows_multiple$share
      } else if (nrow(backflows_multiple) == 0) {
        1
      } else {
        stop("This should never ever happen! ;-)")
      }

      get_negflows(s_id) * share_flow
    }) %>%
    purrr::reduce(`+`)

  }

  downstream_loads_in <- if(reverse_flow & (!is.null(downstream_section_loads_in) | !is.null(downstream_negflow_section_loads_in)))  {

    # Case 0: initialise matrix  with downstream section loads as default
    loads_neg_cor <- if(!is.null(downstream_section_loads_in)) {
      downstream_section_loads_in
    } else {
      downstream_negflow_section_loads_in
    }

    # Case 1: both flows_neg are zero -> set load to zero
    if(any(flows_neg == 0 & downstream_negflow_section_flows_in == 0)) {
      if(debug) message(">>>> Case 1: both flows_neg are zero -> set load to zero")
    loads_neg_cor[flows_neg == 0 & downstream_negflow_section_flows_in == 0, ] <- 0
    }

    # Case 2: section flow_neg is zero, downstream flow_neg exists -> use downstream negflow load
    if(any(flows_neg == 0 & downstream_negflow_section_flows_in > 0)) {
      if(debug) message(">>>> Case 2: section flow_neg is zero, downstream flow_neg exists -> use downstream negflow load")
    loads_neg_cor[flows_neg == 0 & downstream_negflow_section_flows_in > 0, ] <- downstream_negflow_section_loads_in[flows_neg == 0 & downstream_negflow_section_flows_in > 0, ]
    }
    # Case 3: downstream flow_neg is zero, section flow_neg exists -> use section negflow load
    if(any(flows_neg > 0 & downstream_negflow_section_flows_in == 0)) {
      if(debug) message(">>>> Case 3: downstream flow_neg is zero, section flow_neg exists -> use section negflow load")
      loads_neg_cor[flows_neg > 0 & downstream_negflow_section_flows_in == 0, ] <- downstream_section_loads_in[flows_neg > 0 & downstream_negflow_section_flows_in == 0, ]
    }

    # Case 4: both have negative flows -> choose load based on total difference
    flows_double_neg_ids <- which(flows_neg > 0 & downstream_negflow_section_flows_in > 0)
    if(length(flows_double_neg_ids) > 0) {
      if(debug) message(">>>> Case 4: both have negative flows -> choose load based on total difference")

    for (idx in flows_double_neg_ids) {
      flows_double_neg_loads_diff <- rowSums(downstream_section_loads_in[idx, ]) -
        rowSums(downstream_negflow_section_loads_in[idx, ])

      loads_neg_cor[idx, ] <- if (flows_double_neg_loads_diff > 0) {
        downstream_section_loads_in[idx, ]
      } else {
        downstream_negflow_section_loads_in[idx, ]
      }
     }
    }

    loads_neg_cor
  } else {
    NULL
  }



df_sel_up_in <- df_sel_up %>%
    dplyr::filter(!source %in% sections_without_selected_section,
                  target_id == section_id_up)


  inflows_lateral_loads_in <-  if(nrow(df_sel_up_in) > 0) {
    stats::setNames(lapply(cols_concentration, function(col_concentration) {

    kwb.utils::catAndRun(sprintf("Calculating inflows and lateral section inflow concentration for '%s'",
                                 col_concentration),
                         expr = {

                           col_load <- stringr::str_replace(col_concentration, "conc", "load") %>% stringr::str_remove("\\.m3")

                           inflows_lateral_loads_in <- lapply(seq_len(nrow(df_sel_up_in)), function(i) {

                             df_sel_up_in_tmp <- df_sel_up_in[i,]
                             inflow_id <- df_sel_up_in_tmp$flow_id

                             if(!inflow_id %in% names(model_input)) {
                               stop(sprintf("Required dataset with flow_id '%s' for calculating %s (%s) is missing in provided input/flow dataset. Please fix it!",
                                            inflow_id,
                                            section_id_up,
                                            section_name_up))
                             }

                             stopifnot(inflow_id %in% names(model_input))

                             flow_load <- if (use_dynamic & !is.null(shares_timeseries_wide) & !is.na(df_sel_up_in_tmp$section_out_timeseries_id[1]) & df_sel_up_in_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide)) {
                               calculate_flow_share(flow = model_input[[inflow_id]], shares_timeseries_id = df_sel_up_in_tmp$section_out_timeseries_id[1], shares_timeseries_wide = shares_timeseries_wide, debug = debug) * period_seconds * df_sel_up_in_tmp[[col_concentration]]
                             } else if (use_dynamic & is.function(df_sel_up_in_tmp$section_out_function_parsed[[1]])) {
                               df_sel_up_in_tmp$section_out_function_parsed[[1]](model_input[[inflow_id]]) * period_seconds * df_sel_up_in_tmp[[col_concentration]]
                             } else if (df_sel_up_in_tmp$value < 1) {
                                 model_input[[inflow_id]] * period_seconds * df_sel_up_in_tmp$value * df_sel_up_in_tmp[[col_concentration]]
                             } else {
                               model_input[[inflow_id]] * period_seconds * df_sel_up_in_tmp[[col_concentration]]

                             }

                             flow_load <- ifelse(flow_load < 0, 0, flow_load)

                             tibble::tibble("{inflow_id}.{col_load}" := flow_load)

                           }) %>%
                             dplyr::bind_cols()


                           col_name <- sprintf("%s", col_load) %>% stringr::str_remove("\\.m3")

                           tibble::tibble(!!col_name := rowSums(inflows_lateral_loads_in, na.rm = TRUE))
                         },
                         newLine = 1,
                         dbg = debug)

  }), nm = stringr::str_remove(cols_concentration, "conc_")) %>%
    dplyr::bind_cols()
  } else {
    NULL
  }


    # Liste aller möglichen Inputs
    loads_list_export <- stats::setNames(list(
      upstream_section_loads_in,
      inflows_lateral_loads_in,
      downstream_loads_in,
      downstream_section_loads_in,
      downstream_negflow_section_loads_in
    ), nm = c("upstream", "lateral", "downstream", "downstream_section", "downstream_negative"))

    # Entferne NULL-Einträge
    loads_non_null_export <- purrr::compact(loads_list_export)

    if (length(loads_non_null_export) == 0) {
      stop(sprintf("No lateral inflow, upstream or downstream section defined for '%s (%s)'",
                   section_name_up,
                   section_id_up))
    }

    # Falls nur ein Element vorhanden ist, gib es direkt zurück
    # Falls mehrere vorhanden sind, summiere sie auf
    loads_in_export <- if (length(loads_non_null_export) == 1) {
      loads_non_null_export[[1]]
    } else {
      purrr::reduce(loads_non_null_export, `+`) %>% tibble::as_tibble()
    }

    names(loads_in_export) <- stringr::str_replace(names(loads_in_export), ".*load_", "")


    # Liste aller möglichen Inputs
    loads_list <- stats::setNames(list(
      upstream_section_loads_in,
      inflows_lateral_loads_in,
      downstream_loads_in
    ), nm = c("upstream", "lateral", "downstream"))

    # Entferne NULL-Einträge
    loads_non_null <- purrr::compact(loads_list)

    if (length(loads_non_null) == 0) {
      stop(sprintf("No lateral inflow, upstream or downstream section defined for '%s (%s)'",
                   section_name_up,
                   section_id_up))
    }

    # Falls nur ein Element vorhanden ist, gib es direkt zurück
    # Falls mehrere vorhanden sind, summiere sie auf
    loads_in <- if (length(loads_non_null) == 1) {
      loads_non_null[[1]]
    } else {
      purrr::reduce(loads_non_null, `+`) %>% tibble::as_tibble()
    }

    names(loads_in) <- stringr::str_replace(names(loads_in), ".*load_", "")


    flows_in <-  kwb.utils::catAndRun(sprintf("Calculating inflows and lateral section inflows for section '%s' (%s)",
                                              section_name_up,
                                              section_id_up),
                                      expr = {

                                        if(is.null(upstream_section_loads_in) & !is.null(inflows_lateral_loads_in)) {
                                          lapply(seq_len(nrow(df_sel_up_in)), function(i) {
                                            df_sel_up_in_tmp <- df_sel_up_in[i,]
                                            flow_cor_lateral <- ifelse(model_input[[df_sel_up_in_tmp$flow_id]] < 0, 0, model_input[[df_sel_up_in_tmp$flow_id]])

                                            tibble::tibble("{df_sel_up_in_tmp$flow_id}" := dplyr::case_when(
                                              use_dynamic && !is.null(shares_timeseries_wide) && !is.na(df_sel_up_in_tmp$section_out_timeseries_id[1]) && df_sel_up_in_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide) ~ calculate_flow_share(flow = flow_cor_lateral,
                                                                                                                                                                                                                                                                  shares_timeseries_id = df_sel_up_in_tmp$section_out_timeseries_id[1],
                                                                                                                                                                                                                                                                  shares_timeseries_wide = shares_timeseries_wide,
                                                                                                                                                                                                                                                                  debug = debug),
                                              use_dynamic && is.function(df_sel_up_in_tmp$section_out_function_parsed[[1]]) ~ check_if_function_and_calculate_flow(df_sel_up_in_tmp, flow_cor_lateral),
                                              !use_dynamic && df_sel_up_in_tmp$value < 1 || use_dynamic && !is.function(df_sel_up_in_tmp$section_out_function_parsed[[1]]) && df_sel_up_in_tmp$value < 1  ~ flow_cor_lateral * df_sel_up_in_tmp$value,
                                              .default = flow_cor_lateral)
                                            )
                                          }) %>%
                                            dplyr::bind_cols() %>%
                                            rowSums(na.rm = TRUE)

                                        } else if (is.null(inflows_lateral_loads_in) & !is.null(upstream_section_loads_in)) {
                                          lapply(seq_len(nrow(df_sel_up_in_section)), function(i) {
                                            df_sel_up_in_section_tmp <- df_sel_up_in_section[i,]
                                            flow_cor_section <- ifelse(model_input[[df_sel_up_in_section_tmp$flow_id]] < 0, 0, model_input[[df_sel_up_in_section_tmp$flow_id]])

                                            tibble::tibble("{df_sel_up_in_section_tmp$flow_id}" :=  dplyr::case_when(
                                              use_dynamic && !is.null(shares_timeseries_wide) && !is.na(df_sel_up_in_section_tmp$section_out_timeseries_id[1]) && (df_sel_up_in_section_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide)) ~ calculate_flow_share(flow = flow_cor_section,
                                                                                                                                                                                                                                                                                    shares_timeseries_id = df_sel_up_in_section_tmp$section_out_timeseries_id[1],
                                                                                                                                                                                                                                                                                    shares_timeseries_wide = shares_timeseries_wide),
                                              use_dynamic && is.function(df_sel_up_in_section_tmp$section_out_function_parsed[[1]]) ~  check_if_function_and_calculate_flow(df_sel_up_in_section_tmp, flow_cor_section),
                                              !use_dynamic && df_sel_up_in_section_tmp$value < 1 || use_dynamic && !is.function(df_sel_up_in_section_tmp$section_out_function_parsed[[1]]) && df_sel_up_in_section_tmp$value < 1  ~ flow_cor_section * df_sel_up_in_section_tmp$value,
                                              .default = flow_cor_section)
                                            )
                                          }) %>%
                                            dplyr::bind_cols() %>%
                                            rowSums(na.rm = TRUE)

                                        } else {
                                          lapply(seq_len(nrow(df_sel_up_in)), function(i) {
                                            df_sel_up_in_tmp <- df_sel_up_in[i,]
                                            flow_cor_lateral <- ifelse(model_input[[df_sel_up_in_tmp$flow_id]] < 0, 0, model_input[[df_sel_up_in_tmp$flow_id]])

                                            tibble::tibble("{df_sel_up_in_tmp$flow_id}" :=  dplyr::case_when(
                                              use_dynamic && !is.null(shares_timeseries_wide) && !is.na(df_sel_up_in_tmp$section_out_timeseries_id[1]) && (df_sel_up_in_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide)) ~ calculate_flow_share(flow = flow_cor_lateral,
                                                                                                                                                                                                                                                                    shares_timeseries_id = df_sel_up_in_tmp$section_out_timeseries_id[1],
                                                                                                                                                                                                                                                                    shares_timeseries_wide = shares_timeseries_wide,
                                                                                                                                                                                                                                                                    debug = debug),
                                              use_dynamic && is.function(df_sel_up_in_tmp$section_out_function_parsed[[1]]) ~ check_if_function_and_calculate_flow(df_sel_up_in_tmp, flow_cor_lateral),
                                              !use_dynamic && df_sel_up_in_tmp$value < 1 || use_dynamic && !is.function(df_sel_up_in_tmp$section_out_function_parsed[[1]]) && df_sel_up_in_tmp$value < 1  ~ flow_cor_lateral * df_sel_up_in_tmp$value,
                                              .default = flow_cor_lateral)
                                            )

                                          }) %>%
                                            dplyr::bind_cols() %>%
                                            dplyr::bind_cols(lapply(seq_len(nrow(df_sel_up_in_section)), function(i) {
                                              df_sel_up_in_section_tmp <- df_sel_up_in_section[i,]
                                              flow_cor_section <- ifelse(model_input[[df_sel_up_in_section_tmp$flow_id]] < 0, 0, model_input[[df_sel_up_in_section_tmp$flow_id]])
                                              tibble::tibble("{df_sel_up_in_section_tmp$flow_id}" :=  dplyr::case_when(
                                                use_dynamic && !is.null(shares_timeseries_wide) && !is.na(df_sel_up_in_section_tmp$section_out_timeseries_id[1]) && (df_sel_up_in_section_tmp$section_out_timeseries_id[1] %in% names(shares_timeseries_wide)) ~ calculate_flow_share(flow = flow_cor_section,
                                                                                                                                                                                                                                                                                      shares_timeseries_id = df_sel_up_in_section_tmp$section_out_timeseries_id[1],
                                                                                                                                                                                                                                                                                      shares_timeseries_wide = shares_timeseries_wide,
                                                                                                                                                                                                                                                                                      debug = debug),
                                                use_dynamic && is.function(df_sel_up_in_section_tmp$section_out_function_parsed[[1]]) ~ check_if_function_and_calculate_flow(df_sel_up_in_section_tmp, flow_cor_section),
                                                !use_dynamic && df_sel_up_in_section_tmp$value < 1 || use_dynamic && !is.function(df_sel_up_in_section_tmp$section_out_function_parsed[[1]]) && df_sel_up_in_section_tmp$value < 1 ~ flow_cor_section * df_sel_up_in_section_tmp$value,
                                                .default = flow_cor_section)
                                              )
                                            }) %>%
                                              dplyr::bind_cols()) %>%
                                            rowSums(na.rm = TRUE)
                                        }
                                      },
                                      newLine = 1,
                                      dbg = debug
    )


    if(reverse_flow) {
    flows_in <-  kwb.utils::catAndRun(sprintf("Adding downstream (compensation of negative section flow) inflows for section '%s' (%s)",
                                              section_name_up,
                                              section_id_up),
                                      expr = {

                                        flows_downstream <- {
                                          flows_neg_cor <- flows_neg

                                          # Case 1: both flows are zero -> set flow to zero
                                          if(any(flows_neg == 0 & downstream_negflow_section_flows_in == 0)) {
                                          flows_neg_cor[flows_neg == 0 & downstream_negflow_section_flows_in == 0] <- 0
                                          }
                                          # Case 2: upstream flow is zero, downstream flow exists -> use downstream flow
                                          if(any(downstream_negflow_section_flows_in > 0 & flows_neg == 0)) {
                                          flows_neg_cor[downstream_negflow_section_flows_in > 0 & flows_neg == 0] <-
                                            downstream_negflow_section_flows_in[downstream_negflow_section_flows_in > 0 & flows_neg == 0]
                                          }
                                          # Case 3: both have negative flows -> choose based on total load difference
                                          flows_double_neg_ids <- which(flows_neg > 0 & downstream_negflow_section_flows_in > 0)
                                          if (length(flows_double_neg_ids) > 0) {
                                            flows_double_neg_loads_diff <- rowSums(downstream_section_loads_in[flows_double_neg_ids,]) -
                                              rowSums(downstream_negflow_section_loads_in[flows_double_neg_ids,])

                                            flows_neg_cor[flows_double_neg_ids] <- ifelse(flows_double_neg_loads_diff > 0,
                                                                                          flows_neg[flows_double_neg_ids],
                                                                                          downstream_negflow_section_flows_in[flows_double_neg_ids])
                                          }

                                          flows_neg_cor
                                        }

                                        flows_in + flows_downstream

                                      },
                                      newLine = 1,
                                      dbg = debug)
    }




    c_in <- if(sum(flows_in <= 0) > 0) {
      c_in_tmp <- loads_in/(flows_in * period_seconds)
      c_in_tmp[flows_in <= 0,] <- 0
      c_in_tmp
      } else {
      #message("All > 0 cbm/second")
      loads_in/(flows_in * period_seconds)
      }

    names(c_in) <- cols_concentration

    section <- config$sections %>%
      dplyr::filter(.data[["section_id"]] == section_id_up)

    conc_in  <-  stats::setNames(lapply(cols_concentration, function(col_concentration) {

    substance_name <-  stringr::str_remove(col_concentration, pattern = "conc_") %>% stringr::str_remove("\\..*")

    substance_name_in_k <- config$k$substance_name %in% substance_name

    k <- if(sum(substance_name_in_k) == 1) {
      kwb.utils::catAndRun(messageText = sprintf("Setting 'k' for '%s' from config$k to %f",
                                                 substance_name,
                                                 config$k$k[substance_name_in_k]),
                           expr = config$k$k[substance_name_in_k],
                           dbg = debug)
    } else {
      kwb.utils::catAndRun(messageText = sprintf("Setting 'k' for '%s' to 0 as no substance is defined in config$k",
                                                 ifelse(substance_name == "tracer",
                                                        stringr::str_remove(col_concentration,
                                                                            pattern = "conc_"),
                                                        substance_name)),
                           expr = 0,
                           dbg = debug)
    }

    is_outflow_and_no_volume <- stringr::str_starts(section$section_id, "Out") & is.na(section$volume_m3)
    outflow_storage_volume <- 1 # m3

    V <- if(is_outflow_and_no_volume ) {
      kwb.utils::catAndRun(sprintf("Setting storage volume for '%s' from 'NA' to '%d m3'",
                                   sprintf("%s (%s)",
                                           section$section_id,
                                           section$section_name),
                                   outflow_storage_volume),
                           expr = {
                             outflow_storage_volume
                           },
                           dbg = debug)
    } else {
      if(is.na(section$volume_m3)) {
        stop(sprintf("No section volume defined for '%s'. Please define it config$section",
                     sprintf("%s (%s)",
                             section$section_id,
                             section$section_name)
                             )
             )
      } else {
        section$volume_m3
      }
    }

    section_concentration <- calc_conc(c_in = c_in[[col_concentration]],
                                       c_0 = ifelse(col_concentration %in% names(section),
                                                    section[[col_concentration]],
                                                    0
                                                    ),
                                       Q =  ifelse(flows_in <= 0,
                                                   0.000001,
                                                   flows_in),
                                       V = V,
                                       k =  k,
                                       t = period_seconds)

    #col_load <- stringr::str_replace(col_concentration, "conc", "load") %>% stringr::str_remove("\\.m3")

    model_input[, date_or_datetime_col] %>%
      dplyr::bind_cols(
        tibble::tibble(#"{section_id_up}.{col_load}" :=  loads_in[[substance_name_load_unit]] ,
                       "{section_id_up}.{col_concentration}" := section_concentration)
        )

  }), nm = stringr::str_remove(cols_concentration, "conc_"))


  loads_conc_in_df_long <-  conc_in %>%
    purrr::reduce(dplyr::full_join, by = date_or_datetime_col) %>%
    tidyr::pivot_longer(- tidyselect::all_of(date_or_datetime_col)) %>%
    dplyr::mutate(value = dplyr::if_else(is.nan(value), 0, value),
                  section_id = stringr::str_remove(name, "\\..*"),
                  para_type = stringr::str_remove(name, section_id) %>%
                    stringr::str_remove(".") %>%
                    stringr::str_remove("_.*"),
                  para_name = stringr::str_remove(name, ".*_") %>%
                    stringr::str_remove("\\..*"),
                  para_source = dplyr::if_else(para_name == "tracer",
                                               stringr::str_remove(name, ".*tracer\\."),
                                               NA_character_),
                  para_unit = dplyr::if_else(para_name != "tracer",
                                             stringr::str_remove(name, sprintf(".*%s\\.", para_name)) %>%
                                             stringr::str_replace("\\.", "/"),
                                             NA_character_)) %>%
    dplyr::bind_rows()

  get_result <- function(result_type) {

  section_ids <- loads_conc_in_df_long  %>%
    dplyr::filter(para_type == result_type) %>%
    dplyr::pull(section_id) %>%
    unique()

  stats::setNames(lapply(section_ids, function(section_id) {
    loads_conc_in_df_long  %>%
      dplyr::filter(para_type == result_type,
                    section_id == section_id) %>%
      dplyr::mutate(name_short = stringr::str_remove(name, pattern = sprintf("%s.%s_", section_id, result_type))) %>%
      dplyr::select(tidyselect::all_of(date_or_datetime_col), name_short, value) %>%
      tidyr::pivot_wider(names_from = name_short, values_from = value)
  }), nm = section_ids)
  }

  conc <- get_result("conc")

  ### get upstream sections
  upstream_sections <- if(reverse_flow) {
    df %>%
    dplyr::filter(source_id %in% get_section_idnames(config)$id,
                  source_id != section_id_up,
                  tolower(no_transfer_section_up_neg_flow) != "yes" | is.na(no_transfer_section_up_neg_flow))
  } else {
  NULL
}


 load_neg <- if(reverse_flow) {

  if(nrow(upstream_sections) == 1) {
    if(debug) {
      sprintf("adding backflow from '%s' (%s) to upstream section %s",
              section_id_up,
              section_name_up,
              paste0(sprintf("'%s' (%s)",
                             upstream_sections$source_id,
                             upstream_sections$source),
                     collapse = ", "))
    }


    stats::setNames(list(stats::setNames(list(conc[[section_id_up]][,-1] * flows_neg * period_seconds),
                                         section_id_up)),
                    nm = upstream_sections$source_id)

  } else if (nrow(upstream_sections) > 1) {
    if(debug) {
      sprintf("adding backflow from '%s' (%s) to upstream sections %s",
              section_id_up,
              section_name_up,
              paste0(sprintf("'%s' (%s)",
                             upstream_sections$source_id,
                             upstream_sections$source),
                     collapse = " & "))
    }


    upstream_sections_defined <- upstream_sections %>%
      dplyr::group_by(order_name) %>%
      dplyr::summarise(n = dplyr::n(),
                       n_backflows = sum(!is.na(section_backflow_out_share))) %>%
      dplyr::filter(n_backflows > 0)

    stopifnot(upstream_sections_defined$n == upstream_sections_defined$n | is.na(upstream_sections_defined$n))

    # divide upstream backflow by share provided in column "section_backflow_out_share" in  configuration file "config/backflows_multiple.csv"
    stats::setNames(lapply(seq_len(nrow(upstream_sections)), function(i) {
     stats::setNames(list((conc[[section_id_up]][,-1] * flows_neg * upstream_sections$section_backflow_out_share[i] * period_seconds)),
        nm = section_id_up)}),
     nm = upstream_sections$source_id)

  } else {
    if(debug) {
      "no upstream section to add backflowing water"
    }
  }
 } else {
   NULL
  }


  if (result_type == "list") {
     list(conc = conc,
          load = stats::setNames(list(loads_list_export), nm = section_id_up),
          load_neg = load_neg,
          flows = stats::setNames(list(flows_in), nm = section_id_up)
          )
    } else {
      loads_conc_in_df_long
    }

}
