#' Prepare Model input
#'
#' @param temporal_resolution specify temporal resolution of model input dataset.
#' (default: "days"). Valid options are: "days" or "hours"
#' @param config model network configuration (as retrieved by \code{\link{config_read}})
#' @param cso cso dataset (default: kwb.BerlinWaterModel::cso)
#' @param inflows inflows dataset (default: kwb.BerlinWaterModel::inflows)
#' @param rain rain dataset (default: kwb.BerlinWaterModel::rain)
#' @param evapo_p evaporation dataset (default: kwb.BerlinWaterModel::evapo_p)
#' @param shares_timeseries shares timeseries dataset (default: kwb.BerlinWaterModel::shares_timeseries)
#' @param ww waterworks dataset (default: kwb.BerlinWaterModel::ww)
#' @param wwtp wastewater treatment plant dataset (default: kwb.BerlinWaterModel::wwtp)
#' @param bfshare_dynamic should dynamic bankfiltration shares be used or the static
#' ones contained in column "bank_filtration_share" of config$flows_in_out
#' @param share_wwtp_sch_to_nordgraben_timeseries if TRUE time series to "Nordgraben"
#' is used (column name defined in parameter "col_wwtp_sch_nordgraben") and remaining
#' water is transfered to "Panke" (column name defined in parameter "col_wwtp_sch_panke")
#' @param share_wwtp_sch_panke_1 share of WWTP Schoenerlinde to Panke before (default: 0.1)
#' date_separation_panke_1_2; only used if share_wwtp_sch_to_nordgraben_timeseries == FALSE
#' @param  date_separation_panke_1_2 date to separate shares before (i.e. share_wwtp_sch_panke_1)
#' and after (share_wwtp_sch_panke_2) given date (default: "2025-04-15")
#' (default: 1 - share_wwtp_sch_panke); only used if share_wwtp_sch_to_nordgraben_timeseries == FALSE
#' @param share_wwtp_sch_panke_1 share of WWTP Schoenerlinde to Panke before (default: 0.9)
#' date_separation_panke_1_2; only used if share_wwtp_sch_to_nordgraben_timeseries == FALSE
#' @param col_wwtp_sch  column name of WWTP Schoenerlinde (default: "Q_KW_SCH")
#' @param col_wwtp_sch_nordgraben  column name of WWTP Schoenerlinde outflow to
#' Nordgraben (default: "Q_KW_SCH_Nordgraben")
#' @param col_wwtp_sch_panke column name of WWTP Schoenerlinde outflow to Panke
#' (default: "Q_KW_SCH_Panke")
#' @param col_panke_baseflow_no_Q_wwtp (default: "Panke_baseflow_no_Q_KW_SCH_Panke")
#' @param date_min minimum date, i.e. start of simulation (default: "2002-01-01)
#' @param date_max maximum date, i.e. end of simulation (default: "2022-12-31)
#' @param debug print debug messages (default: TRUE)
#' @return input dataset, will be filled in case temporal resolution in increased
#' @export
#' @importFrom stringr str_replace_na str_starts
#' @importFrom dplyr left_join select across relocate group_by
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate floor_date
prepare_input <- function(temporal_resolution = "days",
                          config = config_read(),
                          cso = kwb.BerlinWaterModel::cso,
                          inflows = kwb.BerlinWaterModel::inflows,
                          rain = kwb.BerlinWaterModel::rain,
                          evapo_p = kwb.BerlinWaterModel::evapo_p,
                          shares_timeseries = kwb.BerlinWaterModel::shares_timeseries,
                          ww = kwb.BerlinWaterModel::ww,
                          wwtp = kwb.BerlinWaterModel::wwtp,
                          bfshare_dynamic = FALSE,
                          share_wwtp_sch_to_nordgraben_timeseries = TRUE,
                          share_wwtp_sch_panke_1 = 0.1,
                          date_separation_panke_1_2 = "2015-04-15",
                          share_wwtp_sch_panke_2 = 0.9,
                          col_wwtp_sch = "Q_KW_SCH",
                          col_wwtp_sch_nordgraben = "Q_KW_SCH_Nordgraben",
                          col_wwtp_sch_panke = "Q_KW_SCH_Panke",
                          col_panke_baseflow_no_Q_wwtp = "Panke_baseflow_no_Q_KW_SCH_Panke",
                          date_min = "2002-01-01",
                          date_max = "2022-12-31",
                          debug = TRUE) {

  if(!share_wwtp_sch_to_nordgraben_timeseries) {
  stopifnot(share_wwtp_sch_panke_1 >= 0 &  share_wwtp_sch_panke_1 <= 1)
  stopifnot(share_wwtp_sch_panke_2 >= 0 &  share_wwtp_sch_panke_2 <= 1)

  share_wwtp_sch_nordgraben_1 <- 1 - share_wwtp_sch_panke_1
  stopifnot(is.numeric(share_wwtp_sch_panke_1))

  share_wwtp_sch_nordgraben_2 <- 1 - share_wwtp_sch_panke_2
  stopifnot(is.numeric(share_wwtp_sch_panke_2))
}

  stopifnot(temporal_resolution %in% c("days", "hours"))


  rain_df <-  kwb.utils::catAndRun(messageText = sprintf("Adding DWD rain data%s and convert from 'mm/h' to 'm3/s'",
                                                         ifelse(temporal_resolution == "days",
                                                                " and aggregate from 'hourly' to 'daily' values",
                                                                "")),
                                   expr = {
                                     if(temporal_resolution == "days") {
                                       rain %>%
                                         dplyr::mutate(date = as.Date(datetime, tz = "UTC")) %>%
                                         dplyr::relocate(date, .before = "datetime") %>%
                                         dplyr::select(- datetime) %>%
                                         dplyr::group_by(date) %>%
                                         dplyr::summarise(dplyr::across(.cols = tidyselect::starts_with("DWD"),
                                                                        .fns = ~ sum(.x, na.rm = TRUE)/(24*3600)/1000))
                                     } else {
                                       rain %>%
                                         dplyr::group_by(datetime) %>%
                                         dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("DWD"),
                                                                     .fns = ~ sum(.x, na.rm = TRUE)/3600/1000))
                                     }
                                    },
                                   dbg = debug
                                   ) %>%
    dplyr::filter()


  sections_with_area <- config$sections %>%
    dplyr::filter(!is.na(area_m2) & area_m2 > 0)


  rain.direct_tot  <- rain_df[,1] %>%
    dplyr::bind_cols(stats::setNames(lapply(sections_with_area$section_id, function(s_id) {
    rain_df$DWD_0433 * sections_with_area$area_m2[sections_with_area$section_id == s_id]
  }), nm = sprintf("rain.direct_%s", sections_with_area$section_id)) %>%
    dplyr::bind_rows())


  evapo_p_cbm_per_second <- evapo_p %>%
    dplyr::select(date, mean) %>%
    dplyr::mutate(mean = - mean / 1000 / 24 / 3600) %>%
    dplyr::rename(DWD_evapo.p  = mean)

  evapo_p_df <- if(temporal_resolution == "days") {
    evapo_p_cbm_per_second
  } else {
    evapo_p_cbm_per_second %>%
      kwb.BerlinWaterModel::fill_timeseries(temporal_resolution = "hours",
                                            direction = "down")
  }

  evapo_p_df_tot  <- evapo_p_df[,1] %>%
    dplyr::bind_cols(stats::setNames(lapply(sections_with_area$section_id, function(s_id) {
      evapo_p_df$DWD_evapo.p * sections_with_area$area_m2[sections_with_area$section_id == s_id]
    }), nm = sprintf("evapo.p_%s", sections_with_area$section_id)) %>%
      dplyr::bind_rows())


  col_datetime <- "date"
  col_datetime_join <- ifelse(temporal_resolution != "days", "datetime", col_datetime)

  date_separation_panke_1_2 <- if(col_datetime_join  == "date") {
    as.Date(date_separation_panke_1_2)
  } else {
    as.POSIXct(sprintf("%s 23:59:59", date_separation_panke_1_2), tz = "UTC")
  }

  aggregate_cso_to_timeframe <- function(data, temporal_resolution) {

    if (temporal_resolution == "hours") {
      data %>%
        dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour")) %>%
        dplyr::group_by(datetime, CSO_id) %>%
        dplyr::summarise(cbm_per_second = mean(cbm_per_second, na.rm = TRUE), .groups = "drop")
    } else if (temporal_resolution == "days") {
      data %>%
        dplyr::mutate(date = as.Date(lubridate::floor_date(datetime, "day"))) %>%
        dplyr::group_by(date, CSO_id) %>%
        dplyr::summarise(cbm_per_second = mean(cbm_per_second, na.rm = TRUE), .groups = "drop")
    } else {
      stop("Unsupported timeframe. Use 'hours' or 'days'.")
    }
  }

  cso_df <- aggregate_cso_to_timeframe(cso,
                                       temporal_resolution = temporal_resolution) %>%
    tidyr::pivot_wider(names_from = "CSO_id",
                       values_from = "cbm_per_second")

  bfstypes_equations <- config$bfstypes_equations %>%
    bfs_convert_equation()

  bfstypes_wellgalleries <- config$bfstypes_wellgalleries %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::mutate(flow_id = sprintf("%s%s",
                                    waterworks,
                                    dplyr::if_else(stringr::str_starts(gallery, "FRI"),
                                                   stringr::str_sub(gallery, 1,1) %>% tolower(),
                                                   stringr::str_sub(gallery, 1,4) %>% tolower())
                                                   )) %>%
    dplyr::arrange(flow_id)

  ww_bfshares <- get_bfshares(config = config,
                              ww = ww,
                              temporal_resolution = temporal_resolution,
                              bfshare_dynamic = bfshare_dynamic) %>%
    dplyr::mutate(is_mar = dplyr::if_else(is.na(is_mar), "no", is_mar))

  ww_bfshare_selected <- ww_bfshares %>%
    dplyr::select(- tidyselect::all_of(c("waterworks",
                                         "gallery",
                                         "type",
                                         "cbm_per_day",
                                         "is_mar",
                                         "total.cbm_per_second",
                                         "gw.cbm_per_second")),
                  - tidyselect::starts_with("bank_filtration_share")) %>%
    dplyr::rename(cbm_per_second = bf.cbm_per_second) %>%
    ### filter out duplicated flow_id for Johannisthal (used for "GW Haltung & Rowassergewinnung")
    ### otherwise -> non-unique id cols for tidyr::pivot_wider -> list data structure
    dplyr::filter(!(flow_id == "JOHnkh" & flow_type == "Rohwassergewinnung")) %>%
    dplyr::mutate(cbm_per_second = dplyr::if_else(flow_type %in% c("Grundwasserhaltung", "Altlastenabwehr"),
                                                  abs(cbm_per_second),
                                                  - abs(cbm_per_second))) %>%
    dplyr::select(- tidyselect::starts_with("flow_type")) %>%
    tidyr::pivot_wider(names_from = "flow_id", values_from = "cbm_per_second") %>%
    fill_timeseries(col_datetime = col_datetime,
                    temporal_resolution = temporal_resolution,
                    direction = "up")

  flows <- ww_bfshare_selected %>%
    dplyr::full_join(wwtp %>%
                       tidyr::pivot_wider(names_from = "id", values_from = "cbm_per_second") %>%
                       fill_timeseries(col_datetime = col_datetime,
                                       temporal_resolution = temporal_resolution,
                                       direction = "up"),
                     by = col_datetime_join) %>%
    dplyr::full_join(inflows %>% tidyr::pivot_wider(names_from = "id", values_from = "cbm_per_second") %>%
                       fill_timeseries(col_datetime = col_datetime,
                                       temporal_resolution = temporal_resolution,
                                       direction = "down"),
                     by = col_datetime_join)

  flows <- if (inherits(flows[[col_datetime_join]], "POSIXct")) {
    flows %>%
      dplyr::filter(!!as.symbol(col_datetime_join) >= as.POSIXct(sprintf("%s 00:00:00", date_min), tz = "UTC"),
                    !!as.symbol(col_datetime_join) <= as.POSIXct(sprintf("%s 23:59:59", date_max), tz = "UTC"))
  } else {
    flows %>%
      dplyr::filter(!!as.symbol(col_datetime_join) >= as.Date(date_min),
                    !!as.symbol(col_datetime_join) <= as.Date(date_max))
  }

  stopifnot(col_wwtp_sch %in% names(flows))
  stopifnot(c("Neuenhagener", "Q_KW_MHF", "Panke_baseflow") %in% names(flows))

  if(share_wwtp_sch_to_nordgraben_timeseries & !col_wwtp_sch_nordgraben %in% names(flows)) {
    stop(sprintf("You selected 'share_wwtp_sch_to_nordgraben_timeseries' == TRUE, but there is no flow_id named '%s' in the input dataset. Please add it to 'data/wwtp.rda' using 'data-raw/inputs.R'!",
                 col_wwtp_sch_nordgraben))
  }


  flows <- if(share_wwtp_sch_to_nordgraben_timeseries) {
    if(all(is.na(flows[[col_wwtp_sch_nordgraben]]))) {
      stop(sprintf("Please specify discharge from 'WWTP Schoenerlinde to Nordgraben in column '%s' of flows dataset",
                   col_wwtp_sch_nordgraben
                   ))
    }

    flows %>%
      dplyr::mutate("{col_wwtp_sch_panke}" := dplyr::if_else(.data[[col_wwtp_sch]] - .data[[col_wwtp_sch_nordgraben]] < 0,
                                                             0,
                                                             .data[[col_wwtp_sch]] - .data[[col_wwtp_sch_nordgraben]]))

  } else {
   flows %>%
     dplyr::mutate("{col_wwtp_sch_nordgraben}" := dplyr::if_else(.data[[col_datetime_join]] < date_separation_panke_1_2,
                                                                 share_wwtp_sch_nordgraben_1 *  .data[[col_wwtp_sch]],
                                                                 share_wwtp_sch_nordgraben_2 *  .data[[col_wwtp_sch]]),
                   "{col_wwtp_sch_panke}" := dplyr::if_else(.data[[col_datetime_join]] < date_separation_panke_1_2,
                                                            {{share_wwtp_sch_panke_1}} *  .data[[col_wwtp_sch]],
                                                            {{share_wwtp_sch_panke_2}} *  .data[[col_wwtp_sch]]))

  }



  flows <- flows %>%
    dplyr::mutate("Neuenhagener_no_Q_KW_MHF" = dplyr::if_else(.data[["Neuenhagener"]] - .data[["Q_KW_MHF"]] < 0,
                                                              0,
                                                              .data[["Neuenhagener"]] - .data[["Q_KW_MHF"]]),
                  "{col_panke_baseflow_no_Q_wwtp}" := dplyr::if_else(.data[["Panke_baseflow"]] - .data[[col_wwtp_sch_panke]] < 0,
                                                                       0,
                                                                       .data[["Panke_baseflow"]] - .data[[col_wwtp_sch_panke]])) %>%
    dplyr::left_join(rain_df, by = col_datetime_join)


  rain_runoff_segments <- config$flows_in_out %>%
    dplyr::filter(stringr::str_starts(flow_type, "rain_runoff"))


  missing_rainstation_id <- sapply(seq_len(nrow(rain_runoff_segments)), function(i) {
    !rain_runoff_segments$rainstation_id[i] %in% names(flows)
  })

  if(any(missing_rainstation_id)) {
    missing_rainstation_id

    message(sprintf("The rainstation_id '%s' was not found in the input dataset for rainfall-runoff flow_id = '%s' (%s).\nPlease add it manually!",
                 rain_runoff_segments$rainstation_id[missing_rainstation_id],
                 rain_runoff_segments$flow_id[missing_rainstation_id ],
                 rain_runoff_segments$flow_name[missing_rainstation_id ]))
  }

  rain_runoff_segments <- rain_runoff_segments[!missing_rainstation_id,]

  rainfall_runoff_df <- stats::setNames(lapply(seq_len(nrow(rain_runoff_segments)), function(i) {
    rain_runoff_segment <- rain_runoff_segments[i,]

    flows[[rain_runoff_segment$rainstation_id]] *  rain_runoff_segment$rain_runoff_coefficient *  rain_runoff_segment$rain_EZG_area_m2

  }), nm = rain_runoff_segments$flow_id) %>%
    dplyr::bind_cols()

  flows <- flows %>%
    dplyr::bind_cols(rainfall_runoff_df) %>%
    dplyr::left_join(cso_df, by = col_datetime_join) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("CSO"), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::left_join(rain.direct_tot, by = col_datetime_join) %>%
    dplyr::left_join(evapo_p_df_tot, by = col_datetime_join)

  check_shares_timeseries(shares_timeseries = shares_timeseries,
                          config = config,
                          debug = debug)

  shares_timeseries_wide <- if(!is.null(shares_timeseries)) {

    shares_timeseries_wide_tmp <- shares_timeseries %>%
      tidyr::pivot_wider(names_from = "id",
                         values_from = "share") %>%
      fill_timeseries(col_datetime = col_datetime,
                      temporal_resolution = temporal_resolution,
                      direction = "up")

    if (inherits(shares_timeseries_wide_tmp[[col_datetime_join]], "POSIXct")) {
      shares_timeseries_wide_tmp %>%
        dplyr::filter(!!as.symbol(col_datetime_join) >= as.POSIXct(sprintf("%s 00:00:00", date_min), tz = "UTC"),
                      !!as.symbol(col_datetime_join) <= as.POSIXct(sprintf("%s 23:59:59", date_max), tz = "UTC"))
    } else {
      shares_timeseries_wide_tmp %>%
        dplyr::filter(!!as.symbol(col_datetime_join) >= as.Date(date_min),
                      !!as.symbol(col_datetime_join) <= as.Date(date_max))
    }

  } else {
    NULL
  }

  input <- list(flows = flows,
                shares_timeseries = shares_timeseries_wide,
                ww_bfshares =   ww_bfshares %>%
                  fill_month_to_start(datetime_col = temporal_resolution,
                                      date_min = date_min,
                                      date_max = date_max)
                )
  attr(input, "bfshare_dynamic") <- bfshare_dynamic
  attr(input, "share_wwtp_sch_to_nordgraben_timeseries") <- share_wwtp_sch_to_nordgraben_timeseries
  attr(input, "temporal_resolution") <- temporal_resolution

  attr(input$flows, "bfshare_dynamic") <- bfshare_dynamic
  attr(input$flows, "share_wwtp_sch_to_nordgraben_timeseries") <- share_wwtp_sch_to_nordgraben_timeseries
  attr(input$flows, "temporal_resolution") <- temporal_resolution

  attr(input$shares_timeseries, "temporal_resolution") <- temporal_resolution

  input
}

