#' Get Bank Filtration shares Model input
#'
#' @param config model network configuration (as retrieved by \code{\link{config_read}})
#' @param ww waterworks dataset (default: kwb.BerlinWaterModel.public::ww)
#' @param temporal_resolution specify temporal resolution of model input dataset.
#' (default: "days"). Valid options are: "days" or "hours"
#' @param bfshare_dynamic should dynamic bankfiltration shares be used or the static
#' ones contained in column "bank_filtration_share" of config$flows_in_out
#' @return data framer with flow ids, date/datetime and well gallery metadata,
#' column "bank_filtration_share" is set depending on bfshare_dynamic == TRUE
#' (based on column "bank_filtration_share_dynamic") or FALSE (bank_filtration_share_static )
#' @importFrom dplyr arrange filter mutate if_else select rename left_join count
#' pull
#' @importFrom stringr str_starts str_sub
#' @importFrom tidyr replace_na
#' @importFrom gridExtra grid.arrange
#' @export
#'
get_bfshares <- function(config,
                         ww = kwb.BerlinWaterModel.public::ww,
                         temporal_resolution = "days",
                         bfshare_dynamic = TRUE) {

stopifnot(temporal_resolution %in% c("days", "hours"))

if(sum(unique(config$flows_in_out$is_mar) %in% c(NA_character_, "no", "yes")) == 0) {

  specified_vals <- unique(config$flows_in_out$is_mar)[unique(config$flows_in_out$is_mar) %in% c(NA_character_, "no", "yes")]

  stop(sprintf(paste0("Please specfify well galleries in config file 'flows_in_out.csv', ",
  "which receive mainly surface water from managed aquifer recharge by setting ",
  "the specific 'flow_id' via column 'is_mar' to 'yes'. 'NA' or 'no' will be ",
  "considered as bank filtration well galleries.\nCurrently the follwing values are specified:\n%s"),
  paste0(specified_vals, collapse = ", "))
  )

}

col_datetime <- "date"
col_datetime_join <- ifelse(temporal_resolution != "days", "datetime", col_datetime)


bfstypes_equations <- config$bfstypes_equations %>%
  bfs_convert_equation()

bfstypes_wellgalleries <- config$bfstypes_wellgalleries %>%
  dplyr::filter(!is.na(type)) %>%
  dplyr::mutate(flow_id = sprintf("%s%s",
                                  waterworks,
                                  dplyr::if_else(stringr::str_starts(gallery, "FRI"),
                                                 stringr::str_sub(gallery, 1,1) %>% tolower(),
                                                 stringr::str_sub(gallery, 1,4) %>% tolower())
  )
  ) %>%
  dplyr::arrange(flow_id)


ww_bfshare <- config$flows_in_out %>%
  dplyr::filter(stringr::str_starts(flow_id, "WW")) %>%
  shorten_ww_flow_id() %>%
  dplyr::select(flow_id, bank_filtration_share, is_mar) %>%
  dplyr::rename(bank_filtration_share_static = bank_filtration_share) %>%
  dplyr::left_join(ww, by = c("flow_id" = "id")) %>%
  dplyr::filter(!is.na(cbm_per_second)) %>%
  dplyr::left_join(bfstypes_wellgalleries,
                   by = "flow_id") %>%
  dplyr::mutate(cbm_per_day = cbm_per_second * 24 * 3600,
                bank_filtration_share_dynamic = NA) %>%
  dplyr::filter(!is.na(date))


for(type in bfstypes_equations$type) {

  bfs_dynamic <- bfstypes_equations[bfstypes_equations$type == type,]

  condition_flow <- !is.na(ww_bfshare$cbm_per_day) & ww_bfshare$cbm_per_day > ifelse(is.na(bfs_dynamic$Q_min_m3d), 0, bfs_dynamic$Q_min_m3d) & ww_bfshare$cbm_per_day < ifelse(is.na(bfs_dynamic$Q_max_m3d), 9999999999, bfs_dynamic$Q_max_m3d)

  condition_type_and_flow <- !is.na(ww_bfshare$type) & ww_bfshare$type == type & condition_flow

  flow_cbm_per_day <- ww_bfshare$cbm_per_day[condition_type_and_flow]


  bfs_dyn <- bfs_dynamic$equation_function[[1]](flow_cbm_per_day)  / 100

  condition_out_of_range <- flow_cbm_per_day > 0 & is.na(bfs_dyn)
  if(any(condition_out_of_range)) {
    warning(sprintf("There are %d data points out fo range for the following flow ids: %s\nSetting values to -9999",
                    sum(condition_out_of_range),
                    paste0(unique(ww_bfshare$flow_id[condition_out_of_range]), collapse = ", ")))
    ww_bfshare$bank_filtration_share_dynamic[condition_out_of_range] <- - 9999
  }

  ww_bfshare$bank_filtration_share_dynamic[condition_type_and_flow] <-  bfs_dynamic$equation_function[[1]](flow_cbm_per_day) / 100

}

col_bfshare <- ifelse(bfshare_dynamic,
                      "bank_filtration_share_dynamic",
                      "bank_filtration_share_static")

ww_bfshare %>%
  dplyr::rename(total.cbm_per_second = cbm_per_second) %>%
  dplyr::mutate(bank_filtration_share = dplyr::if_else(stringr::str_ends(flow_id, pattern = "gwa") &  bfshare_dynamic,
                                                       1,
                                                       dplyr::if_else(is.na(type),
                                                                      bank_filtration_share_static,
                                                                      tidyr::replace_na(.data[[col_bfshare]], 0))),
                bf.cbm_per_second = bank_filtration_share * tidyr::replace_na(total.cbm_per_second, 0),
                gw.cbm_per_second = (1 - bank_filtration_share) * tidyr::replace_na(total.cbm_per_second, 0))

}


if(FALSE) {
  ww_bfshare <- get_bfshares(config = config,
                             ww = ww,
                             temporal_resolution = temporal_resolution,
                             bfshare_dynamic = TRUE) %>%
    dplyr::select(- total.cbm_per_second, - gw.cbm_per_second) %>%
    dplyr::rename(cbm_per_second = bf.cbm_per_second)


  bfstypes_equations <- config$bfstypes_equations %>%
    bfs_convert_equation()


  ww_bfshare_long <- ww_bfshare %>%
    dplyr::select(- bank_filtration_share) %>%
    tidyr::pivot_longer(cols = c("bank_filtration_share_static", "bank_filtration_share_dynamic"))

  flow_ids <- ww_bfshare_long %>%
    dplyr::count(flow_id, type) %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::arrange(flow_id) %>%
    dplyr::pull(flow_id)


  pdff <- "waterworks_galleries_bfshare_static_vs_dynamic_new-equations-1.pdf"
  kwb.utils::preparePdf(pdfFile = pdff)

  # >>> Set TRUE, wenn du 4 Plots pro Seite moechtest
  combine_plots <- TRUE

  plots <- lapply(flow_ids, function(f_id) {

    ww_bfshare_long_tmp <- ww_bfshare_long %>%
      dplyr::filter(flow_id == f_id) %>%
      dplyr::mutate(
        name = dplyr::if_else(name == "bank_filtration_share_static", "Static", "Dynamic")
      )

    bfs_equation_tmp <- bfstypes_equations[bfstypes_equations$type == unique(ww_bfshare_long_tmp$type), ]

    label <- sprintf(
      "%s\n(Type: %d, %d m\u00B3/d - %d m\u00B3/d)",
      bfs_equation_tmp$equation_text,
      bfs_equation_tmp$type,
      bfs_equation_tmp$Q_min_m3d,
      bfs_equation_tmp$Q_max_m3d
    )

    ggplot2::ggplot(
      ww_bfshare_long_tmp,
      ggplot2::aes(x = date, y = value, colour = name, linetype = name)
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      # Achsenbeschriftungen und Ticks
      ggplot2::scale_x_date(
        name  = "Year",
        date_breaks = "2 years",
        date_labels = "%Y"
      ) +
      ggplot2::scale_y_continuous(
        name   = "Surface Water Fraction",
        breaks = seq(0, 1, 0.2),
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      # Farben & Linientypen fuer Dynamic/Static
      ggplot2::scale_colour_manual(values = c("Dynamic" = "#1f78b4", "Static" = "red")) +
      ggplot2::scale_linetype_manual(values = c("Dynamic" = "solid", "Static" = "dashed")) +
      # Titel + Gleichung oben
      ggplot2::labs(
        title    = f_id,
        subtitle = label,
        colour   = NULL,
        linetype = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 13),
        axis.text  = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11.5, face = "italic"),
        legend.position = "top",
        legend.text     = ggplot2::element_text(size = 12)
      )
  })

  # >>> Falls mehrere Plots pro Seite gewuenscht
  if (combine_plots) {
    n_per_page <- 4
    n_pages <- ceiling(length(plots) / n_per_page)
    for (i in seq_len(n_pages)) {
      gridExtra::grid.arrange(
        grobs = plots[((i - 1) * n_per_page + 1):min(i * n_per_page, length(plots))],
        ncol = 2, nrow = 2
      )
    }
  } else {
    # jeweils einzeln
    for (p in plots) print(p)
  }

  kwb.utils::finishAndShowPdf(pdff)

}
