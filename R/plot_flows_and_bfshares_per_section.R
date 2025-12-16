#' Plot Flows and BF & MAR shares per section
#'
#' @param config list with config as imported with \code{\link{config_read}}
#' @param flows flows as retrieved by xxxx
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#' @param ww waterworks dataset (default: kwb.BerlinWaterModel::ww)
#' @param scale_factor scale factor for increasing size of labels
#' @param debug print debug messages (default: TRUE)
#' @param add_caption_simulation add caption with simulation information (default: FALSE)
#' @param add_caption_weblink Add one line in caption to reference webpage of vignette (default: FALSE)
#' @param add_caption_outflowsmultiple Add information of model configuration for
#' multiple outflows (default: FALSE)
#' @returns plots flows and BF & MAR shares for each section
#' @export
#' @importFrom ggplot2 position_dodge2 geom_jitter geom_boxplot theme theme_bw
#' guides guide_legend labs geom_text scale_y_log10
#' @importFrom matrixStats weightedMedian
plot_flows_and_bfshares_per_section <- function(config,
                                                flows,
                                                network,
                                                ww,
                                                scale_factor = 1,
                                                debug = TRUE,
                                                add_caption_simulation = FALSE,
                                                add_caption_weblink = FALSE,
                                                add_caption_outflowsmultiple = FALSE) {


  use_dynamic <- attr(flows, "use_dynamic")
  bfshare_dynamic <- attr(flows, "bfshare_dynamic")
  temporal_resolution <- attr(flows, "temporal_resolution")
  col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                                 "datetime",
                                 "date")

  section_idnames <- get_section_idnames(config)


  links_orderid <- get_flowpath_table(outflow_id = "Out", network, config) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(source_id = get_ids_from_names(source, config),
                  section_id = get_ids_from_names(order_name, config),
                  target_id = get_ids_from_names(target, config)) %>%
    shorten_ww_flow_id(col_flow_id = "target_id")


  id_links_orderid <- links_orderid %>%
    dplyr::count(order_id, order_name) %>%
    dplyr::arrange(dplyr::desc(order_id)) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::relocate("id", .before = "order_id") %>%
    dplyr::select(-n)

  links_orderid_id <- id_links_orderid %>%
    dplyr::left_join(links_orderid, by = c("order_id", "order_name"))

  ww_bfshare <- get_bfshares(config = config,
                             ww = ww,
                             temporal_resolution = temporal_resolution,
                             bfshare_dynamic = bfshare_dynamic) %>%
    dplyr::select(- total.cbm_per_second, - gw.cbm_per_second) %>%
    dplyr::rename(cbm_per_second = bf.cbm_per_second)

  ww_bfshare_meta <- ww_bfshare %>%
    dplyr::group_by(flow_id, waterworks, gallery, type) %>%
    dplyr::mutate(median_bank_filtration_share = dplyr::if_else(cbm_per_second > 0,
                                                                bank_filtration_share,
                                                                NA)) %>%
    dplyr::summarise(median_cbm_per_second = median(cbm_per_second, na.rm = TRUE),
                     median_bank_filtration_share = median(median_bank_filtration_share, na.rm = TRUE),
                     .groups = "drop")


  lapply(unique(links_orderid_id$id), function(id) {

    link_orderid_id <- links_orderid_id[links_orderid_id$id == id,]

    section_name <- unique(link_orderid_id$order_name)
    section_id <- unique(link_orderid_id$section_id)


    kwb.utils::catAndRun(sprintf("Running for id %d/%s (%s: %s)",
                                 id,
                                 length(unique(links_orderid_id$id)),
                         section_id,
                         section_name),
                         expr = {

    target_ids <- unique(link_orderid_id$target_id)

    target_ids_ww <- target_ids[stringr::str_starts(target_ids, pattern = "See", negate = TRUE) & stringr::str_starts(target_ids, pattern = stringr::regex("^(H[0-9]|S[0-9]|Out)"), negate = TRUE)]

    if(length(target_ids_ww) > 0) {

      flows_bfshares <- if(length(target_ids_ww) > 0) {
        target_ids_ww_exist <- target_ids_ww %in% unique(ww_bfshare$flow_id)
        if(all(target_ids_ww_exist)) {
          flows[, c(col_date_or_datetime, section_id)] %>%
            dplyr::left_join(ww_bfshare %>%
                               dplyr::filter(flow_id %in% target_ids_ww) %>%
                               dplyr::select(- tidyselect::all_of(c("waterworks",
                                                                    "gallery",
                                                                    "type",
                                                                    "cbm_per_day",
                                                                    "is_mar")),
                                             - tidyselect::starts_with("bank_filtration_share"),
                                             - tidyselect::all_of("flow_type")) %>%
            dplyr::mutate(cbm_per_second = abs(cbm_per_second)) %>%
            tidyr::pivot_wider(names_from = "flow_id", values_from = "cbm_per_second") %>%
            fill_timeseries(col_datetime = col_date_or_datetime,
                            temporal_resolution = temporal_resolution,
                            direction = "up"),
            by = col_date_or_datetime)
        } else {
          stop(sprintf("The following flow_ids (%s) to waterworks do not exist for 'section %s (%s)'",
                       paste0(target_ids_ww[!target_ids_ww_exist],collapse = ", "),
                       section_name,
                       section_id),
          )
        }
      }

      flow_ids_bf <- config$flows_in_out %>%
        dplyr::filter(stringr::str_starts(flow_id, "WW_")) %>%
        shorten_ww_flow_id() %>%
        dplyr::filter(is.na(is_mar) | is_mar == "no",
                      flow_id %in%  names(flows_bfshares),
                      stringr::str_ends(flow_id, "gwa", negate = TRUE)) %>%
        dplyr::pull(flow_id)


      ### Fix surface water flows (if MAR or well gallery is abstraction from section)
      if(length(flow_ids_bf > 0)) {
        flows_bfshares[[section_id]] <- flows_bfshares[[section_id]] + abs(rowSums(flows_bfshares[, flow_ids_bf], na.rm = TRUE))
        flows[[section_id]] <- flows[[section_id]] + abs(rowSums(flows_bfshares[, flow_ids_bf], na.rm = TRUE))
      }



      bfshares_percent <- if(length(target_ids_ww) > 0) {
        target_ids_ww_exist <- target_ids_ww %in% unique(ww_bfshare$flow_id)
        if(all(target_ids_ww_exist)) {
          flows[, c(col_date_or_datetime, section_id)] %>%
            dplyr::left_join(ww_bfshare %>%
                               dplyr::filter(flow_id %in% target_ids_ww) %>%
                               dplyr::select(- tidyselect::all_of(c("waterworks",
                                                                    "gallery",
                                                                    "type",
                                                                    "cbm_per_day",
                                                                    "cbm_per_second",
                                                                    "is_mar")),
                                             - tidyselect::starts_with("bank_filtration_share_"),
                                             - tidyselect::all_of("flow_type")) %>%
                               tidyr::pivot_wider(names_from = "flow_id", values_from = "bank_filtration_share") %>%
                               fill_timeseries(col_datetime = col_date_or_datetime,
                                               temporal_resolution = temporal_resolution,
                                               direction = "up"),
                             by = col_date_or_datetime) %>%
            dplyr::select(- tidyselect::all_of(section_id)) %>%
            tidyr::pivot_longer(names_to = "flow_id",
                                values_to = "bank_filtration_share",
                                - col_date_or_datetime) %>%
            dplyr::mutate(bank_filtration_share = bank_filtration_share * 100)
        } else {
          stop(sprintf("The following flow_ids (%s) to waterworks do not exist for 'section %s (%s)'",
                       paste0(target_ids_ww[!target_ids_ww_exist],collapse = ", "),
                       section_name,
                       section_id),
          )
        }
      }

      bfshares_q <- if(length(target_ids_ww) > 0) {
        target_ids_ww_exist <- target_ids_ww %in% unique(ww_bfshare$flow_id)
        if(all(target_ids_ww_exist)) {
          flows[, c(col_date_or_datetime, section_id)] %>%
            dplyr::left_join(ww_bfshare %>%
                               dplyr::filter(flow_id %in% target_ids_ww) %>%
                               dplyr::select(- tidyselect::all_of(c("waterworks",
                                                                    "gallery",
                                                                    "type",
                                                                    "cbm_per_day",
                                                                    "is_mar")),
                                             - tidyselect::starts_with("bank_filtration_share"),
                                             - tidyselect::all_of("flow_type")) %>%
                               tidyr::pivot_wider(names_from = "flow_id", values_from = "cbm_per_second") %>%
                               fill_timeseries(col_datetime = col_date_or_datetime,
                                               temporal_resolution = temporal_resolution,
                                               direction = "up"),
                             by = col_date_or_datetime) %>%
            dplyr::select(- tidyselect::all_of(section_id)) %>%
            tidyr::pivot_longer(names_to = "flow_id",
                                values_to = "cbm_per_second",
                                - col_date_or_datetime)
        } else {
          stop(sprintf("The following flow_ids (%s) to waterworks do not exist for 'section %s (%s)'",
                       paste0(target_ids_ww[!target_ids_ww_exist],collapse = ", "),
                       section_name,
                       section_id),
          )
        }
      }

      bfshares_percent_q <- bfshares_percent %>%
        dplyr::left_join(bfshares_q,
                         by = c(col_date_or_datetime, "flow_id")) %>%
        dplyr::filter(stringr::str_ends(flow_id, "gwa") |  flow_id %in% flow_ids_bf)  %>%
        dplyr::mutate(flow_type = dplyr::if_else(flow_id %in% flow_ids_bf,
                                                 "bank filtrate",
                                                 "managed aquifer recharge"))

      bfshares_percent_q_bf <- bfshares_percent %>%
        dplyr::left_join(bfshares_q,
                         by = c(col_date_or_datetime, "flow_id")) %>%
        dplyr::filter(flow_id %in% flow_ids_bf)  %>%
        dplyr::mutate(flow_type = dplyr::if_else(flow_id %in% flow_ids_bf,
                                                 "bank filtrate",
                                                 "managed aquifer recharge"))

      bfshares_percent_q_mar <- bfshares_percent %>%
        dplyr::left_join(bfshares_q,
                         by = c(col_date_or_datetime, "flow_id")) %>%
        dplyr::filter(!flow_id %in% flow_ids_bf & stringr::str_ends(flow_id, "gwa", negate = TRUE))  %>%
        dplyr::mutate(flow_type = dplyr::if_else(flow_id %in% flow_ids_bf,
                                                 "bank filtrate",
                                                 "managed aquifer recharge"))


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



bfstypes_equations <- config$bfstypes_equations %>%
  bfs_convert_equation() %>%
  dplyr::select("type", "equation_function",  "equation_text")

stats_bf <- bfshares_percent_q_bf %>%
  dplyr::filter(cbm_per_second > 0) %>%
  dplyr::group_by(flow_id, flow_type) %>%
  dplyr::summarise(cbm_per_second.median = median(cbm_per_second, na.rm = TRUE),
                   cbm_per_second.sd = sd(cbm_per_second, na.rm = TRUE),
                   bank_filtration_share.median = median(bank_filtration_share, na.rm = TRUE),
                   bank_filtration_share.sd = sd(bank_filtration_share, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(bfstypes_wellgalleries, "flow_id") %>%
  dplyr::left_join(bfstypes_equations,
                   by = "type") %>%
  dplyr::mutate(type_equation = ifelse(!is.na(type) & bfshare_dynamic,
                                       sprintf(", type: %d, %s", type, equation_text),
                                       ""),
                type = ifelse(!is.na(type) & bfshare_dynamic,
                              sprintf(", type: %d", type),
                              ""),
                label = sprintf("%s (%s '%s': %3.4f \u00B1 %3.4f m\u00B3/s, %3.1f \u00B1 %3.1f %%%s%s)",
                                flow_id,
                                flow_type,
                                dplyr::if_else(type_equation == "",
                                       "static",
                                       "dynamic"),
                                cbm_per_second.median,
                                cbm_per_second.sd,
                                bank_filtration_share.median,
                                bank_filtration_share.sd,
                                type,
                                ifelse((is.na(equation_text) & bfshare_dynamic) | bfshare_dynamic == FALSE,
                                       "",
                                       sprintf(", %s", equation_text))
                )
  )




      stats_mar <- bfshares_percent_q_mar %>%
        dplyr::filter(cbm_per_second > 0) %>%
        dplyr::group_by(flow_id, flow_type) %>%
        dplyr::summarise(cbm_per_second.median = median(cbm_per_second, na.rm = TRUE),
                         cbm_per_second.sd = sd(cbm_per_second, na.rm = TRUE),
                         bank_filtration_share.median = median(bank_filtration_share, na.rm = TRUE),
                         bank_filtration_share.sd = sd(bank_filtration_share, na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::left_join(bfstypes_wellgalleries, by = "flow_id") %>%
        dplyr::left_join(bfstypes_equations,
                         by = "type") %>%
        dplyr::mutate(type_equation = ifelse(bfshare_dynamic,
                                             sprintf(", type: %d, %s", type, equation_text),
                                                     ""),
                      type = ifelse(!is.na(type) & bfshare_dynamic,
                                    sprintf(", type: %d", type),
                                    ""),
                      label = sprintf("%s (%s '%s': %3.4f \u00B1 %3.4f m\u00B3/s, %3.1f \u00B1 %3.1f %%%s%s)",
                                      flow_id,
                                      flow_type,
                                      dplyr::if_else(type_equation == "",
                                                     "static",
                                                     "dynamic"),
                                      cbm_per_second.median,
                                      cbm_per_second.sd,
                                      bank_filtration_share.median,
                                      bank_filtration_share.sd,
                                      type,
                                      ifelse((is.na(equation_text) & bfshare_dynamic) | bfshare_dynamic == FALSE,
                                             "",
                                             sprintf(", %s", equation_text))
                                      )
                         )


stats_bf_mar_caption <- if(nrow(stats_bf) > 0 & nrow(stats_mar) == 0) {
  paste0(stats_bf$label, collapse = "\n")
} else if (nrow(stats_bf) == 0 & nrow(stats_mar) > 0) {
  paste0(stats_mar$label, collapse = "\n")
} else if (nrow(stats_bf) > 0 & nrow(stats_mar) > 0) {
  paste0(c(stats_bf$label, stats_mar$label), collapse = "\n")
} else {
  ""
}



      label_bf_mar <- bfshares_percent_q %>%
        dplyr::count(flow_type, flow_id) %>%
        dplyr::group_by(flow_type) %>%
        dplyr::summarise(label = paste0(flow_id, collapse = "|"))


      labels_groups <- bfshares_percent_q %>%
        dplyr::mutate(bank_filtration_share.median = bank_filtration_share*cbm_per_second) %>%
        dplyr::left_join(label_bf_mar, by = "flow_type") %>%
        dplyr::group_by(!!dplyr::sym(col_date_or_datetime), flow_type, label) %>%
        dplyr::summarise(
          # Gewichteter Median
          bank_filtration_share = matrixStats::weightedMedian(
            x = bank_filtration_share,
            w = cbm_per_second,
            na.rm = TRUE),
          cbm_per_second = sum(cbm_per_second, na.rm = TRUE)
          ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(flow_type, label) %>%
        dplyr::summarise(
          cbm_per_second.median = median(cbm_per_second, na.rm = TRUE),
          cbm_per_second.sd     = sd(cbm_per_second, na.rm = TRUE),

          # Gewichteter Mittelwert
          bank_filtration_share.mean = sum(bank_filtration_share * cbm_per_second, na.rm = TRUE) /
            sum(cbm_per_second, na.rm = TRUE),

          # Gewichtete Standardabweichung
          bank_filtration_share.sd = {
            w <- cbm_per_second
            x <- bank_filtration_share
            wm <- sum(w * x, na.rm = TRUE) / sum(w, na.rm = TRUE)
            sqrt(sum(w * (x - wm)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
          },

          # Gewichteter Median
          bank_filtration_share.w.median = matrixStats::weightedMedian(
            x = bank_filtration_share,
            w = cbm_per_second,
            na.rm = TRUE
          ),

          .groups = "drop"
        ) %>%
        dplyr::mutate(group_label = sprintf("%s%s ('%s': %3.4f \u00B1 %3.4f m\u00B3/s, %3.1f \u00B1 %3.1f %%)",
                                            flow_type,
                                            ifelse(flow_type == "bank filtrate" & bfshare_dynamic & !all(is.na(stats_bf$equation_text)),
                                                   " 'dynamic'",
                                                   ifelse(flow_type == "bank filtrate" & (!bfshare_dynamic | all(is.na(stats_bf$equation_text))),
                                                          " 'static'",
                                                          "")),
                                            label,
                                            cbm_per_second.median,
                                            cbm_per_second.sd,
                                            bank_filtration_share.w.median,
                                            bank_filtration_share.sd)) %>%
        dplyr::mutate(group_label = paste0(group_label, collapse = " &\n"))



      group_labels <- tibble::tibble(flow_type = "surface water",
                                     group_label = sprintf("surface water (%3.4f \u00B1 %3.4f m\u00B3/s)",
                                     median(flows_bfshares[[section_id]]),
                                     sd(flows_bfshares[[section_id]])))


      if(nrow(labels_groups) > 0) {

      group_labels <- group_labels %>%
        dplyr::bind_rows(labels_groups[,c("flow_type", "group_label")])
      }


      tmp <- flows_bfshares %>%
        tidyr::pivot_longer(names_to = "flow_id",
                            values_to = "cbm_per_second",
                            - tidyselect::all_of(col_date_or_datetime)) %>%
        dplyr::left_join(config$flows_in_out[,c("flow_id", "is_mar")] %>%
                           shorten_ww_flow_id(), by = "flow_id") %>%
        dplyr::filter(is.na(is_mar) | tolower(is_mar) == "no") %>%
        dplyr::mutate(id = id,
                      flow_type = dplyr::if_else(flow_id %in% section_idnames$id,
                                                 "surface water",
                                                 if(length(target_ids_ww) > 0) {
                                                   dplyr::if_else(stringr::str_ends(flow_id, pattern = "gwa"),
                                                                  "managed aquifer recharge",
                                                                  "bank filtrate")
                                                 } else {
                                                   NA_character_}
                      )) %>%
        dplyr::left_join(group_labels, by = "flow_type") %>%
        dplyr::relocate(id, .before = date)



      tmp_long <- tmp %>%
        dplyr::filter(cbm_per_second > 0) %>%
        dplyr::group_by(!!dplyr::sym(col_date_or_datetime), id, group_label) %>%
        dplyr::summarise(cbm_per_second = sum(cbm_per_second, na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::mutate(month = format(!!dplyr::sym(col_date_or_datetime), format = "%m") %>% as.factor())


      label_spacing <- -0.25  # sinnvoller fixer Abstand


      caption_simulation <- sprintf("Flows simulated with an '%s' temporal resolution for time period (%s - %s) with R package kwb.BerlinWaterModelBerlin (https://github.com/kwb-r/kwb.BerlinWaterModel/dev/).",
                                    dplyr::if_else(temporal_resolution == "days", "daily", "hourly"),
                                    min(tmp$date),
                                    max(tmp$date))
      caption_outflowsmultiple <- sprintf("\nMultiple outflows in in river network are simulated by %s.",
                                          dplyr::if_else(use_dynamic,
                                                         "dynamic splitting factors, i.e. functions dependent on inflows (defined in column 'section_out_function' of configuration file 'outflows_multiple.csv')",
                                                         "constant splitting factors (defined in column 'section_out_share' of configuration file 'outflows_multiple.csv')"
                                                         )
                                          )


      medians_df <- tmp_long %>%
        dplyr::group_by(month, group_label) %>%
        dplyr::summarise(median_flow = median(cbm_per_second), .groups = "drop") %>%
        dplyr::mutate(
          x_numeric = as.numeric(factor(month)),
          flow_offset = as.numeric(factor(group_label)),
          label_x = x_numeric + (flow_offset - mean(unique(flow_offset))) * 0.25 + label_spacing
        )

      tmp_long %>%
        ggplot2::ggplot(ggplot2::aes(x = month, y = cbm_per_second, col = group_label, group = group_label)) +
        # Jitter mit leichtem Versatz
        ggplot2::geom_jitter(
          position = ggplot2::position_dodge2(width = 0.5),
          alpha = 0.1
        ) +
        ggplot2::scale_y_log10(
          limits = c(0.01, 100),
          breaks = c(0.01, 0.1, 1, 10, 100),
          minor_breaks = log10_minor_breaks(),  # Automatische Zwischenwerte
          labels = function(x) format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
        ) +
        # Boxplots mit Versatz pro Monat und flow_type_label
        ggplot2::geom_boxplot(
          ggplot2::aes(group = interaction(month, group_label)),
          position = ggplot2::position_dodge2(width = 0.75),
          alpha = 0.5,
          outlier.shape = NA, # Ausreißer nicht doppelt anzeigen
          width = 0.6
        ) +
        # Median-Label hinzufügen
        ggplot2::geom_text(
          data = medians_df,
          ggplot2::aes(x = label_x, y = median_flow,
                       label = label_signif_clean(median_flow),
                       col = group_label),
          hjust = 1,
          vjust = 0.5,
          fontface = "bold",
          size = 3,
          show.legend = FALSE
        ) +
        ggplot2::labs(x = "Month",
                      y = "Flow (m\u00B3/s)",
                      title = sprintf("Surface water section: %s (%s)",
                                      section_name,
                                      section_id),
                      #subtitle =  subtitle,
                      caption = sprintf("%s%s%s%s%s\n%s",
                                        dplyr::if_else(add_caption_simulation, caption_simulation, ""),
                                        dplyr::if_else(add_caption_weblink, "\nThe workflow for this plot is reproducible and available at: https://kwb-r.github.io/kwb.BerlinWaterModel/articles/abstractions_per_section.html", ""),
                                        dplyr::if_else(add_caption_outflowsmultiple, caption_outflowsmultiple, ""),
                                        dplyr::if_else(bfshare_dynamic & any(!is.na(stats_bf$equation_text)), "\nDynamic bank filtration shares are based on well gallery abstraction rates (and function parameters 'equation_a|b' of configuration file 'bfstypes_equations.csv')", ""),
                                        dplyr::if_else((bfshare_dynamic & any(is.na(stats_bf$equation_text))) | !bfshare_dynamic, "\nStatic bank filtration shares (constant values defined in column 'bank_filtration_share' of configuration file 'flows_in_out.csv')", ""),
                                        stats_bf_mar_caption
                                        )
) +
        ggplot2::guides(col = ggplot2::guide_legend(title = NULL, ncol = 1)) +
        ggplot2::annotation_logticks(sides = "l", scaled = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "top",
          axis.ticks = ggplot2::element_line(linewidth = 0.3 * scale_factor, color = "black"),
          axis.ticks.length = grid::unit(4 * scale_factor, "pt"),
          axis.title.x = ggplot2::element_text(size = 18 * scale_factor, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 18 * scale_factor, face = "bold"),
          axis.text.x = ggplot2::element_text(size = 16 * scale_factor),
          axis.text.y = ggplot2::element_text(size = 16 * scale_factor),
          legend.text = ggplot2::element_text(size = 12 * scale_factor),
          plot.title = ggplot2::element_text(size = 20 * scale_factor, face = "bold"),
          #plot.margin = grid::unit(c(1, 1, 2, 3) * scale_factor, "lines"),
          plot.caption = ggplot2::element_text(size = 9 * scale_factor),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
    } else {
      message(sprintf("No outflow to well gallery or managed aquifer recharge for section %s (%s)",
                      section_name,
                      section_id))
    }

  },
  dbg = debug
  )



  })

}
