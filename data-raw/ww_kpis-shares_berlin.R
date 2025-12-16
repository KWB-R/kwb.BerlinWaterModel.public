library(kwb.BerlinWaterModel.public)

# read config files ##########################################################

config <- kwb.BerlinWaterModel.public::config_read(config_dir = "inst/extdata/config/network_complete")
#config <- kwb.BerlinWaterModel.public::config_read(config_dir = "inst/extdata/config/network_complete_mean-start-conc")
#config <- kwb.BerlinWaterModel.public::config_read(config_dir = "inst/extdata/config/network_complete_2019")

config <- kwb.BerlinWaterModel.public::add_rain_direct_and_evaporation(config)

config <- kwb.BerlinWaterModel.public::add_tracers(config)

config <- kwb.BerlinWaterModel.public::add_substances(config)

network <- kwb.BerlinWaterModel.public::prepare_network(config)


#kwb.BerlinWaterModel.public::check_network_errors(network)

# adaptation of inflow concentrations
# Havel
#config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "Borgsdorf"] <- 0.24
config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "Borgsdorf"] <- 0.17 # 2020
# Dahme
#config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "DaW_0.165"] <- 0.38
config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "DaW_0.165"] <- 0.29 # 2020
# Spree
#config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "MgS_8.576"] <- 0.46
#config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "GoK_3.41"] <- 0.46
config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "MgS_8.576"] <- 0.35 # 2020
config$flows_in_out$conc_ValsartansaeureAeq.mg.m3[config$flows_in_out$flow_id == "GoK_3.41"] <- 0.35 # 2020
#

# plot interactive network map ###############################################

### Network graph: simple
#plot_network_simple(network)

### Network graph: complex
#net_complex <- kwb.BerlinWaterModel.public::plot_network_complex(network,
#                                                        config,
#                                                        show_labels = TRUE)

#htmlwidgets::saveWidget(net_complex, file = "water-cycle_complex.html")
#zip(zipfile = "water-cycle_complex.zip", files = "water-cycle_complex.html")


# data preparation ###########################################################

temporal_resolution <- "days" # "hours" or "days"

col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                               "datetime",
                               "date")

use_scenario <- TRUE

inputs <- kwb.BerlinWaterModel.public::add_scenario(
  config = config,
  use_scenario = use_scenario, # should scaling factors be used (in config$scenarios) or not?
  debug = TRUE)


date_min <- "2002-01-01"
date_max <- "2022-12-31"

input_list <- kwb.BerlinWaterModel.public::prepare_input(temporal_resolution = temporal_resolution,
                                                  config = config,
                                                  cso = inputs$cso,
                                                  inflows = inputs$inflows,
                                                  share_wwtp_sch_to_nordgraben_timeseries = TRUE,
                                                  # share_wwtp_sch_panke_1 = 0.1,
                                                  # date_separation_panke_1_2 = "2015-04-15",
                                                  # share_wwtp_sch_panke_2 = 0.9,
                                                  rain = inputs$rain,
                                                  evapo_p = inputs$evapo_p,
                                                  ww = inputs$ww,
                                                  wwtp = inputs$wwtp,
                                                  bfshare_dynamic = FALSE,
                                                  date_min = date_min,
                                                  date_max = date_max)


# calculate flows using functions in outflows_multiple.csv for river branching
system.time(
  flows_dynamic <- kwb.BerlinWaterModel.public::calculate_flows_auto(config = config,
                                                              input_list = input_list,
                                                              network = network,
                                                              use_dynamic = TRUE,
                                                              debug = TRUE)
)


flow_stats <- kwb.BerlinWaterModel.public::calculate_flow_stats(flows = flows_dynamic)

openxlsx::write.xlsx(flow_stats,
                     sprintf("surface-water_kpis_scenario-%s.xlsx",
                             ifelse(use_scenario, "yes", "no")), overwrite = TRUE)

### Szenario Schönerlinde Masterplan Wasser S3.50: 0,5 m³/s -> Panke, Rest -> Nordgraben; keine Ableitung Panke -> Nordgraben (ab 1.3.2019)
if(FALSE) {
  input_list$flows$Q_KW_SCH_Panke <- ifelse(input_list$flows$date > "2019-03-01",
                                            ifelse(input_list$flows$Q_KW_SCH < 0.5, input_list$flows$Q_KW_SCH, 0.5),
                                            input_list$flows$Q_KW_SCH_Panke)
  input_list$flows$Q_KW_SCH_Nordgraben <- ifelse(input_list$flows$date > "2019-03-01",
                                                 input_list$flows$Q_KW_SCH - input_list$flows$Q_KW_SCH_Panke,
                                                 input_list$flows$Q_KW_SCH_Nordgraben)
  input_list$shares_timeseries$share_Panke_to_Nordgraben <- ifelse(input_list$shares_timeseries$date > "2019-03-01", 0,
                                                                   input_list$shares_timeseries$share_Panke_to_Nordgraben)
}

ww_galleries <- input_list$ww_bfshares %>%
  dplyr::mutate(
    waterworks = stringr::str_sub(flow_id, 1, 3),
    gallery    = stringr::str_sub(flow_id, 4, stringr::str_length(flow_id))
  )

# Nested Lookup mit KPIs
ww_galleries_lookup <- ww_galleries %>%
  dplyr::select(waterworks, gallery, date, cbm_per_day) %>%
  dplyr::group_by(waterworks, gallery) %>%
  tidyr::nest(data = c(date, cbm_per_day)) %>%
  dplyr::mutate(
    # gallery-Spalte in jede nested-Data schreiben
    data = purrr::map2(
      data, gallery,
      ~ dplyr::mutate(.x, gallery = .y)
    ),
    # optional auch waterworks in die nested data:
    # data = purrr::map2(
    #   data, waterworks,
    #   ~ dplyr::mutate(.x, waterworks = .y)
    # ),
    n_rows = purrr::map_int(data, nrow),
    kpis   = purrr::map(
      data,
      ~ kwb.BerlinWaterModel.public::compute_gallery_kpis(
        df                = .x,
        gallery_col       = "gallery",
        date_col          = "date",
        q_col             = "cbm_per_day",
        temporal_resolution = "auto",
        date_min = date_min,
        date_max = date_max
      )
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    synthetic_daily_from_month = purrr::map_lgl(kpis, ~ .x$meta$synthetic_daily_from_month),
    temporal_resolution        = purrr::map_chr(kpis, ~ .x$meta$temporal_resolution),
    has_daily_extremes         = purrr::map_lgl(kpis, ~ .x$meta$has_daily_extremes)
  )


kpis <- list(per_gallery = kwb.BerlinWaterModel.public::get_gallery_kpis_per_gallery(ww_galleries_lookup),
             per_year = kwb.BerlinWaterModel.public::get_gallery_kpis_per_year(ww_galleries_lookup),
             per_month = kwb.BerlinWaterModel.public::get_gallery_kpis_per_month(ww_galleries_lookup))

openxlsx::write.xlsx(kpis,
                     sprintf("ww-galleries_kpis_scenario-%s.xlsx",
                             ifelse(use_scenario, "yes", "no")), overwrite = TRUE)



pdff <- sprintf("rohwasserquellen_berlinweit_scenario-%s.pdf", ifelse(use_scenario, "yes", "no"))

kwb.utils::preparePdf(pdfFile = pdff, landscape = TRUE)

input_list <- kwb.BerlinWaterModel.public::prepare_input(temporal_resolution = temporal_resolution,
                                                  config = config,
                                                  cso = inputs$cso,
                                                  inflows = inputs$inflows,
                                                  share_wwtp_sch_to_nordgraben_timeseries = TRUE,
                                                  # share_wwtp_sch_panke_1 = 0.1,
                                                  # date_separation_panke_1_2 = "2015-04-15",
                                                  # share_wwtp_sch_panke_2 = 0.9,
                                                  rain = inputs$rain,
                                                  evapo_p = inputs$evapo_p,
                                                  ww = inputs$ww,
                                                  wwtp = inputs$wwtp,
                                                  bfshare_dynamic = FALSE,
                                                  date_min = "2002-01-01",
                                                  date_max = "2022-12-31")



# --- Plot -----------------------------------------------------------------
legend_text_size <- 8


ww_berlin_raw <- input_list$ww_bfshares %>%
  dplyr::filter(flow_type == "Rohwassergewinnung")

ww_berlin <- ww_berlin_raw %>%
  dplyr::mutate(
    mar.cbm_per_second = dplyr::if_else(is_mar == tolower("yes"),
                                        bf.cbm_per_second, 0),
    bf.cbm_per_second  = dplyr::if_else(is_mar == tolower("yes"),
                                        0, bf.cbm_per_second)
  ) %>%
  dplyr::select(-cbm_per_day) %>%
  tidyr::pivot_longer(
    cols      = tidyselect::all_of(c("bf.cbm_per_second",
                                     "gw.cbm_per_second",
                                     "mar.cbm_per_second")),
    names_to  = "name",
    values_to = "value"
  ) %>%
  #dplyr::select(date, total.cbm_per_second, name, value) %>%
  dplyr::group_by(date, name) %>%
  dplyr::summarise(
    total.cbm_per_second = sum(total.cbm_per_second, na.rm = TRUE),
    value = sum(value, na.rm = TRUE),
    percent = value / total.cbm_per_second
  ) %>%
  # Reihenfolge im Stack: UF (bf) unten, dann GWA, dann GW
  dplyr::mutate(
    name = factor(
      name,
      levels = c("bf.cbm_per_second", "mar.cbm_per_second", "gw.cbm_per_second")
    )
  )

readr::write_csv2(ww_berlin, sprintf("ww-shares_static_scenario-%s.csv",
                                     ifelse(use_scenario, "yes", "no"))
)


# Schöne Legenden-Labels
labels_sources <- c(
  "bf.cbm_per_second"  = "Uferfiltrat (UF)",
  "mar.cbm_per_second" = "Grundwasseranreicherung (GWA)",
  "gw.cbm_per_second"  = "landseitiges Grundwasser"
)

ww_berlin_stats <- ww_berlin %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(value_mean = mean(value),
                   value_sd = sd(value),
                   percent_mean = round(sum((value * percent) / sum(value)), 3)) %>%
  dplyr::left_join(tibble::tibble(name = names(labels_sources),
                                  name_label = as.character(labels_sources))) %>%
  dplyr::mutate(label = sprintf("%s (%2.2f m\u00B3/s \u00B1 %1.2f m\u00B3/s; %2.1f %%)",
                                name_label,
                                value_mean,
                                value_sd,
                                100 * percent_mean))



labels_sources <- ww_berlin_stats$label
names(labels_sources) <- ww_berlin_stats$name


cols_sources <- c(
  "bf.cbm_per_second"  = "#00bfc4",  # UF Uferfiltrat
  "mar.cbm_per_second" = "#0000ff",  # GWA
  "gw.cbm_per_second"  = "#102c54"   # GW Grundwasser
)


gg1 <- ww_berlin %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = percent, fill = name)) +
  ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                    colour   = NA) +
  ggplot2::scale_x_date(
    # Haupt-Ticks & Labels: jährlich
    breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                 max(ww_berlin$date, na.rm = TRUE),
                 by = "1 year"),
    date_labels = "%Y",
    # optionale Neben-Ticks: monatlich (ohne Beschriftung)
    minor_breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                       max(ww_berlin$date, na.rm = TRUE),
                       by = "1 month")
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent) +
  ggplot2::scale_fill_manual(
    name   = "",
    values = cols_sources,
    breaks = c("gw.cbm_per_second", "mar.cbm_per_second", "bf.cbm_per_second"),
    labels = labels_sources,
    guide  = ggplot2::guide_legend(reverse = FALSE)   # Legende gleiche Reihenfolge wie Stack
  ) +
  ggplot2::labs(title = "Statische UF/GWA-Anteile",
                caption = sprintf("Grundwasseranreicherung nur durch stark beeinflusste Brunnengallerien abgebildet (%s). Nicht durch Obefl\u00E4chenwasserentnahmen!",
                                   paste0(unique(ww_berlin_raw$flow_id[ww_berlin_raw$is_mar == tolower("yes")]), collapse = ", ")
                                   ),
                x = "Jahr",
                y = "Prozentualer Anteil (%)"
                ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top",
                 legend.title    = ggplot2::element_text(size = 10),
                 legend.text     = ggplot2::element_text(size = legend_text_size),
                 axis.title.x    = ggplot2::element_text(size = 14),
                 axis.title.y    = ggplot2::element_text(size = 14),
                 axis.text.x     = ggplot2::element_text(size = 12),
                 axis.text.y     = ggplot2::element_text(size = 12))
gg1


gg2 <- ww_berlin %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = value, fill = name)) +
  ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                    colour   = NA) +
  ggplot2::scale_x_date(
    # Haupt-Ticks & Labels: jährlich
    breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                 max(ww_berlin$date, na.rm = TRUE),
                 by = "1 year"),
    date_labels = "%Y",
    # optionale Neben-Ticks: monatlich (ohne Beschriftung)
    minor_breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                       max(ww_berlin$date, na.rm = TRUE),
                       by = "1 month")
  ) +
  ggplot2::scale_fill_manual(
    name   = "",
    values = cols_sources,
    breaks = c("gw.cbm_per_second", "mar.cbm_per_second", "bf.cbm_per_second"),
    labels = labels_sources,
    guide  = ggplot2::guide_legend(reverse = FALSE)   # Legende gleiche Reihenfolge wie Stack
  ) +
  ggplot2::labs(title = "Statische UF/GWA-Anteile",
                caption = sprintf("Grundwasseranreicherung nur durch stark beeinflusste Brunnengallerien abgebildet (%s). Nicht durch Obefl\u00E4chenwasserentnahmen!",
                                  paste0(unique(ww_berlin_raw$flow_id[ww_berlin_raw$is_mar == tolower("yes")]), collapse = ", ")
                ),
                x = "Jahr",
                y = "F\u00F6rdermenge (m\u00B3/s)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top",
                 legend.title    = ggplot2::element_text(size = 10),
                 legend.text     = ggplot2::element_text(size = legend_text_size),
                 axis.title.x    = ggplot2::element_text(size = 14),
                 axis.title.y    = ggplot2::element_text(size = 14),
                 axis.text.x     = ggplot2::element_text(size = 12),
                 axis.text.y     = ggplot2::element_text(size = 12))

gg2


input_list <- kwb.BerlinWaterModel.public::prepare_input(temporal_resolution = temporal_resolution,
                                                  config = config,
                                                  cso = inputs$cso,
                                                  inflows = inputs$inflows,
                                                  share_wwtp_sch_to_nordgraben_timeseries = TRUE,
                                                  # share_wwtp_sch_panke_1 = 0.1,
                                                  # date_separation_panke_1_2 = "2015-04-15",
                                                  # share_wwtp_sch_panke_2 = 0.9,
                                                  rain = inputs$rain,
                                                  evapo_p = inputs$evapo_p,
                                                  ww = inputs$ww,
                                                  wwtp = inputs$wwtp,
                                                  bfshare_dynamic = TRUE,
                                                  date_min = "2002-01-01",
                                                  date_max = "2022-12-31")



# --- Plot -----------------------------------------------------------------

ww_berlin_raw <- input_list$ww_bfshares %>%
  dplyr::filter(flow_type == "Rohwassergewinnung")

ww_berlin <- ww_berlin_raw %>%
  dplyr::mutate(
    mar.cbm_per_second = dplyr::if_else(is_mar == tolower("yes"),
                                        bf.cbm_per_second, 0),
    bf.cbm_per_second  = dplyr::if_else(is_mar == tolower("yes"),
                                        0, bf.cbm_per_second)
  ) %>%
  dplyr::select(-cbm_per_day) %>%
  tidyr::pivot_longer(
    cols      = tidyselect::all_of(c("bf.cbm_per_second",
                                     "gw.cbm_per_second",
                                     "mar.cbm_per_second")),
    names_to  = "name",
    values_to = "value"
  ) %>%
  #dplyr::select(date, total.cbm_per_second, name, value) %>%
  dplyr::group_by(date, name) %>%
  dplyr::summarise(
    total.cbm_per_second = sum(total.cbm_per_second, na.rm = TRUE),
    value = sum(value, na.rm = TRUE),
    percent = value / total.cbm_per_second
  ) %>%
  # Reihenfolge im Stack: UF (bf) unten, dann GWA, dann GW
  dplyr::mutate(
    name = factor(
      name,
      levels = c("bf.cbm_per_second", "mar.cbm_per_second", "gw.cbm_per_second")
    )
  )

readr::write_csv2(ww_berlin, sprintf("ww-shares_dynamic_scenario-%s.csv",
                                     ifelse(use_scenario, "yes", "no"))
)


# Schöne Legenden-Labels
labels_sources <- c(
  "bf.cbm_per_second"  = "Uferfiltrat (UF)",
  "mar.cbm_per_second" = "Grundwasseranreicherung (GWA)",
  "gw.cbm_per_second"  = "landseitiges Grundwasser"
)

ww_berlin_stats <- ww_berlin %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(value_mean = mean(value),
                   value_sd = sd(value),
                   percent_mean = round(sum((value * percent) / sum(value)), 3)) %>%
  dplyr::left_join(tibble::tibble(name = names(labels_sources),
                                  name_label = as.character(labels_sources))) %>%
  dplyr::mutate(label = sprintf("%s (%2.2f m\u00B3/s \u00B1 %1.2f m\u00B3/s; %2.1f %%)",
                                name_label,
                                value_mean,
                                value_sd,
                                100 * percent_mean))


labels_sources <- ww_berlin_stats$label
names(labels_sources) <- ww_berlin_stats$name


cols_sources <- c(
  "bf.cbm_per_second"  = "#00bfc4",  # UF Uferfiltrat
  "mar.cbm_per_second" = "#0000ff",  # GWA
  "gw.cbm_per_second"  = "#102c54"   # GW Grundwasser
)


gg1 <- ww_berlin %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = percent, fill = name)) +
  ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                    colour   = NA) +
  ggplot2::scale_x_date(
    # Haupt-Ticks & Labels: jährlich
    breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                 max(ww_berlin$date, na.rm = TRUE),
                 by = "1 year"),
    date_labels = "%Y",
    # optionale Neben-Ticks: monatlich (ohne Beschriftung)
    minor_breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                       max(ww_berlin$date, na.rm = TRUE),
                       by = "1 month")
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent) +
  ggplot2::scale_fill_manual(
    name   = "",
    values = cols_sources,
    breaks = c("gw.cbm_per_second", "mar.cbm_per_second", "bf.cbm_per_second"),
    labels = labels_sources,
    guide  = ggplot2::guide_legend(reverse = FALSE)   # Legende gleiche Reihenfolge wie Stack
  ) +
  ggplot2::labs(title = "Dynamische UF/GWA-Anteile",
                caption = sprintf("Grundwasseranreicherung nur durch stark beeinflusste Brunnengallerien abgebildet (%s). Nicht durch Obefl\u00E4chenwasserentnahmen!",
                                  paste0(unique(ww_berlin_raw$flow_id[ww_berlin_raw$is_mar == tolower("yes")]), collapse = ", ")
                ),
                x = "Jahr",
                y = "Prozentualer Anteil (%)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top",
                 legend.title    = ggplot2::element_text(size = 10),
                 legend.text     = ggplot2::element_text(size = legend_text_size),
                 axis.title.x    = ggplot2::element_text(size = 14),
                 axis.title.y    = ggplot2::element_text(size = 14),
                 axis.text.x     = ggplot2::element_text(size = 12),
                 axis.text.y     = ggplot2::element_text(size = 12)
  )

gg1


gg2 <- ww_berlin %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = value, fill = name)) +
  ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                    colour   = NA) +
  ggplot2::scale_x_date(
    # Haupt-Ticks & Labels: jährlich
    breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                 max(ww_berlin$date, na.rm = TRUE),
                 by = "1 year"),
    date_labels = "%Y",
    # optionale Neben-Ticks: monatlich (ohne Beschriftung)
    minor_breaks = seq(min(ww_berlin$date, na.rm = TRUE),
                       max(ww_berlin$date, na.rm = TRUE),
                       by = "1 month")
  ) +
  ggplot2::scale_fill_manual(
    name   = "",
    values = cols_sources,
    breaks = c("gw.cbm_per_second", "mar.cbm_per_second", "bf.cbm_per_second"),
    labels = labels_sources,
    guide  = ggplot2::guide_legend(reverse = FALSE)   # Legende gleiche Reihenfolge wie Stack
  ) +
  ggplot2::labs(title = "Dynamische UF/GWA-Anteile",
                caption = sprintf("Grundwasseranreicherung nur durch stark beeinflusste Brunnengallerien abgebildet (%s). Nicht durch Obefl\u00E4chenwasserentnahmen!",
                                  paste0(unique(ww_berlin_raw$flow_id[ww_berlin_raw$is_mar == tolower("yes")]), collapse = ", ")
                ),
                x = "Jahr",
                y = "F\u00F6rdermenge (m\u00B3/s)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top",
                 legend.title    = ggplot2::element_text(size = 10),
                 legend.text     = ggplot2::element_text(size = legend_text_size),
                 axis.title.x    = ggplot2::element_text(size = 14),
                 axis.title.y    = ggplot2::element_text(size = 14),
                 axis.text.x     = ggplot2::element_text(size = 12),
                 axis.text.y     = ggplot2::element_text(size = 12)
  )

gg2


kwb.utils::finishAndShowPdf(PDF = pdff)
