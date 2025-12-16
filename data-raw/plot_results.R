#####################################################################################################
# Plot water qualities ##############################################################################
#####################################################################################################

# --- Defaults (Okabe-Ito, farbfehlsicher) ----------------------------------------------------------
tracer_levels <- c("tracer.cso",
                   "tracer.inlet",
                   "tracer.rain_direct",
                   "tracer.rain_runoff",
                   "tracer.ww_discharge",
                   "tracer.wwtp",
                   "tracer_sum")

tracer_labels <- c(
  "tracer.cso"         = "Mischwasser\u00FCberl\u00E4ufe",
  "tracer.inlet"       = "Zufluss",
  "tracer.rain_direct" = "Regen: direkt",
  "tracer.rain_runoff" = "Regen: Oberflächenabfluss",
  "tracer.ww_discharge"= "Wasserwerkseinleitung",
  "tracer.wwtp"        = "Kl\u00E4rwerk",
  "tracer_sum"         = "Summe"
)

tracer_colors <- c(
  "tracer.cso"         = "#FF0000", # Rot
  "tracer.inlet"       = "#009E73", # Blaugrün
  "tracer.rain_direct" = "#56B4E9", # Hellblau
  "tracer.rain_runoff" = "#106b93", # Blau
  "tracer.ww_discharge"= "#F0E442", # Gelb
  "tracer.wwtp"        = "#9c6b30", # Braun
  "tracer_sum"         = "#F48FB1"  # Pink
)


# prepare data for plotting
qualities <- qualities_00_dynamic_reverse

# aggregate results to monthly averages
qualities <- kwb.BerlinWaterModel.public::aggregate_qualities_monthly(qualities_00_dynamic_reverse)

dat <- qualities$conc %>%
  dplyr::bind_rows(.id = "section_id") %>%
  dplyr::mutate(tracer_sum = tracer.cso + tracer.inlet + tracer.rain_runoff + tracer.rain_direct + tracer.ww_discharge + tracer.wwtp) %>%
  tidyr::pivot_longer(- tidyselect::all_of(c("section_id", col_date_or_datetime)),
                      names_to = "substance_unit") %>%
  dplyr::left_join(get_section_idnames(config), by = c("section_id" = "id")) %>%
  dplyr::rename(section_name = name) %>%
  dplyr::relocate(section_name, .after = "section_id")  %>%
  dplyr::filter(stringr::str_starts(substance_unit, "tracer")) %>%
  dplyr::mutate(
    substance_unit = factor(substance_unit, levels = tracer_levels),
    substance_label = tracer_labels[as.character(substance_unit)]
  )

##########################################################################################################
# plots for each section with all tracers in pdf #########################################################
##########################################################################################################

pdff <- sprintf("section_concentrations_%s_2002-2022_tracer.pdf", temporal_resolution)

kwb.utils::preparePdf(pdfFile = pdff)

section_ids <- names(qualities$conc)[order(names(qualities$conc))]

lapply(section_ids, function(id) {

  dat_tmp <- dat %>%
    dplyr::filter(section_id == id)

  dat_tmp %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[col_date_or_datetime]],
                                 y = value,
                                 col = substance_unit,
                                 group = substance_unit)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_color_manual(
      values = tracer_colors,
      breaks = tracer_levels,
      labels = tracer_labels,
      drop   = FALSE
    ) +
    # ---> Prozentdarstellung Y-Achse
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(title = sprintf("%s: %s",
                                  unique(dat_tmp$section_id),
                                  unique(dat_tmp$section_name)
                                  ),
                  x = NULL,
                  y = "Tracer-Anteil (%)",
                  color = "Tracer") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 12),
      legend.text  = ggplot2::element_text(size = 11)
    ) %>%
    print()
})

kwb.utils::finishAndShowPdf(pdff)


##########################################################################################################
# plots for each section with all selected substances in pdf #############################################
##########################################################################################################

# --- Defaults (Okabe-Ito, farbfehlsicher) ----------------------------------------------------------
substance_levels <- c("Fluoranthen.mg.m3"
#                      , "Valsartan.mg.m3"
                      )


substance_labels <- c(
  "Fluoranthen.mg.m3"         = " Fluoranthen (\u00B5g/L)"
#  ,"Valsartan.mg.m3" = "Valsartans\u00E4aure (\u00B5g/L)"
)

substance_colors <- c(
  "Fluoranthen.mg.m3"   = "red3"  # Rot
#  ,"Valsartan.mg.m3"     = "steelblue2" # Blau
)


# prepare data for plotting
qualities <- qualities_00_dynamic_reverse

# aggregate results to monthly averages
qualities <- kwb.BerlinWaterModel.public::aggregate_qualities_monthly(qualities_00_dynamic_reverse)

dat <- qualities$conc %>%
  dplyr::bind_rows(.id = "section_id") %>%
  tidyr::pivot_longer(- tidyselect::all_of(c("section_id", col_date_or_datetime)),
                      names_to = "substance_unit") %>%
  dplyr::left_join(get_section_idnames(config), by = c("section_id" = "id")) %>%
  dplyr::rename(section_name = name) %>%
  dplyr::relocate(section_name, .after = "section_id")  %>%
  dplyr::filter(substance_unit %in% substance_levels) %>%
  dplyr::mutate(
    substance_name = stringr::str_remove(substance_unit, "\\..*"),
    unit = stringr::str_remove(substance_unit, pattern = "\\.") %>%
      stringr::str_remove(pattern = substance_name) %>%
      stringr::str_replace("\\.", "/"),
    substance_label = sprintf("%s (%s)", substance_name, unit),
    substance_unit = factor(substance_unit, levels = substance_levels),
  )

pdff <- sprintf("section_substance_concentrations_%s_Fluoranthen.pdf", temporal_resolution)

kwb.utils::preparePdf(pdfFile = pdff)

section_ids <- names(qualities$conc)[order(names(qualities$conc))]

lapply(section_ids, function(id) {

  dat_tmp <- dat %>%
    dplyr::filter(section_id == id)

  dat_tmp %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[col_date_or_datetime]],
                                 y = value,
                                 col = substance_unit,
                                 group = substance_unit)) +
    ggplot2::geom_line(linewidth = 0.3) +
    ggplot2::scale_color_manual(
      values = substance_colors,
      breaks = substance_levels,
      labels = substance_labels,
      drop   = FALSE
    ) +
    # ---> Prozentdarstellung Y-Achse
    #ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(title = sprintf("%s: %s",
                                  unique(dat_tmp$section_id),
                                  unique(dat_tmp$section_name)
    ),
    x = NULL,
    y = sprintf("Konzentration (%s)", unique(dat$unit)),
    color = "Substanz") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 12),
      legend.text  = ggplot2::element_text(size = 11)
    ) %>%
    print()
})

kwb.utils::finishAndShowPdf(pdff)

############################################################################################
# Plotten eines Abschnittes als interaktiver Graph #########################################
############################################################################################
  dat_tmp <- dat %>%
    dplyr::filter(section_id == "H08")

  plot <- dat_tmp %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[col_date_or_datetime]],
                                 y = value,
                                 col = substance_unit,
                                 group = substance_unit)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_color_manual(
      values = tracer_colors,
      breaks = tracer_levels,
      labels = tracer_labels,
      drop   = FALSE
    ) +
    # ---> Prozentdarstellung Y-Achse
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(title = sprintf("%s: %s",
                                  unique(dat_tmp$section_id),
                                  unique(dat_tmp$section_name)
    ),
    x = NULL,
    y = "Tracer-Anteil (%)",
    color = "Tracer") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 12),
      legend.text  = ggplot2::element_text(size = 11)
    )
  # ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[col_date_or_datetime]],
    #                                        y = value,
    #                                        col = substance_unit)) +
    # ggplot2::geom_line() +
    # ggplot2::labs(title = sprintf("%s: %s", unique(dat_tmp$section_id), unique(dat_tmp$section_name))) +
    # ggplot2::theme_bw() +
    # ggplot2::theme(legend.position = "top")

  plotly::ggplotly(plot)
  plotly <- plotly::ggplotly(plot)
  htmlwidgets::saveWidget(plotly,
                          file = sprintf("H08_Unterhavel2_hourly_2016-2017_CSO-0.5.html"))


################################################################################################
# Plot flows ###################################################################################
################################################################################################
flows <- flows_dynamic
#flows <- kwb.BerlinWaterModel.public::aggregate_flows_monthly(flows_dynamic)

flows_df_long <- flows %>%
  tidyr::pivot_longer(cols = !tidyselect::starts_with(col_date_or_datetime),
                      names_to = "section_id", values_to = "cbm_per_second") %>%
  dplyr::left_join(kwb.BerlinWaterModel.public::get_section_idnames(config) %>%
                     dplyr::rename(section_id = id,
                                   section_name = name),
                   by = "section_id") %>%
  dplyr::relocate("cbm_per_second", .after = "section_name")

# plot flow of single section ######################################################

gg <- flows_df_long %>%
  dplyr::filter(section_id == "S07") %>%
  ggplot2::ggplot(ggplot2::aes(x = datetime, y = cbm_per_second)) +
  ggplot2::geom_point(size = 0.5, col = "blue") +
  ggplot2::geom_line(col = "blue") +
  ggplot2::labs(x = "Date", y = "Flow (m3/s)", col = "Section Name") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

gg

plotly::ggplotly(gg)

# plots of flows for all sections in pdf ###########################################

pdff <- "BerlinWaterModel_flows_hours_2002-2022.pdf"
kwb.utils::preparePdf(pdff)
y_max <- 10*ceiling(max(flows_df_long$cbm_per_second)/10)
y_min <- 10*floor(min(flows_df_long$cbm_per_second)/10)
sapply(unique(flows_df_long$section_id)[order(unique(flows_df_long$section_id))], function(id) {

  x <- flows_df_long %>%
    dplyr::filter(section_id == id)

  label <- sprintf("%s: %s", x$section_id[1], x$section_name[1])

  gg1 <- x %>%
    ggplot2::ggplot(ggplot2::aes(x = !! rlang::sym(col_date_or_datetime), y = cbm_per_second)) +
    ggplot2::geom_point(size = 0.5, col = "blue") +
    ggplot2::geom_line(col = "blue") +
    ggplot2::ylim(c(y_min, y_max)) +
    ggplot2::labs(x = "Date", y = "Flow (m3/s)", title = label) +
    ggplot2::theme_bw()

  print(gg1)
})
kwb.utils::finishAndShowPdf(pdff)

###################################################################################################################
# additional preparation for plots below ##########################################################################
###################################################################################################################

sections <- config$sections %>%
  dplyr::select(section_id, section_name) %>%
  dplyr::mutate(section_label = sprintf("%s: %s",
                                        section_id,
                                        section_name)) %>%
  dplyr::select(- section_name)

dat <- qualities$conc %>%
  #dplyr::filter(date >= "2010-01-01" & date <= "2020-12-31") %>%
  dplyr::bind_rows(.id = "section_id") %>%
  dplyr::left_join(sections)

###################################################################################################################
# Plot of rain shares for each section (boxplots) with means (red diamonds) ordered by rain share #################
###################################################################################################################

ordered <- dat %>%
  dplyr::group_by(section_label) %>%
  dplyr::summarise("stats_mean" = dplyr::across(tidyselect::all_of("tracer.rain_runoff"), function(x) mean(x, na.rm = TRUE))) %>%
  dplyr::arrange(dplyr::desc(stats_mean))


# Berechnung des Mittelwerts pro section_id
mean_values <- dat %>%
  dplyr::group_by(section_label) %>%
  dplyr::summarise(mean_tracer = mean(tracer.rain_runoff, na.rm = TRUE)) %>%
  dplyr::left_join(sections, by = "section_label")

gg <- dat %>%
  dplyr::select(section_label, date, tracer.rain_runoff) %>%
  dplyr::left_join(mean_values) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = forcats::fct_reorder(as.factor(section_label), mean_tracer, .desc = FALSE),
                       y = tracer.rain_runoff)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_point(data = mean_values, ggplot2::aes(x = section_label, y = mean_tracer),
             shape = 23, size = 3, fill = "red", color = "black", inherit.aes = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(y = "Regenanteil (%)", x = "Gewässerabschnitte") +
  ggplot2::coord_flip() +
  ggplot2::theme_bw()

plotly::ggplotly(gg)


###################################################################################################################
# Plot of WWTP shares for each section (boxplots) with means (red diamonds) ordered by WWTP share #################
###################################################################################################################

# Berechnung des Mittelwerts pro section_id
mean_values <- dat %>%
  dplyr::group_by(section_label) %>%
  dplyr::summarise(mean_tracer = mean(tracer.wwtp, na.rm = TRUE)) %>%
  dplyr::left_join(sections, by = "section_label")

gg <- dat %>%
  dplyr::select(section_label, date, tracer.wwtp) %>%
  dplyr::left_join(mean_values) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = forcats::fct_reorder(as.factor(section_label), mean_tracer, .desc = FALSE),
                                         y = tracer.wwtp)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_point(data = mean_values, ggplot2::aes(x = section_label, y = mean_tracer),
                      shape = 23, size = 3, fill = "red", color = "black", inherit.aes = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(y = "Abwasseranteil (%)", x = "Gewässerabschnitte") +
  ggplot2::coord_flip() +
  ggplot2::theme_bw()

plotly::ggplotly(gg)


####################################################################################################
# Plots of monthly WWTP and/or rain shares (boxplots) for all sections (pdf and/or html) ###########
####################################################################################################

dat_monthly <- dat %>%
  dplyr::select(section_label, date,
                tracer.rain_runoff
              , tracer.wwtp         # comment line out to show rain only
  ) %>%
  tidyr::pivot_longer(cols = tidyselect::starts_with("tracer")) %>%
  dplyr::mutate(month_number = format(date, format = "%m") %>% as.factor(),
                name = dplyr::if_else(name == "tracer.rain_runoff", "Regenwasser",
#                                      "")) #%>%              # remove comment to show rain only
                                      "Abwasser")) #%>%       # comment line out to show rain only
#dplyr::group_by(section_label, name, month_number) %>%
#dplyr::summarise(dplyr::across(tidyselect::starts_with("value"), .fns = mean))

section_labels <- unique(dat_monthly$section_label)[order(unique(dat_monthly$section_label))]

export_html <- FALSE
debug <- TRUE
pdff <- "sections_tracers_rain_monthly_2002-2022_flow-0.5_y50_test.pdf"

kwb.utils::preparePdf(pdfFile = pdff)

lapply(section_labels, function(label) {

  message(sprintf("Plotting section '%s'", label))

  gg <- dat_monthly  %>%
    dplyr::filter(section_label == label) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = month_number,
                                           y = value,
                                           col = name)) +
#    ggplot2::geom_boxplot(color = "deepskyblue3") +         # remove comment to show rain only
    ggplot2::geom_boxplot() +                                # comment line out to show rain only
#    ggplot2::scale_y_continuous(limits = c(0,0.5), labels = scales::percent_format()) +    # remove to show rain only
    ggplot2::guides(col = ggplot2::guide_legend(title.position="top", title ="")) +
    ggplot2::labs(y = "Traceranteil (%)", x = "Monat", title = sprintf("Monatliche Anteile (2002-2022) für %s",
                                                                       label)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")


  if(export_html) {
    p_gg <- plotly::ggplotly(gg)

    htmlwidgets::saveWidget(widget = p_gg,
                            file = sprintf("tracers_rain-wwtp_monthly_%s.html",
                                           janitor::make_clean_names(label)))
  }

  gg

})

kwb.utils::finishAndShowPdf(pdff)

##############################################################################################
### Flow & Tracers for all sections for Masterplan Period ####################################
##############################################################################################

sections <- config$sections

qualities_selected <- qualities$conc %>%
  dplyr::bind_rows(.id = "section_id")

flows_selected <- flows_dynamic %>%
  tidyr::pivot_longer(-date, names_to = "section_id", values_to = "cbm_per_second")


tmp <- qualities_selected %>%
  dplyr::left_join(flows_selected) %>%
  dplyr::filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-11-30")) %>%
  dplyr::select(section_id, date, cbm_per_second, tidyselect::starts_with("tracer")) %>%
  tidyr::pivot_longer(- tidyselect::all_of(c("section_id", "date"))) %>%
  dplyr::mutate(type = dplyr::if_else(name == "cbm_per_second",
                                      "Durchfluss (m3/s)",
                                      "Traceranteil (%)"))


pdff <- "sections_tracers_flow_2019-03-01_2019-11-30_00_test.pdf"

kwb.utils::preparePdf(pdfFile = pdff)

section_ids <- unique(tmp$section_id)[order(unique(tmp$section_id))]

lapply(section_ids, function(id) {

  section_name <- sections$section_name[sections$section_id == id]

  message(sprintf("Plotting section '%s'", section_name))


  gg <- tmp %>%
    dplyr::filter(section_id == id) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = value, col = name)) +
    ggplot2::facet_wrap( ~ type, ncol = 1, scales = "free_y") +
    ggplot2::geom_line() +
    #ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::guides(col = ggplot2::guide_legend(title.position="top", title ="")) +
    ggplot2::labs(y = "",
                  x = "Datum",
                  title = sprintf("Tägliche Anteile (2019-03-01 - 2019-11-30) für %s (%s)",
                                  section_name,
                                  id)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  gg
})

kwb.utils::finishAndShowPdf(pdff)


###################################################################################################################
# Plots of yearly WWTP & rain shares over time for all sections  ##################################################
###################################################################################################################

# WWTP shares ##########
dat_yearly <- dat %>%
  dplyr::mutate(year = format(date, format = "%Y") %>% as.integer()) %>%
  dplyr::group_by(section_label, year)

gg <- dat_yearly %>%
  dplyr::summarise(mean_tracer = mean(tracer.wwtp, na.rm = TRUE)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = year,
                                         y = mean_tracer,
                                         col = section_label)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(y = "Abwasseranteil (%)", x = "Gewässerabschnitte", title = "Jährliche Abwasseranteile") +
  ggplot2::guides(col = ggplot2::guide_legend(title.position="top", title ="Gewässerabschnitte")) +
  ggplot2::theme_bw()

plotly::ggplotly(gg)

# rain shares ##########
gg <- dat_yearly %>%
  dplyr::summarise(mean_tracer = mean(tracer.rain_runoff, na.rm = TRUE)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = year,
                                         y = mean_tracer,
                                         col = section_label)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(y = "Regenwasseranteil (%)", x = "Gewässerabschnitte", title = "Jährliche Regenwasseranteile") +
  ggplot2::guides(col = ggplot2::guide_legend(title.position="top", title ="Gewässerabschnitte")) +
  ggplot2::theme_bw()

plotly::ggplotly(gg)


####################################################################################################
# Plot of monthly rain shares over time for all sections in one diagram ############################
####################################################################################################

dat_monthly <- dat %>%
  dplyr::mutate(year_month = sprintf("%s-01",
                                     format(date, format = "%Y-%m"))) %>%
  dplyr::group_by(section_label, year_month)

gg <- dat_monthly  %>%
  dplyr::summarise(mean_tracer = mean(tracer.rain_runoff, na.rm = TRUE)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = year_month,
                                         y = mean_tracer,
                                         col = section_label)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(y = "Regenwasseranteil (%)", x = "Gewässerabschnitte", title = "Monatliche Regenwasseranteile") +
  ggplot2::guides(col = ggplot2::guide_legend(title.position="top", title ="Gewässerabschnitte")) +
  ggplot2::theme_bw()

plotly::ggplotly(gg)


###############################################################################################################
### Analyse results: comparison results hourly vs.daily for flows and tracers (selected section as html-file ##
###############################################################################################################

flows_hours <- readRDS(file = "flows_hours.Rds")
flows_days <- readRDS(file = "flow_days.Rds")

qualities_hours <- readRDS(file = "qualities_hours.Rds")
qualities_days <- readRDS(file = "qualities_days.Rds")

plot_comparison <- function(section_id) {
  section_id_hours <- sprintf("%s_hours", section_id)
  section_id_days <- sprintf("%s_days", section_id)
  res_comparison <- flows_hours[,c("datetime", section_id)]  %>%
    dplyr::rename(!! section_id_hours := section_id) %>%
    dplyr::left_join(flows_days[,c("date", section_id)] %>%
                       dplyr::rename(datetime = date,
                                     !! section_id_days := section_id) %>%
                       dplyr::mutate(datetime = as.POSIXct(datetime, tz = "UTC"))) %>%
    tidyr::fill(tidyselect::matches(section_id_days), .direction = "down") %>%
    dplyr::left_join(qualities_hours$conc[[section_id]] %>%
                       dplyr::select("datetime", tidyselect::starts_with("tracer")) %>%
                       dplyr::rename_with(~ paste0(.x, "_hours"), starts_with("tracer")) %>%
                       dplyr::left_join(qualities_days$conc[[section_id]] %>%
                                          dplyr::select("date", tidyselect::starts_with("tracer")) %>%
                                          dplyr::rename_with(~ paste0(.x, "_days"), starts_with("tracer")) %>%
                                          dplyr::rename(datetime = date) %>%
                                          dplyr::mutate(datetime = as.POSIXct(datetime, tz = "UTC"))) %>%
                       tidyr::fill(tidyselect::matches("^tracer.*_days$"), .direction = "down")) %>%
    tidyr::pivot_longer(-datetime) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value))

  gg <- res_comparison %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = datetime, y = value, col = name)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()

  plotly_gg <- plotly::ggplotly(gg)
  htmlwidgets::saveWidget(plotly_gg,
                          file = sprintf("res-comparison_days-hours_%s.html", section_id))
}

plot_comparison(section_id = "S14")


###############################################################################################################
### Analyse results: tracer for Tegeler See (when is lake "filled") ###########################################
###############################################################################################################

section_id <- "H03"
flows <- flows_static
qualities_00 <- qualities_00_static

flow_positive_0.000001 <- dplyr::if_else(flows[[section_id]] < 0,  0.000001, flows[[section_id]])

conc_flow_positive_0.000001 <- kwb.BerlinWaterModel.public::calc_conc(c_in = 1,
                                                               c_0 = 0,
                                                               Q = flow_positive_0.000001,
                                                               V = 30000000,
                                                               k = 0,
                                                               t = rep(24*3600, nrow(flows)))

median(conc_flow_positive_0.000001)

plot(seq_len(nrow(flows))/365,
     conc_flow_positive_0.000001,
     col = "blue",
     xlab = "Years",
     ylab = "Tracer share (%)",
     main = sprintf("%s (%s): with positive flow >= 0.000001  m3/s (median: %.3f m3/s, 90%%: %.2f years, 95%%: %.2f years)",
                    kwb.BerlinWaterModel.public::get_names_from_ids(section_id, config),
                    section_id,
                    median(flow_positive_0.000001),
                    which(conc_flow_positive_0.000001 > 0.90)[1]/365,
                    which(conc_flow_positive_0.000001 > 0.95)[1]/365
     ))
abline(v = c(which(conc_flow_positive_0.000001 > 0.90)[1]/365,
             which(conc_flow_positive_0.000001 > 0.99)[1]/365), col = "red")

points(seq_len(nrow(flows))/365,
       qualities_00$conc[[section_id]]$tracer.cso + qualities_00$conc[[section_id]]$tracer.inlet + qualities_00$conc[[section_id]]$tracer.rain_runoff + qualities_00$conc[[section_id]]$tracer.ww_discharge + qualities_00$conc[[section_id]]$tracer.wwtp,
       col = "red")


###############################################################################
# plot daily and hourly rain data and save as html-graph ######################
###############################################################################

if(FALSE) {
  # put hourly input into input_hours and daily input into input_days
  #  input_hours <- input
  input_days <- input

  tmp <- input_hours[,c("datetime", "DWD_0433")] %>%
    dplyr::rename(DWD_0433_hours = DWD_0433) %>%
    dplyr::left_join(input_days[,c("date", "DWD_0433")] %>%
                       dplyr::rename(datetime = date,
                                     DWD_0433_days = DWD_0433) %>%
                       dplyr::mutate(datetime = as.POSIXct(datetime, tz = "UTC"))) %>%
    tidyr::fill(DWD_0433_days, .direction = "down") %>%
    tidyr::pivot_longer(-datetime) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value))

  gg <- tmp %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = datetime, y = value, col = name)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()

  plotly_gg <- plotly::ggplotly(gg)

  htmlwidgets::saveWidget(plotly_gg, file = "input_dwd_days_hours.html")
}

