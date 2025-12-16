#remotes::install_github("kwb-r/kwb.BerlinWaterModel@1cd9b6b50ea05328f5d9ef18c9ab89dd6e2d0314")
library(kwb.BerlinWaterModel)

config <- kwb.BerlinWaterModel::config_read(config_dir = "inst/extdata/config/network_complete")

config <- kwb.BerlinWaterModel::add_tracers(config)

config <- kwb.BerlinWaterModel::add_substances(config)

network <- kwb.BerlinWaterModel::prepare_network(config)


#kwb.BerlinWaterModel::check_network_errors(network)

### Network graph: simple
#plot_network_simple(network)

### Network graph: complex
# net_complex <- kwb.BerlinWaterModel::plot_network_complex(network,
#                                                          config,
#                                                          show_labels = TRUE)


#htmlwidgets::saveWidget(net_complex, file = "water-cycle_complex.html")
#zip(zipfile = "water-cycle_complex.zip", files = "water-cycle_complex.html")

return_inputs <- FALSE
temporal_resolution <- "days" # "hours" or "days"


ww_flow_ids <- config$flows_in_out %>%
  dplyr::filter(stringr::str_starts(flow_id, "WW_")) %>%
  shorten_ww_flow_id() %>%
  dplyr::filter(stringr::str_ends(flow_id, "gwa", negate = TRUE)) %>%
  dplyr::arrange(flow_id) %>%
  dplyr::pull(flow_id)


col_datetime <- ifelse(temporal_resolution == "hours",
                       "datetime",
                       "date")

input <- kwb.BerlinWaterModel::prepare_input(temporal_resolution = temporal_resolution,
                                             config = config,
                                             cso = cso,
                                             inflows = inflows,
                                             share_wwtp_sch_panke = 0.8,
                                             rain = rain,
                                             ww = ww,
                                             wwtp = wwtp,
                                             bfshare_dynamic = FALSE,
                                             date_min = "2002-01-01",
                                             date_max = "2022-12-31")

input_dynamic <- kwb.BerlinWaterModel::prepare_input(temporal_resolution = temporal_resolution,
                                             config = config,
                                             cso = cso,
                                             inflows = inflows,
                                             share_wwtp_sch_panke = 0.8,
                                             rain = rain,
                                             ww = ww,
                                             wwtp = wwtp,
                                             bfshare_dynamic = TRUE,
                                             date_min = "2002-01-01",
                                             date_max = "2022-12-31")

sum(names(input) %in% ww_flow_ids)

ww_sum_static <- input[,c(col_datetime, ww_flow_ids)] %>%
  dplyr::mutate(sum_static = rowSums(dplyr::across(tidyselect::all_of(ww_flow_ids)), na.rm = TRUE)) %>%
  dplyr::select(tidyselect::all_of(col_datetime), sum_static)

ww_sum_dynamic <- input_dynamic[,c(col_datetime, ww_flow_ids)] %>%
  dplyr::mutate(sum_dynamic = rowSums(dplyr::across(tidyselect::all_of(ww_flow_ids)), na.rm = TRUE)) %>%
  dplyr::select(tidyselect::all_of(col_datetime), sum_dynamic)

ww_sum <- ww_sum_static %>%
  dplyr::bind_cols(ww_sum_dynamic[,2])

if(FALSE) {
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

col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                               "datetime",
                               "date")

links_by_orderid <- kwb.BerlinWaterModel::get_flowpath_table(outflow_id = "Out",
                                                             network = network,
                                                             config = config)


flows_order11 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_11,
  input = input,
  config = config,
  return_inputs = return_inputs
)

input_order10 <- input %>%
  dplyr::left_join(flows_order11, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order10 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_10,
  input = input_order10,
  config = config,
  return_inputs = return_inputs
)

input_order09 <- input_order10 %>%
  dplyr::left_join(flows_order10,  by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))


flows_order09 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_09,
  input = input_order09,
  config = config,
  return_inputs = return_inputs
)

input_order08 <- input_order09 %>%
  dplyr::left_join(flows_order09, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order08 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_08,
  input = input_order08,
  config = config,
  return_inputs = return_inputs
)

input_order07 <- input_order08 %>%
  dplyr::left_join(flows_order08, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order07 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_07,
  input = input_order07,
  config = config,
  return_inputs = return_inputs
)


input_order06 <- input_order07 %>%
  dplyr::left_join(flows_order07, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order06 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_06,
  input = input_order06,
  config = config,
  return_inputs = return_inputs
)

input_order05 <- input_order06 %>%
  dplyr::left_join(flows_order06, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order05 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_05,
  input = input_order05,
  config = config,
  return_inputs = return_inputs
)

input_order04 <- input_order05 %>%
  dplyr::left_join(flows_order05, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order04 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_04,
  input = input_order04,
  config = config,
  return_inputs = return_inputs
)

input_order03 <- input_order04 %>%
  dplyr::left_join(flows_order04, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))


flows_order03 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_03,
  input = input_order03,
  config = config,
  return_inputs = return_inputs
)

input_order02 <- input_order03 %>%
  dplyr::left_join(flows_order03, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))

flows_order02 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_02,
  input = input_order02,
  config = config,
  return_inputs = return_inputs
)

input_order01 <- input_order02 %>%
  dplyr::left_join(flows_order02, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column")) %>%
  tibble::as_tibble()
#  %>% dplyr::mutate("KLA" = 0)

flows_order01 <-  kwb.BerlinWaterModel::calculate_flows(
  df_order = links_by_orderid$orderid_01,
  input = input_order01,
  config = config,
  return_inputs = return_inputs
)

flows_order00 <- kwb.BerlinWaterModel::calculate_outflows(
list_order = links_by_orderid,
input = input_order01 %>%
  dplyr::left_join(flows_order01, by = col_date_or_datetime, suffix = c(".annoying_duplicate_column", "")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column")) %>%
  tibble::as_tibble(),
config = config,
return_inputs = return_inputs
)

res <- tibble::tibble()

for (i in 0:10) {

x <- if (i == 0) {
  get(sprintf("flows_order%02d", i))
} else {
  res
}

y <- get(sprintf("flows_order%02d", i+1))

res <-  x %>%
  dplyr::left_join(y, by = col_date_or_datetime, suffix = c("", ".annoying_duplicate_column")) %>%
  dplyr::select(-tidyselect::ends_with(".annoying_duplicate_column"))
}


flows_df_wide <- res[,c(names(res[1]), names(res[-1])[order(names(res[-1]))])]

# Add check to compare modeled outflow to balance corrected input data
out_hydrax <- input$PHv_0
out_model <- flows_df_wide$Out
median(out_model/out_hydrax)


#### Plot flows
if(FALSE) {

flows_df_long <- flows_df_wide %>%
  tidyr::pivot_longer(cols = !tidyselect::starts_with(col_date_or_datetime),
                      names_to = "section_id", values_to = "cbm_per_second") %>%
  dplyr::mutate(section_name = kwb.utils::multiSubstitute(section_id,
                                                          stats::setNames(as.list(kwb.BerlinWaterModel::get_section_idnames(config)$name),
                                                                          kwb.BerlinWaterModel::get_section_idnames(config)$id)),
                label = sprintf("%s: %s", section_id, section_name)) %>%
  dplyr::relocate("cbm_per_second", .after = "section_name")



gg <- flows_df_long %>%
  dplyr::filter(section_id == "S06") %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = cbm_per_second, col = label)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(x = "Date", y = "Flow (m3/s)", col = "Section Name") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

gg

plotly::ggplotly(gg)

pdff <- "UrbanWaterModel_flows.pdf"
kwb.utils::preparePdf(pdff)
y_max <- 10*ceiling(max(flows_df_long$cbm_per_second)/10)
y_min <- 10*floor(min(flows_df_long$cbm_per_second)/10)
sapply(unique(flows_df_long$section_id)[order(unique(flows_df_long$section_id))], function(id) {

  x <- flows_df_long %>%
    dplyr::filter(section_id == id)

  label <- sprintf("%s: %s", x$section_id[1], x$section_name[1])

  gg1 <- x %>%
    ggplot2::ggplot(ggplot2::aes(x = col_date_or_datetime, y = cbm_per_second)) +
    ggplot2::geom_point(col = "blue") +
    ggplot2::geom_line(col = "blue") +
    ggplot2::ylim(c(y_min, y_max)) +
    ggplot2::labs(x = "Date", y = "Flow (m3/s)", title = label) +
    ggplot2::theme_bw()

  print(gg1)
})
kwb.utils::finishAndShowPdf(pdff)
}

### Calculate water qualities

qualities_00 <- kwb.BerlinWaterModel::calculate_qualities(input = input_order01,
                                                       flows = flows_df_wide,
                                                       network = network,
                                                       config = config)

# qualities_00 <- readRDS(file = "qualities_days.Rds")

# Adapt start concentrations for all sections
config_01 <- kwb.BerlinWaterModel::set_tracer_starting_conc(config, qualities_00)

qualities_01 <- kwb.BerlinWaterModel::calculate_qualities(input = input_order01,
                                                       flows = flows_df_wide,
                                                       network = network,
                                                       config = config_01)

# Adapt start concentrations for all sections
config_02 <- kwb.BerlinWaterModel::set_tracer_starting_conc(config_01, qualities_01)

qualities_02 <- kwb.BerlinWaterModel::calculate_qualities(input = input_order01,
                                                          flows = flows_df_wide,
                                                          network = network,
                                                          config = config_02)


### prepare qsimVis output
if(FALSE) {

qsimVis_input <- kwb.BerlinWaterModel::prepare_qsimVis_input(config = config,
                                                             flows = flows_days,
                                                             qualities = qualities_days) %>%
  dplyr::mutate(Strang = "") %>%
  dplyr::select(GewaesserId,
                Strang,
                Km,
                tidyselect::starts_with("date"),
                cbm_per_second,
                tracer.cso,
                tracer.rain,
                tracer.wwtp) %>%
  dplyr::rename(Q = cbm_per_second)

col_date_time_idx <- stringr::str_detect(names(qsimVis_input), "^date")
names(qsimVis_input)[col_date_time_idx] <- "Datum"

qsimVis_input <- qsimVis_input %>%
  dplyr::mutate(Datum = format(Datum, format = "%d.%m.%Y %H:%M", tz = "UTC"))

#readr::write_csv2(qsimVis_input,"qsimVis_input2.csv") takes much longer for big datasets
system.time(data.table::fwrite(qsimVis_input, "qsimVis_input_days.csv", sep = ";", dec = ","))
}


dat <- qualities$conc %>%
  dplyr::bind_rows(.id = "section_id") %>%
  dplyr::mutate(tracer_sum = tracer.cso + tracer.inlet + tracer.rain + tracer.wwtp) %>%
  tidyr::pivot_longer(- tidyselect::all_of(c("section_id", col_date_or_datetime)),
                      names_to = "substance_unit") %>%
  dplyr::left_join(get_section_idnames(config), by = c("section_id" = "id")) %>%
  dplyr::rename(section_name = name) %>%
  dplyr::relocate(section_name, .after = "section_id")  %>%
  dplyr::filter(stringr::str_starts(substance_unit, "tracer"))


### Plot water qualities
if(FALSE) {
pdff <- sprintf("section_concentrations_%s_with-concentration.pdf", temporal_resolution)

kwb.utils::preparePdf(pdfFile = pdff)

section_ids <- names(qualities$conc)[order(names(qualities$conc))]

lapply(section_ids, function(id) {

    dat_tmp <- dat %>%
    dplyr::filter(section_id == id)

    dat_tmp %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[col_date_or_datetime]],
                                           y = value,
                                           col = substance_unit)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = sprintf("%s: %s", unique(dat_tmp$section_id), unique(dat_tmp$section_name))) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") %>%
    print()
})

kwb.utils::finishAndShowPdf(pdff)
}

# Plotten eines Abschnittes als interaktiver Graph
if(FALSE) {

  dat_tmp <- dat %>%
    dplyr::filter(section_id == "S19")

  plot <- dat_tmp %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[col_date_or_datetime]],
                                           y = value,
                                           col = substance_unit)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = sprintf("%s: %s", unique(dat_tmp$section_id), unique(dat_tmp$section_name))) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  plotly::ggplotly(plot)
  plotly <- plotly::ggplotly(plot)
  htmlwidgets::saveWidget(plotly,
                          file = sprintf("S19_Spree5_hourly.html"))
}

### Analyse results (comparison results daily vs. hourly)
if(FALSE) {
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

plot_comparison(section_id = "S03")
}

