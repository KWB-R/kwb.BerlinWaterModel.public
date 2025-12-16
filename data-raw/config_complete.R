#remotes::install_github("kwb-r/kwb.BerlinWaterModel@70c9e79f7f492163acd4b5e6784bae77286eaf18")
library(kwb.BerlinWaterModel)

# read config files ##########################################################

#config <- kwb.BerlinWaterModel::config_read(config_dir = "inst/extdata/config/network_complete")
config <- kwb.BerlinWaterModel::config_read(config_dir = "inst/extdata/config/network_complete_mean-start-conc")
#config <- kwb.BerlinWaterModel::config_read(config_dir = "inst/extdata/config/network_complete_2019")

config <- kwb.BerlinWaterModel::add_rain_direct_and_evaporation(config)

config <- kwb.BerlinWaterModel::add_tracers(config)

config <- kwb.BerlinWaterModel::add_substances(config)

network <- kwb.BerlinWaterModel::prepare_network(config)


#kwb.BerlinWaterModel::check_network_errors(network)

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
#net_complex <- kwb.BerlinWaterModel::plot_network_complex(network,
#                                                        config,
#                                                        show_labels = TRUE)

#htmlwidgets::saveWidget(net_complex, file = "water-cycle_complex.html")
#zip(zipfile = "water-cycle_complex.zip", files = "water-cycle_complex.html")


# data preparation ###########################################################

temporal_resolution <- "days" # "hours" or "days"

col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                               "datetime",
                               "date")

inputs <- kwb.BerlinWaterModel::add_scenario(
  config = config,
  use_scenario = FALSE, # should scaling factors be used (in config$scenarios) or not?
  debug = TRUE)

input_list <- kwb.BerlinWaterModel::prepare_input(temporal_resolution = temporal_resolution,
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
                                                  date_min = "2016-01-01",
                                                  date_max = "2022-12-31")

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

# input with dynamic bank filtration share (depending on Q) ####################################
input_list_bfdynamic <- kwb.BerlinWaterModel::prepare_input(temporal_resolution = temporal_resolution,
                                                            config = config,
                                                            cso = cso,
                                                            inflows = inflows,
                                                            share_wwtp_sch_to_nordgraben_timeseries = TRUE,
                                                            # share_wwtp_sch_panke_1 = 0.1,
                                                            # date_separation_panke_1_2 = "2015-04-15",
                                                            # share_wwtp_sch_panke_2 = 0.9,
                                                            rain = rain,
                                                            ww = ww,
                                                            wwtp = wwtp,
                                                            bfshare_dynamic = TRUE,
                                                            date_min = "2002-01-01",
                                                            date_max = "2022-12-31")



################################################################################################
### Calculate flows ############################################################################
################################################################################################

system.time(
flows_static <- kwb.BerlinWaterModel::calculate_flows_auto(config = config,
                                                           input_list = input_list,
                                                           network = network,
                                                           use_dynamic = FALSE,
                                                           debug = TRUE)
)

# calculate flows using functions in outflows_multiple.csv for river branching
system.time(
  flows_dynamic <- kwb.BerlinWaterModel::calculate_flows_auto(config = config,
                                                              input_list = input_list,
                                                              network = network,
                                                              use_dynamic = TRUE,
                                                              debug = TRUE)
)

flows_stats <- kwb.BerlinWaterModel::calculate_flow_stats(flows = flows_dynamic)

flows_dynamic_neg_flows_stat <- kwb.BerlinWaterModel::get_reverse_flows_per_section(flows = flows_dynamic)
#openxlsx::write.xlsx(x = flows_neg_stat_evap_30, file = "flows_neg_stat_evap30_2019.xlsx")

# Wenn Durchfluss im Tegeler negativ, dann Ausgleich von Oberhavel (über Seeleitung) #########################
if(FALSE) {
  input_fixtegel <- input
  input_fixtegel$Seeleitung_in[flows$H03 < 0] <- input$Seeleitung_in[flows$H03 < 0] + abs(flows$H03[flows$H03 < 0])
  input_fixtegel$Seeleitung_out[flows$H03 < 0] <- input$Seeleitung_out[flows$H03 < 0] + flows$H03[flows$H03 < 0]

  system.time(
    flows <- kwb.BerlinWaterModel::calculate_flows_auto(config = config,
                                                        input = input_fixtegel,
                                                        network = network,
                                                        debug = TRUE))
}

# Save flows of one or all sections to xls
openxlsx::write.xlsx(x = flows_dynamic, file = "flows_days_2019_start-con-2019.xlsx")
# openxlsx::write.xlsx(x = flows_dynamic[c("date", "H03")], file = "flows_H03_days.xlsx")


################################################################################################
### Calculate water qualities ##################################################################
################################################################################################

system.time(
  qualities_00_dynamic_forward <- kwb.BerlinWaterModel::calculate_qualities(input_list = input_list,
                                                                    flows = flows_dynamic,
                                                                    network = network,
                                                                    config = config,
                                                                    reverse_flow = FALSE,
                                                                    debug = FALSE)
)

system.time(
  qualities_00_dynamic_reverse <- kwb.BerlinWaterModel::calculate_qualities(input_list = input_list,
                                                                    flows = flows_dynamic,
                                                                    network = network,
                                                                    config = config,
                                                                    branchwise = TRUE,
                                                                    reverse_flow = TRUE,
                                                                    debug = FALSE)
)

### Save qualities for one or all sections in XLSX
#openxlsx::write.xlsx(x = qualities$conc$S21, file = "qualities_S21_days_dynamic_branching_fixJOH.xlsx")
openxlsx::write.xlsx(x = qualities_00_dynamic_reverse$conc, file = "qualities_hours_2017-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,2.xlsx")

# qualities_00_dynamic_reverse <- readRDS(file = "qualities_2002-2022_Val5.5_Cbz.Rds")

# Save result of latest time point in sections.csv
config_01 <- kwb.BerlinWaterModel::set_tracer_starting_conc(config, qualities_00_dynamic_reverse)
readr::write_csv2(config_01$sections, "inst/extdata/config/network_complete_2019/sections.csv")

# Adapt start concentrations for all sections to mean of "qualities_00_dynamic_reverse" (after section has tracer_sum > 99.9 percent)
config_01_dynamic_reverse <- kwb.BerlinWaterModel::set_tracer_starting_conc_stats(config = config,
                                                                                  qualities = qualities_00_dynamic_reverse,
                                                                                  aggregation_function = mean,
                                                                                  minimum_tracer_sum = 0.999)

readr::write_csv2(config_01_dynamic_reverse$sections, "inst/extdata/config/network_complete_mean-start-conc/sections.csv")

# save RDS
#saveRDS(flows_dynamic, file = "flows_hours_2002-2022.Rds")
saveRDS(qualities_00_dynamic_reverse, file = "qualities_hours_2016-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,20.Rds")

# Read RDS
qualities_00_dynamic_reverse <- readRDS(file = "qualities_hours_2016-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,20.Rds")
flows_dynamic <- readRDS(file = "flows_hours_2002-2022.Rds")


#############################################################################################
### prepare qsimVis output ##################################################################
#############################################################################################

if(FALSE) {

  qsimVis_input <- kwb.BerlinWaterModel::prepare_qsimVis_input(
    config = config,
    flows = flows_dynamic,
    qualities = qualities_00_dynamic_reverse) %>%
    dplyr::select(GewaesserId,
                  Strang = "section_id",
                  Km,
                  tidyselect::starts_with("date"),
                  cbm_per_second,
                  tracer.cso,
                  tracer.rain_runoff,
                  tracer.wwtp
                  #, ValsartansaeureAeq.mg.m3
                  , Fluoranthen.mg.m3
    ) %>%
    dplyr::rename(Q = cbm_per_second)

  # add section name
  for(x in config$sections$section_id){
    qsimVis_input$Strang[qsimVis_input$Strang == x] <-
      paste(x,config$sections$section_name[config$sections$section_id == x], sep = ".")
  }

#  qsimVis_input[is.na(qsimVis_input)] <- 0

  col_date_time_idx <- stringr::str_detect(names(qsimVis_input), "^date")
  names(qsimVis_input)[col_date_time_idx] <- "Datum"

  qsimVis_input <- qsimVis_input %>%
  dplyr::mutate(Datum = format(Datum, format = "%d.%m.%Y %H:%M", tz = "UTC"))

  #readr::write_csv2(qsimVis_input,"qsimVis_input2.csv") takes much longer for big datasets
  system.time(data.table::fwrite(qsimVis_input, "qsimVis_input_hours_2017-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,20.csv", sep = ";", dec = ","))

}

#############################################################################################
### Diverse checks ##########################################################################
#############################################################################################

# Add check to compare modeled outflow to balance corrected input data
out_hydrax <- input_list$flows$PHv_0
out_model <- flows_dynamic$Out
median(out_model/out_hydrax)

# Quick check if results at outflows are "more-or-less" the same for "static"
# for "dynamic" multiple outflows!
summary(flows_static$Out - flows_dynamic$Out)

### Check: results should be identical for S01 and S05
summary(flows_static$S01 - flows_dynamic$S01)
summary(flows_static$S05 - flows_dynamic$S05)


### Check: results should be different for S07 and S21
summary(flows_static$S07 - flows_dynamic$S07)
summary(flows_static$S21 - flows_dynamic$S21)


# Quick check if results at outflows are "more-or-less" the same for "static"
# for "dynamic" multiple outflows!
summary(flows_static$Out - flows_dynamic$Out)

### Check: results should be identical for S01 and S05
summary(flows_static$S01 - flows_dynamic$S01)
summary(flows_static$S05 - flows_dynamic$S05)


### Check: results should be different for S07 and S21
summary(flows_static$S07 - flows_dynamic$S07)
summary(flows_static$S21 - flows_dynamic$S21)


# check of difference between ww_sum for bfshare_static and bfshare_dynamic #####################
if(FALSE) {
  ww_flow_ids <- config$flows_in_out %>%
    dplyr::filter(stringr::str_starts(flow_id, "WW_")) %>%
    shorten_ww_flow_id() %>%
    dplyr::filter(stringr::str_ends(flow_id, "gwa", negate = TRUE)) %>%
    dplyr::arrange(flow_id) %>%
    dplyr::pull(flow_id)

  sum(names(input_list$flows) %in% ww_flow_ids)

  ww_sum_static <- input_list$flows[,c(col_date_or_datetime, ww_flow_ids)] %>%
    dplyr::mutate(sum_static = rowSums(dplyr::across(tidyselect::all_of(ww_flow_ids)), na.rm = TRUE)) %>%
    dplyr::select(tidyselect::all_of(col_date_or_datetime), sum_static)

  ww_sum_dynamic <- input_list_bfdynamic$flows[,c(col_date_or_datetime, ww_flow_ids)] %>%
    dplyr::mutate(sum_dynamic = rowSums(dplyr::across(tidyselect::all_of(ww_flow_ids)), na.rm = TRUE)) %>%
    dplyr::select(tidyselect::all_of(col_date_or_datetime), sum_dynamic)

  median(ww_sum_static$sum_static)
  median(ww_sum_dynamic$sum_dynamic)
}

#### Comparison: BF shares static vs dynamic for each well gallery ##########################

flow_ids <- config$flows_in_out %>%
  kwb.BerlinWaterModel::shorten_ww_flow_id() %>%
  dplyr::pull(flow_id)

ww_bfshare <- kwb.BerlinWaterModel::get_bfshares(config = config,
                                                 ww = ww,
                                                 temporal_resolution = temporal_resolution,
                                                 bfshare_dynamic = bfshare_dynamic)


ww_bfshare_long <- ww_bfshare %>%
  dplyr::select(- bank_filtration_share) %>%
  tidyr::pivot_longer(cols = c("bank_filtration_share_static", "bank_filtration_share_dynamic"))

flow_ids <- ww_bfshare_long %>%
  dplyr::count(flow_id, type) %>%
  dplyr::filter(!is.na(type)) %>%
  dplyr::arrange(flow_id) %>%
  dplyr::pull(flow_id)


pdff <- "waterworks_galleries_bfshare_static_vs_dynamic.pdf"
kwb.utils::preparePdf(pdfFile = pdff)

lapply(flow_ids, function(f_id) {

  ww_bfshare_long_tmp <- ww_bfshare_long %>%
    dplyr::filter(flow_id == f_id) %>%
    dplyr::mutate(name = dplyr::if_else(name == "bank_filtration_share_static",
                                        "static",
                                        "dynamic")#,
                  #value = value * 100
    )

  bfs_equation_tmp <- bfstypes_equations[bfstypes_equations$type == unique(ww_bfshare_long_tmp$type),]

  label <- sprintf("%s: %s (Type: %d, %d m3/d - %d m3/d)",
                   f_id,
                   bfs_equation_tmp$equation_text,
                   bfs_equation_tmp$type,
                   bfs_equation_tmp$Q_min_m3d,
                   bfs_equation_tmp$Q_max_m3d)


  ww_bfshare_long_tmp %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = date,
                                           y = value,
                                           col = name)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                name = "Surface Water Influence [%]",
                                limits = c(0,1)) +
    ggplot2::labs(title = label) +
    ggplot2::theme_bw() +
    ggplot2::guides(col = ggplot2::guide_legend(title.position="top", title ="Surface Water Influence [%]")) +
    ggplot2::theme(legend.position = "top")

})

kwb.utils::finishAndShowPdf(pdff)




