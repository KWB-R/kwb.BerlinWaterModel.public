#remotes::install_github("kwb-r/kwb.BerlinWaterModel.public")
library(kwb.BerlinWaterModel.public)

# read config files ##########################################################

config <- kwb.BerlinWaterModel.public::config_read(config_dir = "inst/extdata/config/network_complete")

config <- kwb.BerlinWaterModel.public::add_rain_direct_and_evaporation(config)

config <- kwb.BerlinWaterModel.public::add_tracers(config)

config <- kwb.BerlinWaterModel.public::add_substances(config)

network <- kwb.BerlinWaterModel.public::prepare_network(config)


# plot interactive network map ###############################################

### Network graph
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

inputs <- kwb.BerlinWaterModel.public::add_scenario(
  config = config,
  use_scenario = FALSE, # should scaling factors be used (in config$scenarios) or not?
  debug = TRUE)

input_list <- kwb.BerlinWaterModel.public::prepare_input(temporal_resolution = temporal_resolution,
                                                  config = config,
                                                  cso = inputs$cso,
                                                  inflows = inputs$inflows,
                                                  share_wwtp_sch_to_nordgraben_timeseries = TRUE,
                                                  rain = inputs$rain,
                                                  evapo_p = inputs$evapo_p,
                                                  ww = inputs$ww,
                                                  wwtp = inputs$wwtp,
                                                  bfshare_dynamic = FALSE,  # use TRUE for dynamic bank filtration share (depending on Q)
                                                  date_min = "2016-01-01",
                                                  date_max = "2016-12-31")

################################################################################################
### Calculate flows ############################################################################
################################################################################################

system.time(
  flows_dynamic <- kwb.BerlinWaterModel.public::calculate_flows_auto(config = config,
                                                              input_list = input_list,
                                                              network = network,
                                                              use_dynamic = TRUE, # FALSE: static values for flow shares at river branchings
                                                              debug = TRUE)
)

flows_stats <- kwb.BerlinWaterModel.public::calculate_flow_stats(flows = flows_dynamic)

flows_dynamic_neg_flows_stat <- kwb.BerlinWaterModel.public::get_reverse_flows_per_section(flows = flows_dynamic)
#openxlsx::write.xlsx(x = flows_neg_stat_evap_30, file = "flows_neg_stat_evap30_2019.xlsx")


# Save flows of one or all sections to xls
openxlsx::write.xlsx(x = flows_dynamic, file = "flows_days_2019_start-con-2019.xlsx")
# openxlsx::write.xlsx(x = flows_dynamic[c("date", "H03")], file = "flows_H03_days.xlsx")


################################################################################################
### Calculate water qualities ##################################################################
################################################################################################

system.time(
  qualities_00_dynamic_reverse <- kwb.BerlinWaterModel.public::calculate_qualities(input_list = input_list,
                                                                    flows = flows_dynamic,
                                                                    network = network,
                                                                    config = config,
                                                                    branchwise = TRUE,
                                                                    reverse_flow = TRUE, # use FALSE for calculation without considering backflows
                                                                    debug = FALSE)
)

### Save qualities for one or all sections in XLSX
#openxlsx::write.xlsx(x = qualities$conc$S21, file = "qualities_S21_days_dynamic_branching_fixJOH.xlsx")
openxlsx::write.xlsx(x = qualities_00_dynamic_reverse$conc, file = "qualities_hours_2017-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,2.xlsx")

# save RDS for flows and qualities
#saveRDS(flows_dynamic, file = "flows_hours_2002-2022.Rds")
saveRDS(qualities_00_dynamic_reverse, file = "qualities_hours_2016-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,20.Rds")

# Read RDS for flows and qualities
qualities_00_dynamic_reverse <- readRDS(file = "qualities_hours_2016-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,20.Rds")
flows_dynamic <- readRDS(file = "flows_hours_2002-2022.Rds")


#############################################################################################
### prepare qsimVis output for map visualisation ############################################
#############################################################################################

if(FALSE) {

  qsimVis_input <- kwb.BerlinWaterModel.public::prepare_qsimVis_input(
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
                  #, Fluoranthen.mg.m3
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

