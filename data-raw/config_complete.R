#remotes::install_github("kwb-r/kwb.BerlinWaterModel.public")
library(kwb.BerlinWaterModel.public)

# read config files ##########################################################

config <- kwb.BerlinWaterModel.public::config_read(config_dir = "inst/extdata/config/network_complete")

config <- kwb.BerlinWaterModel.public::add_rain_direct_and_evaporation(config)

config <- kwb.BerlinWaterModel.public::add_tracers(config)

#config <- kwb.BerlinWaterModel.public::add_substances(config)

network <- kwb.BerlinWaterModel.public::prepare_network(config)


# plot interactive network map ###############################################

### Network graph
net_complex <- kwb.BerlinWaterModel.public::plot_network_complex(
  network,
  config,
  show_labels = TRUE)

htmlwidgets::saveWidget(net_complex, file = "water-cycle_complex.html")


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
                                                  date_min = "2019-01-01",
                                                  date_max = "2019-12-31")

################################################################################################
### Calculate flows ############################################################################
################################################################################################

system.time(
  flows_dynamic <- kwb.BerlinWaterModel.public::calculate_flows_auto(
    config = config,
    input_list = input_list,
    network = network,
    use_dynamic = TRUE, # FALSE: static values for flow shares at river branchings
    debug = TRUE)
)

flows_stats <- kwb.BerlinWaterModel.public::calculate_flow_stats(
  flows = flows_dynamic)

DT::datatable(flows_stats$per_section, caption = "Flow stats (per section)")
DT::datatable(flows_stats$per_year, caption = "Flow stats (per year)")
DT::datatable(flows_stats$per_month, caption = "Flow stats (per month)")

flows_dynamic_neg_flows_stat <- kwb.BerlinWaterModel.public::get_reverse_flows_per_section(
  flows = flows_dynamic)

DT::datatable(flows_dynamic_neg_flows_stat, caption = "Sections with negative flows in 2019")


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

### Save qualities for all sections in XLSX
openxlsx::write.xlsx(x = qualities_00_dynamic_reverse$conc, file = "qualities_00_dynamic_reverse_concentrations.xlsx")

# save RDS for flows and qualities

saveRDS(qualities_00_dynamic_reverse, file = "qualities_00_dynamic_reverse_concentrations.Rds")


#############################################################################################
### Diverse checks ##########################################################################
#############################################################################################

# Add check to compare modeled outflow to balance corrected input data
out_hydrax <- input_list$flows$PHv_0
out_model <- flows_dynamic$Out
median(out_model/out_hydrax)

