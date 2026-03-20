# Tutorial

## Preparation

### Model Configuration and Network

``` r
library(kwb.BerlinWaterModel.public)

# read config files ##########################################################
config_dir <- system.file("extdata/config/network_complete_mean-start-conc",
                          package = "kwb.BerlinWaterModel.public")

config <- kwb.BerlinWaterModel.public::config_read(config_dir = config_dir)
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> Rows: 2 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (4): backflow_id, section_name, section_backflow_out_id, section_backflo...
#> dbl (1): section_backflow_out_share
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 5 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> dbl (7): type, equation_a, equation_b, r2, rmse, Q_min_m3d, Q_max_m3d
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 49 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (2): waterworks, gallery
#> dbl (1): type
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 8 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (2): flow_type, id_base
#> dbl (1): conc_Fluoranthen.ug.L
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 176 Columns: 15
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (6): Bemerkung, ds_link_id, surface_water, section_id, section_name, CSO_id
#> dbl (9): water_body_km, y-coordinate, x-coordinate, lat_grad, lat_min, lat_s...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 101 Columns: 12
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (8): section_id, flow_id, flow_name, rainstation_id, is_mar, flow_in_out...
#> dbl (4): rain_runoff_coefficient, rain_EZG_area_m2, ABIMO_runoff_m3/a, bank_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 2 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (1): substance_name
#> dbl (2): k, bf_reduction
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 14 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (9): outflow_id, section_name, section_in_id, section_out_id, section_ou...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 43 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (3): section_id, GewaesserId, comment
#> dbl (2): start_km, Km
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 24 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (3): flow_category, flow_id, flow_type
#> dbl (1): scaling_factor
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
#> 
#> Rows: 36 Columns: 16
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr  (6): section_id, section_name, section_in_id, section_out_id, no_neg_fl...
#> dbl (10): volume_m3, area_m2, branch_id, conc_tracer.cso, conc_tracer.inlet,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

config <- kwb.BerlinWaterModel.public::add_rain_direct_and_evaporation(config)
#> Adding direct rain and potential evaaporation to 'config$flows_in_out' for 9 sections (S01, S02, S03, S04, H03, H05, H06, H08, H09) ... ok. (0.01 secs)

config <- kwb.BerlinWaterModel.public::add_tracers(config)
#> Adding tracers for tracking of the following sources: CSO, inlet, rain_runoff, rain_direct, ww_discharge, WWTP ... ok. (0.01 secs)

#config <- kwb.BerlinWaterModel.public::add_substances(config)

network <- kwb.BerlinWaterModel.public::prepare_network(config)
```

### Visualise Network

``` r

### Network graph
net_complex <- kwb.BerlinWaterModel.public::plot_network_complex(
  network,
  config,
  show_labels = TRUE)
#> Links is a tbl_df. Converting to a plain data frame.
#> Nodes is a tbl_df. Converting to a plain data frame.

net_complex
```

``` r

## Export network
htmlwidgets::saveWidget(net_complex, file = "water-cycle_complex.html")
```

### Prepare Simulation

``` r
temporal_resolution <- "days" # "hours" or "days"

col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                               "datetime",
                               "date")

inputs <- kwb.BerlinWaterModel.public::add_scenario(
  config = config,
  use_scenario = FALSE, # should scaling factors be used (in config$scenarios) or not?
  debug = TRUE)
#> No scenario selected. Using datasets (i.e. 'cso', 'evapo_p', 'inflows', 'rain', 'ww', 'wwtp') provided with R packag 'kwb.BerlinWaterModel'.

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
#> Adding DWD rain data and aggregate from 'hourly' to 'daily' values and convert from 'mm/h' to 'm3/s' ... ok. (0.01 secs) 
#> Checking multiple outflows (i.e. 'share_Panke_to_BSSKanal', 'share_Panke_to_Nordgraben') time series for section 'Panke' (1/1) ... ok. (0.00 secs)
```

## Run Simulation

### Water Quantity

``` r
system.time(
  flows_dynamic <- kwb.BerlinWaterModel.public::calculate_flows_auto(
    config = config,
    input_list = input_list,
    network = network,
    use_dynamic = TRUE, # FALSE: static values for flow shares at river branchings
    debug = TRUE)
)
#> Calculating flow for section 'Müggelsee' ... ok. (0.10 secs) 
#> Calculating flow for section 'Seddinsee und Gosener Kanal' ... ok. (0.10 secs) 
#> Calculating flow for section 'Erpe (NH-MF)' ... ok. (0.10 secs) 
#> Calculating flow for section 'Langer See (Dahme)' ... ok. (0.09 secs) 
#> Calculating flow for section 'Müggelspree 2' ... ok. (0.10 secs) 
#> Calculating flow for section 'Dahme' ... ok. (0.09 secs) 
#> Calculating flow for section 'MHG-Graben' ... ok. (0.10 secs) 
#> Calculating flow for section 'Müggelspree' ... ok. (0.09 secs) 
#> Calculating flow for section 'Wuhle' ... ok. (0.09 secs) 
#> Calculating flow for section 'Rummelsburger See' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 1' ...
#> use dynamic & function for flow from section 'Dahme' to 'Spree 1'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Britzer Verbindungskanal' ...
#> use dynamic & function for flow from section 'Spree 1' to 'Britzer Verbindungskanal'
#> ok. (0.13 secs) 
#> Calculating flow for section 'Rudower Teltowkanal 1' ...
#> use dynamic & function for flow from section 'Dahme' to 'Rudower Teltowkanal 1'
#> ok. (0.14 secs) 
#> Calculating flow for section 'Spree 2' ...
#> use dynamic & function for flow from section 'Spree 1' to 'Spree 2'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Neuköllner SF-Kanal' ...
#> use dynamic & function for flow from section 'Britzer Verbindungskanal' to 'Neuköllner SF-Kanal'
#> ok. (0.14 secs) 
#> Calculating flow for section 'Panke' ... ok. (0.10 secs) 
#> Calculating flow for section 'Rudower Teltowkanal 2' ... ok. (0.10 secs) 
#> Calculating flow for section 'Spree 3' ...
#> use dynamic & static value for flow from section 'Spree 2' to 'Spree 3'
#> ok. (0.12 secs) 
#> Calculating flow for section 'BSS-Kanal' ...
#> use dynamic & function for flow from section 'Spree 3' to 'BSS-Kanal'
#> use dynamic & time series for flow from section 'Panke' to 'BSS-Kanal'
#> ok. (0.15 secs) 
#> Calculating flow for section 'Landwehrkanal' ...
#> use dynamic & static value for flow from section 'Spree 2' to 'Landwehrkanal'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Nordgraben' ...
#> use dynamic & time series for flow from section 'Panke' to 'Nordgraben'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Oberhavel 1' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 4' ...
#> use dynamic & function for flow from section 'Spree 3' to 'Spree 4'
#> ok. (0.13 secs) 
#> Calculating flow for section 'Tegeler Fließ' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 1' ...
#> use dynamic & function for flow from section 'Britzer Verbindungskanal' to 'Teltowkanal 1'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Oberhavel 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 5' ... ok. (0.09 secs) 
#> Calculating flow for section 'Tegeler See' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Oberhavel 3' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 6' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 3' ... ok. (0.09 secs) 
#> Calculating flow for section 'Unterhavel 1' ... ok. (0.10 secs) 
#> Calculating flow for section 'Wannsee' ...
#> use dynamic & function for flow from section 'Teltowkanal 3' to 'Wannsee'
#> ok. (0.14 secs) 
#> Calculating flow for section 'Unterhavel 2' ... ok. (0.10 secs)
#>    user  system elapsed 
#>    6.38    0.07    6.49

### Analyse Results
flows_stats <- kwb.BerlinWaterModel.public::calculate_flow_stats(
  flows = flows_dynamic)
#> Berechnung der Gewaesserkennwerte aus den Tageswerten.

DT::datatable(flows_stats$per_section, caption = "Flow stats (per section)")
```

``` r
DT::datatable(flows_stats$per_year, caption = "Flow stats (per year)")
```

``` r
DT::datatable(flows_stats$per_month, caption = "Flow stats (per month)")
```

``` r

flows_dynamic_neg_flows_stat <- kwb.BerlinWaterModel.public::get_reverse_flows_per_section(
  flows = flows_dynamic)

DT::datatable(flows_dynamic_neg_flows_stat, caption = "Sections with negative flows in 2019")
```

### Water Quality

``` r

system.time(
  qualities_00_dynamic_reverse <- kwb.BerlinWaterModel.public::calculate_qualities(input_list = input_list,
                                                                    flows = flows_dynamic,
                                                                    network = network,
                                                                    config = config,
                                                                    branchwise = TRUE,
                                                                    reverse_flow = TRUE, # use FALSE for calculation without considering backflows
                                                                    debug = FALSE)
)
#> ############### Calculating branch 1 / 5
#> Calculating water quality 'forward' for section S01: Seddinsee und Gosener Kanal
#> Calculating water quality 'forward' for section S02: Langer See (Dahme)
#> Calculating water quality 'forward' for section S03: Dahme
#> Calculating water quality 'forward' for section S21a: Rudower Teltowkanal 1
#> Calculating water quality 'forward' for section S21: Rudower Teltowkanal 2
#> (1/2): Calculating water quality 'backward' for section Rudower Teltowkanal 1 (S21a) in branch_id 1
#> (2/2): Calculating water quality 'backward' for section Dahme (S03) in branch_id 1
#> Re-Calculating water quality 'forward' for branch_id: 1 (neg. flows in branch), section 'S21: Rudower Teltowkanal 2'
#> ############### Calculating branch 2 / 5
#> Calculating water quality 'forward' for section S04: Müggelsee
#> Calculating water quality 'forward' for section S06: Erpe (NH-MF)
#> Calculating water quality 'forward' for section S05a: Müggelspree 2
#> Calculating water quality 'forward' for section S10: MHG-Graben
#> Calculating water quality 'forward' for section S05: Müggelspree
#> Calculating water quality 'forward' for section S08: Wuhle
#> Calculating water quality 'forward' for section S11: Rummelsburger See
#> Calculating water quality 'forward' for section S07: Spree 1
#> Calculating water quality 'forward' for section S12: Britzer Verbindungskanal
#> Calculating water quality 'forward' for section S09: Spree 2
#> Calculating water quality 'forward' for section S13: Neuköllner SF-Kanal
#> Calculating water quality 'forward' for section S15: Spree 3
#> Calculating water quality 'forward' for section S22: Teltowkanal 1
#> Calculating water quality 'forward' for section S22a: Teltowkanal 2
#> (1/6): Calculating water quality 'backward' for section Britzer Verbindungskanal (S12) in branch_id 2
#> (2/6): Calculating water quality 'backward' for section Spree 1 (S07) in branch_id 2
#> (3/6): Calculating water quality 'backward' for section Müggelspree (S05) in branch_id 2
#> (4/6): Calculating water quality 'backward' for section Müggelspree 2 (S05a) in branch_id 2
#> (5/6): Calculating water quality 'backward' for section Erpe (NH-MF) (S06) in branch_id 2
#> (6/6): Calculating water quality 'backward' for section Müggelsee (S04) in branch_id 2
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S10: MHG-Graben'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S08: Wuhle'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S11: Rummelsburger See'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S09: Spree 2'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S13: Neuköllner SF-Kanal'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S15: Spree 3'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S22: Teltowkanal 1'
#> Re-Calculating water quality 'forward' for branch_id: 2 (neg. flows in branch), section 'S22a: Teltowkanal 2'
#> ############### Calculating branch 3 / 5
#> Calculating water quality 'forward' for section S16: Panke
#> Calculating water quality 'forward' for section H01: Nordgraben
#> Calculating water quality 'forward' for section H04: Oberhavel 1
#> Calculating water quality 'forward' for section H02: Tegeler Fließ
#> Calculating water quality 'forward' for section H05: Oberhavel 2
#> Calculating water quality 'forward' for section H03: Tegeler See
#> Calculating water quality 'forward' for section H06: Oberhavel 3
#> (1/3): Calculating water quality 'backward' for section Tegeler See (H03) in branch_id 3
#> (2/3): Calculating water quality 'backward' for section Oberhavel 2 (H05) in branch_id 3
#> (3/3): Calculating water quality 'backward' for section Oberhavel 1 (H04) in branch_id 3
#> Re-Calculating water quality 'forward' for branch_id: 3 (neg. flows in branch), section 'H02: Tegeler Fließ'
#> Re-Calculating water quality 'forward' for branch_id: 3 (neg. flows in branch), section 'H06: Oberhavel 3'
#> ############### Calculating branch 4 / 5
#> Calculating water quality 'forward' for section S18: BSS-Kanal
#> Calculating water quality 'forward' for section S14: Landwehrkanal
#> Calculating water quality 'forward' for section S17: Spree 4
#> Calculating water quality 'forward' for section S19: Spree 5
#> Calculating water quality 'forward' for section S20: Spree 6
#> ############### Calculating branch 5 / 5
#> Calculating water quality 'forward' for section S23: Teltowkanal 3
#> Calculating water quality 'forward' for section H07: Unterhavel 1
#> Calculating water quality 'forward' for section H09: Wannsee
#> Calculating water quality 'forward' for section H08: Unterhavel 2
#> Calculating water quality 'forward' for section Out: Outflow Potsdamer Havel
#> (1/1): Calculating water quality 'backward' for section Wannsee (H09) in branch_id 5
#> Re-Calculating water quality 'forward' for branch_id: 5 (neg. flows in branch), section 'H08: Unterhavel 2'
#> Re-Calculating water quality 'forward' for branch_id: 5 (neg. flows in branch), section 'Out: Outflow Potsdamer Havel'
#>    user  system elapsed 
#>   15.01    0.28   15.38

### Save qualities for all sections in XLSX
openxlsx::write.xlsx(x = qualities_00_dynamic_reverse$conc, file = "qualities_00_dynamic_reverse_concentrations.xlsx")

# save RDS for flows and qualities
saveRDS(flows_dynamic, file = "flows_dynamic.Rds")
saveRDS(qualities_00_dynamic_reverse, file = "qualities_00_dynamic_reverse_concentrations.Rds")
```
