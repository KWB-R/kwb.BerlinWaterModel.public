# Abstractions per Section

## Prepare Model

``` r
### Install R package from "dev" branch
# remotes::install_github("kwb-r/kwb.BerlinWaterModel@dev")


library(kwb.BerlinWaterModel.public)

config <- kwb.BerlinWaterModel.public::config_read()
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
#> dbl (1): conc_Valsartan.ug.L
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
#> Rows: 105 Columns: 12
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
#> Rows: 36 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ";"
#> chr (6): section_id, section_name, section_in_id, section_out_id, no_neg_flo...
#> dbl (3): volume_m3, area_m2, branch_id
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

config <- kwb.BerlinWaterModel.public::add_tracers(config)
#> Adding tracers for tracking of the following sources: CSO, inlet, rain_runoff, rain_direct, ww_discharge, WWTP ...
#> Section starting concentrations missing for: cso, inlet, rain_runoff, rain_direct, ww_discharge, wwtp
#> 0 will be assumed as starting concentration for all 'sections'!
#> ok. (0.01 secs)

config <- kwb.BerlinWaterModel.public::add_substances(config)
#> Section starting concentrations missing for: conc_Valsartan.mg.m3
#> 0 will be assumed as starting concentration for all 'sections'!

network <- kwb.BerlinWaterModel.public::prepare_network(config)

temporal_resolution <- "days" # "hours" or "days"


### scenario definition: reduction of inlet flows based on file config/scenarios.csv
inflows <- kwb.BerlinWaterModel.public::inflows %>%
  dplyr::left_join(config$scenarios %>%
                     dplyr::select(flow_id, scaling_factor) %>%
                     dplyr::rename(id = flow_id) %>%
                     dplyr::mutate(scaling_factor = as.numeric(scaling_factor)),
                   by = "id") %>%
  dplyr::mutate(cbm_per_second = dplyr::if_else(!is.na(scaling_factor),
                                                cbm_per_second * scaling_factor,
                                                cbm_per_second)) %>%
  dplyr::select(- scaling_factor)

### Prepare input for "static" BF-shares
input_list_bfs_static <- kwb.BerlinWaterModel.public::prepare_input(temporal_resolution = temporal_resolution,
                                             config = config,
                                             cso = cso,
                                             inflows = inflows,
                                             share_wwtp_sch_panke_1 = 0.1,
                                             date_separation_panke_1_2 = "2015-06-30",
                                             share_wwtp_sch_panke_2 = 0.9,
                                             rain = rain,
                                             ww = ww,
                                             wwtp = wwtp,
                                             bfshare_dynamic = FALSE,
                                             date_min = "2002-01-01",
                                             date_max = "2022-12-31")
#> Adding DWD rain data and aggregate from 'hourly' to 'daily' values and convert from 'mm/h' to 'm3/s' ... ok. (0.05 secs) 
#> Checking multiple outflows (i.e. 'share_Panke_to_BSSKanal', 'share_Panke_to_Nordgraben') time series for section 'Panke' (1/1) ... ok. (0.00 secs)


### Prepare input for "dynamic" BF-shares
input_list_bfs_dynamic <- kwb.BerlinWaterModel.public::prepare_input(temporal_resolution = temporal_resolution,
                                                        config = config,
                                                        cso = cso,
                                                        inflows = inflows,
                                                        share_wwtp_sch_panke_1 = 0.1,
                                                        date_separation_panke_1_2 = "2015-06-30",
                                                        share_wwtp_sch_panke_2 = 0.9,
                                                        rain = rain,
                                                        ww = ww,
                                                        wwtp = wwtp,
                                                        bfshare_dynamic = TRUE,
                                                        date_min = "2002-01-01",
                                                        date_max = "2022-12-31")
#> Adding DWD rain data and aggregate from 'hourly' to 'daily' values and convert from 'mm/h' to 'm3/s' ... ok. (0.05 secs) 
#> Checking multiple outflows (i.e. 'share_Panke_to_BSSKanal', 'share_Panke_to_Nordgraben') time series for section 'Panke' (1/1) ... ok. (0.00 secs)
```

## Run Model

``` r

### Run model for "static" BF-shares
system.time(
  flows_bfs_static <- kwb.BerlinWaterModel.public::calculate_flows_auto(config = config,
                                                      input_list = input_list_bfs_static,
                                                      network = network,
                                                      use_dynamic = TRUE,
                                                      debug = TRUE)
)
#> Calculating flow for section 'Müggelsee' ... ok. (0.09 secs) 
#> Calculating flow for section 'Seddinsee und Gosener Kanal' ... ok. (0.09 secs) 
#> Calculating flow for section 'Erpe (NH-MF)' ... ok. (0.09 secs) 
#> Calculating flow for section 'Langer See (Dahme)' ... ok. (0.09 secs) 
#> Calculating flow for section 'Müggelspree 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Dahme' ... ok. (0.09 secs) 
#> Calculating flow for section 'MHG-Graben' ... ok. (0.09 secs) 
#> Calculating flow for section 'Müggelspree' ... ok. (0.08 secs) 
#> Calculating flow for section 'Wuhle' ... ok. (0.09 secs) 
#> Calculating flow for section 'Rummelsburger See' ... ok. (0.08 secs) 
#> Calculating flow for section 'Spree 1' ...
#> use dynamic & function for flow from section 'Dahme' to 'Spree 1'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Britzer Verbindungskanal' ...
#> use dynamic & function for flow from section 'Spree 1' to 'Britzer Verbindungskanal'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Rudower Teltowkanal 1' ...
#> use dynamic & function for flow from section 'Dahme' to 'Rudower Teltowkanal 1'
#> ok. (0.11 secs) 
#> Calculating flow for section 'Spree 2' ...
#> use dynamic & function for flow from section 'Spree 1' to 'Spree 2'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Neuköllner SF-Kanal' ...
#> use dynamic & function for flow from section 'Britzer Verbindungskanal' to 'Neuköllner SF-Kanal'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Panke' ... ok. (0.09 secs) 
#> Calculating flow for section 'Rudower Teltowkanal 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 3' ...
#> use dynamic & static value for flow from section 'Spree 2' to 'Spree 3'
#> ok. (0.11 secs) 
#> Calculating flow for section 'BSS-Kanal' ...
#> use dynamic & function for flow from section 'Spree 3' to 'BSS-Kanal'
#> use dynamic & time series for flow from section 'Panke' to 'BSS-Kanal'
#> ok. (0.15 secs) 
#> Calculating flow for section 'Landwehrkanal' ...
#> use dynamic & static value for flow from section 'Spree 2' to 'Landwehrkanal'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Nordgraben' ...
#> use dynamic & time series for flow from section 'Panke' to 'Nordgraben'
#> ok. (0.11 secs) 
#> Calculating flow for section 'Oberhavel 1' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 4' ...
#> use dynamic & function for flow from section 'Spree 3' to 'Spree 4'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Tegeler Fließ' ... ok. (0.08 secs) 
#> Calculating flow for section 'Teltowkanal 1' ...
#> use dynamic & function for flow from section 'Britzer Verbindungskanal' to 'Teltowkanal 1'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Oberhavel 2' ... ok. (0.10 secs) 
#> Calculating flow for section 'Spree 5' ... ok. (0.11 secs) 
#> Calculating flow for section 'Tegeler See' ... ok. (0.10 secs) 
#> Calculating flow for section 'Teltowkanal 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Oberhavel 3' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 6' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 3' ... ok. (0.09 secs) 
#> Calculating flow for section 'Unterhavel 1' ... ok. (0.09 secs) 
#> Calculating flow for section 'Wannsee' ...
#> use dynamic & function for flow from section 'Teltowkanal 3' to 'Wannsee'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Unterhavel 2' ... ok. (0.09 secs)
#>    user  system elapsed 
#>    5.92    0.22    6.17


### Run model for "dynamic" BF-shares
system.time(
  flows_bfs_dynamic <- kwb.BerlinWaterModel.public::calculate_flows_auto(config = config,
                                                                 input_list = input_list_bfs_dynamic,
                                                                 network = network,
                                                                 use_dynamic = TRUE,
                                                                 debug = TRUE)
)
#> Calculating flow for section 'Müggelsee' ... ok. (0.08 secs) 
#> Calculating flow for section 'Seddinsee und Gosener Kanal' ... ok. (0.09 secs) 
#> Calculating flow for section 'Erpe (NH-MF)' ... ok. (0.09 secs) 
#> Calculating flow for section 'Langer See (Dahme)' ... ok. (0.09 secs) 
#> Calculating flow for section 'Müggelspree 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Dahme' ... ok. (0.09 secs) 
#> Calculating flow for section 'MHG-Graben' ... ok. (0.09 secs) 
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
#> ok. (0.12 secs) 
#> Calculating flow for section 'Spree 2' ...
#> use dynamic & function for flow from section 'Spree 1' to 'Spree 2'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Neuköllner SF-Kanal' ...
#> use dynamic & function for flow from section 'Britzer Verbindungskanal' to 'Neuköllner SF-Kanal'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Panke' ... ok. (0.09 secs) 
#> Calculating flow for section 'Rudower Teltowkanal 2' ... ok. (0.08 secs) 
#> Calculating flow for section 'Spree 3' ...
#> use dynamic & static value for flow from section 'Spree 2' to 'Spree 3'
#> ok. (0.12 secs) 
#> Calculating flow for section 'BSS-Kanal' ...
#> use dynamic & function for flow from section 'Spree 3' to 'BSS-Kanal'
#> use dynamic & time series for flow from section 'Panke' to 'BSS-Kanal'
#> ok. (0.15 secs) 
#> Calculating flow for section 'Landwehrkanal' ...
#> use dynamic & static value for flow from section 'Spree 2' to 'Landwehrkanal'
#> ok. (0.11 secs) 
#> Calculating flow for section 'Nordgraben' ...
#> use dynamic & time series for flow from section 'Panke' to 'Nordgraben'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Oberhavel 1' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 4' ...
#> use dynamic & function for flow from section 'Spree 3' to 'Spree 4'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Tegeler Fließ' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 1' ...
#> use dynamic & function for flow from section 'Britzer Verbindungskanal' to 'Teltowkanal 1'
#> ok. (0.12 secs) 
#> Calculating flow for section 'Oberhavel 2' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 5' ... ok. (0.09 secs) 
#> Calculating flow for section 'Tegeler See' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 2' ... ok. (0.08 secs) 
#> Calculating flow for section 'Oberhavel 3' ... ok. (0.09 secs) 
#> Calculating flow for section 'Spree 6' ... ok. (0.09 secs) 
#> Calculating flow for section 'Teltowkanal 3' ... ok. (0.09 secs) 
#> Calculating flow for section 'Unterhavel 1' ... ok. (0.09 secs) 
#> Calculating flow for section 'Wannsee' ...
#> use dynamic & function for flow from section 'Teltowkanal 3' to 'Wannsee'
#> ok. (0.11 secs) 
#> Calculating flow for section 'Unterhavel 2' ... ok. (0.10 secs)
#>    user  system elapsed 
#>    5.94    0.14    6.19
```

## Plot Results

``` r

prepare_plot <- function(flows, 
                         combine_plots = FALSE, 
                         scale_factor = 1, 
                         landscape = TRUE,  
                         width.cm = NULL, 
                         height.cm = NULL,
                         ...) {

pdff <- sprintf("flows-and-bfshares-per-section_bfshare-%s_multiple-outflows-%s_%s_%s_scale-factor-%s.pdf",
                ifelse(attr(flows, "bfshare_dynamic"), "dynamic-equations-new", "static"),
                ifelse(attr(flows, "use_dynamic"), "dynamic", "static"),
                min(flows$date),
                max(flows$date),
                stringr::str_replace(scale_factor, "\\.", "-"))

kwb.utils::preparePdf(pdff, landscape = landscape, width.cm = width.cm, height.cm = height.cm)

plots <- kwb.BerlinWaterModel.public::plot_flows_and_bfshares_per_section(config = config,
                                                                flows = flows,
                                                                network = network,
                                                                ww = ww,
                                                                scale_factor = scale_factor,
                                                                debug = TRUE,
                                                                ...)

plots <- plots[!sapply(plots, is.null)]


if (combine_plots) {
    n_per_page <- 2
    n_pages <- ceiling(length(plots) / n_per_page)
    for (i in seq_len(n_pages)) {
      gridExtra::grid.arrange(
        grobs = plots[((i - 1) * n_per_page + 1):min(i * n_per_page, length(plots))],
        ncol = 1, nrow = 2
      )
    }
  } else {
    # jeweils einzeln
    print(plots)
  }

dev.off()
#kwb.utils::finishAndShowPdf(pdff)

pdff
}

if(TRUE) {
scale_factors <- c(1,1.1,1.2)
sapply(scale_factors, function(scale_factor) {
  
pdff_dynamic <- prepare_plot(flows = flows_bfs_dynamic,
                             combine_plots = FALSE,
                             scale_factor = scale_factor,
                             landscape = TRUE)

pdff_static <- prepare_plot(flows = flows_bfs_static, 
                             combine_plots = FALSE, 
                             scale_factor = scale_factor,
                             landscape = TRUE)
})
}
#> Running for id 1/35 (S04: Müggelsee) ... ok. (0.42 secs) 
#> Running for id 2/35 (S01: Seddinsee und Gosener Kanal) ... ok. (0.38 secs) 
#> Running for id 3/35 (S06: Erpe (NH-MF)) ... ok. (0.35 secs) 
#> Running for id 4/35 (S02: Langer See (Dahme)) ... ok. (0.37 secs) 
#> Running for id 5/35 (S05a: Müggelspree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree 2 (S05a)
#> ok. (0.00 secs) 
#> Running for id 6/35 (S03: Dahme) ...
#> No outflow to well gallery or managed aquifer recharge for section Dahme (S03)
#> ok. (0.00 secs) 
#> Running for id 7/35 (S10: MHG-Graben) ...
#> No outflow to well gallery or managed aquifer recharge for section MHG-Graben (S10)
#> ok. (0.00 secs) 
#> Running for id 8/35 (S05: Müggelspree) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree (S05)
#> ok. (0.00 secs) 
#> Running for id 9/35 (S08: Wuhle) ... ok. (0.39 secs) 
#> Running for id 10/35 (S11: Rummelsburger See) ...
#> No outflow to well gallery or managed aquifer recharge for section Rummelsburger See (S11)
#> ok. (0.00 secs) 
#> Running for id 11/35 (S07: Spree 1) ... ok. (0.36 secs) 
#> Running for id 12/35 (S12: Britzer Verbindungskanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Britzer Verbindungskanal (S12)
#> ok. (0.00 secs) 
#> Running for id 13/35 (S21a: Rudower Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 1 (S21a)
#> ok. (0.00 secs) 
#> Running for id 14/35 (S09: Spree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 2 (S09)
#> ok. (0.00 secs) 
#> Running for id 15/35 (S13: Neuköllner SF-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Neuköllner SF-Kanal (S13)
#> ok. (0.00 secs) 
#> Running for id 16/35 (S16: Panke) ...
#> No outflow to well gallery or managed aquifer recharge for section Panke (S16)
#> ok. (0.00 secs) 
#> Running for id 17/35 (S21: Rudower Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 2 (S21)
#> ok. (0.00 secs) 
#> Running for id 18/35 (S15: Spree 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 3 (S15)
#> ok. (0.00 secs) 
#> Running for id 19/35 (S18: BSS-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section BSS-Kanal (S18)
#> ok. (0.00 secs) 
#> Running for id 20/35 (S14: Landwehrkanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Landwehrkanal (S14)
#> ok. (0.00 secs) 
#> Running for id 21/35 (H01: Nordgraben) ...
#> No outflow to well gallery or managed aquifer recharge for section Nordgraben (H01)
#> ok. (0.00 secs) 
#> Running for id 22/35 (H04: Oberhavel 1) ... ok. (0.40 secs) 
#> Running for id 23/35 (S17: Spree 4) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 4 (S17)
#> ok. (0.00 secs) 
#> Running for id 24/35 (H02: Tegeler Fließ) ...
#> No outflow to well gallery or managed aquifer recharge for section Tegeler Fließ (H02)
#> ok. (0.00 secs) 
#> Running for id 25/35 (S22: Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 1 (S22)
#> ok. (0.00 secs) 
#> Running for id 26/35 (H05: Oberhavel 2) ... ok. (0.46 secs) 
#> Running for id 27/35 (S19: Spree 5) ... ok. (0.45 secs) 
#> Running for id 28/35 (H03: Tegeler See) ... ok. (0.51 secs) 
#> Running for id 29/35 (S22a: Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 2 (S22a)
#> ok. (0.00 secs) 
#> Running for id 30/35 (H06: Oberhavel 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Oberhavel 3 (H06)
#> ok. (0.00 secs) 
#> Running for id 31/35 (S20: Spree 6) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 6 (S20)
#> ok. (0.00 secs) 
#> Running for id 32/35 (S23: Teltowkanal 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 3 (S23)
#> ok. (0.00 secs) 
#> Running for id 33/35 (H07: Unterhavel 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Unterhavel 1 (H07)
#> ok. (0.00 secs) 
#> Running for id 34/35 (H09: Wannsee) ... ok. (0.45 secs) 
#> Running for id 35/35 (H08: Unterhavel 2) ... ok. (0.49 secs) 
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
#> [[4]]
#> 
#> [[5]]
#> 
#> [[6]]
#> 
#> [[7]]
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[8]]
#> Warning: Removed 13 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 13 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[9]]
#> 
#> [[10]]
#> 
#> [[11]]
#> Warning: Removed 36 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 36 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[12]]
#> Warning: Removed 1 row containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> Running for id 1/35 (S04: Müggelsee) ... ok. (0.42 secs) 
#> Running for id 2/35 (S01: Seddinsee und Gosener Kanal) ... ok. (0.39 secs) 
#> Running for id 3/35 (S06: Erpe (NH-MF)) ... ok. (0.37 secs) 
#> Running for id 4/35 (S02: Langer See (Dahme)) ... ok. (0.38 secs) 
#> Running for id 5/35 (S05a: Müggelspree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree 2 (S05a)
#> ok. (0.00 secs) 
#> Running for id 6/35 (S03: Dahme) ...
#> No outflow to well gallery or managed aquifer recharge for section Dahme (S03)
#> ok. (0.00 secs) 
#> Running for id 7/35 (S10: MHG-Graben) ...
#> No outflow to well gallery or managed aquifer recharge for section MHG-Graben (S10)
#> ok. (0.00 secs) 
#> Running for id 8/35 (S05: Müggelspree) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree (S05)
#> ok. (0.00 secs) 
#> Running for id 9/35 (S08: Wuhle) ... ok. (0.42 secs) 
#> Running for id 10/35 (S11: Rummelsburger See) ...
#> No outflow to well gallery or managed aquifer recharge for section Rummelsburger See (S11)
#> ok. (0.00 secs) 
#> Running for id 11/35 (S07: Spree 1) ... ok. (0.38 secs) 
#> Running for id 12/35 (S12: Britzer Verbindungskanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Britzer Verbindungskanal (S12)
#> ok. (0.00 secs) 
#> Running for id 13/35 (S21a: Rudower Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 1 (S21a)
#> ok. (0.00 secs) 
#> Running for id 14/35 (S09: Spree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 2 (S09)
#> ok. (0.00 secs) 
#> Running for id 15/35 (S13: Neuköllner SF-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Neuköllner SF-Kanal (S13)
#> ok. (0.00 secs) 
#> Running for id 16/35 (S16: Panke) ...
#> No outflow to well gallery or managed aquifer recharge for section Panke (S16)
#> ok. (0.00 secs) 
#> Running for id 17/35 (S21: Rudower Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 2 (S21)
#> ok. (0.00 secs) 
#> Running for id 18/35 (S15: Spree 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 3 (S15)
#> ok. (0.00 secs) 
#> Running for id 19/35 (S18: BSS-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section BSS-Kanal (S18)
#> ok. (0.00 secs) 
#> Running for id 20/35 (S14: Landwehrkanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Landwehrkanal (S14)
#> ok. (0.00 secs) 
#> Running for id 21/35 (H01: Nordgraben) ...
#> No outflow to well gallery or managed aquifer recharge for section Nordgraben (H01)
#> ok. (0.00 secs) 
#> Running for id 22/35 (H04: Oberhavel 1) ... ok. (0.59 secs) 
#> Running for id 23/35 (S17: Spree 4) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 4 (S17)
#> ok. (0.00 secs) 
#> Running for id 24/35 (H02: Tegeler Fließ) ...
#> No outflow to well gallery or managed aquifer recharge for section Tegeler Fließ (H02)
#> ok. (0.00 secs) 
#> Running for id 25/35 (S22: Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 1 (S22)
#> ok. (0.00 secs) 
#> Running for id 26/35 (H05: Oberhavel 2) ... ok. (0.46 secs) 
#> Running for id 27/35 (S19: Spree 5) ... ok. (0.47 secs) 
#> Running for id 28/35 (H03: Tegeler See) ... ok. (0.49 secs) 
#> Running for id 29/35 (S22a: Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 2 (S22a)
#> ok. (0.00 secs) 
#> Running for id 30/35 (H06: Oberhavel 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Oberhavel 3 (H06)
#> ok. (0.00 secs) 
#> Running for id 31/35 (S20: Spree 6) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 6 (S20)
#> ok. (0.00 secs) 
#> Running for id 32/35 (S23: Teltowkanal 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 3 (S23)
#> ok. (0.00 secs) 
#> Running for id 33/35 (H07: Unterhavel 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Unterhavel 1 (H07)
#> ok. (0.00 secs) 
#> Running for id 34/35 (H09: Wannsee) ... ok. (0.45 secs) 
#> Running for id 35/35 (H08: Unterhavel 2) ... ok. (0.44 secs) 
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
#> [[4]]
#> 
#> [[5]]
#> 
#> [[6]]
#> 
#> [[7]]
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[8]]
#> Warning: Removed 13 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 13 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[9]]
#> 
#> [[10]]
#> 
#> [[11]]
#> Warning: Removed 36 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 36 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[12]]
#> Warning: Removed 1 row containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> Running for id 1/35 (S04: Müggelsee) ... ok. (0.44 secs) 
#> Running for id 2/35 (S01: Seddinsee und Gosener Kanal) ... ok. (0.39 secs) 
#> Running for id 3/35 (S06: Erpe (NH-MF)) ... ok. (0.37 secs) 
#> Running for id 4/35 (S02: Langer See (Dahme)) ... ok. (0.37 secs) 
#> Running for id 5/35 (S05a: Müggelspree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree 2 (S05a)
#> ok. (0.00 secs) 
#> Running for id 6/35 (S03: Dahme) ...
#> No outflow to well gallery or managed aquifer recharge for section Dahme (S03)
#> ok. (0.00 secs) 
#> Running for id 7/35 (S10: MHG-Graben) ...
#> No outflow to well gallery or managed aquifer recharge for section MHG-Graben (S10)
#> ok. (0.00 secs) 
#> Running for id 8/35 (S05: Müggelspree) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree (S05)
#> ok. (0.00 secs) 
#> Running for id 9/35 (S08: Wuhle) ... ok. (0.40 secs) 
#> Running for id 10/35 (S11: Rummelsburger See) ...
#> No outflow to well gallery or managed aquifer recharge for section Rummelsburger See (S11)
#> ok. (0.00 secs) 
#> Running for id 11/35 (S07: Spree 1) ... ok. (0.42 secs) 
#> Running for id 12/35 (S12: Britzer Verbindungskanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Britzer Verbindungskanal (S12)
#> ok. (0.00 secs) 
#> Running for id 13/35 (S21a: Rudower Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 1 (S21a)
#> ok. (0.00 secs) 
#> Running for id 14/35 (S09: Spree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 2 (S09)
#> ok. (0.00 secs) 
#> Running for id 15/35 (S13: Neuköllner SF-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Neuköllner SF-Kanal (S13)
#> ok. (0.00 secs) 
#> Running for id 16/35 (S16: Panke) ...
#> No outflow to well gallery or managed aquifer recharge for section Panke (S16)
#> ok. (0.00 secs) 
#> Running for id 17/35 (S21: Rudower Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 2 (S21)
#> ok. (0.00 secs) 
#> Running for id 18/35 (S15: Spree 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 3 (S15)
#> ok. (0.00 secs) 
#> Running for id 19/35 (S18: BSS-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section BSS-Kanal (S18)
#> ok. (0.00 secs) 
#> Running for id 20/35 (S14: Landwehrkanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Landwehrkanal (S14)
#> ok. (0.00 secs) 
#> Running for id 21/35 (H01: Nordgraben) ...
#> No outflow to well gallery or managed aquifer recharge for section Nordgraben (H01)
#> ok. (0.00 secs) 
#> Running for id 22/35 (H04: Oberhavel 1) ... ok. (0.43 secs) 
#> Running for id 23/35 (S17: Spree 4) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 4 (S17)
#> ok. (0.00 secs) 
#> Running for id 24/35 (H02: Tegeler Fließ) ...
#> No outflow to well gallery or managed aquifer recharge for section Tegeler Fließ (H02)
#> ok. (0.00 secs) 
#> Running for id 25/35 (S22: Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 1 (S22)
#> ok. (0.00 secs) 
#> Running for id 26/35 (H05: Oberhavel 2) ... ok. (0.52 secs) 
#> Running for id 27/35 (S19: Spree 5) ... ok. (0.67 secs) 
#> Running for id 28/35 (H03: Tegeler See) ... ok. (0.56 secs) 
#> Running for id 29/35 (S22a: Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 2 (S22a)
#> ok. (0.00 secs) 
#> Running for id 30/35 (H06: Oberhavel 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Oberhavel 3 (H06)
#> ok. (0.00 secs) 
#> Running for id 31/35 (S20: Spree 6) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 6 (S20)
#> ok. (0.00 secs) 
#> Running for id 32/35 (S23: Teltowkanal 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 3 (S23)
#> ok. (0.00 secs) 
#> Running for id 33/35 (H07: Unterhavel 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Unterhavel 1 (H07)
#> ok. (0.00 secs) 
#> Running for id 34/35 (H09: Wannsee) ... ok. (0.52 secs) 
#> Running for id 35/35 (H08: Unterhavel 2) ... ok. (0.49 secs) 
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
#> [[4]]
#> 
#> [[5]]
#> 
#> [[6]]
#> 
#> [[7]]
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[8]]
#> Warning: Removed 13 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 13 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[9]]
#> 
#> [[10]]
#> 
#> [[11]]
#> Warning: Removed 36 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 36 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[12]]
#> Warning: Removed 1 row containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> Running for id 1/35 (S04: Müggelsee) ... ok. (0.52 secs) 
#> Running for id 2/35 (S01: Seddinsee und Gosener Kanal) ... ok. (0.59 secs) 
#> Running for id 3/35 (S06: Erpe (NH-MF)) ... ok. (0.52 secs) 
#> Running for id 4/35 (S02: Langer See (Dahme)) ... ok. (0.46 secs) 
#> Running for id 5/35 (S05a: Müggelspree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree 2 (S05a)
#> ok. (0.00 secs) 
#> Running for id 6/35 (S03: Dahme) ...
#> No outflow to well gallery or managed aquifer recharge for section Dahme (S03)
#> ok. (0.00 secs) 
#> Running for id 7/35 (S10: MHG-Graben) ...
#> No outflow to well gallery or managed aquifer recharge for section MHG-Graben (S10)
#> ok. (0.00 secs) 
#> Running for id 8/35 (S05: Müggelspree) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree (S05)
#> ok. (0.00 secs) 
#> Running for id 9/35 (S08: Wuhle) ... ok. (0.61 secs) 
#> Running for id 10/35 (S11: Rummelsburger See) ...
#> No outflow to well gallery or managed aquifer recharge for section Rummelsburger See (S11)
#> ok. (0.00 secs) 
#> Running for id 11/35 (S07: Spree 1) ... ok. (0.43 secs) 
#> Running for id 12/35 (S12: Britzer Verbindungskanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Britzer Verbindungskanal (S12)
#> ok. (0.00 secs) 
#> Running for id 13/35 (S21a: Rudower Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 1 (S21a)
#> ok. (0.00 secs) 
#> Running for id 14/35 (S09: Spree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 2 (S09)
#> ok. (0.00 secs) 
#> Running for id 15/35 (S13: Neuköllner SF-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Neuköllner SF-Kanal (S13)
#> ok. (0.00 secs) 
#> Running for id 16/35 (S16: Panke) ...
#> No outflow to well gallery or managed aquifer recharge for section Panke (S16)
#> ok. (0.00 secs) 
#> Running for id 17/35 (S21: Rudower Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 2 (S21)
#> ok. (0.00 secs) 
#> Running for id 18/35 (S15: Spree 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 3 (S15)
#> ok. (0.00 secs) 
#> Running for id 19/35 (S18: BSS-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section BSS-Kanal (S18)
#> ok. (0.00 secs) 
#> Running for id 20/35 (S14: Landwehrkanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Landwehrkanal (S14)
#> ok. (0.00 secs) 
#> Running for id 21/35 (H01: Nordgraben) ...
#> No outflow to well gallery or managed aquifer recharge for section Nordgraben (H01)
#> ok. (0.00 secs) 
#> Running for id 22/35 (H04: Oberhavel 1) ... ok. (0.71 secs) 
#> Running for id 23/35 (S17: Spree 4) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 4 (S17)
#> ok. (0.00 secs) 
#> Running for id 24/35 (H02: Tegeler Fließ) ...
#> No outflow to well gallery or managed aquifer recharge for section Tegeler Fließ (H02)
#> ok. (0.00 secs) 
#> Running for id 25/35 (S22: Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 1 (S22)
#> ok. (0.00 secs) 
#> Running for id 26/35 (H05: Oberhavel 2) ... ok. (0.49 secs) 
#> Running for id 27/35 (S19: Spree 5) ... ok. (0.55 secs) 
#> Running for id 28/35 (H03: Tegeler See) ... ok. (0.53 secs) 
#> Running for id 29/35 (S22a: Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 2 (S22a)
#> ok. (0.00 secs) 
#> Running for id 30/35 (H06: Oberhavel 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Oberhavel 3 (H06)
#> ok. (0.00 secs) 
#> Running for id 31/35 (S20: Spree 6) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 6 (S20)
#> ok. (0.00 secs) 
#> Running for id 32/35 (S23: Teltowkanal 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 3 (S23)
#> ok. (0.00 secs) 
#> Running for id 33/35 (H07: Unterhavel 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Unterhavel 1 (H07)
#> ok. (0.00 secs) 
#> Running for id 34/35 (H09: Wannsee) ... ok. (0.47 secs) 
#> Running for id 35/35 (H08: Unterhavel 2) ... ok. (0.46 secs) 
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
#> [[4]]
#> 
#> [[5]]
#> 
#> [[6]]
#> 
#> [[7]]
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[8]]
#> Warning: Removed 13 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 13 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[9]]
#> 
#> [[10]]
#> 
#> [[11]]
#> Warning: Removed 36 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 36 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[12]]
#> Warning: Removed 1 row containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> Running for id 1/35 (S04: Müggelsee) ... ok. (0.46 secs) 
#> Running for id 2/35 (S01: Seddinsee und Gosener Kanal) ... ok. (0.42 secs) 
#> Running for id 3/35 (S06: Erpe (NH-MF)) ... ok. (0.41 secs) 
#> Running for id 4/35 (S02: Langer See (Dahme)) ... ok. (0.41 secs) 
#> Running for id 5/35 (S05a: Müggelspree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree 2 (S05a)
#> ok. (0.00 secs) 
#> Running for id 6/35 (S03: Dahme) ...
#> No outflow to well gallery or managed aquifer recharge for section Dahme (S03)
#> ok. (0.00 secs) 
#> Running for id 7/35 (S10: MHG-Graben) ...
#> No outflow to well gallery or managed aquifer recharge for section MHG-Graben (S10)
#> ok. (0.00 secs) 
#> Running for id 8/35 (S05: Müggelspree) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree (S05)
#> ok. (0.00 secs) 
#> Running for id 9/35 (S08: Wuhle) ... ok. (0.42 secs) 
#> Running for id 10/35 (S11: Rummelsburger See) ...
#> No outflow to well gallery or managed aquifer recharge for section Rummelsburger See (S11)
#> ok. (0.00 secs) 
#> Running for id 11/35 (S07: Spree 1) ... ok. (0.44 secs) 
#> Running for id 12/35 (S12: Britzer Verbindungskanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Britzer Verbindungskanal (S12)
#> ok. (0.00 secs) 
#> Running for id 13/35 (S21a: Rudower Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 1 (S21a)
#> ok. (0.00 secs) 
#> Running for id 14/35 (S09: Spree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 2 (S09)
#> ok. (0.00 secs) 
#> Running for id 15/35 (S13: Neuköllner SF-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Neuköllner SF-Kanal (S13)
#> ok. (0.00 secs) 
#> Running for id 16/35 (S16: Panke) ...
#> No outflow to well gallery or managed aquifer recharge for section Panke (S16)
#> ok. (0.00 secs) 
#> Running for id 17/35 (S21: Rudower Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 2 (S21)
#> ok. (0.00 secs) 
#> Running for id 18/35 (S15: Spree 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 3 (S15)
#> ok. (0.00 secs) 
#> Running for id 19/35 (S18: BSS-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section BSS-Kanal (S18)
#> ok. (0.00 secs) 
#> Running for id 20/35 (S14: Landwehrkanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Landwehrkanal (S14)
#> ok. (0.00 secs) 
#> Running for id 21/35 (H01: Nordgraben) ...
#> No outflow to well gallery or managed aquifer recharge for section Nordgraben (H01)
#> ok. (0.00 secs) 
#> Running for id 22/35 (H04: Oberhavel 1) ... ok. (0.44 secs) 
#> Running for id 23/35 (S17: Spree 4) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 4 (S17)
#> ok. (0.00 secs) 
#> Running for id 24/35 (H02: Tegeler Fließ) ...
#> No outflow to well gallery or managed aquifer recharge for section Tegeler Fließ (H02)
#> ok. (0.00 secs) 
#> Running for id 25/35 (S22: Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 1 (S22)
#> ok. (0.00 secs) 
#> Running for id 26/35 (H05: Oberhavel 2) ... ok. (0.50 secs) 
#> Running for id 27/35 (S19: Spree 5) ... ok. (0.65 secs) 
#> Running for id 28/35 (H03: Tegeler See) ... ok. (0.52 secs) 
#> Running for id 29/35 (S22a: Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 2 (S22a)
#> ok. (0.00 secs) 
#> Running for id 30/35 (H06: Oberhavel 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Oberhavel 3 (H06)
#> ok. (0.00 secs) 
#> Running for id 31/35 (S20: Spree 6) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 6 (S20)
#> ok. (0.00 secs) 
#> Running for id 32/35 (S23: Teltowkanal 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 3 (S23)
#> ok. (0.00 secs) 
#> Running for id 33/35 (H07: Unterhavel 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Unterhavel 1 (H07)
#> ok. (0.00 secs) 
#> Running for id 34/35 (H09: Wannsee) ... ok. (0.46 secs) 
#> Running for id 35/35 (H08: Unterhavel 2) ... ok. (0.45 secs) 
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
#> [[4]]
#> 
#> [[5]]
#> 
#> [[6]]
#> 
#> [[7]]
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[8]]
#> Warning: Removed 13 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 13 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[9]]
#> 
#> [[10]]
#> 
#> [[11]]
#> Warning: Removed 36 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 36 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[12]]
#> Warning: Removed 1 row containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> Running for id 1/35 (S04: Müggelsee) ... ok. (0.44 secs) 
#> Running for id 2/35 (S01: Seddinsee und Gosener Kanal) ... ok. (0.39 secs) 
#> Running for id 3/35 (S06: Erpe (NH-MF)) ... ok. (0.38 secs) 
#> Running for id 4/35 (S02: Langer See (Dahme)) ... ok. (0.37 secs) 
#> Running for id 5/35 (S05a: Müggelspree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree 2 (S05a)
#> ok. (0.00 secs) 
#> Running for id 6/35 (S03: Dahme) ...
#> No outflow to well gallery or managed aquifer recharge for section Dahme (S03)
#> ok. (0.00 secs) 
#> Running for id 7/35 (S10: MHG-Graben) ...
#> No outflow to well gallery or managed aquifer recharge for section MHG-Graben (S10)
#> ok. (0.00 secs) 
#> Running for id 8/35 (S05: Müggelspree) ...
#> No outflow to well gallery or managed aquifer recharge for section Müggelspree (S05)
#> ok. (0.00 secs) 
#> Running for id 9/35 (S08: Wuhle) ... ok. (0.41 secs) 
#> Running for id 10/35 (S11: Rummelsburger See) ...
#> No outflow to well gallery or managed aquifer recharge for section Rummelsburger See (S11)
#> ok. (0.00 secs) 
#> Running for id 11/35 (S07: Spree 1) ... ok. (0.37 secs) 
#> Running for id 12/35 (S12: Britzer Verbindungskanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Britzer Verbindungskanal (S12)
#> ok. (0.00 secs) 
#> Running for id 13/35 (S21a: Rudower Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 1 (S21a)
#> ok. (0.00 secs) 
#> Running for id 14/35 (S09: Spree 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 2 (S09)
#> ok. (0.00 secs) 
#> Running for id 15/35 (S13: Neuköllner SF-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Neuköllner SF-Kanal (S13)
#> ok. (0.00 secs) 
#> Running for id 16/35 (S16: Panke) ...
#> No outflow to well gallery or managed aquifer recharge for section Panke (S16)
#> ok. (0.00 secs) 
#> Running for id 17/35 (S21: Rudower Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Rudower Teltowkanal 2 (S21)
#> ok. (0.00 secs) 
#> Running for id 18/35 (S15: Spree 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 3 (S15)
#> ok. (0.00 secs) 
#> Running for id 19/35 (S18: BSS-Kanal) ...
#> No outflow to well gallery or managed aquifer recharge for section BSS-Kanal (S18)
#> ok. (0.00 secs) 
#> Running for id 20/35 (S14: Landwehrkanal) ...
#> No outflow to well gallery or managed aquifer recharge for section Landwehrkanal (S14)
#> ok. (0.00 secs) 
#> Running for id 21/35 (H01: Nordgraben) ...
#> No outflow to well gallery or managed aquifer recharge for section Nordgraben (H01)
#> ok. (0.00 secs) 
#> Running for id 22/35 (H04: Oberhavel 1) ... ok. (0.41 secs) 
#> Running for id 23/35 (S17: Spree 4) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 4 (S17)
#> ok. (0.00 secs) 
#> Running for id 24/35 (H02: Tegeler Fließ) ...
#> No outflow to well gallery or managed aquifer recharge for section Tegeler Fließ (H02)
#> ok. (0.00 secs) 
#> Running for id 25/35 (S22: Teltowkanal 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 1 (S22)
#> ok. (0.00 secs) 
#> Running for id 26/35 (H05: Oberhavel 2) ... ok. (0.66 secs) 
#> Running for id 27/35 (S19: Spree 5) ... ok. (0.45 secs) 
#> Running for id 28/35 (H03: Tegeler See) ... ok. (0.51 secs) 
#> Running for id 29/35 (S22a: Teltowkanal 2) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 2 (S22a)
#> ok. (0.00 secs) 
#> Running for id 30/35 (H06: Oberhavel 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Oberhavel 3 (H06)
#> ok. (0.00 secs) 
#> Running for id 31/35 (S20: Spree 6) ...
#> No outflow to well gallery or managed aquifer recharge for section Spree 6 (S20)
#> ok. (0.00 secs) 
#> Running for id 32/35 (S23: Teltowkanal 3) ...
#> No outflow to well gallery or managed aquifer recharge for section Teltowkanal 3 (S23)
#> ok. (0.00 secs) 
#> Running for id 33/35 (H07: Unterhavel 1) ...
#> No outflow to well gallery or managed aquifer recharge for section Unterhavel 1 (H07)
#> ok. (0.00 secs) 
#> Running for id 34/35 (H09: Wannsee) ... ok. (0.44 secs) 
#> Running for id 35/35 (H08: Unterhavel 2) ... ok. (0.45 secs) 
#> [[1]]
#> 
#> [[2]]
#> 
#> [[3]]
#> 
#> [[4]]
#> 
#> [[5]]
#> 
#> [[6]]
#> 
#> [[7]]
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[8]]
#> Warning: Removed 13 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 13 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[9]]
#> 
#> [[10]]
#> 
#> [[11]]
#> Warning: Removed 36 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 36 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> 
#> [[12]]
#> Warning: Removed 1 row containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> [1] "flows-and-bfshares-per-section_bfshare-static_multiple-outflows-dynamic_2002-01-01_2022-12-31_scale-factor-1.pdf"  
#> [2] "flows-and-bfshares-per-section_bfshare-static_multiple-outflows-dynamic_2002-01-01_2022-12-31_scale-factor-1-1.pdf"
#> [3] "flows-and-bfshares-per-section_bfshare-static_multiple-outflows-dynamic_2002-01-01_2022-12-31_scale-factor-1-2.pdf"
```
