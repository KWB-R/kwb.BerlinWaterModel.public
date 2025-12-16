## code to prepare `dwd-rain-ezg` dataset and baseflow calculation to substract rain from stream flow goes here
remotes::install_github("kwb-r/kwb.dwd@get-rid-of-rgdal")


install.packages("rdwd")
library(dplyr)
rdwd::updateRdwd()

config_baseflow <- readr::read_csv2("inst/extdata/config/config_baseflow.csv")

generate_pdf_with_baseflow_graphs <- FALSE
generate_html_widget_rain_stations <- FALSE

# read balance corrected flow data set of provided by SenMVKU
flow_balance_corrected <- "inst/extdata/input_data/q_balance-corrected_2002-2022.csv" %>%
  readr::read_csv() %>%
  dplyr::mutate(Datum = lubridate::dmy(Datum)) %>%
  dplyr::rename(date = Datum)

# read fisbroker-data of sewer network
kanal <- kwb.fisbroker::read_wfs(dataset_id = "s02_09kanalisation2012")

# read Berlin area to derive middle point for proximity search of rain stations
landesgrenze <- kwb.fisbroker::read_wfs(dataset_id = "s_wfs_alkis_land")

bbox <- landesgrenze %>%
  sf::st_transform(4326) %>%
  sf::st_point_on_surface() %>%
  sf::st_bbox()

# get list of DWD rain stations in and around Berlin (26 km around center)
Berlin_stations <- rdwd::nearbyStations(
  lat = bbox[2],
  lon = bbox[1],
  res = "daily",
  var = "more_precip",
  per = "historical",
  radius = 26
)

Berlin_rain_daily_stations <- Berlin_stations %>%
  dplyr::filter(!is.na(Stations_id)) %>%
  sf::st_as_sf(coords = c("geoLaenge", "geoBreite"), crs = 4326) %>%
  dplyr::mutate(label = sprintf(
    "%d: %s (%s - %s)",
    Stations_id,
    Stationsname,
    von_datum,
    bis_datum
  ))

# read daily rain data of Berlin rain stations
Berlin_rain_daily_list <- stats::setNames(lapply(Berlin_rain_daily_stations$url, function(url) {
  rdwd::dataDWD(url)
}),
nm = Berlin_rain_daily_stations$Stationsname)

Berlin_rain_daily_df <- dplyr::bind_rows(Berlin_rain_daily_list, .id = "Stationsname")

Berlin_rain_daily_df_wide <- Berlin_rain_daily_df %>%
  dplyr::select(STATIONS_ID, MESS_DATUM, RS) %>%
  tidyr::pivot_wider(names_from = STATIONS_ID, values_from = RS)

#
# River baseflows section
#
baseflow_list <- stats::setNames(lapply(config_baseflow$name_river, function(name_river) {
  kwb.utils::catAndRun(sprintf("Calculating baseflow for river %s", name_river),
                       expr = {

                         config_baseflow_selected <- config_baseflow[config_baseflow$name_river == name_river, ]

# generate map with EZG of selected river and location of available rain stations
                         if (generate_html_widget_rain_stations == TRUE) {
                           ezg_fisbroker <- kanal %>%
                             dplyr::filter(ageb1 == config_baseflow_selected$ageb1) %>%
                             sf::st_transform(4326)

                           lf <- ezg_fisbroker %>%
                             sf::st_union() %>%
                             sf::st_transform(4326) %>%
                             leaflet::leaflet() %>%
                             leaflet::addTiles() %>%
                             leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
                             leaflet::addPolygons(color = "blue") %>%
                             #  leaflet::addPolygons(color = "green", data = ezg_shp) %>%
                             leaflet::addCircleMarkers(
                               color = "red",
                               #opacity = 0,
                               radius = 10,
                               stroke = FALSE,
                               label = ~ label,
                               labelOptions = leaflet::labelOptions(
                                 noHide = TRUE,
                                 offset = c(0, -12),
                                 textOnly = TRUE,
                                 textsize = "15px",
                                 style = list("color" = "red")
                               ),
                               data = Berlin_rain_daily_stations
                             )

                           htmlwidgets::saveWidget(lf, sprintf("Ezg_dwd-stations_%s.html", name_river))
                         }

# Extract baseflows for rivers & co with different baseflow methods
                         conversion_factor <- config_baseflow_selected$area_m2 * config_baseflow_selected$runoff_coefficient / 1000 / 24 / 3600

                         river_baseflow <- flow_balance_corrected %>%
                           dplyr::select(date,
                                         tidyselect::matches(config_baseflow_selected$flow_id)) %>%
                           dplyr::rename(Q = !!config_baseflow_selected$flow_id) %>%
                           dplyr::mutate(
                             Qbase_boughton = round(grwat::gr_baseflow(Q, method = 'boughton'), 2),
                             Qbase_lynehollick = round(grwat::gr_baseflow(Q, method = 'lynehollick'), 2),
                             Qbase_furey = round(grwat::gr_baseflow(Q, method = 'furey'), 2),
                             Qbase_maxwell = round(grwat::gr_baseflow(Q, method = 'maxwell'), 2)
                           ) %>%
                           dplyr::left_join(
                             Berlin_rain_daily_df_wide %>%
                               dplyr::select(
                                 MESS_DATUM,
                                 as.character(config_baseflow_selected$rain_station_id)
                               ) %>%
                               dplyr::rename(
                                 "rain_mm" = as.character(config_baseflow_selected$rain_station_id),
                                 "date" = MESS_DATUM
                               ),
                             by = "date"
                           ) %>%
                           dplyr::mutate(
                             "rain_daily_flow_m3_s" = rain_mm * conversion_factor,
                             "Q-Qbase_boughton-rain_runoff" = Q - Qbase_boughton - rain_daily_flow_m3_s,
                             "Q-Qbase_lynehollick-rain_runoff" = Q - Qbase_lynehollick - rain_daily_flow_m3_s,
                             "Q-Qbase_furey-rain_runoff" = Q - Qbase_furey - rain_daily_flow_m3_s,
                             "Q-Qbase_maxwell-rain_runoff" = Q - Qbase_maxwell - rain_daily_flow_m3_s
                           )  %>%
                           dplyr::filter(!is.na(rain_mm)) # alle Werte ohne fehlende Regendaten
                         # dplyr::filter(date >= "2010-01-01") # Zeitraum 2010-2022

# prepare table with offsets (flow minus baseflow minus rain inputs calculated from runoff coefficient of EZG and rain data)
                         baseflow_rain_offset <- river_baseflow %>%
                           dplyr::summarise(dplyr::across(tidyselect::starts_with("Q-Qbase"), .fns = sum)) %>%
                           tidyr::pivot_longer(
                             cols = tidyselect::everything(),
                             names_to = "method",
                             values_to = sprintf("%s offset_sum_daily_cbm_per_s", name_river)
                           )


                         methods <- c('boughton',
                                      'chapman',
                                      'furey',
                                      'jakeman',
                                      'lynehollick',
                                      'maxwell')

# generate pdf with graphs for all baseflow methods
                         if (generate_pdf_with_baseflow_graphs == TRUE) {
                           pdff <- sprintf("Baseflow-methods_%s.pdf", name_river)
                           kwb.utils::preparePdf(pdfFile = pdff)

                           g <- lapply(methods, function(my_method) {
                             message(
                               sprintf(
                                 "Generating baseflow for river  %s with method %s",
                                 name_river,
                                 my_method
                               )
                             )

                             river_baseflow  %>%
                               dplyr::mutate(Qbase = grwat::gr_baseflow(Q, method = my_method)) %>%
                               ggplot2::ggplot() +
                               ggplot2::geom_area(ggplot2::aes(date, Q),
                                                  fill = 'steelblue',
                                                  color = 'black') +
                               ggplot2::geom_area(
                                 ggplot2::aes(date, Qbase),
                                 fill = 'orangered',
                                 color = 'black'
                               ) +
                               #   ggplot2::scale_x_date(limits = c(lubridate::ymd(20030101), lubridate::ymd(20231231))) +
                               ggplot2::labs(title = sprintf(
                                 "Baseflow for river %s with method %s",
                                 name_river,
                                 my_method
                               )) +
                               ggplot2::theme_bw()
                           })

                           print(g)

                           #kwb.utils::finishAndShowPdf(pdff)
                           dev.off()
                         }

# write csv with baseflow data (lynehollick method)
                         readr::write_csv(
                           river_baseflow %>%
                             dplyr::select(date, Qbase_lynehollick) %>%
                             dplyr::rename(river_baseflow = Qbase_lynehollick),
                           file = sprintf("Baseflow_%s.csv", name_river)
                         )

                         list(river_baseflow = river_baseflow,
                              baseflow_rain_offset = baseflow_rain_offset)

                       })
}),
nm = config_baseflow$name_river)

# prepare dataframe with baseflow and offset data for all rivers
baseflow_df <- tibble::tibble()

for (name_river in  names(baseflow_list)) {
  tmp_df <- baseflow_list[[name_river]]$river_baseflow  %>%
    dplyr::select(date, Qbase_lynehollick) %>%
    dplyr::rename(!!sprintf("%s_baseflow", name_river) := Qbase_lynehollick)

  if (name_river == names(baseflow_list)[1]) {
    baseflow_df <- tmp_df
  } else {
    baseflow_df <- baseflow_df %>%
      dplyr::left_join(tmp_df, by = "date")
  }
}

# add baseflow to flow dataset as additional columns
readr::write_csv(flow_balance_corrected %>%
                   dplyr::left_join(baseflow_df, by = "date"),
                 file = "flow_balance_corrected_baseflow.csv")


#
# trial to use RADOLAN data to get rain data for river EZG, but no daily resolution resolution available (only monthly)
#

if (FALSE) {
  # Set the target directory
  export_dir <- "D:/impetus/dwd/radolan"

  # Create required directories
  kwb.utils::createDirectory(file.path(export_dir, "daily/historical"))


  yearmonth_start <- "2006-10" # first month available on DWD
  yearmonth_end <- "2023-12"

  # Download corresponding RADOLAN files from DWD's FTP server
  kwb.dwd::download_radolan(
    resolution = "daily",
    export_dir = export_dir,
    start_daily = yearmonth_start,
    end_daily = yearmonth_end
  )


  dwd_daily_list <- stats::setNames(lapply(shapefiles, function(shapefile) {
    kwb.dwd::read_daily_data_over_shape(
      file = shapefile,
      variable = "precipitation",
      from = yearmonth_start,
      to = yearmonth_end,
      quiet = TRUE
    )
  }), nm = shapefiles)



  usethis::use_data(dwd_rain_ezg_wuhle, overwrite = TRUE)
}
