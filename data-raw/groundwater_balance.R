if(FALSE) {

config <- kwb.BerlinWaterModel::config_read(config_dir = "inst/extdata/config/network_complete")

col_datetime <- "date"
col_datetime_join <- ifelse(temporal_resolution != "days", "datetime", col_datetime)
bfshare_dynamic <- FALSE
temporal_resolution <- "days"

bfstypes_equations <- config$bfstypes_equations %>%
  kwb.BerlinWaterModel::bfs_convert_equation()

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


ww_bfshare <- config$flows_in_out %>%
  dplyr::filter(stringr::str_starts(flow_id, "WW")) %>%
  kwb.BerlinWaterModel::shorten_ww_flow_id() %>%
  dplyr::select(flow_id, bank_filtration_share) %>%
  dplyr::rename(bank_filtration_share_static = bank_filtration_share) %>%
  dplyr::left_join(kwb.BerlinWaterModel::ww, by = c("flow_id" = "id")) %>%
  dplyr::filter(!is.na(cbm_per_second)) %>%
  dplyr::left_join(bfstypes_wellgalleries) %>%
  dplyr::mutate(cbm_per_day = cbm_per_second * 24 * 3600,
                bank_filtration_share_dynamic = NA) %>%
  dplyr::filter(!is.na(date))

for(type in bfstypes_equations$type) {

  bfs_dynamic <- bfstypes_equations[bfstypes_equations$type == type,]

  condition_flow <- !is.na(ww_bfshare$cbm_per_day) & ww_bfshare$cbm_per_day > ifelse(is.na(bfs_dynamic$Q_min_m3d), 0, bfs_dynamic$Q_min_m3d) & ww_bfshare$cbm_per_day < ifelse(is.na(bfs_dynamic$Q_max_m3d), 9999999999, bfs_dynamic$Q_max_m3d)

  condition_type_and_flow <- !is.na(ww_bfshare$type) & ww_bfshare$type == type & condition_flow

  flow_cbm_per_day <- ww_bfshare$cbm_per_day[condition_type_and_flow]


  bfs_dyn <- bfs_dynamic$equation_function[[1]](flow_cbm_per_day)  / 100

  condition_out_of_range <- flow_cbm_per_day > 0 & is.na(bfs_dyn)
  if(any(condition_out_of_range)) {
    warning(sprintf("There are %d data points out fo range for the following flow ids: %s\nSetting values to -9999",
                    sum(condition_out_of_range),
                    paste0(unique(ww_bfshare$flow_id[condition_out_of_range]), collapse = ", ")))
    ww_bfshare$bank_filtration_share_dynamic[condition_out_of_range] <- - 9999
  }

  ww_bfshare$bank_filtration_share_dynamic[condition_type_and_flow] <-  bfs_dynamic$equation_function[[1]](flow_cbm_per_day) / 100

}

col_bfshare <- ifelse(bfshare_dynamic,
                      "bank_filtration_share_dynamic",
                      "bank_filtration_share_static")

ww_bfshare_selected <- ww_bfshare %>%
  dplyr::mutate(bank_filtration_share = tidyr::replace_na(.data[[col_bfshare]], 0),
                cbm_per_second.bf = bank_filtration_share * tidyr::replace_na(cbm_per_second, 0),
                cbm_per_second.gw = (1 - bank_filtration_share) * tidyr::replace_na(cbm_per_second, 0))

ww_bfshare_selected <- ww_bfshare %>%
  dplyr::mutate(bank_filtration_share = tidyr::replace_na(.data[[col_bfshare]], 0),
                cbm_per_second.bf = bank_filtration_share * tidyr::replace_na(cbm_per_second, 0),
                cbm_per_second.gw = (1 - bank_filtration_share) * tidyr::replace_na(cbm_per_second, 0))



url <- "https://fbinter.stadt-berlin.de/fb/atom/Gewaesserkarte/Gewaesserkarte.zip"
tfile <- basename(url)

download.file(url, destfile = basename(url))

unzip(zipfile = tfile,
      exdir = "lakes_berlin")

lakes_berlin <- sf::read_sf("lakes_berlin/Gewaesser_Berlin_Flaechen.shp",
                            options = "ENCODING=WINDOWS-1252") %>%
  sf::st_transform(25833) %>%
  sf::st_make_valid()


system.time(gw_recharge <- read_arcegmo("C:/kwb/projects/impetus/SenUMVK/ArcEgmo/Übergabe_2021_05_05/SL4/Monatswerte_SL4/GWN_SL4.txt"))
load("../gw_recharge.rda")

gw_recharge <- gw_recharge %>%
  dplyr::filter(date >= as.Date("2002-01-01"))

efl <- sf::read_sf("C:/kwb/projects/impetus/SenUMVK/ArcEgmo/Übergabe_2021_05_05/SL4/Modellflaechen/efl.shp") %>%
  sf::st_transform(25833)


options(future.globals.maxSize = 8e9)  # Setzt das Speicherlimit auf 1GB
feflow_shp_files <- list.files("inst/extdata/gis/Selections/MS0/",
                               full.names = TRUE,pattern = "selected\\.shp$",
)

system.time(
  feflow_catchments_and_recharge <- get_feflow_catchments_and_recharge(files = feflow_shp_files,
                                                                     lakes_berlin = lakes_berlin,
                                                                     crop_waterbodies = TRUE)
)

system.time(
argegmo_recharge <- get_argegmo_recharge(feflow_catchments_and_recharge = feflow_catchments_and_recharge,
                                         efl = efl,
                                         gw_recharge = gw_recharge)
)


argegmo_recharge_df <- argegmo_recharge %>%
  dplyr::bind_rows(,.id = "filename")


tmp <- argegmo_recharge_df %>%
  tibble::as_tibble() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(date = as.Date(date),
                flow_id = filename %>%
                  stringr::str_remove(pattern = " Edge.*") %>%
                  stringr::str_remove("Gal") %>%
                  stringr::str_remove("\\s+") %>%
                  (\(x) stringr::str_c(stringr::str_sub(x, 1, 3), stringr::str_to_lower(stringr::str_sub(x, 4, 4))))(),
                days_in_month = format(date, format = "%d") %>%  as.integer(),
                cbm_per_second.rch_arcegmo = as.numeric(area_m2 * (value / 1000) / days_in_month / 24 / 3600)
  ) %>%
  dplyr::group_by(flow_id, date, days_in_month) %>%
  dplyr::summarise(catchment_area_m2.arcegmo = sum(area_m2),
                   cbm_per_second.rch_arcegmo =  sum(cbm_per_second.rch_arcegmo, na.rm = TRUE),
                   .groups = "drop")  %>%
  dplyr::left_join(feflow_catchments_and_recharge %>%
                     tibble::as_tibble() %>%
                     dplyr::select(filename, area_m2, rch_mm_per_year) %>%
                     dplyr::mutate(flow_id = filename %>%
                                     stringr::str_remove(pattern = " Edge.*") %>%
                                     stringr::str_remove("Gal") %>%
                                     stringr::str_remove("\\s+") %>%
                                     (\(x) stringr::str_c(stringr::str_sub(x, 1, 3), stringr::str_to_lower(stringr::str_sub(x, 4, 4))))(),
                                   rch_mm_per_year =  as.numeric(area_m2 * rch_mm_per_year/1000/365/24/3600)) %>%
                     dplyr::rename(catchment_area_m2.feflow = area_m2,
                                   cbm_per_second.rch_feflow = rch_mm_per_year),
                  by = "flow_id") %>%
  dplyr::left_join(ww_bfshare_selected, by = c("flow_id", "date")) %>%
  dplyr::mutate(mm_per_month.rch_arcegmo = 1000*24*3600*days_in_month*cbm_per_second.rch_arcegmo/catchment_area_m2.arcegmo %>% as.numeric(),
                mm_per_month.rch_feflow = 1000*24*3600*days_in_month*cbm_per_second.rch_feflow/catchment_area_m2.feflow %>%  as.numeric())


tmp_yearly <- tmp %>%
  dplyr::select(flow_id,
                date,
                tidyselect::starts_with("catch"),
                tidyselect::starts_with("mm_per_month")) %>%
  dplyr::mutate(year = format(date, format = "%Y")) %>%
  dplyr::group_by(flow_id, year) %>%
  dplyr::summarise(
    catchment_area_m2.arcegmo = dplyr::first(catchment_area_m2.arcegmo),
    catchment_area_m2.feflow = dplyr::first(catchment_area_m2.feflow),
    mm_per_year.rch_arcegmo = sum(mm_per_month.rch_arcegmo),
    mm_per_year.rch_feflow = sum(mm_per_month.rch_feflow)
    )


gg <- tmp_yearly  %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(cols = tidyselect::starts_with("mm_per_year")) %>%
  #dplyr::filter(flow_id == "FRIa") %>%
  dplyr::group_by(flow_id, year, name) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, col = name, group = name)) +
  ggplot2::facet_wrap(~ flow_id, ncol = 1, scales = "free_y") +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Spatially Avaraged Mean Yearly ArcEGMO Recharge for SVM Well Gallery Catchment Area (without Surface Water Bodies)",
                x = "Year",
                y = "Groundwater Recharge (mm/a)") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

pdff <- "svm-catchments-per-gallery_FRI_ArcEGMO-rch_mm-per-year_20022020.pdf"
kwb.utils::preparePdf(pdff, height.cm = 50)
gg
kwb.utils::finishAndShowPdf(pdff)

readr::write_csv(tmp_yearly, file = "svm-catchments-per-gallery_FRI_ArcEGMO-rch_mm-per-year_20022020.csv")

readr::write_csv(tmp_dat, "svm-model-areas_arcegmo-annual-recharge_2002-2020.csv")
htmlwidgets::saveWidget(plotly::ggplotly(gg), "svm-model-areas_arcegmo-annual-recharge_2002-2020.html")



tmp_monthly <- tmp %>%
  dplyr::select(flow_id, date, days_in_month, tidyselect::starts_with("cbm_per_second")) %>%
  dplyr::mutate(dplyr::across(
    .cols = tidyselect::starts_with("cbm_per_second"),
    .fns = ~ .x * days_in_month * 24 * 3600,
    .names = "{.col}"  # temporär, wir passen gleich um
  )) %>%
  dplyr::rename_with(
    .cols = starts_with("cbm_per_second"),
    .fn = ~ stringr::str_replace(., "cbm_per_second", "cbm_per_month")
  )

tmp_bf <- tmp %>%
  dplyr::group_by(flow_id) %>%
  dplyr::summarise(bf_mean = mean(bank_filtration_share, na.rm = TRUE),
                   bf_sd = sd(bank_filtration_share, na.rm = TRUE),
                   bf_label = sprintf("'%s' BF share (%.1f \u00B1 %.1f %%)",
                                      dplyr::if_else(bf_sd == 0,
                                                     "static",
                                                     "dynamic"),
                                      100*bf_mean,
                                      100*bf_sd))

observation_period <- range(tmp_monthly$date)
observation_period[1] <- stringr::str_replace(observation_period[1], "31", "01")

observation_period_days <- as.numeric(diff(observation_period))

tmp_period <- tmp_monthly %>%
  dplyr::mutate(days_pumping = dplyr::if_else(cbm_per_month > 0, days_in_month, 0)) %>%
  dplyr::group_by(flow_id) %>%
  dplyr::summarise(dplyr::across(.cols = c("days_pumping", tidyselect::starts_with("cbm_per_month")),
                                 ~ sum(.x, na.rm = TRUE)
  )) %>%
  dplyr::rename_with(
    .cols = starts_with("cbm_per_month"),
    .fn = ~ stringr::str_replace(., "cbm_per_month", "cbm")
  ) %>%
  dplyr::mutate(percent_pumping.days = days_pumping / observation_period_days,
                percent_pumping.gw_feflow = cbm.gw /  cbm.rch_feflow,
                percent_pumping.gw_arcegmo = cbm.gw /  cbm.rch_arcegmo) %>%
  dplyr::left_join(tmp_bf) %>%
  dplyr::mutate(
    title = sprintf("%s (%.0f m3 abstraction, %.0f m3 BF, %.0f m3 GW, %d days of pumping, i.e. %.1f %% of %s - %s):",
                    flow_id,
                    cbm,
                    cbm.bf,
                    cbm.gw,
                    days_pumping,
                    100 * percent_pumping.days,
                    observation_period[1],
                    observation_period[2]),
    subtitle = sprintf("%s, percental GW withdrawal: %.1f %% (ArcEgmo), %.1f %% (FEFLOW)",
                    bf_label,
                    100 * percent_pumping.gw_arcegmo,
                    100 * percent_pumping.gw_feflow))


flow_ids <- unique(tmp_monthly$flow_id)

debug <- TRUE

pdff <- "groundwater_catchments.pdf"

kwb.utils::preparePdf(pdff)

lapply(flow_ids[1:length(flow_ids)], function(id) {

kwb.utils::catAndRun(messageText = id,
                     expr = {


tmp_monthly_long <- tmp_monthly %>%
  dplyr::filter(flow_id == id
  ) %>%
  dplyr::select(flow_id, date, tidyselect::starts_with("cbm_per_month")) %>%
  dplyr::mutate(dplyr::across(
    .cols = tidyselect::starts_with("cbm_per_month"),
    .fns = ~ cumsum(ifelse(is.na(.x), 0, .x)),
    .names = "{.col}"
  )) %>%
  tidyr::pivot_longer(cols = tidyselect::starts_with("cbm")) %>%
  dplyr::group_by(flow_id, date)


gg <- tmp_monthly_long %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = date, y = value, col = name)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_log10(limits = c(1, max(tmp_monthly_long$value, na.rm = TRUE))) +
  ggplot2::labs(x = "Date",
                y = "Cumulative Flow (m3)",
                title = tmp_period$title[tmp_period$flow_id == id],
                subtitle = tmp_period$subtitle[tmp_period$flow_id == id]) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

#plotly_gg <- plotly::ggplotly(gg)
#htmlwidgets::saveWidget(plotly_gg,
#                        file = sprintf("time-series_rch-gw_%s.html", id),
#                        title = id)
gg
}

,
dbg = debug)

})

kwb.utils::finishAndShowPdf(pdff)

system.time(
argegmo_recharge_per_year <- argegmo_recharge_df %>%
  dplyr::mutate(rch_mm = value * area_m2) %>%
  dplyr::group_by(filename, date) %>%
  dplyr::summarise(geometry = geometry %>%
                     sf::st_combine() %>%
                     sf::st_union(),
                   area_m2 = sum(area_m2),
                   rch_mm = sum(rch_mm)/area_m2) %>%
  dplyr::mutate(year = format(date, format = "%Y") %>% as.integer()) %>%
  dplyr::group_by(filename, geometry, year) %>%
  dplyr::summarise(rch_mm_per_year = sum(rch_mm))
)

argegmo_recharge_mean <- argegmo_recharge_df %>%
  dplyr::mutate(year = format(date, format = "%Y") %>% as.integer(),
                rch_mm = value * area_m2) %>%
  dplyr::mutate(year_min = min(year),
                year_max = max(year)) %>%
  dplyr::group_by(filename, geometry) %>%
  dplyr::summarise(area_m2 = sum(area_m2),
                   rch_mm_per_year  = sum(rch_mm)/area_m2)


gw_recharge_selected_sf <- feflow_catchments_and_recharge %>%
  dplyr::rename(feflow_rch_mm_per_year = rch_mm_per_year) %>%
  dplyr::mutate(feflow_rch_mm_per_year = as.numeric(feflow_rch_mm_per_year)) %>%
  dplyr::left_join(argegmo_recharge_mean %>%
                     tibble::as_tibble() %>%
                     dplyr::select(-geometry) %>%
                     dplyr::rename(argegmo_rch_mm_per_year_20022020 = rch_mm_per_year) %>%
                     dplyr::mutate(argegmo_rch_mm_per_year_20022020 = as.numeric(argegmo_rch_mm_per_year_20022020))) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(dplyr::desc(area_m2))


gw_recharge_selected_sf %>%
  dplyr::select(-geometry) %>%
  readr::write_csv2(file = "feflow_catchments_and_recharge_feflow-argegmo_with-surface-water-bodies.csv")



# Farbpalette definieren: von blau nach rot
pal <- leaflet::colorNumeric(
  palette = "RdBu",  # Alternativ "viridis" oder "coolwarm"
  domain = gw_recharge_selected_sf$feflow_rch_mm_per_year,
  reverse = FALSE  # Invertiert, damit Blau für niedrige Werte und Rot für hohe Werte gilt
)


plotly_gg <- gw_recharge_selected_sf %>%
  dplyr::mutate(feflow_rch_mm_per_year = as.numeric(feflow_rch_mm_per_year),
                argegmo_rch_mm_per_year_20022020 = as.numeric(argegmo_rch_mm_per_year_20022020)) %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  #dplyr::group_by(geometry) %>%
  #dplyr::summarise(value = mean(value)) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(
    fillColor = ~pal(feflow_rch_mm_per_year),  # Farbwerte aus `value`
    color = "black",  # Schwarz umrandet für bessere Sichtbarkeit
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    layerId = ~feflow_rch_mm_per_year,
    popup = ~sprintf("%s<br>FEFLOW: %f, ArgEgmo (2002-2020): %f",
                     filename %>% stringr::str_remove(pattern = " Edge.*"),
                     feflow_rch_mm_per_year,
                     argegmo_rch_mm_per_year_20022020),
    group = "FEFLOW recharge"
  ) %>%
  leaflet::addPolygons(
    data = lakes_berlin %>% sf::st_transform(4326),
    fillColor = "blue",  # Farbwerte aus `value`
    layerId = ~GEWNAME,
    weight = 2,
    opacity = 1,
    popup = ~paste("See:", GEWNAME),
    group = "Surface Water Bodies"
  ) %>%
  leaflet::addPolygons(
    data = argegmo_recharge_mean %>%
      dplyr::mutate(rch_mm_per_year = as.numeric(rch_mm_per_year)) %>%
      sf::st_as_sf() %>%
      sf::st_transform(4326),
    fillColor = ~pal(rch_mm_per_year),  # Farbwerte aus `value`
    layerId = ~rch_mm_per_year,
    weight = 2,
    opacity = 1,
    popup = ~sprintf("ArgEgmo (2002-2020): %f",
                     rch_mm_per_year),
    group = "ArcEgmo (2002-2020) recharge"
  ) %>%
  leaflet::addLegend(
    pal = pal,
    values = ~argegmo_rch_mm_per_year_20022020,
    position = "bottomright",
    title = "ArgEgmo GWN"
  ) %>%
  # Layer Control zum An- und Ausschalten der Gruppen
  leaflet::addLayersControl(
    overlayGroups = c("ArcEgmo (2002-2020) recharge",
                      "FEFLOW recharge",
                      "Surface Water Bodies"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) #%>%
  # leaflet::addControl(
  #   sprintf("<h3>%s</h3>", basename(file)),
  #   position = "topright"
  # )

htmlwidgets::saveWidget(plotly_gg, file = "groundwater-recharge_feflow-arcegmo_FRI.html")





year_selected <- 2002


argegmo_recharge_per_year %>%
  dplyr::ungroup() %>%
  dplyr::select(-geometry) %>%
  tidyr::pivot_wider(names_from = year, values_from = rch_mm_per_year) %>%
  View()

argegmo_recharge_per_year_selected <- argegmo_recharge_per_year %>%
  dplyr::filter(year == year_selected) %>%
  dplyr::mutate(rch_mm_per_year = as.numeric(rch_mm_per_year))



# Farbpalette definieren: von blau nach rot
pal <- leaflet::colorNumeric(
  palette = "RdBu",  # Alternativ "viridis" oder "coolwarm"
  domain = argegmo_recharge_per_year_selected$rch_mm_per_year,
  reverse = FALSE  # Invertiert, damit Blau für niedrige Werte und Rot für hohe Werte gilt
)

argegmo_recharge_per_year_selected %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(layerId = ~rch_mm_per_year,
                       fillColor = ~pal(rch_mm_per_year)) %>%
  leaflet::addControl(sprintf("<h3>ArcEgmo (%d)</h3>", year_selected),
                      position = "topright") %>%
  leaflet::addLegend(
    pal = pal,
    values = ~rch_mm_per_year,
    position = "bottomright",
    title = "ArgEgmo GWN"
  )


wells_shp <- "Y:/GRW_Department/GRW_Organisation/General data/BWB/GIS Daten/Brunnen/ESRI Shape/Brunnen.shp"
wells <- sf::st_read(wells_shp, options = "ENCODING=WINDOWS-1252") %>%
  sf::st_set_crs(3068) %>%
  sf::st_transform(4326)

waterbodies_shp <- "Y:/GRW_Department/GRW_Organisation/General data/Gewässer_Ogre/Gewaesser_merged_by_OgRe.shp"
waterbodies <- sf::st_read(waterbodies_shp, options = "ENCODING=WINDOWS-1252") %>%
  sf::st_set_crs(3068) %>%
  sf::st_transform(4326)


unique_filenames <- unique(feflow_catchments_and_recharge$filename)
color_palette <- leaflet::colorFactor(RColorBrewer::brewer.pal(length(unique_filenames), "Set3"), unique_filenames)


feflow_catchments


leaflet_map <-  feflow_catchments_and_recharge %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)



# Füge für jede `filename` eine eigene LayerGroup hinzu
for (filename in unique_filenames) {
  leaflet_map <- leaflet_map %>%
    leaflet::addPolygons(data = feflow_catchments_and_recharge %>%
                           sf::st_transform(4326) %>%
                  dplyr::filter(filename == !!filename),
                layerId = ~filename,
                group = filename,
                color = "black", # Rand um die Polygone
                fillColor = ~color_palette(filename),
                fillOpacity = 0.7,
                weight = 1)
}

# Kontroll-Panel für das An- und Abwählen der einzelnen `filename`-Gruppen
feflow_catchments <- leaflet_map %>%
  leaflet::addCircles(data = wells,
                      popup = ~sprintf("Brunnengalerie: %s (%s)", GALERIE, WERK)) %>%
  leaflet::addLayersControl(overlayGroups = unique_filenames,
                   options =  leaflet::layersControlOptions(collapsed = FALSE))


htmlwidgets::saveWidget(feflow_catchments, file = "feflow_catchments.html")


feflow_catchments %>%
  leaflet::addPolygons(layerId = ~filename,
                       color = "",
                       fillColor = ~color_palette(filename),
                       popup = ~paste("Filename:", filename)) %>%
group = "Feflow Polygons"
  leaflet::addControl(sprintf("<h3>ArcEgmo (%d)</h3>", year_selected),
                      position = "topright") %>%
  leaflet::addLegend(
    pal = pal,
    values = ~rch_mm_per_year,
    position = "bottomright",
    title = "ArgEgmo GWN"
  )


library(leaflet)

# Erstelle eine Leaflet-Karte
m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 13.405, lat = 52.52, popup = "Berlin") %>%
  addControl("<h3>Kartentitel</h3>", position = "topcenter")

# Karte anzeigen
m


# Linien zusammenführen
feflow_union <- sf::st_union(feflow)

# Linien in Polygon umwandeln
feflow_polygons <- sf::st_cast(feflow_union, "POLYGON")

plot(feflow_union)

efl <- sf::read_sf("C:/kwb/projects/impetus/SenUMVK/ArcEgmo/Übergabe_2021_05_05/SL4/Modellflaechen/efl.shp")

efl %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(layerId = ~EZG)

feflow_efl <- sf::st_intersects(feflow, efl)


feflow_shp_files <- list.files("inst/extdata/gis/Selections/MS0/",
                               full.names = TRUE,pattern = "\\.shp$")

pdff <- "feflow_catchments.pdf"
kwb.utils::preparePdf(pdff)

lapply(feflow_shp_files, function(file) {

  feflow <- sf::read_sf(file) %>%
    sf::st_set_crs(3068) %>%
    sf::st_transform(25833) %>%
    sf::st_zm()


  # Linien zusammenführen
  feflow_union <- sf::st_union(feflow)

  plot(feflow_union)
  title(basename(file))
})
kwb.utils::finishAndShowPdf(PDF = pdff)

}

read_arcegmo <- function(path) {

  readr::read_tsv(path) %>%
    tidyr::pivot_longer(-KEN, names_to = "month_year") %>%
    dplyr::mutate(month_year = lubridate::my(month_year),
                  date = lubridate::rollback(month_year + months(1),
                                             roll_to_first = FALSE)
    ) %>%
    dplyr::select(- month_year) %>%
    dplyr::relocate(date, .before = KEN)
}



get_feflow_catchments_and_recharge <- function(files, lakes_berlin, crop_waterbodies = TRUE) {
  future::plan(future::multisession)  # Parallele Verarbeitung mit mehreren Prozessoren

  results <- future.apply::future_lapply(files, function(file) {
    message(sprintf("Getting FEFLOW catchment and groundwater recharge for '%s'", basename(file)))

    feflow <- sf::read_sf(file) %>%
      sf::st_set_crs(3068) %>%
      sf::st_transform(25833) %>%
      sf::st_zm()

    if (crop_waterbodies) {
      feflow <- feflow %>%
        sf::st_difference(sf::st_union(lakes_berlin))
    }

    feflow %>%
      dplyr::mutate(area_m2 = sf::st_area(.),
                    rch_mm_per_year = area_m2 * RCH,
                    geometry = sf::st_union(geometry)) %>%
      dplyr::group_by(geometry) %>%
      dplyr::summarise(area_m2 = sum(area_m2),
                       rch_mm_per_year = sum(rch_mm_per_year) / area_m2)
  }, future.seed = TRUE)

  results_named <- stats::setNames(results, basename(files))
  dplyr::bind_rows(results_named, .id = "filename")
}

get_argegmo_recharge <- function(feflow_catchments_and_recharge, efl, gw_recharge) {

  filenames <- unique(feflow_catchments_and_recharge$filename)

  results <- lapply(filenames, function(file) {
    message(sprintf("Calculating ArgEgmo groundwater recharge for '%s'", basename(file)))

    feflow <- feflow_catchments_and_recharge %>%
      dplyr::filter(filename == file)

    efl_match <- feflow %>%
      sf::st_union() %>%
      sf::st_intersects(efl)

    efl_intersects <- feflow %>%
      sf::st_intersection(efl[unlist(efl_match), ]) %>%
      dplyr::mutate(area_m2 = sf::st_area(.))

    efl_intersects_by_eflid <- efl_intersects %>%
      dplyr::group_by(EFLID) %>%
      dplyr::summarise(area_m2 = sum(area_m2))

    gw_recharge_selected <- efl_intersects_by_eflid %>%
      dplyr::ungroup() %>%
      tibble::as_tibble() %>%
      dplyr::left_join(
        gw_recharge %>%
          dplyr::filter(date >= as.Date("2002-01-01") & date <= as.Date("2022-12-31"),
                        KEN %in% unique(efl_intersects$EFLID)) %>%
          dplyr::group_by(KEN, date) %>%
          dplyr::summarise(value = sum(value)) %>%
          dplyr::ungroup(),
        by = c("EFLID" = "KEN")
      ) %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(value = sum(value * area_m2) / sum(area_m2))

    gw_recharge_selected
  })

  stats::setNames(results, filenames)
}
