# 4. Wasserflächen vorbereiten
url <- "https://fbinter.stadt-berlin.de/fb/atom/Gewaesserkarte/Gewaesserkarte.zip"
tfile <- basename(url)

download.file(url, destfile = basename(url))

unzip(zipfile = tfile,
      exdir = "lakes_berlin")


lakes_berlin <- sf::read_sf("lakes_berlin/Gewaesser_Berlin_Flaechen.shp",
                            options = "ENCODING=WINDOWS-1252") %>%
  dplyr::mutate(area = sf::st_area(.) %>% as.numeric()) %>%
  dplyr::arrange(dplyr::desc(area)) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()


lakes_berlin_sel <- lakes_berlin %>%
  dplyr::filter(area > 150000)


## DWD datasets
remotes::install_github("kwb-r/kwb.dwd@get-rid-of-rgdal")


sf::write_sf(lakes_berlin_sel, "lakes_berlin_selected.shp")
shape_file <- "lakes_berlin_selected.shp"



# Only data of full months can currently be read!
evapo_p_raw <- kwb.dwd::read_daily_data_over_shape(
  #file = shape_file,
  shape = lakes_berlin_sel,
  variable = "evapo_p",
  from = "200201",
  to = "202212"
  #to = "202212"
)


evapo_p <- evapo_p_raw %>%
  dplyr::mutate(date = sprintf("%02d-%02d-%02d", year, month, day) %>%  as.Date()) %>%
  tibble::as_tibble()


usethis::use_data(evapo_p, overwrite = TRUE)

evapo_p_yearly <- evapo_p %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(evapo_p_mm_a = sum(mean))

gg_evapo_p_yearly  <- evapo_p_yearly %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x =  year, y = evapo_p_mm_a)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

plotly_evapo_p_yearly <- plotly::ggplotly(gg_evapo_p_yearly)

htmlwidgets::saveWidget(plotly_evapo_p_yearly, file = "evapo_p_yearly.html")



temporal_resolution <- "days"
debug <- TRUE

sections_with_area <- config$sections %>%
  dplyr::filter(!is.na(area_m2) & area_m2 > 0)

col_datetime <- "date"
col_datetime_join <- ifelse(temporal_resolution != "days", "datetime", col_datetime)


evapo_p_cbm_per_second <- evapo_p %>%
  dplyr::select(date, mean) %>%
  dplyr::mutate(mean = - mean / 1000 / 24 / 3600) %>%
  dplyr::rename(DWD_evapo.p  = mean)

evapo_p_df <- if(temporal_resolution == "days") {
  evapo_p_cbm_per_second
} else {
  evapo_p_cbm_per_second %>%
    kwb.BerlinWaterModel.public::fill_timeseries(temporal_resolution = "hours",
                                          direction = "down")
}

evapo_p_df_tot  <- stats::setNames(lapply(sections_with_area$section_id, function(s_id) {
  evapo_p_df$DWD_evapo.p * sections_with_area$area_m2[sections_with_area$section_id == s_id]
}), nm = sprintf("DWD_evapo.p_%s", sections_with_area$section_id)) %>%
  dplyr::bind_rows()

gg_evapo_p <- evapo_p_df[,1] %>%
  dplyr::bind_cols(evapo_p_df_tot) %>%
  tidyr::pivot_longer(cols = - tidyselect::all_of("date"),
                      values_to = "cbm_per_second") %>%
  dplyr::mutate(name = stringr::str_remove(name, pattern = "DWD_evapo\\.p_")) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = cbm_per_second, col = name)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()


plotly_gg_evapo_p <- plotly::ggplotly(gg_evapo_p)
htmlwidgets::saveWidget(plotly_gg_evapo_p, file = "evapo_p.html")

rain_df <-  kwb.utils::catAndRun(messageText = sprintf("Adding DWD rain data%s and convert from 'mm/h' to 'm3/s'",
                                                       ifelse(temporal_resolution == "days",
                                                              " and aggregate from 'hourly' to 'daily' values",
                                                              "")),
                                 expr = {
                                   if(temporal_resolution == "days") {
                                     rain %>%
                                       dplyr::mutate(date = as.Date(datetime, tz = "UTC")) %>%
                                       dplyr::relocate(date, .before = "datetime") %>%
                                       dplyr::select(- datetime) %>%
                                       dplyr::group_by(date) %>%
                                       dplyr::summarise(dplyr::across(.cols = tidyselect::starts_with("DWD"),
                                                                      .fns = ~ sum(.x, na.rm = TRUE)/(24*3600)/1000))
                                   } else {
                                     rain %>%
                                       dplyr::group_by(datetime) %>%
                                       dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("DWD"),
                                                                   .fns = ~ sum(.x, na.rm = TRUE)/3600/1000))
                                   }
                                 },
                                 dbg = debug
) %>%
  dplyr::filter(date >= as.Date("2002-01-01"),
                date <= as.Date("2022-12-31"))



rain.direct_tot  <- stats::setNames(lapply(sections_with_area$section_id, function(s_id) {
  rain_df$DWD_0433 * sections_with_area$area_m2[sections_with_area$section_id == s_id]
}), nm = sprintf("rain.direct_%s", sections_with_area$section_id)) %>%
  dplyr::bind_rows()

gg_rain.direct <- rain_df[,1] %>%
  dplyr::bind_cols(rain.direct_tot) %>%
  tidyr::pivot_longer(cols = - tidyselect::all_of("date"),
                      values_to = "cbm_per_second") %>%
  dplyr::mutate(name = stringr::str_remove(name, pattern = "DWD_0433_")) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = cbm_per_second, col = name)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

plotly_gg_rain.direct <- plotly::ggplotly(gg_rain.direct)
htmlwidgets::saveWidget(plotly_gg_rain.direct, file = "rain_direct.html")

rain_minus_evapo.p <- rain_df %>%
  dplyr::left_join(evapo_p_df) %>%
  dplyr::mutate(rain_minus_evapo.p = DWD_0433 - abs(DWD_evapo.p)) %>%
  dplyr::select(date, rain_minus_evapo.p)


rain_minus_evapo.p_tot  <- stats::setNames(lapply(sections_with_area$section_id, function(s_id) {
  rain_minus_evapo.p$rain_minus_evapo.p * sections_with_area$area_m2[sections_with_area$section_id == s_id]
}), nm = sprintf("rain_minus_evapo.p_%s", sections_with_area$section_id)) %>%
  dplyr::bind_rows()

gg_rain_minus_evapo.p <- rain_df[,1] %>%
  dplyr::bind_cols(rain_minus_evapo.p_tot) %>%
  tidyr::pivot_longer(cols = - tidyselect::all_of("date"),
                      values_to = "cbm_per_second") %>%
  dplyr::mutate(name = stringr::str_remove(name, pattern = "rain_minus_evapo\\.p_")) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = cbm_per_second, col = name)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

plotly_rain_minus_evapo.p <- plotly::ggplotly(gg_rain_minus_evapo.p )
htmlwidgets::saveWidget(plotly_rain_minus_evapo.p, file = "rain_minus_evapo.p.html")




rain_monthly <- rain %>%
  dplyr::mutate(year = format(datetime, format = "%Y") %>%  as.numeric(),
                month = format(datetime, format = "%m") %>%  as.numeric(),
                day = format(datetime, format = "%d") %>%  as.numeric(),
                days_in_month = lubridate::days_in_month(datetime),
                year_month = sprintf("%d-%02d-%02d",
                                     year,
                                     month,
                                     days_in_month)) %>%
  dplyr::group_by(year_month, month) %>%
  dplyr::summarise(rain_mm_month = sum(DWD_0433, na.rm = TRUE))


evapo_p_monthly <- evapo_p %>%
  dplyr::mutate(days_in_month = lubridate::days_in_month(date),
                year_month = sprintf("%d-%02d-%02d",
                                     year,
                                     month,
                                     days_in_month)) %>%
  dplyr::group_by(year_month, month) %>%
  dplyr::summarise(evapo_p_mm_month = sum(mean))


rain_evapo_p_monthly <- evapo_p_monthly %>%
  dplyr::left_join(rain_monthly,
                   by = c("year_month", "month")) %>%
  dplyr::mutate(period = dplyr::if_else(month >= 10 | month <= 4, "Winter", "Sommer"),
                year_month = year_month %>% as.Date(),
                rain_minus_evapo_p_mm_per_month = rain_mm_month - evapo_p_mm_month) %>%
  dplyr::rename(date = year_month)


rain_evapo_p_yearly <- rain_evapo_p_monthly %>%
  dplyr::mutate(year = format(date, format = "%Y") %>%  as.numeric()) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise_at(.vars = c("evapo_p_mm_month",
                                "rain_mm_month",
                                "rain_minus_evapo_p_mm_per_month"), .funs = sum) %>%
  dplyr::mutate(date = sprintf("%d-12-31", year) %>% as.Date()) %>%
  dplyr::rename(rain_minus_evapo_p_mm_per_year = rain_minus_evapo_p_mm_per_month)


gg_rain_evapo_p_monthly <- rain_evapo_p_monthly %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = rain_minus_evapo_p_mm_per_month)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

gg_evapo_p_monthly <- rain_evapo_p_monthly %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = evapo_p_mm_month)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly_evapo_p_monthly  <- plotly::ggplotly(gg_evapo_p_monthly)
htmlwidgets::saveWidget(plotly_evapo_p_monthly, file = "plotly_evapo_p_monthly.html")

plotly_rain_evapo_p_monthly  <- plotly::ggplotly(gg_rain_evapo_p_monthly)
htmlwidgets::saveWidget(plotly_rain_evapo_p_monthly, file = "plotly_rain_evapo_p_monthly.html")


gg_rain_evapo_p_monthly_summer_winter <- rain_evapo_p_monthly %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = rain_minus_evapo_p_mm_per_month, color = period)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly_rain_evapo_p_monthly_summer_winter  <- plotly::ggplotly(gg_rain_evapo_p_monthly_summer_winter)
htmlwidgets::saveWidget(plotly_rain_evapo_p_monthly_summer_winter, file = "plotly_rain_evapo_p_monthly_summer_winter.html")


gg_rain_evapo_p_yearly <- rain_evapo_p_yearly %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = rain_minus_evapo_p_mm_per_year )) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly_rain_evapo_p_yearly  <- plotly::ggplotly(gg_rain_evapo_p_yearly)
htmlwidgets::saveWidget(plotly_rain_evapo_p_yearly, file = "plotly_rain_evapo_p_yearly.html")

