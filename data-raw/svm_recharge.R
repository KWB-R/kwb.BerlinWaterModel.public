if(FALSE) {

svm_meta <- readxl::read_xlsx(path = "inst/extdata/gis/svm/Auflistung_SVM_20230105.xlsx",
                              skip = 1,
                              n_max = 5) %>%
  dplyr::mutate(id = c(5,4,3,1,2))

svm_boundaries <- sf::read_sf("inst/extdata/gis/svm/SVM_Modellraender.shp") %>%
  dplyr::mutate(filename = sprintf("SVM_0%d", 1:5),
                id = 1:5,
                Area = sf::st_area(.)) %>%
  sf::st_transform(crs = 25833) %>%
  sf::st_zm()


system.time(gw_recharge <- read_arcegmo("C:/kwb/projects/impetus/SenUMVK/ArcEgmo/Übergabe_2021_05_05/SL4/Monatswerte_SL4/GWN_SL4.txt"))
load("../gw_recharge.rda")

gw_recharge <- gw_recharge %>%
  dplyr::filter(date >= as.Date("2002-01-01"))

system.time(
  feflow_catchments_and_recharge <- get_feflow_catchments_and_recharge(svm_boundaries = svm_boundaries,
                                                                       lakes_berlin = lakes_berlin,
                                                                       crop_waterbodies = TRUE)
)


system.time(
svm_recharge <- get_argegmo_recharge(feflow_catchments_and_recharge = feflow_catchments_and_recharge,
                                     efl = efl,
                                     gw_recharge = gw_recharge)
)


svm_recharge_df  <- svm_recharge %>%
  dplyr::bind_rows(,.id = "filename")


tmp <- svm_recharge_df %>%
  tibble::as_tibble() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id = stringr::str_remove(filename, "SVM_0") %>% as.numeric()) %>%
  dplyr::left_join(svm_meta %>% dplyr::select(id,  SVM)) %>%
  dplyr::rename(svm_id = id,
                svm_name =  SVM) %>%
  dplyr::mutate(date = as.Date(date),
                days_in_month = format(date, format = "%d") %>%  as.integer(),
                cbm_per_second.rch_arcegmo = as.numeric(area_m2 * (value / 1000) / days_in_month / 24 / 3600)
  ) %>%
  dplyr::group_by(svm_id, svm_name, date, days_in_month) %>%
  dplyr::summarise(catchment_area_m2.arcegmo = sum(area_m2),
                   cbm_per_second.rch_arcegmo =  sum(cbm_per_second.rch_arcegmo, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(rch_mm_per_month = 1000*24*3600*days_in_month*cbm_per_second.rch_arcegmo/catchment_area_m2.arcegmo)



tmp_dat <- tmp %>%
  dplyr::mutate(year = format(date, format = "%Y") %>% as.integer()) %>%
  dplyr::group_by(svm_id, svm_name, catchment_area_m2.arcegmo, year) %>%
  dplyr::summarise(rch_mm_per_year = sum(rch_mm_per_month) %>% as.numeric())


gg <- tmp_dat  %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = rch_mm_per_year, col = svm_name)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Spatially Avaraged Mean Yearly ArcEGMO Recharge for SVM Model Areas (without Surface Water Bodies)",
                x = "Year",
                y = "Groundwater Recharge (mm/a)") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")


readr::write_csv(tmp_dat, "svm-model-areas_arcegmo-annual-recharge_2002-2020.csv")
htmlwidgets::saveWidget(plotly::ggplotly(gg), "svm-model-areas_arcegmo-annual-recharge_2002-2020.html")


tmp_monthly <- tmp %>%
  dplyr::select(date, days_in_month, tidyselect::starts_with("cbm_per_second")) %>%
  dplyr::mutate(dplyr::across(
    .cols = tidyselect::starts_with("cbm_per_second"),
    .fns = ~ .x * days_in_month * 24 * 3600,
    .names = "{.col}"  # temporär, wir passen gleich um
  )) %>%
  dplyr::rename_with(
    .cols = starts_with("cbm_per_second"),
    .fn = ~ stringr::str_replace(., "cbm_per_second", "cbm_per_month")
  )


svm_boundaries %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(layerId = ~filename,
                       popup = ~filename)

}

get_feflow_catchments_and_recharge <- function(svm_boundaries, lakes_berlin, crop_waterbodies = TRUE) {
  future::plan(future::multisession)  # Parallele Verarbeitung mit mehreren Prozessoren

  ids <- seq_len(nrow(svm_boundaries))

  results <- future.apply::future_lapply(ids, function(id) {
    message(sprintf("Getting FEFLOW catchment and groundwater recharge for '%s'", svm_boundaries$filename[id]))

    feflow <- svm_boundaries[id,] %>%
      sf::st_transform(25833) %>%
      sf::st_zm()

    if (crop_waterbodies) {
      feflow <- feflow %>%
        sf::st_difference(sf::st_union(lakes_berlin))
    }

    feflow %>%
      dplyr::mutate(area_m2 = sf::st_area(.),
                    #rch_mm_per_year = area_m2 * RCH,
                    geometry = sf::st_union(geometry)) %>%
      dplyr::group_by(geometry) %>%
      dplyr::summarise(area_m2 = sum(area_m2)
      )
  }, future.seed = TRUE)

  results_named <- stats::setNames(results, sprintf("SVM_0%d",ids))
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

