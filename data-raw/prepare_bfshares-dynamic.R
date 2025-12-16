tbl <- readxl::read_xlsx(path = "inst/extdata/input_data/Brunnen-Galerien-Typ-Gleichung_v1.0.0.xlsx",
                         sheet = "BWB-Galerien-Linie") %>%
  janitor::clean_names() %>%
  dplyr::rename(gallery = gallery_name) %>%
  dplyr::select(- surface_water_contribution)



cols_selected <- c("a", "b", "r2", "rmse", "qmin_m3_d","qmax_m3_d")



bfstypes_equations <- tbl %>%
  dplyr::count(a, b, r2, rmse, qmin_m3_d, qmax_m3_d) %>%
  dplyr::arrange(a, b) %>%
  dplyr::mutate(type = 1:dplyr::n()) %>%
  dplyr::filter(!is.na(a), !is.na(b)) %>%
  dplyr::relocate(type, .before = a) %>%
  dplyr::select(- n)


bfstypes_wellgalleries <- tbl %>%
  dplyr::left_join(bfstypes_equations) %>%
  dplyr::select(- tidyselect::all_of(c("a", "b", "r2", "rmse", "qmin_m3_d", "qmax_m3_d"))) %>%
  dplyr::arrange(waterworks, gallery)


bfstypes_equations <- bfstypes_equations  %>%
  dplyr::rename(equation_a = a,
                equation_b = b,
                Q_min_m3d = qmin_m3_d,
                Q_max_m3d = qmax_m3_d)



readr::write_csv2(bfstypes_wellgalleries, "inst/extdata/config/network_complete/bfstypes_wellgalleries.csv")
readr::write_csv2(bfstypes_equations, "inst/extdata/config/network_complete/bfstypes_equations.csv")
