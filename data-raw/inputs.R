config <- kwb.BerlinWaterModel.public::config_read(config_dir = "inst/extdata/config/network_complete")

################################################################################################
# CSO data #####################################################################################
################################################################################################

unzip("inst/extdata/input_data/cso/cso.zip",
      exdir = "inst/extdata/input_data/cso/csv/")

cso_files <- list.files("inst/extdata/input_data/cso/csv/",
                        pattern = "\\.csv$",
                        full.names = TRUE)


cso <- stats::setNames(lapply(cso_files, function(cso_file) {

  readr::read_csv(file = cso_file) %>%
    tidyr::pivot_longer(- tidyselect::all_of(c("Time", "Seconds")),
                        names_to = "ds_link_id",
                        values_to = "cbm_per_second") %>%
    dplyr::rename(datetime = Time, seconds = Seconds) %>%
    dplyr::mutate(datetime = lubridate::dmy_hms(datetime),
                  cbm_per_second = dplyr::if_else(cbm_per_second < 0, 0, cbm_per_second)
    )
}), nm = basename(cso_files)) %>%
  dplyr::bind_rows(.id = "file_name") %>%
  dplyr::relocate(file_name, .before = datetime) %>%
  dplyr::arrange(datetime)

cso <- cso %>%
  dplyr::left_join(config$cso %>% dplyr::select(ds_link_id, CSO_id),
                   by = "ds_link_id") %>%
  dplyr::group_by(file_name, datetime, seconds, CSO_id) %>%
  dplyr::summarise(cbm_per_second = sum(cbm_per_second, na.rm = TRUE))

usethis::use_data(cso, overwrite = TRUE)

### cleanup
fs::dir_delete("inst/extdata/input_data/cso/csv/")

################################################################################################
# flow data surface waters (for inlet flows) ###################################################
################################################################################################

flow_balance_corrected <-  readr::read_csv2("inst/extdata/input_data/q_balance-corrected_2002-2022.csv") %>%
  dplyr::mutate(Datum = lubridate::dmy(Datum)) %>%
  dplyr::rename(date = Datum)

q_stjosephsstieg_meta <- "inst/extdata/input_data/q_St-Joseph-Steg_2002-2022.csv" %>%
  readr::read_csv2(n_max = 1, col_names = FALSE)

id_string <- stringr::str_extract(q_stjosephsstieg_meta$X3, "[0-9]+")

q_stjosephsstieg <- "inst/extdata/input_data/q_St-Joseph-Steg_2002-2022.csv" %>%
  readr::read_csv2(skip = 1, col_names = FALSE) %>%
  dplyr::rename(date = X1,
                {{id_string}} := X2) %>%
  dplyr::mutate(date = lubridate::dmy(date))


q_seeleitung <- "inst/extdata/input_data/q_Seeleitung_2002-2022.csv" %>%
  readr::read_csv2() %>%
  dplyr::rename(date = Datum) %>%
  dplyr::mutate(date = lubridate::dmy(date))


flow_balance_corrected_all <- flow_balance_corrected %>%
  dplyr::full_join(q_stjosephsstieg) %>%
  dplyr::full_join(q_seeleitung) %>%
  dplyr::arrange(date)

inflows <- flow_balance_corrected_all[complete.cases(flow_balance_corrected_all),]


inlet_flow_ids <- config$flows_in_out$flow_id[config$flows_in_out$flow_type == "inlet"]

inflows <- inflows %>%
  tidyr::pivot_longer(cols = names(inflows)[-1],
                      names_to = "id",
                      values_to = "cbm_per_second") %>%
  dplyr::mutate(cbm_per_second = dplyr::if_else(id %in% inlet_flow_ids & cbm_per_second < 0,
                                                0,
                                                cbm_per_second)
                )

usethis::use_data(inflows, overwrite = TRUE)

################################################################################################
# WWTP data ####################################################################################
################################################################################################

wwtp <- "inst/extdata/input_data/KW_WW_monatl_Mittel.csv" %>%
  readr::read_delim(delim = ";", escape_backslash = FALSE, trim_ws = TRUE) %>%
  dplyr::mutate(date = as.Date(lubridate::ceiling_date(lubridate::as_date(sprintf("%d-%02d-01", Jahr, Monat)), "month") - lubridate::days(1)),
                days_in_month = lubridate::day(date))  %>%
  dplyr::relocate(date, days_in_month, .before = Jahr) %>%
  dplyr::select(- Jahr, - Monat, - Monat_Jahr) %>%
  dplyr::select(! tidyselect::contains("WW"))


wwtp_muenchehofe <- readr::read_csv2(file = "inst/extdata/input_data/q_KWMuenchehofe_2002-2022.csv", skip = 1, col_names = c("date", "cbm_per_second")) %>%
  dplyr::mutate(date = lubridate::dmy(date),
                id = "Q_KW_MHF") %>%
  dplyr::select(date, id, cbm_per_second)

wwtp_schoenerlinde <- readr::read_csv2(file = "inst/extdata/input_data/q_KWSchoenerlinde_2002-2022.csv", skip = 1, col_names = c("date", "cbm_per_second")) %>%
  dplyr::mutate(date = lubridate::dmy(date),
                id = "Q_KW_SCH") %>%
  dplyr::select(date, id, cbm_per_second)

wwtp_bc_renamings <- readr::read_csv(file = "inst/extdata/input_data/q_KW_Ruhleben_Stahnsdorf_Wassmannsdorf_bilanzkorrigiert_renamings.csv")

wwtp_bc <- "inst/extdata/input_data/q_KW_Ruhleben_Stahnsdorf_Wassmannsdorf_bilanzkorrigiert_2002-2022.csv" %>%
  readr::read_csv2() %>%
  dplyr::rename(date = Datum) %>%
  dplyr::mutate(date = lubridate::dmy(date)) %>%
  tidyr::pivot_longer(names_to = "id", values_to = "cbm_per_second", - date) %>%
  dplyr::left_join(wwtp_bc_renamings, by = c("id" = "id_old")) %>%
  dplyr::select(- "id") %>%
  dplyr::rename(id = "id_new") %>%
  dplyr::select(date, id, cbm_per_second)

unique(wwtp_bc$id)


wwtp_sch_to_nordgraben <- "inst/extdata/input_data/q_KWSchoenerlinde_Nordgraben_2002-2022.csv" %>%
  readr::read_csv2(col_names = c("date", "Q_KW_SCH_Nordgraben"), skip = 1) %>%
  dplyr::select(- tidyselect::starts_with("X")) %>%
  dplyr::mutate(date = lubridate::dmy(date)) %>%
  tidyr::pivot_longer(names_to = "id", values_to = "cbm_per_second", - date) %>%
  dplyr::select(date, id, cbm_per_second)


wwtp <- wwtp_muenchehofe %>%
  dplyr::bind_rows(wwtp_schoenerlinde) %>%
  dplyr::bind_rows(wwtp_bc) %>%
  dplyr::bind_rows(wwtp_sch_to_nordgraben)


wwtp_wide <- wwtp %>%
  tidyr::pivot_wider(names_from = "id",
                     values_from = "cbm_per_second")

summary(wwtp_wide$Q_KW_SCH - wwtp_wide$Q_KW_SCH_Nordgraben)

usethis::use_data(wwtp, overwrite = TRUE)

################################################################################################
# Water works data (abstraction per gallery, incl. GWA) ########################################
################################################################################################

ww_wide <- "inst/extdata/input_data/Rohwasser_GesamtBericht_Monatlich.xlsx" %>%
  readxl::read_excel(sheet = "Monatsmengen (Galerien)",
                     range = "A4:DK285") %>%
  dplyr::mutate(date = as.Date(lubridate::ceiling_date(lubridate::as_date(sprintf("%d-%02d-01", as.integer(Jahr), as.integer(Monat))), "month") - lubridate::days(1)),
                days_in_month = lubridate::day(date)) %>%
  dplyr::rename(year = Jahr,
              month = Monat) %>%
  dplyr::relocate(date, days_in_month, .before = year) %>%
  dplyr::select(- year, - month, - Datum)

ww_wide_cbm_per_second <- ww_wide %>%
  dplyr::mutate(dplyr::across(-c(date, days_in_month), ~ . / (days_in_month * 24 * 60 * 60)))


ww_long_cbm_per_second <- ww_wide_cbm_per_second %>%
  tidyr::pivot_longer(cols = tidyselect::contains("@"),
                      names_to = "name",
                      values_to = "cbm_per_second") %>%
  tidyr::separate(col = name, into = c("gallery", "flow_type", "waterworks"), sep = "@")


gwa_long_cbm_per_second <- ww_long_cbm_per_second %>%
  dplyr::filter(flow_type == "Grundwasseranreicherung",
                !stringr::str_starts(gallery, "Galeriesumme")) %>%
  dplyr::group_by(date, days_in_month, flow_type, waterworks) %>%
  dplyr::summarise(gallery = sprintf("%sgwa",
                                     stringr::str_sub(dplyr::first(gallery), 1, 3)),
                   cbm_per_second = sum(cbm_per_second, na.rm = TRUE)) %>%
  dplyr::select(date, days_in_month, gallery, flow_type, waterworks, cbm_per_second)

gwh_altlasten_long_cbm_per_second <- ww_long_cbm_per_second %>%
  dplyr::filter(flow_type %in% c("Grundwasserhaltung", "Altlastenabwehr"),
                waterworks == "WW Johannisthal",
                stringr::str_starts(gallery, "Galeriesumme", negate = TRUE)) %>%
  dplyr::mutate(gallery = stringr::str_remove(gallery, "-+?.*")) %>%
  dplyr::select(date, days_in_month, gallery, flow_type, waterworks, cbm_per_second)


ww <- ww_long_cbm_per_second %>%
  dplyr::filter(flow_type == "Rohwassergewinnung",
                ! stringr::str_starts(gallery, "Galeriesumme")) %>%
  dplyr::bind_rows(gwa_long_cbm_per_second) %>%
  dplyr::bind_rows(gwh_altlasten_long_cbm_per_second) %>%
  dplyr::mutate(id = gallery %>% stringr::str_remove("-.*")) %>%
  dplyr::select(date, id, flow_type, cbm_per_second)


# ww <- ww %>%
#   dplyr::bind_rows(ww %>%
#                      dplyr::mutate(id = stringr::str_sub(id, 1, 3)) %>%
#                      dplyr::group_by(date, id) %>%
#                      dplyr::summarise(cbm_per_second = sum(cbm_per_second, na.rm = TRUE)))

usethis::use_data(ww, overwrite = TRUE)

################################################################################################
# Share Panke to Nordgraben time series ########################################################
################################################################################################

shares_timeseries <- readr::read_csv2(file = "inst/extdata/input_data/share_Panke_to_Nordgraben_2002-2022.csv") %>%
  dplyr::rename(date = Datum) %>%
  dplyr::mutate(date = lubridate::dmy(date)) %>%
  tidyr::pivot_longer(names_to = "id", values_to = "share", - date) %>%
  dplyr::select(date, id, share)


shares_timeseries <- shares_timeseries %>%
  dplyr::bind_rows(shares_timeseries %>%
                     dplyr::mutate(id  =  dplyr::if_else(id == "share_Panke_to_Nordgraben",
                                                         "share_Panke_to_BSSKanal",
                                                         id),
                                   share = dplyr::if_else(id == "share_Panke_to_BSSKanal",
                                                          1 - share,
                                                          share)))

#shares_timeseries %>% tidyr::pivot_wider(names_from = "id", values_from = "share")

usethis::use_data(shares_timeseries, overwrite = TRUE)
