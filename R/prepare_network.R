#' Prepare Network Data
#'
#' @param config config as retrieved by \code{\link{config_read}}
#'
#' @returns data structure for plot_network functions and for further calculations
#' @export
#' @importFrom dplyr count if_else rename
#' @importFrom stringr str_remove
prepare_network <- function(config) {

  if (!"outflows_multiple" %in% names(config)) {
    stop("Das 'config'-Objekt enth\u00E4lt kein 'outflows_multiple'.")
  }

  if (is.null(config$outflows_multiple)) {
    stop("'outflows_multiple' ist NULL.")
  }

  outflows_multiple_idname <- config$outflows_multiple %>%
    dplyr::select(section_out_id, section_out_name) %>%
    dplyr::rename(id =  section_out_id, name = section_out_name)

  outflows_multiple <- config$outflows_multiple %>%
    #dplyr::mutate(to_id = stringr::str_remove(outflow_id, "MO_"))
    dplyr::mutate(section_out_share = as.numeric(section_out_share))


  sections_fromtoid <- config$sections %>%
    dplyr::filter(is.na(section_in_id) | is.na(section_out_id)) %>%
    dplyr::mutate(from_id = section_in_id, to_id = section_id) %>%
    dplyr::bind_rows(config$sections %>%
                       dplyr::mutate(from_id = section_id, to_id = section_out_id)) %>%
    dplyr::filter(! stringr::str_starts(to_id, "MO")) %>%
    dplyr::select(from_id, to_id) %>%
    dplyr::distinct(.) %>%
    dplyr::arrange(from_id)

  sections_idname <- get_section_idnames(config)

  outflows_multiple_selected <- outflows_multiple[stringr::str_remove(outflows_multiple$outflow_id, "MO_") %in% sections_idname$id, ]

  outflows_multiple_selected <- outflows_multiple_selected %>%
    dplyr::mutate(from_id = stringr::str_remove(outflow_id, "MO_")) %>%
    dplyr::rename("weight" = section_out_share, "to_id" = section_out_id) %>%
    dplyr::select("from_id", "to_id", "weight")

  flows_in_out_idname <- config$flows_in_out %>%
    dplyr::select(flow_id, flow_name) %>%
    dplyr::rename(id = flow_id, name = flow_name)

  idname <- sections_idname %>%
    dplyr::bind_rows(outflows_multiple_idname) %>%
    dplyr::bind_rows(flows_in_out_idname) %>%
    dplyr::count(id, name) %>%
    dplyr::arrange(id) %>%
    dplyr::select(id, name)


  flows_in_out_selected <- config$flows_in_out %>%
    dplyr::mutate(
      from_id = dplyr::if_else(
        stringr::str_detect(flow_in_out, "out"),
        section_id,
        flow_id
      ),
      to_id = dplyr::if_else(
        stringr::str_detect(flow_in_out, "out"),
        flow_id,
        section_id
      )
    ) %>%
    dplyr::left_join(idname, by = c("from_id" = "id")) %>%
    dplyr::rename("from_name" = name) %>%
    dplyr::left_join(idname, by = c("to_id" = "id")) %>%
    dplyr::rename("to_name" = name) %>%
    dplyr::select("from_id", "from_name", "to_id", "to_name")

  network <- sections_fromtoid %>%
    dplyr::filter(!stringr::str_detect(to_id, "MO_")) %>%
    dplyr::bind_rows(outflows_multiple_selected)  %>%
    dplyr::distinct(.) %>%
    dplyr::arrange(from_id)

  network <- network %>%
    dplyr::mutate(from_name = get_names_from_ids(from_id, config),
                  to_name = get_names_from_ids(to_id, config)) %>%
    dplyr::bind_rows(flows_in_out_selected) %>%
    dplyr::filter(from_name != "inlet") %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight), 1, weight))

  network$source_group <- dplyr::case_when(
    stringr::str_starts(network$from_id, "H[0-9]+|S[0-9]+") ~ "Fliessabschnitt",
    stringr::str_starts(network$from_id, "Q_KW") ~ "Klarwasser",
    stringr::str_starts(network$from_id, "ageb") ~ "Regenwasser (Oberfl\u00E4chenabfluss)",
    stringr::str_starts(network$from_id, "rain\\.direct") ~ "Regenwasser (direkt)",
    stringr::str_starts(network$from_id, "evapo\\.p") ~ "Verdunstung (potentiell)",
    stringr::str_starts(network$from_id, "Seeleitung") ~ "Seeleitung",
    stringr::str_starts(network$from_id, "WW") ~ "Trinkwasserf\u00F6rderung",
    stringr::str_starts(network$from_id, "MW\u00DC") ~ "Mischwasser\u00FCberlauf",
    .default =  "Zufluss"
  )

  network$target_group <- dplyr::case_when(
    stringr::str_starts(network$to_id, "H[0-9]+|S[0-9]+") ~ "Fliessabschnitt",
    stringr::str_starts(network$to_id, "Q_KW") ~ "Klarwasser",
    stringr::str_starts(network$to_id, "ageb") ~ "Regenwasser (Oberfl\u00E4chenabfluss)",
    stringr::str_starts(network$to_id, "rain\\.direct") ~ "Regenwasser (direkt)",
    stringr::str_starts(network$to_id, "evapo\\.p") ~ "Verdunstung (potentiell)",
    stringr::str_starts(network$to_id, "Seeleitung") ~ "Seeleitung",
    stringr::str_starts(network$to_id, "WW") ~ "Trinkwasserf\u00F6rderung",
    stringr::str_starts(network$to_id, "MW\u00DC") ~ "Mischwasser\u00FCberlauf",
    stringr::str_starts(network$to_id, "Out") ~ "Outflow",
    .default =  "Undefiniert"
  )

  network_flows <- network %>%
    dplyr::group_by(from_id) %>%
    dplyr::filter(stringr::str_starts(from_id, "H|S"),
                  stringr::str_starts(to_id, "S|H|Out"),
                  stringr::str_starts(to_id, "Seeleitung",negate = TRUE)) %>%
    dplyr::summarise(weight_sum = sum(weight))

  if(!all(network_flows$weight_sum == 1)) {
    network_flows_wrong <- network_flows %>%
      dplyr::filter(weight_sum != 1) %>%
      dplyr::arrange(from_id)

    stop(sprintf("The sum of all out outflows for the following %d section(s) does not equal 1:\n%s\n%s",
                 nrow(network_flows_wrong),
                 paste0(network_flows_wrong$from_id,
                        ": ",
                        get_names_from_ids(network_flows_wrong$from_id, config),
                        " (flow sum = ",
                         network_flows_wrong$weight_sum,
                         ")",
                         collapse = ",\n"),
                 "Please fix it in the configuration files 'sections' and/or 'outflows_multiple'!"))
  }

  return(network)

}
