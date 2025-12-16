#' Helper function: get link names
#'
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#'
#' @return tibble with link names and columns source and target
#' @export
get_link_names <- function(network) {

  network %>%
    dplyr::select(from_name, to_name, weight) %>%
    dplyr::rename(source = from_name, target = to_name) %>%
    dplyr::distinct(.) %>%
    dplyr::rename(value = weight) %>%
    dplyr::filter(!stringr::str_starts(target, "MO"))
}

#' Helper function: get nodes
#'
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#' @param config config as retrieved by \code{\link{config_read}}
#' @return tibble with columns name (from_name) and group (source_group)
#' @export
get_nodes <- function(network, config) {

  network %>%
    dplyr::select(from_name, source_group) %>%
    dplyr::rename(name = from_name, group = source_group) %>%
    dplyr::bind_rows(
      network %>%
        dplyr::select(to_name, target_group) %>%
        dplyr::rename(name = to_name, group = target_group)
    ) %>%
    dplyr::distinct(.) %>%
    dplyr::arrange(tolower(name)) %>%
    dplyr::left_join(config$sections[, c("section_name", "volume_m3")], by = c("name" = "section_name")) %>%
    dplyr::mutate(nodesize = sqrt(volume_m3 / 10000)) %>%
    dplyr::filter(!stringr::str_starts(name, "MO"))
}


#' Helper: shorten WW flow id
#'
#' @param df df with WW flow data andf flow ids as columns
#' @param col_flow_id column name with flow id (default: "flow_id")
#'
#' @return vector with shortened WW flow ids
#' @export
#'
#' @importFrom dplyr ensym
#' @importFrom stringr str_detect str_split_fixed
shorten_ww_flow_id <- function(df, col_flow_id = "flow_id") {

  stopifnot(col_flow_id %in% names(df))


  flow_id <- dplyr::ensym(col_flow_id)

  df %>%
    dplyr::mutate(!!flow_id := dplyr::if_else(
      stringr::str_detect(!!flow_id, "^WW"),
      !!flow_id %>%
        stringr::str_remove("WW_") %>%
        stringr::str_split_fixed(pattern = "_", 2) %>%
        as.data.frame() %>%
        dplyr::mutate(V2 = tolower(V2),
                      !!flow_id := paste0(V1, V2)) %>%
        dplyr::pull(!!flow_id),
      !!flow_id
    ))

}


#' Helper Get date or datetime columns
#'
#' @param df data frame
#'
#' @return names of columns of class Date or POSIXct
#' @keywords internal
#'
get_date_or_datetime_columns <- function(df) {
  # Initialisieren einer leeren Liste, um die Namen der Spalten zu speichern
  date_or_datetime_columns <- c()

  # Durch alle Spalten des DataFrames iterieren
  for (col_name in names(df)) {
    # Überprüfen, ob der Typ der Spalte Date oder POSIXct ist
    if (inherits(df[[col_name]], "Date") ||
        inherits(df[[col_name]], "POSIXct")) {
      # Den Namen der Spalte zur Liste hinzufügen
      date_or_datetime_columns <- c(date_or_datetime_columns, col_name)
    }
  }

  # Rückgabe der Liste der Spaltennamen
  return(date_or_datetime_columns)
}


#' Helper function: get section idnames
#'
#' @param config config as retrieved by \code{\link{config_read}}
#'
#' @return tibble with section names and ids contained in configuration
#' @export
#'
get_section_idnames <- function(config) {

  outflows_multiple <- config$outflows_multiple %>%
    dplyr::mutate(section_out_share = as.numeric(section_out_share))

  sections_selected <- config$sections %>%
    dplyr::mutate("weight" = 1) %>%
    dplyr::rename("from_id" = section_in_id, "to_id" = section_out_id) %>%
    dplyr::select("section_id", "from_id", "to_id", "weight")


  sections_idname <- config$sections %>%
    dplyr::select(section_id, section_name) %>%
    dplyr::rename(id = section_id, name = section_name) %>%
    dplyr::bind_rows(outflows_multiple %>%
                       dplyr::select(section_out_id, section_out_name) %>%
                       dplyr::rename(id = section_out_id,
                                     name = section_out_name)) %>%
    dplyr::arrange(id)


  values <- c(sections_selected$from_id, sections_selected$to_id)
  undefined_section_id <- values[stringr::str_starts(values, "S|H")][!values[stringr::str_starts(values, "S|H")] %in% sections_idname$id]

  sections_idname <- if (length(undefined_section_id) > 0) {
    sections_idname %>%
      dplyr::bind_rows(tibble::tibble(id = undefined_section_id, name = undefined_section_id)) %>%
      dplyr::arrange(id)
  } else {
    sections_idname
  }

  sections_idname %>%
    dplyr::distinct() %>%
    dplyr::arrange(id) %>%
    dplyr::filter(!is.na(id))
}


#' Helper function: get names from ids
#'
#' @param vector vector with ids
#' @param config config as retrieved by \code{\link{config_read}}
#'
#' @return tibble with section names and ids contained in configuration
#' @export
#'
#'
get_names_from_ids <- function(vector, config) {

  outflows_multiple_idname <- config$outflows_multiple %>%
    dplyr::select(section_out_id, section_out_name) %>%
    dplyr::rename(id =  section_out_id, name = section_out_name)

  flows_in_out_idname <- config$flows_in_out %>%
    dplyr::select(flow_id, flow_name) %>%
    dplyr::rename(id = flow_id, name = flow_name)


  idname <- get_section_idnames(config) %>%
    dplyr::bind_rows(outflows_multiple_idname) %>%
    dplyr::bind_rows(flows_in_out_idname) %>%
    dplyr::count(id, name) %>%
    dplyr::arrange(id) %>%
    dplyr::select(id, name)

  sapply(vector, function(x) {
    cond <- which(idname$id == x)
    if (length(cond) > 0) {
      idname$name[cond]
    } else {
      x
    }
  }) %>% as.character()
}


#' Helper function: get ids from names
#'
#' @param vector vector with names
#' @param config config as retrieved by \code{\link{config_read}}
#'
#' @return tibble with section names and ids contained in configuration
#' @export
#'
#'
get_ids_from_names <- function(vector, config) {

  outflows_multiple_idname <- config$outflows_multiple %>%
    dplyr::select(section_out_id, section_out_name) %>%
    dplyr::rename(id =  section_out_id, name = section_out_name)

  flows_in_out_idname <- config$flows_in_out %>%
    dplyr::select(flow_id, flow_name) %>%
    dplyr::rename(id = flow_id, name = flow_name)

  idname <- get_section_idnames(config) %>%
    dplyr::bind_rows(outflows_multiple_idname) %>%
    dplyr::bind_rows(flows_in_out_idname) %>%
    dplyr::count(id, name) %>%
    dplyr::arrange(id) %>%
    dplyr::select(id, name)


  sapply(vector, function(x) {
    cond <- which(idname$name == x)

    if(length(cond) != 1) {
      stop(sprintf("'flow_name' == '%s' is detected %d times but needs to be unique. Please fix 'flows_in_out' and assure that 'flow_name' is unique for each 'flow_id'",
                   x,
                   length(cond))
           )
    }

    idname$id[cond]

  }) %>% as.character()
}

#' Check if function and calculate flow
#'
#' @param df data frame with one row containing column 'section_out_function_parsed'
#' with function for calculating flow
#' @param q flow vector
#' @returns if function: calculated flow vector, if not: NA_real
#' @export
#'
check_if_function_and_calculate_flow <- function(df, q) {

stopifnot("section_out_function_parsed" %in% names(df))

if(is.function(df$section_out_function_parsed[[1]])) {
  ifelse(df$section_out_function_parsed[[1]](q) < 0,
         0,
         df$section_out_function_parsed[[1]](q))
} else {
  NA_real_
}

}


#' Format numeric values with significant digits and minimal trailing zeros
#'
#' This function formats numeric values to a specified number of significant digits,
#' avoiding scientific notation and unnecessary trailing zeros. Useful for plotting labels
#' where readability is important across varying numeric magnitudes.
#'
#' @param x Numeric vector of values to be formatted.
#' @param digits Integer. Number of significant digits to retain (default is 2).
#'
#' @returns A character vector of formatted numbers.
#' @export
#'
#' @examples
#' label_signif_clean(c(0.0001234, 0.0456, 1.23, 12.3, 123.4))
label_signif_clean <- function(x, digits = 2) {
  sapply(x, function(val) {
    format(signif(val, digits = digits), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  })
}

#' Generate minor tick marks for a log10 axis
#'
#' Returns a function that computes minor tick positions for a log10-scaled axis.
#' This is useful in conjunction with `scale_y_log10()` or `scale_x_log10()` in `ggplot2`,
#' where minor breaks are not added by default.
#'
#' @param minor_base A numeric vector of factors (default is 1:9) to be multiplied by each power of 10.
#' This defines which intermediate ticks (e.g., 2, 3, ..., 9) are included between each decade (e.g., between 0.1 and 1).
#'
#' @return A function that takes a numeric range and returns a numeric vector of minor tick positions.
#'
#' @examples
#' \dontrun{
#' ggplot2::scale_y_log10(
#'   limits = c(0.01, 100),
#'   breaks = c(0.01, 0.1, 1, 10, 100),
#'   minor_breaks = kwb.BerlinWaterModel.public::log10_minor_breaks()
#' )
#'}
#' @export
log10_minor_breaks <- function(minor_base = 1:9) {
  function(x) {
    decade <- floor(log10(range(x, na.rm = TRUE)))
    unlist(lapply(decade[1]:decade[2], function(d) minor_base * 10^d))
  }
}

#' Calculate flow share
#'
#' @param flow vector with flows (same length as)
#' @param shares_timeseries_id id column in shares_timeseries_wide
#' @param shares_timeseries_wide tibble or data frame with shares time series
#' data in wide format
#' @param debug print debug messages (default: FALSE)
#' @returns flow vector corrected with values from shares_timeseries data.frame
#' for provided shares_timeseries_id
#' @export
calculate_flow_share <- function(flow, shares_timeseries_id, shares_timeseries_wide, debug = FALSE) {

  shares_timeseries_wide_txt <- deparse(substitute(shares_timeseries_wide))

if(!is.na(shares_timeseries_id) & is.data.frame(shares_timeseries_wide) & (shares_timeseries_id %in% names(shares_timeseries_wide))) {

  stopifnot(length(flow) == nrow(shares_timeseries_wide))

  if(debug) {
    message(sprintf("Calculating flow share with shares_timeseries_id '%s' using dataset '%s'",
                    shares_timeseries_id,
                    shares_timeseries_wide_txt))
  }

  flow * shares_timeseries_wide[[shares_timeseries_id]]
} else {
  NA_real_
}

}


#' Get Reverse Flows Per Section
#'
#' @param flows flows
#'
#' @returns tibble with sections ordered decreasing by number of data points below
#' zero and date_min (first neg. flow) and date_max (last neg. flow) within dataset
#' @export
#' @importFrom dplyr arrange desc filter group_by n summarise
#'
get_reverse_flows_per_section <- function(flows) {

temporal_resolution <- attr(flows, "temporal_resolution")

col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                               "datetime",
                               "date")

col_date_or_datetime_min <- sprintf("%s_min", col_date_or_datetime)
col_date_or_datetime_max <- sprintf("%s_max", col_date_or_datetime)

flows_long <- flows %>%
  tidyr::pivot_longer(names_to = "section_id",
                      values_to = "cbm_per_second",
                      - tidyselect::any_of(col_date_or_datetime))


flows_sections_below_zero <- flows_long %>%
  dplyr::filter(cbm_per_second < 0) %>%
  dplyr::group_by(section_id) %>%
  dplyr::summarise(
    n_flow_below_zero = dplyr::n(),
    "{col_date_or_datetime_min}" := min(.data[[col_date_or_datetime]], na.rm = TRUE),
    "{col_date_or_datetime_max}" := max(.data[[col_date_or_datetime]], na.rm = TRUE),
    cbm_per_second.mean = mean(cbm_per_second, na.rm = TRUE),
    cbm_per_second.median = median(cbm_per_second, na.rm = TRUE),
    cbm_per_second.sd = sd(cbm_per_second, na.rm = TRUE),
    cbm_per_second.min = min(cbm_per_second, na.rm = TRUE),
    cbm_per_second.max = max(cbm_per_second, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_flow_below_zero > 0) %>%
  dplyr::arrange(dplyr::desc(n_flow_below_zero))

flows_sections_below_zero
}


#' Calculate Flow Statistics Per Section
#'
#' Computes discharge statistics from a daily or hourly flow dataset
#' for each river section. The function supports both daily data
#' (using a `date` column) and hourly data (using a `datetime` column),
#' depending on the `"temporal_resolution"` attribute of the input.
#'
#' The returned statistics include:
#' * **MQ** – Mean discharge
#' * **MNQ_years** – Mean of yearly minimum values (yearly low flows)
#' * **MHQ_years** – Mean of yearly maximum values (yearly high flows)
#' * **NNQ** – Overall minimum discharge (absolute low flow)
#' * **HHQ** – Overall maximum discharge (absolute high flow)
#'
#' Additional tables contain statistics per year and per month, including
#' monthly mean, monthly minimum, and monthly maximum discharges.
#'
#' @param flows A data frame containing a date or datetime column
#'   (depending on `attr(flows, "temporal_resolution")`) followed by
#'   one column per river section with discharge values.
#'
#' @returns
#' A list with three elements:
#' \describe{
#'   \item{per_section}{A tibble with discharge statistics per section
#'     (MQ, MNQ_years, MHQ_years, NNQ, HHQ).}
#'   \item{per_year}{A tibble with statistics per section and year
#'     (MQ, MNQ_months, MHQ_months, NQ, HQ).}
#'   \item{per_month}{A tibble with statistics per section and month
#'     (MQ, MNQ_same_months, MHQ_same_months, NQ, HQ).}
#' }
#'
#' @export
calculate_flow_stats <- function(flows) {

  temporal_resolution <- attr(flows, "temporal_resolution")

  col_date_or_datetime <- ifelse(temporal_resolution == "hours",
                                 "datetime",
                                 "date")

  flows_long <- flows %>%
    tidyr::pivot_longer(cols = -1,
                        names_to = "section_id",
                        values_to = "cbm_per_second") %>%
    dplyr::left_join(get_section_idnames(config) %>%
                       dplyr::rename(section_id = id,
                                     section_name = name),
                     by = "section_id") %>%
    dplyr::relocate("section_name", .after = "section_id")

  # add columns for year and month
    if (col_date_or_datetime == "date") {
    flows_long_years_months <- flows_long %>%
      dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date))
    message("Berechnung der Gewässerkennwerte aus den Tageswerten.")
    } else  {
    flows_long_years_months <- flows_long %>%
      dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime))
      message("Berechnung der Gewässerkennwerte aus den Stundenwerten. Üblicherweise werden die Gewässerkennwerte aus den Tagesmittelwerten berechnet!")
    }

  flows_stats_section <- flows_long_years_months %>%
    dplyr::group_by(section_id, section_name) %>%
    dplyr::summarise(
      MQ = round(mean(cbm_per_second, na.rm = TRUE), 3),
      MNQ_years = round(mean(
        cbm_per_second[!is.na(cbm_per_second)] %>%
          split(year[!is.na(cbm_per_second)]) %>%
          sapply(min),
        na.rm = TRUE), 3),
      MHQ_years = round(mean(
        cbm_per_second[!is.na(cbm_per_second)] %>%
          split(year[!is.na(cbm_per_second)]) %>%
          sapply(max),
        na.rm = TRUE), 3),
      NNQ = round(min(cbm_per_second, na.rm = TRUE), 3),
      HHQ = round(max(cbm_per_second, na.rm = TRUE), 3)
    )

  flows_stats_per_year <- flows_long_years_months %>%
    dplyr::group_by(section_id, section_name, year) %>%
    dplyr::summarise(
      MQ = round(mean(cbm_per_second, na.rm = TRUE), 3),
      MNQ_months = round(mean(
        cbm_per_second[!is.na(cbm_per_second)] %>%
          split(month[!is.na(cbm_per_second)]) %>%
          sapply(min),
        na.rm = TRUE), 3),
      MHQ_months = round(mean(
        cbm_per_second[!is.na(cbm_per_second)] %>%
          split(month[!is.na(cbm_per_second)]) %>%
          sapply(max),
        na.rm = TRUE), 3),
      NQ = round(min(cbm_per_second, na.rm = TRUE), 3),
      HQ = round(max(cbm_per_second, na.rm = TRUE), 3)
    )

  flows_stats_per_month <- flows_long_years_months %>%
    dplyr::group_by(section_id, section_name, month, year) %>%
    dplyr::summarise(NQ = min(cbm_per_second, na.rm = TRUE),
                     MQ = mean(cbm_per_second, na.rm = TRUE),
                     HQ = max(cbm_per_second, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(section_id, section_name, month) %>%
    dplyr::summarise(
      MQ = round(mean(MQ), 3),
      MNQ_same_months = round(mean(NQ), 3),
      MHQ_same_months = round(mean(HQ), 3),
      NQ = round(min(NQ), 3),
      HQ = round(max(HQ), 3)
    )

  list(per_section = flows_stats_section,
       per_year = flows_stats_per_year,
       per_month = flows_stats_per_month)
}

#' Merge nested two-level lists while preserving structure
#'
#' This function merges two nested lists of the form
#' \code{list(top_id = list(sub_id = tibble))}, as used in the
#' BerlinWaterModel package. The structure (top-level and sub-level keys)
#' is preserved, and missing elements are added. If the same element exists
#' in both lists, the preferred side can be chosen.
#'
#' @param x A nested list with structure \code{list(top_id = list(sub_id = tibble))}.
#' @param y A nested list with the same structure as \code{x}.
#' @param prefer Character string, either \code{"left"} (default) or \code{"right"}.
#'   If \code{"left"}, values from \code{x} are kept in case of duplicates.
#'   If \code{"right"}, values from \code{y} overwrite those in \code{x}.
#'
#' @return A nested list with the same two-level structure containing the
#'   union of all elements from \code{x} and \code{y}.
#'
#' @examples
#' library(tibble)
#'
#' a <- list("S03" = list("S21" = tibble(a = 1, b = 2)))
#' b <- list("S03" = list("S07" = tibble(a = 3, b = 4)))
#' c <- list("S01" = list("S02" = tibble(a = 5, b = 6)))
#'
#' # Default: prefer = "left"
#' merge_two_level_lists(a, b)
#'
#' # Right side overwrites in case of duplicates
#' merge_two_level_lists(a, b, prefer = "right")
#'
#' # Merge more than two lists using purrr::reduce
#' purrr::reduce(list(a, b, c), merge_two_level_lists)
#'
#' @export
merge_two_level_lists <- function(x, y, prefer = c("left", "right")) {
  prefer <- match.arg(prefer)

  all_top <- union(names(x), names(y))

  stats::setNames(lapply(all_top, function(top_key) {
    sx <- x[[top_key]]
    sy <- y[[top_key]]

    if (is.null(sx)) return(sy)
    if (is.null(sy)) return(sx)

    # Beide Sublisten vorhanden -> Subkeys vereinigen und je nach 'prefer' zusammenführen
    all_sub <- union(names(sx), names(sy))
    stats::setNames(lapply(all_sub, function(sub_key) {
      vx <- sx[[sub_key]]
      vy <- sy[[sub_key]]

      if (is.null(vx)) return(vy)
      if (is.null(vy)) return(vx)

      # Beide vorhanden:
      if (prefer == "left") vx else vy
    }), all_sub)
  }), all_top)
}
