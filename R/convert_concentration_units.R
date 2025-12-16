#' Convert concentration units to kg/m3
#'
#' @param df data frame with "conc_<substance-short-name>.<g|mg|mg|ng>.<L>"
#' @param return_inputs should input data frame also be returned (default: FALSE)
#' or only newly calculated columns?
#'
#' @returns tibble with "conc_<substance-short-name>.kg.m3"
#' @export
#' @importFrom units as_units set_units
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @examples
#' df <- data.frame(
#' conc_As.ng.L = c(10, 50, 100),  # Nanogramm pro Liter
#' conc_Zn.ug.L = c(500, 1000, 2000),  # Mikrogramm pro Liter
#' conc_Fe.mg.L = c(0.1, 0.2, 0.3),    # Milligramm pro Liter
#' conc_Cu.g.L = c(0.005, 0.01, 0.02)  # Gramm pro Liter
#')
#'
#' convert_concentration_units(df, return_inputs = TRUE)
#'
convert_concentration_units <- function(df, return_inputs = FALSE) {

  pattern_substances <- "conc_([A-Za-z]+)"
  pattern_conc <- "conc_([A-Za-z]+)\\.(ng.L|ug.L|mg.L|g.L)"

  conc_input_check_df <- stringr::str_split_fixed(names(df)[stringr::str_detect(names(df), pattern_substances)],
                     pattern = "\\.", n = 3) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::rename(mass = V2,
                  volume = V3) %>%
    tidyr::separate(V1, into = c("data_type", "substance_name"))

  stopifnot(all(unique(conc_input_check_df$mass) %in% c("ng","ug", "mg", "g")))
  stopifnot(all(unique(conc_input_check_df$volume) %in% c("L")))

  new_df <- lapply(colnames(df), function(col) {
    match <- regexpr(pattern_conc, col, perl = TRUE)

    if (match[1] != -1) {
      parts <- regmatches(col, regexec(pattern_conc, col))[[1]]
      element <- parts[2]  # Zn, Fe, Cu, etc.

      # Definiere Einheit basierend auf der Namenskonvention
      unit_full <- stringr::str_replace(parts[3], "\\.", "/")  # ng.L, ug.L, mg.L, g.L

      # Setze Einheiten mit `units`
      column_with_units <-  df[[col]] * units::as_units(unit_full)

      # Konvertiere in mg/m3
      converted_column <- units::set_units(column_with_units, "mg/m^3")

      # Neuen Spaltennamen setzen
      new_colname <- paste0("conc_", element, ".mg.m3")

      # Numerischen Wert extrahieren
      tibble::tibble(!! new_colname := as.numeric(converted_column))
    }
    }) %>% dplyr::bind_cols()

  if(return_inputs) {
    df %>%
      dplyr::bind_cols(new_df)
  } else {
    df %>%
      dplyr::select("flow_type", "id_base") %>%
      dplyr::bind_cols(new_df)
  }
}


