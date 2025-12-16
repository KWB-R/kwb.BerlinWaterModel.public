#' Combine and clean dataframe
#'
#' @param list list miwth Urban Water Model results
#'
#' @return tibble with Urban Water Model results
#' @export
#' @importFrom  purrr reduce
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#'
combine_and_clean_dfs <- function(list) {
  time_columns <- c("date", "datetime")

  # Finde die gemeinsame Zeitspalte
  time_col <- time_columns[time_columns %in% names(list[[1]])]

  if (length(time_col) == 0) {
    stop("Keine gemeinsame Zeitspalte gefunden.")
  }

  time_col <- time_col[1]  # Nutze die erste gefundene Zeitspalte

  # Kombiniere die DataFrames anhand der Zeitspalte
  combined <- purrr::reduce(list, dplyr::full_join, by = time_col)

  # Identifiziere die Spaltennamen ohne die Zeitspalte
  col_names <- setdiff(names(combined), time_col)

  # Erstelle eine Liste von Spaltennamen ohne Suffixe
  base_col_names <- unique(sub("\\..*", "", col_names))

  # Initialisiere eine leere Liste zum Speichern der bereinigten Spalten
  clean_data <- list()
  clean_data[[time_col]] <- combined[[time_col]]

  # Prüfe doppelte Spalten und füge sie zur bereinigten Liste hinzu
  for (col in base_col_names) {
    cols_to_check <- combined %>% dplyr::select(matches(paste0("^", col, "(\\.x|\\.y|\\.\\d+)?$")))

    # Prüfe, ob die Werte in allen doppelten Spalten identisch sind
    if (ncol(cols_to_check) > 1) {
      are_equal <- apply(cols_to_check, 1, function(row) length(unique(row[!is.na(row)])) == 1)

      if (all(are_equal)) {
        # Wenn alle Werte in den doppelten Spalten identisch sind, nutze den gemeinsamen Wert
        clean_data[[col]] <- cols_to_check[[1]]
      } else {
        # Wenn die Werte nicht identisch sind, summiere die Werte
        clean_data[[col]] <- rowSums(cols_to_check, na.rm = TRUE)
      }
    } else {
      # Wenn es nur eine Spalte gibt, füge die Spalte hinzu
      clean_data[[col]] <- cols_to_check[[1]]
    }
  }

  # Konvertiere die Liste zurück in ein tibble
  result <- tibble::as_tibble(clean_data)

  # Sortiere die Spaltennamen, wobei die Zeitspalte immer an erster Stelle bleibt
  sorted_col_names <- c(time_col, sort(setdiff(names(result), time_col), method = "radix"))
  result <- result %>% dplyr::select(tidyselect::all_of(sorted_col_names))

  return(result)
}
