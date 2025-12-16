#' Check Network Errors
#'
#' @param network tibble with water cycle flow network network, as retrieved by
#' \code{{\link{prepare_network}}}
#'
#' @returns list with information on network errors
#' @export
#'
check_network_errors <- function(network) {
  # Pruefen, ob die benoetigten Spalten vorhanden sind
  if (!all(c("from_name", "to_name") %in% colnames(network))) {
    stop("Die Daten muessen die Spalten 'from_name' und 'to_name' enthalten.")
  }

  # Pruefen, ob die Spalten leer sind
  if (any(is.na(network$from_name)) || any(is.na(network$to_name))) {
    stop("Die Spalten 'from_name' und 'to_name' duerfen keine leeren Werte enthalten.")
  }

  # Extrahieren der einzigartigen Werte aus den Spalten
  from_names <- unique(network$from_name)
  to_names <- unique(network$to_name)

  # Finden der fehlenden Verbindungen
  missing_to <- setdiff(from_names, to_names)
  missing_from <- setdiff(to_names, from_names)

  # Pruefen, ob alle Werte sowohl als 'from_name' als auch 'to_name' verwendet werden
  unused_from <- setdiff(from_names, network$to_name)
  unused_to <- setdiff(to_names, network$from_name)

  # Ergebnis zurueckgeben
  if (length(missing_to) == 0 && length(missing_from) == 0 && length(unused_from) == 0 && length(unused_to) == 0) {
    message("Es gibt keine Netzwerkfehler. Jede 'from_name' hat eine entsprechende 'to_name' und umgekehrt. Alle Knoten werden verwendet.")
    return(NULL)
  } else {
    if (length(missing_to) > 0) {
      message("Netzwerkfehler gefunden: Die folgenden 'from_name' haben keine entsprechende 'to_name':")
      print(missing_to)
    }
    if (length(missing_from) > 0) {
      message("Netzwerkfehler gefunden: Die folgenden 'to_name' werden nicht als 'from_name' verwendet:")
      print(missing_from)
    }
    if (length(unused_from) > 0) {
      message("Die folgenden 'from_name' werden nicht als 'to_name' verwendet und sind somit isoliert:")
      print(unused_from)
    }
    if (length(unused_to) > 0) {
      message("Die folgenden 'to_name' werden nicht als 'from_name' verwendet und sind somit isoliert:")
      print(unused_to)
    }
    return(list(missing_to = missing_to, missing_from = missing_from, unused_from = unused_from, unused_to = unused_to))
  }
}
