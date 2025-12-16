#' Helper function: parse equation
#'
#' @param equation equation in text form (either "constant" -> y = c or "ln" ->
#' y = a * ln(x) + b)
#' @returns R function of parsed equation
#' @export
#' @importFrom stringr str_match str_trim str_remove
parse_equation <- function(equation) {

  equation_txt <- deparse(substitute(equation))

  # Prüfe, ob die Gleichung eine Konstante ist (z. B. "y = 95")
  match_constant <- stringr::str_match(equation, "y = ([0-9.]+)$")
  if (!is.na(match_constant[1, 1])) {
    constant_value <- as.numeric(match_constant[1, 2])
    return(function(x) sapply(x, function(i) constant_value))  # Immer den konstanten Wert zurückgeben
  }

  # Extrahiere Koeffizienten aus einer logarithmischen Gleichung (y = a * ln(x) + b)
  matches <- stringr::str_match(equation, "y = ([0-9.]+)ln\\(x\\) ([+-] [0-9.]+)")
  if (is.na(matches[1, 1])) {
    stop(sprintf(paste0("Function is not provided in correct format in '%s'.\n",
                       "Only 'y = a * ln(x) + b' or a constant 'y = c' are currently supported!"),
                 equation_txt))
    return(NULL)  # Falls keine gültige Gleichung
  }

  a <- as.numeric(matches[1, 2] %>%  stringr::str_trim() %>%  stringr::str_remove("\\s+?"))
  b <- as.numeric(matches[1, 3] %>%  stringr::str_trim() %>%  stringr::str_remove("\\s+?"))

  return(function(x) a * log(x) + b)
}


#' Helper function: parse a and
#'
#' @param a parameter a
#' @param b parameter b
#' @returns R function of parsed equation
#' @export
#' @examples
#' a <- 1
#' b <- 2
#' parse_a_and_b(a, b)
parse_a_and_b <- function(a, b) {
  a <- ifelse(is.na(a), 0, a)
  b <- ifelse(is.na(b), 0, b)
  stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)

  # Funktionskoerper mit Zahlen einsetzen

  #alt: A * log(Q) + B
  #body_expr <- substitute(A * log(Q) + B, list(A = a, B = b))

  #neu: SW[%] = 100 / (1 + exp( - ( b*log(Q) - a ) ))
  body_expr <- substitute(
    100 / (1 + exp(-(B * log(Q) - A))),
    list(A = a, B = b)
  )

  # Funktion (Q) -> <eingesetzter Ausdruck> bauen
  f <- function(Q) NULL
  body(f) <- body_expr
  f
}


#' Bank Filtration Share: convert equation
#'
#' @param df data frame with bank filtration share equations, defined in sublist
#' config$bfstypes_equations as retrieved by \code{\link{config_read}}
#' @param col_equation_a column name of equation parameter a (default: "equation_a")
#' @param col_equation_b column name of equation parameter b (default: "equation_b")
#' @returns adds "equation_function" to data frame
#' @export
#'
#' @importFrom dplyr rowwise mutate relocate
bfs_convert_equation <- function(df,
                                 col_equation_a = "equation_a",
                                 col_equation_b = "equation_b") {

available_column_names <- names(df)

stopifnot(col_equation_a %in% available_column_names)
stopifnot(col_equation_b %in% available_column_names)


 df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(equation_function =  list(parse_a_and_b(a = .data[[col_equation_a]],
                                                          b = .data[[col_equation_b]]))) %>%
    dplyr::relocate(equation_function, .after = tidyselect::all_of(col_equation_b))

 df$equation_text <- sapply(df$equation_function, function(x) {

   paste(deparse(x), collapse = "") %>%
     stringr::str_replace("function", "BF share") %>%
     stringr::str_replace("BF share \\(Q\\)", "BF share (Q) =")
 })

 df

}
