#' Aggregate Qualities Monthly
#'
#' @param qualities list as retrieved by \code{\link{calcualate_qualities}}
#' @param aggregation_function function used for aggregation (default: median)
#' @param minimum_tracer_sum minimum tracer sum (default: 0.999 i.e. 99.9 percent)
#' for filtering out results for with tracer has not reached almost 100 percent
#' @returns list with sublist "conc" with monthly aggregated concentrations data
#' @export
#'
aggregate_qualities_monthly <- function(qualities,
                                        aggregation_function = median,
                                        minimum_tracer_sum = 0.999) {

  col_date_or_datetime <- ifelse(any(stringr::str_detect(names(qualities$conc[[1]]), "date")),
                                 "date",
                                 "datetime")

    conc <- stats::setNames(lapply(names(qualities$conc), function(section_id) {

      tracer <- qualities$conc[[section_id]]  %>%
        dplyr::mutate(tracer_sum = rowSums(dplyr::across(dplyr::starts_with("tracer")))) %>%
        dplyr::filter(tracer_sum > minimum_tracer_sum) %>%
        dplyr::mutate(yearmonth = stringr::str_sub(.data[[col_date_or_datetime]], 1, 7),
                      day = as.numeric(stringr::str_sub(.data[[col_date_or_datetime]], 9, 10))) %>%
        dplyr::group_by(yearmonth) %>%
        dplyr::summarise(day = round(median(day),0),
                         dplyr::across(.cols = tidyselect::starts_with("tracer"),
                                       .fns = aggregation_function)) %>%
        dplyr::mutate(date = sprintf("%s-%s", yearmonth, day) %>% as.Date()) %>%
        dplyr::relocate(date, .before = yearmonth) %>%
        dplyr::select(-yearmonth, -day, -tracer_sum)

      tracer_sum <- rowSums(tracer[,stringr::str_subset(names(tracer), pattern = "tracer\\.")])

      tracer <- tracer %>%
        dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("tracer"), ~ .x / tracer_sum))


      if(any(stringr::str_starts(names(qualities$conc[[section_id]]), "conc"))) {

      substances <- qualities$conc[[section_id]]  %>%
        dplyr::mutate(yearmonth = stringr::str_sub(.data[[col_date_or_datetime]], 1, 7),
                      day = as.numeric(stringr::str_sub(.data[[col_date_or_datetime]], 9, 10))) %>%
        dplyr::group_by(yearmonth) %>%
        dplyr::summarise(day = round(median(day),0),
                         dplyr::across(.cols = tidyselect::starts_with("conc"),
                                       .fns = aggregation_function)) %>%
        dplyr::mutate(date = sprintf("%s-%s", yearmonth, day) %>% as.Date()) %>%
        dplyr::relocate(date, .before = yearmonth) %>%
        dplyr::select(-yearmonth, -day)

      tracer <- tracer %>%
        dplyr::left_join(substances,
                         by = col_date_or_datetime)
      }

      tracer

    }), nm = names(qualities$conc))


    list(conc = conc)

}
