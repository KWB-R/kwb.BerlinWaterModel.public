#' Aggregate Flows Monthly
#'
#' @param flows flows as retrieved by \code{\link{calculate_flows_auto}}
#' @param aggregation_function function used for aggregation (default: median)
#' @returns tibble with monthly aggregated flows data
#' @export
#'
aggregate_flows_monthly <- function(flows,
                                    aggregation_function = median) {

  col_date_or_datetime <- ifelse(any(stringr::str_detect(names(flows), "datetime")),
                                 "datetime",
                                 "date")


      flows  %>%
        dplyr::mutate(yearmonth = stringr::str_sub(.data[[col_date_or_datetime]], 1, 7),
                      day = as.numeric(stringr::str_sub(.data[[col_date_or_datetime]], 9, 10))) %>%
        dplyr::group_by(yearmonth) %>%
        dplyr::summarise(day = round(median(day),0),
                         dplyr::across(.cols = - tidyselect::any_of(c("yearmonth", "date")),
                                       .fns = aggregation_function)) %>%
        dplyr::mutate(date = sprintf("%s-%s", yearmonth, day) %>% as.Date()) %>%
        dplyr::relocate(date, .before = yearmonth) %>%
        dplyr::select(-yearmonth, -day)


}
