#' Fill Timeseries based on User Defined Intervall (days, hours, minutes or seconds)
#'
#' @param df data frame with data
#' @param col_datetime column of date or datetime
#' @param temporal_resolution select one of ("days", "hours", "minutes", "seconds"),
#' (default: "days")
#' @param direction in which direction should the parameter be filled ("up" or
#' "down"), default: "up"
#' @return tibble with filled dates. Note that if min(daste) == "xxxx-xx-end-of-month-day"
#' the min(date) is set to "xxxx-xx-01", as monthly measured (originally summed)
#' values were set to the last day of the month, but are valid for the whole month
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr arrange_at full_join
#' @importFrom tidyr fill pivot_wider separate
#' @importFrom tidyselect matches
#' @importFrom rlang :=
fill_timeseries <- function(df, col_datetime = "date", temporal_resolution = "days", direction = "up")
{
  stopifnot(col_datetime %in% names(df))

  temp_res <- c("seconds" = "sec", "minutes" = "min", "hours" = "hour", "days" = "days")
  stopifnot(temporal_resolution %in% names(temp_res))

  stopifnot(direction %in% c("up", "down"))

  dates <- df[[col_datetime]]
  date_min_monthly <- as.Date(paste0(stringr::str_sub(min(dates), 1, 8), "01"))

  date_min <- if (min(dates) != date_min_monthly) {
    date_min_monthly
  } else {
    min(dates)
  }


  date_sequence <- if(names(temp_res[temporal_resolution]) == "days") {
    seq.Date(from = date_min, to = max(dates),
             by = temp_res[temporal_resolution])
  } else {
    seq.POSIXt(from = as.POSIXct(date_min, tz = "UTC"),
               to = as.POSIXct(max(dates)+1, tz = "UTC") - 1,
               by = temp_res[temporal_resolution])
  }

  new_dates <-  tibble::tibble(!!(col_datetime) := date_sequence)


  df_new <- df %>%
    dplyr::full_join(new_dates,
                     by = col_datetime) %>%
    dplyr::arrange_at(col_datetime) %>%
    tidyr::fill(!tidyselect::matches(col_datetime), .direction = direction)


  if(temporal_resolution != "days") {
    df_new <- df_new %>%
      dplyr::rename(datetime = col_datetime)
  }

  df_new

}
