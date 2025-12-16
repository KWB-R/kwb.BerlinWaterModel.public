#' Expand monthly end-of-month values to all days or hours of the same month
#'
#' For each row in `df`, this function assumes that the time column
#' contains a monthly value at (or near) the end of a month
#' (e.g. "2002-01-31" or "2002-01-31 23:00:00") and expands it backwards
#' to all days or all hours from the 1st of that month up to the given
#' timestamp.
#'
#' If `temporal_res = "auto"` (default), the function chooses
#' daily expansion when a column `date` is present and hourly expansion
#' when a column `datetime` is present.
#'
#' The resulting series can optionally be cropped to a user-defined time
#' window via `date_min` and `date_max`.
#'
#' @param df A data frame or tibble.
#' @param date_col Name of the daily date column (default: "date").
#' @param datetime_col Name of the hourly datetime column
#'   (default: "datetime").
#' @param temporal_res One of "auto", "days", "hours". If "auto",
#'   the presence of `date_col` / `datetime_col` decides.
#' @param date_min Optional lower bound of the output time window. For
#'   daily mode this is interpreted as `Date` (or converted via
#'   `as.Date()`); for hourly mode it can be a `POSIXct` or `Date`.
#'   If `NULL`, no lower cropping is applied.
#' @param date_max Optional upper bound of the output time window.
#'   Same interpretation as `date_min`. If `NULL`, no upper cropping
#'   is applied.
#'
#' @return
#' A tibble where each original row is expanded so that all days (for
#' "days") or all hours (for "hours") from the 1st of the respective
#' month up to the original timestamp are present. All other columns
#' are copied to the created rows. The result is then cropped to
#' `[date_min, date_max]` if those are supplied. The name of the time
#' column is preserved ("date" or "datetime").
#'
#' @export
#'
#' @importFrom dplyr mutate rowwise ungroup arrange across all_of filter
#' @importFrom tidyr unnest
#' @importFrom rlang sym
fill_month_to_start <- function(df,
                                date_col      = "date",
                                datetime_col  = "datetime",
                                temporal_res  = c("auto", "days", "hours"),
                                date_min      = NULL,
                                date_max      = NULL)
{
  temporal_res <- match.arg(temporal_res)

  # --- decide mode ----------------------------------------------------------
  if (temporal_res == "auto") {
    if (date_col %in% names(df) && !(datetime_col %in% names(df))) {
      temporal_res <- "days"
    } else if (datetime_col %in% names(df)) {
      temporal_res <- "hours"
    } else {
      stop("Neither '", date_col, "' nor '", datetime_col,
           "' found in data and temporal_res = 'auto'.")
    }
  }

  if (temporal_res == "days") {
    # --------- daily variant (column 'date') ----------------------------
    stopifnot(date_col %in% names(df))
    dt_sym <- rlang::sym(date_col)
    other_cols <- setdiff(names(df), date_col)

    out <- df %>%
      dplyr::mutate(
        !!dt_sym := as.Date(.data[[date_col]]),
        .month_start = as.Date(format(!!dt_sym, "%Y-%m-01"))
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        !!dt_sym := list(seq(.month_start, !!dt_sym, by = "day"))
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = !!dt_sym) %>%
      dplyr::select(-.month_start) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(other_cols)), !!dt_sym)

    # --- crop to [date_min, date_max] if provided -----------------------
    if (!is.null(date_min) || !is.null(date_max)) {
      if (!is.null(date_min)) {
        date_min_d <- as.Date(date_min)
        out <- out %>%
          dplyr::filter(.data[[date_col]] >= date_min_d)
      }
      if (!is.null(date_max)) {
        date_max_d <- as.Date(date_max)
        out <- out %>%
          dplyr::filter(.data[[date_col]] <= date_max_d)
      }
    }

    return(out)

  } else {
    # --------- hourly variant (column 'datetime') -----------------------
    stopifnot(datetime_col %in% names(df))
    dt_sym <- rlang::sym(datetime_col)
    other_cols <- setdiff(names(df), datetime_col)

    # detect timezone (if any), fallback to UTC
    tz <- attr(df[[datetime_col]], "tzone")
    if (is.null(tz) || tz == "") tz <- "UTC"

    out <- df %>%
      dplyr::mutate(
        !!dt_sym := as.POSIXct(.data[[datetime_col]], tz = tz),
        .date_part   = as.Date(!!dt_sym),
        .month_start = as.POSIXct(
          paste0(format(.date_part, "%Y-%m-01"), " 00:00:00"),
          tz = tz
        )
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        !!dt_sym := list(seq(.month_start, !!dt_sym, by = "hour"))
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = !!dt_sym) %>%
      dplyr::select(-.month_start, -.date_part) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(other_cols)), !!dt_sym)

    # --- crop to [date_min, date_max] if provided -----------------------
    if (!is.null(date_min) || !is.null(date_max)) {
      # convert bounds to POSIXct in same tz
      if (!is.null(date_min)) {
        if (inherits(date_min, "Date")) {
          date_min_dt <- as.POSIXct(paste0(as.Date(date_min), " 00:00:00"), tz = tz)
        } else {
          date_min_dt <- as.POSIXct(date_min, tz = tz)
        }
        out <- out %>%
          dplyr::filter(.data[[datetime_col]] >= date_min_dt)
      }
      if (!is.null(date_max)) {
        if (inherits(date_max, "Date")) {
          date_max_dt <- as.POSIXct(paste0(as.Date(date_max), " 23:59:59"), tz = tz)
        } else {
          date_max_dt <- as.POSIXct(date_max, tz = tz)
        }
        out <- out %>%
          dplyr::filter(.data[[datetime_col]] <= date_max_dt)
      }
    }

    return(out)
  }
}
