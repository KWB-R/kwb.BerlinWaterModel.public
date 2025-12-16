#' Compute abstraction metrics per gallery
#'
#' Computes yearly, monthly and aggregated abstraction metrics for a single
#' gallery (Brunnengalerie).
#'
#' The function automatically detects whether daily values were synthetically
#' generated from monthly data, e.g. by completing a daily grid and applying
#' \code{tidyr::fill(direction = "up")}. In that case, all volume-based yearly
#' and monthly metrics are computed, but daily extreme metrics
#' (\code{Q1Ist}, \code{MQ1Ist}, \code{Q30}, \code{MQ30}) are suppressed and
#' returned as \code{NA}, because they would not represent real daily extremes.
#'
#' Months with \code{NA} or zero abstraction are excluded from the calculation
#' of monthly-based KPIs (e.g. NQMonat, HQMonat, MQMonat). Additional size
#' indicators are provided: \code{n_months_with_data},
#' \code{n_months_in_operation} and \code{n_months_observation_period} per
#' gallery, as well as two percentage indicators describing the share of
#' operational months within the observed data and within the full observation
#' period.
#'
#' @param df A \code{data.frame} containing at least gallery ID, date and
#'   abstraction rates (e.g. in \eqn{m^3/d}) for a single gallery.
#' @param gallery_col Name of the gallery column (default: \code{"gallery"}).
#' @param date_col Name of the date column (default: \code{"date"}). The column
#'   must be coercible to \code{Date}.
#' @param q_col Name of the abstraction column (e.g. \code{"cbm_per_day"},
#'   interpreted as \eqn{m^3/d}).
#' @param temporal_resolution Character string specifying the temporal
#'   resolution, one of \code{"auto"}, \code{"daily"} or \code{"monthly"}.
#'   If set to \code{"auto"}, the resolution is inferred from the median time
#'   step between consecutive dates.
#' @param date_min Optional start date of the observation period. If
#'   \code{NULL} (default), the minimum of the date column is used.
#' @param date_max Optional end date of the observation period. If
#'   \code{NULL} (default), the maximum of the date column is used.
#'
#' @return A named \code{list} with the following elements:
#'   \item{per_year}{A tibble of yearly metrics per gallery and year, including
#'     yearly volumes (e.g. \code{QA}, \code{NQMonat}, \code{HQMonat},
#'     \code{Q365}, \code{Q1Ist}, \code{Q30}) as well as the number of months
#'     with data (\code{n_months_with_data}), the number of months in
#'     operation (\code{n_months_in_operation}), the length of the observation
#'     period in months (\code{n_months_observation_period}) and the
#'     corresponding percentages
#'     (\code{n_months_operation_with_data_percent},
#'     \code{n_months_operation_within_observationperiod_percent}).}
#'   \item{per_gallery}{A tibble of aggregated metrics per gallery over the full
#'     observation period (e.g. \code{MQA}, \code{NQA}, \code{HQA}, \code{MNQA},
#'     \code{NNQA}, \code{MHQA}, \code{HHQA}, \code{MQ365}, \code{HQ365},
#'     \code{MQ1Ist}, \code{MQ30}), together with
#'     \code{n_months_with_data}, \code{n_months_in_operation},
#'     \code{n_months_observation_period},
#'     \code{n_months_operation_with_data_percent} and
#'     \code{n_months_operation_within_observationperiod_percent}.}
#'   \item{per_month}{A tibble of monthly metrics per gallery and calendar
#'     month aggregated over all years (e.g. \code{MQMonat}, \code{NQMonat},
#'     \code{HQMonat}), plus the number of months with data
#'     (\code{n_months_with_data}), the number of months in operation
#'     (\code{n_months_in_operation}), the observation-period length in months
#'     (\code{n_months_observation_period}) and the corresponding percentages
#'     (\code{n_months_operation_with_data_percent},
#'     \code{n_months_operation_within_observationperiod_percent}).}
#'   \item{meta}{A named list with meta information such as
#'     \code{temporal_resolution}, \code{synthetic_daily_from_month},
#'     \code{has_daily_extremes}, the vector
#'     \code{requires_real_daily_for = c("Q1Ist", "MQ1Ist", "Q30", "MQ30")},
#'     and the effective \code{date_min}, \code{date_max} and
#'     \code{n_months_observation_period}.}
#'
#' @importFrom rlang ensym as_name
#' @importFrom dplyr mutate arrange group_by summarise ungroup select filter n relocate starts_with
#' @importFrom lubridate year month floor_date days_in_month
#' @importFrom stats median
#'
#' @export
compute_gallery_kpis <- function(
    df,
    gallery_col = "gallery",
    date_col    = "date",
    q_col       = "cbm_per_day",
    temporal_resolution = c("auto", "daily", "monthly"),
    date_min = NULL,
    date_max = NULL
) {
  temporal_resolution <- match.arg(temporal_resolution)

  # --- Tidy-eval setup --------------------------------------------------------
  gallery_sym <- rlang::ensym(gallery_col)
  date_sym    <- rlang::ensym(date_col)
  q_sym       <- rlang::ensym(q_col)

  gallery_name <- rlang::as_name(gallery_sym)
  date_name    <- rlang::as_name(date_sym)

  # --- Normalise date & derive year/month ------------------------------------
  df <- df %>%
    dplyr::mutate(
      !!date_sym := as.Date(!!date_sym)
    ) %>%
    dplyr::mutate(
      year  = lubridate::year(!!date_sym),
      month = lubridate::floor_date(!!date_sym, unit = "month")
    )

  # --- Effective observation period (date_min / date_max) --------------------
  # If not provided, use the range in df
  if (is.null(date_min)) {
    date_min_eff <- suppressWarnings(min(df[[date_name]], na.rm = TRUE))
  } else {
    date_min_eff <- as.Date(date_min)
  }

  if (is.null(date_max)) {
    date_max_eff <- suppressWarnings(max(df[[date_name]], na.rm = TRUE))
  } else {
    date_max_eff <- as.Date(date_max)
  }

  if (all(is.infinite(date_min_eff)) || all(is.na(date_min_eff))) {
    date_min_eff <- NA
  }
  if (all(is.infinite(date_max_eff)) || all(is.na(date_max_eff))) {
    date_max_eff <- NA
  }

  n_months_observation_period <- if (any(is.na(c(date_min_eff, date_max_eff)))) {
    NA_integer_
  } else {
    dm <- lubridate::floor_date(date_min_eff, "month")
    dM <- lubridate::floor_date(date_max_eff, "month")
    (lubridate::year(dM) - lubridate::year(dm)) * 12L +
      (lubridate::month(dM) - lubridate::month(dm)) + 1L
  }

  # --- Small helpers without warnings ----------------------------------------
  safe_mean <- function(x) {
    x <- x[is.finite(x) & !is.na(x)]
    if (length(x) == 0L) NA_real_ else mean(x)
  }

  safe_min <- function(x) {
    x <- x[is.finite(x) & !is.na(x)]
    if (length(x) == 0L) NA_real_ else min(x)
  }

  safe_max <- function(x) {
    x <- x[is.finite(x) & !is.na(x)]
    if (length(x) == 0L) NA_real_ else max(x)
  }

  roll_sum <- function(x, k = 30L) {
    n <- length(x)
    res <- rep(NA_real_, n)
    if (n < k) return(res)
    s <- sum(x[1:k])
    res[k] <- s
    if (n > k) {
      for (i in (k + 1L):n) {
        s <- s + x[i] - x[i - k]
        res[i] <- s
      }
    }
    res
  }

  # --- Auto-detect temporal resolution (no warnings) --------------------------
  if (temporal_resolution == "auto") {
    dt_df <- df %>%
      dplyr::arrange(!!gallery_sym, !!date_sym) %>%
      dplyr::group_by(!!gallery_sym) %>%
      dplyr::mutate(
        dt_days = as.numeric(
          difftime(dplyr::lead(!!date_sym), !!date_sym, units = "days")
        )
      ) %>%
      dplyr::ungroup()

    dt_values <- dt_df$dt_days
    dt_values <- dt_values[is.finite(dt_values) & !is.na(dt_values)]

    med_dt <- if (length(dt_values) == 0L) {
      NA_real_
    } else {
      stats::median(dt_values)
    }

    temporal_resolution <- if (is.na(med_dt)) {
      "monthly"
    } else if (med_dt <= 1.5) {
      "daily"
    } else if ( med_dt >= 27 && med_dt <= 31 * 1.5 ) {
      "monthly"
    } else {
      "monthly"
    }
  }

  # Flag: are daily values synthetically derived from monthly data?
  synthetic_daily_from_monthly <- FALSE

  # --- Prepare daily / monthly base tables -----------------------------------
  if (temporal_resolution == "daily") {
    # Daily values (aggregate in case of multiple records per day)
    daily_df <- df %>%
      dplyr::group_by(!!gallery_sym, year, !!date_sym) %>%
      dplyr::summarise(
        Qday = sum(!!q_sym, na.rm = TRUE),
        .groups = "drop"
      )

    # Detect "fill(direction = 'up')" pattern -> values constant within month
    daily_month_stats <- daily_df %>%
      dplyr::mutate(
        month = lubridate::floor_date(!!date_sym, unit = "month")
      ) %>%
      dplyr::group_by(!!gallery_sym, month) %>%
      dplyr::summarise(
        n_days  = dplyr::n(),
        q_min   = safe_min(Qday),
        q_max   = safe_max(Qday),
        q_range = q_max - q_min,
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(q_range), n_days > 1L)

    if (nrow(daily_month_stats) > 0) {
      frac_const <- mean(daily_month_stats$q_range <= 0, na.rm = TRUE)
      if (!is.na(frac_const) && frac_const >= 0.8) {
        synthetic_daily_from_monthly <- TRUE
      }
    }

    # Monthly volumes (sum of daily values) – works for both real and
    # synthetically "filled" daily series
    monthly_totals <- daily_df %>%
      dplyr::mutate(
        month = lubridate::floor_date(!!date_sym, unit = "month")
      ) %>%
      dplyr::group_by(!!gallery_sym, year, month) %>%
      dplyr::summarise(
        QMonat = sum(Qday, na.rm = TRUE),
        .groups = "drop"
      )

  } else {
    # Monthly series: q_col is m3/d (monthly mean). Convert to monthly volume.
    daily_df <- NULL

    monthly_totals <- df %>%
      dplyr::group_by(!!gallery_sym, year, month) %>%
      dplyr::summarise(
        days_in_month = unique(lubridate::days_in_month(month)),
        q_mean        = safe_mean(!!q_sym),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        QMonat = q_mean * days_in_month
      ) %>%
      dplyr::select(!!gallery_sym, year, month, QMonat)
  }

  # --- Yearly metrics (month-based, excl. NA/0 months for NQ/HQ etc.) ---------
  per_year <- monthly_totals %>%
    dplyr::group_by(!!gallery_sym, year) %>%
    dplyr::summarise(
      QA = sum(QMonat[!is.na(QMonat)], na.rm = TRUE),   # yearly total [m3/year]

      n_months_with_data    = sum(!is.na(QMonat)),
      n_months_in_operation = sum(QMonat > 0, na.rm = TRUE),

      NQMonat = {
        x <- QMonat[!is.na(QMonat) & QMonat > 0]
        if (length(x) == 0L) NA_real_ else min(x)
      },
      HQMonat = {
        x <- QMonat[!is.na(QMonat) & QMonat > 0]
        if (length(x) == 0L) NA_real_ else max(x)
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Q365 = QA / 365,                       # mean daily abstraction in year [m3/d]
      Q1Ist = NA_real_,
      Q30   = NA_real_,
      n_months_observation_period = n_months_observation_period,
      n_months_operation_with_data_percent =
        ifelse(
          n_months_with_data > 0,
          100 * n_months_in_operation / n_months_with_data,
          NA_real_
        ),
      n_months_operation_within_observationperiod_percent =
        ifelse(
          !is.na(n_months_observation_period) & n_months_observation_period > 0,
          100 * n_months_in_operation / n_months_observation_period,
          NA_real_
        )
    ) %>%
    dplyr::relocate(
      dplyr::starts_with("n_months_"),
      .after = year
    )

  # --- Daily extreme metrics (only for real daily data) -----------------------
  has_daily_extremes <- temporal_resolution == "daily" && !synthetic_daily_from_monthly

  if (has_daily_extremes && !is.null(daily_df) && nrow(daily_df) > 0) {
    q1_q30_per_year <- daily_df %>%
      dplyr::group_by(!!gallery_sym, year) %>%
      dplyr::arrange(!!date_sym, .by_group = TRUE) %>%
      dplyr::mutate(
        Q1_roll  = Qday,
        Q30_roll = roll_sum(Qday, k = 30L)
      ) %>%
      dplyr::summarise(
        Q1Ist = safe_max(Q1_roll),
        Q30   = {
          x <- Q30_roll[!is.na(Q30_roll)]
          if (length(x) == 0L) NA_real_ else max(x)
        },
        .groups = "drop"
      )

    per_year <- per_year %>%
      dplyr::select(-Q1Ist, -Q30) %>%
      dplyr::left_join(q1_q30_per_year, by = c(gallery_name, "year"))
  } else {
    per_year$Q1Ist <- NA_real_
    per_year$Q30   <- NA_real_
  }

  # --- Aggregated metrics per gallery -----------------------------------------
  per_gallery <- per_year %>%
    dplyr::group_by(!!gallery_sym) %>%
    dplyr::summarise(
      n_years = dplyr::n(),

      n_months_with_data    = sum(n_months_with_data,    na.rm = TRUE),
      n_months_in_operation = sum(n_months_in_operation, na.rm = TRUE),

      # n_months_observation_period is constant for this gallery
      n_months_observation_period = {
        x <- n_months_observation_period[!is.na(n_months_observation_period)]
        if (length(x) == 0L) NA_integer_ else max(x)
      },

      MQA  = safe_mean(QA),
      NQA  = safe_min(QA),
      HQA  = safe_max(QA),
      MNQA = safe_mean(NQMonat),
      NNQA = safe_min(NQMonat),
      MHQA = safe_mean(HQMonat),
      HHQA = safe_max(HQMonat),
      MQ365 = safe_mean(Q365),
      HQ365 = safe_max(Q365),
      MQ1Ist = safe_mean(Q1Ist),
      MQ30   = safe_mean(Q30),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_months_operation_with_data_percent =
        ifelse(
          n_months_with_data > 0,
          100 * n_months_in_operation / n_months_with_data,
          NA_real_
        ),
      n_months_operation_within_observationperiod_percent =
        ifelse(
          !is.na(n_months_observation_period) & n_months_observation_period > 0,
          100 * n_months_in_operation / n_months_observation_period,
          NA_real_
        )
    ) %>%
    dplyr::relocate(
      dplyr::starts_with("n_months_"),
      .after = n_years
    )

  if (!has_daily_extremes) {
    per_gallery$MQ1Ist <- NA_real_
    per_gallery$MQ30   <- NA_real_
  }

  # --- Monthly metrics (per calendar month over all years, excl. NA/0) --------
  per_month <- monthly_totals %>%
    dplyr::mutate(
      month_num  = lubridate::month(month),
      month_name = factor(
        month_num,
        levels = 1:12,
        labels = month.abb
      )
    ) %>%
    dplyr::group_by(!!gallery_sym, month_num, month_name) %>%
    dplyr::summarise(
      n_months_with_data    = sum(!is.na(QMonat)),
      n_months_in_operation = sum(QMonat > 0, na.rm = TRUE),
      MQMonat = {
        x <- QMonat[!is.na(QMonat) & QMonat > 0]
        if (length(x) == 0L) NA_real_ else mean(x)
      },
      NQMonat = {
        x <- QMonat[!is.na(QMonat) & QMonat > 0]
        if (length(x) == 0L) NA_real_ else min(x)
      },
      HQMonat = {
        x <- QMonat[!is.na(QMonat) & QMonat > 0]
        if (length(x) == 0L) NA_real_ else max(x)
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_months_observation_period = n_months_observation_period,
      n_months_operation_with_data_percent =
        ifelse(
          n_months_with_data > 0,
          100 * n_months_in_operation / n_months_with_data,
          NA_real_
        ),
      n_months_operation_within_observationperiod_percent =
        ifelse(
          !is.na(n_months_observation_period) & n_months_observation_period > 0,
          100 * n_months_in_operation / n_months_observation_period,
          NA_real_
        )
    ) %>%
    dplyr::relocate(
      dplyr::starts_with("n_months_"),
      .after = month_name
    )

  # --- Return -----------------------------------------------------------------
  list(
    per_year    = per_year,
    per_gallery = per_gallery,
    per_month   = per_month,
    meta = list(
      temporal_resolution        = temporal_resolution,
      synthetic_daily_from_month = synthetic_daily_from_monthly,
      has_daily_extremes         = has_daily_extremes,
      requires_real_daily_for    = c("Q1Ist", "MQ1Ist", "Q30", "MQ30"),
      date_min                   = date_min_eff,
      date_max                   = date_max_eff,
      n_months_observation_period = n_months_observation_period
    )
  )
}



#' Extract aggregated KPIs per gallery from a lookup table
#'
#' Helper to flatten the nested \code{kpis$per_gallery} entries from
#' \code{ww_galleries_lookup} (or a similar lookup table) into a single tibble.
#'
#' This returns one row per gallery with all aggregated KPIs that were computed
#' over the full observation period (e.g. \code{MQA}, \code{NQA}, \code{HQA},
#' \code{MNQA}, \code{NNQA}, \code{MHQA}, \code{HHQA}, \code{MQ365},
#' \code{HQ365}, \code{MQ1Ist}, \code{MQ30}, ...).
#'
#' Any \code{waterworks} or \code{gallery} columns inside \code{per_gallery}
#' are dropped to avoid name clashes; the outer ID columns are kept.
#'
#' @param lookup A tibble with columns \code{waterworks}, \code{gallery} and
#'   a list-column \code{kpis} as returned by \code{compute_gallery_kpis()}.
#'
#' @return A tibble with one row per gallery, containing all aggregated KPIs
#'   from \code{kpis$per_gallery} and the identifying columns
#'   \code{waterworks} and \code{gallery}.
#'
#' @importFrom dplyr select mutate any_of
#' @importFrom tidyr unnest
#' @importFrom purrr map
#'
#' @export
get_gallery_kpis_per_gallery <- function(lookup) {
  lookup %>%
    dplyr::select(waterworks, gallery, kpis) %>%
    dplyr::mutate(
      per_gallery = purrr::map(kpis, "per_gallery"),
      per_gallery = purrr::map(
        per_gallery,
        ~ dplyr::select(.x, -dplyr::any_of(c("waterworks", "gallery")))
      )
    ) %>%
    dplyr::select(-kpis) %>%
    tidyr::unnest(per_gallery)
}

#' Extract yearly KPIs from a galleries lookup table
#'
#' Helper to flatten the nested \code{kpis$per_year} entries from
#' \code{ww_galleries_lookup} (or a similar lookup table) into a single tibble.
#'
#' @param lookup A tibble with columns \code{waterworks}, \code{gallery} and
#'   a list-column \code{kpis} as returned by \code{compute_gallery_kpis()}.
#'
#' @return A tibble with yearly KPIs, one row per gallery and year, including
#'   the columns \code{waterworks} and \code{gallery}.
#'
#' @importFrom dplyr select mutate any_of
#' @importFrom tidyr unnest
#' @importFrom purrr map
#'
#' @export
get_gallery_kpis_per_year <- function(lookup) {
  lookup %>%
    dplyr::select(waterworks, gallery, kpis) %>%
    dplyr::mutate(
      per_year = purrr::map(kpis, "per_year"),
      per_year = purrr::map(
        per_year,
        ~ dplyr::select(.x, -dplyr::any_of(c("waterworks", "gallery")))
      )
    ) %>%
    dplyr::select(-kpis) %>%
    tidyr::unnest(per_year)
}

#' Extract monthly KPIs from a galleries lookup table
#'
#' Helper to flatten the nested \code{kpis$per_month} entries from
#' \code{ww_galleries_lookup} (or a similar lookup table) into a single tibble.
#'
#' @param lookup A tibble with columns \code{waterworks}, \code{gallery} and
#'   a list-column \code{kpis} as returned by \code{compute_gallery_kpis()}.
#'
#' @return A tibble with monthly KPIs (gallery × calendar month), including
#'   the columns \code{waterworks} and \code{gallery}.
#'
#' @importFrom dplyr select mutate any_of
#' @importFrom tidyr unnest
#' @importFrom purrr map
#'
#' @export
get_gallery_kpis_per_month <- function(lookup) {
  lookup %>%
    dplyr::select(waterworks, gallery, kpis) %>%
    dplyr::mutate(
      per_month = purrr::map(kpis, "per_month"),
      per_month = purrr::map(
        per_month,
        ~ dplyr::select(.x, -dplyr::any_of(c("waterworks", "gallery")))
      )
    ) %>%
    dplyr::select(-kpis) %>%
    tidyr::unnest(per_month)
}

#' Extract meta information from a galleries lookup table
#'
#' Helper to flatten the \code{meta} entries from the \code{kpis} list-column
#' in \code{ww_galleries_lookup} (or a similar lookup table). Each gallery gets
#' one row with all meta fields as columns.
#'
#' @param lookup A tibble with columns \code{waterworks}, \code{gallery} and
#'   a list-column \code{kpis} as returned by \code{compute_gallery_kpis()}.
#'
#' @return A tibble with one row per gallery and all meta information expanded
#'   into regular columns (e.g. \code{temporal_resolution},
#'   \code{synthetic_daily_from_month}, \code{has_daily_extremes}, ...),
#'   including \code{waterworks} and \code{gallery}.
#'
#' @importFrom dplyr select mutate
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom tibble as_tibble_row
#'
#' @export
get_gallery_kpis_meta <- function(lookup) {
  lookup %>%
    dplyr::select(waterworks, gallery, kpis) %>%
    dplyr::mutate(
      meta = purrr::map(kpis, "meta"),
      meta = purrr::map(meta, tibble::as_tibble_row)
    ) %>%
    dplyr::select(-kpis) %>%
    tidyr::unnest(meta)
}
