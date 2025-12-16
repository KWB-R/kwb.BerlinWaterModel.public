#' Calculate Outflows
#'
#' @param list_order list for each order id (corresponding to same level of distance
#' of section from selected outflow)
#' @param input input as retrieved by \code{\link{prepare_input}}
#' @param config list with config as imported with \code{\link{config_read}}
#' @param use_dynamic for multiple outputs only: should static shares
#' (as defined in column "section_out_share" of "outflows_multiple.csv") be used
#' for separating the flow within a section or a function (as defined in column
#' "section_out_function" of "outflows_multiple.csv")), (default: TRUE)
#' @param return_inputs should also input data be returned in result dataset
#' (default: FALSE)
#' @return tibble with outflows added to provided dataset
#' @export
#' @importFrom stringr str_starts
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble

calculate_outflows <- function(list_order, input, config, use_dynamic = TRUE, return_inputs) {

outflow_pattern <- "Out"

df_order <- dplyr::bind_rows(list_order) %>%
  dplyr::mutate(source_id = get_ids_from_names(source, config = config),
                target_id = get_ids_from_names(target, config = config)) %>%
  dplyr::filter(stringr::str_starts(target_id, outflow_pattern))


if(nrow(df_order) == 0) {
  stop("No outflow id has been defined. Please define an outflow in the 'flows_in_out.csv'
       configuration file and name it starting with 'Out'. Otherwise it cannot be properly
       detected.")
}

outflows_inflows_df <- lapply(df_order$source_id, function(id) {
  outflow_inflows <- df_order %>%
    dplyr::filter(source_id == id)

    if(use_dynamic) {
      if(is.function(outflow_inflows$section_out_function_parsed[[1]])) {
      tibble::as_tibble(outflow_inflows$section_out_function_parsed[[1]](input[,id]))
      } else {
        if(is.numeric(outflow_inflows$value)) {
          tibble::as_tibble(input[,id] * outflow_inflows$value)
        } else {
          stop(sprintf("No static value defined in 'section_out_share' in config file 'outflows_multiple' for source '%s' to target '%s'. Please set it there!",
                       outflow_inflows$source,
                       outflow_inflows$target
          ))
        }
      }
    } else {
      if(is.numeric(outflow_inflows$value)) {
        tibble::as_tibble(input[,id] * outflow_inflows$value)
      } else {
        stop(sprintf("No static value defined in 'section_out_share' in config file 'outflows_multiple' for source '%s' to target '%s'. Please set it there!",
                     outflow_inflows$source,
                     outflow_inflows$target
        ))
      }
    }
  }) %>% dplyr::bind_cols() %>% tibble::as_tibble()


outflow_ids <- unique(df_order$target_id)

outflows_df <- lapply(seq_along(outflow_ids), function(id) {
  outflow_inflows <- df_order %>%
    dplyr::filter(target_id %in% outflow_ids[id])

  outflows <- lapply(seq_len(nrow(outflow_inflows)), function(i) {
    tibble::as_tibble(input[,get_ids_from_names(outflow_inflows$source[i], config)] * outflow_inflows$value[i])
    }) %>% dplyr::bind_cols() %>% tibble::as_tibble()

  tibble::tibble(!!outflow_ids[id] := rowSums(outflows, na.rm = TRUE))
}) %>% dplyr::bind_cols()

date_or_datetime_col <- get_date_or_datetime_columns(input)

stopifnot(length(date_or_datetime_col) > 0)

out <- if(return_inputs) {
  input[, date_or_datetime_col] %>%
  dplyr::bind_cols(outflows_inflows_df) %>%
  dplyr::bind_cols(outflows_df)
} else {
  input[, date_or_datetime_col] %>%
  dplyr::bind_cols(outflows_df)
}

out
}
