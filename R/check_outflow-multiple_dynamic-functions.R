#' Check outflow multiple dynamic functions
#'
#' @param config list with config as imported with \code{\link{config_read}}
#' @param q total section flow used for testing, needs to be a scalar! (default: 1)
#' @param allowed_relative_offset_percent maximum allowed percental offset for sum of all
#' section outflows compared to section inflow (i.e. parameter q), default: 0.0001)
#'
#' @returns nothingt if check passes for all outflow_id s with defined functions,
#' otherwise error
#' @export
#'
check_outflow_multiple_dynamic_functions <- function(config, q = 1, allowed_relative_offset_percent = 0.001) {

outflow_ids <- config$outflows_multiple %>%
    dplyr::group_by(outflow_id) %>%
    dplyr::filter(sapply(section_out_function_parsed, is.function)) %>%
    dplyr::pull(outflow_id) %>%
    unique()

check_results_df <- stats::setNames(lapply(outflow_ids, function(id) {
outflow_multiple_selected <- config$outflows_multiple %>%
  dplyr::filter(outflow_id == id)

outflow_multiple_selected_is_function <- sapply(outflow_multiple_selected$section_out_function_parsed, is.function)

 if(all(outflow_multiple_selected_is_function)) {
   lapply(seq_len(nrow(outflow_multiple_selected)), function(i) {
     tibble::tibble("{outflow_multiple_selected$section_out_id[i]}" := outflow_multiple_selected$section_out_function_parsed[[i]](q))
   }) %>%
     dplyr::bind_cols() %>%
     dplyr::mutate(q_sum = rowSums(.),
                   q_rel = q_sum/q)

 } else {
   outflow_multiple_selected_no_function_df <- outflow_multiple_selected[!outflow_multiple_selected_is_function,]

   stop(sprintf("'section_out_function' need to be defined for all downstream sections of '%s (%s)' for each 'outflow_id' (%s) within 'outflows_multiple.csv'.\nPlease add function(s) for the following section(s): %s",
                unique(outflow_multiple_selected_no_function_df$section_name),
                unique(outflow_multiple_selected_no_function_df$section_in_id),
                unique(outflow_multiple_selected_no_function_df$outflow_id),
                paste0(outflow_multiple_selected_no_function_df$section_out_name,
                       collapse = ", "))
   )
 }

}), nm = outflow_ids) %>%
  dplyr::bind_rows(.id = "outflow_id")


check_results_df_failed <- check_results_df %>%
  dplyr::filter(q_rel * 100 >= (100 + allowed_relative_offset_percent) | q_rel * 100 <= (100 - allowed_relative_offset_percent))

if(nrow(check_results_df_failed) > 0) {
  stop(sprintf("There are problematic entries for the following 'outflow_id's within 'outflows_multiple.csv' not respecting the maximal allowed offset (i.e. %f %% allowed offset compared to total section flow):\n%s",
               allowed_relative_offset_percent,
               paste0(sprintf("%s (%3.4f %%)",
                              check_results_df_failed$outflow_id,
                              check_results_df_failed$q_rel * 100 - 100), collapse = ", ")
               )
  )
}


}
