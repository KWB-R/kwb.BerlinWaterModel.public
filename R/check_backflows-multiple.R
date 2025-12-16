#' Check backflows multiple
#'
#' @param config list with config as imported with \code{\link{config_read}}
#'
#' @returns nothing besides stops the code in case checks do not pass or message
#' in case no config file configs/backflows_multiple.csv is provided
#' @export
#'
check_backflows_multiple <- function(config) {

if("backflows_multiple" %in% names(config)) {


  backflow_ids <- config$backflows_multiple %>%
    dplyr::group_by(backflow_id) %>%
    dplyr::pull(backflow_id) %>%
    unique()

check_results_df <- stats::setNames(lapply(backflow_ids, function(id) {
backflow_multiple_selected <- config$backflows_multiple %>%
  dplyr::filter(backflow_id == id)


 if(any(is.na(backflow_multiple_selected$section_backflow_out_share)) & !all(is.na(backflow_multiple_selected$section_backflow_out_share))) {

     backflow_multiple_selected_missing <- backflow_multiple_selected %>%
       dplyr::filter(is.na(section_backflow_out_share))

     stop(sprintf("There is a problematic entry in 'config/backflows_multiple.csv' for backflow from section '%s': '%s' of %d/%d upsteam sections (%s) suming up to %.2f %% instead of 100%%",
                  unique(backflow_multiple_selected_missing$backflow_id),
                  unique(backflow_multiple_selected_missing$section_name),
                  nrow(backflow_multiple_selected_missing),
                  nrow(backflow_multiple_selected),
                  paste0(sprintf("'%s': '%s'",
                                 backflow_multiple_selected_missing$section_backflow_out_id,
                                 backflow_multiple_selected_missing$section_backflow_out_name),
                         collapse = ", "),
                  100*sum(backflow_multiple_selected$section_backflow_out_share, na.rm = TRUE)
                  )
          )
   } else if (sum(backflow_multiple_selected$section_backflow_out_share) != 1) {
   stop(sprintf("There is a problematic entry in 'config/backflows_multiple.csv' for backflow from section '%s': '%s' of %d upsteam sections (%s) suming up to %.2f %% instead of 100%%",
                unique(backflow_multiple_selected$backflow_id),
                unique(backflow_multiple_selected$section_name),
                nrow(backflow_multiple_selected),
                paste0(sprintf("'%s': '%s' - %.2f %%",
                               backflow_multiple_selected$section_backflow_out_id,
                               backflow_multiple_selected$section_backflow_out_name,
                               100*backflow_multiple_selected$section_backflow_out_share),
                       collapse = ", "),
                100*sum(backflow_multiple_selected$section_backflow_out_share)
   )
   )
 } else if (all(is.na(backflow_multiple_selected$section_backflow_out_share))) {
   stop(sprintf("All entries in 'config/backflows_multiple.csv' for backflow from section '%s': '%s' of %d upsteam sections (%s) are missing. Please fix column 'section_backflow_out_share' of 'config/backflows_multiple.csv'",
                unique(backflow_multiple_selected$backflow_id),
                unique(backflow_multiple_selected$section_name),
                nrow(backflow_multiple_selected),
                paste0(sprintf("'%s': '%s'",
                               backflow_multiple_selected$section_backflow_out_id,
                               backflow_multiple_selected$section_backflow_out_name),
                       collapse = ", ")
                       )
     )
 } else {
  ### do nothing, in this case the check has passed!
 }

}), nm = backflow_ids)


} else {
 message("Skipping 'check_backflows_multiple()' check as there is no 'config/backflows_multiple'")
}
}
