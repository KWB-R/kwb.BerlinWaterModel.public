#' Set Tracer Starting Concentrations To Final Modelling Results
#'
#' @param config config list structure as retrieved by \code{\link{config_read}}
#' @param qualities qualities list result structure
#'
#' @returns config list with starting concentrations (defined in config$sections)
#' based on qualities. The last time step is used as starting concentration
#' @export
#'
set_tracer_starting_conc <- function(config, qualities) {

  for(section_id in names(qualities$conc)) {

    idx <- config$sections$section_id == section_id
    end <- nrow(qualities$conc[[section_id]])-1
    config$sections$conc_tracer.cso[idx] <- qualities$conc[[section_id]]$tracer.cso[end]
    config$sections$conc_tracer.inlet[idx] <- qualities$conc[[section_id]]$tracer.inlet[end]
    config$sections$conc_tracer.rain_runoff[idx] <- qualities$conc[[section_id]]$tracer.rain_runoff[end]
    config$sections$conc_tracer.rain_direct[idx] <- qualities$conc[[section_id]]$tracer.rain_direct[end]
    config$sections$conc_tracer.ww_discharge[idx] <- qualities$conc[[section_id]]$tracer.ww_discharge[end]
    config$sections$conc_tracer.wwtp[idx] <- qualities$conc[[section_id]]$tracer.wwtp[end]
  }

  config
}
