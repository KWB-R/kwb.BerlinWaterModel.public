#' Calculate Qualities
#'
#' @param input_list input_list as retrieved by \code{\link{prepare_input}}
#' @param flows tibble with (modelled) flows for the network (e.g. precomputed
#'   upstream by the user)
#' @param network tibble with water cycle flow network data, as retrieved by
#'   \code{\link{prepare_network}}
#' @param config list with config as imported with \code{\link{config_read}}
#' @param reverse_flow calculate reverse flow (default: FALSE)
#' @param branchwise improved calculation workflow minimising unneeded section
#'   calculations (default: TRUE)
#' @param max_sections restrict number of calculated sections in case problems
#'   occur. Provide a number <= number of sections to be calculated. If NULL, all
#'   sections will be calculated (default: NULL). Only used if reverse_flow = FALSE
#' @param debug print debug messages (default: TRUE)
#'
#' @return returns modelled qualities in tibble format
#' @export
#' @importFrom dplyr count pull
#' @importFrom stringr str_starts
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
calculate_qualities <- function(input_list,
                                flows,
                                network,
                                config,
                                reverse_flow = FALSE,
                                branchwise = TRUE,
                                max_sections = NULL,
                                debug = TRUE) {


qualities <- if(reverse_flow) {

  if(branchwise) {
    calculate_qualities_backward_branchwise(input_list = input_list,
                                            flows = flows,
                                            network = network,
                                            config = config,
                                            debug = debug)
  } else {
    calculate_qualities_backward(input_list = input_list,
                               flows = flows,
                               network = network,
                               config = config,
                               debug = debug)
  }

} else {

  calculate_qualities_forward(input_list = input_list,
                              flows = flows,
                              network = network,
                              config = config,
                              max_sections = max_sections,
                              debug = debug)
}

qualities

}
