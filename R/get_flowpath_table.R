#' Get Flowpath Table
#'
#' @param outflow_id id of outflow section
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#' @param config config
#'
#' @return list for each order id containing tibble with all sections of same
#' order id
#' @export
#' @importFrom dplyr arrange desc filter left_join pull relocate
get_flowpath_table <- function(outflow_id, network, config) {

  outflow_name <- get_section_idnames(config) %>%
    dplyr::filter(id == outflow_id) %>%
    dplyr::pull(name)

  stopifnot(length(outflow_name) == 1)

  flowpath_df <- calculate_flowpath(
        flow_name = outflow_name,
        links = get_link_names(network),
        nodes = get_nodes(network, config),
        backward = TRUE) %>%
        dplyr::left_join(get_section_idnames(config), by = "name") %>%
        dplyr::relocate("id", .before = "name") %>%
        dplyr::arrange(dplyr::desc(order))

  decreasing_orders <- unique(flowpath_df$order)

  stats::setNames(
    lapply(decreasing_orders, function(order) {
      order_names <- flowpath_df %>%
        dplyr::filter(!!order == order) %>%
        dplyr::pull(name)

      x <- lapply(order_names, function(order_name) {
        get_link_names(network) %>%
          dplyr::filter(source %in% order_name |
                          target %in% order_name) %>%
          dplyr::mutate(order_id = order, order_name = order_name) %>%
          dplyr::relocate(order_id, order_name, .before = "source")

      }) %>% dplyr::bind_rows()

      x[!duplicated(x),]
    }),
    sprintf("orderid_%02d", decreasing_orders)
  )
}
