#' Helper function: find node order
#'
#' @param links links
#' @param nodes nodes
#' @param start_node start node
#' @param reverse should network direction be reversed? (default: FALSE)
#' @param mode 	Character constant, gives whether the shortest paths to or from
#' the given vertices should be calculated for directed graphs. If out then the
#' shortest paths from the vertex, if ⁠in⁠ then to it will be considered. If all,
#' the default, then the graph is treated as undirected, i.e. edge directions
#' are not taken into account. This argument is ignored for undirected graphs
#' (default: "out")
#'
#' @return data frame with additional column "order"
#' @export
#' @importFrom igraph distances reverse_edges V vcount graph_from_data_frame
#' @importFrom dplyr mutate
#' @importFrom rlang .data
find_node_orders <- function(links, nodes, start_node, reverse = FALSE, mode = "out") {
  # Erstellen eines gerichteten Graphen aus den Links
  graph <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)

  if (reverse) {
    graph <- igraph::reverse_edges(graph)
  }

  # Berechnung der topologischen Sortierung basierend auf Abhängigkeiten
  topo_order <- igraph::topo_sort(graph, mode = mode)

  # Initialisieren der Ordnungen
  orders <- rep(NA, igraph::vcount(graph))
  names(orders) <- igraph::V(graph)$name

  # Zuweisung der Ordnungen gemäß der topologischen Sortierung
  for (i in seq_along(topo_order)) {
    node_name <- igraph::V(graph)$name[topo_order[i]]
    # Berechne die maximale Reihenfolge der Vorgängerknoten (from_id)
    predecessors <- igraph::neighbors(graph, v = topo_order[i], mode = "in")
    if (length(predecessors) == 0) {
      orders[node_name] <- 0  # Startknoten erhält Order 1
    } else {
      orders[node_name] <- max(orders[igraph::V(graph)$name[predecessors]]) + 1
    }
  }

  # Hinzufügen der Ordnungen zu den Knoten
  nodes <- nodes %>%
    dplyr::mutate(order = orders[.data$name])

  return(nodes)
}

#' Calculate Flowpath
#'
#' @param flow_name flow name (as named in nodes) where calculation should start
#' @param links links
#' @param nodes nodes
#' @param backward should flowpath be calculated backwards? (default: FALSE)
#'
#' @return data frame with column "order" indicating the flow path order
#' @export
#' @importFrom dplyr arrange desc filter
calculate_flowpath <- function(flow_name,
                               links,
                               nodes,
                               backward = FALSE) {

  stopifnot(flow_name %in% nodes$name)
  stopifnot(any(nodes$group == "Fliessabschnitt"))

  find_node_orders(links = links,
                   nodes = nodes,
                   start_node = flow_name,
                   reverse = backward,
                   mode = "out") %>%
    dplyr::arrange(dplyr::desc(.data$order)) %>%
    dplyr::filter(.data$group == "Fliessabschnitt",
                  !is.na(.data$order))
}
