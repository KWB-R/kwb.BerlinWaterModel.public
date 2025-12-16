#' Plot Network: simple
#'
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#'
#' @return simple network graph
#' @export
#' @importFrom networkD3 simpleNetwork
plot_network_simple <- function(network) {

  stopifnot(all(c("from_name", "to_name") %in% names(network)))

  networkD3::simpleNetwork(
    network[, c("from_name", "to_name")],
    #, height="100px", width="100px",
    Source = 1,
    # column number of source
    Target = 2,
    # column number of target
    linkDistance = 10,
    # distance between node. Increase this value to have more space between nodes
    charge = -900,
    # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
    fontSize = 14,
    # size of the node names
    fontFamily = "serif",
    # font og node names
    linkColour = "#666",
    # colour of edges, MUST be a common colour for the whole graph
    nodeColour = "#69b3a2",
    # colour of nodes, MUST be a common colour for the whole graph
    opacity = 0.9,
    # opacity of nodes. 0=transparent. 1=no transparency
    zoom = T                    # Can you zoom on the figure?
  )
}

#' Plot Network: complex
#'
#' @param network tibble with water cycle flow network data, as retrieved by
#' \code{{\link{prepare_network}}}
#' @param config list with config as imported with \code{\link{config_read}}
#' @param show_labels show labels (default: FALSE)
#' @return complex network graph
#' @export
#' @importFrom dplyr bind_rows distinct filter left_join mutate select rename
#' @importFrom tibble tibble
#' @importFrom networkD3 forceNetwork JS
#' @importFrom stringr str_starts
#' @importFrom htmlwidgets onRender
#' @importFrom utils capture.output
plot_network_complex <- function(network, config, show_labels = FALSE) {

  stopifnot(all(c("from_name", "source_group", "to_name", "target_group") %in% names(network)))
  stopifnot("volume_m3" %in% names(config$sections))

  nodes <- get_nodes(network, config)

  duplicated_nodenames <- nodes$name[duplicated(nodes$name)]

  if (length(duplicated_nodenames) > 0) {
    duplicated_nodes <- nodes %>% dplyr::filter(.data$name %in% duplicated_nodenames)
    stop(sprintf("The following nodes contain duplicated names. Please check and fix in your config files:\n\n%s",
                 paste(utils::capture.output(print(duplicated_nodes)), collapse = "\n")))
  }


  links_names <- get_link_names(network)

  ids <- c(links_names$source, links_names$target)
  ids <- ids[!duplicated(ids)]
  ids <- ids[order(ids)]

  ids_df <- tibble::tibble(name = ids, id = 0:(length(ids) - 1))

  links_ids <- links_names %>%
    dplyr::mutate(
      source = sapply(.data$source, function(name)
        ids_df$id[ids_df$name == name]),
      target = sapply(.data$target, function(name)
        ids_df$id[ids_df$name == name])
    )

  # Ueberpruefen der benoetigten Spalten
  required_links_cols <- c("source", "target", "value")
  required_nodes_cols <- c("name", "group", "nodesize")

  missing_links_cols <- setdiff(required_links_cols, colnames(links_ids))
  missing_nodes_cols <- setdiff(required_nodes_cols, colnames(nodes))

  if (length(missing_links_cols) > 0) {
    stop(paste("Fehlende Spalten in links_ids:", paste(missing_links_cols, collapse = ", ")))
  }

  if (length(missing_nodes_cols) > 0) {
    stop(paste("Fehlende Spalten in nodes:", paste(missing_nodes_cols, collapse = ", ")))
  }

  # Ueberpruefen, ob alle source und target Werte gueltige Knoten sind
  invalid_sources <- setdiff(links_ids$source, seq_len(nrow(nodes)) - 1)
  invalid_targets <- setdiff(links_ids$target, seq_len(nrow(nodes)) - 1)

  if (length(invalid_sources) > 0 | length(invalid_targets) > 0) {
    stop(paste("Ungueltige source/target Werte gefunden. Ungueltige Quellen:", paste(invalid_sources, collapse = ", "),
               ". Ungueltige Ziele:", paste(invalid_targets, collapse = ", ")))
  }


  colourScale <- networkD3::JS(
    'd3.scaleOrdinal()
                     .domain(["Zufluss", "Regenwasser (Oberfl\u00E4chenabfluss)", "Regenwasser (direkt)", "Fliessabschnitt", "Verdunstung (potentiell)", "Trinkwasserf\u00F6rderung", "Klarwasser", "Mischwasser\u00FCberlauf", "Seeleitung", "Outflow"])
                     .range(["grey", "darkblue", "blue", "lightblue", "yellow" , "orange", "darkred", "red" , "green", "black"]);'
  )


  p <- networkD3::forceNetwork(
    Links = links_ids,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    Nodesize = "nodesize",
    Group = "group",
    opacity = 0.8,
    colourScale = colourScale,
    legend = TRUE,
    arrows = TRUE,
    fontSize = 7,
    zoom = TRUE,
    opacityNoHover = 1,
    linkDistance = networkD3::JS("function(d) { return d.value * 20; }")
  )

  # Benutzerspezifisches JavaScript, um die Beschriftungen immer anzuzeigen
  if (show_labels) {
    p <- htmlwidgets::onRender(p, 'function(el) {
      d3.selectAll(".node text").style("visibility", "visible");
    }')
  }

  return(p)

}
