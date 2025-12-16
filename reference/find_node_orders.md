# Helper function: find node order

Helper function: find node order

## Usage

``` r
find_node_orders(links, nodes, start_node, reverse = FALSE, mode = "out")
```

## Arguments

- links:

  links

- nodes:

  nodes

- start_node:

  start node

- reverse:

  should network direction be reversed? (default: FALSE)

- mode:

  Character constant, gives whether the shortest paths to or from the
  given vertices should be calculated for directed graphs. If out then
  the shortest paths from the vertex, if ⁠in⁠ then to it will be
  considered. If all, the default, then the graph is treated as
  undirected, i.e. edge directions are not taken into account. This
  argument is ignored for undirected graphs (default: "out")

## Value

data frame with additional column "order"
