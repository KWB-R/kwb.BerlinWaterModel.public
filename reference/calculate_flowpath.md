# Calculate Flowpath

Calculate Flowpath

## Usage

``` r
calculate_flowpath(flow_name, links, nodes, backward = FALSE)
```

## Arguments

- flow_name:

  flow name (as named in nodes) where calculation should start

- links:

  links

- nodes:

  nodes

- backward:

  should flowpath be calculated backwards? (default: FALSE)

## Value

data frame with column "order" indicating the flow path order
