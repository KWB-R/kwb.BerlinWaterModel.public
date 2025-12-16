# Plot Flows and BF & MAR shares per section

Plot Flows and BF & MAR shares per section

## Usage

``` r
plot_flows_and_bfshares_per_section(
  config,
  flows,
  network,
  ww,
  scale_factor = 1,
  debug = TRUE,
  add_caption_simulation = FALSE,
  add_caption_weblink = FALSE,
  add_caption_outflowsmultiple = FALSE
)
```

## Arguments

- config:

  list with config as imported with
  [`config_read`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/config_read.md)

- flows:

  flows as retrieved by xxxx

- network:

  tibble with water cycle flow network data, as retrieved by
  `{`[`prepare_network`](https://kwb-r.github.io/kwb.BerlinWaterModel.public/reference/prepare_network.md)`}`

- ww:

  waterworks dataset (default: kwb.BerlinWaterModel.public::ww)

- scale_factor:

  scale factor for increasing size of labels

- debug:

  print debug messages (default: TRUE)

- add_caption_simulation:

  add caption with simulation information (default: FALSE)

- add_caption_weblink:

  Add one line in caption to reference webpage of vignette (default:
  FALSE)

- add_caption_outflowsmultiple:

  Add information of model configuration for multiple outflows (default:
  FALSE)

## Value

plots flows and BF & MAR shares for each section
