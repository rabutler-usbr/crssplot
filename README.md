
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crssplot

**WIP** - working to convert
[BoulderCodeHub/Process-CRSS-Res](https://github.com/BoulderCodeHub/Process-CRSS-Res)
into a package.

Goals are to:

1.  Provide an easy, mostly automated way to produce the “standard” crss
    figures accross many scenarios.
2.  Provide a set of plots with a common API for creating new CRSS
    figures for any slot/variable. These functions can be wrapped in
    code from goal 1 in this package or used in other
    analyses/frameworks.

## Setup

    library(devtools)
    devtools::install_github('rabutler-usbr/crsspub')

## Standard Figures

1.  Setup a yaml file that determines which scenarios are used and which
    plots are created. [yaml
    Specification](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification)
    includes the details about the yaml file configuration.
      - It does not matter where you save this file. It can be saved in
        this repository or in the CRSS folder.
2.  Then call:

<!-- end list -->

    ui <- parse_yaml_input("path/to/yml/file.yml")
    process_everything(ui)

See
[doc/README.md](https://github.com/BoudlerCodeHub/Process-CRSS-Res/doc/README.md)
for more details on how this process works in the overall CRSS
publication process.

## Generic Plotting Functions

This section demonstrates how to use the generic plotting functions to
recreate the standard CRSS results, though it does so using a
truncated/sample set of them.

``` r
library(crssplot)
library(patchwork)
# ex_pe is sample data frame that comes from RWDataPlyr and running the 
# automated process above
# TODO: add in sample code that shows how to generate ex_pe from RWDataPlyr and
# add in the ScenarioGroup and add in the initial conditions
```

``` r
# Cloud figures
```

``` r
p1 <- scens_plot_probs(
  ex_pe, years = 2021:2026,
  vars = "powell_wy_min_lt_3525", 
  subtitle = "Percent of traces less than elevation 3,525' in any water year"
)
p2 <- scens_plot_probs(
  ex_pe, years = 2021:2026,
  vars = "powell_wy_min_lt_3490", 
  subtitle = "Percent of traces less than elevation 3,490' in any water year"
)

p1 + p2 +
  plot_layout(guides = "collect")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
