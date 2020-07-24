## crssplot

**WIP** - working to convert [BoulderCodeHub/Process-CRSS-Res](https://github.com/BoulderCodeHub/Process-CRSS-Res) into a package. 

Goals are to: 

1. Provide an easy, mostly automated way to produce the "standard" crss figures accross many scenarios.
2. Provide a set of plots with a common API for creating new CRSS figures for any slot/variable. These functions can be wrapped in code from goal 1 in this package or used in other analyses/frameworks.

### Setup

```
library(devtools)
devtools::install_github('rabutler-usbr/crsspub')
```

### Standard Figures

1. Setup a yaml file that determines which scenarios are used and which plots are created. [yaml Specification](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification) includes the details about the yaml file configuration.
    * It does not matter where you save this file. It can be saved in this repository or in the CRSS folder.
2. Then call:

```
ui <- parse_yaml_input("path/to/yml/file.yml")
process_everything(ui)
```

See [doc/README.md](https://github.com/BoudlerCodeHub/Process-CRSS-Res/doc/README.md) for more details on how this process works in the overall CRSS publication process. 
