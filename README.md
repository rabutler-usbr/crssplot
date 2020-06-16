## Process CRSS Results

Repository includes code used to process CRSS results, starting in April 2015.

Code extends [RWDataPlyr](https://github.com/BoulderCodeHub/RWDataPlyr) and relies on
[CRSSIO](https://github.com/BoulderCodeHub/CRSSIO) (custom R Packages) to generate the "standard" CRSS figures and tables. Required packages are:

* CoRiverNF (https://github.com/BoulderCodeHub/CoRiverNF)
* CRSSIO (https://github.com/BoulderCodeHub/CRSSIO)
* data.table
* devtools
* dplyr
* feather
* ggplot2
* grid
* gridExtra
* reshape2
* RWDataPlyr
* scales
* stringr
* tidyr

### Setup

Ensure the above packages are installed on the computer. CoRiverNF and CRSSIO can be installed as follows, while the others can be installed from CRAN:

```
library(devtools)
devtools::install_github('BoulderCodeHub/CoRiverNF')
devtools::install_github('BoulderCodeHub/CRSSIO')
```

### Usage

1. Setup a yaml file that determines which scenarios are used and which plots are created. [yaml Specification](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification) includes the details about the yaml file configuration.
  - It does not matter where you save this file. It can be saved in this repository or in the CRSS folder.
2. Edit [main.R](https://github.com/BoulderCodeHub/Process-CRSS-Res/blob/master/code/main.R).
  - set the path in `ui <- parse_yaml_input("some/path/file.yaml")` to point to the yaml file created in step 1. The path can be an absolute path or a path relative to the Process-CRSS-Res folder. 
3. Call `source("code/main.R")` to process all of the results.

See [doc/README.md](doc/README.md) for more details on how this process works in the overall CRSS publication process. 
