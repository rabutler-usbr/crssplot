## Process CRSS Results
Repository includes code used to process CRSS results, starting in April 2015.

Code relies on [RWDataPlot](https://github.com/rabutler/RWDataPlot) and 
[CRSSIO](https://github.com/rabutler/CRSSIO) (custom R Packages) and the following packages available on CRAN:
* devtools
* dplyr
* ggplot2
* grid
* gridExtra
* reshape2
* scales

Ensure the above packages are installed on the computer. RWDataPlot and CRSSIO can be installed as follows:
```
library(devtools)
devtools::install_github('rabutler/RWDataPlot')
devtools::install_github('rabutler/CRSSIO')
```

Ensure that the environment variable CRSS_DIR is set on the computer.

See [doc](doc) for detailed instructions on updating and running the code each month.
