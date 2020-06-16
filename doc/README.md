# Process CRSS Results

The following attempts to document the process used to process any CRSS runs.  

**Warning:** the code may aggregate data from 35-70 or more individual CRSS runs (depending on the month of the CRSS run). This can be memory intensive. However, the latest processing was performed on a standard laptop with 8 GB of memory.

## Overall Process  

1. Aggregate necessary data, data for this month and any previous month, and apply attributes
1. Create figures/tables
    1. Provide numbers behind figures, where necessary 
1. Convert May_SysTableFull2016_2026.csv to excel file
1. create results PPT

Steps 1 and 2 are handled in this repository.

## Detailed Instructions

### Setting up Your Computer

- Fork a copy of this repository, or update your local branch of an already forked version. This code is setup so that it does not need to be in the same folder as the CRSS results you are processing. After you complete the processing for the current run, issue a pull request into BoulderCodeHub, and then create a release tag, e.g., "January 2016 Official".
  - **You should not have to change the R working directory for any reason.**
- Before opening R or R-Studio, ensure that the CRSS_DIR environment variable is set correctly. It should point to the CRSS folder that you want the results to be created in. The CRSS folder does not need to contain all of the scenario results, i.e., you can save these results to your local computer while the scenario results are stored on the server.

For example, a typical setup could include the following:

- A local copy of CRSS in C:/CRSS/CRSS.2017; `CRSS_DIR` environment variable would be set to this path, and the results will be saved to C:/CRSS/CRSS.2017/results/someProvidedName
- All of the scenario results stored on some server in a shared folder, e.g., M:/Shared/CRSS/2017/Scenario
- A local copy of this repository stored in C:/CRSS/Process-CRSS-Res

If you find any issues when running the code, please submit [issues on Github](https://github.com/BoulderCodeHub/Process-CRSS-Res/issues).

### Generating Standard Figures and Tables

The user provided yaml file determines which figures and tables are created for which scenarios. The [yaml specification](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification) page provides the details about how this file must be specified.

#### Per-Scenario Tables and Figures

Some figures and tables can be created for individual scenarios. See ["scenario plot specifications"](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification#scenario-plot-specifications). These include: 
* "standard individual figures" - the chance of shortage and surplus on one plot; a barchart of the different shortage tiers for 1 scenario
* "standard individual tables" - tables of the chances of being in any IG/DCP tier for each year. 
* "shortage conditions" - A scatter plot identifying which traces end the specified year in shortage, the October-December Powell release and Powell WY release from the previous year, and the LB intervening gains (or MWD ICS delivery/creation) for each of theses traces.
* "Mead pool elevation scatter plot" - a scatter plot of Mead December elevation vs. trace number for traces near 1,075'.

#### Scenario Comparison Figures

Additional plots are useful for comparing multiple scenarios. See ["plot_group"](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification#plot_group). These figures include:
* "standard comparisons" - plots comparing the chances of falling below various elevations at Mead and Powell and the End-of-December 10/50/90th percentiles. 
* "annual computed state depletions (csd)" - box plots of the annual CSD for each state
* "heatmaps" - a heatmap showing the chance that Powell and Mead are in various elevation bands. Best kept to two scenarios.
* "cloud plots" - shaded area plots that shade the 10th-90th percentile December elevations for Powell and Mead, and show the Median. The plots also include historical elevation.

#### Configuring `main.R`

Edit [main.R](https://github.com/BoulderCodeHub/Process-CRSS-Res/blob/master/code/main.R) so the path in `ui <- parse_yaml_input("some/path/file.yaml")` points to the yaml file. The path can be an absolute path or a path relative to the Process-CRSS-Res folder. 
	
### Processing Files to Post to Stakeholder Modeling Team

Use the [crssrelease](https://github.com/BoulderCodeHub/crssrelease) package to finish up the process of creating the necessary files to post for the Stakeholder Modeling Team. The README file for that package provides examples for using it to:

1. Create the files that are posted
2. Create the model changes pdf
3. Create the CRSS zip package
