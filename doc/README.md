# Process CRSS Results

The following attempts to document the process used to process any CRSS runs.  

**Warning:** the code may aggregate data from 35-70 or more individual CRSS runs (depending on the month of the CRSS run). This can be memory intensive. However, the latest processing was performed on a standard laptop with 8 GB of memory.

## Overall Process  

1. Aggregate necessary data, this months and any previous months data, and apply attributes
1. Create figures/tables
  1. provide numbers behind figures, where necessary 
1. Convert May_SysTableFull2016_2026.csv to excel file
1. create results PPT

## Detailed Instructions

### Setting up Your Computer

- Fork a copy of this repository, or update your local branch of an already forked version. This code is setup so that it does not need to be in the folder as the CRSS results you are processing. 
  - You should not have to change the R working directory for any reason
- Before opening R or R-Studio, ensure that the CRSS_DIR environment variable is set correctly. It should point to the CRSS folder that you want the results to be created in. The CRSS folder does not need to contain all of the scenario results, i.e., you can save these results to your local computer while the scenario results are stored on the server.

For example, a typical setup could include the following:

- A local copy of CRSS in C:/CRSS/CRSS.2017; `CRSS_DIR` environment variable would be set to this path, and the results will be saved to C:/CRSS/CRSS.2017/results/someProvidedName
- All of the scenario results stored on some server in a shared folder, e.g., M:/Shared/CRSS/2017/Scenario
- A local copy of this repository stored in C:/CRSS/Process-CRSS-Res

### Generating Standard Figures and Tables

#### Tables and Figures

The following tables and figures are created by running `main.R`. The steps necessary to update `main.R` are included in the following section. The boolean variable that controls whether each set of figures are created are included in parentheses. 
  
* System conditions table (`makeFiguresAndTables`)
  * Trim to 5-years for PPT (not handled in this code)
  * Provide table through 2026 to Modeling Team(s)
* A simple 5-year table which only includes the chance of shortage and the chance of Powell falling below 3,490' as compared to a different model run (`createSimple5yrTable`)
* Standard CRSS results (`makeFiguresAndTables`)
  * Mead/Powell EOCY elevations
  * Percent traces < 3490 and 3525 at Powell
  * Percent of traces < 1020 and 1000 at Mead
  * Percent traces in Shortage and Surplus
  * Distribution of LB shortage by tier
* Conditions leading to Shortage figure (`createShortDonditions`)
  * uses data from MTOM
* A figure showing Mead's end-of-calendar year elevations for its initial conditions (from MTOM) (`addPEScatterFig`)
* Conditional probabilities (`computeConditionalProbs`)
  * **Note:** this code was last utilized in 2015; it likely requires modification to work with the new data configuration.
  * P(2016 Short | WY 2016 Rel = 7.48)
  * P(2017 Short (by tier) | WY 2016 Rel = 7.48)
  * P(2017 Short (by tier) | WY 2016 Rel = 8.23) **this is currently coded as chance of upper 
    elevation balancing with 8.23 release [See Issue #4](https://github.com/rabutler/Process-CRSS-Res/issues/4)**
  * P(2017 Short (by tier) | WY 2016 Rel > 8.23)

#### Configuring `main.R`

Start by editing the User input section of `main.R`:

1. Update the boolean "switches" to create the figures and data you desire. `getSyscondData` and `getPeData` get the data used to create the figures in the following switches, so the code must be run with them set to `TRUE` once before creating any of the other figures. The figures that are created in each of the other sections are described in the previous section. The most common configuration would be to set `getSysCondData`, `getPeData`, `makeFiguresAndTables`, and `createSimple5yrTable` to `TRUE`, and the remaining three variables to `FALSE`. The get data portion can be turned to `FALSE` if they are successfully run once, but the plotting needs to be re-run for some reason.
1. Update `iFolder` to point to the folder that contains all of the scenario results.
1. Update `crssMonth` to a unique folder name. The results created by this code will be saved to `CRSSDIR/crssMonth`.
1. Update the `scens`, `icList`, and `icMonth` variables. These variables should all contain the same names, as they are meant to work together. 
  1. Edit the `scens` variable. This is where you group individual scenarios together, e.g., runs with many different initial conditions. Each entry in the list should be a group of scenarios and the name of the list entry will be used to create "Scenario Groups" refered to as `Agg` in the data frame. For example:
  ```
  scens <- list(
    'April 2016' = makeAllScenNames('Apr2016_2017','DNF','2007Dems','IG',1981:2010),
    'August 2016' = 'Aug2016_2017,DNF,2007Dems,IG'
  )
  ```
  This will create two scenario groups: "April 2016" and "August 2016". "April 2016"" is comprised of 30 individual scenarios, i.e., 30 differenct scenarios will be combined together before computing statistics on the April 2016 runs. The "August 2016" data contains only one scenario. This setup reflects the current SOP for CRSS runs: the April run is initialized 30 times while the August run starts with only one initial condition. The scenario groups are the variable used to label the scenarios in all of the figures.
  *** Make sure that each scenario folder only shows up in one entry of the list. The code does not expect to need to group a scenario in multiple groups. ***
  1. Update `icList`. Again, it should have the same names as `scens`. This variable tells the code what to use for the initial conditions for the Powell and Mead EOCY elevation plots. Each name should either contain a file path to an Excel file that contains all of the MTOM results that are read into CRSS, or numeric variable of length 2. For the latter, the first value is Powell's initial elevation and the second value is Mead's initial elevation. Carrying forward the `scens` example, `icList` would be configured as follows:
  ```
  icList <- list(
    "April 2016" = file.path(CRSSDIR, "dmi/InitialConditions/apr_2016/MTOM2CRSS_Monthly.xlsx"),
    "August 2016" = c(3605.83, 1078.93)
  )
  ```
  Because the "April 2016" scenario group contains 30 individual scenarios, it needs 30 different initial conditions while the "August 2016" scenario group only needs one set of initial conditions.
  1. Update `icMonth`. Again, it should have the same names as `scens`. This variable provides the month that will be used for the above initial conditions in YY-mmm format. Ex:
  ```
  icMonth <- c('April 2016' = '16-Dec', 'August 2016' = '16-Dec')
  ```
1. Update `ss5` and `tableFootnote` as described in `main.R`
1. Update `mainScenGroup` and `mainScenGroup.name`. The `mainScenGroup` is the  scenario group (from `scens`) that will be used for the figures that only show one scenario group while `mainScenGroup.name` is the text that these figures are annotated with.
1. Update `yrs2show` and `peYrs`. These determine the x-axis time frame for the figures. Typically, `peYrs` will include the year of the initial conditions, while `yrs2show` does not need to.

XXX START HERE XXX


1. Run main.R: `source('code/main.R')`
1. Convert [Month]_SysTableFull2016_2026.csv to excel file
1. Verify results
  1. In the system conditions table, the first year conditions should sync with MTOM results. Percent of time in each tier at Powell, may not match exactly due to the April adjustments. For example, MTOM could show a different percent of traces switching from Upper Elevation Balancing to Equalization than CRSS. 
  1. Verify the conditional probability table. The conditional probabilities should add up to the total chances of shortage.
  1. The range of iniitial conditions in the Powell and Mead EOCY plots should sync with the MTOM results.
1. Convert pdf figures to png files
1. Create results Power Point
  1. Use the previous month's PPT as a template
  1. Use system conditions table for the first 5-years and round to the nearest ones. There may be some manual adjustment of the rounded numbers so that tiers that should sum to 100% do.
  1. Use the CondProbs.csv file to fill in the conditional probability table.
  1. Add other figures
1. Send an email to the Reclamation only modeling team to review the results. Include the following files:
  * PowerPoint File
  * System Conditions Excel file
  * Critical Stats csv file
  * The 5-year simple table in the body of the email
  * Summary of the results
  
1. Prepare files for the Conditions Leading to a Shortage in 2016 plot **or** set `createShortConditions` in `main.R` to `FALSE`. Uses MTOM results to develop this figure. See CRSS_DIR/MTOM/FirstYearCondMTOM/[Month]_MTOM.xlsx for example.
  1. Copy over Shortage.Shortage Flag, Powell.Pool Elevation, Powell.Outflow, and PowellActualAnnual ReleaseVolume sheets from the MTOM ensemble spreadsheet
  1. Sum the Oct-Dec Powell.Outflow for current year
  1. Copy the 1981-2010 columns of the Oct-Dec release, WYRelease, Shortage, and Dec Elevation to the appropriate columns in the DataToExport worksheet.
    * The LBPrct sheet should not change aslong as the LB inflows are using 1981-2010
  1. Export the DataToExport worksheet to csv file.
  1. Code to create the figure is called in main.R, but it is in plotFirstYearShortCond.R
    * **Will likely need to edit the x and y coordinates in the annotation calls in main.R**
  
Posting results for Stakeholder Modeling Workgroup:
1. Edit the USER INPUT in code/moveFiles.R and save the file.
  * `scens.limit` should include the 30 ensemble initial conditions from MTOM
    * ***`scens.limit` was removed from the code for the August update. Next time there are 30 scenarios, it may need to be added back in.***
  * `iFolder` is the location of the results generated by RiverSMART 
  * `oFolder` is the location on Dirty Devil that will is shared with Stakeholder Modeling Workgroup
  * `ff` is the list of Excel files to copy. By default, Flags, KeySlots, OWDAnn, Res, xtraRes, and SystemConditions are moved over.
1. Move the files over by running moveFiles.R: `source('code/moveFiles.R')`
1. Copy the KeySlots.csv file to the Stakeholder Modeling Work Group area (by hand)
1. Copy the CRSS zip file to the same directory.
	
### Processing Files to Post

1. Use the Post-process function in RiverSMART to create the necessary Excel files for posting to Stakeholder Workgroup. 
1. Copy over all of the scenario folders from the local machine to the server.
