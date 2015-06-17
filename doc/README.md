## Process CRSS Results

The following attempts to document the process used in the May CRSS run to compute all 
results. 

**Warning:** the code aggregates data from 30 individual CRSS runs. This can be memory intensive.
The aggregating code was run on a computer with 48GB of memory. There is not guarantee 
that the same aggregating code will work on a PC. 

### Overall Process  

1. Aggregate necessary data, and apply attributes
1. Combine with previous month(s) CRSS data (at least for EOCY data)
1. Create figures/tables
  1. provide numbers behind figures, where necessary 
1. Convert May_SysTableFull2016_2026.csv to excel file
1. create results PPT

### Detailed Instructions

Follow these instructions for updating the CRSS results:

1. Get initial conditions data from MTOM results
  1. Append EOCY 2015 elevations from MTOM onto Mead/Powell EOCY elevations
    1. Need to save csv file from the Powell elevations and Mead elevations from the MTOM Ensemble output spreadsheets. The Ensemble Output spreadsheet should be requested from BCOO and saved to Manoa under the MTOM directory.
      1. Delete header rows, so that only header row contains years (keep row 2)
      1. Edit column names for columns B, C, and D to correspond to MTOM_Min, MTOM_Max, MTOM_Most
      1. Add in column for other runs, e.g., Min, Most, Max. Only need data for month of I.C. For example, add in December 2015 elevation from latest 24-Month Study results.
      1. If making the changes to the csv file in Excel, ensure the formatting of the numbers is correct. For example, after adding in the December 2015 elevation, make sure it appears as a number and not a date.
    1. Make sure the csv file does not have any extra, empty rows. If above changes were made in Excel, it's worth opening in a text editor to see what the end of the file looks like. There should be one blank line at the end of the file, and not many rows that are comma seperated with no data.
1. Edit the main.R file
  1. Edit the variables in the user input area, as necessary.
  1. Be sure to add an additional value to `startMonthMap`. For example, if updating for new June results, do not only edit the May entry, but add in a June entry. This ensures backwards compatability with previous month results. Example:
  ```
  # original value
  startMonthMap <- c('May2015_2016' = 'May 2015 DNF', 'Apr2015_2016_a3' = 'Apr 2015 DNF')
  # new value
  startMonthMap <- c('Jun2015_2016' = 'Jun 2015 DNF', 'May2015_2016' = 'May 2015 DNF', 
	  'Apr2015_2016_a3' = 'Apr 2015 DNF')
  ```
1. Prepare files for the Conditions Leading to a Shortage in 2016 plot **or** set `createShortConditions` in `main.R` to `FALSE`. Uses MTOM results to develop this figure. See CRSS_DIR/MTOM/FirstYearCondMTOM/[Month]_MTOM.xlsx for example.
  1. Copy over Shortage.Shortage Flag, Powell.Pool Elevation, Powell.Outflow, and PowellActualAnnual ReleaseVolume sheets from the MTOM ensemble spreadsheet
  1. Sum the Oct-Dec Powell.Outflow for current year
  1. Copy the 1981-2010 columns of the Oct-Dec release, WYRelease, Shortage, and Dec Elevation to the appropriate columns in the DataToExport worksheet.
    * The LBPrct sheet should not change aslong as the LB inflows are using 1981-2010
  1. Export the DataToExport worksheet to csv file.
  1. Code to create the figure is called in main.R, but it is in plotFirstYearShortCond.R
    * **Will likely need to edit the x and y coordinates in the annotation calls in main.R**
1. Set working directory to CRSS-Process-Res folder
1. Make sure CRSS_DIR environment variable is set to the CRSS folder, e.g., C:/model/CRSS/CRSS.2015
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

### Tables and Figures

The following tables and figures are created by running the above code
  
* System conditions table
  * Trim to 5-years for PPT
  * Provide table through 2026 to Modeling Team(s)
* Standard CRSS results
  * Mead/Powell EOCY elevations
  * Percent traces < 3490 
  * Percent traces in Shortage and Surplus
* Critical elevation thresholds
  * Percent traces < 3490 
  * Percent of traces < 1025, 1020, 1000
  * Percent of traces in LB Shortage and Level 3 shortage
* Conditional probabilities
  * P(2016 Short | WY 2016 Rel = 7.48)
  * P(2017 Short (by tier) | WY 2016 Rel = 7.48)
  * P(2017 Short (by tier) | WY 2016 Rel = 8.23) **this is currently coded as chance of upper 
    elevation balancing with 8.23 release [See Issue #4](https://github.com/rabutler/Process-CRSS-Res/issues/4)**
  * P(2017 Short (by tier) | WY 2016 Rel > 8.23)
* Conditions leading to Shortage figure
  * uses data from MTOM
	