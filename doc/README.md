## Process CRSS Results

The following attempts to document the process used in the May CRSS run to compute all 
results. 

**Warning:** the code aggregates data from 30 individual CRSS runs. This can be memory intensive.
The aggregating code was run on a computer with 48GB of memory. There is not guarantee 
that the same aggregating code will work on a PC. 

1. Aggregate necessary data
  1. Append EOCY 2015 elevations from MTOM onto Mead/Powell EOCY elevations
    1. Need to save csv file from the Powell elevations and Mead elevations from the MTOM 
       Ensemble output spreadsheets.
      1. Delete header rows, so that only header row is one that has years in them
      1. Edit column names for Run0-Run2 to correspond to MTOM_Min, MTOM_Max, MTOM_Most
      1. Add in column for other runs, e.g., min, most, max. Only need data for month of I.C.
1. Combine with previous month data (at least for EOCY data)
1. Create figures/tables
  1. provide numbers behind figures, where necessary 
  
  
## Tables and Figures
  
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
    elevation balancing with 8.23 release (See Issue #4)[https://github.com/rabutler/Process-CRSS-Res/issues/4]**
  * P(2017 Short (by tier) | WY 2016 Rel > 8.23)
* Conditions leading to Shortage figure
  * uses data from MTOM
	