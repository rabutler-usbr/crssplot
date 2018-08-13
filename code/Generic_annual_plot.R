##############################################################################
#This script creates annual plots of Outflow and PE to compare two CRSS runs

#DEVELOPMENT IS ON GOING ON THIS

#Contents 
## 1. Set Up ##
## 2. User Input ##
## 3. Process Results ## 
## 4. Plot ## 

#   Created by C. Felletter 8/2018
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 
library(RWDataPlyr)

## Directory Set Up
#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scenarios are folder are kept
scen_dir = file.path(CRSSDIR,"Scenario") 
#containing the sub folders for each ensemble

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

list.dirs(scen_dir) #list dirs in set folder folder for us in next input

#### Normally You'll Only Change The Below ####
#scenarios you want to compare 
scenarios <- list( #Short Name for Run = File Name with RDFs
  "April 2018" = "Apr18,DNF,2007Dems,IG,Most", 
  "Asp.NoTribs" = "9601.NoTribs,DNF,2007Dems,IG.9003.NoTribs,MTOM_Most"
)

list.files(file.path(scen_dir,scenarios[1])) #list files in scen folder for next input

#rdf file with slot you want 
file = "KeySlots.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scenarios[1],file))) #check slots in rdf

variables = "Mead.Pool Elevation"
# variables = "Powell.Inflow"

floworpe = "pe" #"flow" or "pe" 
cyorwy = "cy" #"cy" or "wy" 

#plot inputs 
startyr = 2019 #filter out all years < this year
endyr = 2022 #filter out all years > this year

#file names 
figs <- 'Generic_AnnualFig' #objectslot + .pdf will be added when creating plots

figuretype <- 2 #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance 
# IF PICKING 3 you must specify a month
exc_month = 12 #1 - Jan, 12 - Dec

#### End of Normally You'll Only Change This ####

# the mainScenGroup is the name of the subfolder this analysis will be stored
#under in the results folder 
mainScenGroup <- names(scenarios)[1]

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Additional plotting functions and libraries 
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
#see RWDATPlyr Workflow for more information 
# library(CRSSIO)
# plotEOCYElev() and csVarNames()
source('code/Stat_emp_ExcCrv.r')

# some sanity checks that UI is correct:
if(!(mainScenGroup %in% names(scenarios))) 
  stop(mainScenGroup, ' is not found in scenarios.')

# check folders
if(!file.exists(file.path(scen_dir, scenarios[1])) 
   | !file.exists(file.path(scen_dir, scenarios[2])))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly. 
       Please ensure Scenarios is set correctly.')

ofigs <- file.path(CRSSDIR,'results',mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

#y axis titles 
if (floworpe == "flow"){
  if (cyorwy == "cy"){
    y_lab = "Annual CY Flow (ac-ft/mo)"
  } else {
    y_lab = "Annual WY Flow (ac-ft/mo)"
  }
} else {
  if (cyorwy == "cy"){
    y_lab = "EOCY PE (ft)"
    peperiod = "eocy"
  } else {
    y_lab = "EOWY PE (ft)"
    peperiod = "eowy"
  }
}
figuretypes = c("Mean","Bxplt","Exceedance")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#generic agg file 
rwa1 <- rwd_agg(data.frame(
  file = file,
  slot = variables, 
  period = if (floworpe == "flow"){
    cyorwy
  } else{
    peperiod
  },
  summary = if (floworpe == "flow"){
    "sum"
  } else{
    NA
  },
  eval = NA,
  t_s = NA,
  variable = variables,
  stringsAsFactors = FALSE
))

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scenarios,
  agg = rwa1,
  scen_dir = scen_dir
) 

unique(scen_res$Variable) #check variable names
unique(scen_res$TraceNumber) #check trace numbers 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Choosen Figure Type 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(paste0(file.path(ofigs,figs),"_",variables,"_",figuretypes[figuretype],".pdf"), width=9, height=6)

variable = variables

#    -------------------        All Trace Mean        ----------------------

if (figuretype == 1){
  p <- scen_res %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
    geom_line() +
    geom_point() +
    labs(title = paste("Mean",variable,startyr,"-",endyr), 
         y = y_lab, x = "Year") 
  print(p)
}

#    -------------------        All Trace Boxplot        ----------------------

if (figuretype == 2){
  p <- scen_res %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
    geom_boxplot() +
    labs(title = paste(variable,startyr,"-",endyr), 
         y = y_lab, x = "Year") 
  print(p)
}

#    -------------------        Percent Exceedance of Traces       ----------------------

if (figuretype == 3){
  p <- scen_res %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    ggplot(aes(Value, color = Scenario)) + 
    stat_eexccrv() + 
    labs(title = paste(variable,"Trace Exceedance",startyr,"-",endyr), 
         y = y_lab, x = "Year") 
  print(p)
}


dev.off()
