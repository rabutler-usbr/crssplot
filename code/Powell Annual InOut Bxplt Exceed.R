##############################################################################
#This script creates annual boxplots of all traces and exceedances for 
#Powell Inflow and Outflow to compare two CRSS runs
#It is intended as an example of how Generic_annual_plot.R can be ADAPTED to 
#create a custom set of outputs

#Contents 
## 1. Set Up ##
## 2. User Input ##
## 3. Process Results ## 
## 4. Plot ## 

#   Created by C. Felletter 10/2018
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 
library(RWDataPlyr)

## Directory Set Up
#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scens are folder are kept
scen_dir = file.path(CRSSDIR,"Scenario") 
#containing the sub folders for each ensemble

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

list.dirs(scen_dir) #list dirs in set folder folder for us in next input

#### Normally You'll Only Change The Below ####
#scens you want to compare 
scens <- list(
  "Aug 2018" = "Aug2018_2019,DNF,2007Dems,IG,Most",
  "Aug 2018 + Fix" = "Aug2018_2019_9000,DNF,2007Dems,IG_9000,Most_BM_FGltsp"
)


list.files(file.path(scen_dir,scens[1])) #list files in scen folder for next input

#rdf file with slot you want 
file = "Res.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],file))) #check slots in rdf

#### ADAPTATION ####
variables = c("Powell.Inflow","Powell.Outflow")

floworpe = "flow" #"flow" or "pe" 
cyorwy = "cy" #"cy" or "wy" 

#plot inputs 
startyr = 2019 #filter out all years > this year
filteryrlessorequal = 2026 #filter out all years > this year

#file names 
figs <- 'Powell_Annual_InOut' #objectslot + .pdf will be added when creating plots

#### ADAPTATION ####
figuretypes <- c(2,3,2,3) #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance 
# IF PICKING 3 you must specify a month
exc_month = 12 #1 - Jan, 12 - Dec

#### End of Normally You'll Only Change This ####

# the mainScenGroup is the name of the subfolder this analysis will be stored
#under in the results folder 
mainScenGroup <- names(scens)[1]

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
if(!(mainScenGroup %in% names(scens))) 
  stop(mainScenGroup, ' is not found in scens.')

# check folders
if(!file.exists(file.path(scen_dir, scens[1])) 
   | !file.exists(file.path(scen_dir, scens[2])))
  stop('scens folder(s) do not exist or scen_dir is set up incorrectly. 
       Please ensure scens is set correctly.')

ofigs <- file.path(CRSSDIR,'results',mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

## create a pdf  
#### ADAPTATION ####
pdf(paste0(file.path(ofigs,figs),".pdf"), width=9, height=6)

#### ADAPTATION ####
for(i in 1:length(variables)){ #loop through variables and also possibly floworpe
  #cyorwy 

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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #### ADAPTATION ####
variable = variables[i] #loop through multiple

  #generic agg file 
rwa1 <- rwd_agg(data.frame(
  file = file,
  slot = variable, 
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
  variable = variable,
  stringsAsFactors = FALSE
))

#rw_scen_aggregate() will aggregate and summarize multiple scens, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scens.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) 

unique(scen_res$Variable) #check variable names
# unique(scen_res$TraceNumber) #check trace numbers 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Choosen Figure Type 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### ADAPTATION ####
for (j in 1:length(figuretypes)){ #loop through fig types 

figuretype <- figuretypes[j]   
  
#    -------------------        All Trace Mean        ----------------------

if (figuretype == 1){
  p <- scen_res %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year >= startyr) %>% #filter year
    dplyr::filter(Year <= filteryrlessorequal) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
    geom_line() +
    geom_point() +
    labs(title = paste("Mean",variable,startyr,"-",filteryrlessorequal), 
         y = y_lab, x = "Year") 
  print(p)
}

#    -------------------        All Trace Boxplot        ----------------------

if (figuretype == 2){
  p <- scen_res %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year >= startyr) %>% #filter year
    dplyr::filter(Year <= filteryrlessorequal) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
    geom_boxplot() +
    labs(title = paste(variable,startyr,"-",filteryrlessorequal), 
         y = y_lab, x = "Year") 
  print(p)
}

#    -------------------        Percent Exceedance of Traces       ----------------------

if (figuretype == 3){
  p <- scen_res %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year >= startyr) %>% #filter year
    dplyr::filter(Year <= filteryrlessorequal) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    ggplot(aes(Value, color = Scenario)) + 
    stat_eexccrv() + 
    labs(title = paste(variable,"Trace Exceedance",startyr,"-",filteryrlessorequal), 
         y = y_lab, x = "Year") 
  print(p)
}

#### ADAPTATION ####
} #close j fig type loop
} #close i variable loop


dev.off()
