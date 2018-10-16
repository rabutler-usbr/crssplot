##############################################################################
#Generic Script to make Multitrace Plot and Exceedance Plot for Flow from rdf

#Only supports CY daily flow for FG and Aspinall

#DEVELOPMENT IS ON GOING ON THIS

#Contents 
## 1. Set Up ##
## 2. User Input ##
## 3. Process Results ## 
## 4. Plot ## 

#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

## Directory Set Up
#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scenarios are folder are kept
scen_dir = file.path(CRSSDIR,"Scenario") 
#containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

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

# list.files(file.path(scen_dir,scens[1])) #list files in scen folder for next input

#variables, figuretypes, customcaptions & y_labs should be set to a single value
#but could be used to loop through multiple plots if all c() variable were extended 
#see example below 


#rdf file with slot you want, daily output only in this one file 
file = "DailyFlows.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],file))) #check slots in rdf

## Process Variables ##

rdffiles <- c("DailyFlows.rdf") #rdf file with slot you want

variables = c("DailyFlows.FlamingGorgeDaily")
# variables = c("BlueMesaData.TargetDailyOutflowFromBlueMesa_AllDays")
# variables = c("DailyFlows.FlamingGorgeDaily","DailyFlows.JensenDaily")

timesteps <- c("daily") #"annual" or "monthly"

## Plot Variables ##

combineplots <<- F #F for individual files saved, true to combineplots multiple files

figuretypes <- c(1) #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance 

custom_y_labs <- c(NA) #NA gives default "Daily Flow (cfs)", enter if other 

plotyr = 2019 #filter out all years but this year

customcaptions <- c(NA) #NA or this will over write the default captions 
#set for mean (1) and boxplot (2) 

#file names 
figname <<- 'Generic_Fig' 
#if combineplots is T figs saved as figname.pdf w/o captions 
#if F figs saved individually as figname_timestep_cyorwy_variable_figuretype.imgtype

#### End of Normal Change Section ####

#output image parameters 
width <<- 9 #inches
height <<- 6

imgtype <<- "pdf" #supports pdf, png, jpeg. pdf looks the best 
#only works when individual plots are selected 


#### Example of Multi Slot Process 
## Process Variables ##

# rdffiles <- c("Res.rdf","Res.rdf","Res.rdf") #rdf file with slot you want
# variables <- c("Powell.Inflow","Powell.Pool Elevation","Powell.Outflow") #RW Object.Slot
# timesteps <- c("annual","monthly","annual") #"annual" or "monthly"
# floworpes <- c("flow","pe","flow") #"flow" or "pe"
# cyorwys <- c("cy","cy","cy") #"cy" or "wy". wy not tested for all plot configurations
# mainScenGroup <<- names(scens)[1] #name of the subfolder this analysis will be stored
# ## Plot Variables ##
# combineplots <<- T #F for individual files saved, true to combineplots multiple files
# figuretypes <- c(2,1,3) #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance
# exc_months <- c(12,12,12) #1 - Jan, 12 - Dec
# startyr <<- 2019 #filter out all years > this year
# endyr <<- 2026 #filter out all years > this year
# customcaptions <- c(NA,NA,"Test") #NA or this will over write the default caption on boxplots

# the mainScenGroup is the name of the subfolder this analysis will be stored
#under in the results folder 
mainScenGroup <- names(scens)[1]

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source('code/Generic_Scen_Process.r')
source('code/Generic_Daily_Plot.r')

ofigs <- file.path(results_dir,mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


ofigs <<- file.path(results_dir,mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

if(combineplots == T){
  pdf(file.path(ofigs,paste0(figname,".pdf")), width=width, height=height)
}

#this could be used to loop through multiple plots 
for(i in 1:length(variables)){
  ## Process Variables ##
  rdffile <<- rdffiles[i] 
  variable <<- variables[i]
  timestep <<- timesteps[i]
  ## Plot Variables ##
  figuretype <<- figuretypes[i]
  customcaption <<- customcaptions[i]
  custom_y_lab <<- custom_y_labs[i]
  
  ## Process ##
  scen_res <- generic.scen.process(scen_dir,scens,timestep) 

  # ## Plot ##
  # if(timestep == "annual"){
  #   generic.annual.plot(scen_res)
  # } else if(timestep == "monthly"){
    generic.daily.plot(scen_res)
  # } else {
  #   stop(paste0("Plot type ",timestep," not supported"))
  # }
  # 
} #close process and plot loop i 

if(combineplots == T){
  dev.off()
}


      
      