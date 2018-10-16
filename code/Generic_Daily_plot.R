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
#   Updated by CF on 10/2018 to include logic for adapting for development of
#   multiple plot types in one pdf
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

#variables, figuretypes & customcaptions should be set to a single value
#but could be used to loop through multiple plots if additional loops were added

#rdf file with slot you want, daily output only in this one file 
file = "DailyFlows.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],file))) #check slots in rdf

variables = c("DailyFlows.FlamingGorgeDaily")
# variables = c("BlueMesaData.TargetDailyOutflowFromBlueMesa_AllDays")
# variables = c("DailyFlows.FlamingGorgeDaily","DailyFlows.JensenDaily")

figuretypes <- 2 #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance 

customcaptions <- NA #NA or this will over write the default caption on boxplots

#plot inputs 
y_lab = "Daily Flow (cfs)"

plotyr = 2019 #filter out all years > this year
# plotfoldertitle = "FG Dev" #folder to create for output in results dir 

#file names 
figs <- 'Generic_MonthlyFig' #variable + .pdf will be added when creating plots
#must change to custom name if using multiple plots 

#### End of Normal Change Section ####

#output image parameters 
width <- 9 #inches
height <- 6
imgtype <- "pdf" #supports pdf, png, jpeg. pdf looks the best 

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
library('scales') #need this for scale_x_date()
#see RWDATPlyr Workflow for more information 
# library(CRSSIO)
# plotEOCYElev() and csVarNames()
source('code/Stat_emp_ExcCrv.r')
source('code/stat-boxplot-custom.r')

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

figurenames <- c("Mean","Bxplt","Exceedance")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

j=1
variable = variables[j]
title = variable

#Special rw scen agg since RWDataPlyr doesn't support daily 
for (i in 1:length(scens)) {

  scen_res_x <- file.path(scen_dir,scens[i],file) %>% #this means "pipe" the data to the next function 
    rdf_to_rwtbl2() %>%
    filter(ObjectSlot == variable)
  
  # #filter out Most,Min,Max only would do this for MTOM 
  # filter(TraceNumber >= first_ensemble[1]) 
  
  #add on Scenario since rdf_to_rwtbl2 doesn't add it  
  scen_res_x <- cbind.data.frame(
    scen_res_x,
    Scenario = rep(names(scens)[i], Times = length(scen_res_x$Timestep))
  )
  
  #convert Timestep chr to POSIXct
  scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M")) 
  scen_res_x$Timestep <- as.Date(scen_res_x$Timestep)
  #first entry is 2019-1-31 24:00 which gets converted to 2019-02-01, is that okay????? 
  
  if(i == 1){
    scen_res = scen_res_x
  } else {
    scen_res = rbind.data.frame(scen_res,scen_res_x)
  }
} #close i Scenario loop 

unique(scen_res$ObjectSlot) #check variable names 
unique(scen_res$Scenario) #check Scenario names 
# unique(scen_res$TraceNumber) #check trace numbers 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))
scen_res$DayNum = as.numeric(format.Date(scen_res$Timestep, format = "%d"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Choosen Figure Type 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
# pdf(file.path(ofigs,paste0(plotitle,variable,".pdf")), width=9, height=6)
#enable the above if using loops (advanced) and want one pdf
#must disable ggsave at bottom and captions won't work w/o ggsave

#this could be used to loop through multiple plots 
figuretype <- figuretypes
customcaption <- customcaptions

#figure captions
if (is.na(customcaption) &  figuretype == 1){
  caption <- "Note: Month axis label is the previous EoM CRSS timestep, e.g. Feb label is Jan 31."
} else if (is.na(customcaption) &  figuretype == 2){
  caption <- "Note: The boxplots show the distribution of daily flow grouped by month for all traces. The boxes correspond to the 25th and 75th quantiles,\nthe whiskers enclose the 10th to 90th quantiles,with points representing data that falls outside this range."
} else if (is.na(customcaption)){
  caption <- "" #no caption 
} else {
  caption <- customcaption #user supplied 
}

message(paste("Creating ",variable, figurenames[figuretype])) 

#    -------------------        All Trace Mean        ----------------------

if (figuretype == 1){
  p <- scen_res %>%
  dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Timestep, Value, color = Scenario)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma) + #add commas to axis 
  labs(title = paste("Mean",variable,plotyr), y = y_lab,caption = caption) +
  scale_x_date("Month", breaks = date_breaks("months"),
             labels = date_format("%b")) +
  theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
}

message("The months appear to start in Feb since first CRSS timestep is Jan 31")

#    -------------------        All Trace Boxplot        ----------------------
if (figuretype == 2){
  p <- scen_res %>%
  dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  dplyr::group_by(Scenario, MonthNum) %>% #don't need to do this since only one var
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  # geom_boxplot() +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) + 
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = paste(title,plotyr), y = y_lab) +  
  labs(caption = caption) +
  theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
}
  
#    -------------------        Percent Exceedance of Traces       ----------------------
if (figuretype == 3){
  p <- scen_res %>%
  dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  ggplot(aes(Value, color = Scenario)) + 
  stat_eexccrv() + 
  labs(title = paste(plotyr,title,"Trace Exceedance"), y = y_lab, x = "Exceedance") +
  scale_x_continuous(labels = percent) +
  labs(caption = caption) +
  theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
}

## save off image 
ggsave(filename = paste0(file.path(ofigs,figs),"_",variable,"_",figurenames[figuretype],".",imgtype), width = width, height = height, units ="in")

dev.off()

# } #close j variable loop 


