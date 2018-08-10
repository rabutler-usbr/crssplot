##############################################################################
#This script creates daily FG plots for LTSP

#DEVELOPMENT IS ON GOING ON THIS

#Contents 
## 1. Set Up ##
## 2. User Input ##
## 3. Process Results ## 
## 4. Plot ## 

#   Created by C. Felletter 8/2018
##############################################################################

library('tidyverse')
library('RWDataPlyr')
library('scales') #need this for scale_x_date()
source('code/Stat_emp_ExcCrv.r')

#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")
#default structure from RiverSmart study
scen_dir <- file.path(CRSSDIR,"Scenario") 
#OR if somewhere else
# scen_dir <- "CompleteFilePath" #replace this with path to folder with senarios 

list.dirs(scen_dir) #list dirs in set folder folder for us in next input

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#scenarios you want to compare 
scenarios <- list( #Short Name for Run = File Name with RDFs
  "Baseline" = "results_base_LTSP_12182017", 
  "LTSP" = "results_LTSP_12142017",
  "LTSP_SMB" = "results_LTSP_SMB_12192017", 
  "LTSP_SMB_BF" = "results_LTSP_SMB_BF_12192017"
)

list.files(file.path(scen_dir,scenarios[1])) #list files in scen folder for next input

#rdf file with slot you want 
file = "DailyFlows.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scenarios[1],file))) #check slots in rdf

variables = c("DailyFlows.FlamingGorgeDaily","DailyFlows.JensenDaily","DailyFlows.YampaDailyDeerlodgeDepleted")

#plot inputs 
y_lab = "Daily Flow (cfs)"
filteryr = 2019 #filter out all years > this year

#file names 
figs <- 'FG_LTSP_Graphs' #objectslot + .pdf will be added when creating plots

# the mainScenGroup is the name of the subfolder this analysis will be stored
#under in the results folder 
mainScenGroup <- names(scenarios)[1]

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT TILL PLOTS (Sect. 4)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    --------------        MODIFY AT YOUR OWN RISK        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

j=i=1

pdf(file.path(oFigs,paste0(plotitle,".pdf")), width=9, height=6)

for (j in 1:length(variables)) {
  objectslot = variables[j]
  
  message(paste("Getting",objectslot))

  for (i in 1:length(scenarios)) {
    message(paste("Reading in",scenarios[i]))
    #try getting daily another way
    scen_res_x <- file.path(scen_dir,scenarios[i],file) %>% #this means "pipe" the data to the next function 
      rdf_to_rwtbl2() %>%
      filter(ObjectSlot == objectslot)
    
    # #filter out Most,Min,Max only would do this for MTOM 
    # filter(TraceNumber >= first_ensemble[1]) 
    
    #add on Scenario since rdf_to_rwtbl2 doesn't add it  
    scen_res_x <- cbind.data.frame(
      scen_res_x,
      Scenario = rep(names(scenarios)[i], Times = length(scen_res_x$Timestep))
    )
    
    #convert Timestep chr to POSIXct
    scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M")) 
    scen_res_x$Timestep <- as.Date(scen_res_x$Timestep)
    #first entry is 2019-1-31 24:00 which gets converted to 2019-02-01, is that okay????? 
    
    if(i == 1 && j == 1){
      scen_res = scen_res_x
    } else {
      scen_res = rbind.data.frame(scen_res,scen_res_x)
    }
  } #close i Scenario loop 
  
} #close j variable loop 
  

unique(scen_res$ObjectSlot) #check variable names 
unique(scen_res$Scenario) #check Scenario names 

scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Choosen Figure Type 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## plot 
pdf(file.path(ofigs,paste0(figs,".pdf")), width=9, height=6)


#    -------------------        PLOT TYPE 1        ----------------------
#plot all variable for one scenario 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## PLOT TYPE 1 User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
filteryr = 2035 #currently setup only to plot one year 
scen = scenarios[1] #change this to number 1-4, type 'scenarios' to get list
target <- data.frame(yintercept=18600) 
title = paste0("FG Releases ",scen,"\n(Model Year ",filteryr,")")
y_lab_1 = "Release/Flow (cfs)"  
y_lab_2 = "Release/Flow (cms)"
x_lab = "" #if you want to add an x label
scale1to2 = 0.028316846592 #cfs to cms
ylims = c(0, 25000)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## END PLOT TYPE 1 User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scen_res %>%
  dplyr::filter(Year == filteryr) %>% #filter year 
  dplyr::filter(Scenario == scen) %>% #filter scenario 
  dplyr::group_by(ObjectSlot, Timestep) %>% #don't need to do this since only one var
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Timestep, Value, color = ObjectSlot)) + 
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=yintercept), data=target) +
  scale_y_continuous(
    name = y_lab_1, 
    sec.axis = sec_axis(~ . * scale1to2 , name = y_lab_2), 
    limits = ylims
  ) + 
  labs(title = title) +
  scale_x_date(x_lab,
               breaks = date_breaks("months"),
               labels = date_format("%b"))


# #    -------------------        PLOT TYPE 2        ----------------------
# #plot all scenarios for one variable (not something you asked for)
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## PLOT TYPE 2 User Input ##
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# filteryr = 2035 #currently setup only to plot one year 
# variable = variables[2]
# target <- data.frame(yintercept=18600) 
# title = paste0(variable,"\n(Model Year ",filteryr,")")
# y_lab_1 = "Release/Flow (cfs)"
# y_lab_2 = "Release/Flow (cms)"
# x_lab = "" #if you want to add an x label
# scale1to2 = 0.028316846592 #cfs to cms
# ylims = c(0, 25000)
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## END PLOT TYPE 2 User Input ##
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# scen_res %>%
#   dplyr::filter(Year == filteryr) %>% #filter year
#   dplyr::filter(ObjectSlot == variable) %>% #filter variable
#   dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
#   dplyr::summarise(Value = mean(Value)) %>%
#   ggplot(aes(Timestep, Value, color = Scenario)) +
#   geom_point() +
#   geom_line() +
#   geom_hline(aes(yintercept=yintercept), data=target) +
#   scale_y_continuous(
#     name = y_lab_1,
#     sec.axis = sec_axis(~ . * scale1to2 , name = y_lab_2),
#     limits = ylims
#     ) +
#   labs(title = title) +
#   scale_x_date(x_lab,
#              breaks = date_breaks("months"),
#              labels = date_format("%b"))



#    -------------------        PLOT TYPE 3        ----------------------
#    -------------------        Percent Exceedance of Traces       ----------------------

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## PLOT TYPE 3 User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# filteryr = 2035 #currently looking at all years  
variable = variables[1] #"DailyFlows.FlamingGorgeDaily"
target <- data.frame(yintercept=18600) 
title = paste0(variable," (Apr-Jul)")
y_lab_1 = "Release (cfs)"
y_lab_2 = "Release (cms)"
x_lab = "Percent Exceedance" #if you want to add an x label
scale1to2 = 0.028316846592 #cfs to cms
ylims = c(0, 5000)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## END PLOT TYPE 2 User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scen_res %>%
  # dplyr::filter(Year == filteryr) %>% #filter year
  dplyr::filter(Month == "April" | Month == "May" | Month == "June" | Month == "July") %>% #filter year
  dplyr::filter(ObjectSlot == variable) %>% #filter variable
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Value, color = Scenario)) + 
  stat_eexccrv() + 
  scale_y_continuous(
    name = y_lab_1,
    sec.axis = sec_axis(~ . * scale1to2 , name = y_lab_2),
    limits = ylims
  ) +
  labs(title = title) +
  scale_x_continuous(labels = percent)

# #    -------------------        All Trace Boxplot        ----------------------
# 
# scen_res %>%
#   dplyr::filter(Year <= filteryr) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
#   dplyr::group_by(Scenario, MonthNum) %>% #don't need to do this since only one var
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
#   geom_boxplot() +
#   # stat_boxplot_custom() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   labs(title = paste(title,filteryr), y = y_lab) 

dev.off()

message('Figures saved to: ', ofigs)
