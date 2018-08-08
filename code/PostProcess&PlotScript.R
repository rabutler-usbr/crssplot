#NOT IN CRSS FOLDER, somewhere for her to put with CRSSDIR reference 

#find the scenarios 
# iFolder <- "M:/Shared/CRSS/2018/Scenario" #set to custom location

# set crssMonth to the month CRSS was run. data and figures will be saved in 
# a folder with this name

library('tidyverse')
library('RWDataPlyr')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    -------------------        USER INPUT        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")
#default structure from RiverSmart study
iFolder <- paste0(CRSSDIR,"/Scenario/") 
#OR if somewhere else
iFolder <- "CompleteFilePath" #replace this with path to folder with senarios 

list.dirs(iFolder) #list dirs in set folder folder for us in next input

#scenarios you want to compare 
scens <- list( #Short Name for Run = File Name with RDFs
  "April 2018" = "Apr2018,DNF,2007Dems,IG,MTOM_Most", 
  "Asp.NoTribs" = "9601.NoTribs,DNF,2007Dems,IG.9003.NoTribs,MTOM_Most"
)

list.files(paste0(iFolder,scens[1])) #list files in scen folder for next input

#rdf file with slot you want 
file = "DailyFlows.rdf" 

rdf_slot_names(read_rdf(iFile = paste0(iFolder,scens[1],"/",file))) #check slots in rdf

objectslot = "DailyFlows.FlamingGorgeDaily"
# objectslot ="DailyFlows.JensenDaily"                 
# objectslot ="DailyFlows.YampaDailyNatural"          
# objectslot ="DailyFlows.YampaDailyDeerlodgeDepleted"




#plot type 




#plot inputs 
y_lab = "Daily Flow"
title = objectslot
filteryrlessorequal = 2019

#plot type 



# scen_dir = paste0(MTOMDIR,"/Output Data/RDF Process/") #set to the folder containing the sub folders for each ensemble
# names(my_scens) = my_scens #naming #must name these

# file = "UBRes.rdf" #"MTOM.rdf"
# #read in the rdf
# rdf <- read_rdf(iFile = paste0(iFolder,scens[1],"/",file))
# # ensure the slot you want is in the rdf:
# rdf_slot_names(rdf)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    -------------------        CODE - MODIFY AT YOUR OWN RISK        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#############
#Generic Script to make Multitrace Plot and Exceedance Plot for Flow from rdf
#############

# getwd() #already set wd based on project 




source('Stat_emp_ExcCrv.r')

####CODE####
rwa1 <- rwd_agg(data.frame(
  file = file,
  slot = objectslot, 
  period = "asis", #Use for Daily or Monthly output, other options "cy","wy","eocy","eowy","djf","July"
  #see RWDATPlyr Workflow for more information on period, summary, eval, t_s
  summary = NA,
  eval = NA,
  t_s = NA,
  variable = objectslot, #could assign short variable name but keep full name
  stringsAsFactors = FALSE
))

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,#rwa2 for new run with daily output 
  scen_dir = iFolder 
  # ,keep_cols = T #keep all cols since need date and keep_cols = F will only give Month, Year
)

#try getting daily another way
i=1 # make this into a loop 
rdf <- read_rdf(iFile = paste0(iFolder,"/",scens[i],"/",file))

head(
  rdf %>% #this means "pipe" the data to the next function 
  rdf_to_rwtbl2() %>%
  filter(ObjectSlot == objectslot) #%>% 
  # #filter out Most,Min,Max
  # filter(TraceNumber >= first_ensemble[1]) 
  
  #########RESUME HERE IS working on Timestep is char with what i need in form of 2019-1-31 24:00 ---- convert to date
  
  ) #%>% 

unique(scen_res$Variable) #check variable names 

## plot 
pdf(paste0(oFigs,'/UBresDevPlots.pdf'), width=9, height=6)


#    -------------------        All Trace Mean        ----------------------

head(
  scen_res %>%
    dplyr::filter(Variable == objectslot) 
)

#only for yearly data 

scen_res %>%
  dplyr::filter(Variable == objectslot) %>%
  dplyr::filter(Year <= filteryrlessorequal) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, color = Scenario)) + 
  geom_line() +
  labs(title = title, y = y_lab) 

#    -------------------        All Trace Boxplot        ----------------------



scen_res %>%
  dplyr::filter(Variable == objectslot) %>%
  dplyr::filter(Year <= filteryrlessorequal) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

#    -------------------        Percent Exceedance of Traces       ----------------------

objectslot = "FlamingGorgeDaily"
# objectslot = "JensenDaily"
# objectslot = "DailyBlackCanyon"
# objectslot = "DailyWhitewater"
y_lab = "Daily Flow"
title = objectslot
filteryrlessorequal = 2026

scen_res %>%
  dplyr::filter(Variable == objectslot) %>%
  dplyr::filter(Year <= filteryrlessorequal) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  stat_eexccrv() + 
  labs(title = title, y = y_lab) 


dev.off()
}
