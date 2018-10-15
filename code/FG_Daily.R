#############
#Generic Script to make Multitrace Plot and Exceedance Plot for Flow from rdf
#############

#NOT IN CRSS FOLDER, somewhere for her to put with CRSSDIR reference 

library('tidyverse')
library('RWDataPlyr')
library('scales') #need this for scale_x_date()
source('code/Stat_emp_ExcCrv.r')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    -------------------        USER INPUT        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")
#default structure from RiverSmart study
iFolder <- file.path(CRSSDIR,"Scenario") 
#OR if somewhere else
# iFolder <- "CompleteFilePath" #replace this with path to folder with senarios 

list.dirs(iFolder) #list dirs in set folder folder for us in next input

#scenarios you want to compare 
scens <- list(
  "April 2018" = "Apr2018,DNF,2007Dems,IG,MTOM_Most", 
  "Asp.NoTribs" = "9601.NoTribs,DNF,2007Dems,IG.9003.NoTribs,MTOM_Most",
  "Baseline" = "results_base_LTSP_12182017", 
  "LTSP" = "results_LTSP_12142017"
)

list.files(file.path(iFolder,scens[1])) #list files in scen folder for next input

#rdf file with slot you want 
file = "DailyFlows.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(iFolder,scens[1],file))) #check slots in rdf

variables = c("DailyFlows.FlamingGorgeDaily","DailyFlows.JensenDaily")
# objectslot = "DailyFlows.FlamingGorgeDaily"
# objectslot ="DailyFlows.JensenDaily"                 
# objectslot ="DailyFlows.YampaDailyNatural"          
# objectslot ="DailyFlows.YampaDailyDeerlodgeDepleted"

#plot inputs 
y_lab = "Daily Flow (cfs)"
filteryrlessorequal = 2019 #filter out all years > this year
plotfoldertitle = "FG Dev" #folder to create for output in results dir
plotitle = "FG_Daily_Plots" #objectslot + .pdf will be added when creating plots 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    ----        CODE - MODIFY AT YOUR OWN RISK        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oFigs <- file.path(CRSSDIR,'results', plotfoldertitle) 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}
message('Figures and tables will be saved to: ', oFigs)

j=i=1
for (j in 1:length(variables)) {
  
objectslot = variables[j]
title = objectslot


####CODE####
# rwa1 <- rwd_agg(data.frame(
#   file = file,
#   slot = objectslot, 
#   period = "asis", #Use for Daily or Monthly output, other options "cy","wy","eocy","eowy","djf","July"
#   #see RWDATPlyr Workflow for more information on period, summary, eval, t_s
#   summary = NA,
#   eval = NA,
#   t_s = NA,
#   variable = objectslot, #could assign short variable name but keep full name
#   stringsAsFactors = FALSE
# ))
# 
# #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
# scen_res <- rw_scen_aggregate(
#   scens,
#   agg = rwa1,#rwa2 for new run with daily output 
#   scen_dir = iFolder 
#   # ,keep_cols = T #keep all cols since need date and keep_cols = F will only give Month, Year
# )

for (i in 1:length(scens)) {
  #try getting daily another way
  scen_res_x <- file.path(iFolder,scens[i],file) %>% #this means "pipe" the data to the next function 
    rdf_to_rwtbl2() %>%
    filter(ObjectSlot == objectslot)
  
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


scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#    ----        PLOTS - MODIFY AT YOUR OWN RISK        ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## plot 
pdf(file.path(oFigs,paste0(plotitle,objectslot,"Verify.pdf")), width=9, height=6)

#    -------------------        All Trace Mean        ----------------------
scen_res %>%
  # dplyr::filter(ObjectSlot == objectslot) %>% #don't need to do this since only one var
  dplyr::filter(Year <= filteryrlessorequal) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(Timestep, Value, color = Scenario)) + 
  geom_point() +
  geom_line() +
  labs(title = paste("Mean",title,filteryrlessorequal), y = y_lab) +
  scale_x_date(breaks = date_breaks("months"),
             labels = date_format("%b"))

#    -------------------        All Trace Boxplot        ----------------------

scen_res %>%
  # dplyr::filter(Variable == objectslot) %>%
  dplyr::filter(Year <= filteryrlessorequal) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  dplyr::group_by(Scenario, MonthNum) %>% #don't need to do this since only one var
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  # ggplot(aes(x = Timestep, y = Value, color = Scenario)) + #can't use this since must factor to have sperate boxes
  geom_boxplot() +
  # stat_boxplot_custom() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = paste(title,filteryrlessorequal), y = y_lab) 

#    -------------------        Percent Exceedance of Traces       ----------------------

scen_res %>%
  # dplyr::filter(Variable == objectslot) %>%
  dplyr::filter(Year <= filteryrlessorequal) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
  ggplot(aes(Value, color = Scenario)) + 
  stat_eexccrv() + 
  labs(title = paste(filteryrlessorequal,title,"Trace Exceedance"), y = y_lab, x = "Exceedance") +
  scale_x_continuous(labels = percent)

dev.off()

} #close j variable loop 


