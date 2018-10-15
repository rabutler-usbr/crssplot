##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two CRSS runs
##############################################################################

#plot inputs 
startyr = 2019 #filter out all years < this year
endyr = 2026 #filter out all years > this year


library(RWDataPlyr)
if(packageVersion("RWDataPlyr") < "0.6.1"){
  # need 0.6.0 or higher for makeAllScenNames
  detach("package:RWDataPlyr")
  devtools::install_github("BoulderCodeHub/RWDataPlyr")
  library(RWDataPlyr)
}
suppressPackageStartupMessages(library(data.table))

# ** make sure CRSS_DIR is set correctly before running

CRSSDIR <- Sys.getenv("CRSS_DIR")
# iFolder <- "M:/Shared/CRSS/2018/Scenario"
iFolder <- paste0(CRSSDIR,"/Scenario/")
# set crssMonth to the month CRSS was run. data and figures will be saved in 
# a folder with this name

scens <- list(
  "April 2018" = "Apr2018,DNF,2007Dems,IG,MTOM_Most", 
  "Asp.NoTribs" = "9601.NoTribs,DNF,2007Dems,IG.9003.NoTribs,MTOM_Most",
  "Baseline" = "results_base_LTSP_12182017", 
  "LTSP" = "results_LTSP_12142017"
)

# ofigs <- file.path(CRSSDIR,'results',mainScenGroup) 
ofigs <- "C:/Users/cfelletter/Documents/CRSS working/2018UBRedesign"
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}


message('Figures and tables will be saved to: ', ofigs)



# library('devtools')
# devtools::install_github('BoulderCodeHub/RWDataPlyr', build_vignettes = TRUE)
library('RWDataPlyr')
library('scales')
library('tidyverse')


# # #see https://github.com/BoulderCodeHub/RWDataPlyr
# # #look at sample work flow
# vignette("rwdataplyr-workflow", package = "RWDataPlyr")

rwa1 <- rwd_agg(read.csv("C:/Users/cfelletter/Documents/CRSS working/RDF Process/rw_agg_UBdev_monthly_noGun.csv", stringsAsFactors = FALSE)) 

# scen_dir = paste0(MTOMDIR,"/Output Data/RDF Process/") #set to the folder containing the sub folders for each ensemble
# names(my_scens) = my_scens #naming #must name these

# file = "UBRes.rdf" #"MTOM.rdf"
# #read in the rdf
# rdf <- read_rdf(iFile = paste0(iFolder,scens[1],"/",file))
# # ensure the slot you want is in the rdf:
# rdf_slot_names(rdf)


#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = iFolder
)

unique(scen_res$Variable) #check variable names 
unique(scen_res$Scenario) #check Scenario names 
unique(scen_res$Year) #check Years names 


#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))

## plot 
pdf(paste0(ofigs,'/UBresDevPlots_Monthly_HP_Verify.pdf'), width=9, height=6)
# for(i in 1:1){

variable = "Fontenelle.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 


variable = "Fontenelle.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
Apr1PEtarget <- data.frame(yintercept=6468) 
Jul1PEtarget <- data.frame(yintercept=6500) 
Aug1PEtarget <- data.frame(yintercept=6505.5) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=Apr1PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Jul1PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Aug1PEtarget) +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 


variable = "FlamingGorge.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
MaxPEtarget <- data.frame(yintercept=6039) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=MaxPEtarget) +
  labs(title = title, y = y_lab) 


# }
dev.off()
