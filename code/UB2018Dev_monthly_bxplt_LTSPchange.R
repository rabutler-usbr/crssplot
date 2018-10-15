##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two CRSS runs
##############################################################################


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
  "Aug 2018" = "Aug2018_2019,DNF,2007Dems,IG,Most",
  "Aug 2018 + Fix" = "Aug2018_2019_9000,DNF,2007Dems,IG_9000,Most_BM_FGltsp"
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

rwa1 <- rwd_agg(read.csv("C:/Users/cfelletter/Documents/CRSS working/RDF Process/rw_agg_UBdev_monthly.csv", stringsAsFactors = FALSE)) 

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




minvals <- cbind.data.frame(
  #check min BM outflow from offc
  scen_res %>%
    dplyr::filter(Variable == "FlamingGorge.Outflow") %>%
    dplyr::filter(Scenario == names(scens[1]))  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)),
  
  #check min BM outflow from fix
  scen_res %>%
    dplyr::filter(Variable == "FlamingGorge.Outflow") %>%
    dplyr::filter(Scenario == names(scens[2]))  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)),

  scen_res %>%
    dplyr::filter(Variable == "Powell.Inflow") %>%
    dplyr::filter(Scenario == names(scens[1]))  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)),
  
  #check min BM outflow from fix
  scen_res %>%
    dplyr::filter(Variable == "Powell.Inflow") %>%
    dplyr::filter(Scenario == names(scens[2]))  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)))

print(minvals[,c(1,2,4,6,8)])

write.csv(minvals[,c(1,2,4,6,8)],file = paste0(ofigs,'/FGltsp_mins.csv'))


#plot the below 
## plot 
pdf(paste0(ofigs,'/UBresDevPlots_Monthly_FGltsp.pdf'), width=9, height=6)
# for(i in 1:1){


variable = "FlamingGorge.Outflow"
y_lab = "Monthly Flow (acre-ft/month)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
# # Jan31PEtarget <- data.frame(yintercept=7487) 
# # Feb31PEtarget <- data.frame(yintercept=7485) 
# Mar31PEtarget <- data.frame(yintercept=7484.5) 
# # Apr31PEtarget <- data.frame(yintercept=7491) 
# # MayNov31PEtarget <- data.frame(yintercept=7495) 
# JunJul31PEtarget <- data.frame(yintercept=7516.4) 
# # Aug31PEtarget <- data.frame(yintercept=7507) 
# # Sep31PEtarget <- data.frame(yintercept=7498) 
# # Oct31PEtarget <- data.frame(yintercept=7496.5) 
# # Dec31PEtarget <- data.frame(yintercept=7490) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  # # geom_hline(aes(yintercept=yintercept), data=Jan31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=Feb31PEtarget) +
  # geom_hline(aes(yintercept=yintercept), data=Mar31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=Apr31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=MayNov31PEtarget) +
  # geom_hline(aes(yintercept=yintercept), data=JunJul31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=Aug31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=Sep31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=Oct31PEtarget) +
  # # geom_hline(aes(yintercept=yintercept), data=Dec31PEtarget) +
  labs(title = title, y = y_lab) 


variable = "Powell.Inflow"
y_lab = "Monthly Flow (acre-ft/month)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 

variable = "Powell.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 

variable = "Powell.Outflow"
y_lab = "Monthly Flow (acre-ft/month)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 



dev.off()
