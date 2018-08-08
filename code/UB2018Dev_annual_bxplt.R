###############################################################################
#Breaking out this script from UB2018Dev_main_&_annual_bxplt.R
#This script creates annual boxplots of Outflow and PE to compare two CRSS runs
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
  "April 2018" = "Apr2018,DNF,2007Dems,IG,MTOM_Most", 
  "Asp.NoTribs" = "9601.NoTribs,DNF,2007Dems,IG.9003.NoTribs,MTOM_Most"
)

oFigs <- "C:/Users/cfelletter/Documents/CRSS working/Aspinall"
message('Figures and tables will be saved to: ', oFigs)



# library('devtools')
# devtools::install_github('BoulderCodeHub/RWDataPlyr', build_vignettes = TRUE)
library('RWDataPlyr')
library('scales')
library('tidyverse')

rwa1 <- rwd_agg(read.csv("C:/Users/cfelletter/Documents/CRSS working/RDF Process/rw_agg_UBdev.csv", stringsAsFactors = FALSE)) 

# scen_dir = paste0(MTOMDIR,"/Output Data/RDF Process/") #set to the folder containing the sub folders for each ensemble
# names(my_scens) = my_scens #naming #must name these

# file = "UBRes.rdf" #"MTOM.rdf"
# #read in the rdf
# rdf <- read_rdf(iFile = paste0(iFolder,scens[1],"/",file))
# # ensure the slot you want is in the rdf:
# rdf_slot_names(rdf)

for(i in 1:1){
#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = iFolder
)

unique(scen_res$Variable) #check variable names 

## plot 
pdf(paste0(oFigs,'/UBresDevPlots.pdf'), width=9, height=6)

variable = "Powell.Inflow.CY"
y_lab = "Annual Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Powell.Pool Elevation"
y_lab = "EOCY PE"
title = paste0("EOCY",variable)

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Powell.Inflow.May"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Crystal.Outflow.CY"
y_lab = "Annual Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Crystal.Pool Elevation"
y_lab = "EOCY PE"
title = paste0("EOCY",variable)

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow.CY"
y_lab = "Annual Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Pool Elevation"
y_lab = "EOCY PE"
title = paste0("EOCY",variable)

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow.May"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow.May"
y_lab = "Mean Monthly Flow"
title = variable
DryTarget <- data.frame(yintercept=55340) 
AvgDryTarget <- data.frame(yintercept=496200)
ModWetTarget <- data.frame(yintercept=882350) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  geom_hline(aes(yintercept=yintercept), data=DryTarget) +
  geom_hline(aes(yintercept=yintercept), data=AvgDryTarget) +
  geom_hline(aes(yintercept=yintercept), data=ModWetTarget) +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow.CY"
y_lab = "Mean Annual Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

# # Not currently output part of the rdf output 
# variable = "GunnisonNearGrandJunction.CFSInflow.May"
# y_lab = "Mean Monthly Flow"
# title = variable
# 
# scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value)) %>%
#   ggplot(aes(Year, Value, color = Scenario)) + 
#   geom_line() +
#   labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow.CY"
y_lab = "Mean Annual Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Pool Elevation"
y_lab = "Mean EOCY PE"
title = paste0("EOCY",variable)

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow.May"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Fontenelle.Outflow.CY"
y_lab = "Annual Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Fontenelle.Pool Elevation"
y_lab = "EOCY PE"
title = paste0("EOCY",variable)

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "Fontenelle.Pool Elevation.March"
y_lab = "Mean Monthly PE"
title = paste0("Mean",variable)
PEtarget <- data.frame(yintercept=6468) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  geom_hline(aes(yintercept=yintercept), data=PEtarget) +
  labs(title = title, y = y_lab) 

variable = "Fontenelle.Pool Elevation.June"
y_lab = "Mean Monthly PE"
title = paste0("Mean",variable)
PEtarget <- data.frame(yintercept=6500) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  geom_hline(aes(yintercept=yintercept), data=PEtarget) +
  labs(title = title, y = y_lab) 

variable = "Fontenelle.Pool Elevation.July"
y_lab = "Mean Monthly PE"
title = paste0("Mean",variable)
PEtarget <- data.frame(yintercept=6505.5) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  geom_hline(aes(yintercept=yintercept), data=PEtarget) +
  labs(title = title, y = y_lab) 

dev.off()
}

#monthly plots 
for(i in 1:1){
  
## plot 
pdf(paste0(oFigs,'/UBres_Monthly_DevPlots.pdf'), width=9, height=6)

#FlamingGorge
variable = "FlamingGorge.Outflow.March"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow.April"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow.May"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow.June"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow.July"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

#BlueMesa
variable = "BlueMesa.Outflow.March"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow.April"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow.May"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow.June"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow.July"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

#Gun
variable = "GunnisonNearGrandJunction.AFOutflow.March"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow.April"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow.May"
y_lab = "Mean Monthly Flow"
title = variable
DryTarget <- data.frame(yintercept=55340) 
AvgDryTarget <- data.frame(yintercept=496200)
ModWetTarget <- data.frame(yintercept=882350) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  geom_hline(aes(yintercept=yintercept), data=DryTarget) +
  geom_hline(aes(yintercept=yintercept), data=AvgDryTarget) +
  geom_hline(aes(yintercept=yintercept), data=ModWetTarget) +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow.June"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow.July"
y_lab = "Mean Monthly Flow"
title = variable

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + 
  geom_boxplot() +
  # stat_boxplot_custom() +
  labs(title = title, y = y_lab) 

dev.off()
}
