source('code/makeScenNames.R')
source('code/getSysCondData.R')
source('code/dataTaggingFunctions.R')

# script should create everything necessary for the results in order

# scenarios are orderd model,supply,demand,policy,initial conditions
scens <- makeAllScenNames('May2015_2016','DNF','2007Dems','IG',c(1981:2010,'MTOM_Most','Most'))

getSysCondData(scens, 'M:/Shared/CRSS/CRSS.2015/Scenario', 'C:/alan/CRSS/CRSS.2015/results/MaySysCond.txt', 
               addAggAttribute = TRUE, aggBasedOnIC)

rm(list = ls())

