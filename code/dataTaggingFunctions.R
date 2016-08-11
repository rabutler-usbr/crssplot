
# creates a variable (aggregation number) based on the scenario name. 
# uses the initial conditions (I.C.) to aggregate differently
# I.C. that come from the ensemble forecasts via MTOM are tagged 1
# I.C. that come from MTOM using the min/most/max probable forecast use 2
# I.C. that come from 24-MS using the min/most/max probable forecast use 3
# I.C. are assumed to be the 5 dimension in the scenario names
# assumes it is getting a vector of multiple scenarios.

aggBasedOnIC <- function(scen)
{
  scen <- simplify2array(strsplit(as.character(scen),',',fixed = T))[5,]

  zz <- rep(-99,length(scen))
  zz[scen %in% 1981:2010] <- 1
  zz[scen %in% c('MTOM_Max','MTOM_Most','MTOM_Min')] <- 2
  zz[scen %in% c('Min','Most','Max')] <- 3
  
  zz
}

# assumes adding attribute based on something in scenario names
# if not found in attMap, will default to attribute name.
# scenI: the index into the scenario name to use
# attMap: name mapping vector to connect attribute info to new value
addAttByScenName <- function(scen, scenI, attMap)
{
  scen <- simplify2array(strsplit(as.character(scen),',',fixed = T))[scenI,]
  
  zz <- scen
  for(i in 1:length(attMap)){
    zz[zz==names(attMap)[i]] <- attMap[i]
  }
  
  zz
}

# use this for april which has different number of dimensions
aggBasedOnIC_april <- function(scen)
{
  scen <- simplify2array(strsplit(as.character(scen),',',fixed = T))[4,]
  
  zz <- rep(-99,length(scen))
  zz[scen %in% 1981:2010] <- 1
  zz[scen %in% c('MTOM_Max','MTOM_Most','MTOM_Min')] <- 2
  zz[scen %in% c('Min','Most','Max')] <- 3
  
  zz
}

# Scenario is assumed to be the full scenario name, and scens is a list of 
# scenarios that should be combined together
aggFromScenList <- function(Scenario, scens)
{
  zz <- rep(NA, length(Scenario))
  
  for(i in 1:length(scens)){
    zz[Scenario %in% scens[[i]]] <- names(scens)[i]
  }
  
  zz
}
