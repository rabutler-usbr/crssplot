
# creates a variable (aggregation number) based on the scenario name. 
# uses the initial conditions (I.C.) to aggregate differently
# I.C. that come from the ensemble forecasts via MTOM are tagged 1
# I.C. that come from MTOM using the min/most/max probable forecast use 2
# I.C. that come from 24-MS using the min/most/max probable forecast use 3
# I.C. are assumed to be the 5 dimension in the scenario names

aggBasedOnIC <- function(scen)
{
  scen <- simplify2array(strsplit(as.character(scen),',',fixed = T))[5]
  
  if(scen %in% 1981:2010){
    zz <- 1
  } else if(scen %in% c('MTOM_Max','MTOM_Most','MTOM_Min')){
    zz <- 2
  } else if(scen %in% c('Min','Most','Max')){
    zz <- 3
  } else{
    zz <- -99
  }
  
  zz
}
