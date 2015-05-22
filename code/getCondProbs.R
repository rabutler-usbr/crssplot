# get conditional probabilities

library(dplyr)
library(reshape2)

# res - results data frame from systemConditions.txt
# Prob( yr1 cond1 | yr2 cond2) 
# ex:
# Prob(2016 Shortage | WY 2016 Release = 7.48) =
#   getConditionalProbs(res, 2016, 2016, 'lbShortage','mer748')
# P(2017 Short (by tier) | WY 2016 Rel = 7.48) = 
#   getConditionalProbs(res, 2017, 2016, c('lbShortage','lbShortageStep1','lbshort2','lbshort3),
#                       'mer748')
getConditionalProbs <- function(res, yr1, yr2, cond1, cond2)
{
  if(yr1 == yr2){
    zz <- dplyr::filter(res, Year == yr1, Variable %in% c(cond1, cond2))
  } else{
    zz <- rbind(dplyr::filter(res, Year == yr1, Variable %in% cond1),
                dplyr::filter(res, Year == yr2, Variable %in% cond2))
  }

  zz <- zz %>% select(Value, Variable, Trace, Scenario) %>%
    mutate(lName = paste(Scenario,Trace,sep = '_')) %>% # create unique run, trace combination
    select(Value,Variable,lName)
  zz <- reshape2::dcast(zz, lName ~ Variable, value.var = 'Value')

  # can this be replaced using dplyr::filter?
  rowKeep <- rep(F,nrow(zz))
  for(i in 1:length(cond2)){
    rowKeep <- rowKeep | zz[[cond2[i]]] == 1
  }
  #zz <- zz[zz[[cond2]] == 1,]
  zz <- zz[rowKeep,]
  retVal <- c()
  for(i in 1:length(cond1)){
    retVal[cond1[i]] <- mean(zz[[cond1[i]]])
  }
  retVal
}
