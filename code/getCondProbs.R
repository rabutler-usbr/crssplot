# get conditional probabilities

library(dplyr)

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
  stop("need to convert dcast to tidyr::spread")
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

get_all_cond_probs <- function(sysCond, scenario, yrs2show, ofile)
{
  warning(
    "The conditional probabilities have not been computed for a long time.\n", 
    "Please carefully review the code and results."
  )
  
  sysCond <- dplyr::filter(sysCond, Year %in% yrs2show & Agg == scenario)
  sysTable <- CRSSIO::createSysCondTable(sysCond, yrs2show)
  
  cp1 <- getConditionalProbs(
    sysCond, 
    yrs2show[1], 
    yrs2show[1], 
    'lbShortage',
    'mer748'
  )
  cp2 <- getConditionalProbs(
    sysCond, 
    yrs2show[1], 
    yrs2show[1], 
    'lbShortage',
    'ueb823'
  )
  cp3 <- getConditionalProbs(
    sysCond, 
    yrs2show[1],
    yrs2show[1], 
    'lbShortage',
    c('eq','uebGt823')
  )
  cp4 <- getConditionalProbs(
    sysCond, 
    yrs2show[2], 
    yrs2show[1], 
    c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
    'mer748'
  )
  cp5 <- getConditionalProbs(
    sysCond, 
    yrs2show[2], 
    yrs2show[1], 
    c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
    'ueb823'
  )
  cp6 <- getConditionalProbs(
    sysCond, 
    yrs2show[2], 
    yrs2show[1], 
    c('lbShortage','lbShortageStep1','lbShortageStep2', 'lbShortageStep3'), 
    c('eq','uebGt823')
  )
  
  # create data table from the above values
  cpt1 <- data.frame(
    'ChanceOf' = c(paste(yrs2show[1],names(cp1)),paste(yrs2show[2],names(cp4))),
    'PrctChance' = c(cp1,cp4)
  )
  rr <- which(
    rownames(sysTable$fullTable) == 
      'Mid-Elevation Release Tier - annual release = 7.48 maf'
  )
  cc <- which(colnames(sysTable$fullTable) == yrs2show[1])
  cpt1$PowellWYRel <- paste('7.48 MAF;',sysTable$fullTable[rr,cc])
  
  cpt2 <- data.frame(
    'ChanceOf' = c(paste(yrs2show[1],names(cp2)),paste(yrs2show[2],names(cp5))),
    'PrctChance' = c(cp2,cp5)
  )
  rr <- which(
    rownames(sysTable$fullTable) == 
      "Upper Elevation Balancing - annual release = 8.23 maf"
  )
  cpt2$PowellWYRel <- paste('8.23 MAF;',sysTable$fullTable[rr,cc])
  
  cpt3 <- data.frame(
    'ChanceOf' = c(paste(yrs2show[1],names(cp3)),paste(yrs2show[2],names(cp6))),
    'PrctChance' = c(cp3,cp6)
  )
  rr <- which(
    rownames(sysTable$fullTable) == 
      "Upper Elevation Balancing - annual release > 8.23 maf"
  )
  rr2 <- which(
    rownames(sysTable$fullTable) == "Equalization - annual release > 8.23 maf"
  )
  cpt3$PowellWYRel <- paste(
    '> 8.23 MAF;',
    sysTable$fullTable[rr,cc] + sysTable$fullTable[rr2,cc]
  )
  
  cpt1 <- rbind(cpt1,cpt2,cpt3)
  
  # rearrange columns
  cpt1 <- cpt1[c('PowellWYRel','ChanceOf','PrctChance')]
  cpt1$PrctChance <- cpt1$PrctChance*100
  data.table::fwrite(cpt1, ofile, row.names = F)
}
