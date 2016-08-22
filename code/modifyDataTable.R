# pulled out of RWDataPlot -> modifyDataTable.R during development of version 0.3.1
# b/c it contains references to multiple functions that do not exist in the RWDataPlot
# package.

# this function was orignially used during DCP analysis during Spring 2015 and prior

dummyFunction <- function()
{
  # tag for drought contingency modeling
  if(tags == 'DCP'){
    scenFull <- strsplit(sPath,'/')[[1]]
    scenFull <- scenFull[length(scenFull)]
    
    dryOrWet15 <- readDryWetFile(scenFull, '15To19')
    dryOrWet20 <-readDryWetFile(scenFull, '20To26')
    vulnT <- readVulnFile(scenFull)
    d15 <- match(allRes$Trace, names(dryOrWet15))
    d15 <- dryOrWet15[d15]
    d20 <- match(allRes$Trace, names(dryOrWet20))
    d20 <- dryOrWet20[d20]
    vv <- match(allRes$Trace, names(vulnT))
    vv <- vulnT[vv]
    
    allRes$DryOrWet2015To2019 <- simplify2array(d15)
    allRes$DryOrWet2020To2026 <- simplify2array(d20)
    allRes$VulnTrace <- simplify2array(vv)
    
    # create a separate column for supply, one for Scenario, and one for hyd-Scenario
    tmp <- as.character(allRes$Scenario)
    allRes$Hyd_Scen <- allRes$Scenario
    onlyScen <- getScenNoSupply(scenFull)
    onlyHyd <- getScenHydrology(scenFull)
    allRes$Scenario <- rep(onlyScen, nrow(allRes))
    allRes$Hydrology <- rep(onlyHyd, nrow(allRes))
  }
}