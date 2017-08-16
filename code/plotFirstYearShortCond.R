
library(tidyverse)
library(scales)
library(grid)
library(RWDataPlyr)

getMTOMConditionsData <- function(iFile, filterOn) 
{
  zz <- read.csv(iFile)
  
  if(filterOn == 'shortage'){
    # trim to only traces with shortages  
    zz <- dplyr::filter(zz, Shortage == 1)
  } else if(filterOn == 'pe'){
    zz <- dplyr::filter(zz, DecElev <= 1077)
  } else {
    stop('invalid filterOn passed to plotFirstYearShortCond')
  }
  
  zz$LBPrct <- paste0(round(zz$LBPrct,0),'%')
  zz$WYRelease <- zz$WYRelease/1000
  zz$OND.Release <- as.factor(zz$OND.Release/1000)
  zz$HydrologyYear <- as.factor(zz$HydrologyYear)
  
  zz
}

getCRSSConditionsData <- function(iFolder, scenario, filterOn, dataYear) 
{
  # mead PE in December dataYear
  # Powell WY Rel in dataYear
  # Powell OND Rel in dataYear
  # LB side inflow in dataYear
  sal <- createSlotAggList(matrix(
    c("KeySlots.rdf", "Mead.Pool Elevation", "EOCY", NA, "DecElev",
    "KeySlots.rdf", "PowellOperation.PowellWYRelease", "EOCY", .000001, "WYRelease",
    "Check.rdf", "TotVal.Mead", "AnnualSum", NA, "nfAbvMead",
    "Check.rdf", "TotVal.BelowMead", "AnnualSum", NA, "nfBelowMead"),
    byrow = TRUE,
    ncol = 5
  ))
  sal2 <- createSlotAggList(matrix(
    c("KeySlots.rdf", "Powell.Outflow", "Monthly", NA, "powellOut"),
    byrow = TRUE,
    nrow = 1
  ))
  zz <- getDataForAllScens(scenario, scenario, sal, iFolder, "crssCond.feather", TRUE)
  on.exit(file.remove("crssCond.feather"))
  zzMon <- getDataForAllScens(scenario, scenario, sal2, iFolder, "crssCond2.feather", TRUE)
  on.exit(file.remove("crssCond2.feather"), add = TRUE)
  
  zz <- zz %>%
    filter(Year == dataYear) %>%
    group_by(Scenario, Year, Trace) %>%
    spread(Variable, Value) %>%
    mutate(lbGains = nfAbvMead + nfBelowMead) %>%
    select(-nfAbvMead, -nfBelowMead)
  lbAvg <- mean(zz$lbGains)
  
  zzMon <- zzMon %>%
    filter(Year == dataYear, Month %in% c("Oct", "Nov", "Dec")) %>%
    group_by(Scenario, Trace, Year, Variable) %>%
    summarise(Value = sum(Value)/1000000) %>% # now OND total release 
    group_by(Scenario, Trace, Year) %>%
    spread(Variable, Value)
  
  message("Assuming that ", scenario, " uses full observed hydrology\n",
          "If this is not the case, plotFirstYearShortCond() needs to be revisited")
  
  zz <- full_join(zz, zzMon, by = c("Scenario", "Trace", "Year")) %>%
    ungroup() %>%
    select(-Scenario, -Year) %>%
    mutate(lbGains = paste0(round(lbGains/lbAvg*100,0), "%"),
           Trace = as.factor(Trace + 1905),
           powellOut = as.factor(powellOut),
           WYRelease = round(WYRelease,2)) %>%
    rename(LBPrct = lbGains, OND.Release = powellOut, HydrologyYear = Trace)
    
  
  if(filterOn == 'shortage'){
    # trim to only traces with shortages  
    zz <- dplyr::filter(zz, DecElev <= 1075)
  } else if(filterOn == 'pe'){
    zz <- dplyr::filter(zz, DecElev <= 1077)
  } else {
    stop('invalid filterOn passed to plotFirstYearShortCond')
  }
  on.exit()
  zz
}

# filterOn: valid choices are 'shortage', 'pe'
# if filterOn == pe, then will keep all traces that are <= 1077
plotFirstYearShortCond <- function(model, iFile, scenario, filterOn = 'shortage', dataYear)
{
  if(model == "MTOM"){
    zz <- getMTOMConditionsData(iFile, filterOn)
  } else if(model == "CRSS"){
    zz <- getCRSSConditionsData(iFile, scenario, filterOn, dataYear)
  } else {
    stop("invalid model passed to plotFirstYearShortCond")
  }
  

  gradLabs <- round(range(zz$WYRelease),1)
  gradLabs <- round(seq(gradLabs[1],gradLabs[2],length = 5),1)

  # label points with LB prct of avg.
  gg <- ggplot(zz, aes(HydrologyYear, DecElev, shape = OND.Release, color = WYRelease,
                       label = LBPrct))
  gg <- gg + geom_point(size = 4) + 
    geom_hline(aes(yintercept = 1075),linetype = 2) + 
    scale_color_gradient(paste0('Powell WY ',dataYear,'\nRelease [MAF]'), low = 'red', 
                         high = 'blue', breaks = gradLabs) + 
    geom_text(hjust = .4, vjust = -.6,size = 3.5) + 
    labs(shape = paste0('Powell Oct-Dec\n',dataYear,' Release [MAF]'), x = paste0(dataYear,' Hydrology from Year'),
         y = paste0('Mead End-of-December ',dataYear,' elevation [ft]')) +
    scale_y_continuous(minor_breaks = 900:1200, breaks = seq(900,1200,1), label = comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(round(min(zz$DecElev),0)-1.2,max(c(1075,zz$DecElev))+.5))
  gg
}
