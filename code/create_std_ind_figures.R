create_std_ind_figures <- function(cs, sys_cond, mainScenGroup, yrs2show, ui)
{
  critStatsFig1 <- plotCritStats(dplyr::filter(
    cs, 
    AggName == mainScenGroup, 
    !(Variable %in% c('mead_min_lt_1020','lbSurplus'))
  ), 
  yrs2show, 
  ui$defaults$annText
  )
  
  critStatsFig2 <- plotCritStats(dplyr::filter(
    cs, 
    AggName == mainScenGroup, 
    !(Variable %in% c('mead_dec_lt_1025','lbSurplus'))
  ), 
  yrs2show, 
  ui$defaults$annText
  )
  
  # shortage surplus figure
  # defaults ok for legendTitle, nC, and legLoc
  ssPlot <- plotShortageSurplus(
    dplyr::filter(
      sys_cond, 
      Variable %in% c('lbShortage', 'lbSurplus'),
      Agg == mainScenGroup
    ), 
    yrs2show, 
    names(mainScenGroup)
  )
  
  # stacked barplot of different shortage tiers
  # default for annSize is ok
  shortStack <- plotShortStackedBar(
    dplyr::filter(
      sys_cond, 
      Variable %in% c('lbShortageStep1','lbShortageStep2','lbShortageStep3'),
      Agg == mainScenGroup
    ), 
    yrs2show, 
    ui$defaults$annText
  )
  
  list(
    critStatsFig1,
    critStatsFig2,
    ssPlot,
    shortStack
  )
}
