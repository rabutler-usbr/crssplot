create_std_ind_figures <- function(cs, sys_cond, scenario, ui)
{
  sy <- ui[["scen_tree"]][[scenario]][["start_year"]]
  ey <- ui[["ind_plots"]][["std_ind_figures"]][[scenario]][["options"]][["end_year"]]
  yrs2show <- sy:ey
  
  ann_txt <- ui[["ind_plots"]][["std_ind_figures"]][[scenario]][["options"]][["ann_text"]]
 
  critStatsFig1 <- plotCritStats(
    dplyr::filter(
      cs, 
      ScenarioGroup == scenario, 
      !(Variable %in% c('mead_min_lt_1020','lbSurplus'))
    ), 
    yrs2show, 
    ann_txt
  )
  
  critStatsFig2 <- plotCritStats(dplyr::filter(
    cs, 
    ScenarioGroup == scenario, 
    !(Variable %in% c('mead_dec_lt_1025','lbSurplus'))
  ), 
  yrs2show, 
  ann_txt
  )
  
  # shortage surplus figure
  # defaults ok for legendTitle, nC, and legLoc
  ssPlot <- plotShortageSurplus(
    dplyr::filter(
      sys_cond, 
      Variable %in% c('lbShortage', 'lbSurplus'),
      ScenarioGroup == scenario
    ), 
    yrs2show, 
    scenario
  )
  
  # stacked barplot of different shortage tiers
  # default for annSize is ok
  shortStack <- plotShortStackedBar(
    dplyr::filter(
      sys_cond, 
      Variable %in% c('lbShortageStep1','lbShortageStep2','lbShortageStep3'),
      ScenarioGroup == scenario
    ), 
    yrs2show, 
    ann_txt
  )
  
  list(
    critStatsFig1,
    critStatsFig2,
    ssPlot,
    shortStack
  )
}
