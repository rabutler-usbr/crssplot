create_std_ind_figures <- function(cs, sys_cond, scenario, ui)
{
  sy <- ui[["scen_tree"]][[scenario]][["start_year"]]
  ey <- ui[["ind_plots"]][["std_ind_figures"]][[scenario]][["options"]][["end_year"]]
  yrs2show <- sy:ey
  
  ann_txt <- ui[["ind_plots"]][["std_ind_figures"]][[scenario]][["options"]][["ann_text"]]
  
  all_vars <- unique(cs$Variable)
  
  critStatsFig1 <- vars_plot_probs(
    cs,
    scenarios = scenario,
    years = yrs2show,
    vars = all_vars[!(all_vars %in% c('mead_min_lt_1020','lbSurplus'))],
    caption = ann_txt,
    var_labels = csVarNames(),
    legend_wrap = 14
  ) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 4, title = NULL))
  
  critStatsFig2 <- vars_plot_probs(
    cs,
    scenarios = scenario,
    years = yrs2show,
    vars = all_vars[!(all_vars %in% c('mead_dec_lt_1025','lbSurplus'))],
    caption = ann_txt,
    var_labels = csVarNames(),
    legend_wrap = 14
  ) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 4, title = NULL))
  
  # shortage surplus figure ---------------------
  my_title <- paste(
    'Percent of Traces with Lower Basin Surplus or Shortage\nResults from the',
    scenario, 'CRSS Run*'
  )
  var_name <- c("lbShortage" = 'Shortage of Any Amount',
               "lbSurplus" = 'Surplus of Any Amount')
  
  ssPlot <- vars_plot_probs(
    sys_cond,
    scenarios = scenario,
    years = yrs2show,
    vars = c('lbShortage', 'lbSurplus'),
    title = my_title,
    var_labels = var_name
  ) +
    theme(legend.position = "bottom")
  
  # stacked barplot of different shortage tiers ---------------------------
  var_name <- c("lbShortageStep1" = "Step 1 Shortage",
               "lbShortageStep2" = "Step 2 Shortage",
               "lbShortageStep3" = "Step 3 Shortage")
  shortStack <- vars_plot_probs(
    sys_cond,
    scenarios = scenario,
    years = yrs2show,
    vars = c('lbShortageStep1','lbShortageStep2','lbShortageStep3'),
    caption = ann_txt,
    var_labels = var_name,
    plot_type = 'stacked bar'
  ) +
    theme(legend.position = "bottom")
  
  gg_list(
    "ind_crit_stats1" = critStatsFig1,
    "ind_crit_stats2" = critStatsFig2,
    "ind_short_surplus" = ssPlot,
    "ind_stacked_short" = shortStack
  )
}
