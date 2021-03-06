create_scenario_comparison_figures <- function(pe, cs, ui, o_files)
{
  # 1. loop through the multiple plot_groups
  # 2. create temp_pe and temp_cs
  # 3. c() the plot_list
  # 4. return that list
  olist <- list()
  
  for (i in seq_along(ui[["plot_group"]])) {
    
    if (should_create_plot(ui[["plot_group"]][[i]], "std_comparison")) {
    
      message("  ... plot_group: ", i, " ", names(ui[["plot_group"]])[i])
        
      cur_pg <- ui[["plot_group"]][[i]]
      
      yrs2show <- cur_pg[["std_comparison"]][["years"]]
      peYrs <- c(yrs2show[1] - 1, yrs2show)
      
      tmp_pe <- pe %>%
        filter(ScenarioGroup %in% cur_pg[["plot_scenarios"]])
      
      tmp_cs <- cs %>%
        filter(ScenarioGroup %in% cur_pg[["plot_scenarios"]])
      
      # 10/50/90 ----------------------
      message("    ... 10/50/90s")
      
      powellPE <- scens_plot_range(
        tmp_pe,
        vars = "powell_dec_pe",
        years = peYrs,
        plot_colors = cur_pg[["plot_colors"]],
        title = 'Lake Powell End-of-December Elevation',
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        y_lab = "feet"
      )
      
      meadPE <- scens_plot_range(
        tmp_pe,
        vars = "mead_dec_pe",
        years = peYrs,
        plot_colors = cur_pg[["plot_colors"]],
        title = 'Lake Mead End-of-December Elevation',
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        y_lab = "feet"
      )
      
      # Critical elevation thresholds ------------ 
      # figures and data table have sysCond for some, and 
      # read in crit stats for others
      message("    ... starting critical stats")
      
      ptitle <- paste(
        'Lake Powell: Percent of Traces Less than Power Pool', 
        "(elevation 3,490\') in Any Water Year",
        sep = "\n"
      )
      
      p_3490_fig <- scens_plot_probs(
        tmp_cs,
        vars = "powell_wy_min_lt_3490",
        years = yrs2show,
        title = ptitle,
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        plot_colors = cur_pg[["plot_colors"]]
      )
      
      shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'

      shortFig <- scens_plot_probs(
        tmp_cs,
        vars = "lbShortage",
        years = yrs2show,
        title = shortTitle,
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        plot_colors = cur_pg[["plot_colors"]]
      )
      
      surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
      
      surpFig <- scens_plot_probs(
        tmp_cs,
        vars = "lbSurplus",
        years = yrs2show,
        title = surpTitle,
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        plot_colors = cur_pg[["plot_colors"]]
      )
      
      p_3525_fig <- scens_plot_probs(
        tmp_cs,
        vars = "powell_wy_min_lt_3525",
        years = yrs2show,
        title = "Lake Powell: Percent of Traces Less than elevation 3,525' in Any Water Year",
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        plot_colors = cur_pg[["plot_colors"]]
      )
      
      m_1025_fig <- scens_plot_probs(
        tmp_cs,
        vars = "mead_dec_lt_1025",
        years = yrs2show,
        title = "Lake Mead: Percent of Traces Less than elevation 1,025' in December",
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        plot_colors = cur_pg[["plot_colors"]]
      )
      
      m_1000_fig <- scens_plot_probs(
        tmp_cs,
        vars = "mead_min_lt_1000",
        years = yrs2show,
        title = "Lake Mead: Percent of Traces Less than elevation 1,000' in Any Month",
        color_label = ui$defaults$color_label,
        legend_wrap = ui$defaults$legend_wrap,
        plot_colors = cur_pg[["plot_colors"]]
      )

      olist[[names(ui[["plot_group"]])[i]]] <- 
        gg_list(
          "powellPE" = powellPE,
          "meadPE" =meadPE,
          "p3525" = p_3525_fig,
          "p3490" = p_3490_fig,
          "m1025" = m_1025_fig,
          "m1000" = m_1000_fig,
          "shortage" = shortFig,
          "surplus" = surpFig
        )
    }
  }
  
  pgs_out(olist)
}