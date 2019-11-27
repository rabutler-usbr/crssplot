create_scenario_comparison_figures <- function(pe, cs, peYrs, yrs2show, ui, 
                                               o_files)
{
  # 10/50/90 ----------------------
  message("  ... 10/50/90s")
  powellPE <- plotEOCYElev(
    pe, 
    peYrs, 
    "powell_dec_pe", 
    'Powell End-of-December Elevation', 
    ui$defaults$color_label,
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  meadPE <- plotEOCYElev(
    pe, 
    peYrs, 
    "mead_dec_pe", 
    'Mead End-of-December Elevation', 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  # Critical elevation thresholds ------------ 
  # figures and data table have sysCond for some, and 
  # read in crit stats for others
  message("  ... starting critical stats")
  
  ptitle <- paste(
    'Powell: Percent of Traces Less than Power Pool', 
    "(elevation 3,490\') in Any Water Year",
    sep = "\n"
  )
  
  p_3490_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'powell_wy_min_lt_3490', 
    '', 
    ptitle, 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  shortTitle <- 'Lower Basin: Percent of Traces in Shortage Conditions'
  shortFig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'lbShortage', 
    '', 
    shortTitle, 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  surpTitle <- 'Lower Basin: Percent of Traces in Surplus Conditions'
  surpFig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'lbSurplus', 
    '', 
    surpTitle, 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  # 1025, 1000, 3490, 3525 
  p_3525_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    'powell_wy_min_lt_3525', 
    '', 
    "Powell: Percent of Traces Less than elevation 3,525' in Any Water Year", 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  m_1025_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    "mead_dec_lt_1025", 
    '', 
    "Mead: Percent of Traces Less than elevation 1,025' in December", 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  m_1000_fig <- compare_crit_stats(
    cs, 
    yrs2show, 
    "mead_min_lt_1000", 
    '', 
    "Mead: Percent of Traces Less than elevation 1,000' in Any Month", 
    ui$defaults$color_label, 
    legendWrap = ui$defaults$legend_wrap,
    plot_colors = ui$plot_group$plot_colors
  )
  
  list(
    powellPE,
    meadPE,
    p_3525_fig,
    p_3490_fig,
    m_1025_fig,
    m_1000_fig,
    shortFig,
    surpFig
  )
}