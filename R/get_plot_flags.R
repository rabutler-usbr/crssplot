get_plot_flags <- function(ui)
{
  csd_flag <- FALSE
  std_comparison <- FALSE
  heat <- FALSE
  cloud <- FALSE
  mead_pe_scatter <- FALSE
  shortage_conditions <- FALSE
  
  for (i in seq_along(ui[["plot_group"]])) {
    csd_flag <- csd_flag | ui[["plot_group"]][[i]][["csd_ann"]][["create"]]
    std_comparison <- std_comparison | ui[["plot_group"]][[i]][["std_comparison"]][["create"]]
    heat <- heat | ui[["plot_group"]][[i]][["heat"]][["create"]]
    cloud <- cloud | ui[["plot_group"]][[i]][["cloud"]][["create"]]
  }
  
  cond_probs <- FALSE

  for (i in seq_along(ui[["scen_tree"]])) {
    cond_probs <- cond_probs | ui[["scen_tree"]][[i]][["cond_probs"]]
  }
  
  # mead_pe_scatter
  for (i in seq_along(ui[["ind_plots"]][["mead_pe_scatter"]])) {
    mead_pe_scatter <- mead_pe_scatter | 
      isTRUE(ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["create"]])
  }
  
  # shortage_conditions
  for (i in seq_along(ui[["ind_plots"]][["shortage_conditions"]])) {
    shortage_conditions <- shortage_conditions | 
      isTRUE(ui[["ind_plots"]][["shortage_conditions"]][[i]][["create"]])
  }
  
  list(
    csd_flag = csd_flag, 
    cond_probs = cond_probs, 
    std_comparison = std_comparison,
    heat = heat,
    cloud = cloud,
    simple_5yr = simple_5yr,
    mead_pe_scatter = mead_pe_scatter,
    shortage_conditions = shortage_conditions
  )
}
