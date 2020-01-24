get_plot_flags <- function(ui)
{
  csd_flag <- FALSE
  std_comparison <- FALSE
  heat <- FALSE
  cloud <- FALSE
  simple_5yr <- FALSE
  
  for (i in seq_along(ui[["plot_group"]])) {
    csd_flag <- csd_flag | ui[["plot_group"]][[i]][["csd_ann"]][["create"]]
    std_comparison <- std_comparison | ui[["plot_group"]][[i]][["std_comparison"]][["create"]]
    heat <- heat | ui[["plot_group"]][[i]][["heat"]][["create"]]
    cloud <- cloud | ui[["plot_group"]][[i]][["cloud"]][["create"]]
    simple_5yr <- simple_5yr | ui[["plot_group"]][[i]][["simple_5yr"]][["create"]]
  }
  
  cond_probs <- FALSE

  for (i in seq_along(ui[["scen_tree"]])) {
    cond_probs <- cond_probs | ui[["scen_tree"]][[i]][["cond_probs"]]
    
  }
  
  list(
    csd_flag = csd_flag, 
    cond_probs = cond_probs, 
    std_comparison = std_comparison,
    heat = heat,
    cloud = cloud,
    simple_5yr = simple_5yr
  )
}
