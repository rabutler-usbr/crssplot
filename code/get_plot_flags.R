get_plot_flags <- function(ui)
{
  csd_flag <- FALSE
  
  for (i in seq_along(ui[["plot_group"]])) {
    csd_flag <- csd_flag | ui[["plot_group"]][[i]][["csd_ann"]][["create"]]
  }
  
  cond_probs <- FALSE
  for (i in seq_along(ui[["scen_tree"]])) {
    cond_probs <- cond_probs | ui[["scen_tree"]][[i]][["cond_probs"]]
  }
  
  list(csd_flag = csd_flag, cond_probs = cond_probs)
}
