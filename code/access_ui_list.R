# access_ui_list.R
# helper functions to access the bohemeth that is the ui-list. 

# gets all of the scenarios that will be plotted, either individually, or in a 
# comparison plot, or have a table created for them
get_all_plot_scenarios <- function(ui)
{
  scens <- c(
    names(ui$ind_plots$std_ind_figures),
    names(ui$ind_plots$std_ind_tables)
  )
  
  if (ui$create_figures$standard_figures) {
    for (i in seq_along(ui[["plot_group"]])) {
      scens <- c(scens, ui$plot_group[[i]]$plot_scenarios)
    }
  }
  
  if (ui$create_figures$pe_clouds) {
    scens <- c(scens, ui$clouds$scenarios)
  }
  
  if (ui$create_figures$heatmap) {
    scens <- c(scens, names(ui$heatmap$scenarios))
  }
  
  unique(scens)
}

get_cond_prob_scens <- function(ui)
{
  ss <- c()
  
  for (i in seq_along(ui[["scen_tree"]])) {
    if (ui[["scen_tree"]][[i]][["cond_probs"]]) {
      ss <- c(ss, ui[["scen_tree"]][[i]][["name"]])
    }
  }
  
  ss
}
