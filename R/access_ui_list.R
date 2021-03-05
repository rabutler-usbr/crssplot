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
  
  for (i in seq_along(ui[["plot_group"]])) {
    # just add all of the scenarios in plot_group
    scens <- c(scens, ui$plot_group[[i]]$plot_scenarios)
  }
  
  unique(scens)
}
