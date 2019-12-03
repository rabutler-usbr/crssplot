# access_ui_list.R
# helper functions to access the bohemeth that is the ui-list. 

# gets all of the scenarios that will be plotted, either individually, or in a 
# comparison plot, or have a table created for them
get_all_plot_scenarios <- function(ui)
{
  unique(c(
    names(ui$ind_plots$std_ind_figures),
    names(ui$ind_plots$std_ind_tables),
    ui$plot_group$plot_scenarios
  ))
}
