# this will specify the list of individual plots to make
# ind_plots
#   $std_ind_figures
#   $std_ind_tables
#   $shortage_conditions
#   $pe_scatter
# each entry in the list, will have multiple lists where the name corresponds
# to a scenario, and that is a list that has the plot options in it:
#   $std_ind_figures[['Scenario']]
#     $options

specify_individual_plots <- function(scen_list, std_fig_specs)
{
  ind_plots <- list(
    "std_ind_figures" = list(),
    "std_ind_tables" = list(),
    "shortage_conditions" = list(),
    "pe_scatter" = list()
  )
 
  # loop through scen_list and find scenarios with XXX = TRUE
  for (i in seq_along(scen_list)) {
    if (scen_list[[i]]$std_ind_figures) {
      # create the list
      scen_name <- scen_list[[i]]$name
      tmp = list("options" = list(
        ann_text = std_fig_specs[[scen_name]]$ann_text,
        end_year = std_fig_specs[[scen_name]]$end_year
      ))
      
      ind_plots$std_ind_figures[[scen_name]] <- tmp
    }
    
    if (scen_list[[i]]$std_ind_table) {
      scen_name <- scen_list[[i]]$name
      
      # TODO: are there any table options?
      ind_plots$std_ind_tables[[scen_name]] <- TRUE
    }
  }
  
  # TODO: check for other options, and if they don't exist use defaults
  
  ind_plots
}