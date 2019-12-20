# Functions to check and specify values in UI lists

check_plot_group_colors <- function(pg)
{
  # loop through plot_group (pg) entries
  
  for (i in seq_along(pg)) {
    
    if (!exists("plot_colors", where = pg[[i]]) & 
        length(pg[[i]][["plot_scenarios"]]) >= 1) 
    {
        pg[[i]][["plot_colors"]] <- scales::hue_pal()(
          length(pg[[i]][["plot_scenarios"]])
        )
      
      #names(plot_group[["plot_colors"]]) <- plot_group[["plot_scenarios"]]
    }
    
    #TODO: change so plot_colors does not have to be specified; if it isn't then
    # what happens?
    
    names(pg[[i]][["plot_colors"]]) <- pg[[i]][["plot_scenarios"]]
  }
  
  pg
}

# if csd_ann is not specified, then set it to FALSE
check_plot_group_type <- function(pg, plot_type)
{
  for (i in seq_along(pg)) {
    if (!exists(plot_type, where = pg[[i]])) {
      pg[[i]][[plot_type]] <- list(create = FALSE)
    }
  }
  
  pg
}

