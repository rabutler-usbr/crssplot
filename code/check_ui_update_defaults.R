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

# checks the scen_names in heat and ensures that they match the specified
# plot_scenarios
check_heat_scen_names <- function(pg)
{
  #TODO: there should be no duplicate names
  
  err <- NULL
  
  for (i in seq_along(pg)) {
    if (exists("heat", where = pg[[i]]) & pg[[i]][["heat"]][["create"]]) {
      spec_names <- names(pg[[i]][["heat"]][["scen_names"]])
      spec_names <- spec_names[!(spec_names %in% pg[[i]][["plot_scenarios"]])]
      
      if (length(spec_names) > 0) {
        err <- c(
          err, 
          paste0(
            "The following scen_names in the heat scenarios for the ", 
            names(pg)[[i]], 
            " plot_group, do not appear in the specified plot_scenarios:\n  -",
            paste(spec_names, collapse = "\n  -")
          )
        )
      }
    }
  }
  
  assert_that(length(err) == 0, msg = paste(err, collapse = "\n"))
  
  pg
}

# checks the scen_names in heat and ensures that they match the specified
# plot_scenarios
check_simple5yr_scen_names <- function(pg)
{
  err <- NULL
  e2 <- NULL
  
  for (i in seq_along(pg)) {
    if (exists("simple_5yr", where = pg[[i]]) & pg[[i]][["simple_5yr"]][["create"]]) {
      spec_names <- names(pg[[i]][["simple_5yr"]][["scen_names"]])
      
      if (length(spec_names) != 2) {
        e2 <- c(e2, paste0(
          "There should be exactly 2 specified scenarios in the ", 
          names(pg)[[i]],
          " plot_group, because it wants to create the '5-year simple' table"
        ))
      }
      
      spec_names <- spec_names[!(spec_names %in% pg[[i]][["plot_scenarios"]])]
      
      if (length(spec_names) > 0) {
        err <- c(
          err, 
          paste0(
            "The following scen_names in the 5-year simple for the ", 
            names(pg)[[i]], 
            " plot_group, do not appear in the specified plot_scenarios:\n  -",
            paste(spec_names, collapse = "\n  -")
          )
        )
      }
      
     
    }
  }
  err <- c(e2, err)
  assert_that(length(err) == 0, msg = paste(err, collapse = "\n"))
  
  pg
}

# checks the scen_labs in cloud and ensures that they have the same length
# as the plot_scenarios
check_cloud_scen_names <- function(pg)
{
  err <- NULL

  for (i in seq_along(pg)) {
    if (exists("cloud", where = pg[[i]]) & pg[[i]][["cloud"]][["create"]]) {
      spec_names <- pg[[i]][["cloud"]][["scen_labs"]]
      
      if (length(spec_names) != length(pg[[i]][["plot_scenarios"]])) {
        err <- c(
          err, 
          paste0(
            "The scen_labs in the ", 
            names(pg)[[i]], 
            " plot_group, need to have the same length as the specified plot_scenarios:"
          )
        )
      }
    }
  }

  assert_that(length(err) == 0, msg = paste(err, collapse = "\n"))
  
  pg
}
