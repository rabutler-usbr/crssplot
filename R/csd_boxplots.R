#' Wrapper to plot csd boxplots for all states.
#' @param zz df containing the csd_ann data
#' @param ui UI list
#' @noRd
create_all_csd_boxplots <- function(zz, ui)
{
  # loop through multiple plot groups
  olist <- list()
  state_info <- csd_state_info()
  states <- state_info[["states"]]

  for (i in seq_along(ui[["plot_group"]])) {
    
    cur_pg <- ui[["plot_group"]][[i]]
    
    if (cur_pg[["csd_ann"]][["create"]]) {
    
      message("  ... plot_group: ", i, " ", names(ui[["plot_group"]])[i])
      tmp_zz <- zz %>%
        filter(ScenarioGroup %in% cur_pg[["plot_scenarios"]])
      
      # get the years to show and the colors
      
      pg_color <- cur_pg[["plot_colors"]]
      pg_years <- cur_pg[["csd_ann"]][["years"]]
      
      # call the boxplot for each state
      for (state in states) {
        tmp <- scens_plot_boxplot(
          tmp_zz,
          vars = state_info[["slots"]][state],
          years = pg_years,
          plot_colors = pg_color,
          title = paste(state, "Annual Actual Use"),
          y_lab = "acre-ft"
        ) +
          geom_hline(
            yintercept = state_info[["apportionment"]][state], 
            linetype = 2, color = "red"
          )
        
        olist <- c(olist, list(tmp))
      }
    }
  }
  
  olist
}

csd_state_info <- function()
{
  states <- c("CO", "NM", "UT", "WY","AZ", "NV", "CA", "MX")
  
  slots <- paste(
    "computed_state_depletions", 
    c("colorado_annual_actual", "new_mexico_annual_actual", "utah_annual_actual",
      "wyoming_annual_actual","arizona_annual_actual", "nevada_annual_actual", 
      "california_annual_actual","mexico_annual_actual_delivery"), 
    sep = "_"
  )
  
  names(slots) <- states
  
  app <- c(3855375, 838125, 1713500, 1043000, 2800000, 300000, 4400000, 1500000)
  names(app) <- states
  
  list(states = states, slots = slots, apportionment = app)
}
