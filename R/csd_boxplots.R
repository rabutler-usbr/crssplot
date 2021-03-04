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
        #csd_bxp(zz, state, yrs, plot_colors)
        #tmp <- csd_bxp(tmp_zz, state, pg_years, pg_color)
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

#' Annual boxplot for one variable and multiple scenarios
#' 
#' `csd_bxp()` creates an annual boxplot for one variable and multiple 
#' scenarios. It uses all scenarios found in `zz`. It could be more generic, 
#' except that it relies on `csd_state_info()` that maps state names, to their
#' computed state depletions slot, and includes their apportionment.
#' 
#' @param zz Data frame. Must have Variable, Year, Value, and Agg columns. 
#' 
#' @param state One of: c("CO", "NM", "UT", "WY","AZ", "NV", "CA", "MX").
#' 
#' @param yrs The years to show in plot. `zz` is filtered to only contain 
#'   these years.
#'   
#' @param plot_colors Named vector. Names must match unique Agg values. Sets 
#'   plots to use specific colors for each scenario.
#'   
#' @return `gg` object.
#' 
#' @export
csd_bxp <- function(zz, state, yrs, plot_colors)
{
  state_info <- csd_state_info()

  # boxplot the CSD -----------------------
    gg <- zz %>% 
      filter(Variable %in% state_info[["slots"]][state], Year %in% yrs) %>%
      ggplot(aes(as.factor(Year), Value, fill = ScenarioGroup)) +
      CRSSIO::stat_boxplot_custom() +
      scale_fill_manual(values = plot_colors) +
      labs(title = paste(state, "Annual Actual Use"), x = NULL, y = "acre-ft") +
      geom_hline(
        yintercept = state_info[["apportionment"]][state], 
        linetype = 2, color = "red"
      )
  
  gg
}
