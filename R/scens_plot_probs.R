
#' Plot scenario comparison figures
#' 
#' `scens_plot_probs()` plots probability plots, i.e., the chance of a variable
#' occurring. Different `scenarios` are shown as different colors, and if there
#' are different variables (`vars`) they are shown as different facets. 
#' 
#' `...` is used to pass additional options to ggplot functions. The following
#' are used: 
#' 
#' - "title", and "caption" are passed to [ggplot2::labs()]. "color_label" and 
#' "y_lab" are also passed using `color` and `y` parameters, respectively.
#' - "legend_wrap" is used to modify labels in legends etc. by calling 
#' [stringr::str_wrap()] on the appropriate variable, with 
#' `width = legend_wrap`.
#' - "facet_scales", "facet_nrow", and "facet_ncol" are all passed to 
#' [ggplot2::facet_wrap()].
#' 
#' `scens_plot_probs()` and `scens_plot_range()` will use the following 
#' additional options: "y", "title", "color_label", "legend_wrap", 
#' "facet_scales", "facet_nrow", and "facet_ncol".
#' 
#' The legend order can be modified by converting "ScenarioGroup" column to a 
#' factor before calling `scens_plot_*()`, with the levels specifying the
#' order the scenarios will show up in the legend.
#' 
#' @param df Data frame. Must have "Year", "Variable", "ScenarioGroup", and 
#'   "Value" columns.
#'   
#' @param vars Character vector specifying the variable(s) to use.
#' 
#' @param years Numeric vector specifying the years to show. If `NULL`, use all
#'   years in `df`.
#'   
#' @param scenarios Character vector specifying the scenarios to use (found in
#' `df$ScenarioGroup`). If `NULL`, use all scenarios in `df`.
#' 
#' @param plot_colors Named character vector to set custom plot colors. Names
#'   should match scenarios found in `df$ScenarioGroup`. 
#'   
#' @param scen_labels Named character vector to set custom legend labels for the
#'   scenarios. Used to show legend labels that are different from values found
#'   in `df$ScenarioGroup`.
#'   
#' @param ... Parameters passed to other functions. See details.
#'   
#' @return `gg` object.
#' 
#' @rdname scens_plot_
#' @export
scens_plot_probs <- function(df, vars,  years = NULL, scenarios = NULL, 
                             plot_colors = NULL, scen_labels = NULL, ...) {
  
  # check df -------------------------------
  check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value"))
  
  # update scenarios if NULL --------------------------
  if (is.null(scenarios)) {
    scenarios <- unique(df$ScenarioGroup)
  }
  
  # scen_labels ---------------------------------------
  if (is.null(scen_labels)) {
    scen_labels <- waiver()
  }
  
  # compute stats -----------------------------------
  if (!is.null(years)) {
    df <- filter(df, Year %in% years)
  } else {
    years <- unique(df$Year)
  }
  
  df <- df %>%
    dplyr::filter(Variable %in% vars, ScenarioGroup %in% scenarios) %>%
    # compute the 10/50/90 and aggregate by start month
    dplyr::group_by(ScenarioGroup, Year, Variable) %>%
    dplyr::summarise(
      Value = mean(Value)
    )
  
  # parse ... and other plot options
  plot_colors <- determine_plot_colors(plot_colors, scenarios)
  
  if (length(years) < 15) {
    myLabs <- 1900:3000
  } else {
    myLabs <- seq(1900, 3000, 5)
  }
  
  ops <- list(...)
  # these are the plotting options this function can handle
  exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
               "facet_scales", "facet_nrow", "facet_ncol")
  
  check_options(names(ops), exp_ops)
  if (!exists("color_label", where = ops)) {
    ops[["color_label"]] <- "Scenario"
  }
  if (!exists("facet_scales", where = ops)) {
    ops[["facet_scales"]] <- "fixed"
  }
  
  if (!is.null(ops$legend_wrap)) {
    zz <- zz %>%
      mutate(ScenarioGroup = stringr::str_wrap(
        StartMonth, 
        width = ops$legend_wrap
      ))
    
    # also update the plot color names
    names(plot_colors) <- stringr::str_wrap(
      names(plot_colors), 
      width = ops$legend_wrap
    )
  }
  
  # plot --------------------------------------------
  gg <- ggplot(
    df, 
    aes(Year, Value, color = ScenarioGroup)
  ) +
    geom_line(size = 1) + 
    scale_x_continuous(
      breaks = myLabs,
      minor_breaks = 1900:3000, 
      labels = myLabs
    ) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(y = ops$y_lab, title = ops$title, caption = ops$caption) +
    scale_color_manual(
      values = plot_colors, 
      guide = guide_legend(title = ops$color_label),
      labels = scen_labels
    ) +
    theme_crss()
  
  if (length(vars) > 1) {
    gg <- gg + 
      facet_wrap(~Variable, scales = ops$facet_scales, nrow = ops$facet_nrow, 
                 ncol = ops$facet_ncol)
  }
  
  gg
}