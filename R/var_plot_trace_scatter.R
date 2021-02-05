
#' @description 
#' `var_plot_trace_scatter()` creates a trace number vs. value scatter plot for
#' a single year and variable. If multiple `scenarios` are specified the 
#' scenarios are shown as different facets. 
#' 
#' @param color_by For `var_plot_trace_scatter()`, the points can be colored 
#'   based on a specified column in the `df`. This should be specified as a 
#'   string, and should exist in `df`. 
#'   
#' @examples 
#' # scatter plot for Mead elevation in Dec. 2021
#' var_plot_trace_scatter(
#'   ex_pe, 
#'   vars = "mead_dec_pe", 
#'   years = 2021, 
#'   scenarios = "April ST CT"
#' )
#' 
#' # add in a new variable to be used to color the points:
#' zz <- mutate(ex_pe, color_cat = case_when(
#'   Value > 1095 ~ "No concern",
#'   Value > 1076 ~ "Some concern",
#'   Value > 1074 ~ "Moderate concern",
#'   TRUE ~ "concern")
#' )
#' 
#' cc <- c("No concern" = "grey20", "Some concern" = "blue", 
#'         "Moderate concern" = "steelblue", "concern" = "red")
#' 
#' # color by the new variable, and show two scenarios:
#' gg <- var_plot_trace_scatter(
#'   zz, 
#'   vars = "mead_dec_pe", 
#'   years = 2021, 
#'   scenarios = c("April ST CT", "April ST 2007 UCRC"), 
#'   color_by = "color_cat"
#' )
#' 
#' @rdname scens_plot_
#' @export
var_plot_trace_scatter <- function(df, scenarios,  years, vars, color_by = NULL,
                                   plot_colors = NULL, ...) {
  check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value", 
                               "TraceNumber", color_by))
  
  # check vars and years --------------------------
  assert_that(
    vars %in% unique(df$Variable) && length(vars) == 1, 
    msg = "In `var_plot_trace_scatter()`, there should be only 1 `vars` and it must exist in `df`."
  )
  
  assert_that(
    years %in% unique(df$Year) && length(years) == 1, 
    msg = "In `var_plot_trace_scatter()`, there should be only 1 `years` and it must exist in `df`."
  )
  
  df <- df %>%
    filter(Variable == vars, Year == years, ScenarioGroup %in% scenarios)
  
  # parse ... and other plot options
  if (!is.null(color_by)) {
    plot_colors <- determine_plot_colors(plot_colors, unique(df[[color_by]]))
  }
  
  # check options ----------------------
  ops <- list(...)
  # these are the plotting options this function can handle
  exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
               "facet_scales", "facet_nrow", "facet_ncol", "subtitle")
  
  check_options(names(ops), exp_ops)
  if (!exists("color_label", where = ops)) {
    ops[["color_label"]] <- NULL
  }
  if (!exists("facet_scales", where = ops)) {
    ops[["facet_scales"]] <- "fixed"
  }
  
  if (!exists("y_lab", where = ops)) {
    ops[["y_lab"]] <- NULL
  }
  
  # TODO: update for color_by
  if (!is.null(ops$legend_wrap) && !is.null(color_by)) {
    df <- df %>%
      mutate_at(color_by = stringr::str_wrap(
        color_by, 
        width = ops$legend_wrap
      ))
    
    # also update the plot color names
    names(plot_colors) <- stringr::str_wrap(
      names(plot_colors), 
      width = ops$legend_wrap
    )
  }
  
  # plot --------------------------------------------
  if (is.null(color_by)) {
    gg <- ggplot(df, aes(TraceNumber, Value))
  } else {
    gg <- ggplot(df, aes_string("TraceNumber", "Value", color = color_by)) +
      scale_color_manual(values = plot_colors)
  }
  
  gg <- gg +
    geom_point(size = 3, shape = 18) +
    scale_y_continuous(
      labels = scales::comma, 
      minor_breaks = seq(800, 1200, 5)
    ) +
    labs(
      y = ops$y_lab, title = ops$title, caption = ops$caption, 
      x = "trace number", color = ops$color_label, subtitle = ops$subtitle
    ) + 
    theme_crss()
  
  if (length(scenarios) > 1) {
    gg <- gg + 
      facet_wrap(
        ~ScenarioGroup, 
        scales = ops$facet_scales, 
        nrow = ops$facet_nrow, 
        ncol = ops$facet_ncol
      )
  }
  
  gg
  
  gg
}