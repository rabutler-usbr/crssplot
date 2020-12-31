
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