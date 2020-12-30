#' @description 
#' `vars_plot_probs()` plots the probabilities for multiple variables on a a 
#' single plot. Different `vars` are shown as different colors, and if there are 
#' multiple `scenarios` they are shown as different facets. The variables' 
#' probabilities can be shown as lines (`plot_type = "line"`) or as stacked bars
#' (`plot_type = "stacked bar"`). For stacked bars, care should be taken by user
#' to ensure summing probabilities makes sense.
#' 
#' @param plot_type For `vars_plot_probs()`, should the plot use lines 
#'   (`'line'`), or stacked bars (`'stacked bar'`). Can also use `1` or `2`, 
#'   respectively.
#' 
#' @examples 
#' vv <- c("mead_min_lt_1000", "mead_min_lt_1020", "powell_wy_min_lt_3490", 
#' "powell_dec_lt_3525")
#' 
#' gg <- vars_plot_probs(ex_pe, "April ST CT", vars = vv)
#' 
#' @rdname scens_plot_
#' @export
vars_plot_probs <- function(df, scenarios,  years = NULL, vars = NULL, 
                            plot_colors = NULL, var_labels = NULL, 
                            plot_type = "line", ...) {
  # check df -------------------------------
  check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value"))
  
  # update vars if NULL --------------------------
  if (is.null(vars)) {
    vars <- unique(df$Variable)
  }
  
  # var_labels ---------------------------------------
  if (is.null(var_labels)) {
    var_labels <- waiver()
  }
  
  # compute stats -----------------------------------
  if (!is.null(years)) {
    df <- filter(df, Year %in% years)
  } else {
    years <- unique(df$Year)
  }
  
  # check plot type ---------------------------------
  plot_types <- c("line", "stacked bar")
  if (is.character(plot_type)) {
    plot_type <- match.arg(plot_type, plot_types)
  } else if (is.numeric(plot_type)) {
    assert_that(plot_type %in% 1:2)
    plot_type <- plot_types[plot_type]
  } else {
    stop("`plot_type` is an invalid type.")
  }
  
  df <- df %>%
    dplyr::filter(Variable %in% vars, ScenarioGroup %in% scenarios) %>%
    dplyr::group_by(ScenarioGroup, Year, Variable) %>%
    dplyr::summarise(Value = mean(Value))
  
  # parse ... and other plot options
  plot_colors <- determine_plot_colors(plot_colors, vars)
  
  myLabs <- get_year_breaks(years)
  
  ops <- list(...)
  # these are the plotting options this function can handle
  exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
               "facet_scales", "facet_nrow", "facet_ncol")
  
  check_options(names(ops), exp_ops)
  if (!exists("color_label", where = ops)) {
    ops[["color_label"]] <- NULL
  }
  if (!exists("facet_scales", where = ops)) {
    ops[["facet_scales"]] <- "fixed"
  }
  
  if (!exists("y_lab", where = ops)) {
    ops[["y_lab"]] <- "Percent of Traces"
  }
 
  if (!is.null(ops$legend_wrap)) {
    df <- df %>%
      mutate(Variable = stringr::str_wrap(
        Variable, 
        width = ops$legend_wrap
      ))
    
    # also update the plot color names
    names(plot_colors) <- stringr::str_wrap(
      names(plot_colors), 
      width = ops$legend_wrap
    )
    
    if (!is(var_labels, "waiver")) {
      tmp <- stringr::str_wrap(names(var_labels), width = ops$legend_wrap)
      var_labels <- stringr::str_wrap(var_labels, width = ops$legend_wrap)
      names(var_labels) <- tmp
    }
  }
  
  # plot --------------------------------------------
  yL <- c(0, 1)
  
  if (plot_type == "line") {
    gg <- ggplot(df, aes(Year, Value, color = Variable)) +
      geom_line(size = 1) +
      scale_color_manual(
        values = plot_colors, 
        guide = guide_legend(title = ops$color_label),
        labels = var_labels
      )
  } else {
    # stacked barplot
    gg <- ggplot(df, aes(Year, Value, fill = Variable)) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(
        values = plot_colors, 
        guide = guide_legend(title = ops$color_label),
        labels = var_labels
      )
  }
  
  gg <- gg + 
    scale_x_continuous(
      breaks = myLabs,
      minor_breaks = 1900:3000, 
      labels = myLabs
    ) + 
    coord_cartesian(ylim = yL) +
    scale_y_continuous(
      minor_breaks = seq(yL[1], yL[2], 0.05), 
      breaks = seq(yL[1], yL[2], 0.10),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(y = ops$y_lab, title = ops$title, caption = ops$caption) + 
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
}
  
  