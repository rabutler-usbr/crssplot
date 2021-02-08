#' @description 
#' `vars_plot_heatmap()` creates a heatmap based on the percentage of traces 
#' that fall into each bin, i.e., variable. The heatmap is years on the x axis
#' and variables on the y axis, with the color representing the percent of 
#' traces in a given variable. Works with the `title`, `subtitle`, `caption`, 
#' `y_lab`, `color_label`, `legend_wrap`, `facet_scales`, `facet_nrow`, 
#' and `facet_ncol` plot options.
#' 
#' @examples 
#' 
#' vv <- c("mead_min_lt_1000", "mead_min_lt_1020", "powell_wy_min_lt_3490", 
#' "powell_dec_lt_3525")
#' 
#' gg <- vars_plot_probs(ex_pe, "April ST CT", vars = vv, years = 2020:2026)
#' 
#' # or show both scenarios:
#' ss <- unique(ex_pe$ScenarioGroup)
#' gg <- vars_plot_heatmap(ex_pe, ss, vars = vv, years = 2020:2026)
#' 
#' @rdname scens_plot_
#' @export
vars_plot_heatmap <- function(df, scenarios,  years = NULL, vars = NULL, 
                              var_labels = NULL, ...) {
  # check df -------------------------------
  check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value"))
  
  assert_that(
    all(scenarios %in% unique(df$ScenarioGroup)),
    msg = "All `scenarios` must exist in df$ScenarioGroup."
  )
  
  # update vars if NULL --------------------------
  if (is.null(vars)) {
    vars <- unique(df$Variable)
  } else {
    assert_that(
      all(vars %in% df$Variable),
      msg = "All specified `vars` must exist in `df$Variable`."
    )
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
  
  n_yrs <- length(years)
  
  df <- df %>%
    dplyr::filter(Variable %in% vars, ScenarioGroup %in% scenarios) %>%
    dplyr::group_by(ScenarioGroup, Year, Variable) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    dplyr::mutate(Value = dplyr::if_else(Value == 0, NA_real_, Value * 100)) %>%
    dplyr::mutate(
      val_lab = paste0(formatC(Value, digits=0, format = "f"), "%"),
      val_lab = dplyr::if_else(val_lab == "NA%", "", val_lab),
      val_lab = dplyr::if_else(val_lab == "0%", "<1%", val_lab),
      Variable = factor(Variable, levels = rev(vars))
    )
  
  # get year labels. Since it is discrete have to treat differently than other
  # plots and assign empty string to years we don't want labled.
  myLabs <- get_year_breaks(years)
  
  yr_lab <- years
  names(yr_lab) <- years
  
  for (yy in years) {
    if (!(yy %in% myLabs)) {
      yr_lab[as.character(yy)] <- ''
    }
  }
  
  ops <- list(...)
  # these are the plotting options this function can handle
  exp_ops <- c("y_lab", "title", "subtitle", "caption", "color_label", 
               "legend_wrap", 
               "facet_scales", "facet_nrow", "facet_ncol")
  
  check_options(names(ops), exp_ops)
  if (!exists("color_label", where = ops)) {
    ops[["color_label"]] <- "%"
  }
  if (!exists("facet_scales", where = ops)) {
    ops[["facet_scales"]] <- "fixed"
  }
  
  if (!exists("y_lab", where = ops)) {
    ops[["y_lab"]] <- NULL
  }
  
  if (!is.null(ops$legend_wrap)) {
    df <- df %>%
      mutate(
        Variable = stringr::str_wrap(Variable, width = ops$legend_wrap),
        Variable = factor(
          Variable, 
          levels = rev(stringr::str_wrap(vars, width = ops$legend_wrap))
        )
      )
    
    if (!is(var_labels, "waiver")) {
      tmp <- stringr::str_wrap(names(var_labels), width = ops$legend_wrap)
      var_labels <- stringr::str_wrap(var_labels, width = ops$legend_wrap)
      names(var_labels) <- tmp
    }
  }
  
  # plot --------------------------------------------
  gg <- ggplot(df, aes(as.factor(Year), Variable, fill = Value)) +
    geom_tile() +
    # # from https://uigradients.com/#HoneyDew
    # scale_fill_gradient(
    #   low = "#F8FFAE", 
    #   high = "#43C6AC", 
    #   na.value = "grey90", 
    #   trans = "sqrt"
    # ) + 
    scale_fill_gradient(
      low = "#ffffff",
      high = "#006699",
      na.value = "grey90",
      trans = "sqrt"
    ) +
    geom_vline(xintercept = seq(1.5, n_yrs, 1), color = "white", size = 1) +
    geom_hline(
      yintercept = seq(0.5, length(vars) + 0.5), 
      color = "white", size = 1
    ) +
    geom_text(aes(label = val_lab), size = 3, color = "black") +
    scale_y_discrete(labels = var_labels) +
    scale_x_discrete(labels = yr_lab) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(color = "grey30", margin = margin(r = 12)),
      strip.text = element_text(size = 12),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines"),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    labs(
      y = ops$y_lab, title = ops$title, caption = ops$caption, 
      subtitle = ops$subtitle, x = NULL, fill = ops$color_label
    )
  
  if (length(scenarios) > 1) {
    gg <- gg + 
      facet_wrap(
        ~ScenarioGroup, 
        scales = ops$facet_scales, 
        nrow = ops$facet_nrow, 
        ncol = ops$facet_ncol,
        strip.position = "top",
        labeller = label_wrap_gen()
      )
  }
  
  gg
}