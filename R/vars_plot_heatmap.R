vars_plot_heatmap <- function(df, scenarios,  years = NULL, vars = NULL, 
                              var_labels = NULL, ...) {
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
  
  df <- df %>%
    dplyr::filter(Variable %in% vars, ScenarioGroup %in% scenarios) %>%
    dplyr::group_by(ScenarioGroup, Year, Variable) %>%
    dplyr::summarise(Value = mean(Value))
  
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
  gg <- ggplot(df, aes(as.factor(Year), Variable, fill = Value)) +
    facet_wrap(
      ~Agg, 
      nrow = 1, 
      strip.position = "top", 
      labeller = label_wrap_gen()
    ) +
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
      yintercept = seq(0.5, length(tier_names) + 0.5), 
      color = "white", size = 1
    ) +
    geom_text(aes(label = val_lab), size = 3, color = "black") +
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
      y = y_title, 
      x = NULL, fill = "%", title = my_title,
      subtitle = "Percent of Traces in each Elevation Range",
      caption = heat_ui[['caption']]
    )
  
  gg
}