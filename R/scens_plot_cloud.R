#' @description 
#' `scens_plot_cloud()` plots the range of results for multiple scenarios. The
#' range is shown as a shaded region (cloud) extending from the 10th to 90th 
#' percentiles, along with a solid line for the median. Typically 
#' this is done for only one variable, but multiple variables can be provided 
#' and will be shown as separate facets. 
#' 
#' Additionally, `scens_plot_cloud()` can use the "fill_label" option. 
#' 
#' @param historical Data frame of historical data to add to the figure. Must
#'   have a "Year" column, and the same number of additional columns as the 
#'   length of `vars`. If there is only one variable, then the column names in 
#'   this data frame do not matter (except for Year). However, if there are 
#'   more than one variable, then the column names must match those in `vars`.
#'   
#' @param connect_historical If `historical` data are provided, and this is 
#'   `TRUE`, then the historical year prior to the first projected year of data
#'   will be "connected" to the projections so as to provide a connected look 
#'   to the plot. If `FALSE`, then projected data are shown as they natively 
#'   exist in `df` and if `historical` data are not provided this has no affect
#'   on the output.
#' 
#' @rdname scens_plot_
#' @export
scens_plot_cloud <- function(df, vars, historical = NULL, years = NULL, 
                             scenarios = NULL, plot_colors = NULL, 
                             scen_labels = NULL, connect_historical = TRUE, ...) {
  # check df -------------------------------
  check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value"))
  
  assert_that(
    all(vars %in% df$Variable), 
    msg = "All specified `vars` must exist in `df$Variable`"
  )
  
  # check historical -----------------------
  if (!is.null(historical)) {
    req_cols <- "Year"
    if (length(vars) > 1) {
      req_cols <- c(req_cols, vars)
    }
    
    check_required_columns(historical, req_cols)
    assert_that(
      ncol(historical) == length(vars) + 1, 
      msg = "`historical` should have the same number of columns as the length of `vars` + 1."
    )

    if (length(vars) == 1) {
      colnames(historical)[2] <- vars
    }
    
    historical <- tidyr::pivot_longer(
      historical, -Year, names_to = "Variable", values_to = "middle"
    )
    historical$ScenarioGroup <- "Historical"
  }
  
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
      'middle' = median(Value), 
      'bottom' = stats::quantile(Value, .1), 
      'top' = stats::quantile(Value, .9)
    )

  if (!is.null(historical)) {
    if (isTRUE(connect_historical)) {
      # repeat the year before projections start in the projection data
      last_year <- min(df$Year) - 1
      h_tmp <- dplyr::filter(historical, Year == last_year)
      h_tmp$ScenarioGroup <- NULL
      
      # repeat these data for bottom and top columns
      h_tmp$bottom <- h_tmp$middle
      h_tmp$top <- h_tmp$middle
      
      # repeat these data for each Scenario Group
      tmpsg <- unique(df$ScenarioGroup)
      h_tmp <- dplyr::bind_rows(lapply(tmpsg, function(i) {
        tmp <- h_tmp
        tmp$ScenarioGroup <- i
        tmp
      }))
      df <- bind_rows(h_tmp, df)
    }
    
    df <- bind_rows(df, historical)
  }
  
  years <- unique(df$Year)
  
  # plot options ----------------------------------------------
  
  if ("Historical" %in% names(plot_colors)) {
    plot_colors <- determine_plot_colors(plot_colors, c(scenarios, "Historical"))
  } else {
    plot_colors <- determine_plot_colors(plot_colors, scenarios)
    plot_colors <- c(plot_colors, "Historical" = "black")
  }
  
  ops <- list(...)
  # these are the plotting options this function can handle
  exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
               "facet_scales", "facet_nrow", "facet_ncol", "fill_label", 
               "subtitle")
  
  check_options(names(ops), exp_ops)
 
  if (!exists("facet_scales", where = ops)) {
    ops[["facet_scales"]] <- "fixed"
  }
  
  if (exists("legend_wrap", where = ops)) {
    tmp_width <- ops[["legend_wrap"]]
  } else {
    tmp_width <- 1000
  }
  
  # change scenario names, and the plot colors to wrap
  all_scens <- unique(df$ScenarioGroup)
  df$ScenarioGroup <- stringr::str_wrap(df$ScenarioGroup, tmp_width)
  names(plot_colors) <- stringr::str_wrap(names(plot_colors), tmp_width)
  
  # ensure "Historical" shows up last in the legend
  if ("Historical" %in% all_scens) {
    all_scens <- c(all_scens[all_scens != "Historical"], "Historical")
    all_scens <- stringr::str_wrap(all_scens, tmp_width)
    df$ScenarioGroup <- factor(df$ScenarioGroup, levels = all_scens)
  }
  
  if (!exists("color_label", where = ops)) {
    ops[["color_label"]] <- stringr::str_wrap(
      "Historical and median projections",
      tmp_width
    )
  } else {
    ops[["color_label"]] <- stringr::str_wrap(ops[["color_label"]], tmp_width)
  }
  
  if (!exists("fill_label", where = ops)) {
    ops[["fill_label"]] <- stringr::str_wrap(
      "10th to 90th percentile of full range", 
      tmp_width
    )
  } else {
    ops[["fill_label"]] <- stringr::str_wrap(ops[["fill_label"]], tmp_width)
  }
  
  myLabs <-  get_year_breaks(years)
  
  gg <- ggplot(df, aes(Year)) +
    geom_ribbon(data = filter(df, ScenarioGroup != "Historical"),
                aes(ymin = bottom, ymax = top, fill = ScenarioGroup), 
                alpha = 0.5, linetype = 2, size = 0.5) +
    geom_line(aes(y = middle, color = ScenarioGroup), size = 1) +
    scale_fill_manual(
      values = plot_colors,
      guide = guide_legend(title = ops$fill_label),
      labels = scen_labels
    ) +
    scale_color_manual(
      values = plot_colors,
      guide = guide_legend(title = ops$color_label),
      labels = scen_labels
    ) +
    labs(y = ops[["y_lab"]], title = ops[["title"]], 
         subtitle = ops[['subtitle']], x = NULL, caption = ops[['caption']],
         color = ops[['color_label']]) +
    scale_x_continuous(
      breaks = myLabs,
      minor_breaks = 1900:3000, 
      labels = myLabs,
      expand = c(0,0)
    ) + 
    scale_y_continuous(labels = scales::comma)
  
  if (length(vars) > 1) {
    gg <- gg + 
      facet_wrap(~Variable, scales = ops$facet_scales, nrow = ops$facet_nrow, 
                 ncol = ops$facet_ncol)
  }
  
  gg
}
