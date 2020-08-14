#' @description 
#' `scen_plot_cloud()` plots the range of results for multiple scenarios. The
#' range is shown as a shaded region (cloud) extending from the 10th to 90th 
#' percentiles, along with a solid line for the median. Typically 
#' this is done for only one variable, but multiple variables can be provided 
#' and will be shown as separate facets. 
#' 
#' @param historical Data frame of historical data to add to the figure. Must
#'   have a "Year" column, and the same number of additional columns as the 
#'   length of `vars`. If there is only one variable, then the column names in 
#'   this data frame do not matter (except for Year). However, if there are 
#'   more than one variable, then the column names must match those in `vars`.
#' 
#' @rdname scens_plot_
#' @export
scens_plot_cloud <- function(df, vars, historical = NULL, years = NULL, 
                             scenarios = NULL, plot_colors = NULL, 
                             scen_labels = NULL, ...) {
  # check df -------------------------------
  crssplot:::check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value"))
  
  # check historical -----------------------
  if (!is.null(historical)) {
    req_cols <- "Year"
    if (length(vars) > 1) {
      req_cols <- c(req_cols, vars)
    }
    
    crssplot:::check_required_columns(historical, req_cols)
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
    df <- bind_rows(df, historical)
  }
  
  test_cols <- c("Historical" = "black", "April ST 2007 UCRC" = "red", 
                 "April ST CT" = "blue")
  
  ggplot(df, aes(Year)) +
    geom_line(aes(y = middle, color = ScenarioGroup)) +
    geom_ribbon(data = filter(df, ScenarioGroup != "Historical"),
                aes(ymin = bottom, ymax = top, fill = ScenarioGroup)) +
    scale_fill_manual(values = test_cols) +
    scale_color_manual(values = test_cols) +
    labs(
      fill = "10th to 90th\npercentile of full\nrange",
      color = "Historical and\nmedian projected\npool elevation"
    )
}
