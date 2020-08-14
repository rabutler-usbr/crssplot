
#' Plot scenario comparison figures
#' 
#' `scen_plot_range()` plots the range of results for multiple scenarios. The
#' range is shown as lines for 10th, 50th, and 90th percentiles. Typically 
#' this is done for only one variable, but multiple variables can be provided 
#' and will be shown as separate facets. 
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
#' `scen_plot_range()` will use the following additional options: "y", "title", 
#' "color_label", "legend_wrap", "facet_scales", "facet_nrow", and "facet_ncol".
#' 
#' The legend order can be modified by converting "ScenarioGroup" column to a 
#' factor before calling `scen_plot_range()`, with the levels specifying the
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
#' @examples
#' # quick simple plot of one variable
#' scens_plot_range(ex_pe, "mead_dec_pe"))
#' 
#' # now add ylabel 
#' scens_plot_range(ex_pe, "mead_dec_pe", y_lab = "feet")
#' 
#' # show two variables
#' scens_plot_range(
#'   ex_pe, 
#'   c("powell_dec_pe", "mead_dec_pe"), 
#'   facet_scales = "free_y"
#' )
#' 
#' # subset scenarios
#' scens_plot_range(ex_pe, "mead_dec_pe", scenarios = "April ST CT"))
#' 
#' # custom colors and scenario labels + add title and caption
#' pc <- c("April ST CT" = "red", "April ST 2007 UCRC" = "black")
#' sl <- c("April ST CT" = "s1", "April ST 2007 UCRC" = "s2")
#' scens_plot_range(ex_pe, 
#'   "powell_dec_pe", 
#'   plot_colors = pc, 
#'   scen_labels = sl,
#'   title = "PE", 
#'   caption = "this is a caption")
#' )
#' 
#' # change to two rows in stead of two columns for showing two variables
#' scens_plot_range(
#'   ex_pe, 
#'   c("powell_dec_pe", "mead_dec_pe"), 
#'   facet_scales = "free_y",
#'   facet_nrow = 2)
#' )
#' 
#' @rdname scens_plot_
#' @export
scens_plot_range <- function(df, vars, years = NULL, scenarios = NULL, 
                             plot_colors = NULL, scen_labels = NULL, ...) {
  # TODO: in the future, add in argument for the range that is plotted. 
  # right now, it is only 10/50/90
 
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
      '50th' = median(Value), 
      '10th' = stats::quantile(Value, .1), 
      '90th' = stats::quantile(Value, .9)
    ) %>%
    ungroup() %>%
    tidyr::pivot_longer(
      c("50th", "10th", "90th"), 
      names_to = "Percentile", 
      values_to = "Value"
    )
  
  # parse ... and other plot options
  plot_colors <- determine_plot_colors(plot_colors, scenarios)
  
  qLt <- c(2, 1, 3)
  names(qLt) <- c('90th','50th','10th')
  
  df <- mutate(df, Percentile = factor(Percentile, levels = names(qLt)))
  
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
        ScenarioGroup, 
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
    aes(Year, Value, color = ScenarioGroup, linetype = Percentile)
  ) +
    geom_line(size = 1) + 
    scale_x_continuous(
      breaks = myLabs,
      minor_breaks = 1900:3000, 
      labels = myLabs
    ) + 
    scale_y_continuous(labels = scales::comma) +
    labs(y = ops$y_lab, title = ops$title, caption = ops$caption) +
    scale_color_manual(
      values = plot_colors, 
      guide = guide_legend(title = ops$color_label),
      labels = scen_labels
    ) +
    scale_linetype_manual(values = qLt) +
    theme_crss()
  
  if (length(vars) > 1) {
    gg <- gg + 
      facet_wrap(~Variable, scales = ops$facet_scales, nrow = ops$facet_nrow, 
                 ncol = ops$facet_ncol)
  }
  
  gg
}

# ensures the data frame has all the required columns
check_required_columns <- function(df, req_cols) {
  call_fxn <- deparse(sys.calls()[[sys.nframe() - 1]])
  
  assert_that(
    all(req_cols %in% names(df)), 
    msg = paste("`df` in", call_fxn, "does not contain required columns:\n",
                paste(req_cols[!(req_cols %in% names(df))], collapse = ", "))
  )
  
  invisible(df)
}

check_options <- function(x, available_ops) {
  call_fxn <- deparse(sys.calls()[[sys.nframe() - 1]])
  
  if (any(!(x %in% available_ops))) {
    tmp <- x[!(x %in% available_ops)]
    
    warning(
      paste(
        "In", call_fxn, paste(tmp, collape = ", "), 
        "are passed as plot options, but are not handled in that function."
      )
    )
  }
  
  invisible(x)
}
