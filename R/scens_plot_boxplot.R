#' @description 
#' `scens_plot_boxplot()` is similar to `scens_plot_range()` and 
#' `scens_plot_cloud()`, except it uses boxplots to show the range for each 
#' year. The boxplot are based on [CRSSIO::stat_boxplot_custom()], so whiskers
#' extend to 5th and 95th percentiles, and outliers are shown for points beyond
#' those percentiles.
#' 
#' @examples 
#' # quick simple plot using 1 variable and all years
#' scens_plot_boxplot(ex_pe, vars = "powell_dec_pe")
#' 
#' # fully customized for multiple variables, custom colors, and custom names
#' tst_names <- c("April ST 2007 UCRC" = "Scen 1", "April ST CT" = "Scen 2")
#' pal <- c("April ST 2007 UCRC" = "#fc8d62", "April ST CT" = "#8da0cb")
#' scens_plot_boxplot(
#'   ex_pe, 
#'   vars = c("powell_dec_pe", "mead_dec_pe"), 
#'   years = 2021:2036,
#'   title = "Mead and Powell", subtitle = "End-of-December Elevation",
#'   y_lab = "(feet)", caption = "Results from April 20xx",
#'   facet_scales = "free_y", 
#'   plot_colors = pal,
#'   scen_labels = tst_names,
#'   legend_wrap = 10
#' )
#'
#' @rdname scens_plot_
#' @export
scens_plot_boxplot <- function(df, vars, years = NULL, scenarios = NULL, 
                             plot_colors = NULL, scen_labels = NULL, ...) {
  # check df -------------------------------
  check_required_columns(df, c("Year", "Variable", "ScenarioGroup", "Value"))
  
  assert_that(
    all(vars %in% df$Variable), 
    msg = "All specified `vars` must exist in `df$Variable`"
  )
  
  # update scenarios if NULL --------------------------
  if (is.null(scenarios)) {
    scenarios <- unique(df$ScenarioGroup)
  }
  
  # scen_labels ---------------------------------------
  if (is.null(scen_labels)) {
    scen_labels <- waiver()
  }
  
  # years  -----------------------------------
  if (!is.null(years)) {
    df <- filter(df, Year %in% years)
  } else {
    years <- unique(df$Year)
  }
  
  # filter down ------------------------------
  df <- df %>%
    dplyr::filter(Variable %in% vars, ScenarioGroup %in% scenarios)
  
  # parse ... and other plot options -------------------------
  plot_colors <- determine_plot_colors(plot_colors, scenarios)
  
  myLabs <- get_year_breaks(years)
  ll <- rep("", length(years))
  ll[years %in% myLabs] <- years[years %in% myLabs]
  
  ops <- list(...)
  # these are the plotting options this function can handle
  exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
               "facet_scales", "facet_nrow", "facet_ncol", "subtitle")
  
  check_options(names(ops), exp_ops)
  if (!exists("color_label", where = ops)) {
    ops[["color_label"]] <- "Scenario"
  }
  if (!exists("facet_scales", where = ops)) {
    ops[["facet_scales"]] <- "fixed"
  }
  
  if (!is.null(ops$legend_wrap)) {
    df <- df %>%
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
  
  # plot --------------------------
  gg <- df %>% 
    ggplot(aes(as.factor(Year), Value, fill = ScenarioGroup)) +
    CRSSIO::stat_boxplot_custom() +
    scale_fill_manual(
      values = plot_colors,
      guide = guide_legend(title = ops$color_label),
      labels = scen_labels
    ) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
    scale_x_discrete(breaks = years, labels = ll) +
    labs(y = ops$y_lab, title = ops$title, caption = ops$caption, x = NULL, 
         subtitle = ops$subtitle) +
    theme_crss()
  
  if (length(vars) > 1) {
    gg <- gg + 
      facet_wrap(~Variable, scales = ops$facet_scales, nrow = ops$facet_nrow, 
                 ncol = ops$facet_ncol)
  }
  
  gg
}
