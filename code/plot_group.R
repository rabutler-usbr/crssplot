#' creates a `plot_group` object from a list `x`. `x` must have certain entries
#' otherwise the conversion will fail
#' 
#' Required elements in x: scenarios
#' Will fill in: scen_names, plot_colors, years, caption
#' Will create std_comparison, csd_ann, heat, cloud plot types and plots entry
plot_group <- function(x, defaults)
{
  assert_that("scenarios" %in% names(x))
  
  validate_plot_group(new_plot_group(x, defaults))
}

validate_plot_group <- function(x)
{
  valid_names <- c("scenarios", "scen_names", "plot_colors", "years", "caption",
                   "std_comparison", "csd_ann", "heat", "cloud", "plots")
  assert_that(all(names(x) %in% valid_names))
  assert_that(all(valid_names %in% names(x)))
  
  x
}

new_plot_group <- function(x, defaults)
{
  # set top level plot options
  pg <- list(
    scenarios = x[["scenarios"]],
    scen_names = get_pg_scen_names(x),
    plot_colors = get_pg_plot_colors(x),
    years = get_pg_years(x, defaults),
    caption = get_pg_caption(x)
  )
  
  # TODO: update these to their specific classes
  # go through the different plot types; for now just store them as is
  
  plot_types <- c("std_comparison", "csd_ann", "heat", "cloud")
  
  if (exists("plots", where = x)) {
    # plots are specified as sequence. Create these plots based on defaults
    create_plots <- x[["plots"]]
    
    for (pt in plot_types) {
      if (pt %in% create_plots) {
        pg[[pt]] <- list(create = TRUE)
      } else {
        pg[[pt]] <- list(create = FALSE)
      }
    }
  } else {
    # check if each plot type exists. Create it if it does. Set to FALSE if 
    # it doesn't
    create_plots <- c()
    for (pt in plot_types) {
      
      if (exists(pt, where = x)) {
        pg[[pt]] <- x[[pt]]
        create_plots <- c(create_plots, pt)
      } else {
        pg[[pt]] <- list(create = FALSE)
      }
    }
  }
  
  pg[["plots"]] <- create_plots
  
  class(pg) <- "plot_group"
  
  pg
}

get_pg_scen_names <- function(x)
{
  if (is.null(x[["scen_names"]])) {
    scen_names <- x[["scenarios"]]
    names(scen_names) <- x[["scenarios"]]
  } else {
    scen_names <- x[["scen_names"]]
    names(scen_names) <- x[["scenarios"]]
  }
  
  scen_names
}

get_pg_plot_colors <- function(x)
{
  if (is.null(x[["plot_colors"]])) {
    plot_colors <- scales::hue_pal()(length(x[["scenarios"]]))
    names(plot_colors) <- x[["scenarios"]]
  } else {
    plot_colors <- x[["plot_colors"]]
    names(plot_colors) <- x[["scenarios"]]
  }
  
  plot_colors
}

get_pg_years <- function(x, defaults)
{
  if (is.null(x[["years"]])) {
    years <- defaults[["plot_years"]]
  } else {
    years <- x[["years"]][1]:x[["years"]][2]
  }
  
  years
}

get_pg_caption <- function(x)
{
  x[["caption"]]
}
