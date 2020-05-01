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
  
  # TODO: remove the "plot_scenarios" entry after we are done converting to 
  # only the yaml based structure.
  pg <- structure(
    list(
      scenarios = x[["scenarios"]],
      plot_scenarios = x[["scenarios"]],
      scen_names = get_pg_scen_names(x),
      plot_colors = get_pg_plot_colors(x),
      years = get_pg_years(x, defaults),
      caption = get_pg_caption(x),
      std_comparison = new_plot_spec(FALSE),
      csd_ann = new_plot_spec(FALSE),
      heat = new_plot_spec(FALSE),
      cloud = new_plot_spec(FALSE)
    ),
    class = "plot_group"
  )
  
  # TODO: update these to their specific classes
  # go through the different plot types; for now just store them as is
  
  plot_types <- c("std_comparison", "csd_ann", "heat", "cloud")
 
  if (exists("plots", where = x)) {
    # plots are specified as sequence. Create these plots based on defaults
    create_plots <- x[["plots"]]
    
    
    if ("std_comparison" %in% create_plots) 
      pg[["std_comparison"]] <- new_plot_spec(TRUE)
    
    if ("csd_ann" %in% create_plots)
      pg[["csd_ann"]] <- new_plot_spec(TRUE)
    
    if ("heat" %in% create_plots)
      pg[["heat"]] <- heat_spec(x, pg)
    
    if ("cloud" %in% create_plots)
      pg[["cloud"]] <- new_plot_spec(TRUE)
   
  } else {
    # check if each plot type exists. Create it if it does. Set to FALSE if 
    # it doesn't
    
    # TODO: update to call correct functions for everything except heat
    
    if (exists("std_comparison", where = x))
      pg[["std_comparison"]] <- new_plot_spec(TRUE)
    
    if (exists("csd_ann", where = x))
      pg[["csd_ann"]] <- new_plot_spec(TRUE)
    
    if (exists("heat", where = x))
      pg[["heat"]] <- heat_spec(x, pg)
    
    if (exists("cloud", where = x))
      pg[["cloud"]] <- new_plot_spec(TRUE)
    
    create_plots <- c()
    for (pt in plot_types) {
      if (pg[[pt]][["create"]]) 
        create_plots <- c(create_plots, pt)
    }
  
  }
  
  pg[["plots"]] <- create_plots
  
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
