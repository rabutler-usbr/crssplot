#' plot_spec is a generic class for plot specifications
plot_spec <- function(x, plot_type, pg)
{
  # if plot_type does not exist, set create to FALSE and exit
  # if plot_type exists, and create is set to FALSE, then set that and exit
  # otherwise, check for years, inerit from pg, and then exit
  
  if (!exists(plot_type, pg) || isFALSE(x[[plot_type]][["create"]])) {
    ps <- new_plot_spec(FALSE)
  } else {
    if (is.null(x[[plot_type]][["years"]])) {
      years <- pg[["years"]]
    } else {
      years <- get_years_att(x[[plot_type]])
    }
    
    ps <- new_plot_spec(TRUE)
    ps[["years"]] <- years
  }
  
  ps
}

new_plot_spec <- function(create)
{
  structure(list(create = create), class = "plot_spec")
}

#' heat_spec is a class for specifying the heatmap plots
heat_spec <- function(ui, pg)
{
  x <- plot_spec(ui, "heat", pg) 
  
  if (x[["create"]]) {
  
    # scen_names and caption, inherit from pg if not specified
    x <- x %>%
      inherit_from_pg("scenarios", ui, pg) %>%
      inherit_from_pg("scen_names", ui, pg) %>%
      inherit_from_pg("caption", ui, pg)
  
    # title: defaults to ''
    if (exists("title", ui)) {
      x[["title"]] <- ui[["title"]]
    } else {
      x[["title"]] <- ""
    }
  }

  class(x) <- c("heat_spec", class(x))
  
  x
}

#' sets the specified `att` in `x`. Checks to see if it exists in `ui`. If it
#' does not, then defaults to `att` in `pg`.
inherit_from_pg <- function(x, att, ui, pg)
{
  assert_that(att %in% c("scen_names", "caption", "years", "scenarios", 
                         "plot_colors"))
  
  if (!exists(att, where = ui)) {
    x[[att]] <- pg[[att]]
  } else {
    x[[att]] <- switch(
      att,
      "scen_names" = get_scen_names_att(ui, x),
      "caption" = ui[["caption"]],
      "years" = get_years_att(ui),
      "scenarios" = pg[["scenarios"]],
      "title" = ui[["title"]]
    )
  }
  
  x
}

get_scen_names_att <- function(ui, ps)
{
  att <- ui[["scen_names"]]
  names(att) <- ps[["scenarios"]]
  att
}

get_years_att <- function(ui)
{
  ui[["years"]][1]:ui[["years"]][2]
}

get_plot_colors_att <- function(ui, ps)
{
  att <- ui[["plot_colors"]]
  names(att) <- ps[["scenarios"]]
  att
}
