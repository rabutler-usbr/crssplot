#' plot_spec is a generic class for plot specifications
#' @noRd
plot_spec <- function(x, plot_type, pg)
{
  # if plot_type does not exist, set create to FALSE and exit
  # if plot_type exists, and create is set to FALSE, then set that and exit
  # otherwise, check for years, inerit from pg, and then exit
 
  if (should_create(x, plot_type)) {
    if (is.list(x[[plot_type]]) && exists("years", x[[plot_type]])) {
      years <- get_years_att(x[[plot_type]])
    } else {
      years <- pg[["years"]]
    }
    
    ps <- new_plot_spec(TRUE)
    ps[["years"]] <- years
  } else {
    ps <- new_plot_spec(FALSE)
  }
  
  ps
}

new_plot_spec <- function(create)
{
  structure(list(create = create), class = "plot_spec")
}

#' std_comp_spec is a class for specifying the standard comparison plots
#' @noRd
std_comp_spec <- function(ui, pg)
{
  x <- plot_spec(ui, "std_comparison", pg)
  
  x <- inherit_plot_specs(x, ui, pg, "std_comparison")
  
  class(x) <- c("std_comp_spec", class(x))
  
  x
}

#' heat_spec is a class for specifying the heatmap plots
#' @noRd
heat_spec <- function(ui, pg)
{
  x <- plot_spec(ui, "heat", pg) 

  x <- inherit_plot_specs(x, ui, pg, "heat")

  class(x) <- c("heat_spec", class(x))
  
  x
}

#' cloud_spec is a class for specifying the cloud plots
#' @noRd
cloud_spec <- function(ui, pg)
{
  x <- plot_spec(ui, "cloud", pg) 
  
  x <- inherit_plot_specs(x, ui, pg, "cloud")
  
  # set attributes that do not exist in other plot types. For cloud, this is 
  # title_append
  x[["title_append"]] <- ui[["cloud"]][["title_append"]]
  
  class(x) <- c("cloud_spec", class(x))
  
  x
}

#' csd_spec is a class for specifying the annual computed state depletions plots
#' @noRd
csd_spec <- function(ui, pg)
{
  x <- plot_spec(ui, "csd_ann", pg) 
  
  x <- inherit_plot_specs(x, ui, pg, "csd_ann")
  
  class(x) <- c("csd_spec", class(x))
  
  x
}

inherit_plot_specs <- function(x, ui, pg, plot_type)
{
  if (x[["create"]]) {
    
    # scen_names and caption, inherit from pg if not specified
    x <- x %>%
      inherit_from_pg("scenarios", ui, pg, plot_type) %>%
      inherit_from_pg("scen_names", ui, pg, plot_type) %>%
      inherit_from_pg("caption", ui, pg, plot_type) %>%
      inherit_from_pg("plot_colors", ui, pg, plot_type)
    
    # title: defaults to ''
    if (is_att_specified(ui[[plot_type]], "title")) {
      x[["title"]] <- ui[[plot_type]][["title"]]
    } else {
      x[["title"]] <- ""
    }
  }
  
  x
}

#' sets the specified `att` in `x`. Checks to see if it exists in `ui`. If it
#' does not, then defaults to `att` in `pg`.
#' @noRd
inherit_from_pg <- function(x, att, ui, pg, plot_type)
{
  assert_that(att %in% c("scen_names", "caption", "years", "scenarios", 
                         "plot_colors"))

  if (is_att_specified(ui[[plot_type]], att)) {
    x[[att]] <- switch(
      att,
      "scen_names" = get_scen_names_att(ui[[plot_type]], pg),
      "caption" = ui[[plot_type]][["caption"]],
      "years" = get_years_att(ui),
      "scenarios" = pg[["scenarios"]],
      "plot_colors" = get_plot_colors_att(ui[[plot_type]], pg)
    )
  } else {
    x[[att]] <- pg[[att]]
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

# the plot specifications that come in from yaml can typically be sequences 
# lists, or booleans, or not exist. This will return TRUE/FALSE based on 
# checking the three different possibilities of data type, and if it does exist
# will determine if the user has specified that it should be created
should_create <- function(pg, plot_type)
{
  # the pg can be specified as individual plot names, or as a entry called 
  # plots. First check to see if "plots" exists. Otherwise, check to see if the 
  # plto type exists and go from there. 
  if (exists("plots", pg)) {
    return(plot_type %in% pg[["plots"]])
  } else {
    # 1 - does the specified plot type exist at all
    if (!exists(plot_type, pg)) {
      return(FALSE)
    } else{
      if (is.list(pg[[plot_type]])) {
        # if it is a list, then assume that the user wants it created, unless, 
        # create is set to FALSE
        if (exists("create", pg[[plot_type]]))
          return(isTRUE(pg[[plot_type]][["create"]]))
        else
          return(TRUE)
      } else {
        # it should be a boolean scalar
        return(isTRUE(pg[[plot_type]]))
      }
    }
  }
}

is_att_specified <- function(ui, att)
{
  if (is.list(ui)) 
    return(exists(att, where = ui))
  else
    return(FALSE)
}
