library(yaml)
#library(fs) # for is_absolute_path()
library(assertthat)

parse_yaml_input <- function(file)
{
  zz <- yaml.load_file(file)
  
  assert_that(all(required_entries(1) %in% names(zz)))
  
  zz <- zz %>%
    set_defaults()
  
  zz
}

#' @param ui User input from yaml file as a list
set_defaults <- function(ui) 
{
  if (exists("defaults", where = ui)) {
    # loop through and set any unspecified defaults
    defaults <- get_global_defaults()
    for (d in names(defaults)) {
      if (!exists(d, where = ui[["defaults"]])) {
        ui[["defaults"]][[d]] <- defaults[[d]]
      }
    }
  } else {
    # set defaults to defaults
    ui[["defaults"]] <- get_global_defaults()
  }
  
  ui
}

get_global_defaults <- function() {
  list(
    # in the comma seperated scenario folder names, currently the 5th entry is  
    # the initial conditions entry
    # update if for some reason the scenario naming convention has changed
    ic_dim_number = 5,
    # setting to NULL will not wrap legend entries at all
    legend_wrap = 20,
    # how to label the color scale on the plots
    color_label = 'Scenario',
    # text that will be added to figures
    end_year = 2060,
    start_year = 2019
  )
}

required_entries <- function(level) {
  rv <- NULL
  
  if (level == 1) {
    rv <- c("process_data", "folders", "scenarios")
  }
  
  rv
}
