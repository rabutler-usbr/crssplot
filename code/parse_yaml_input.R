library(yaml)
#library(fs) # for is_absolute_path()
library(assertthat)

parse_yaml_input <- function(file)
{
  zz <- yaml.load_file(file)
  
  assert_that(all(required_entries("top") %in% names(zz)))
  
  zz <- zz %>%
    set_defaults() %>%
    set_process_data() %>%
    set_folders()
  
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
  
  if (level == "top") {
    rv <- c("process_data", "folders", "scenarios")
  } else if (level == "folders") {
    rv <- c("i_folder", "CRSSDIR", "crss_month", "pdf_name")
  } else if (level == "process_data") {
    rv <- c("sys_cond_data", "pe_data", "csd_dat", "crss_short_cond_data")
  }
  
  rv
}

# checks the `process_data` entry. It should either be a boolean or a list of
# entries (incomplete is ok). Creates the list expected by process_everything
set_process_data <- function(ui) 
{
  # process_data <- list(
  #   sys_cond_data = TRUE,
  #   pe_data = TRUE,
  #   csd_data = FALSE,
  #   crss_short_cond_data = FALSE
  # )

  process_data <- ui[["process_data"]]
  
  if (is.list(process_data)) {
    # check each entry and make sure its a boolean and if it doesn't exist
    # then set it to false 
    pd_entries <- required_entries("process_data")
    
    for (pde in pd_entries) {
      if (exists(pde, where = process_data)) {
        assert_that(
          is.logical(process_data[[pde]]) && length(process_data[[pde]]) == 1,
          msg = paste0(
            "Each entry in process_data should be a boolean of length=1\n",
            pde, " is not."
          )
        )
      } else {
        process_data[[pde]] <- FALSE
      }
    }
    
  } else {
    # should be a boolean length 1 
    assert_that(
      is.logical(process_data) && length(process_data) == 1,
      msg = "if process_data is not a list, it should be a boolean of length=1"
    )
    
    tmp <- process_data
    process_data <- list(
      sys_cond_data = tmp,
      pe_data = tmp,
      csd_data = tmp,
      crss_short_cond_data = tmp
    )
  }
  
  ui[["process_data"]] <- process_data
  
  ui
}

set_folders <- function(ui)
{
  # check that all required sequences exist
  req_folders <- required_entries("folders")
  assert_that(all(req_folders %in% names(ui[["folders"]])))
  
  # check for optional entries - only "extra_label" is optional
  if (!exists("extra_label", where = ui[["folders"]])) {
    ui[["folders"]][["extra_label"]] <- ''
  }
  
  # all required sequences and extra_label should be strings of length 1
  for (rf in c(req_folders, "extra_label")) {
    tmp <- ui[["folders"]][[rf]]
    assert_that(
      is.character(tmp) && length(tmp) == 1,
      msg = paste0("User input for folders-", rf, " should be a single string.")
    )
  }
  
  ui
}
