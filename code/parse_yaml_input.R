library(yaml)
#library(fs) # for is_absolute_path()
library(assertthat)
library(purrr)
source("code/plot_group.R")
source("code/plot_spec.R")

parse_yaml_input <- function(file)
{
  zz <- yaml.load_file(file)
  
  assert_that(all(required_entries("top") %in% names(zz)))

  zz <- zz %>%
    set_defaults() %>%
    set_process_data() %>%
    set_folders() %>%
    set_scenarios() %>%
    set_plot_groups()
  
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
  
  # TODO: also need to set the following
  # from old specification code:
  # # TODO: update so that these are computed if not specified
  # # years to show the crit stats figures  
  ui[["defaults"]][['plot_years']] <- 
    ui[["defaults"]][["start_year"]]:ui[["defaults"]][["end_year"]]
  # # years to show the Mead/Powell 10/50/90 figures for
  # defaults[['pe_yrs']] <- (defaults$start_year - 1):defaults$end_year
  
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
    rv <- c("i_folder", "crss_month", "pdf_name")
  } else if (level == "process_data") {
    rv <- c("sys_cond_data", "pe_data", "csd_dat", "crss_short_cond_data")
  } else if (level == "scenarios") {
    rv <- c("name", "folder", "ic", "start_year")
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
  
  # check for optional entries - only "extra_label" and CRSSDIR are optional
  if (!exists("extra_label", where = ui[["folders"]])) {
    ui[["folders"]][["extra_label"]] <- ''
  }
  
  if (!exists("CRSSDIR", where = ui[["folders"]])) {
    # if CRSSDIR is not specifed, then defaults to $CRSS_DIR, but will error if
    # than environment variable is not specified
    crss_dir <- Sys.getenv("CRSS_DIR")
    assert_that(
      crss_dir != "", 
      msg = "CRSS_DIR environment variable does not exist."
    )
    assert_that(
      dir.exists(crss_dir), 
      msg = "Folder specified by CRSS_DIR environment variable does not exist."
    )
    
    ui[["folders"]][["CRSSDIR"]] <- crss_dir
  }
  
  # all required sequences and extra_label should be strings of length 1
  for (rf in c(req_folders, "extra_label")) {
    tmp <- ui[["folders"]][[rf]]
    assert_that(
      is.character(tmp) && length(tmp) == 1,
      msg = paste0("User input for folders-", rf, " should be a single string.")
    )
  }
  
  # pdf_name should end in .pdf, if it does not, append .pdf to it.
  pdf_ext <- tools::file_ext(ui[["folders"]][["pdf_name"]])
  if (pdf_ext != "pdf")
    ui[["folders"]][["pdf_name"]] <- paste0(
      ui[["folders"]][["pdf_name"]], 
      ".pdf"
    )
  
  ui
}

# parse the secenarios entry and create scenarios$scens, scenarios$ic_list
# scenarios$ic_month, ind_plots, and scen_tree
set_scenarios <- function(ui)
{
  ss <- ui[["scenarios"]]
  assert_that(length(ss) >= 1)
  
  # all scenario names should be unique
  scen_names <- names(ss)
  check_unique_names(ss, "scenario")
  
  scens <- list()
  ic_list <- list()
  ic_month <- list()
  ind_plots <- list()
  scen_tree <- list()
  
  # loop through all scenarios 
  req_names <- required_entries("scenarios")
  
  all_scenarios <- c()
  std_ind_fig_ops <- list()
  
  for (cur_scen in ss) {
    # check that it has all required columns
    assert_that(all(req_names %in% names(cur_scen)))
    
    cur_scen <- check_scenarios_specification(cur_scen) %>%
      eval_scenarios_specification() %>%
      check_std_ind_figures(ui[["defaults"]]) %>%
      check_std_ind_tables()
    
    mead_pe_scatter <- check_mead_pe_scatter(cur_scen)
    short_conditions <- check_shortage_conditions(cur_scen)

    all_scenarios <- c(
      all_scenarios,
      create_scenario(
        cur_scen[["name"]],
        scen_folders = cur_scen[["folder"]],
        ic = cur_scen[["ic"]],
        start_year = cur_scen[["start_year"]],
        std_ind_tables = cur_scen[["std_ind_tables"]],
        std_ind_figures = cur_scen[["std_ind_figures"]][["create"]],
        mead_pe_scatter = mead_pe_scatter,
        shortage_conditions = short_conditions
      )
    )
    
    # if creating std figures, then update its options
    if (cur_scen[["std_ind_figures"]][["create"]]) {
      std_ind_fig_ops[[cur_scen[["name"]]]] <- list(
        ann_text = cur_scen[["std_ind_figures"]][["ann_text"]],
        end_year = cur_scen[["std_ind_figures"]][["end_year"]]
      )
    }
  }
 
  ui[["scenarios"]] <- scenario_to_vars(all_scenarios)
  ui[["ind_plots"]] <- specify_individual_plots(
    all_scenarios, 
    std_ind_fig_ops, 
    ui[["defaults"]]
  )
  ui[["scen_tree"]] <- all_scenarios
  
  ui
}

# checks a single scenario specification
check_scenarios_specification <- function(scen)
{
  assert_that(is.character(scen[["name"]]) && length(scen[["name"]]) == 1)
  assert_that(is.character(scen[["folder"]]))
  assert_that(is.numeric(scen[["start_year"]]) && 
                length(scen[["start_year"]]) == 1)
  
  if (is.list(scen[["ic"]])) {
    assert_that(all(names(scen[["ic"]]) %in% c("powell", "mead")))
  } else {
    assert_that(is.character(scen[["ic"]]) && length(scen[["ic"]]) == 1)
  }
  
  invisible(scen)
}

# checks if any of the values are r statements, and if they are, evaluates
# them. Also, if ic is explicitly specified for powell and mead, converts that
# to a vector (from list)
eval_scenarios_specification <- function(scen)
{
  # check name, folder, and ic
  scen <- eval_r_var(scen, "name") %>%
    eval_r_var("folder")
  
  if (is.list(scen[["ic"]])) {
    # convert to vector
    scen[["ic"]] <- c(scen[["ic"]][["powell"]], scen[["ic"]][["mead"]])
  } else {
    scen <- eval_r_var(scen, "ic")
  }
  
  scen
}

# Check to see if entry is an r statement, if it is, then it evaluates the r
# statment. Otherwise it does nothing. 
# We assume that the entry has already been checked to see if it is a character
# scalar, i.e., is_r_statement assumes that it is checking a string of length 
# == 1.
eval_r_var <- function(scen, entry)
{
  if (is_r_statement(scen[[entry]])) {
    # strip of `r and `
    tmp <- scen[[entry]] %>%
      strip_r_from_string() %>%
      parse(text = ., keep.source = FALSE) %>%
      eval()
    
    scen[[entry]] <- tmp
  }
  
  scen
}

strip_r_from_string <- function(x)
{
  x <- sub("^`r\\s", "", x)
  x <- sub("`$", "", x)
  
  x
}

#' Checks that the single character vector is an r statement, as denoted by
#' "`r statement`"
is_r_statement <- function(x)
{
  grepl("`r\\s.{1,}`", x)
}

check_mead_pe_scatter <- function(scen)
{
  if (exists("pe_scatter", scen)) {
    req_vals <- c("year", "model", "ann_text", "add_threshold_stats")
    assert_that(
      all(names(scen[["pe_scatter"]]) %in% req_vals) && 
        all(req_vals %in% names(scen[["pe_scatter"]])),
      msg = "year, model, ann_text, and add_threhold_stats must all be specified in pe_scatter."
    )
    assert_that(purrr::is_scalar_numeric(scen[["pe_scatter"]][["year"]]))
    assert_that(
      scen[["pe_scatter"]][["model"]] %in% c("CRSS", "MTOM"),
      msg = "model should be either CRSS or MTOM"
    )
    assert_that(purrr::is_scalar_character(scen[["pe_scatter"]][["ann_text"]]))
    assert_that(
      purrr::is_scalar_logical(scen[["pe_scatter"]][["add_threshold_stats"]])
    )
    pe_scatter <- scen[["pe_scatter"]]
    pe_scatter[["create"]] <- TRUE
    
  } else {
    pe_scatter <- list(create = FALSE)
  }
  
  pe_scatter
}

check_shortage_conditions <- function(scen)
{
  if (exists("shortage_conditions", scen)) {
    req_vals <- c("year", "model", "color_var", "subtitle", "segment_locs",
                  "annotation_loc")
    assert_that(
      all(names(scen[["shortage_conditions"]]) %in% req_vals) && 
        all(req_vals %in% names(scen[["shortage_conditions"]])),
      msg = paste(
        paste(req_vals, collpase = ", "), 
        "must all be specified in shortage_conditions"
      )
    )
    ss <- scen[["shortage_conditions"]]
    assert_that(purrr::is_scalar_numeric(ss[["year"]]))
    assert_that(
      ss[["model"]] %in% c("CRSS", "MTOM"),
      msg = "model should be either CRSS or MTOM"
    )
    if (ss[["model"]] == "CRSS")
      # CRSS
      assert_that(ss[["color_var"]] %in% c("mwdIcs", "WYRelease"))
    else
      # MTOM
      assert_that(ss[["color_var"]] %in% c("WYRelease"))
    
    assert_that(purrr::is_scalar_character(ss[["subtitle"]]))
    assert_that(is.numeric(ss[["segment_locs"]]) && 
                  length(ss[["segment_locs"]]) == 4)
    assert_that(is.numeric(ss[["annotation_loc"]]) && 
                  length(ss[["annotation_loc"]]) == 2)
    
    
    short <- ss
    short[["create"]] <- TRUE
    
  } else {
    short <- list(create = FALSE)
  }
  
  short
}

# checks to see if user specified that std_ind_figures should be created
# if it does, checks other optional parameters, and sets to default values
check_std_ind_figures <- function(scen, defaults)
{
  if (exists("std_ind_figures", scen)) {

    sif <- scen[["std_ind_figures"]]
    
    # it can be either a scalar logical or a list
    assert_that(is_scalar_logical(sif) || is.list(sif))
    
    if (isTRUE(sif) || 
        (is.list(sif) && is.null(sif[["create"]])) || 
        (is.list(sif) && isTRUE(sif[["create"]]))) {
      # if it is not a list, make it a list
      if (!is.list(sif))
        sif <- list()
      # set create if it is not already set
      sif[["create"]] <- TRUE
      # ann_text can be specified; if it is, it should be scalar character 
      # or null
      if (exists("ann_text", where = sif))
        assert_that(is_scalar_character(sif[["ann_text"]]) || 
                      is.null(sif[["ann_text"]]))
      
      # end_year can be specified; if it is, it should be scalar int; if it is
      # not, then set it to defaults
      if (exists("end_year", where = sif)) {
        assert_that(is_scalar_integer(sif[["end_year"]]))
      } else {
        sif[["end_year"]] <- defaults[["end_year"]]
      }
      
      scen[["std_ind_figures"]] <- sif
      
    } else if (isFALSE(sif)) {
      scen[["std_ind_figures"]] <- list()
      scen[["std_ind_figures"]][["create"]] <- FALSE
    }
    
  } else {
    scen[["std_ind_figures"]] <- list()
    scen[["std_ind_figures"]][["create"]] <- FALSE
  }
  
  scen
}

check_std_ind_tables <- function(scen)
{
  if (exists("std_ind_tables", scen)) {
    assert_that(is_scalar_logical(scen[["std_ind_tables"]]))
  } else {
    scen[["std_ind_tables"]] <- FALSE
  }
  
  scen
}

set_plot_groups <- function(ui)
{
  # TODO: set this to "plot_groups" at some point. 
  # 0 - pg can be unspecified. If it is, then can skip everything

  if (exists("plot_group", where = ui)) {
    # 1 fully expand the specified plot_groups, inheritiing from defaults
    # caption, years, colors, scen_names can all be specified at the plot_group
    # or individual plot level 
    # expand years out rather than just end points
    plot_group <- list()
    
    # ensure each plot_group has a unique name
    pg_names <- simplify2array(
      lapply(
        seq_along(ui[["plot_group"]]), 
        function(x) {names(ui[["plot_group"]][[x]])}
      )
    )
    check_unique_names(pg_names, "plot_group")
    
    for (pg in ui[["plot_group"]]) {
      plot_group[[names(pg)]] <- plot_group(pg[[1]], ui[["defaults"]])
    }
    
    # 2 convert to expected list structure
    ui[["plot_group"]] <- plot_group
  }
  
  ui
}

check_unique_names <- function(ss, group_name)
{
  assert_that(
    length(unique(ss)) == length(ss),
    msg = paste0(
      "All", group_name, "names must be unique.\n", 
      paste(unique(ss[duplicated(ss)]), collapse = ", "), 
      " show(s) up more than once"
    )
  )
  invisible(ss)
}

#' Takes one plot_group, and fully expands it, inheriting defaults from defaults
#' plot_group (`pg`) will have scenarios, scen_names, plot_colors, years, 
#' caption entries after it is expanded
expand_plot_group <- function(pg, defaults)
{
  
}
