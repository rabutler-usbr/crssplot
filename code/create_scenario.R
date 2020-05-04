library(assertthat)

# create a single scenario as a list, that has all important information
# specified in it
create_scenario <- function(name, scen_folders, ic, start_year, 
                            std_ind_tables = FALSE, std_ind_figures = FALSE,
                            cond_probs = FALSE, mead_pe_scatter = NULL,
                            shortage_conditions = NULL)
{
  assert_that(length(name) == 1 & is.character(name))
  assert_that(length(scen_folders) >= 1 & is.character(scen_folders))
  assert_that(
    (length(ic) == 2 & is.numeric(ic)) | (length(ic) == 1 & is.character(ic)),
    msg = "`ic` should either be a numeric with 2 values, or a file path."
  ) 
  assert_that(length(start_year) == 1 & is.number(start_year))
  assert_that(
    start_year >= 2000, 
    msg = "Expect the model year to begin after 1999"
  )
  
  ic_month <- paste(as.character(start_year - 1 - 2000), "DEC", sep = "-")
  
  # check the way the shortage conditions are specified
  if (!is.null(shortage_conditions)) {
    shortage_conditions <- check_shortage_cond_spec(shortage_conditions) %>%
      update_shortage_cond_spec()
  }
  
  rr <- list()
  rr[[name]] <- list(
    name = name, 
    scen_folders = scen_folders, 
    ic = ic, 
    ic_month = ic_month,
    start_year = start_year,
    std_ind_tables = std_ind_tables,
    std_ind_figures = std_ind_figures,
    cond_probs = cond_probs,
    mead_pe_scatter = mead_pe_scatter,
    shortage_conditions = shortage_conditions
  )
  
  rr
}

# converts the list to the different specified variables
scenario_to_vars <- function(scenarios)
{
  assert_that(is.list(scenarios) & length(scenarios) >= 1)
  
  # make sure all scenario names are unique
  dup_scens <- duplicated(names(scenarios))
  assert_that(
    !any(dup_scens),
    msg = paste0(
      "There should be no duplicated scenario names.\n", 
      "Duplicate scenario names are:\n  - ",
      paste(unique(names(scenarios)[dup_scens]), collapse = "\n  - "))
  )
  
  required_variables <- c("name", "ic", "scen_folders", "ic_month", "start_year")
  optional_variables <- c("std_ind_tables", "std_ind_figures", "cond_probs",
                          "mead_pe_scatter", "shortage_conditions")
  
  scens <- list()
  ic_list <- list()
  ic_month <- c()
  all_names <- c()
  
  for (i in seq_len(length(scenarios))) {
    tmp_scen <- scenarios[[i]]
    assert_that(
      all(names(tmp_scen) %in% c(required_variables, optional_variables)),
      msg = paste(
        "Names in the scenario are:", paste(names(tmp_scen), collapse = ", "),
        "\nAll required + optional variables are:", 
        paste(c(required_variables, optional_variables), collapse = ", ")
      )
    )
    assert_that(
      all(required_variables %in% names(tmp_scen))
    )
    
    name <- tmp_scen$name
    scens[[name]] <- tmp_scen$scen_folders
    ic_list[[name]] <- tmp_scen$ic
    ic_month <- c(ic_month, name = tmp_scen$ic_month)
    all_names <- c(all_names, name)
  }
  
  names(ic_month) <- all_names
  
  list(scens = scens, ic_list = ic_list, ic_month = ic_month)
}

# check the shortage condition specification
check_shortage_cond_spec <- function(shortage_conditions)
{
  if (!isFALSE(shortage_conditions[["create"]])) {
    assert_that(
      shortage_conditions$model %in% c("CRSS", "MTOM"), 
      msg = "The shortage conditions model should either be 'MTOM' or 'CRSS'"
    )
    
    if (shortage_conditions$model == "CRSS") {
      assert_that(
        is.null(shortage_conditions$res_file),
        msg = "`res_file` should be null when model is CRSS"
      )
  
      assert_that(
        shortage_conditions$color_var %in% c("mwdIcs", "WYRelease"),
        msg = paste0(
          "When using CRSS to determine conditions leading to shortage,\n",
          "the `color_var` should be either 'mwdIcs', or 'WYRelease'."  
        )
      )
      
      } else {
      # have already determined that it is either CRSS or MTOM,so must be MTOM
      assert_that(
        shortage_conditions$color_var %in% c("WYRelease"),
        msg = paste0(
          "When using MTOM to determine conditions leading to shortage,\n",
          "the `color_var` must be 'WYRelease'."  
        )
      )
    }
  }
  invisible(shortage_conditions)
}

update_shortage_cond_spec <- function(shortage_conditions)
{
  if (!isFALSE(shortage_conditions[["create"]])) {
    shortage_conditions[['title']] <- paste(
      'Conditions Leading to a Lower Basin Shortage in',
      shortage_conditions$year + 1
    )
    
    if (shortage_conditions[["model"]] == "CRSS") {
      # the label for the percent of average
      shortage_conditions[['lb_label']] <- "Total LB natural inflow percent\nof average (1906-2015)"
      
    } else {
      # its MTOM
      # the label for the percent of average
      shortage_conditions[['lb_label']] <- 'LB total side inflow percent\nof average (1981-2015)'
    }
  }
  shortage_conditions
}
