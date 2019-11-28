library(assertthat)

# create a single scenario as a list, that has all important information
# specified in it
create_scenario <- function(name, scen_folders, ic, start_year, 
                            std_ind_tables = FALSE, std_ind_figures = FALSE)
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
  
  rr <- list()
  rr[[name]] <- list(
    name = name, 
    scen_folders = scen_folders, 
    ic = ic, 
    ic_month = ic_month,
    start_year = start_year,
    std_ind_tables = std_ind_tables,
    std_ind_figures = std_ind_figures
  )
  
  rr
}

# converts the list to the different specified variables
scenario_to_vars <- function(scenarios)
{
  assert_that(is.list(scenarios) & length(scenarios) >= 1)
  
  scens <- list()
  ic_list <- list()
  ic_month <- c()
  all_names <- c()
  
  for (i in seq_len(length(scenarios))) {
    tmp_scen <- scenarios[[i]]
    assert_that(
      all(names(tmp_scen) %in% c("name", "ic", "scen_folders", "ic_month"))
    )
    assert_that(
      all(c("name", "ic", "scen_folders", "ic_month") %in% names(tmp_scen))
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
