library(assertthat)
library(fs)

# checks that scenario names match in the different variables they are 
# specified in. Returns `scens` invisibly, if all checks pass. Otherwise 
# provides error messages.
crss_res_check_scen_names <- function(scens, icList, icMonth, ui)
{
  ss5 <- ui$simple_5yr$ss5
  heatmap_names <- ui$heatmap$scenarios
  
  # check that the names of scens, icList, and icMonth are all the same; they
  # don't necessarily need to be in the same order, just all exist in one another
  assert_that(
    all(names(scens) %in% names(icList), names(icList) %in% names(scens), 
        names(scens) %in% names(icMonth), names(icMonth) %in% names(scens),
        names(icList) %in% names(icMonth), names(icMonth) %in% names(icList)),
    msg = paste(
      "scenario group names do not match.",
      "\nthe names() of scens, icList, and icMonth should all be the same"
    )
  )
  
  # if we made it here, we know names() of scens, icList, and icMonth all match, 
  # so just check to make sure that ss5 and heatmap_names is withing scens
  assert_that(
    all(names(ss5) %in% names(scens)), 
    msg = "scenario goup names of ss5 must match the names found in scens"
  )
  
  assert_that(
    all(names(heatmap_names) %in% names(scens)),
    msg = "Scenario group names of `heatmap_names` must match the names found in `scens`"
  )
  
  if (ui$create_figures$pe_clouds) {
    assert_that(
      all(ui$clouds$scenarios %in% names(ui$scenarios$scens)),
      msg = "Scenarios specified for the clouds to not match those available."
    )
  }
  
  if (ui$create_figures$short_conditions) {
    assert_that(
      length(ui$shortage_conditions$res_file) == 1, 
      msg = "conditions leading to shortage is only designed to work with 1 scenario"
    )
  }
  
  invisible(scens)
}

# creats the necesary folders for saving the output data, and ensures the 
# folders exist
# returns the folder paths that the results and figures will be saved to as a 
# list
crss_res_directory_setup <- function(i_folder, get_pe_data, get_sys_cond_data,  
                                     CRSSDIR, crss_month)
{
 
  # onlyl check if reading in data if you have to getData
  if (get_pe_data | get_sys_cond_data) {
    message('Scenario data will be read in from: ', i_folder)
    assert_that(
      dir.exists(i_folder),
      msg = paste(
        i_folder, 
        'does not exist. Please ensure iFolder is set correctly.'
      )
    )
  }
  
  # folder location to save figures and fully procssed tables
  assert_that(
    dir.exists(CRSSDIR),
    msg = paste(
      CRSSDIR, 
      "does not exist.\n",
      "** Please ensure CRSS_DIR environment variable is set correctly."
    )
  )
  
  tmp_res_rolder <- file.path(CRSSDIR, 'results')
  
  if (!file.exists(tmp_res_rolder)) {
    message(tmp_res_rolder,'does not exist. Creating this folder...')
    dir.create(tmp_res_rolder)
  }
  
  oFigs <- file.path(CRSSDIR,'results', crss_month) 
  if (!file.exists(oFigs)) {
    message(paste('Creating folder:', oFigs))
    dir.create(oFigs)
  }
  message('Figures and tables will be saved to: ', oFigs)
  
  png_out <- file.path(oFigs, "png")
  
  if (!file.exists(png_out)) {
    message("Creating folder: ", png_out)
    dir.create(png_out)
  }
  message("pngs will be saved to: ", png_out)
  
  # folder to save procssed text files to (intermediate processed data)
  resFolder <- file.path(CRSSDIR,'results', crss_month, 'tempData')
  if (!file.exists(resFolder)) {
    message(paste('Creating folder:', resFolder))
    dir.create(resFolder)
  }
  message('Intermediate data will be saved to: ', resFolder)
  
  # figure data --------------------
  fig_data <- file.path(oFigs, "figure_data")
  if (!file.exists(fig_data)) {
    message("Creating folder: ", fig_data)
    dir.create(fig_data)
  }
  message("Figure data will be saved to: ", fig_data)
  
  # tables --------------------------
  tables <- file.path(oFigs, "tables")
  if (!file.exists(tables)) {
    message("Creating folder: ", tables)
    dir.create(tables)
  }
  message("Tables will be saved to: ", tables)
  
  # return
  list(figs_folder = oFigs, res_folder = resFolder, png_out = png_out, 
       figure_data = fig_data, tables = tables)
}

# returns a list of all the necessary output file names
crss_res_get_file_names <- function(main_pdf)
{
  # return
  list(
    sys_cond_file = 'SysCond.feather' ,
    tmp_pe_file = 'tempPE.feather',
    # file name of Powell and Mead PE data
    cur_month_pe_file = 'MeadPowellPE.feather',
    short_cond_fig = 'shortConditionsFig.pdf',
    simple_5yr_file = '5yrSimple.pdf',
    mead_cloud = "Mead.png",
    powell_cloud = "Powell.png",
    main_pdf = main_pdf,
    csd_file = "csd_ann.feather"
  )
}

# goes through all the file names, and appends on the correct file paths, so
# all are fully specified paths
crss_res_append_file_path <- function(file_names, figs_folder, res_folder)
{
  res <- c("sys_cond_file", "tmp_pe_file", "cur_month_pe_file", "csd_file")
  
  for (i in names(file_names)) {
    if (i %in% res) {
      file_names[[i]] <- file.path(res_folder, file_names[[i]])
    } else {
      file_names[[i]] <- file.path(figs_folder, file_names[[i]])
    }
  }
  
  file_names
}

construct_table_file_name <- function(table_name, scenario, yrs, extra_label)
{
  year_lab <- paste0(yrs[1], '_', tail(yrs, 1))
  
  if (extra_label != '') {
    extra_label <- paste0(extra_label, "_")
  }
  
  str_replace_all(scenario, " ", "") %>%
    paste0("_", extra_label, table_name, "_", year_lab, ".csv") %>%
    path_sanitize()
}
