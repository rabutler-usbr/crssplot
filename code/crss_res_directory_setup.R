library(assertthat)

# checks that scenario names match in the different variables they are 
# specified in. Returns `scens` invisibly, if all checks pass. Otherwise 
# provides error messages.
crss_res_check_scen_names <- function(scens, icList, icMonth, mainScenGroup, 
                                      ss5, heatmap_names)
{
  # some sanity checks that UI is correct
  assert_that(
    mainScenGroup %in% names(scens), 
    msg = paste(mainScenGroup, ' is not found in scens.')
  )
  
  assert_that(
    mainScenGroup %in% names(icList), 
    msg = paste(mainScenGroup, ' is not found in icList')
  )
  
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
      file.exists(i_folder),
      msg = paste(
        i_folder, 
        'does not exist. Please ensure iFolder is set correctly.'
      )
    )
  }
  
  # folder location to save figures and fully procssed tables
  assert_that(
    file.exists(CRSSDIR),
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
  
  # folder to save procssed text files to (intermediate processed data)
  resFolder <- file.path(CRSSDIR,'results', crssMonth, 'tempData')
  if (!file.exists(resFolder)) {
    message(paste('Creating folder:', resFolder))
    dir.create(resFolder)
  }
  message('Intermediate data will be saved to: ', resFolder)
  
  # return
  list(figs_folder = oFigs, res_folder = resFolder)
}

# returns a list of all the necessary output file names
crss_res_get_file_names <- function(extra_label, yrs, main_pdf)
{
  year_lab <- paste0(yrs[1], '_', tail(yrs, 1))
  
  # file name for the system conditions procssed file
  sysCondTable <- paste0(extra_label, 'SysTableFull', year_lab, '.csv') 
  
  critStatsProc <- paste0(extra_label, 'CritStats.csv')
  dcp_prob_file <- paste0(extra_label, year_lab, "dcp_probs.csv")

  # return
  list(
    sys_cond_file = 'SysCond.feather' ,
    tmp_pe_file = 'tempPE.feather',
    # file name of Powell and Mead PE data
    cur_month_pe_file = 'MeadPowellPE.feather',
    sys_cond_table = sysCondTable,
    crit_stats_proc = critStatsProc,
    cond_prob_file = 'CondProbs.csv',
    dcp_prob_file = dcp_prob_file,
    short_cond_fig = 'shortConditionsFig.pdf',
    simple_5yr_file = '5yrSimple.pdf',
    mead_cloud = "Mead.png",
    powell_cloud = "Powell.png",
    main_pdf = main_pdf
  )
}

# goes through all the file names, and appends on the correct file paths, so
# all are fully specified paths
crss_res_append_file_path <- function(file_names, figs_folder, res_folder)
{
  res <- c("sys_cond_file", "tmp_pe_file", "cur_month_pe_file")
  
  for (i in names(file_names)) {
    if (i %in% res) {
      file_names[[i]] <- file.path(res_folder, file_names[[i]])
    } else {
      file_names[[i]] <- file.path(figs_folder, file_names[[i]])
    }
  }
  
  file_names
}
