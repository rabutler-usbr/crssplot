library(assertthat)

# checks that scenario names match in the different variables they are 
# specified in. Returns `scens` invisibly, if all checks pass. Otherwise 
# provides error messages.
crss_res_check_scen_names <- function(scens, icList, icMonth, mainScenGroup, ss5)
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
  # so just check to make sure that ss5 is withing scens
  assert_that(
    all(names(ss5) %in% names(scens)), 
    msg = "scenario goup names of ss5 must match the names found in scens"
  )
  
  invisible(scens)
}

crss_res_directory_setup <- function(getPeData, getSysCondData, iFolder, CRSSDIR,
                                     crssMonth)
{
 
  # onlyl check if reading in data if you have to getData
  if (getPeData | getSysCondData) {
    message('Scenario data will be read in from: ', iFolder)
    assert_that(
      file.exists(iFolder),
      msg = paste(
        iFolder, 
        'does not exist. Please ensure iFolder is set correctly.'
      )
    )
  }
  
  # folder location to save figures and fully procssed tables
  assert_that(
    file.exists(CRSSDIR),
    msg = paste(
      CRSSDIR, 
      ' does not exist. Please ensure CRSS_DIR environment variable is set correctly'
    )
  )
  
  tmp_res_rolder <- file.path(CRSSDIR, 'results')
  
  if (!file.exists(tmp_res_rolder)) {
    message(tmp_res_rolder,'does not exist. Creating this folder...')
    dir.create(tmp_res_rolder)
  }
  
  oFigs <- file.path(CRSSDIR,'results', crssMonth) 
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
  list(oFigs = oFigs, resFolder = resFolder)
}

crss_res_get_file_names <- function(extra_label, yrs2show)
{
  sysCondFile <- 'SysCond.feather' # file name of system conditions data
  tmpPEFile <- 'tempPE.feather'
  curMonthPEFile <- 'MeadPowellPE.feather' # file name of Powell and Mead PE data
  
  # file name for the system conditions procssed file
  sysCondTable <- paste0(extra_label, 'SysTableFull', yrs2show[1], '_',
                         tail(yrs2show, 1),'.csv') 
  
  
  critStatsProc <- paste0(extra_label, 'CritStats.csv')
  condProbFile <- 'CondProbs.csv'
  
  dcp_prob_file <- paste0(extra_label, yrs2show[1], "_", tail(yrs2show,1), 
                          "dcp_probs.csv")
  
  shortCondFig <- 'shortConditionsFig.pdf'
  
  simple5YrFile <- '5yrSimple.pdf'
  
  traceMap <- read.csv('data/Trace2IcMap.csv')
}

