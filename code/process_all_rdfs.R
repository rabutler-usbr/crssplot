process_all_rdfs <- function(ui, o_files, folder_paths, traceMap)
{
  # System Conditions ------------------------
  if (ui$process_data$sys_cond_data) {
    message('starting to get system conditions data')
    
    # system condition rwa
    sys_rwa <- CRSSIO::sys_cond_rwa()
    if (packageVersion("CRSSIO") <= "0.7.0")
      sys_rwa$period <- "eocy"
    
    getScenarioData(
      ui$scenarios$scens, 
      ui$folders$i_folder, 
      o_files$sys_cond_file,
      TRUE,
      'aggFromScenList', 
      sys_rwa
    )
    message('finished geting system conditions data')
  }
  
  # PE data -------------------------
  if (ui$process_data$pe_data) {
    ## get the Mead and Powel EOCY Data
    message('starting to get PE data')
    
    pe_rwa <- read_rwd_agg("data/MPPEStats_sam.csv")
    
    getScenarioData(ui$scenarios$scens, ui$folders$i_folder, 
                    o_files$tmp_pe_file, TRUE, 
                    'aggFromScenList', pe_rwa)
    
    # append initial conditions onto May data
    getAndAppendIC(ui$scenarios$scens, o_files$tmp_pe_file, 
                   o_files$cur_month_pe_file, ui$scenarios$ic_list, 
                   ui$scenarios$ic_month, 
                   TRUE, 'aggFromScenList', traceMap, 
                   icDimNumber = ui$defaults$ic_dim_number)
    
    message('finished getting PE data')
  }
  
  # shortage conditions ----------------------
  if (ui$process_data$crss_short_cond_data) {
    message("Starting to get CRSS shortage condition data...")

    get_shortcond_from_rdf(
      scenario = ui[["scenarios"]][["scens"]], 
      i_folder = ui$folders$i_folder, 
      oFolder = folder_paths$res_folder
    )
    
    message("Done getting CRSS shortage condition data")
  }
  
  # CSD ---------------------------
  if (ui$process_data$csd_data) {
    message('starting to get annual computed state depletions data')
    
    # system condition rwa
    sys_rwa <- rwd_agg(rdfs = "CSD_ann.rdf")
    
    getScenarioData(
      ui[["scenarios"]][["scens"]],
      ui[["folders"]][["i_folder"]], 
      o_files[["csd_file"]],
      TRUE,
      'aggFromScenList', 
      sys_rwa
    )
    message('finished geting annual computed state depletions data')
  }
  
  invisible(sum(unlist(ui$process_data)))
}