create_ig_sys_table <- function(zz, scenario, yrs, o_folder, ui)
{
  assert_that(length(scenario) == 1)
  
  # create the IG system cond. table
  sysTable <- CRSSIO::crsso_get_sys_cond_table(
    dplyr::filter(zz, Year %in% yrs & ScenarioGroup == scenario), yrs
  )
  
  f_name <- construct_table_file_name(
    "SysTableFull", scenario, yrs, ui$folders$extra_label
  )
  
  f_name <- file.path(o_folder[['tables']], f_name)
  
  # save the sys cond table
  data.table::fwrite(
    as.data.frame(sysTable[['fullTable']]), 
    f_name, 
    row.names = TRUE
  )
  
  invisible(zz)
}
