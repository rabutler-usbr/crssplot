create_ig_sys_table <- function(zz, scenario, yrs, o_files)
{
  assert_that(length(scenario) == 1)
  
  # create the IG system cond. table
  sysTable <- CRSSIO::crsso_get_sys_cond_table(
    dplyr::filter(zz, Year %in% yrs & Agg == scenario), yrs
  )
  
  # save the sys cond table
  data.table::fwrite(
    as.data.frame(sysTable[['fullTable']]), 
    o_files$sys_cond_table, 
    row.names = TRUE
  )
  
  invisible(zz)
}
