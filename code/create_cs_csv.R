create_cs_csv <- function(cs, scenario, o_files)
{
  csVars <- csVarNames()
  # create data table to save crit stats
  cs_out <- cs %>%
    dplyr::filter(
      Year %in% yrs2show, 
      AggName == scenario, 
      Variable != 'lbSurplus'
    ) %>%
    # compute the percent of traces by averaging values 
    group_by(Year,Variable) %>%
    summarise(Value = mean(Value)) %>%
    dplyr::mutate(vName = csVars[Variable]) %>%
    # reshape to be easier to print out
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(vName, Value)
  
  data.table::fwrite(cs_out, o_files$crit_stats_proc, row.names = FALSE)
  
  invisible(cs)
}