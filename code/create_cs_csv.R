create_cs_csv <- function(cs, scenario, o_folders, ui)
{
  sy <- ui[["scen_tree"]][[scenario]][["start_year"]]
  ey <- ui[["ind_plots"]][["std_ind_figures"]][[scenario]][["options"]][["end_year"]]
  yrs2show <- sy:ey
  
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
  
  o_file <- file.path(
    o_folders[["figure_data"]], 
    path_sanitize(paste0(str_replace_all(scenario, " ", ""), "_CritStats.csv"))
  )
  message("      ... saving crit stats figure data")
  data.table::fwrite(cs_out, o_file, row.names = FALSE)
  
  invisible(cs)
}