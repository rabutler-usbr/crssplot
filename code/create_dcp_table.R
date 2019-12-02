create_dcp_table <- function(z1, z2, scenario, yrs, o_folder, ui)
{
  dcp_probs <- bind_rows(
    filter(z1, Agg == scenario),
    filter(z2, Agg == scenario)
  ) %>%
    filter(Year %in% yrs) %>%
    format_dcp_table()
  
  f_name <- construct_table_file_name(
    "dcp_probs", scenario, yrs, ui$folders$extra_label
  )
  
  f_name <- file.path(o_folder[['tables']], f_name)
  
  # save the dcp probabilities table
  data.table::fwrite(dcp_probs, f_name, row.names = TRUE)
  
  invisible(TRUE)
}
