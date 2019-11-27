create_dcp_table <- function(z1, z2, scenario, yrs, o_files)
{
  dcp_probs <- bind_rows(
    filter(z1, Agg == scenario),
    filter(z2, Agg == scenario)
  ) %>%
    filter(Year %in% yrs) %>%
    format_dcp_table()
  
  # save the dcp probabilities table
  data.table::fwrite(
    dcp_probs, 
    o_files$dcp_prob_file, 
    row.names = TRUE
  )
  
  invisible(TRUE)
}
