
process_everything <- function(ui)
{
  # check and setup --------------------
  # check and setup the files, directories, scenarios, etc.
  scens <- ui$scenarios$scens
  icList <- ui$scenarios$ic_list
  icMonth <- ui$scenarios$ic_month
  yrs2show <- ui$defaults$plot_years
  peYrs <- ui$defaults$pe_yrs

  crss_res_check_scen_names(
    scens, 
    icList, 
    icMonth, 
    ui
  )

  ui$folders$i_folder <- update_ifolder(ui$folders$i_folder)
  
  folder_paths <- crss_res_directory_setup(
    ui$folders$i_folder, 
    get_pe_data = ui$process_data$pe_data, 
    get_sys_cond_data = ui$process_data$crss_short_cond_data,
    CRSSDIR = ui$folders$CRSSDIR,
    crss_month = ui$folders$crss_month
  )
  
  o_files <- crss_res_get_file_names(main_pdf = ui$folders$pdf_name) %>%
    crss_res_append_file_path(
      figs_folder = folder_paths$figs_folder, 
      res_folder = folder_paths$res_folder
    )
  
  traceMap <- utils::read.csv(
    system.file("extdata", "Trace2IcMap.csv", package = "crssplot")
  )
  
  # PROCESS RESULTS --------------
  process_all_rdfs(ui, o_files, folder_paths, traceMap)
  
  # READ IN CORRECT DFs -----------------------

  # ui$create_figures$standard_figures | ui$create_figures$pe_clouds | 
  # std_ind_figures
  all_plotted_scens <- get_all_plot_scenarios(ui)
  plot_flags <- get_plot_flags(ui)

  if (TRUE) {
    pe <- feather::read_feather(o_files$cur_month_pe_file) %>%
      filter(ScenarioGroup %in% all_plotted_scens)
    
    # compare crit stats for all scenarios
    # call once each for powell LT 3490, shortage, and surplus
    # get the necessary variables by filtering from the pe and syscond data files
    cs <- pe %>%
      filter(
        Variable %in% c('mead_min_lt_1000', 'mead_min_lt_1020', 
                        'powell_wy_min_lt_3490', 
                        'powell_wy_min_lt_3525', 'mead_min_lt_1025', 
                        "mead_min_lt_1025", "mead_dec_lt_1025", 
                        "powell_dec_lt_3525")
      )
    
    cs <- feather::read_feather(o_files$sys_cond_file) %>%
      filter(Variable %in% c('lbSurplus', 'lbShortage')) %>%
      rbind(cs) %>%
      filter(ScenarioGroup %in% all_plotted_scens)
  }
  
  # TODO: switch this to be true if creating heatmap, or dcp table
  if (TRUE) {
    dcp_yrs <- c(min(yrs2show) - 1, yrs2show)
    tmp <- feather::read_feather(o_files$cur_month_pe_file)
    lb_dcp <- compute_mead_dcp_probs(tmp, all_plotted_scens, 2019:2026)
    ub_dcp <- compute_powell_dcp_probs(tmp, all_plotted_scens, 2019:2026)
  }
  
  # TODO: heatmap, individual tables, ui$create_figures$conditional_probs
  if (TRUE) {
    sys_cond <- feather::read_feather(o_files$sys_cond_file)
  }
  
  # PLOTTING -------------------------------
  message("starting to create figures and tables")
  
  # heatmap ---------------------------------------------
  if (plot_flags[["heat"]]) {
    # system condition heatmap -------------------------
    message("... System conditions heatmap")
    
    create_mead_powell_heatmaps(
      lb_dcp, sys_cond, 
      ui,
      folder_paths
    )
  }

  #if (ui$create_figures$std_ind_tables) { 
  for (i in seq_along(ui[["ind_plots"]][["std_ind_tables"]])) {
    
    if (i == 1) message("... creating individual scenario tables")
    cur_scen <- names(ui[["ind_plots"]][["std_ind_tables"]])[i]
    message("   ... ", cur_scen)

    # Individual scenario tables ----------------
    # system conditions table 
    message("      ... creating system conditions table")
    create_ig_sys_table(sys_cond, cur_scen, yrs2show, folder_paths, ui)
    
    # get the DCP related probabilities
    message("      ... DCP Probabilities")
    create_dcp_table(lb_dcp, ub_dcp, cur_scen, yrs2show, folder_paths, ui)
  }

  # individual scenario figures -----------------
  ind_figs <- list()
  for (i in seq_along(ui[["ind_plots"]][["std_ind_figures"]])) {
    if (i == 1) message("... creating individual scenario figures")
    cur_scen <- names(ui[["ind_plots"]][["std_ind_figures"]])[i]
    message("   ... ", cur_scen)
    # now create figures only for the current scenario
    # defaults are ok for legendTitle, legLoc, nC, and annSize
    # drop Mead LT 1025 from one plot and Mead LT 1020 from 
    # the other plot
    tmp <- create_std_ind_figures(cs, sys_cond, cur_scen, ui)
    ind_figs <- c(ind_figs, tmp)
    
    create_cs_csv(cs, cur_scen, folder_paths, ui)
  }
  
  comp_figs <- list()
  if (plot_flags[["std_comparison"]]) {
    # std comparison figures -------------
    # includes previous month's results too
    message("... Scenario comparison figures")

    comp_figs <- create_scenario_comparison_figures(
      pe, cs, 
      ui, 
      o_files
    )
  }  
  
  # csd boxplots ---------------------------
  if (plot_flags[["csd_flag"]]) {
    message("... CSD boxplots")
    csd_ann <- feather::read_feather(o_files[["csd_file"]])
    comp_figs <- c(comp_figs, create_all_csd_boxplots(csd_ann, ui))
  }
  
  # mead pe scatter ------------------
  scatter_figs <- list()
  if (plot_flags[["mead_pe_scatter"]]) {
    message("... Mead elevation scatter plot")
    scatter_figs <- create_mead_pe_scatter(ui, o_files, traceMap)
  }
  
  # conditions leading to shortage ---------------------------------
  # pulled annotation out of generic function
  short_cond_figs <- list()
  if (plot_flags[["shortage_conditions"]]) {
    message("... conditions leading to shortage")
    short_cond_figs <- create_short_condition_figure(ui, folder_paths)
  }
  
  # Save figures -----------------------
  if (length(comp_figs) > 0 || length(ind_figs) > 0 || 
      length(scatter_figs) > 0 || length(short_cond_figs > 0)) {
    # save figures and table
    message("\ncreating pdf: ", o_files$main_pdf, "\n")
    grDevices::pdf(o_files$main_pdf, width = 8, height = 6)
    
    for (i in seq_along(comp_figs)) {
      print(comp_figs[[i]])
    }
    
    for (i in seq_along(ind_figs)) {
      print(ind_figs[[i]])
    }
    
    for (i in seq_along(scatter_figs)) {
      print(scatter_figs[[i]])
    }
    
    for (i in seq_along(short_cond_figs)) {
      print(short_cond_figs[[i]])
    }
    grDevices::dev.off()
  }
  
  # plot Clouds ----------------
  if (plot_flags[["cloud"]]) {
    message("... cloud figures")
    plot_both_clouds(pe, ui, folder_paths)
  }
  
  # conditional probabilities ---------------------------
  if (plot_flags[["cond_probs"]]){
    message("... conditional probabilities")
    cp_scens <- get_cond_prob_scens(ui)
    get_all_cond_probs(sys_cond, cp_scens, yrs2show, ui)
  }

  # 5 year simple table -------------------------
  if (isTRUE(plot_flags[["simple_5yr"]])) {
    ## create the 5-yr simple table that compares to the previous run
    message("... creating 5-year simple table")
    tmp_data <- feather::read_feather(o_files$sys_cond_file) %>%
      rbind(feather::read_feather(o_files$cur_month_pe_file))
    
    create_all_simple_5yr(tmp_data, ui, folder_paths)
    
    rm(tmp_data)
  }
  
}

# checks if the i_folder input is an r statement. if it is, then it parses it
# otherwise returns it.
update_ifolder <- function(x) {
  
  if (is_r_statement(x)) {
    # strip of `r and `
    x <- x %>%
      strip_r_from_string() %>%
      parse(text = ., keep.source = FALSE) %>%
      eval()
  }
  
  x
}
