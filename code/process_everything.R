source("code/get_plot_flags.R")
source("code/csd_boxplots.R")

process_everything <- function(ui)
{
  # check and setup --------------------
  # check and setup the files, directories, scenarios, etc.
  scens <- ui$scenarios$scens
  icList <- ui$scenarios$ic_list
  icMonth <- ui$scenarios$ic_month
  yrs2show <- ui$defaults$plot_yrs
  peYrs <- ui$defaults$pe_yrs

  crss_res_check_scen_names(
    scens, 
    icList, 
    icMonth, 
    ui
  )
  
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
  
  traceMap <- read.csv('data/Trace2IcMap.csv')
  
  # PROCESS RESULTS --------------
  process_all_rdfs(ui, o_files, folder_paths, traceMap)
  
  # READ IN CORRECT DFs -----------------------

  # ui$create_figures$standard_figures | ui$create_figures$pe_clouds | 
  # std_ind_figures
  all_plotted_scens <- get_all_plot_scenarios(ui)
  plot_flags <- get_plot_flags(ui)

  if (TRUE) {
    pe <- read_feather(o_files$cur_month_pe_file) %>%
      # The StartMonth column is used as the color variable in plotEOCYElev, and 
      # the names that should show up in the legend/differentiate scenario groups
      # are stored in the Agg Varaible. So easiest to just copy it from Agg to 
      # StartMonth for now
      dplyr::mutate(StartMonth = Agg) %>%
      filter(StartMonth %in% all_plotted_scens)
    
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
      ) %>%
      rename(AggName = Agg) %>%
      select(-StartMonth)
    
    cs <- read_feather(o_files$sys_cond_file) %>%
      rename(AggName = Agg) %>%
      filter(Variable %in% c('lbSurplus', 'lbShortage')) %>%
      rbind(cs) %>%
      filter(AggName %in% all_plotted_scens)
  }
  
  # TODO: switch this to be true if creating heatmap, or dcp table
  if (TRUE) {
    dcp_yrs <- c(min(yrs2show) - 1, yrs2show)
    tmp <- read_feather(o_files$cur_month_pe_file)
    lb_dcp <- compute_mead_dcp_probs(tmp, all_plotted_scens, 2019:2026)
    ub_dcp <- compute_powell_dcp_probs(tmp, all_plotted_scens, 2019:2026)
  }
  
  # TODO: heatmap, individual tables, ui$create_figures$conditional_probs
  if (TRUE) {
    sys_cond <- read_feather(o_files$sys_cond_file)
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
      peYrs, yrs2show, 
      ui, 
      o_files
    )
  }  
  
  # csd boxplots ---------------------------
  if (plot_flags[["csd_flag"]]) {
    message("... CSD boxplots")
    csd_ann <- read_feather(o_files[["csd_file"]])
    comp_figs <- c(comp_figs, create_all_csd_boxplots(csd_ann, ui))
  }
  
  # mead pe scatter ------------------
  scatter_figs <- list()
  if (plot_flags[["mead_pe_scatter"]]) {
    message("... Mead elevation scatter plot")
    scatter_figs <- create_mead_pe_scatter(ui, o_files, traceMap)
  }
  
  # Save figures -----------------------
  if (length(comp_figs) > 0 || length(ind_figs) > 0 || length(scatter_figs) > 0) {
    # save figures and table
    message("\ncreating pdf: ", o_files$main_pdf, "\n")
    pdf(o_files$main_pdf, width = 8, height = 6)
    
    for (i in seq_along(comp_figs)) {
      print(comp_figs[[i]])
    }
    
    for (i in seq_along(ind_figs)) {
      print(ind_figs[[i]])
    }
    
    for (i in seq_along(scatter_figs)) {
      print(scatter_figs[[i]])
    }
    dev.off()
  }
  
  # plot Clouds ----------------
  if (plot_flags[["cloud"]]) {
    message("... cloud figures")
    plot_both_clouds(pe, peYrs, ui, folder_paths)
  }
  
  # conditional probabilities ---------------------------
  if (plot_flags[["cond_probs"]]){
    message("... conditional probabilities")
    cp_scens <- get_cond_prob_scens(ui)
    get_all_cond_probs(sys_cond, cp_scens, yrs2show, ui)
  }
  
  # conditions leading to shortage ---------------------------------
  # pulled annotation out of generic function
  if (ui$create_figures$short_conditions) {
    message("... conditions leading to shortage")
    create_short_condition_figure(ui, o_files)
  }
  
  # 5 year simple table -------------------------
  if (plot_flags[["simple_5yr"]]) {
    ## create the 5-yr simple table that compares to the previous run
    message("... creating 5-year simple table")
    tmp_data <- read_feather(o_files$sys_cond_file) %>%
      rbind(read_feather(o_files$cur_month_pe_file))
    
    create_all_simple_5yr(tmp_data, ui, folder_paths)
    
    rm(tmp_data)
  }
  
}