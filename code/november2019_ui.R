library(assertthat)
source("code/create_scenario.R")
# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# i_folder is a path to the top level crss directory that contains the model 
# output it could be the same as CRSSDIR, but is allowed to be different so that 
# you can read model output from the server, but save figures locally.

nov2019_ui <- function()
{
  # swtiches to read data. if you've already read the data in from rdfs once, 
  # you may be able to set this to FALSE, so it's faster
  process_data <- list(
    sys_cond_data = FALSE,
    pe_data = FALSE,
    crss_short_cond_data = FALSE
  )
  
  # "switches" to create/not create different figures
  # typical figures
  create_figures <- list(
    standard_figures = FALSE,
    simple_5yr_table = FALSE,
    
    # optional figures/tables
    short_conditions = FALSE,
    conditional_probs = FALSE,
    pe_scatter_fig = FALSE,
    
    pe_clouds = FALSE,
    heatmap = TRUE
  )
  
  # ** make sure CRSS_DIR is set correctly before running
  folders <- list(
    i_folder = "M:/Shared/CRSS/2019/Scenario",
    CRSSDIR = Sys.getenv("CRSS_DIR"),
    # set crssMonth to the month CRSS was run. data and figures will be saved in 
    # a folder with this name
    crss_month = "nov2019",
    pdf_name = 'compare_aug_fix_st.pdf',
    # inserted onto some files. Can be ''
    extra_label = ""
  )
  
  # scenarios are orderd model,supply,demand,policy,initial conditions 
  # (if initial conditions are used) scens should be a list, each entry is a 
  # scenario group name, and the entry is a character vector of length 1 to n of 
  # individual scenarios. all of the values in each entry of the list are 
  # combined together and processed as one scenario group. So for a run that has 
  # 30 initial conditions, all 30 runs are averaged/combined together. The names 
  # in the scens list (scenario Groups) will be the Scenario names that show up 
  # on plots.
  
  # defaults ---------------------------
  defaults <- list(
    # in the comma seperated scenario folder names, currently the 5th entry is  
    # the initial conditions entry
    # update if for some reason the scenario naming convention has changed
    ic_dim_number = 5,
    # setting to NULL will not wrap legend entries at all
    legend_wrap = 20,
    # how to label the color scale on the plots
    color_label = 'Scenario',
    # text that will be added to figures
    end_year = 2026
  )
  # TODO: update so that these are computed if not specified
  # years to show the crit stats figures  
  defaults[['plot_yrs']] <- 2020:defaults$end_year 
  # years to show the Mead/Powell 10/50/90 figures for
  defaults[['pe_yrs']] <- 2019:defaults$end_year
  
  # specify the scenarios -------------------------
  all_scenarios <- c(
    create_scenario(
      "Aug 2019 - Full - w/Bug",
      scen_folders = "Aug2019_2020,DNF,2007Dems,IG_DCP,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "Aug 2019 - ST - w/Bug",
      scen_folders = "Aug2019_2020,ISM1988_2017,2007Dems,IG_DCP,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "Aug 2019 - Full - Fixed",
      scen_folders = "Aug2019_2020_v4.1.1,DNF,2007Dems,IG_DCP_v4.2.0,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "Aug 2019 - ST - Fixed",
      scen_folders = "Aug2019_2020_v4.1.1,ISM1988_2017,2007Dems,IG_DCP_v4.2.0,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "June 2019 - ST", 
      scen_folders = rw_scen_gen_names(
        "Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP",
        paste0("Trace", 4:38),
        "DCP_Cons"
      ), 
      ic = file.path(
        folders$CRSSDIR, 
        "dmi/InitialConditions/june_2019/MtomToCrss_Monthly.xlsx"
      ), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "June 2019 - DNF", 
      scen_folders = rw_scen_gen_names(
        "Jun2019_2020,DNF,2007Dems,IG_DCP",
        paste0("Trace", 4:38),
        "DCP_Cons"
      ), 
      ic = file.path(
        folders$CRSSDIR, 
        "dmi/InitialConditions/june_2019/MtomToCrss_Monthly.xlsx"
      ), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "Nov 2019 - DNF",
      scen_folders = "Nov2019_2020,DNF,2007Dems,IG_DCP,Most", 
      ic = c(3608.24, 1087.80), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),
    create_scenario(
      "Nov 2019 - ST",
      scen_folders = "Nov2019_2020,ISM1988_2017,2007Dems,IG_DCP,Most", 
      ic = c(3608.24, 1087.80), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    )
  )

  # convert all_scenarios to the different variables --------------------
  scenarios <- scenario_to_vars(all_scenarios)
  
  # individual plot specification -------------------------
  # text that will be added to figures
  std_ind_figures <- list(
    # "Nov 2019 - DNF" = 
    #   list(ann_text = 'Results from November 2019 CRSS run with full hydrology', end_year = 2026),
    # "Nov 2019 - ST" = 
    #   list(ann_text = 'Results from November 2019 CRSS run with stress test hydrology', end_year = 2026),
    # "Aug 2019 - DNF - Fixed" = 
    #   list(
    #     ann_text = "Results from updated August 2019 CRSS run with full hydrology",
    #     end_year = 2026
    #   ),
    # "Aug 2019 - ST - Fixed" = 
    #   list(
    #     ann_text = "Results from updated August 2019 CRSS run with stress test hydrology",
    #     end_year = 2026
    #   )
    "Aug 2019 - DNF - w/Bug" = list(
      ann_text = "Results from the BAD August 2019 CRSS run with full hydrology",
      end_year = 2026
    )
  )

  ind_plots <- specify_individual_plots(all_scenarios, std_ind_figures, defaults)
    
  # for the 5-year simple table
  # value are the scenario group variable names (should be same as above)
  # the names are the new names that should show up in the table in case you need 
  # to add a footnote or longer name
  # this is the order they will show up in the table, so list the newest run 
  # second there should only be 2 scenarios
  simple_5yr <- list(
    ss5 = c(
      "Aug 2019 - Full - Fixed" = "a", "Nov 2019 - DNF" = "b"
    ),
    # this should either be a footnote corresponding to one of the ss5 names or NA
    tableFootnote = NA,
    # years to use for the simple 5-year table
    yy5 = 2020:2024
  )
  
  # heatmap ---------------
  # use this to select which scenarios are shown in the heat map, and what those
  # scenarios should be labeled as in the heatmap. The names should be existing
  # scenario names, and the values are what they will be labeled as in the heatmap
  heatmap <- list(
    scenarios = c(
      "Aug 2019 - Full - Fixed" = "Full Hydrology", 
      "Aug 2019 - ST - Fixed" = "Stress Test Hydrology"
    ),
    title = "August 2019 CRSS (v2)*",
    years = 2020:2026,
    caption = "*August 2019 CRSS (v2) fixed an error found in the original August 2019 results."
  )
  
  # the mainScenGroup is the scenario to use when creating the current month's 
  # 5-year table, etc. In the plots, we want to show the previous months runs,
  # but in the tables, we only want the current month run. This should match names
  # in scens and icList
  mainScenGroup <- "Aug 2019 - Full - Fixed"
  names(mainScenGroup) <- mainScenGroup
  scenarios[['mainScenGroup']] = mainScenGroup
  
  # select which scenarios are plotted ------------- 
  # and the colors that are used for plotting 
  plot_group <- list(
    # the scenarios to show in Mead/Powell 10/50/90 plots
    # and the crit stats plots
    plot_scenarios = c("Aug 2019 - ST - Fixed", "Aug 2019 - ST - w/Bug")
    # set plotting colors (optional)
    # use scales::hue_pal()(n) to get default ggplot colors
    #plot_colors <- c("#F8766D", "#00BFC4")
  )
  
  if (!exists("plot_colors", where = plot_group) & 
      length(plot_group[["plot_scenarios"]]) >= 1) {
    plot_group[["plot_colors"]] <- scales::hue_pal()(
      length(plot_group[["plot_scenarios"]])
    )
    
    #names(plot_group[["plot_colors"]]) <- plot_group[["plot_scenarios"]]
  }
  
  #TODO: change so plot_colors does not have to be specified; if it isn't then
  # what happens?
  
  names(plot_group$plot_colors) <- plot_group$plot_scenarios
  
  # clouds --------------------------------
  
  # TODO: update this so that scen_labs does not have to be specified, and if it
  # isn't, then just the scenarios are used.
  clouds <- list(
    # scenarios to include in cloud
    scenarios = c("Aug 2019 - Full - Fixed", "Aug 2019 - ST - Fixed"),
    scen_labs = c("Full Hydrology", 
                  "Stress Test Hydrology"),
    # should default to '' if it is not specified
    title_append = "from August 2019 CRSS (v2)*",
    # should be NULL if not specified. not ''
    caption = "*August 2019 CRSS (v2) fixed an error found in the original August 2019 results."
  )
  
  assert_that(
    length(clouds$scen_labs) == length(clouds$scenarios),
    msg = "clouds scen_labs needs to be the same length as the scenarios."
  )
  
  # mead pe scatter parameters -------------------------------
  # plot a single year of Mead PE
  mead_pe_scatter <- list(
    year = 2019,
    # peScatterData should be set to either MTOM or CRSS
    # if relying on combined run, then this is likely MTOM; if using a CRSS only 
    # run, then likely set to CRSS
    model = 'MTOM',
    scenario = "June Stress Test - IG"
  )
  
  # conditions leading to shortage --------------------------- 
  shortage_conditions <- list(
    # string should be either CRSS or MTOM
    model = "CRSS", 
    scenario = "Aug 2018 - IG",
    # yearToAnalyze is used in the plot labeling. This is typically the first year
    # of the MTOM run, e.g., 2017 for a January 2017 MTOM run, or the year before
    # the first year of shortage for a CRSS run, i.e., it uses the December elev.
    # at MEad for that year, and the OND release from that year from Powell
    year = 2019,
    color_var = "mwdIcs",
    subtitle = "Results from the August 2018 CRSS run, based on projected December 31, 2018 conditions from the August 2018 24-Month Study.",
    # specify the segment start and stop c(xstart, xend, ystart, yend)
    segment_locs = c(16.9, 14.5, 1055, 1056.4),
    # specify the annotation location: c(x, y)
    annotaion_loc = c(17, 1054)
    
    # TODO: test MTOM still works
    # model = "MTOM",
    # scenario = NA, # should be NA for MTOM
    # year = 2019,
    # color_var = "WYRelease",
    # subtitle = 'Results from the January 2018 MTOM run based on the January 3, 2017 CBRFC forecast',
    # see doc/README for instructions for how to create this csv file
    # res_file = paste0(folders$CRSSDIR,'/MTOM/FirstYearCondMTOM/Jan2018MTOMResults.csv') 
  )
  
  assert_that(
    shortage_conditions$model %in% c("CRSS", "MTOM"), 
    msg = "The shortage conditions model should either be 'MTOM' or 'CRSS'"
  )
  
  if (shortage_conditions$model == "CRSS") {
    assert_that(
      is.null(shortage_conditions$res_file),
      msg = "`res_file` should be null when model is CRSS"
    )
  
    shortage_conditions[['res_file']] <- file.path(
      folders$CRSSDIR,'results', folders$crss_month, 'tempData'
    )
    
    assert_that(
      length(shortage_conditions$scenario) == 1,
      msg = "Only one scenario should be specified"
    )
    
    assert_that(
      shortage_conditions$color_var %in% c("mwdIcs", "WYRelease"),
      msg = paste0(
        "When using CRSS to determine conditions leading to shortage,\n",
        "the `color_var` should be either 'mwdIcs', or 'WYRelease'."  
      )
    )

    # the label for the percent of average
    shortage_conditions[['lb_label']] <- "Total LB natural inflow percent\nof average (1906-2015)"
  } else {
    # have already determined that it is either CRSS or MTOM,so must be MTOM
    
    assert_that(
      shortage_conditions$color_var %in% c("WYRelease"),
      msg = paste0(
        "When using MTOM to determine conditions leading to shortage,\n",
        "the `color_var` must be 'WYRelease'."  
      )
    )
    
    assert_that(
      is.na(shortage_conditions$scenario), 
      msg = "For MTOM, the scenario should be `NA`"
    )
    
    # the label for the percent of average
    shortage_conditions[['lb_label']] <- 'LB total side inflow percent\nof average (1981-2015)'
  }
  
  shortage_conditions[['title']] <- paste(
    'Conditions Leading to a Lower Basin Shortage in',
    shortage_conditions$year + 1
  )
  
  # return ---------------
  list(
    process_data = process_data,
    create_figures = create_figures,
    folders = folders,
    defaults = defaults,
    scenarios = scenarios,
    simple_5yr = simple_5yr,
    heatmap = heatmap,
    plot_group = plot_group,
    clouds = clouds,
    mead_pe_scatter = mead_pe_scatter,
    shortage_conditions = shortage_conditions,
    ind_plots = ind_plots,
    scen_tree = all_scenarios
  )
}
