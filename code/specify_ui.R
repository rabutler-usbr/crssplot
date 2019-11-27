library(assertthat)
source("code/create_scenario.R")
# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# i_folder is a path to the top level crss directory that contains the model 
# output it could be the same as CRSSDIR, but is allowed to be different so that 
# you can read model output from the server, but save figures locally.

specify_ui <- function()
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
    
    pe_clouds = TRUE,
    heatmap = FALSE,
    # uses main scen group for now
    std_ind_tables = FALSE,
    std_ind_figures = FALSE
  )
  
  # ** make sure CRSS_DIR is set correctly before running
  folders <- list(
    i_folder = "C:/alan/CRSS/tmp_output",
    CRSSDIR = Sys.getenv("CRSS_DIR"),
    # set crssMonth to the month CRSS was run. data and figures will be saved in 
    # a folder with this name
    crss_month = "dev",
    pdf_name = 'ig_vs_na.pdf',
    # inserted onto some files. Can be ''
    extra_label = ""
  )
  
  # scenarios are orderd model,supply,demand,policy,initial conditions 
  # (if initial conditions are used) scens should be a list, each entry is a 
  # scenario group name, and the entry is a character vector of length 1 to n of 
  # individual scenarios. all of the values in each entry of the list are combined 
  # together and processed as one scenario group. So for a run that has 30 initial 
  # conditions, all 30 runs are averaged/combined together. The names in the scens 
  # list (scenario Groups) will be the Scenario names that show up on plots.
  
  # *** the names of scens, icList, and icMonth should all match.
  
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
    annText = 'Results from August 2019 CRSS Run',
    end_year = 2060
  )
  # TODO: update so that these are computed if not specified
  # years to show the crit stats figures  
  defaults[['plot_yrs']] <- 2020:defaults$end_year 
  # years to show the Mead/Powell 10/50/90 figures for
  defaults[['pe_yrs']] <- 2019:defaults$end_year
  
  # specify the scenarios -------------------------
  all_scenarios <- c(
    create_scenario(
      "Aug 2018 - IG",
      scen_folders = "2018/Aug2018_2019,DNF,2007Dems,IG,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2019
    ),
    create_scenario(
      "Aug 2018 - NA",
      scen_folders = "2018/Aug2018_2019,DNF,2007Dems,NA,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2019
    ),
    create_scenario(
      "June Stress Test - IG", 
      scen_folders = rw_scen_gen_names(
        "2019/Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP",
        paste0("Trace", 4:8),
        "DCP_Cons"
      ), 
      ic = file.path(
        folders$CRSSDIR, 
        "dmi/InitialConditions/june_2019/MtomToCrss_Monthly.xlsx"
      ), 
      start_year = 2020
    )
  )
 
  # old specification of scenarios ------------------ 
  # now handled by scenario_to_vars()
  # scens <- list(
  #   "Aug 2018 - IG" = "2018/Scenario/Aug2018_2019,DNF,2007Dems,IG,Most",
  #   "Aug 2018 - NA" = "2018/Scenario/Aug2018_2019,DNF,2007Dems,NA,Most",
  #   "Aug 2019 - IG" = "2019/Scenario/Aug2019_2020,DNF,2007Dems,IG_DCP,Most",
  #   "Aug 2019 - NA-9101" = "2019/Scenario_dev/Aug2019_2020_9101,DNF,2007Dems,NA_DCP_v9001,Most",
  #   "NA-9002" = "2019/Scenario_dev/Aug2019_2020_9002,DNF,2007Dems,NA_DCP,Most"
  # )
  # 
  # # for each scenario group name, it should be either 2 numbers or 2 file paths, 
  # # both ordered powell, then mead.
  # 
  # jun_path <- file.path(
  #   CRSSDIR, 
  #   "dmi/InitialConditions/june_2019/MtomToCrss_Monthly.xlsx"
  # )
  # 
  # icList <- list(
  #   #"June 2019" = jun_path,
  #   "Aug 2019 - IG" = c(3618.56, 1089.40),
  #   "Aug 2019 - NA-9101" = c(3618.56, 1089.40),
  #   "NA-9002" = c(3618.56, 1089.40),
  #   "Aug 2018 - IG" = c(3586.55, 1079.5),
  #   "Aug 2018 - NA" = c(3586.55, 1079.5)
  #   #"August 2018" = c(3586.55, 1079.50),
  #   #"January 2019" = c(3581.85, 1081.46)
  #   # "January 2019 - 110" = file.path(
  #   #   CRSSDIR,
  #   #   "dmi/InitialConditions/jan_2019/MtomToCrss_Monthly.xlsx"
  #   # ),
  #   #"June 2019 - Most" = c(3619.82, 1088.09),
  #   #"June 2019 - No DCP" = c(3619.56, 1085.88),
  #   #"January 2019" = jan_path,
  #   #"June 2019" = jun_path,
  #   #"June 2019 - Stress Test" = jun_path
  #   #"June 2019 - Stress Test - No DCP" = c(3619.56, 1085.88)
  # )
  # 
  # # The month in YY-Mmm format of the intitial condtions for each scenario group
  # icMonth <- c(
  #   "Aug 2019 - IG" = "19-DEC",
  #   "Aug 2019 - NA-9101" = "19-DEC",
  #   "NA-9002" = "19-DEC",
  #   "Aug 2018 - IG" = "18-DEC",
  #   "Aug 2018 - NA" = "18-DEC"
  # )
  
  # convert all_scenarios to the different variables --------------------
  scenarios <- scenario_to_vars(all_scenarios)
    
  # for the 5-year simple table
  # value are the scenario group variable names (should be same as above)
  # the names are the new names that should show up in the table in case you need 
  # to add a footnote or longer name
  # this is the order they will show up in the table, so list the newest run 
  # second there should only be 2 scenarios
  simple_5yr <- list(
    ss5 = c(
      "Aug 2018 - IG" = "a", "Aug 2018 - NA" = "b"
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
      "Aug 2018 - IG" = "August 2018 IGs Extend", 
      "Aug 2018 - NA" = "August 2018 No Action"
    ),
    title = "IG vs. No Action in August 2018",
    years = 2020:2026
  )
  
  # the mainScenGroup is the scenario to use when creating the current month's 
  # 5-year table, etc. In the plots, we want to show the previous months runs,
  # but in the tables, we only want the current month run. This should match names
  # in scens and icList
  mainScenGroup <- "Aug 2018 - IG"
  names(mainScenGroup) <- mainScenGroup
  scenarios[['mainScenGroup']] = mainScenGroup
  
  # select which scenarios are plotted ------------- 
  # and the colors that are used for plotting 
  plot_group <- list(
    # the scenarios to show in Mead/Powell 10/50/90 plots
    # and the crit stats plots
    plot_scenarios = c("Aug 2018 - IG", "Aug 2018 - NA", 
                       "June Stress Test - IG"),
    # set plotting colors (optional)
    # use scales::hue_pal()(n) to get default ggplot colors
    #plot_colors <- c("#F8766D", "#00BFC4")
    plot_colors = scales::hue_pal()(3)
  )
  
  #TODO: change so plot_colors does not have to be specified; if it isn't then
  # what happens?
  
  names(plot_group$plot_colors) <- plot_group$plot_scenarios
  
  # clouds --------------------------------
  
  # TODO: update this so that scen_labs does not have to be specified, and if it
  # isn't, then just the scenarios are used.
  clouds <- list(
    # scenarios to include in cloud
    scenarios = c("Aug 2018 - IG", "Aug 2018 - NA"),
    scen_labs = c("August 2018 - IG extended", 
                  "Augsut 2018 - Revert to 2007 NA Alternative")
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
    shortage_conditions = shortage_conditions
  )
}
