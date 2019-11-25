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
    sys_cond_data = TRUE,
    pe_data = TRUE,
    crss_short_cond_data = FALSE
  )
  
  # "switches" to create/not create different figures
  # typical figures
  create_figures <- list(
    standard_figures = TRUE,
    simple_5yr_table = FALSE,
    
    # optional figures/tables
    short_conditions = FALSE,
    conditional_probs = FALSE,
    pe_scatter_fig = FALSE,
    
    #NOTE:For code to work correctly the main scene group needs to be the DNF dataset.
    dnf_st_boxplot = FALSE, 
    pe_clouds = FALSE
  )
  
  # ** make sure CRSS_DIR is set correctly before running
  folders <- list(
    i_folder = "M:/Shared/CRSS/",
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
  
  defaults <- list(
    # in the comma seperated scenario folder names, currently the 5th entry is  
    # the initial conditions entry
    # update if for some reason the scenario naming convention has changed
    ic_dim_number = 5,
    # setting to NULL will not wrap legend entries at all
    legendWrap <- 20,
    # how to label the color scale on the plots
    colorLabel <- 'Scenario'
  )
  
  # specify the scenarios -------------------------
  all_scenarios <- c(
    create_scenario(
      "Aug 2018 - IG",
      scen_folders = "2018/Scenario/Aug2018_2019,DNF,2007Dems,IG,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2019
    ),
    create_scenario(
      "Aug 2018 - NA",
      scen_folders = "2018/Scenario/Aug2018_2019,DNF,2007Dems,NA,Most", 
      ic = c(3618.56, 1089.40), 
      start_year = 2019
    ),
    create_scenario(
      "June Stress Test - IG", 
      scen_folders = rw_scen_gen_names(
        "Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP",
        paste0("Trace", 4:38),
        "DCP_Cons"
      ), 
      ic = file.path(
        CRSSDIR, 
        "dmi/InitialConditions/june_2019/MtomToCrss_Monthly.xlsx"
      ), 
      start_year = 2020
    )
  )
  
  # old specification that is now handled by scenario_to_vars()
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
  scenarios <- scenario_to_vars(scenarios)
    
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
  
  # use this to select which scenarios are shown in the heat map, and what those
  # scenarios should be labeled as in the heatmap. The names should be existing
  # scenario names, and the values are what they will be labeled as in the heatmap
  heatmap <- list(
    scenarios = c(
      "Aug 2018 - IG" = "August 2018 IGs Extend", 
      "Aug 2018 - NA" = "August 2018 No Action"
    ),
    title = "IG vs. No Action in August 2018"
  )
  
  # the mainScenGroup is the scenario to use when creating the current month's 
  # 5-year table, etc. In the plots, we want to show the previous months runs,
  # but in the tables, we only want the current month run. This should match names
  # in scens and icList
  mainScenGroup <- "Aug 2019 - IG"
  names(mainScenGroup) <- mainScenGroup
  
  # text that will be added to figures
  annText <- 'Results from August 2019 CRSS Run' 
  
  
  
  
  # the scenarios to show in Mead/Powell 10/50/90 plots, and the crit stats plots
  #plot_scenarios <- c("June 2019", "January 2019", "June 2019 - No DCP")
  #plot_scenarios <- c("June 2019 - Stress Test", "June 2019 - Stress Test - No DCP")
  plot_scenarios <- c("Aug 2019 - IG", "Aug 2018 - IG", "Aug 2019 - NA-9101", 
                      "Aug 2018 - NA", "NA-9002")
  
  # set plotting colors (optional)
  # use scales::hue_pal()(n) to get default ggplot colors
  plot_colors <- c("#F8766D", "#00BFC4")
  plot_colors <- scales::hue_pal()(length(plot_scenarios))
  #plot_colors <- c("#619CFF", "#F8766D", "#00BA38")
  names(plot_colors) <- plot_scenarios
  
  end_year <- 2060
  
  yrs2show <- 2020:2060 # years to show the crit stats figures
  peYrs <- 2019:2060 # years to show the Mead/Powell 10/50/90 figures for
  
  # More descriptive labels for hydrologies used in the cloud plots
  cloudScen <- c("June 2019","August 2019")#, "June 2019 Base","June 2019 DCP"
  cloudLabs <-c("June 2019" = "June Full Hydrology",
                "August 2019" = "August Full Hydrology"
  )
  
  # mead pe scatter parameters -------------------------------
  # plot a single year of Mead PE
  peScatterYear <- 2019
  # peScatterData should be set to either MTOM or CRSS
  # if relying on combined run, then this is likely MTOM; if using a CRSS only 
  # run, then likely set to CRSS
  peScatterData <- 'CRSS'
  
  conditionsFrom <- "CRSS" # string should be either CRSS or MTOM
  
  # yearToAnalyze is used in the plot labeling. This is typically the first year
  # of the MTOM run, e.g., 2017 for a January 2017 MTOM run, or the year before
  # the first year of shortage for a CRSS run, i.e., it uses the December elev.
  # at MEad for that year, and the OND release from that year from Powell
  yearToAnalyze <- 2019
  if (conditionsFrom == "CRSS") {
    resFile <- file.path(CRSSDIR,'results', crssMonth, 'tempData')
    # set scenario to NA if using MTOM or 
    # to the main scenario folder if using CRSS
    scenario <- scens[[mainScenGroup]]
    
    short_cond_color_var <- "mwdIcs" # WYRelease or mwdIcs
    
    # the label for the percent of average
    lbLabel <- "Total LB natural inflow percent\nof average (1906-2015)"
    shortCondSubTitle <- "Results from the August 2018 CRSS run, based on projected December 31, 2018 conditions from the August 2018 24-Month Study."
  } else if (conditionsFrom == "MTOM") {
    # see doc/README for instructions for how to create this csv file
    resFile <- paste0(CRSSDIR,'/MTOM/FirstYearCondMTOM/Jan2018MTOMResults.csv') 
    scenario <- NA
    short_cond_color_var <- "WYRelease" #only WYRelease
    # the label for the percent of average
    lbLabel <- 'LB total side inflow percent\nof average (1981-2015)'
    shortCondSubTitle <- 'Results from the January 2018 MTOM run based on the January 3, 2017 CBRFC forecast' 
    
  } else {
    stop("Invalid `conditionsFrom` value.")
  }
  
  shortCondTitle <- 'Conditions Leading to a Lower Basin Shortage in 2020'
  
  list(
    process_data = process_data,
    create_figures = create_figures,
    folders = folders,
    defaults = defaults,
    scenarios = scenarios,
    simple_5yr = simple_5yr,
    heatmap = heatmap
  )
}
