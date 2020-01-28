library(assertthat)
source("code/create_scenario.R")
# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# i_folder is a path to the top level crss directory that contains the model 
# output it could be the same as CRSSDIR, but is allowed to be different so that 
# you can read model output from the server, but save figures locally.

dev_ui <- function()
{
  # switches to read data. if you've already read the data in from rdfs once, 
  # you may be able to set this to FALSE, so it's faster
  process_data <- list(
    sys_cond_data = FALSE,
    pe_data = FALSE,
    csd_data = FALSE,
    crss_short_cond_data = FALSE
  )
  
  # ** make sure CRSS_DIR is set correctly before running
  folders <- list(
    i_folder = "M:/Shared/CRSS",
    CRSSDIR = "C:/alan/CRSS/CRSS.2020", #Sys.getenv("CRSS_DIR"),
    # set crssMonth to the month CRSS was run. data and figures will be saved in 
    # a folder with this name
    crss_month = "jan2020_obs",
    pdf_name = 'dev.pdf',
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
    end_year = 2060
  )
  # TODO: update so that these are computed if not specified
  # years to show the crit stats figures  
  defaults[['plot_yrs']] <- 2019:defaults$end_year 
  # years to show the Mead/Powell 10/50/90 figures for
  defaults[['pe_yrs']] <- 2018:defaults$end_year
  
  # specify the scenarios -------------------------
  all_scenarios <- c(

    # November scenarios
    # create_scenario(
    #   "Nov 2019 - IG DNF",
    #   scen_folders = "2019/Scenario_dev/Nov2019_2020_9005,DNF,2007Dems,IG_DCP_v4.2.0.9000,Most", 
    #   ic = c(3608.24, 1087.8), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Nov 2019 - NA DNF",
    #   scen_folders = "2019/Scenario_dev/Nov2019_2020_9005,DNF,2007Dems,NA_v9002,Most", 
    #   ic = c(3608.24, 1087.8), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Nov 2019 - IG ST",
    #   scen_folders = "2019/Scenario_dev/Nov2019_2020_9005,ISM1988_2017,2007Dems,IG_DCP_v4.2.0.9000,Most", 
    #   ic = c(3608.24, 1087.8), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Nov 2019 - NA ST",
    #   scen_folders = "2019/Scenario_dev/Nov2019_2020_9005,ISM1988_2017,2007Dems,NA_v9002,Most", 
    #   ic = c(3608.24, 1087.8), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # 
    # # January 2020 scenarios
    # create_scenario(
    #   "Jan 2020 - IG DNF (2017)",
    #   scen_folders = "2020/Scenario_dev/Jan2020_2020_v4.2.0.9000,DNF,2007Dems,IG_DCP_4.3.0.9000,Most", 
    #   ic = c(3608.74, 1090.49), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Jan 2020 - NA DNF (2017)",
    #   scen_folders = "2020/Scenario_dev/Jan2020_2020_v4.2.0.9000,DNF,2007Dems,NA_4.3.0.9000,Most", 
    #   ic = c(3608.74, 1090.49), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Jan 2020 - IG ST (2017)",
    #   scen_folders = "2020/Scenario_dev/Jan2020_2020_v4.2.0.9000,ISM1988_2017,2007Dems,IG_DCP_4.3.0.9000,Most", 
    #   ic = c(3608.74, 1090.49), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Jan 2020 - NA ST (2017)",
    #   scen_folders = "2020/Scenario_dev/Jan2020_2020_v4.2.0.9000,ISM1988_2017,2007Dems,NA_4.3.0.9000,Most", 
    #   ic = c(3608.74, 1090.49), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # 
    # # January 2020 scenarios with 2018 NF
    # create_scenario(
    #   "Jan 2020 - IG DNF",
    #   scen_folders = "2020/Scenario/Jan2020_2020,DNF,2007Dems,IG_DCP,Most", 
    #   ic = c(3608.74, 1090.49), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    # create_scenario(
    #   "Jan 2020 - NA DNF",
    #   scen_folders = "2020/Scenario/Jan2020_2020,DNF,2007Dems,NA,Most", 
    #   ic = c(3608.74, 1090.49), 
    #   start_year = 2020,
    #   std_ind_tables = FALSE,
    #   std_ind_figures = FALSE
    # ),
    create_scenario(
      "Jan 2020 - IG ST",
      scen_folders = "2020/Scenario/Jan2020_2020,ISM1988_2018,2007Dems,IG_DCP,Most", 
      ic = c(3608.74, 1090.49), 
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = TRUE,
      mead_pe_scatter = list(
        year = 2020,
        # peScatterData should be set to either MTOM or CRSS
        # if relying on combined run, then this is likely MTOM; if using a CRSS only 
        # run, then likely set to CRSS
        model = 'CRSS'
      ),
      
      shortage_conditions = list(
        # string should be either CRSS or MTOM
        model = "CRSS", 
        # year is used in the plot labeling. This is typically the first year
        # of the MTOM run, e.g., 2017 for a January 2017 MTOM run, or the year before
        # the first year of shortage for a CRSS run, i.e., it uses the December elev.
        # at MEad for that year, and the OND release from that year from Powell
        year = 2020,
        color_var = "mwdIcs",
        subtitle = "Results from the August 2018 CRSS run, based on projected December 31, 2018 conditions from the August 2018 24-Month Study.",
        # specify the segment start and stop c(xstart, xend, ystart, yend)
        segment_locs = c(16.9, 14.5, 1055, 1056.4),
        # specify the annotation location: c(x, y)
        annotation_loc = c(2, 1075)
      )
    ),
    create_scenario(
      "Jan 2020 - NA ST",
      scen_folders = "2020/Scenario/Jan2020_2020,ISM1988_2018,2007Dems,NA,Most", 
      ic = c(3608.74, 1090.49), 
      start_year = 2020,
      std_ind_tables = TRUE,
      std_ind_figures = TRUE,
      # mead pe scatter parameters -------------------------------
      # plot a single year of Mead PE

      mead_pe_scatter = list(
        year = 2020,
        # peScatterData should be set to either MTOM or CRSS
        # if relying on combined run, then this is likely MTOM; if using a CRSS only 
        # run, then likely set to CRSS
        model = 'CRSS',
        ann_text = "These are from the NA - ST run",
        add_threshold_stats = FALSE
      ),
      
      shortage_conditions = list(
        # string should be either CRSS or MTOM
        model = "CRSS", 
        # year is used in the plot labeling. This is typically the first year
        # of the MTOM run, e.g., 2017 for a January 2017 MTOM run, or the year before
        # the first year of shortage for a CRSS run, i.e., it uses the December elev.
        # at MEad for that year, and the OND release from that year from Powell
        year = 2020,
        color_var = "mwdIcs",
        subtitle = "Results from the August 2018 CRSS run, based on projected December 31, 2018 conditions from the August 2018 24-Month Study.",
        # specify the segment start and stop c(xstart, xend, ystart, yend)
        segment_locs = c(16.9, 14.5, 1055, 1056.4),
        # specify the annotation location: c(x, y)
        annotation_loc = c(17, 1054)
      )
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
    "Jan 2020 - IG DNF" = list(
      ann_text = "Results from the January 2020 (observed initial conditions) run",
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
  
  # comparison plots ------------- 
  # and the colors that are used for plotting 
  # the scenarios to show in Mead/Powell 10/50/90 plots
  # and the crit stats plots
  # list of lists. each list has one required entry: plot_scenarios and one 
  # optional entry: plot_colors. The list can be named, or unnamed.
  plot_group <- list(
    # "ig_v_na" = list(
    #   plot_scenarios = c("Aug 2018 - IG", "Aug 2018 - NA", "Aug 2019 - IG Dev",
    #                      "Aug 2019 - NA", "Nov 2019 - IG DNF", "Nov 2019 - NA DNF"),
    #   csd_ann = list(
    #     create = TRUE,
    #     years = 2020:2035
    #   ),
    #   std_comparison = list(
    #     create = TRUE,
    #     years = 2020:2060
    #   )
    #   # set plotting colors (optional)
    #   # use scales::hue_pal()(n) to get default ggplot colors
    #   #plot_colors <- c("#F8766D", "#00BFC4")
    # ),
    # 
    # "dnf_comp" = list(
    #   plot_scenarios = c("Nov 2019 - IG DNF", "Nov 2019 - NA DNF", 
    #                      "Jan 2020 - IG DNF", "Jan 2020 - NA DNF"),
    #   std_comparison = list(
    #     create = TRUE,
    #     years = 2020:2060
    #   ),
    #   csd_ann = list(
    #     create = TRUE,
    #     years = 2020:2040
    #   )
    # ),
    # "st_comp" = list(
    #   plot_scenarios = c("Nov 2019 - IG ST", "Nov 2019 - NA ST", 
    #                      "Jan 2020 - IG ST", "Jan 2020 - NA ST"),
    #   csd_ann = list(
    #     create = TRUE,
    #     years = 2020:2040
    #   )
    # ),
    #"jan_comp" = list(
     # plot_scenarios = c("Jan 2020 - IG ST", "Jan 2020 - IG DNF"),
      # heat = list(
      #   create = TRUE,
      #   scen_names = c(
      #     "Jan 2020 - IG DNF" = "Full Hydrology", 
      #     "Jan 2020 - IG ST" = "Stress Test Hydrology"
      #   ),
      #   title = "Jan 2020 CRSS",
      #   years = 2020:2026,
      #   caption = "This and that"
      # ),
    #   cloud = list(
    #     create = TRUE, 
    #       # scenarios to include in cloud
    #       scen_labs = c("Stress Test Hydrology", "Full Hydrology"),
    #       # should default to '' if it is not specified
    #       title_append = "from January 2020 CRSS",
    #       # should be NULL if not specified. not ''
    #       caption = "This and that"
    #   )
    # )#,
    # "nov_comp" = list(
    #   plot_scenarios = c("Nov 2019 - IG DNF", "Nov 2019 - IG ST"),
    #   heat = list(
    #     create = TRUE,
    #     scen_names = c(
    #       "Nov 2019 - IG DNF" = "Full Hydrology", 
    #       "Nov 2019 - IG ST" = "Stress Test Hydrology"
    #     ),
    #     title = "November 2019 CRSS",
    #     years = 2020:2026,
    #     caption = "This and that"
    #   )
    # )
    # ig_hydro = list(
    #   plot_scenarios = c("Jan 2020 - IG DNF", "Jan 2020 - IG DNF (2017)"),
    #   cloud = list(
    #     create = TRUE,
    #     scen_labs = c("2018 NF", "2017 NF"),
    #     title_append = "from January 2020 (observed) CRSS",
    #     caption = NULL
    #   ),
    #   std_comparison = list(
    #     create = TRUE,
    #     years = 2020:2060
    #   ),
    #   csd_ann = list(
    #     create = TRUE,
    #     years = 2020:2040
    #   )
    # ),
    # nov2Jan = list(
    #   plot_scenarios = c("Nov 2019 - IG DNF", "Jan 2020 - IG DNF"),
    #   simple_5yr = list(
    #     create = TRUE,
    #     scen_names = c(
    #       "Nov 2019 - IG DNF" = "Nov 2019", "Jan 2020 - IG DNF" = "Jan 2020"
    #     ),
    #     # this should either be a footnote corresponding to one of the names or NA
    #     footnote = NA,
    #     # years to use for the simple 5-year table
    #     years = 2020:2024
    #   )
    # ),
    # dnf_comp = list(
    #   plot_scenarios = c("Jan 2020 - IG DNF (2017)", "Jan 2020 - IG DNF"),
    #   simple_5yr = list(
    #     create = TRUE,
    #     scen_names = c(
    #       "Jan 2020 - IG DNF (2017)" = "Jan - 2017 NF*", "Jan 2020 - IG DNF" = "Jan - 2018 NF"
    #     ),
    #     # this should either be a footnote corresponding to one of the names or NA
    #     footnote = NA,
    #     # years to use for the simple 5-year table
    #     years = 2020:2024
    #   )
    # )
  )

  plot_group <- check_plot_group_colors(plot_group) %>%
    check_plot_group_type("std_comparison") %>%
    check_plot_group_type("csd_ann") %>%
    check_plot_group_type("heat") %>%
    check_plot_group_type("cloud") %>%
    check_plot_group_type("simple_5yr") %>%
    check_heat_scen_names() %>%
    check_cloud_scen_names() %>%
    check_simple5yr_scen_names()
  

  # TODO: update this so that scen_labs does not have to be specified, and if it
  # isn't, then just the scenarios are used.
  # clouds <- list(
  #   # scenarios to include in cloud
  #   scenarios = c("Aug 2018 - IG", "Aug 2018 - IG"),
  #   scen_labs = c("Full Hydrology", 
  #                 "Stress Test Hydrology"),
  #   # should default to '' if it is not specified
  #   title_append = "from August 2019 CRSS (Updated December 2019)",
  #   # should be NULL if not specified. not ''
  #   caption = "*There was an error in the original August 2019 CRSS results. The December 2019 Update fixes the error."
  # )
  
  # return ---------------
  list(
    process_data = process_data,
    folders = folders,
    defaults = defaults,
    scenarios = scenarios,
    heatmap = heatmap,
    plot_group = plot_group,
    ind_plots = ind_plots,
    scen_tree = all_scenarios
  )
}
