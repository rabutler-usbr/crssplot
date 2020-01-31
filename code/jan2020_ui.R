library(assertthat)
library(RWDataPlyr)
source("code/create_scenario.R")
# script should create everything necessary for the results in order
# CRSSDIR is the CRSS_DIR environment variable that will tell the code where to
# store the intermediate data and figures/tables created here
# i_folder is a path to the top level crss directory that contains the model 
# output it could be the same as CRSSDIR, but is allowed to be different so that 
# you can read model output from the server, but save figures locally.

jan2020_ui <- function()
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
    i_folder = "C:/Users/rrbutler/Documents/crss/crss.offc/Scenario/",
    CRSSDIR = Sys.getenv("CRSS_DIR"), #"C:/alan/CRSS/CRSS.2020"
    # set crssMonth to the month CRSS was run. data and figures will be saved in 
    # a folder with this name
    crss_month = "jan2020",
    pdf_name = 'jan2020vsAug2019.pdf',
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
    end_year = 2060,
    start_year = 2019
  )
  # TODO: update so that these are computed if not specified
  # years to show the crit stats figures  
  defaults[['plot_yrs']] <- defaults$start_year:defaults$end_year 
  # years to show the Mead/Powell 10/50/90 figures for
  defaults[['pe_yrs']] <- (defaults$start_year - 1):defaults$end_year
  
  # specify the scenarios -------------------------
  all_scenarios <- c(

    create_scenario(
      "Aug 2019 (Update) - DNF",
      scen_folders = "Aug2019_2020_v4.1.1,DNF,2007Dems,IG_DCP_v4.2.0,Most",
      ic = c(3618.56, 1089.4),
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),

    create_scenario(
      "Aug 2019 (Update) - ST",
      scen_folders = "Aug2019_2020_v4.1.1,ISM1988_2017,2007Dems,IG_DCP_v4.2.0,Most",
      ic = c(3618.56, 1089.4),
      start_year = 2020,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),

    create_scenario(
      "Jan 2020 - DNF",
      scen_folders = rw_scen_gen_names(
        "Jan2020_2021,DNF,2007Dems,IG_DCP",
        paste0("Trace", 4:38)
      ),
      ic = file.path(
        folders$CRSSDIR,
        "dmi/InitialConditions/jan_2020/MtomToCrss_Monthly.xlsx"
      ),
      start_year = 2021,
      std_ind_tables = TRUE,
      std_ind_figures = TRUE
    ),

    create_scenario(
      "Jan 2020 - ST",
      scen_folders = rw_scen_gen_names(
        "Jan2020_2021,ISM1988_2018,2007Dems,IG_DCP",
        paste0("Trace", 4:38)
      ),
      ic = file.path(
        folders$CRSSDIR,
        "dmi/InitialConditions/jan_2020/MtomToCrss_Monthly.xlsx"
      ),
      start_year = 2021,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),

    create_scenario(
      "Jan 2020 - DNF (MTOM most)",
      scen_folders = "Jan2020_2021,DNF,2007Dems,IG_DCP,MTOM_Most",
      ic = c(3613.78, 1084.89),
      start_year = 2021,
      std_ind_tables = TRUE,
      std_ind_figures = TRUE
    ),

    create_scenario(
      "Jan 2020 - DNF (MTOM most) - NA",
      scen_folders = "Jan2020_2021,DNF,2007Dems,NA,MTOM_Most",
      ic = c(3613.78, 1084.89),
      start_year = 2021,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),

    create_scenario(
      "Jan 2020 - ST (MTOM most)",
      scen_folders = "Jan2020_2021,ISM1988_2018,2007Dems,IG_DCP,MTOM_Most",
      ic = c(3613.78, 1084.89),
      start_year = 2021,
      std_ind_tables = FALSE,
      std_ind_figures = FALSE
    ),

    create_scenario(
      "Jan 2020 - ST (MTOM most) - NA",
      scen_folders = "Jan2020_2021,ISM1988_2018,2007Dems,NA,MTOM_Most",
      ic = c(3613.78, 1084.89),
      start_year = 2021,
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
    "Jan 2020 - DNF" = list(
      ann_text = "Results from the January 2020 full hydrology run",
      end_year = 2060
    ),
    "Jan 2020 - ST" = list(
      ann_text = "Results from the January 2020 stress test run", 
      end_year = 2051
    ),
    "Jan 2020 - DNF (MTOM most)" = list(
      ann_text = "Results from Jan 2020 MTOM Most full hydrology",
      end_year = 2060
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
    janVAug = list(
      plot_scenarios = c("Jan 2020 - DNF (MTOM most)",  "Aug 2019 (Update) - DNF"),
      simple_5yr = list(
        create = TRUE,
        scen_names = c( "Jan 2020 - DNF (MTOM most)" = "Jan 2020 - DNF (MTOM most)", 
                        "Aug 2019 (Update) - DNF" =  "Aug 2019 (Update) - DNF"),
        years = 2020:2025,
        footnote = NA
      ),
      std_comparison = list(
        create = TRUE,
        years = 2020:2060
      ),
      csd_ann = list(
        create = TRUE,
        years = 2021:2040
      )
    )
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
    plot_group = plot_group,
    ind_plots = ind_plots,
    scen_tree = all_scenarios
  )
}
