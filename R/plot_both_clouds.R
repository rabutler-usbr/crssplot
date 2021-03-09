
plot_both_clouds <- function(pe, ui, folder_paths)
{
  # loop through all plot_groups and if create == TRUE, create the cloud figure
  legendTitle <- "Historical and Median Projected Pool Elevation"
  legendWrap <- ui$defaults$legend_wrap
  
  p_hist <- utils::read.csv(system.file(
    'extdata/HistPowellPE.csv', package = "crssplot"
  ))
  
  m_hist <- utils::read.csv(system.file(
    'extdata/HistMeadPE.csv', package = "crssplot"
  ))
  
  all_clouds <- list()
  
  for (i in seq_along(ui[["plot_group"]])) {
    p_title <- 'Lake Powell End-of-December Elevation'
    m_title <- 'Lake Mead End-of-December Elevation'
    
    if (ui[["plot_group"]][[i]][["cloud"]][["create"]]) {
      
      pg <- ui[["plot_group"]][[i]]
      
      peYrs <- pg[["cloud"]][["years"]]
      scenario <- ui[["plot_group"]][[i]][["plot_scenarios"]]
      scen_labs <- ui[["plot_group"]][[i]][["cloud"]][["scen_names"]]
      plotColors <- get_cloud_colors(ui, i)
      
      if (pg[["cloud"]][["title_append"]] != '') {
        p_title <- paste(p_title, pg[["cloud"]][["title_append"]])
        m_title <- paste(m_title, pg[["cloud"]][["title_append"]])
      }
      
      p_file <- construct_file_name(ui, folder_paths, i, "png_out", "Powell.png")
      m_file <- construct_file_name(ui, folder_paths, i, "png_out", "Mead.png")

      powellCloud <- scens_plot_cloud(
        pe,
        "powell_dec_pe",
        historical = p_hist,
        years = peYrs,
        scenarios = scenario,
        scen_labels = scen_labs,
        plot_colors = plotColors,
        legend_wrap = legendWrap,
        color_label = legendTitle,
        title = p_title
      ) +
        theme_cloud()
      
      powellCloud <- add_dcp_start(powellCloud) %>%
        add_ig_start() %>%
        add_powell_ig_tiers() %>%
        add_logo_vertical(1.395)
      
      ggsave(
        p_file, 
        plot = powellCloud,
        width = 9, 
        height = 6.5, 
        units = "in", 
        dpi = 600
      )
      
      message("   ... saved ", p_file)
      
      meadCloud <- scens_plot_cloud(
        pe,
        "mead_dec_pe",
        historical = m_hist,
        years = peYrs,
        scenarios = scenario,
        scen_labels = scen_labs,
        plot_colors = plotColors,
        legend_wrap = legendWrap,
        color_label = legendTitle,
        title = m_title,
        color_label = "Historical and Median Projected Pool Elevation"
      ) +
        theme_cloud()

      meadCloud <- add_dcp_start(meadCloud) %>%
        add_ig_start() %>%
        add_mead_ig_tiers() %>%
        add_logo_vertical(1.395)
      
      ggsave(
        m_file,
        plot = meadCloud,
        width = 9, 
        height = 6.5, 
        units = "in", 
        dpi = 600
      )
      message("   ... saved ", m_file)
      
      all_clouds[[names(ui[["plot_group"]])[i]]] <- 
        gg_list("powell_cloud" = powellCloud, "mead_cloud" = meadCloud)
    }
  }
  
  pgs_out(all_clouds)
}

get_cloud_colors <- function(ui, pg_i)
{
  if (is.null(ui[["plot_group"]][[pg_i]][["cloud"]][["plot_colors"]])) {
    # use default colors
    ns <- length(ui[["plot_group"]][[pg_i]][["plot_scenarios"]])
    plot_colors <- scales::hue_pal()(ns)
    names(plot_colors) <- ui[["plot_group"]][[pg_i]][["plot_scenarios"]]
  } else {
    plot_colors <- ui[["plot_group"]][[pg_i]][["cloud"]][["plot_colors"]]
  }
  
  # combine the historical, add it first
  plot_colors <- c('Historical Elevation' = "#000000", plot_colors)
  
  plot_colors
}
