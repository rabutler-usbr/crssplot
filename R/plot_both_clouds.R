
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
      
      powellCloud <- add_dcp_line(powellCloud) %>%
        add_ig_line() %>%
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

      meadCloud <- add_dcp_line(meadCloud) %>%
        add_ig_line() %>%
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
    }
  }
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

add_dcp_line <- function(gg) {
  
  if (2019 %in% gg_year_range(gg)) {
  
  min_y <- floor(gg_y_range(gg)[1]/50)*50
  
  gg <- gg + 
    geom_vline(xintercept = 2019, size = 0.8, color = '#808080') +
    annotate(
      "text", 
      x = 2019.1, y = min_y, 
      label = 'Adoption of the Drought\nContingency Plan', 
      size = 2.9, hjust = 0, fontface = "bold", color = '#303030'
    )
  }
  
  gg
}

add_ig_line <- function(gg) {
  if (2007 %in% gg_year_range(gg)) {
    
    min_y <- floor(gg_y_range(gg)[1]/50)*50
    
    gg <- gg +
      geom_vline(
        xintercept = 2007, 
        size = 0.8, 
        color = '#808080'
      ) + 
      annotate(
        "text", x = 2007.1, y = min_y, 
        label = 'Adoption of the 2007\nInterim Guidelines', 
        size = 2.9, 
        hjust = 0,
        fontface = "bold", 
        color = '#303030'
      )
  }
  
  gg
}

add_powell_ig_tiers <- function(gg) {
  yrs <- gg_year_range(gg)
  EQLine <- as.data.frame(utils::read.csv(
    system.file('extdata/EQLine.csv', package = "crssplot")
  ))
  
  if (2007 %in% yrs) {
    ig_start <- 2007
    ig_label <- 2007.1
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  ann_size <- 2.9
  line_size <- 0.6
  
  max_y <- ceiling(gg_y_range(gg)[2]/50)*50
  
  gg <- gg +
    # Adding lines and annotation for Powell ops - plot only if Switch = True
    geom_line(
      data = filter(EQLine, Year %in% yrs), 
      aes(x = Year, y = EQLine), 
      size = line_size,
      color = '#808080', 
      linetype = 3
    ) + 
    annotate(
      "text", x = ig_label, y = max_y, 
      label = "Equalization Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 3575, xend = 2026, yend = 3575, 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) + 
    geom_segment(
      x = ig_start, y = 3525, xend = 2026, yend = 3525, 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 3520, 
      label = "Lower Elevation Balancing Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +    
    annotate(
      "text", x = ig_label, y = 3570, 
      label = "Mid Elevation Release Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) + 
    annotate(
      "text", x = ig_label, y = 3582, 
      label = "Upper Elevation Balancing Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    )
}

add_mead_ig_tiers <- function(gg) {
  
  yrs <- gg_year_range(gg)
  
  if (2007 %in% yrs) {
    ig_start <- 2007
    ig_label <- 2007.1
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  ann_size <- 2.9
  line_size <- 0.6
  
  max_y <- ceiling(gg_y_range(gg)[2]/50)*50
  
  gg <- gg +
    geom_segment(
      x = ig_start, y = 1075, xend = 2026, yend = 1075, 
      size = line_size, 
      color ='#808080', linetype = 3
    ) + 
    annotate(
      "text", x = ig_label, y = 1070, 
      label = "Level 1 Shortage Condition", 
      size = ann_size, hjust = 0, fontface = "italic", color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 1050, xend = 2026, yend = 1050, 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1045, 
      label = "Level 2 Shortage Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 1025, xend = 2026, yend = 1025, 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1020, 
      label = "Level 3 Shortage Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 1145, xend = 2026, yend = 1145, 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1140, 
      label = "Normal or ICS Surplus Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    annotate(
      "text", x = ig_label, y = max_y, 
      label = "Surplus Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    )
  
  gg
}

gg_year_range <- function(gg) {
  tmp <- layer_scales(gg)$x$range$range
  tmp <- tmp[1]:tmp[2]
  
  tmp
}

gg_y_range <- function(gg) {
  layer_scales(gg)$y$range$range
}

