
#' Plot range of conditions as cloud
#' 
#' `plotCloudFigs()` plots the 10th and 90th percentiles as a cloud and the 
#' median as a line. It will also add in historical data, and is hard coded
#' to only work for Powell and Mead elevation. Function adds in reclamation 
#' object.
#' 
#' @param zz Data frame. Must have StartMonth, Year, Variable, and Value 
#'   columns.
#'   
#' @param yrs Years to show in the plot. If any exist before the min in `zz`, 
#'   then historical values are obtained and added to the figure.
#'   
#' @param var Should be either "powell_dec_pe" or "mead_dec_pe". `zz` is 
#'   filtered to only include those data.
#'   
#' @param myTitle Plot title.
#' 
#' @param ui List. Currently requires entries that have scenarios, scenario 
#'   names, color label, and legend wrapping numbers.
#'   
#' @param pg_i Integer. Index into `ui` used for getting the scenarios and 
#'   scenario names. 
#'   
#' @return `ggGrob`
#' 
#' @export

plotCloudFigs <- function(zz, yrs, var, myTitle, ui, pg_i)
{
  # Used to generate cloud figures.  Commented out are colors used for plots in DCP presentations
  # and the median projections from the 07' Interim Guidelines (shown with double hash ##)
  
  scenario <- ui[["plot_group"]][[pg_i]][["plot_scenarios"]]
  scen_labs <- ui[["plot_group"]][[pg_i]][["cloud"]][["scen_names"]]
  legendTitle <- ui$defaults$color_label
  legendWrap <- ui$defaults$legend_wrap
  
  zz <- zz %>%
    dplyr::filter(StartMonth %in% scenario, Year %in% yrs, Variable == var) %>%
    # compute the 10/50/90 and aggregate by start month
    dplyr::group_by(StartMonth, Year, Variable) %>%
    dplyr::summarise('Med' = median(Value), 'Min' = quantile(Value,.1), 
                     'Max' = quantile(Value,.9)) 
  
  # Set tick marks for x and y axis
  myXLabs <- seq(1990,3000,5)
  myYLabs <- seq(900,4000,50)
  
  #  Pulling historical Pool Elevation data; grepl testing to see if Powell occurs anywhere in the 
  #  variable name.  Can be upper or lower case.
  if (var == "powell_dec_pe") {
    
    hist <- read.csv('data/HistPowellPE.csv')
    hist$Variable <- 'powell_dec_pe'
    
    # Adding switch to allow plotting of correct IG important elevations
    is_powell <- TRUE
    EQLine <- as.data.frame(read.csv('data/EQLine.csv'))
    EQLine$StartMonth <- 'Historical Elevation'
    
    ##IGProj <- read.csv('C:/RCodes/Process-CRSS-Res-TribalWaterStudy/data/IGMedProjections_Powell.csv')
    ##IGProj$Variable <- 'Powell.Pool Elevation'
  }else{
    hist <- read.csv('data/HistMeadPE.csv')  
    hist$Variable <- 'mead_dec_pe'
    
    # Adding switch to allow plotting of correct IG important elevations
    is_powell <- FALSE
    
    ##IGProj <- read.csv('C:/RCodes/Process-CRSS-Res-TribalWaterStudy/data/IGMedProjections_Mead.csv')
    ##IGProj$Variable <- 'Mead.Pool Elevation'
  }
  
  # Formatting data frame to match zz
  hist$StartMonth <- 'Historical Elevation'
  hist$Med <- hist$Min <- hist$Max <- hist$EOCYPE
  hist <- within(hist, rm(EOCYPE))
  hist <- hist[c("StartMonth","Year","Variable","Med","Min","Max")]
  
  # Formatting Interim Guidelines data frame to match zz
  ##IGProj$StartMonth <- 'Median Interim Guidelines FEIS'
  ##IGProj$Med <- IGProj$Min <- IGProj$Max <- IGProj$EOCYPE
  ##IGProj <- within(IGProj, rm(EOCYPE))
  ##IGProj <- IGProj[c("StartMonth","Year","Variable","Med","Min","Max")]
  
  # Getting all scenarios passed to fxn
  addIC <- unique(zz$StartMonth)
  
  # Appending last historical year pool elevation for each scenario
  # Must figure out the last year of model projections, and then select the
  # historical year before that
  for (tmp_scen in addIC) {
    first_mod_year <- min(filter(zz, StartMonth == tmp_scen)$Year)
    tmp_hist <- hist %>%
      filter(Year == first_mod_year - 1) %>%
      # add in the Scenario name
      mutate(StartMonth = tmp_scen)
    zz <- bind_rows(zz, tmp_hist)
  }
  
  # Appending historical data
  zz <- bind_rows(hist,zz) %>%
    filter(Year %in% yrs)
  ##zz <- bind_rows(zz,IGProj)
 
  # setup colors --------------
  # # Setting colors for graph- ensures historical data is black on plot
  # historical_color <- "#000000"
  # colorNames <- unique(zz$StartMonth)
  # #DCP colors (to match AZ Big Bang slides)"#54FF9F","#F4A460"
  # #Grey for Interim Guidelines Projections (if included) #8B8682. Add to end.
  # #plotColors <- c("#000000","#F8766D", "#00BFC4") #Conor's reverted colors
  # plotColors <- c(historical_color, "#00BFC4","#F8766D") #Stress test vs DNF colors  NORMAL CLOUD COLORS
  # #plotColors <- c("#000000", "#F8766D") #, "#00BA38", "#619CFF" - multiple comparisons
  # names(plotColors) <- colorNames
  plotColors <- get_cloud_colors(ui, pg_i)
  
  # Adding factors so ggplot does not alphebetize legend
  zz$StartMonth = factor(zz$StartMonth, levels = names(plotColors))
  
  # labels ---------------
  # Generating labels for the lines in ggplot
  histLab = "Historical Elevation"
  ##IGLab = "\"2007 Projections\""
  names(histLab) = "Historical Elevation"
  ##names(IGLab) = "\"2007 Projections\""
  histLab = append(histLab, scen_labs)
  ##histLab = append(histLab, IGLab)
  n1 <- names(histLab)
  n2 <- names(scen_labs)
  
  histLab <- str_wrap(histLab, 15)
  scen_labs <- str_wrap(scen_labs, 15)
  names(histLab) <- n1
  names(scen_labs) <- n2
  
  # Read in Reclamation logo png
  im <- load.image('logo/BofR-vert-cmyk.png')
  im_rast <- grid::rasterGrob(
    im, 
    interpolate = TRUE, 
    width = unit(.75, "inch"), 
    height = unit(.75, "inch")
  )
  
  # Parameters for cloud plot customization (line thicknesses, text size, etc.)
  # Have been pulled out for convenience
  # Text
  TitleSize = 13
  AxisText = 11
  LegendLabText = 9.5
  
  AxisLab = 9
  LabSize = 2.9
  LegendText = 8
  
  # Lines
  IGStartLine = .8
  OpsLines = .6
  Medians = 1
  GridMaj = .25
  GridMin = .25
  
  # Y axis limits
  yaxmin = floor(min(zz$Min)/50)*50
  yaxmax = ceiling(max(zz$Max)/50)*50
  
  # Other
  LegendWidth = 1
  LegendHeight = 2.5
  
  
  # Start making the plot
  gg <- ggplot(zz, aes(x=Year, y=Med, color=StartMonth, group=StartMonth)) +
    theme_light()
  
  # Generate plot a to make ribbon legend
  name <- str_wrap("10th to 90th percentile of full range",20)
  gga <- gg + 
    geom_ribbon(
      data = subset(zz,StartMonth %in% rev(addIC)),
      aes(ymin=Min, ymax=Max, fill = StartMonth), 
      alpha = 0.5, linetype = 2, size = 0.5*Medians
    ) +
    scale_fill_manual(
      name, 
      values = plotColors, 
      guide = guide_legend(order=1),
      labels = scen_labs
    ) + 
    scale_color_manual(
      name,
      values = plotColors,
      guide = guide_legend(order=1),
      labels = scen_labs
    )  +
    theme(
      legend.text = element_text(size=LegendText),
      legend.title = element_text(size=LegendLabText, face="bold"),
      legend.box.margin = margin(0,0,0,0), 
      legend.key = element_rect(), 
      legend.key.size = unit(1.75, 'lines')
    ) 
  legenda <- get_legend(gga)
  
  # Generate plot b to take medians legend
  ggb <- gg + 
    geom_line(size=Medians) + 
    scale_color_manual(
      name = str_wrap("Historical and Median Projected Pool Elevation",20),
      values = plotColors, 
      labels = histLab
    ) +
    theme(
      legend.text = element_text(size=LegendText),
      legend.title = element_text(size=LegendLabText, face="bold"),
      legend.box.margin = margin(0,0,0,0), 
      legend.key = element_rect(), 
      legend.key.size = unit(1.75, 'lines')
    ) 
  legendb <- get_legend(ggb)

  # Make legend grob.  4 rows used to make legend close together and in the 
  # middle with respects to the vertical
  gglegend <- plot_grid(
    NULL, legenda, legendb, NULL, im_rast, NULL, 
    ncol = 1, 
    rel_heights = c(.6, 1, 1, .6, .2, .17)
  )
 
  # Generate plot
  gg <- gg +  
    geom_vline(xintercept=2019, size = IGStartLine, color = '#808080') +
    annotate(
      "text", 
      x = 2019.1, y = yaxmin, 
      label = 'Adoption of the Drought\nContingency Plan', 
      size = LabSize, hjust = 0, fontface = "bold", color = '#303030'
    ) +
    scale_x_continuous(minor_breaks = 1990:3000, breaks = myXLabs,
                       labels = myXLabs, expand = c(0,0)) +
    scale_y_continuous(
      minor_breaks = seq(900,4000,25), 
      breaks = myYLabs, 
      labels = comma, 
      limits = c(yaxmin, yaxmax)
    ) +
    geom_ribbon(
      data = subset(zz,StartMonth %in% addIC),
      aes(ymin = Min, ymax = Max, fill = StartMonth), 
      alpha = 0.5, linetype = 2, size = 0.5 * Medians
    ) +
    geom_line(size=Medians) +
    # add in the historical elevation on top of other lines
    geom_line(
      data = filter(zz, StartMonth == "Historical Elevation"), 
      aes(Year, Med), 
      size = Medians, color = plotColors["Historical Elevation"]
    ) +
    scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                      values = plotColors, guide = FALSE,
                      labels = scen_labs) + 
    scale_color_manual(
      name = str_wrap("Historical and Median Projected Pool Elevation",20),
      values = plotColors, guide = FALSE,
      labels = histLab
    ) +
    labs(
      title = myTitle, 
      x = '', y = 'Elevation (feet msl)\n', 
      caption = ui[["plot_group"]][[pg_i]][["cloud"]][["caption"]]
    ) + 
    theme(plot.title = element_text(size = TitleSize),
          ## axis.text.x = element_text(size = AxisLab),
          axis.text.y = element_text (size =AxisLab),
          axis.title = element_text(size=AxisText, face = "plain", color = 'grey30'),
          panel.grid.minor = element_line(size = GridMin),
          panel.grid.major = element_line(size = GridMaj)) +
    guides(fill=FALSE)
  
  # Add in lines for Powell or Mead operations --------------------
  if (min(yrs) <= 2007) {
    ig_start <- 2007
    ig_label <- 2007.1
    
    # only add the vertical line if we are showing data in/before 2007
    gg <- gg +
      geom_vline(
        xintercept = ig_start, 
        size = IGStartLine, 
        color = '#808080'
      ) + 
      annotate(
        "text", x = ig_label, y = yaxmin, 
        label = 'Adoption of the 2007\nInterim Guidelines', 
        size = LabSize, 
        hjust = 0,
        fontface = "bold", 
        color = '#303030'
      )
    
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  
  if (is_powell) {
    # Powell
    gg <- gg +
      # Adding lines and annotation for Powell ops - plot only if Switch = True
      geom_line(
        data = filter(EQLine, Year %in% yrs), 
        aes(x = Year, y = EQLine), 
        size = OpsLines,
        color = '#808080', 
        linetype = 3
      ) + 
      annotate(
        "text", x = ig_label, y = yaxmax, 
        label = "Equalization Tier", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      ) +
      ##{if(Switch)geom_segment(x=1998, y=3490, xend =2026, yend = 3490, size = OpsLines, 
      ##   color ='#808080', linetype = 3)} + 
      ##{if(Switch)annotate("text", x = 1999.5, y = 3485, label = "Minimum Power Pool", 
      ##   size = LabSize, hjust = 0, fontface = "italic", color = '#505050')} +
     geom_segment(
       x = ig_start, y = 3525, xend = 2026, yend = 3525, 
       size = OpsLines, 
       color ='#808080', 
       linetype = 3
      ) + 
      annotate(
        "text", x = ig_label, y = 3520, 
        label = "Lower Elevation Balancing Tier", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      ) +    
      geom_segment(
        x = ig_start, y = 3575, xend = 2026, yend = 3575, 
        size = OpsLines, 
        color ='#808080', 
        linetype = 3
      ) + 
      annotate(
        "text", x = ig_label, y = 3570, 
        label = "Mid Elevation Release Tier", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      ) + 
      annotate(
        "text", x = ig_label, y = 3582, 
        label = "Upper Elevation Balancing Tier", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      )
    
    #{if(Switch)annotate("text", x=2020.1, y = yaxmin,
    #         label = 'Adoption of the Drought\nResponse Operations', size = LabSize, hjust=0,
    #         fontface = 'bold', color = '#303030')} +
    #Adding Drought Response Ops
    #{if(Switch)geom_segment(x=2020, y=3525, xend = 2026, yend = 3525, size = OpsLines,
    #    color = '#808080', linetype = 6)} +
    #{if(Switch)annotate("text", x = 2020.1, y = 3518, label = "Drought Response Ops\nTarget Elevation",
    #    size = LabSize, hjust = 0, color = '#505050')} +
      
  } else {
    # Mead
    gg <- gg +
      # Adding lines for Mead ops - plot only if Switch = False
      geom_segment(
        x = ig_start, y = 1075, xend = 2026, yend = 1075, 
        size = OpsLines, 
        color ='#808080', linetype = 3
      ) + 
      annotate(
        "text", x = ig_label, y = 1070, 
        label = "Level 1 Shortage Condition", 
        size = LabSize, hjust = 0, fontface = "italic", color = '#505050'
      ) +
      geom_segment(
        x = ig_start, y = 1050, xend = 2026, yend = 1050, 
        size = OpsLines, 
        color ='#808080', 
        linetype = 3
      ) +
      annotate(
        "text", x = ig_label, y = 1045, 
        label = "Level 2 Shortage Condition", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      ) +
      geom_segment(
        x = ig_start, y = 1025, xend = 2026, yend = 1025, 
        size = OpsLines, 
        color ='#808080', linetype = 3
      ) +
      annotate(
        "text", x = ig_label, y = 1020, 
        label = "Level 3 Shortage Condition", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      ) +
      geom_segment(
        x = ig_start, y = 1145, xend = 2026, yend = 1145, 
        size = OpsLines, 
        color ='#808080', linetype = 3
      ) +
      annotate(
        "text", x = ig_label, y = 1140, 
        label = "Normal or ICS Surplus Condition", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      ) +
      annotate(
        "text", x = ig_label, y = yaxmax, 
        label = "Surplus Condition", 
        size = LabSize, 
        hjust = 0, 
        fontface = "italic", 
        color = '#505050'
      )
  }
  
  # Add BOR Logo
  gg <- plot_grid(gg, gglegend, rel_widths = c(2,.4)) #%>%
    #add_logo_horiz()
  gg
}

plot_both_clouds <- function(pe, ui, folder_paths)
{
  # loop through all plot_groups and if create == TRUE, create the cloud figure
    
  for (i in seq_along(ui[["plot_group"]])) {
    p_title <- 'Powell End-of-December Elevation'
    m_title <- 'Mead End-of-December Elevation'
    
    if (ui[["plot_group"]][[i]][["cloud"]][["create"]]) {
      
      pg <- ui[["plot_group"]][[i]]
      
      peYrs <- pg[["cloud"]][["years"]]
      
      if (pg[["cloud"]][["title_append"]] != '') {
        p_title <- paste(p_title, pg[["cloud"]][["title_append"]])
        m_title <- paste(m_title, pg[["cloud"]][["title_append"]])
      }
      
      p_file <- construct_file_name(ui, folder_paths, i, "png_out", "Powell.png")
      m_file <- construct_file_name(ui, folder_paths, i, "png_out", "Mead.png")
      
      powellCloud <- plotCloudFigs(
        pe,
        peYrs, 
        "powell_dec_pe",
        p_title,
        ui,
        i
      )
      
      ggsave(
        p_file, 
        plot = powellCloud,
        width = 9, 
        height = 6.5, 
        units = "in", 
        dpi = 600
      )
      
      message("   ... saved ", p_file)
      
      meadCloud <- plotCloudFigs(
        pe,
        peYrs, 
        "mead_dec_pe", 
        m_title,
        ui,
        i
      )
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

