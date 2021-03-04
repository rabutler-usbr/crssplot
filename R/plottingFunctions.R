
# assumes zz is data already read in and will return one variable for the given 
# yrs rownames of zz should be years, and colnames should be variable names
getSingleVarData <- function(zz, yrs, var)
{
  rr <- match(yrs, rownames(zz))
  cc <- which(colnames(zz) == var)
  zz[rr,cc]
}

formatSimpleTable <- function(zz, scenNames, yrs)
{
  zzRound <- round(zz,0)
  zzRound[3,] <- zzRound[2,] - zzRound[1,]

  zzRound <- matrix(paste0(zzRound,'%'),nrow = nrow(zz), byrow = F)

  # check to see if values are non-zero, but rounded to zero
  # if they are, replace with '< 1%'
  for(i in 1:nrow(zz)){
    for(j in 1:ncol(zz)){
      if(zz[i,j] > 0 & zzRound[i,j] == '0%'){
        zzRound[i,j] <- '< 1%'
      } else if(zz[i,j] < 0 & zzRound[i,j] == '0%'){
        zzRound[i,j] <- '< -1%'
      }
    }
  }
  rownames(zzRound) <- c(scenNames, 'Difference')
  colnames(zzRound) <- yrs
  
  zzRound <- as.data.frame(zzRound)
  zzRound
}

#' @param iData data frame that contains shortage and powell < 3490 variables
#' @param scenNames a named character vector; names are the names that will show 
#'   up in the finished table and the entries are the Scenario Group variable
#'   names that will be used to filter the scenarios
#' @param yrs the years to show in the table
#' @noRd
# Assumes that there are only two scenarios to process
create5YrSimpleTable <- function(iData, scenNames, yrs, addFootnote = NA, ofile)
{
  assert_that(
    length(scenNames) == 2, 
    msg = paste0(
      'Invalid number of scenarios passed to create5YrSimpleTable.\n',
      'Please ensure scenNames have only two scenarios.'
    )
  )

  assert_that(
    all(c('lbShortage', 'powell_wy_min_lt_3490') %in% unique(iData$Variable)),
    msg = "shortage and powell < 3490 variables are not found in iData"
  )
  
  i1 <- iData %>%
    filter(Year %in% yrs) %>%
    filter(Variable %in% c('lbShortage', 'powell_wy_min_lt_3490'), 
           ScenarioGroup %in% names(scenNames)) %>%
    mutate(ScenName = scenNames[ScenarioGroup]) %>%
    group_by(Year, Variable, ScenName) %>%
    # multiply by 100 to display as percent instead of decimal
    dplyr::summarise(PrctTraces = mean(Value)*100) 
  
  shortTable <- i1 %>%
    filter(Variable == 'lbShortage') %>%
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(Year, PrctTraces) %>%
    slice(match(scenNames, ScenName))
  
  rns <- c(shortTable$ScenName)
  
  shortTable <- select(shortTable, -ScenName)
  shortTable <- round(as.matrix(shortTable),0)
    
  shortTable <- as.matrix(rbind(shortTable, shortTable[2,] - shortTable[1,]))
  shortTable <- formatSimpleTable(shortTable, rns, yrs)
  
  pTable <-  i1 %>%
    filter(Variable == 'powell_wy_min_lt_3490') %>%
    ungroup() %>%
    select(-Variable) %>%
    tidyr::spread(Year, PrctTraces) %>%
    slice(match(scenNames, ScenName))
  rns <- c(pTable$ScenName)
  
  pTable <- select(pTable, -ScenName)
  pTable <- round(as.matrix(pTable),0)
  
  pTable <- as.matrix(rbind(pTable, pTable[2,] - pTable[1,]))
  pTable <- formatSimpleTable(pTable, rns, paste('WY',yrs))
  
  myTheme <- gridExtra::ttheme_default(
    gpar.coltext = grid::gpar(cex = 1), 
    gpar.rowtext = grid::gpar(cex = 1), show.hlines = T,
    core.just = 'right'
  )
  
  shortGrob <- gridExtra::tableGrob(shortTable, theme = myTheme)
  pGrob <- gridExtra::tableGrob(pTable, theme = myTheme)
  
  shortLabel <- '% Traces with Lower Basin Shortage'
  pLabel <- '% Traces below 3,490 feet (minimum power pool) at Lake Powell'
  
  gg <- qplot(1:7,1:7,geom = 'blank') + theme_bw() +
    theme(line = element_blank(), text = element_blank()) +
    annotation_custom(grob = pGrob, xmin = 0, ymin = 2,xmax = 7, ymax = 6) + 
    annotation_custom(
      grob = shortGrob, 
      xmin = 0, 
      ymin = 4,
      xmax = 6, 
      ymax = 7.2
    ) +
    annotate(
      "text", 
      x = 1.5, 
      y = 4.65, 
      label = pLabel, 
      hjust = 0, 
      size = 4, 
      fontface = "bold"
    ) +
    annotate(
      "text", 
      x = 1.5, 
      y = 6.25, 
      label = shortLabel, 
      hjust = 0, 
      size = 4, 
      fontface = "bold"
    )
  
  if (!is.na(addFootnote)) {
    gg <- gg +
      annotate(
        'text', 
        x = 1.5, y = 3.4, 
        label = addFootnote, 
        hjust = 0, 
        size = 2
      )
  }
    
  grDevices::pdf(ofile, width = 8, height = 8)
  print(gg)
  grDevices::dev.off()
  
  invisible(iData)
}

create_all_simple_5yr <- function(zz, ui, folder_paths)
{
  for (i in seq_along(ui[["plot_group"]])) {
    if (ui[["plot_group"]][[i]][["simple_5yr"]][["create"]]) {
      
      ofile <- construct_file_name(
        ui, folder_paths, i, "figs_folder", '5yrSimple.pdf'
      )
      
      create5YrSimpleTable(
        zz, 
        ui[["plot_group"]][[i]][["simple_5yr"]][["scen_names"]], 
        ui[["plot_group"]][[i]][["simple_5yr"]][["years"]], 
        ui[["plot_group"]][[i]][["simple_5yr"]][["footnote"]],
        ofile
      )
    }
  }
}

determine_plot_colors <- function(plot_colors, col_vars, msg_string = "Scenario")
{
  if (is.null(plot_colors)) {
    plot_colors <- scales::hue_pal()(length(col_vars))
    names(plot_colors) <- col_vars
  } else {
    # check that there are specified colors for each Variable
    assert_that(
      all(col_vars %in% names(plot_colors)), 
      msg = paste(msg_string, "names not found in `plot_colors`")
    )
  }
  
  plot_colors
}

create_mead_pe_scatter <- function(ui, o_files, traceMap)
{
  gg_out <- list()
  
  myCols <- c('<= 1,075\'' = '#b2182b',
              "1,075'-1,076'" = '#ef8a62',
              "1,076'-1,077'" = '#9970ab',
              "> 1,077'" = '#2166ac')
  
  for (i in seq_along(ui[["ind_plots"]][["mead_pe_scatter"]])) {
    if (isTRUE(ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["create"]])) {
      message("   ... ", names(ui[["ind_plots"]][["mead_pe_scatter"]])[i])
      
      tmp_model <- ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["model"]]
      
      scenario <- names(ui[["ind_plots"]][["mead_pe_scatter"]])[i]
      
      if (tmp_model == "CRSS") {
        pe <- feather::read_feather(o_files$cur_month_pe_file) %>%
          filter(ScenarioGroup == scenario, Variable == "mead_dec_pe")
        
      } else if (tmp_model == "MTOM") {
        
        icDim <- 1981:2015
        tmpIcMonth <- paste(
          stringr::str_replace(
            ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]], "20", ""
          ), 
          "Dec", 
          sep = "-"
        )
        
        decVals <- do.call(
          rbind, 
          lapply(
            icDim, 
            get1TraceIc, 
            icList[[names(ui[["ind_plots"]][["mead_pe_scatter"]])[i]]], 
            tmpIcMonth, 
            traceMap
          )
        )
        traceNum <- traceMap$trace[match(icDim, traceMap$ic)]
        
        pe <- decVals %>%
          select(`Mead.Pool Elevation`) %>%
          rename(Value = `Mead.Pool Elevation`) %>%
          mutate(
            TraceNumber = as.numeric(traceNum), 
            Year = ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]],
            Variable = "mead_dec_pe"
          )
        
      } else {
        stop("Invalid peScatterData variable")
      }
      scatterTitle <- paste(
        'Lake Mead December', 
        ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]], 
        'Elevations from', 
        ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["model"]]
      )
      
      pe <- mutate(
        pe,
        pe_bin = case_when(
          Value <= 1075 ~ '<= 1,075\'', 
          Value <= 1076 ~ "1,075'-1,076'",
          Value <= 1077 ~ "1,076'-1,077'",
          TRUE ~ "> 1,077'"
        ),
        pe_bin = factor(pe_bin, levels = names(myCols))
      )
            
      gg <- var_plot_trace_scatter(
        pe,
        scenarios = scenario,
        years = ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]],
        vars = "mead_dec_pe",
        color_by = "pe_bin",
        plot_colors = myCols,
        y_lab = "Pool Elevation (feet)",
        caption = ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["ann_text"]]
      )
      
      if (ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["add_threshold_stats"]]) {
        tmp_pe <- filter(
          pe, Year == ui[["ind_plots"]][["mead_pe_scatter"]][[i]][["year"]]
        )
        
        nn <- tmp_pe %>%
          mutate(lt1075 = ifelse(Value <= 1075, 1, 0),
                 lt1076 = ifelse(Value <= 1076 & Value > 1075, 1, 0),
                 lt1077 = ifelse(Value <= 1077 & Value > 1075, 1, 0)) %>%
          ungroup() %>%
          summarise(
            lt1075 = sum(lt1075), 
            lt1076 = sum(lt1076), 
            lt1077 = sum(lt1077)
          )
        
        is_are <- function(i) {
          if (i == 1) {
            r <- "run is"
          } else {
            r <- "runs are"
          }
          
          r
        }
        
        myText <- paste0(
          nn$lt1075, ' ', is_are(nn$lt1075), ' below 1,075 ft\n',
          'an additional ', nn$lt1076, ' ', is_are(nn$lt1076),
          ' within 1 ft of being below 1,075 ft\n',
          nn$lt1077, ' ', is_are(nn$lt1077), 
          ' within 2 ft of being below 1,075 ft'
        )
        
        gg <- gg + geom_hline(yintercept = 1075, color = 'red', size = 1) +
          annotate(
            geom = 'text', 
            x = 1, 
            y = max(tmp_pe$Value) - 5, 
            label = myText, 
            hjust = 0
          )
      }
      
      gg_out[[i]] <- gg
    }
  }

  gg_out
}
