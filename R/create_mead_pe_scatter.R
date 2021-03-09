

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
      
      gg_out[[names(ui[["ind_plots"]][["mead_pe_scatter"]])[i]]] <- 
        gg_list("ind_mead_scatter" = gg)
    }
  }

  pgs_out(gg_out)
}
