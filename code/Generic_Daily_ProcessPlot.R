# ##############################################################################
# #This script processes daily rdf files to compare two CRSS runs
# 
# #DEVELOPMENT IS ON GOING ON THIS
# 
#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
# ##############################################################################


generic.daily.process <- function(scen_dir,scens,timestep) { 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  
  #Additional plotting functions and libraries 
  library('tidyverse') #ggplot2,dplyr,tidyr
  library('devtools')
  library(RWDataPlyr)
  library('scales') #need this for scale_x_date()
  source('code/Stat_emp_ExcCrv.r')
  source('code/stat-boxplot-custom.r')
  
  # some sanity checks that UI is correct:
  if(!(mainScenGroup %in% names(scens))) 
    stop(mainScenGroup, ' is not found in scens.')
  
  # check folders
  if(!file.exists(file.path(scen_dir, scens[1])) 
     | !file.exists(file.path(scen_dir, scens[2])))
    stop('scens folder(s) do not exist or scen_dir is set up incorrectly. 
         Please ensure scens is set correctly.')

  figurenames <- c("mean","bxplt","exceedance")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. Process Results 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  title <- variable
  
  #Special rw scen agg since RWDataPlyr doesn't support daily 
  for (i in 1:length(scens)) {
    
    scen_res_x <- file.path(scen_dir,scens[i],rdffile) %>% #this means "pipe" the data to the next function 
      rdf_to_rwtbl2() %>%
      filter(ObjectSlot == variable)
    
    # #filter out Most,Min,Max only would do this for MTOM 
    # filter(TraceNumber >= first_ensemble[1]) 
    
    #add on Scenario since rdf_to_rwtbl2 doesn't add it  
    scen_res_x <- cbind.data.frame(
      scen_res_x,
      Scenario = rep(names(scens)[i], Times = length(scen_res_x$Timestep))
    )
    
    #convert Timestep chr to POSIXct
    scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M")) 
    scen_res_x$Timestep <- as.Date(scen_res_x$Timestep)
    #first entry is 2019-1-31 24:00 which gets converted to 2019-02-01, is that okay????? 
    
    if(i == 1){
      scen_res = scen_res_x
    } else {
      scen_res = rbind.data.frame(scen_res,scen_res_x)
    }
    
  } #close i Scenario loop 
  
  # unique(scen_res$ObjectSlot) #check variable names 
  # unique(scen_res$Scenario) #check Scenario names 

  #get everything on a date 
  scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
  #get a numeric month number
  scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))
  scen_res$DayNum = as.numeric(format.Date(scen_res$Timestep, format = "%d"))
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 4. Plot Choosen Figure Type 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  figs <- figname
  
  #y labels
  if (is.na(custom_y_lab)){
    y_lab <- "Daily Flow (cfs)" #default
  } else {
    y_lab <- custom_y_lab
  }

  #figure captions
  if (is.na(customcaption) &  figuretype == 1){
    caption <- "Note: Month axis label is the previous EoM CRSS timestep, e.g. Feb label is Jan 31."
  } else if (is.na(customcaption) &  figuretype == 2){
    caption <- "Note: The boxplots show the distribution of daily flow grouped by month for all traces. The boxes correspond to the 25th and 75th quantiles,\nthe whiskers enclose the 10th to 90th quantiles,with points representing data that falls outside this range."
  } else if (is.na(customcaption)){
    caption <- "" #no caption 
  } else {
    caption <- customcaption #user supplied 
  }
  
  message(paste("Creating ",variable, figurenames[figuretype])) 
  
  #    -------------------        All Trace Mean        ----------------------

  if (figuretype == 1){
    p <- scen_res %>%
      dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
      dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
      dplyr::summarise(Value = mean(Value)) %>%
      ggplot(aes(Timestep, Value, color = Scenario)) + 
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::comma) + #add commas to axis 
      labs(title = paste("Mean",variable,plotyr), y = y_lab,caption = caption) +
      scale_x_date("Month", breaks = date_breaks("months"),
                   labels = date_format("%b")) +
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
  }
  
  message("The months appear to start in Feb since first CRSS timestep is Jan 31")
  
  #    -------------------        All Trace Boxplot        ----------------------
  if (figuretype == 2){
    p <- scen_res %>%
      dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
      dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
      dplyr::group_by(Scenario, MonthNum) %>% #don't need to do this since only one var
      ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
      # geom_boxplot() +
      stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) + 
      scale_x_discrete("Month",labels = month.abb) + #display abb. month names
      labs(title = paste(title,plotyr), y = y_lab) +  
      labs(caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
  }
  
  #    -------------------        Percent Exceedance of Traces       ----------------------
  if (figuretype == 3){
    p <- scen_res %>%
      dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
      dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
      ggplot(aes(Value, color = Scenario)) + 
      stat_eexccrv() + 
      labs(title = paste(plotyr,title,"Trace Exceedance"), y = y_lab, x = "Exceedance") +
      scale_x_continuous(labels = percent) +
      labs(caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
  }
  
  if(combineplots == F){
    ## save off image 
    ggsave(filename = paste0(file.path(ofigs,figs),"_",variable,"_",figurenames[figuretype],".",imgtype), width = width, height = height, units ="in")
    
    dev.off()
  }
  
  return(scen_res) #in case further anaylsis is desired 
      
} #end function
      