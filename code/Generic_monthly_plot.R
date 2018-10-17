##############################################################################
#This script creates monthly plots of Outflow and PE to compare two CRSS runs

#DEVELOPMENT IS ON GOING ON THIS

#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
##############################################################################

generic.monthly.plot <- function(scen_res) { 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #Additional plotting functions and libraries 
  library('tidyverse') #ggplot2,dplyr,tidyr
  source('code/Stat_emp_ExcCrv.r')
  source('code/stat-boxplot-custom.r')
  
  figurenames <- c("mean","bxplt","exceedance")
  
  ## create a pdf  
  # pdf(paste0(file.path(ofigs,figs),"_",variables,"_",figurenames[figuretypes],".pdf"), width=9, height=6)
  #enable the above if using loops (advanced) and want one pdf
  #must disable ggsave at bottom and captions won't work w/o ggsave
  
  #y axis titles 
  if (floworpe == "flow"){
    y_lab = "Monthly Flow (ac-ft/mo)"
  } else {
    y_lab = "End of Month PE (ft)"
  }
  
  if (is.na(custom_y_lab)){
    y_lab <- y_lab #default
  } else {
    y_lab <- custom_y_lab
  }


  figs <- figname
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 4. Plot Choosen Figure Type 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #figure captions
  if (is.na(customcaption) &  figuretype == 2){
    caption <- "Note: The boxplots show the distribution of traces, one for each year. The boxplot boxes correspond to the 25th and 75th quantiles,\nthe whiskers enclose the 10th to 90th quantiles,with points representing data that falls outside this range."
  } else if (is.na(customcaption)){
    caption <- "" #no caption 
  } else {
    caption <- customcaption #user supplied 
  }
  
  message(paste("Creating",variable,timestep,cyorwy,figurenames[figuretype]))
  
  #    -------------------        All Trace Mean        ----------------------
  
  if (figuretype == 1){
    p <- scen_res %>%
      dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
      dplyr::group_by(Scenario, MonthNum) %>%
      dplyr::summarise(Value = mean(Value)) %>%
      ggplot(aes(x = MonthNum, y = Value, color = Scenario)) + 
      geom_line() +
      geom_point() +
      scale_x_discrete("Month",labels = month.abb) + #display abb. month names
      ########### WHY IS THIS NOT WORKING?     ###########
      labs(title = paste("Mean",variable,startyr,"-",endyr), 
           y = y_lab, x = "Year", caption = caption) +  
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
  }
  
  #    -------------------        All Trace Boxplot        ----------------------
  
  if (figuretype == 2){
    p <- scen_res %>%
      dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
      dplyr::group_by(Scenario, MonthNum) %>%
      ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
      geom_boxplot() +
      scale_x_discrete("Month",labels = month.abb) + #display abb. month names
      labs(title = paste(variable,startyr,"-",endyr), 
           y = y_lab, x = "Year", caption = caption) +  
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
  }
  
  #    -------------------        Percent Exceedance of Traces       ----------------------
  
  if (figuretype == 3){
    p <- scen_res %>%
      dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
      dplyr::filter(MonthNum == exc_month) %>% #This is currently set to filter 
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario, MonthNum) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      scale_x_discrete("Month",labels = month.abb) + #display abb. month names
      labs(title = paste(variable,month.abb[exc_month],"Trace Exceedance",startyr,"-",endyr),
           y = y_lab, caption = caption) +
      scale_x_continuous("Year",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
  }
  
  if(combineplots == F){
    ## save off image
    ggsave(filename = paste0(file.path(ofigs,figs),"_",timestep,"_",cyorwy,"_",variable,"_",figurenames[figuretype],".",imgtype), width = width, height = height, units ="in")
    
    dev.off()
  }

} #end function