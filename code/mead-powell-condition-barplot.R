# barplot of Mead and Powell conditions
# same info that is in the 5-year table, except shown as a barplot

library(tidyverse)

powell_vars <- c(
  "Equalization Tier", "Equalization - annual release > 8.23 maf",
  "Equalization - annual release = 8.23 maf", "Upper Elevation Balancing Tier",
  "Upper Elevation Balancing - annual release > 8.23 maf",
  "Upper Elevation Balancing - annual release = 8.23 maf", 
  "Upper Elevation Balancing - annual release < 8.23 maf",
  "Mid-Elevation Release Tier",                          
  "Mid-Elevation Balancing - annual release = 8.23 maf",
  "Mid-Elevation Release Tier - annual release = 7.48 maf",
  "Lower Elevation Balancing Tier"
)

mead_vars <- c(
  "Shortage Condition - any amount (Mead <= 1,075 ft)",    
  "Shortage - 1st Level (Mead <= 1,075 and >= 1,050",
  "Shortage - 2nd Level (Mead < 1,050 and >= 1,025",       
  "Shortage - 3rd Level (Mead < 1,025)",
  "Surplus Condition - any amount (Mead>= 1,145 ft)",      
  "Surplus - Flood Control","Normal Year or ICS Surplus Condition"
)

mead_powell_condition_barplot <- function(ifile)
{
  yrs2show <- 2019:2026
  zz <- read_feather(ifile)
  
  
  # get the system conditions data for all of the unique Aggs
  aggs <- unique(zz$Agg)
  
  sys_data <- list()
  
  for (aa in aggs) {
    tmp <- CRSSIO::crsso_get_sys_cond_table(
      dplyr::filter(zz, Year %in% yrs2show & Agg == aa), yrs2show
    )
    
    sys_data[[aa]] <- as.data.frame(tmp$limitedTable) %>%
      rownames_to_column(var = "Variable") %>%
      as_tibble() %>%
      gather(Year, Value, -Variable) %>%
      mutate(Scenario = aa)
  }
  
  # convert from list to df
  sys_data <- bind_rows(sys_data)
  
  ggplot(filter(sys_data, Variable %in% mead_vars), aes(Variable, Value, fill = Scenario)) +
    geom_bar(position = "dodge", stat = "identity") +
    facet_wrap(~Year, nrow = 1)
  
  ggplot(filter(sys_data, Variable %in% mead_vars), aes(Year, Value, fill = Scenario)) +
    geom_bar(position = "dodge", stat = "identity") +
    facet_wrap(~Variable, ncol = 1)
}