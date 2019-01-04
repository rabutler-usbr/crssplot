# barplot of Mead and Powell conditions
# same info that is in the 5-year table, except shown as a barplot

library(tidyverse)
library(gridExtra)

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
  "Surplus - Flood Control",
  "Normal Year or ICS Surplus Condition"
)

mead_normal <- tail(mead_vars, 1)
mead_surplus <- c("surplus", "fc_surplus")
names(mead_surplus) <- mead_vars[5:6]

mead_vars2 <- c("short1", "short2", "short3")
  
names(mead_vars2) <- c(
  "Shortage - 1st Level (Mead <= 1,075 and >= 1,050",
  "Shortage - 2nd Level (Mead < 1,050 and >= 1,025",       
  "Shortage - 3rd Level (Mead < 1,025)"
)

ifile <- "C:/alan/CRSS/CRSS.2018/results/Aug2018/tempData/SysCond.feather"

to_percent <- function(x) {paste0(round(x, 0), "%")}

mead_powell_condition_barplot <- function(ifile)
{
  yrs2show <- 2019:2026
  scens <- c("April 2018", "August 2018")
  
  zz <- read_feather(ifile)
  
  
  # get the system conditions data -------------
  # get it for all of the unique Aggs
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

  # shortage plot --------------------
  #short_data
  tmp <- filter(sys_data, Variable %in% names(mead_vars2)) %>%
    mutate(Variable = mead_vars2[Variable]) %>%
    spread(Variable, Value) %>%
    mutate(
      short3_fill = short3 / 2,
      short2_fill = short3 + short2 / 2,
      short1_fill = short3 + short2 + short1 / 2 
    ) 
  
  short_data <- full_join(
    select(tmp, -short3_fill, -short2_fill, -short1_fill) %>%
      gather(Variable, Value, -Year, -Scenario),
    select(tmp, -short1, -short2, -short3) %>%
      rename(short1 = short1_fill, short2 = short2_fill, short3 = short3_fill) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  color_map <- c("#00BFC4", "#1e959a", "#00686d",
                 "#F8766D", "#dc1304", "#7f0900")
  names(color_map) <- c(
    paste0(scens[1], "_short", 1:3), 
    paste0(scens[2], "_short", 1:3)
  )
  
  fill_labeler <- function(x) {
    all_names <- names(color_map)
    names(all_names) <- all_names
    all_names[1:3] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    short_to_long <- c("Shortage - 1st Level (Mead <= 1,075 and >= 1,050",
                       "Shortage - 2nd Level (Mead < 1,050 and >= 1,025",       
                       "Shortage - 3rd Level (Mead < 1,025)")
    names(short_to_long) <- paste0("short", 1:3)
    all_names[4:6] <- short_to_long[all_names[4:6]]
    str_wrap(all_names, width = 20)
  }

  gg_short <- ggplot(short_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = color_map, labels = fill_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Shortage Condition - any amount (Mead <= 1,075')",
      fill = "Shortage Tiers",
      caption = paste(
        "The tops of the bars indicate the chances of any shortage.",
        "The numbers report the chances of the different shortage tiers.",
        sep = "\n"
      )
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines")
    )
  
  # normal year plot -----------------
  
  normal_color_map <- color_map[c(1,4)]
  names(normal_color_map) <- scens
  
  tmp <- filter(sys_data, Variable %in% mead_normal) 
  
  gg_normal <- ggplot(tmp, aes(Scenario, Value, fill = Scenario)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    scale_fill_manual(values = normal_color_map) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = mead_normal,
      fill = "Scenario"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(1, "lines"),
      legend.spacing.x = unit(1, "lines"),
      legend.direction = "horizontal"
    )
  
  # surplus plot ----------------------
  tmp <- filter(sys_data, Variable %in% names(mead_surplus)) %>%
    mutate(Variable = mead_surplus[Variable]) %>%
    spread(Variable, Value) %>%
    # now they are additive
    mutate(surplus = surplus - fc_surplus) %>%
    mutate(
      surplus_fill = surplus / 2,
      fc_surplus_fill = surplus + fc_surplus / 2
    )
  
  surp_data <- full_join(
    select(tmp, -surplus_fill, -fc_surplus_fill) %>%
      gather(Variable, Value, -Year, -Scenario),
    select(tmp, -surplus, -fc_surplus) %>%
      rename(fc_surplus = fc_surplus_fill, surplus = surplus_fill) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  surp_color_map <- c("#00BFC4", "#1e959a",
                 "#F8766D", "#dc1304")
  names(surp_color_map) <- c(
    paste0(scens[1], c("_fc_surplus", "_surplus")), 
    paste0(scens[2], c("_fc_surplus", "_surplus"))
  )
  
  surp_fill_labeler <- function(x) {
    all_names <- names(surp_color_map)
    names(all_names) <- all_names
    all_names[1:2] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    short_to_long <- names(mead_surplus)
    short_to_long[
      short_to_long == "Surplus Condition - any amount (Mead>= 1,145 ft)"
    ] <- "Surplus - Domestic or Quantified"
    names(short_to_long) <- mead_surplus
    
    all_names[3:4] <- short_to_long[all_names[3:4]]
    str_wrap(all_names, width = 20)
  }
  
  gg_surplus <- ggplot(surp_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") +
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = surp_color_map, labels = surp_fill_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Surplus Condition - any amount (Mead >= 1,145 ft)",
      fill = "Surplus Conditions",
      caption = paste(
        "The tops of the bars indicate the chances of any surplus.",
        "The numbers report the chances of the different surplus conditions.",
        sep = "\n"
      )
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines")
    )
  
  # arrange all 3 plots together
  norm_grob <- ggplotGrob(gg_normal)
  short_grob <- ggplotGrob(gg_short)
  surp_grob <- ggplotGrob(gg_surplus)
  
  get_legend <- function(grob) {
    grob$grobs[[which(sapply(grob$grobs, function(x) x$name) == "guide-box")]]
  }
  
  scen_leg <- get_legend(norm_grob)
  short_leg <- get_legend(short_grob)
  surp_leg <- get_legend(surp_grob)
    
  norm_grob <- ggplotGrob(gg_normal + theme(legend.position = 'none'))
  short_grob <- ggplotGrob(gg_short + theme(legend.position = 'none'))
  surp_grob <- ggplotGrob(gg_surplus + theme(legend.position = 'none'))
  
  gg <- grid.arrange(arrangeGrob(
    scen_leg, surp_grob, surp_leg, norm_grob, grid::nullGrob(), short_grob, short_leg,
    layout_matrix = matrix(c(1,1:7), ncol = 2, byrow = TRUE),
    widths = c(.8, .2),
    heights = c(.05, rep(.95/3, 3))
  ))
  
  ggsave(
    "C:/alan/projects/LC Website/LBConditions.png", 
    gg, 
    width = 8.5, 
    height = 11, 
    units = "in"
  )
}