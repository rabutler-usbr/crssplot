# barplot of Mead and Powell conditions
# same info that is in the 5-year table, except shown as a barplot

library(tidyverse)
library(gridExtra)
library(grid)
library(imager)
library(feather)
library(assertthat)

mead_vars <- c(
  "Shortage Condition - any amount (Mead <= 1,075 ft)",    
  "Shortage - 1st Level (Mead <= 1,075 and >= 1,050)",
  "Shortage - 2nd Level (Mead < 1,050 and >= 1,025)",       
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
  "Shortage - 1st Level (Mead <= 1,075 and >= 1,050)",
  "Shortage - 2nd Level (Mead < 1,050 and >= 1,025)",       
  "Shortage - 3rd Level (Mead < 1,025)"
)

# three value colors
color_map_3 <- c("#00BFC4", "#1e959a", "#00686d",
               "#F8766D", "#dc1304", "#7f0900")

color_map_2 <- c(color_map_3[c(1, 2, 4, 5)])


to_percent <- function(x) {paste0(round(x, 0), "%")}

get_legend <- function(grob) {
  grob$grobs[[which(sapply(grob$grobs, function(x) x$name) == "guide-box")]]
}

#' @param zz data.frame. Should be result of using 
#'   [RWDataPlyr::rw_scen_aggregate()] with the `rwd_agg` object from 
#'   [CRSSIO::sys_cond_rwa()].
#'
#' @param yrs2show Years to plot as a vector. E.g., `2019:2026`.
#' 
#' @param scens The 2 scenarios to plot. The names of scens dictate the colors, 
#'   but they should match aggregation scenarios (those in `zz$Agg`). The first
#'   entry will be blue, and the second is red.
#'   
#' @param ofolder The folder to create the .png file.
mead_powell_condition_barplot <- function(zz, yrs2show, scens, ofolder)
{
  assert_that(
    all(scens %in% unique(zz$Agg)), 
    msg = "`scens` not found in the data frame."
  )
  
  assert_that(length(scens) == 2, msg = "There should only be 2 `scens`")
  
  assert_that(dir.exists(ofolder))
  
  # get the system conditions data -------------
  # get it for all of the unique Aggs
  # aggs <- unique(zz$Agg)
  aggs <- scens
  
  sys_data <- list()
  
  for (aa in aggs) {
    tmp <- CRSSIO::crsso_get_sys_cond_table(
      dplyr::filter(zz, Year %in% yrs2show, Agg == aa), yrs2show
    )
    
    sys_data[[aa]] <- as.data.frame(tmp$fullTable) %>%
      rownames_to_column(var = "Variable") %>%
      as_tibble() %>%
      gather(Year, Value, -Variable) %>%
      mutate(Scenario = aa)
  }
  
  # convert from list to df
  sys_data <- bind_rows(sys_data)

  scen_leg <- mead_condition_barplot(sys_data, scens, ofolder)
  powell_condition_barplot(sys_data, scens, ofolder, scen_leg)
  invisible(sys_data)
}

mead_condition_barplot <- function(sys_data, scens, ofolder)
{
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
      rename(
        short1 = short1_fill, 
        short2 = short2_fill, 
        short3 = short3_fill
      ) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  names(color_map_3) <- c(
    paste0(scens[1], "_short", 1:3), 
    paste0(scens[2], "_short", 1:3)
  )
  
  fill_labeler <- function(x) {
    all_names <- names(color_map_3)
    names(all_names) <- all_names
    all_names[1:3] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    short_to_long <- c("Shortage - 1st Level (Mead <= 1,075 and >= 1,050)",
                       "Shortage - 2nd Level (Mead < 1,050 and >= 1,025)",       
                       "Shortage - 3rd Level (Mead < 1,025)")
    names(short_to_long) <- paste0("short", 1:3)
    all_names[4:6] <- short_to_long[all_names[4:6]]
    str_wrap(all_names, width = 20)
  }
  
  gg_short <- ggplot(short_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = color_map_3, labels = fill_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Shortage Condition - any amount (Mead <= 1,075')",
      fill = "Shortage Conditions"
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
  
  normal_color_map <- color_map_3[c(1,4)]
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
      legend.spacing.x = unit(.2, "lines"),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = "grey90")
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
  
  names(color_map_2) <- c(
    paste0(scens[1], c("_fc_surplus", "_surplus")), 
    paste0(scens[2], c("_fc_surplus", "_surplus"))
  )
  
  surp_fill_labeler <- function(x) {
    all_names <- names(color_map_2)
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
    scale_fill_manual(values = color_map_2, labels = surp_fill_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Surplus Condition - any amount (Mead >= 1,145 ft)",
      fill = "Surplus Conditions"
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
  
  scen_leg <- get_legend(norm_grob)
  short_leg <- get_legend(short_grob)
  surp_leg <- get_legend(surp_grob)
  
  norm_grob <- ggplotGrob(gg_normal + theme(legend.position = 'none'))
  short_grob <- ggplotGrob(gg_short + theme(legend.position = 'none'))
  surp_grob <- ggplotGrob(gg_surplus + theme(legend.position = 'none'))
  
  # logo -------------------------------
  logo <- imager::load.image("logo/660LT-TK-flush.png")
  logo <- grid::rasterGrob(logo, interpolate = TRUE)
  
  l2 <- ggplot() +
    geom_blank() + 
    theme_minimal() +
    annotation_custom(logo)
  
  cap_text <- paste(
    "The tops of the bars indicate the chances of any shortage or surplus.",
    "The numbers report the chances of the different shortage or surplus conditions.",
    sep = "\n"
  )
  
  cap_grob <- grid::textGrob(cap_text, x = 1, hjust = 1, gp=gpar(fontsize = 9))
  
  gg <- grid.arrange(arrangeGrob(
    short_grob, short_leg, surp_grob, surp_leg, norm_grob, scen_leg, 
    cap_grob, l2,
    layout_matrix = matrix(c(1:5, NA, 6, 6, 7, 8), ncol = 2, byrow = TRUE),
    widths = c(.8, .2),
    heights = c(rep(.9/3, 3), .05, .05)
    #bottom = cap_text
  ))
  
  ggsave(
    file.path(ofolder, "LBConditions.png"), 
    gg, 
    width = 8.5, 
    height = 11, 
    units = "in"
  )
  
  invisible(scen_leg)
}

powell_condition_barplot <- function(sys_data, scens, ofolder, scen_leg)
{
  powell_vars <- c(
    "Equalization Tier", 
    "Equalization - annual release > 8.23 maf",
    "Equalization - annual release = 8.23 maf", 
    "Upper Elevation Balancing Tier",
    "Upper Elevation Balancing - annual release > 8.23 maf",
    "Upper Elevation Balancing - annual release = 8.23 maf", 
    "Upper Elevation Balancing - annual release < 8.23 maf",
    "Mid-Elevation Release Tier",                          
    "Mid-Elevation Balancing - annual release = 8.23 maf",
    "Mid-Elevation Release Tier - annual release = 7.48 maf"
  )
  
  eq_vars <- c("eq", "eq_823")
  names(eq_vars) <- powell_vars[2:3]
  
  ueb_vars <- c("ueb_gt", "ueb_823", "ueb_lt")
  names(ueb_vars) <- powell_vars[5:7]
  
  mer_vars <- c("mer_823","mer_748")
  names(mer_vars) <- powell_vars[9:10]
  
  leb_vars <- c("leb_gt", "leb_823", "leb_lt")
  names(leb_vars) <- c(
    "Lower Elevation Balancing - annual release > 8.23 maf",
    "Lower Elevation Balancing - annual release = 8.23 maf",
    "Lower Elevation Balancing - annual release < 8.23 maf"
  )
  
  # equalization plot --------------------
  tmp <- filter(sys_data, Variable %in% names(eq_vars)) %>%
    mutate(Variable = eq_vars[Variable]) %>%
    spread(Variable, Value) %>%
    mutate(
      eq_fill = eq_823 + eq / 2,
      eq_823_fill = eq_823 / 2
    ) 
  
  eq_data <- full_join(
    select(tmp, -eq_fill, -eq_823_fill) %>%
      gather(Variable, Value, -Year, -Scenario),
    select(tmp, -eq, -eq_823) %>%
      rename(
        eq = eq_fill, 
        eq_823 = eq_823_fill
      ) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  eq_color_map <- color_map_2
  
  names(eq_color_map) <- c(
    paste(scens[1], c("eq", "eq_823"), sep = "_"), 
    paste(scens[2], c("eq", "eq_823"), sep = "_")
  )
  
  eq_labeler <- function(x) {
    all_names <- names(eq_color_map)
    names(all_names) <- all_names
    all_names[1:2] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    short_to_long <- c("Equalization - annual release > 8.23 maf",
                       "Equalization - annual release = 8.23 maf")
    names(short_to_long) <- c("eq", "eq_823")
    all_names[3:4] <- short_to_long[all_names[3:4]]
    str_wrap(all_names, width = 20)
  }
  
  gg_eq <- ggplot(eq_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = eq_color_map, labels = eq_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Equalization Tier",
      fill = "Releases"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines")
    )
  
  # upper elevation balancing -----------------
  
  tmp <- filter(sys_data, Variable %in% names(ueb_vars)) %>%
    mutate(Variable = ueb_vars[Variable]) %>%
    spread(Variable, Value) %>%
    mutate(
      ueb_gt_fill = ueb_lt + ueb_823 + ueb_gt / 2,
      ueb_823_fill = ueb_lt + ueb_823 / 2,
      ueb_lt_fill = ueb_lt / 2
    ) 
  
  ueb_data <- full_join(
    select(tmp, -ueb_gt_fill, -ueb_823_fill, -ueb_lt_fill) %>%
      gather(Variable, Value, -Year, -Scenario),
    select(tmp, -ueb_gt, -ueb_823, -ueb_lt) %>%
      rename(
        ueb_gt = ueb_gt_fill, 
        ueb_823 = ueb_823_fill,
        ueb_lt = ueb_lt_fill
      ) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  ueb_color_map <- color_map_3
  
  names(ueb_color_map) <- c(
    paste(scens[1], ueb_vars, sep = "_"), 
    paste(scens[2], ueb_vars, sep = "_")
  )
  
  ueb_labeler <- function(x) {
    all_names <- names(ueb_color_map)
    names(all_names) <- all_names
    all_names[1:3] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    short_to_long <- names(ueb_vars)
    names(short_to_long) <- ueb_vars
    all_names[4:6] <- short_to_long[all_names[4:6]]
    str_wrap(all_names, width = 20)
  }
  
  ueb_data$fill_var <- factor(ueb_data$fill_var, levels = names(ueb_color_map))
  
  gg_ueb <- ggplot(ueb_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = ueb_color_map, labels = ueb_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Upper Elevation Balacing Tier",
      fill = "Releases"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines")
    )
  
  # mid elevation release tier -------------------
 
  tmp <- filter(sys_data, Variable %in% names(mer_vars)) %>%
    mutate(Variable = mer_vars[Variable]) %>%
    spread(Variable, Value) %>%
    mutate(
      mer_823_fill = mer_748 + mer_823 / 2,
      mer_748_fill = mer_748 / 2
    ) 
  
  mer_data <- full_join(
    select(tmp, -mer_748_fill, -mer_823_fill) %>%
      gather(Variable, Value, -Year, -Scenario),
    select(tmp, -mer_748, -mer_823) %>%
      rename(
        mer_748 = mer_748_fill, 
        mer_823 = mer_823_fill
      ) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  mer_color_map <- color_map_2
  
  names(mer_color_map) <- c(
    paste(scens[1], mer_vars, sep = "_"), 
    paste(scens[2], mer_vars, sep = "_")
  )
  
  mer_labeler <- function(x) {
    all_names <- names(mer_color_map)
    names(all_names) <- all_names
    all_names[1:2] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    
    short_to_long <- names(mer_vars)
    names(short_to_long) <- mer_vars
    
    all_names[3:4] <- short_to_long[all_names[3:4]]
    str_wrap(all_names, width = 20)
  }
  
  mer_data$fill_var <- factor(mer_data$fill_var, levels = names(mer_color_map))
  
  gg_mer <- ggplot(mer_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = mer_color_map, labels = mer_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Mid-Elevation Release Tier",
      fill = "Releases"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines")
    )
  
  # lower elevation balancing --------------------
  
  tmp <- filter(sys_data, Variable %in% names(leb_vars)) %>%
    mutate(Variable = leb_vars[Variable]) %>%
    spread(Variable, Value) %>%
    mutate(
      leb_gt_fill = leb_lt + leb_823 + leb_gt / 2,
      leb_823_fill = leb_lt + leb_823 / 2,
      leb_lt_fill = leb_lt / 2
    ) 
  
  leb_data <- full_join(
    select(tmp, -leb_gt_fill, -leb_823_fill, -leb_lt_fill) %>%
      gather(Variable, Value, -Year, -Scenario),
    select(tmp, -leb_gt, -leb_823, -leb_lt) %>%
      rename(
        leb_gt = leb_gt_fill, 
        leb_823 = leb_823_fill,
        leb_lt = leb_lt_fill
      ) %>%
      gather(Variable, fill_loc, -Year, -Scenario),
    by = c("Year", "Scenario", "Variable")
  ) %>%
    mutate(val_lab = to_percent(Value)) %>%
    mutate(val_lab = if_else(val_lab == "0%", NA_character_, val_lab)) %>%
    unite(fill_var, Scenario, Variable, remove = FALSE)
  
  leb_color_map <- color_map_3
  
  names(leb_color_map) <- c(
    paste(scens[1], leb_vars, sep = "_"), 
    paste(scens[2], leb_vars, sep = "_")
  )
  
  leb_labeler <- function(x) {
    all_names <- names(leb_color_map)
    names(all_names) <- all_names
    all_names[1:3] <- NA_character_
    all_names <- str_split_fixed(all_names, "_", 2)[,2]
    short_to_long <- names(leb_vars)
    names(short_to_long) <- leb_vars
    all_names[4:6] <- short_to_long[all_names[4:6]]
    str_wrap(all_names, width = 20)
  }
  
  leb_data$fill_var <- factor(leb_data$fill_var, levels = names(leb_color_map))
  
  gg_leb <- ggplot(leb_data, aes(Scenario, Value, fill = fill_var)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Year, nrow = 1, strip.position = "bottom") + 
    geom_text(aes(label = val_lab, y = fill_loc), size = 3) +
    scale_fill_manual(values = leb_color_map, labels = leb_labeler) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_y_continuous(
      breaks = seq(0, 100, 10), 
      labels = to_percent
    ) +
    labs(
      y = NULL,
      x = NULL, 
      title = "Lower Elevation Balacing Tier",
      fill = "Releases"
    ) +
    theme_light() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines")
    )
  
  # arrange all 4 plots together -----------------
  
  eq_leg <- get_legend(ggplotGrob(gg_eq))
  ueb_leg <- get_legend(ggplotGrob(gg_ueb))
  mer_leg <- get_legend(ggplotGrob(gg_mer))
  leb_leg <- get_legend(ggplotGrob(gg_leb))
  
  eq_grob <- ggplotGrob(gg_eq + theme(legend.position = 'none'))
  ueb_grob <- ggplotGrob(gg_ueb + theme(legend.position = 'none'))
  mer_grob <- ggplotGrob(gg_mer + theme(legend.position = 'none'))
  leb_grob <- ggplotGrob(gg_leb + theme(legend.position = 'none'))
  
  # logo -------------------------------
  logo <- imager::load.image("logo/660LT-TK-flush.png")
  logo <- grid::rasterGrob(logo, interpolate = TRUE)
  
  l2 <- ggplot() +
    geom_blank() + 
    theme_minimal() +
    annotation_custom(logo)
  
  cap_text <- paste(
    "The tops of the bars indicate the chances of being in the tier.",
    "The numbers report the chances of the different release categories.",
    sep = "\n"
  )
  
  cap_grob <- grid::textGrob(cap_text, x = 1, hjust = 1, gp=gpar(fontsize = 9))
  
  gg <- grid.arrange(arrangeGrob(
    eq_grob, eq_leg, ueb_grob, ueb_leg, mer_grob, mer_leg, leb_grob, leb_leg, 
    scen_leg, cap_grob, l2,
    layout_matrix = matrix(c(1:8, 9, 9, 10, 11), ncol = 2, byrow = TRUE),
    widths = c(.8, .2),
    heights = c(rep(.9/4, 4), .05, .05)
    #bottom = cap_text
  ))
  
  ggsave(
    file.path(ofolder, "UBConditions.png"), 
    gg, 
    width = 8.5, 
    height = 11, 
    units = "in"
  )
}
