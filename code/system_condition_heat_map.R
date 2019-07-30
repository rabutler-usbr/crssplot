

mead_system_condition_heatmap1 <- function(dcp, yrs, scen_rename, my_title)
{
  tier_names <- mead_tier_names()

  zz <- dcp %>%
    ungroup() %>%
    filter(Year %in% yrs) %>%
    mutate(Value = if_else(Value == 0, NA_real_, Value * 100)) %>%
    mutate(val_lab = formatC(Value, digits=0, format = "f")) %>%
    mutate(
      val_lab = if_else(val_lab == "NA", "", val_lab),
      val_lab = if_else(val_lab == "0", "<1", val_lab)
    ) %>%
    mutate(Agg = scen_rename[Agg]) %>%
    mutate(
      Agg = str_wrap(Agg, width = 10), 
      Variable = factor(tier_names[Variable], levels = rev(tier_names))
    )
  
  zz %>%
    ggplot(aes(Agg, Variable, fill = Value)) +
    facet_wrap(~Year, nrow = 1, strip.position = "top") +
    geom_tile() +
    #scale_fill_gradient2(low="#12c2e9", high = "#c471ed", mid = "white", na.value = "grey90", midpoint = 25) +
    #scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    # from https://uigradients.com/#summer
    # replace summer with .....punyeta, 
    scale_fill_gradient(low = "#F8FFAE", high = "#43C6AC", na.value = "grey90") +
    #scale_fill_gradient(low = "#ef8e38", high = "#108dc7", na.value = "grey90") +
    # log1p transformation also sort of helps
    #scale_fill_gradient(low = "#F8FFAE", high = "#43C6AC", na.value = "grey90", trans = "sqrt") + 
    geom_vline(xintercept = 1.5, color = "white", size = 1) +
    geom_hline(
      yintercept = seq(0.5, length(tier_names) + 0.5), 
      color = "white", size = 1
    ) +
    #theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE)
    geom_text(aes(label = val_lab), size = 3, color = "black") +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "black")
    ) +
    labs(y = NULL, x = NULL, fill = "%", title = my_title,
         subtitle = "Percent of Traces in each Operating Condition")
}

mead_system_condition_heatmap2 <- function(dcp, yrs, scen_rename, my_title)
{
  tier_names <- mead_tier_names()
  
  zz <- dcp %>%
    ungroup() %>%
    filter(Year %in% yrs) %>%
    mutate(Value = if_else(Value == 0, NA_real_, Value * 100)) %>%
    mutate(val_lab = formatC(Value, digits=0, format = "f")) %>%
    mutate(
      val_lab = if_else(val_lab == "NA", "", val_lab),
      val_lab = if_else(val_lab == "0", "<1", val_lab)
    ) %>%
    mutate(Agg = scen_rename[Agg]) %>%
    mutate(
      Agg = str_wrap(Agg, width = 10), 
      Variable = factor(tier_names[Variable], levels = rev(tier_names))
    )
  
  zz %>%
    ggplot(aes(as.factor(Year), Variable, fill = Value)) +
    facet_wrap(~Agg, ncol = 1, strip.position = "top") +
    geom_tile() +
    #scale_fill_gradient2(low="#12c2e9", high = "#c471ed", mid = "white", na.value = "grey90", midpoint = 25) +
    #scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    # from https://uigradients.com/#summer
    # replace summer with .....punyeta, 
    #scale_fill_gradient(low = "#F8FFAE", high = "#43C6AC", na.value = "grey90") +
    #scale_fill_gradient(low = "#ef8e38", high = "#108dc7", na.value = "grey90") +
    # log1p transformation also sort of helps
    scale_fill_gradient(low = "#F8FFAE", high = "#43C6AC", na.value = "grey90", trans = "sqrt") + 
    #geom_vline(xintercept = 1.5, color = "white", size = 1) +
    geom_hline(
      yintercept = seq(0.5, length(tier_names) + 0.5), 
      color = "white", size = 1
    ) +
    #theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE)
    geom_text(aes(label = val_lab), size = 3, color = "black") +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines"),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    labs(y = NULL, x = NULL, fill = "%", title = my_title,
         subtitle = "Percent of Traces in each Operating Condition")
}

mead_system_condition_heatmap <- function(dcp, yrs, scen_rename, my_title)
{
  tier_names <- mead_tier_names()
  
  n_yrs <- length(yrs)
  
  # convert from all of the different dcp tiers to the simplified number of rows
  # then add in labels
  zz <- dcp %>%
    ungroup() %>%
    filter(Year %in% yrs) %>%
    mutate(Agg = scen_rename[Agg]) %>%
    spread(Variable, Value) %>%
    mutate(
      n2 = normal_no_recovery + normal_recovery,
      s1_and_2 = dcp2 + dcp3 + dcp4 + dcp5 + dcp6 + dcp7
    ) %>%
    gather(Variable, Value, -Year, -Agg) %>%
    filter(Variable %in% c("surplus", "n2", "dcp1", "s1_and_2", "dcp8")) %>%
    mutate(Value = if_else(Value == 0, NA_real_, Value * 100)) %>%
    mutate(
      val_lab = paste0(formatC(Value, digits=0, format = "f"), "%"),
      val_lab = if_else(val_lab == "NA%", "", val_lab),
      val_lab = if_else(val_lab == "0%", "<1%", val_lab)
    ) %>%
    mutate(
      Agg = str_wrap(Agg, width = 10), 
      Variable = factor(tier_names[Variable], levels = rev(tier_names))
    )
  
  system_conditions_heat_map(zz, n_yrs, tier_names, my_title)
}

powell_system_condition_heatmap <- function(dcp, yrs, scen_rename, my_title)
{
  tier_names <- powell_tier_names()
  
  n_yrs <- length(yrs)
  
  # convert from all of the different dcp tiers to the simplified number of rows
  # then add in labels
  zz <- dcp %>%
    ungroup() %>%
    filter(Year %in% yrs) %>%
    mutate(Agg = scen_rename[Agg]) %>%
    spread(Variable, Value) %>%
    mutate(
      ueb = uebGt823 + ueb823 + uebLt823,
      mer = mer823 + mer748,
      leb = lebGt823 + leb823 + lebLt823
    ) %>%
    gather(Variable, Value, -Year, -Agg) %>%
    filter(Variable %in% c("eq", "ueb", "mer", "leb")) %>%
    mutate(Value = if_else(Value == 0, NA_real_, Value * 100)) %>%
    mutate(
      val_lab = paste0(formatC(Value, digits=0, format = "f"), "%"),
      val_lab = if_else(val_lab == "NA%", "", val_lab),
      val_lab = if_else(val_lab == "0%", "<1%", val_lab)
    ) %>%
    mutate(
      Agg = str_wrap(Agg, width = 10), 
      Variable = factor(tier_names[Variable], levels = rev(tier_names))
    )
  
  system_conditions_heat_map(zz, n_yrs, tier_names, my_title)
}

system_conditions_heat_map <- function(zz, n_yrs, tier_names, my_title)
{
  # plot as a side-by-side heatmap
  zz %>%
    ggplot(aes(as.factor(Year), Variable, fill = Value)) +
    facet_wrap(~Agg, nrow = 1, strip.position = "top") +
    geom_tile() +
    # from https://uigradients.com/#HoneyDew
    scale_fill_gradient(
      low = "#F8FFAE", 
      high = "#43C6AC", 
      na.value = "grey90", 
      trans = "sqrt"
    ) + 
    geom_vline(xintercept = seq(1.5, n_yrs, 1), color = "white", size = 1) +
    geom_hline(
      yintercept = seq(0.5, length(tier_names) + 0.5), 
      color = "white", size = 1
    ) +
    geom_text(aes(label = val_lab), size = 3, color = "black") +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines"),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    labs(
      y = "Previous-Decmeber Elevation", 
      x = NULL, fill = "%", title = my_title,
      subtitle = "Percent of Traces in each Elevation Range"
    )
}

powell_tier_names <- function() {
  c(
    "eq" = "Powell >= Equalization Elevation",
    "ueb" = "Powell < Equalization Elevation and >= 3,575'",
    "mer" = "Powell < 3,575' and >= 3,525'",
    "leb" = "Powell < 3,525'"
  )
}

mead_tier_names1 <- function() {
  c(
    "surplus" = "Surplus + Recovery of DCP ICS",
    "dcp_recovery" = "Normal + Recovery of DCP ICS",
    "normal" = "Normal",
    "dcp1" = "Normal + DCP L1",
    "dcp2" = "DCP L1 + Shortage L1",
    "dcp3" = "DCP L1 + Shortage L2",
    "dcp4" = "DCP L2 + Shortage L2",
    "dcp5" = "DCP L3 + Shortage L2",
    "dcp6" = "DCP L4 + Shortage L2",
    "dcp7" = "DCP L5 + Shortage L2",
    "dcp8" = "DCP L5 + Shortage L3"
  )
}


mead_tier_names2 <- function() {
  c(
    "surplus" = "Surplus + Recovery of DCP ICS\n(Mead >= 1,145')",
    "dcp_recovery" = "Normal + Recovery of DCP ICS\n(Mead < 1,145' and > 1,110')",
    "normal" = "Normal\n(Mead <= 1,110' and > 1,090')",
    "dcp1" = "Normal + DCP L1\n(Mead <= 1,090' and > 1,075')",
    "dcp2" = "DCP L1 + Shortage L1\n(Mead <= 1,075' and >= 1,050')",
    "dcp3" = "DCP L1 + Shortage L2\n(Mead < 1,050' and > 1,045')",
    "dcp4" = "DCP L2 + Shortage L2\n(Mead <= 1,045' and > 1,040')",
    "dcp5" = "DCP L3 + Shortage L2\n(Mead <= 1,040' and > 1,035')",
    "dcp6" = "DCP L4 + Shortage L2\n(Mead <= 1,035' and > 1,030')",
    "dcp7" = "DCP L5 + Shortage L2\n(Mead <= 1,030' and >= 1,025')",
    "dcp8" = "DCP L5 + Shortage L3\n(Mead < 1,025')"
  )
}

mead_tier_names <- function() {
  c(
    "surplus" = "Mead >= 1,145'",
    "n2" = "Mead 1,145' - 1,090'", 
    "dcp1" = "Mead 1,090' - 1,075'",
    "s1_and_2" = "Mead 1,075' - 1,025'",
    "dcp8" = "Mead < 1,025'"
  )
}
