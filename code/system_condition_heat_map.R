

mead_system_condition_heatmap <- function(dcp, yrs, scen_rename, my_title)
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
      panel.border = element_blank()
    ) +
    labs(y = NULL, x = NULL, fill = "%", title = my_title,
         subtitle = "Percent of Traces in each Operating Condition")
}

mead_tier_names <- function() {
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
