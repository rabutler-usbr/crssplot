library(tidyverse)

mead_system_condition_heatmap <- function(dcp, yrs, scen_rename, my_title,
                                          y_wrap = 15)
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
      Variable = factor(
        str_wrap(tier_names[Variable], y_wrap), 
        levels = rev(str_wrap(tier_names, y_wrap))
      )
    )
  
  gg <- system_conditions_heat_map(
    zz, 
    n_yrs, 
    tier_names, 
    my_title,
    y_title = "Previous Decmeber Elevation"
  ) %>%
    add_logo()
  
  gg
}

powell_system_condition_heatmap <- function(dcp, yrs, scen_rename, my_title, 
                                            y_wrap = 15)
{
  tier_names <- powell_tier_names()
  n_yrs <- length(yrs)

    # convert from all of the different dcp tiers to the simplified number of rows
  # then add in labels
  zz <- dcp %>%
    ungroup() %>%
    select(-Month) %>%
    filter(Year %in% yrs) %>%
    mutate(Agg = scen_rename[Agg]) %>%
    spread(Variable, Value) %>%
    mutate(
      ueb = uebGt823 + ueb823 + uebLt823,
      mer = mer823 + mer748,
      leb = lebGt823 + leb823 + lebLt823
    ) %>%
    gather(Variable, Value, -Year, -Agg, -TraceNumber, -Scenario) %>%
    filter(Variable %in% c("eq", "ueb", "mer", "leb")) %>%
    group_by(Year, Agg, Variable) %>%
    summarise(Value = mean(Value)) %>%
    ungroup() %>%
    mutate(Value = if_else(Value == 0, NA_real_, Value * 100)) %>%
    mutate(
      val_lab = paste0(formatC(Value, digits=0, format = "f"), "%"),
      val_lab = if_else(val_lab == "NA%", "", val_lab),
      val_lab = if_else(val_lab == "0%", "<1%", val_lab)
    ) %>%
    mutate(
      Agg = str_wrap(Agg, width = 10), 
      Variable = factor(
        str_wrap(tier_names[Variable], y_wrap), 
        levels = rev(str_wrap(tier_names, y_wrap))
      )
    )
  
  gg <- system_conditions_heat_map(
    zz, 
    n_yrs, 
    tier_names, 
    my_title,
    y_title = ''
  ) %>%
    add_logo()
  
  gg
}

add_logo <- function(gg)
{
  # arrange all 3 plots together
  gg_grob <- ggplotGrob(gg)
  
  # logo -------------------------------
  logo <- imager::load.image("logo/660LT-TK-flush.png")
  logo <- grid::rasterGrob(logo, interpolate = TRUE)
  
  l2 <- ggplot() +
    geom_blank() + 
    theme_minimal() +
    annotation_custom(logo)

  gg <- grid.arrange(arrangeGrob(
    gg_grob, nullGrob(), l2,
    layout_matrix = matrix(c(1,1,2,3), ncol = 2, byrow = TRUE),
    heights = c(.9, .1),
    widths = c(.8, .2)
    #bottom = cap_text
  ))
  
  gg
}

system_conditions_heat_map <- function(zz, n_yrs, tier_names, my_title, y_title)
{
  # plot as a side-by-side heatmap
  zz %>%
    ggplot(aes(as.factor(Year), Variable, fill = Value)) +
    facet_wrap(~Agg, nrow = 1, strip.position = "top", labeller = label_wrap_gen()) +
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
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(color = "grey30", margin = margin(r = 12)),
      strip.text = element_text(size = 12),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines"), 
      legend.key.height = unit(2, "lines"),
      legend.spacing.x = unit(0, "lines"),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    labs(
      y = y_title, 
      x = NULL, fill = "%", title = my_title,
      subtitle = "Percent of Traces in each Elevation Range"
    )
}

powell_tier_names <- function() {
  c(
    "eq" = "Equalization Tier (Powell >= Equalization [EQ] Elevation)",
    "ueb" = 
      "Upper Elevation Balancing Tier (Powell < EQ Elevation and >= 3,575')",
    "mer" = "Mid-Elevation Release Tier (Powell < 3,575' and >= 3,525')",
    "leb" = "Lower Elevation Balancing Tier (Powell < 3,525')"
  )
}

mead_tier_names <- function() {
  c(
    "surplus" = "Mead >= 1,145'",
    "n2" = "Mead < 1,145' and > 1,090'", 
    "dcp1" = "Mead <= 1,090' and > 1,075'",
    "s1_and_2" = "Mead <= 1,075' and >= 1,025'",
    "dcp8" = "Mead < 1,025'"
  )
}
