library(tidyverse)
library(cowplot)
library(magick)

create_mead_powell_heatmaps <- function(z1, z2, ui, folder_paths)
{
  # loop through all plot_groups, and if create == TRUE, create the heat map
  
  for (i in seq_along(ui[["plot_group"]])) {
    
    if (ui[["plot_group"]][[i]][["heat"]][["create"]]) {
      
      m_file <- construct_file_name(ui, folder_paths, i, "png_out", "mead_heat.png")
      p_file <-construct_file_name(ui, folder_paths, i, "png_out", "powell_heat.png")
  
      scen_names <- ui[["plot_group"]][[i]][["plot_scenarios"]]
      heat_title <- ui[["plot_group"]][[i]][["heat"]][["title"]]
      
      m_heat <- mead_system_condition_heatmap(
        filter(z1, Agg %in% scen_names), 
        ui[["plot_group"]][[i]][["heat"]],
        my_title = paste("Lake Mead Conditions from", heat_title)
      )
      
      ggsave(
        m_file, 
        plot = m_heat, 
        width = 8.91, 
        height = 5.65, 
        units = "in"
      )
      message("   ... saved ", m_file)
      
      p_heat <- powell_system_condition_heatmap(
        filter(z2, Agg %in% scen_names),
        ui[["plot_group"]][[i]][["heat"]],
        my_title = paste("Lake Powell Conditions from", heat_title)
      )
      
      ggsave(
        p_file, 
        plot = p_heat, 
        width = 8.91, 
        height = 5.65, 
        units = "in"
      )
      message("   ... saved ", p_file)
    }
  }
  invisible(TRUE)
}

mead_system_condition_heatmap <- function(dcp, heat_ui, my_title, y_wrap = 15)
{
  yrs <- heat_ui$years
  scen_rename <- heat_ui$scen_names
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
    y_title = "Previous Decemeber Elevation",
    heat_ui
  ) %>%
    add_logo_vertical()
  
  gg
}

powell_system_condition_heatmap <- function(dcp, heat_ui, my_title, y_wrap = 15)
{
  yrs <- heat_ui$years
  scen_rename <- heat_ui$scen_names
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
    y_title = '',
    heat_ui
  ) %>%
    add_logo_vertical()
  
  gg
}

add_logo_vertical <- function(gg)
{
  # now uses cowplot::draw_image which relies on "magick"
  logo_path <- "logo/BofR-vert-cmyk.png"
  
  ggdraw(gg) +
    draw_image(
      logo_path, 
      x = 1.455, y = .13, 
      hjust = 1, vjust = 1, 
      width = 1, height = .12
    )
}

add_logo_horiz <- function(gg)
{
  # arrange all 3 plots together
  gg_grob <- ggplotGrob(gg)
  
  # logo -------------------------------
  logo <- imager::load.image("logo/BofR-horiz-cmyk.png")
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

add_logo_shield <- function(gg)
{
  # arrange all 3 plots together
  gg_grob <- ggplotGrob(gg)
  
  # logo -------------------------------
  #saved from the BOR page footer
  logo <- imager::load.image("code/logo/seal-white.png") 
  
  logo <- grid::rasterGrob(logo, interpolate = TRUE)
  
  l2 <- ggplot() +
    geom_blank() + 
    theme_minimal() +
    annotation_custom(logo)
  
  gg <- grid.arrange(arrangeGrob(
    gg_grob, nullGrob(), l2,
    layout_matrix = matrix(c(1,1,2,3), ncol = 2, byrow = TRUE),
    heights = c(.9, .1),
    widths = c(.9, .1)
    #bottom = cap_text
  ))
  
  gg
}

system_conditions_heat_map <- function(zz, n_yrs, tier_names, my_title, y_title,
                                       heat_ui)
{
  # plot as a side-by-side heatmap
  zz %>%
    ggplot(aes(as.factor(Year), Variable, fill = Value)) +
    facet_wrap(
      ~Agg, 
      nrow = 1, 
      strip.position = "top", 
      labeller = label_wrap_gen()
    ) +
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
      subtitle = "Percent of Traces in each Elevation Range",
      caption = heat_ui[['caption']]
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
