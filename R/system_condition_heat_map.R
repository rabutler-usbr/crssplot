
create_mead_powell_heatmaps <- function(z1, z2, ui, folder_paths)
{
  # loop through all plot_groups, and if create == TRUE, create the heat map
  heat_pgs <- list()
  for (i in seq_along(ui[["plot_group"]])) {
    
    if (ui[["plot_group"]][[i]][["heat"]][["create"]]) {
      
      m_file <- construct_file_name(ui, folder_paths, i, "png_out", "mead_heat.png")
      p_file <-construct_file_name(ui, folder_paths, i, "png_out", "powell_heat.png")
  
      scen_names <- ui[["plot_group"]][[i]][["plot_scenarios"]]
      heat_title <- ui[["plot_group"]][[i]][["heat"]][["title"]]
      
      m_heat <- mead_system_condition_heatmap(
        filter(z1, ScenarioGroup %in% scen_names), 
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
        filter(z2, ScenarioGroup %in% scen_names),
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
      
      heat_pgs[[names(ui[["plot_group"]])[i]]] <- gg_list(
        "powell_heat" = p_heat, "mead_heat" = m_heat
      )
    }
  }
  
  pgs_out(heat_pgs)  
}

#' Call `system_conditions_heat_map()` with Mead specific data. 
#' @param dcp Data frame that includes Year, Agg, Variable, Value. Variable
#'   must include "normal_no_recovery", "normal_recovery", "dcp1" - "dcp8", and
#'   "surplus" variables. 
#' @param heat_ui Additional user input specific to this plot type that must
#'   include `years` and `scen_names`. 
#' @param my_title Title
#' @param y_wrap Number of characters to use in wrapping the y labels.
#' @noRd
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
    mutate(ScenarioGroup = scen_rename[ScenarioGroup]) %>%
    tidyr::spread(Variable, Value) %>%
    mutate(
      n2 = normal_no_recovery + normal_recovery,
      s1_and_2 = dcp2 + dcp3 + dcp4 + dcp5 + dcp6 + dcp7
    ) %>%
    tidyr::gather(Variable, Value, -Year, -ScenarioGroup) %>%
    filter(Variable %in% c("surplus", "n2", "dcp1", "s1_and_2", "dcp8"))

  gg <- vars_plot_heatmap(
    zz, 
    scenarios = unique(zz$ScenarioGroup),
    vars = names(tier_names),
    var_labels = tier_names,
    title = my_title,
    subtitle = "Percent of Traces in each Elevation Range",
    y_lab = "Previous December Elevation",
    caption = heat_ui[['caption']], 
    legend_wrap = 15
  ) %>%
    add_logo_vertical(.92, .02, .99, .22)
  
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
    mutate(ScenarioGroup = scen_rename[ScenarioGroup]) %>%
    tidyr::spread(Variable, Value) %>%
    mutate(
      ueb = uebGt823 + ueb823 + uebLt823,
      mer = mer823 + mer748,
      leb = lebGt823 + leb823 + lebLt823
    ) %>%
    tidyr::gather(Variable, Value, -Year, -ScenarioGroup, -TraceNumber, -Scenario) %>%
    filter(Variable %in% c("eq", "ueb", "mer", "leb"))
  
  gg <- vars_plot_heatmap(
    zz, 
    scenarios = unique(zz$ScenarioGroup),
    vars = names(tier_names),
    var_labels = tier_names,
    title = my_title,
    subtitle = "Percent of Traces in each Elevation Range",
    y_lab = "",
    caption = heat_ui[['caption']],
    legend_wrap = 15
  ) %>%
    add_logo_vertical(.92, .02, .99, .22)
  
  gg
}

add_logo_vertical <- function(gg, left, bottom, right, top)
{
  logo_path <- system.file(
    "extdata/logo/BofR-vert-cmyk-125.png", 
    package = "crssplot"
  )
  
  add_logo_pw(gg, logo_path, left, bottom, right, top)
}

add_logo_pw <- function(gg, logo_path, left, bottom, right, top) {
  
  logo <- png::readPNG(logo_path, native = TRUE)
  
  gg + 
    patchwork::inset_element(logo, left, bottom, right, top, align_to = "full")
}

add_logo_horiz <- function(gg, left, bottom, right, top)
{
  # logo -------------------------------
  logo_path <- system.file(
    "extdata/logo/BofR-horiz-cmyk.png", 
    package = "crssplot"
  )

  add_logo_pw(gg, logo_path, left, bottom, right, top)
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
