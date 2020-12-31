vname <- c("Short1" = "Shortage 1", "Short2" = "Shortage 2", 
           "Short3" = "Shortage 3")

zz <- filter(ex_pe, Variable == "mead_dec_pe") %>%
  mutate(Shortage = case_when(
    Value <= 1075 & Value > 1050 ~ 1,
    Value <= 1050 & Value > 1025 ~ 2, 
    Value <= 1025 ~ 3,
    TRUE ~ 0
  )) %>%
  mutate(
    Short1 = if_else(Shortage == 1, 1, 0),
    Short2 = if_else(Shortage == 2, 1, 0),
    Short3 = if_else(Shortage == 3, 1, 0)
  ) %>%
  select(-Variable, -Value, -Shortage) %>%
  tidyr::pivot_longer(c("Short1", "Short2", "Short3"), names_to = "Variable",
                      values_to = "Value")

# line plots ----------------------------------
test_that("vars_plot_probs() works for lines", {
  expect_s3_class(
    ggplot_build(vars_plot_heatmap(zz, scenarios = "April ST CT")), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(vars_plot_heatmap(zz, scenarios = "April ST CT", 
                                   vars = c("Short2", "Short1", "Short3"))), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      vars_plot_heatmap(
        zz, 
        scenarios = unique(zz$ScenarioGroup), 
        years = 2020:2026, 
        y_lab = "shortage", 
        var_labels = vname,
        title = "ok", subtitle = "then", caption = "here and now", color_label = "prct"
      )
    ), 
    "ggplot_built"
  )
})
