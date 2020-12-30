library(dplyr)

vv <- c("mead_min_lt_1000", "mead_min_lt_1020", "powell_wy_min_lt_3490", 
        "powell_dec_lt_3525")
zz <- filter(ex_pe, Variable %in% vv)

v_names <- c("mead_min_lt_1000" = "Mead < 1,000' in Any Month", 
             "mead_min_lt_1020" = "Mead < 1,020' in Any Month", 
             "powell_wy_min_lt_3490" = "Powell < 3,490' in Any Month in the WY", 
             "powell_dec_lt_3525" = "Powell < 3,525' in December")

v_col <- c("red", "black")
names(v_col) <- vv[1:2]

# line plots ----------------------------------
test_that("vars_plot_probs() works for lines", {
  expect_s3_class(
    ggplot_build(vars_plot_probs(zz, "April ST CT")), 
    "ggplot_built"
  )
  expect_equal(
    vars_plot_probs(zz, "April ST CT"),
    vars_plot_probs(zz, "April ST CT", plot_type = 1)
  )
  expect_equal(
    vars_plot_probs(zz, "April ST CT"),
    vars_plot_probs(zz, "April ST CT", plot_type = 'line')
  )
  expect_s3_class(
    ggplot_build(
      vars_plot_probs(zz, "April ST CT", years = 2021:2040, 
                      var_labels = v_names, legend_wrap = 10)
    ),
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(vars_plot_probs(zz, unique(zz$ScenarioGroup))), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      vars_plot_probs(zz, "April ST CT", vars = vv[1:2], plot_colors = v_col)
    ), 
    "ggplot_built"
  )
})

# stacked bar plots ----------------------------------
vname <- c("Short1" = "Shortage 1", "Short2" = "Shortage 2", 
           "Short3" = "Shortage 3")

vcol <- c("Short1" = "grey80", "Short2" = "grey50", 
          "Short3" = "grey20")
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

test_that("vars_plot_probs() works for stacked bars", {
  expect_s3_class(
    ggplot_build(
      vars_plot_probs(zz, scenarios = "April ST CT", plot_type = "stacked bar")
    ), 
    "ggplot_built"
  )
  
  expect_equal(
    vars_plot_probs(zz, scenarios = "April ST CT", plot_type = "stacked bar"),
    vars_plot_probs(zz, scenarios = "April ST CT", plot_type = 2)
  )
  
  expect_s3_class(
    ggplot_build(
      vars_plot_probs(
        zz, 
        scenarios = c("April ST CT", "April ST 2007 UCRC"), 
        plot_type = "stacked bar"
      )
    ), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      vars_plot_probs(zz, scenarios = "April ST CT", plot_type = 2, 
                      var_labels = vname, plot_colors = vcol)
    ), 
    "ggplot_built"
  )
})
