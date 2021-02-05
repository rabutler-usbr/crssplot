

zz <- mutate(ex_pe, color_cat = case_when(
  Value > 1095 ~ "No concern",
  Value > 1076 ~ "Some concern",
  Value > 1074 ~ "Moderate concern",
  TRUE ~ "concern")
)

cc <- c("No concern" = "grey20", "Some concern" = "blue", 
        "Moderate concern" = "steelblue", "concern" = "red")

test_that("multiplication works", {
  expect_s3_class(
    ggplot_build(
      var_plot_trace_scatter(
        ex_pe, 
        vars = "mead_dec_pe", 
        years = 2021, 
        scenarios = "April ST CT"
      )
    ), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      var_plot_trace_scatter(
        zz, 
        vars = "mead_dec_pe", 
        years = 2021, 
        scenarios = "April ST CT", 
        color_by = "color_cat"
      )
    ), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      var_plot_trace_scatter(
        zz, 
        vars = "mead_dec_pe", 
        years = 2021, 
        scenarios = "April ST CT", 
        color_by = "color_cat",
        plot_colors = cc
      )
    ), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      var_plot_trace_scatter(
        zz, 
        vars = "mead_dec_pe", 
        years = 2021, 
        scenarios = unique(zz$ScenarioGroup), 
        color_by = "color_cat",
        plot_colors = cc,
        title = "Mead end of 2021 elevation", subtitle = "colored by concern", 
        y_lab = "feet"
      )
    ), 
    "ggplot_built"
  )
  
  expect_error(
    var_plot_trace_scatter(
      ex_pe, 
      vars = "mead_dec_pe", 
      years = 2021:2026, 
      scenarios = "April ST CT"
    )
  )
  
  expect_error(
    var_plot_trace_scatter(
      ex_pe, 
      vars = c("mead_dec_pe", "mead_dec_pe"), 
      years = 2026, 
      scenarios = "April ST CT"
    )
  )
})
