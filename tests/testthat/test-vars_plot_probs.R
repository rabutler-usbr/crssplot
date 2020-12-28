
vv <- c("mead_min_lt_1000", "mead_min_lt_1020", "powell_wy_min_lt_3490", 
        "powell_dec_lt_3525")
zz <- filter(ex_pe, Variable %in% vv)

v_names <- c("mead_min_lt_1000" = "Mead < 1,000' in Any Month", 
             "mead_min_lt_1020" = "Mead < 1,020' in Any Month", 
             "powell_wy_min_lt_3490" = "Powell < 3,490' in Any Month in the WY", 
             "powell_dec_lt_3525" = "Powell < 3,525' in December")

v_col <- c("red", "black")
names(v_col) <- vv[1:2]

test_that("vars_plot_probs() works", {
  expect_s3_class(
    ggplot_build(vars_plot_probs(zz, "April ST CT")), 
    "ggplot_built"
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
