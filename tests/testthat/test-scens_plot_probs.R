# scens_plot_probs <- function(df, vars,  years = NULL, scenarios = NULL, 
#                              plot_colors = NULL, scen_labels = NULL, ...)

# exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
#              "facet_scales", "facet_nrow", "facet_ncol")

test_that("scens_plot_probs() works", {
  expect_s3_class(
    ggplot_build(scens_plot_probs(ex_pe, "mead_min_lt_1000")), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_probs(ex_pe, "powell_wy_min_lt_3525", 
                                  y_lab = "percent")),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_probs(
      ex_pe, 
      c("mead_min_lt_1020", "powell_wy_min_lt_3490"), 
      facet_scales = "free_y"
    )),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_probs(ex_pe, "mead_dec_lt_1025", 
                                  scenarios = "April ST CT")),
    "ggplot_built"
  )
  
  pc <- c("April ST CT" = "red", "April ST 2007 UCRC" = "black")
  sl <- c("April ST CT" = "s1", "April ST 2007 UCRC" = "s2")
  expect_s3_class(
    ggplot_build(scens_plot_probs(ex_pe, "mead_dec_lt_1025", plot_colors = pc, 
                     scen_labels = sl,
                     title = "Mead December < 1,025'", 
                     caption = "this is a caption")),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_probs(
      ex_pe, 
      c("mead_dec_lt_1025", "mead_min_lt_1025"), 
      facet_scales = "free_y",
      facet_nrow = 2
    )),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_probs(
      ex_pe, 
      c("mead_min_lt_1020", "mead_min_lt_1000"), 
      facet_scales = "free_y",
      facet_ncol = 2
    )),
    "ggplot_built"
  )
})
