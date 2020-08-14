# scens_plot_range <- function(df, vars, years = NULL, scenarios = NULL, 
#                              plot_colors = NULL, scen_labels = NULL, ...)

# exp_ops <- c("y_lab", "title", "caption", "color_label", "legend_wrap", 
#              "facet_scales", "facet_nrow", "facet_ncol")

test_that("scens_plot_range() works", {
  expect_s3_class(
    ggplot_build(scens_plot_range(ex_pe, "mead_dec_pe")), 
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_range(ex_pe, "mead_dec_pe", y_lab = "feet")),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_range(
      ex_pe, 
      c("powell_dec_pe", "mead_dec_pe"), 
      facet_scales = "free_y"
    )),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_range(ex_pe, "mead_dec_pe", scenarios = "April ST CT")),
    "ggplot_built"
  )
  pc <- c("April ST CT" = "red", "April ST 2007 UCRC" = "black")
  sl <- c("April ST CT" = "s1", "April ST 2007 UCRC" = "s2")
  expect_s3_class(
    ggplot_build(scens_plot_range(ex_pe, "powell_dec_pe", plot_colors = pc, 
                                  scen_labels = sl,
                                  title = "PE", 
                                  caption = "this is a caption")) ,
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_range(
      ex_pe, 
      c("powell_dec_pe", "mead_dec_pe"), 
      facet_scales = "free_y",
      facet_nrow = 2
    )),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot_build(scens_plot_range(
      ex_pe, 
      c("powell_dec_pe", "mead_dec_pe"), 
      facet_scales = "free_y",
      facet_ncol = 2
    )),
    "ggplot_built"
  )
})
