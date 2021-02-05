
tst_names <- c(
  "April ST 2007 UCRC" = "2007 Demands", "April ST CT" = "Current Trend Demands"
)
pal <- c(
  "April ST 2007 UCRC" = "#138d75",
  "April ST CT" = "#f1c40f"
)


test_that("scens_plot_range() works", {
  expect_s3_class(
    ggplot_build(scens_plot_boxplot(ex_pe, vars = "powell_dec_pe")), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      scens_plot_boxplot(
        ex_pe, 
        vars = c("powell_dec_pe", "mead_dec_pe"), 
        years = 2021:2036,
        title = "Mead and Powell", subtitle = "End-of-December Elevation",
        y_lab = "(feet)", caption = "Results from April 20xx",
        facet_scales = "free_y", 
        plot_colors = pal,
        scen_labels = tst_names,
        legend_wrap = 10
      )
    ), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      scens_plot_boxplot(
        ex_pe, 
        vars = c("powell_dec_pe", "mead_dec_pe"), 
        title = "Mead and Powell", subtitle = "End-of-December Elevation",
        y_lab = "(feet)", caption = "Results from April 20xx",
        facet_scales = "free_y", 
        plot_colors = pal,
        scen_labels = tst_names,
        legend_wrap = 10,
        facet_nrow = 2
      )
    ), 
    "ggplot_built"
  )
})
