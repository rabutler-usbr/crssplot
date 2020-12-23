
# scens_plot_cloud <- function(df, vars, historical = NULL, years = NULL, 
#                              scenarios = NULL, plot_colors = NULL, 
#                              scen_labels = NULL, ...)

h_mead <- read.csv(system.file("extdata/HistMeadPE.csv", package = "crssplot"))
h_powell <- read.csv(
  system.file("inst/extdata/HistPowellPE.csv", package = "crssplot")
)
hh <- h_mead
colnames(hh)[2] <- "mead_dec_pe"
hh$powell_dec_pe <- h_powell[,2]

pal <- c(
  "April ST 2007 UCRC" = "#138d75",
  "April ST CT" = "#f1c40f"
)

scens_plot_cloud(ex_pe, "mead_dec_pe", historical = h_mead, legend_wrap = 20,
                 plot_colors = pal, y_lab = "feet", years = 2020:2026) +
  theme_cloud()

test_that("scens_plot_cloud() works", {
  expect_s3_class(
    ggplot_build(scens_plot_cloud(ex_pe, "mead_dec_pe")), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(scens_plot_cloud(ex_pe, "mead_dec_pe", historical = h_mead, 
                                  legend_wrap = 20,
                                  plot_colors = pal, y_lab = "feet", 
                                  years = 2020:2026)), 
    "ggplot_built"
  )
  
  expect_s3_class(
    ggplot_build(
      scens_plot_cloud(ex_pe, c("powell_dec_pe", "mead_dec_pe"), 
                      historical = hh, legend_wrap = 20,
                      plot_colors = pal, y_lab = "feet", 
                      years = 2020:2026,
                      facet_scales = "free_y", fill_label = "ok then")
    ), 
    "ggplot_built"
  )
})
