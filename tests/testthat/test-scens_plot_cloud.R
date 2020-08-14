test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# scens_plot_cloud <- function(df, vars, historical = NULL, years = NULL, 
#                              scenarios = NULL, plot_colors = NULL, 
#                              scen_labels = NULL, ...)

h_mead <- read.csv("inst/extdata/HistMeadPE.csv")

scens_plot_cloud(ex_pe, "mead_dec_pe", historical = h_mead)
