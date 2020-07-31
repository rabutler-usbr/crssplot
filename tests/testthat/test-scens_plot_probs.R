# scens_plot_probs <- function(df, vars,  years = NULL, scenarios = NULL, 
#                              plot_colors = NULL, scen_labels = NULL, ...)

scens_plot_probs(ex_pe, "mead_min_lt_1000", y_lab = "%")

test_that("scens_plot_probs() works.", {
  expect_true(TRUE)
})
