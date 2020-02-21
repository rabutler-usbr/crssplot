library(testthat)
library(yaml)

tst <- yaml.load_file("test.yml")

ui <- set_defaults(tst)
defaults <- ui[["defaults"]]
ui <- parse_yaml_input("test.yml")

test_that("plot_group is properly created", {
  expect_s3_class(
    t1 <- plot_group(tst[["plot_groups"]][[1]][["aug2obs"]], defaults), 
    "plot_group"
  )
  expect_identical(
    t1[["scen_names"]], 
    c("Aug 2018 - IG" = "Aug 2018 - IG", "Aug 2018 - NA" = "Aug 2018 - NA") 
  )
  expect_null(t1[["caption"]])
  expect_setequal(t1[["plots"]], c("std_comparison", "csd_ann", "heat", "cloud"))
  expect_equal(t1[["years"]], defaults[["plot_years"]])
  expect_equal(
    t1[["plot_colors"]], 
    c("Aug 2018 - IG" = "#F8766D", "Aug 2018 - NA" = "#00BFC4")
  )
  
  expect_s3_class(
    t2 <- plot_group(tst[["plot_groups"]][[2]][["aug2june"]], defaults),
    "plot_group"
  )
  expect_identical(
    t2[["scen_names"]], 
    c("Aug 2018 - IG" = "Aug 2018", "June 2019 w/ DCP" = "June 2019 DCP")
  )
  expect_equal(t2[["caption"]], "Every plot gets this")
  expect_setequal(t2[["plots"]], c("std_comparison", "heat"))
  expect_equal(t2[["years"]], 2018:2036)
  expect_equal(
    t2[["plot_colors"]],
    c("Aug 2018 - IG" = "#DEEBF7", "June 2019 w/ DCP" = "#9ECAE1")
  )
  
  expect_equal(ui[["plot_groups"]][["aug2obs"]], t1)
  expect_equal(ui[["plot_groups"]][["aug2june"]], t2)
})
