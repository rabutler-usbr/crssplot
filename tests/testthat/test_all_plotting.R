
create_figures <- TRUE

setup(unzip(
  "results/auto_tests/tempData/tempData.zip", 
  exdir = "results/auto_tests/tempData"
))

teardown({
  ff <- list.files("results/auto_tests/tempData")
  ff <- ff[ff != "tempData.zip"]
  ff <- file.path("results/auto_tests/tempData", ff)
  file.remove(ff)
})

test_that("everything works.", {
  yi <- parse_yaml_input("../test_simple_figures.yml")
  expect_type(yi, "list")
  
  skip_if_not(create_figures, "skipping process_everything()")
  expect_warning(process_everything(yi))
})
