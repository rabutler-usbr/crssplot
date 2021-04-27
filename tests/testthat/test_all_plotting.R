
create_figures <- TRUE

crss_dir <- file.path(tempdir(), "crss_all_plots")
dir.create(crss_dir)

crss_tmp_dir <- file.path(crss_dir, "results", "auto_tests", "tempData")
dir.create(crss_tmp_dir, recursive = TRUE)

setup(unzip(
  "results/auto_tests/tempData/tempData.zip", 
  exdir = crss_tmp_dir
))

teardown({
  unlink(crss_dir, recursive = TRUE)
})

test_that("everything works.", {
  yi <- parse_yaml_input("../test_simple_figures.yml")
  expect_type(yi, "list")
  
  # overwrite the CRSSDIR entry
  yi$folders$CRSSDIR <- crss_dir
  
  skip_if_not(create_figures, "skipping process_everything()")
  expect_warning(process_everything(yi))
})
