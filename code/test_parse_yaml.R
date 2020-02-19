# initial tests
library(testthat)
library(yaml)

ft <- yaml.load_file("test.yml")
us <- yaml.load_file("test_underspecified.yml")

# parse_yaml_input -----------------
test_that("parse_yaml_input() works in its entirety", {
  expect_type(parse_yaml_input("test.yml"), "list")
  expect_type(parse_yaml_input("test_underspecified.yml"), "list")
})

# process_data ---------------------
test_that("process_data specification works", {
  expect_setequal(
    ft[["process_data"]],
    process_data <- list(
      sys_cond_data = TRUE,
      pe_data = TRUE,
      csd_data = TRUE,
      crss_short_cond_data = FALSE
    )
  )
  
  expect_setequal(
    us[["process_data"]],
    process_data <- list(
      sys_cond_data = FALSE,
      pe_data = FALSE,
      csd_data = FALSE,
      crss_short_cond_data = FALSE
    )
  )
})

# folders -----------------------------
test_that("folders specification works", {
  us_tmp <- set_folders(us)
  expect_equal(us_tmp[["folders"]][["extra_label"]], "")
})
