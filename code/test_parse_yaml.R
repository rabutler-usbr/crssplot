# initial tests
library(testthat)
library(yaml)
library(RWDataPlyr)

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

# is_r_statement ----------------------
test_that("is_r_statement() works", {
  expect_true(is_r_statement("`r mean(c(1,2))`"))
  expect_true(is_r_statement("`r mean(c(1,2)) + 5`"))
  expect_true(is_r_statement("`r sumfun();nextfun()`"))
  expect_true(is_r_statement("`r mean(median(max(5)))`"))
  expect_true(is_r_statement("`r mean(c(1,2))%>%anotherfun()`"))
  expect_false(is_r_statement("string"))
  expect_false(is_r_statement("r this()"))
  expect_false(is_r_statement("`r this()"))
  expect_false(is_r_statement("r this()`"))
  expect_false(is_r_statement("`this()`"))
  expect_false(is_r_statement("r is new"))
  expect_false(is_r_statement("`r` is my language of choice"))
})

# strip_r_from_string -------------------
test_that("strip_r_from_string() works", {
  expect_identical(strip_r_from_string("`r mean()`"), "mean()")
  expect_identical(strip_r_from_string("`r mean(c(1,2))`"), "mean(c(1,2))")
  expect_identical(strip_r_from_string("`r df$x`"), "df$x")
  expect_identical(strip_r_from_string("`r x$`r is nice``"), "x$`r is nice`")
})

# eval_r_var --------------
df1 <- list(a = month.abb, b = "`r mean(1:12)`", c = "mean(1:12)")
df2 <- list(
  a = month.abb, 
  b = "`r rw_scen_gen_names('DNF', 'CT', c('IG', 'NA'))`",
  c = "`r month.name`"
)
df1_comp <- list(a = month.abb, b = mean(1:12), c = "mean(1:12)")
df2_comp <- list(
  a = month.abb,
  b = rw_scen_gen_names("DNF", "CT", c("IG", "NA")),
  c = month.name
)

test_that("eval_r_var() works", {
  expect_equal(
    eval_r_var(df1, "b") %>% eval_r_var(entry = "c"),
    df1_comp
  )
  
  expect_equal(
    eval_r_var(df2, entry = "b") %>% eval_r_var(entry = "c"),
    df2_comp
  )
})
