library(yaml)

ft <- parse_yaml_input("../test.yml")
us <- parse_yaml_input("../test_underspecified.yml")

# parse_yaml_input -----------------
test_that("parse_yaml_input() works in its entirety", {
  expect_type(parse_yaml_input("../test.yml"), "list")
  expect_type(parse_yaml_input("../test_underspecified.yml"), "list")
})

# process_data ---------------------
test_that("process_data specification works", {
  expect_mapequal(
    ft[["process_data"]],
    process_data <- list(
      sys_cond_data = TRUE,
      pe_data = TRUE,
      csd_data = TRUE,
      crss_short_cond_data = FALSE
    )
  )

  expect_mapequal(
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
  us_tmp <- crssplot:::set_folders(us)
  expect_equal(us_tmp[["folders"]][["extra_label"]], "")
})

# is_r_statement ----------------------
test_that("is_r_statement() works", {
  irs <- crssplot:::is_r_statement
  expect_true(irs("`r mean(c(1,2))`"))
  expect_true(irs("`r mean(c(1,2)) + 5`"))
  expect_true(irs("`r sumfun();nextfun()`"))
  expect_true(irs("`r mean(median(max(5)))`"))
  expect_true(irs("`r mean(c(1,2))%>%anotherfun()`"))
  expect_false(irs("string"))
  expect_false(irs("r this()"))
  expect_false(irs("`r this()"))
  expect_false(irs("r this()`"))
  expect_false(irs("`this()`"))
  expect_false(irs("r is new"))
  expect_false(irs("`r` is my language of choice"))
})

# strip_r_from_string -------------------
test_that("strip_r_from_string() works", {
  srfs <- crssplot:::strip_r_from_string
  expect_identical(srfs("`r mean()`"), "mean()")
  expect_identical(srfs("`r mean(c(1,2))`"), "mean(c(1,2))")
  expect_identical(srfs("`r df$x`"), "df$x")
  expect_identical(srfs("`r x$`r is nice``"), "x$`r is nice`")
})

# eval_r_var --------------
df1 <- list(a = month.abb, b = "`r mean(1:12)`", c = "mean(1:12)")
df2 <- list(
  a = month.abb,
  b = "`r RWDataPlyr::rw_scen_gen_names('DNF', 'CT', c('IG', 'NA'))`",
  c = "`r month.name`"
)
df1_comp <- list(a = month.abb, b = mean(1:12), c = "mean(1:12)")
df2_comp <- list(
  a = month.abb,
  b = RWDataPlyr::rw_scen_gen_names("DNF", "CT", c("IG", "NA")),
  c = month.name
)

test_that("eval_r_var() works", {
  expect_equal(
    crssplot:::eval_r_var(df1, "b") %>% eval_r_var(entry = "c"),
    df1_comp
  )

  expect_equal(
    crssplot:::eval_r_var(df2, entry = "b") %>% eval_r_var(entry = "c"),
    df2_comp
  )
})
