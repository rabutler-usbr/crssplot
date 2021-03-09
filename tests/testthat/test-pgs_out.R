library(ggplot2)

p1 <- ggplot(economics, aes(date, unemploy)) + geom_line()
p2 <- ggplot(economics, aes(date, unemploy)) + geom_bar(stat = 'identity')

test_that("can create and combine gg_list and pgs_out objects", {
  # create -------------
  expect_type(g1 <- crssplot:::gg_list(p1, p2), "list")
  expect_s3_class(g1, "gg_list")
  expect_true(crssplot:::is.gg_list(g1))
  expect_type(pgs <- crssplot:::pgs_out(list("a" = g1, "b" = g1)), "list")
  expect_s3_class(pgs, "pgs_out")
  expect_true(crssplot:::is.pgs_out(pgs))
  expect_true(crssplot:::is.gg_list(pgs[["a"]]))
  
  # combine ------------
  expect_s3_class(g2 <- crssplot:::test_c(g1, g1), "gg_list")
  expect_length(g2, 4)
  pgs2 <- crssplot:::pgs_out(list("d" = g1))
  expect_s3_class(pgs3 <- crssplot:::test_c(pgs, pgs2), "pgs_out")
  expect_length(pgs3, 3)
  expect_setequal(names(pgs3), c("a", "b", "d"))
  expect_s3_class(pgs4 <- crssplot:::test_c(pgs, pgs), "pgs_out")
  expect_length(pgs4, 2)
  expect_length(pgs4[['a']], 4)
  expect_length(pgs4[['b']], 4)
  expect_setequal(names(pgs4), c("a", "b"))
})
