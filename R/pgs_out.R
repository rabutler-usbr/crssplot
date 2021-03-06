pgs_out <- function(x) {
  assert_that(is.list(x))
  assert_that(!is.null(names(x)), msg = "names(x) cannot be null")
  
  all_gg <- all(sapply(x, function(gg) inherits(gg, "gg_list")))
  
  class(x) <- "pgs_out"
  
  x
}

gg_list <- function(x) {
  assert_that(is.list(x))
  
  # all entries in x should be ggplot objects
  all_gg <- all(sapply(x, function(gg) is.ggplot(gg)))
  assert_that(all_gg, msg = "all entries must be ggplot2 objects.")
  
  class(x) <- "gg_list"
  
  x
}

c.pgs_out <- function(...) {
  x <- NextMethod()
  class(x) <- "pgs_out"
  x
}

c.gg_list <- function(...) {
  x <- NextMethod()
  class(x) <- "gg_list"
  x
}
