pgs_out <- function(x) {
  assert_that(is.list(x))
  if (length(x) > 0) {
    assert_that(!is.null(names(x)), msg = "names(x) cannot be null")
    all_gg <- all(sapply(x, function(gg) inherits(gg, "gg_list")))
  }
  
  class(x) <- "pgs_out"
  
  x
}

gg_list <- function(...) {
  x <- list(...)
  
  # all entries in x should be ggplot objects
  all_gg <- all(sapply(x, function(gg) is.ggplot(gg)))
  assert_that(all_gg, msg = "all entries must be ggplot2 objects.")
  
  class(x) <- "gg_list"
  
  x
}

# TODO: change this to [[ method?
find_pg <- function(x, pg) {
  if (exists(pg, where = x)) {
    r <- x[[pg]]
  } else {
    r <- NULL
  }
  
  r
}

c.pgs_out <- function(...) {
  list_in <- list(...)
  all_names <- do.call(c, lapply(list_in, names))
  unq_names <- unique(all_names)
  
  if (length(all_names) == length(unq_names)) {
    # all names are unique, so pgs_out will just be list with all new names
    # can rely on c.list
    x <- NextMethod()
    class(x) <- "pgs_out"
  } else {
    # names are not unique, so need to go through and for entries that exist in
    # all input, combine together so there is one entry for each unique name
    x <- list()
    for (pg in unq_names) {
      tmp_pgs <- do.call(c, lapply(list_in, find_pg, pg))
      x[[pg]] <- tmp_pgs
    }
    
    class(x) <- "pgs_out"
  }
  
  x
}

c.gg_list <- function(...) {
  x <- NextMethod()
  class(x) <- "gg_list"
  x
}

# needed for tests because I cannot figure out how to call c.pgs_out and 
# c.gg_list without exporting them, which I don't think we need to do
test_c <- function(...) {
  c(...)
}

is.gg_list <- function(x) {
  inherits(x, "gg_list")
}

is.pgs_out <- function(x) {
  inherits(x, "pgs_out")
}

print.pgs_out <- function(x, ...) {
  cat("Plot groups:\n")
  
  lapply(seq_along(x), function(i) {
    cat(" - ", names(x)[i], " (", length(x[[i]]), " plots)\n", sep = "")
  })
}

print.gg_list <- function(x, ...) {
  lapply(x, print)
}
