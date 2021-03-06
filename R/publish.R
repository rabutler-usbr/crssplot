has_publish <- function(x, ...) {
  UseMethod("has_publish")
}

has_publish.plot_group <- function(x, ...) {
  isTRUE(x[['publish']])
}

has_publish.plot_groups <- function(x, ...) {
  any(sapply(x, function(i) has_publish(i)))
}
