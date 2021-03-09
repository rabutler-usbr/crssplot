has_publish <- function(x, ...) {
  UseMethod("has_publish")
}

has_publish.plot_group <- function(x, ...) {
  isTRUE(x[['publish']])
}

has_publish.plot_groups <- function(x, ...) {
  any(sapply(x, function(i) has_publish(i)))
}

#' @pgs pgs_out object (all plot groups and figures)
#' @ui ui object
#' @noRd
save_publish_figs <- function(pgs, ui) {
  # limit the pgs object to those needed by the reports
  # first limit the plot_groups, then limit the figures
  req_figures <- c("powell_heat", "mead_heat", "p3525", "p3490", "m1025", 
                   "m1000", "shortage", "surplus", "powell_cloud", "mead_cloud")
  
  for (i in seq_along(pgs)) {
    if (!has_publish(ui[['plot_group']][[names(pgs[i])]])) {
      pgs[[i]] <- NULL
    } else {
      # limit to only the figures we need
      pgs[[i]] <- pgs[[i]][req_figures]
    }
  }
  
  # get the file path from ui
  ofolder <- get_output_folder(ui, "tempData")
  browser()
  # save rds
  saveRDS(pgs, file.path(ofolder, "publish_pgs.rds"))
  
  # return the saved figures
  pgs
}

