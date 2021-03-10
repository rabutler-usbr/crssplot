has_publish <- function(x, ...) {
  UseMethod("has_publish")
}

has_publish.plot_group <- function(x, ...) {
  isTRUE(x[['publish']])
}

has_publish.plot_groups <- function(x, ...) {
  any(sapply(x, function(i) has_publish(i)))
}

#' @param pgs pgs_out object (all plot groups and figures)
#' @param ui ui object
#' @noRd
save_publish_figs <- function(pgs, ui) {
  message("   ... saving publication figures")
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
  # save rds
  saveRDS(pgs, file.path(ofolder, "publish_pgs.rds"))
  
  # return the saved figures
  pgs
}

create_publish_rmds <- function(pg_names, ui) {
  message("   ... creating publication rmds")
  
  output_folder <- get_output_folder(ui, "publish")
  rmd <- system.file(
    "rmarkdown/templates/crss-official-results/skeleton", 
    "skeleton.Rmd", 
    package = "crssplot"
  )
  
  # parameters in the rmd file are figs_file, pub_title, and pg_name. 
  # first two are same for every file, the last is set in loop
  figs_file <- get_output_folder(ui, "tempData/publish_pgs.rds")
  pub_title <- paste(format(Sys.Date(), "%B %Y"), "Official CRSS Results")
  
  # for each plot group (name), get the rmd template, fill it in, and then save
  # it in the publication directory
  o_files <- c()
  for (pg_name in pg_names) {
    tmp_rmd <- knitr::knit_expand(rmd)
    tmp_file <-  stringr::str_replace_all(pg_name, " ", "-") %>%
      fs::path_sanitize()
    tmp_file <- file.path(output_folder, paste0(tmp_file, ".Rmd"))
    xfun::write_utf8(tmp_rmd, tmp_file)
    o_files <- c(o_files, tmp_file)
  }
  
  o_files
}

render_all_files <- function(rmd_files) {

  for (rmd in rmd_files) {
    message("   ... rendering ", rmd)
    rmarkdown::render(rmd)
  }
  
  invisible(rmd_files)
}
