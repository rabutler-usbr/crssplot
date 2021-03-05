#' Get the legend from ggplot
#' 
#' `gg_get_legend()` gets the legend from a ggplot object and returns as a 
#' gtable grob. 
#' 
#' @param gg object inheriting from `gg`. 
#' 
#' @return gtable, grob
#' 
#' @export
gg_get_legend <- function(gg) {
  assert_that(methods::is(gg, "gg"))
  
  grob <- ggplotGrob(gg)
  
  grob$grobs[[which(sapply(grob$grobs, function(x) x$name) == "guide-box")]]
}
