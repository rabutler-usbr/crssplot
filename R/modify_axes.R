#' Move axis positions and match axis scales
#' 
#' `gg_move_y_axis()` moves the current y axis to the specified `position`. 
#' `pw_match_y_axis()` ensures that the y axes in a patchwork object all use 
#' the same range.
#' 
#' @param gg Object inheriting from `gg`.
#' 
#' @param position "right" or "left"
#' 
#' @examples 
#' library(ggplot2)
#' library(dplyr)
#' library(patchwork)
#' 
#' # two plots with different y-axis ranges
#' p1 <- ex_pe %>%
#'   filter(Variable == "mead_dec_pe", ScenarioGroup == "April ST CT", 
#'         TraceNumber == 1) %>%
#'   ggplot(aes(Year, Value)) +
#'   geom_line()
#'   
#' p2 <- ex_pe %>%
#'   filter(Variable == "mead_dec_pe", ScenarioGroup == "April ST CT", 
#'         TraceNumber == 15) %>%
#'   ggplot(aes(Year, Value)) +
#'   scale_y_continuous(labels = scales::comma) +
#'   geom_line()
#'   
#' # default patchwork has two y-axes both on the left, and with different 
#' # ranges
#' print(p1 | p2)
#' 
#' # move the y-axis to the right side
#' p2 <- gg_move_y_axis(p2)
#' 
#' # and then match the y axis ranges
#' print(pw_match_y_axis(p1 | p2))
#' 
#' @export
#' 
#' @rdname modify_axes
gg_move_y_axis <- function(gg, position = "right") {
  assert_that(methods::is(gg, "gg"))
  assert_that(position %in% c("left", "right"))
  
  gg + 
    guides(y = guide_axis(position = position))
}

#' @param pw Object inheriting from 'patchwork'
#' 
#' @export
#' @rdname modify_axes
pw_match_y_axis <- function(pw) {
  assert_that(methods::is(pw, 'patchwork'))
  
  patches <- patchwork:::get_patches(pw)
  
  pw_range <- sapply(patches$plots, function(tmp) {
    layer_scales(tmp)$y$range$range
  })
  
  pw &
    ylim(min(pw_range), max(pw_range))
}
