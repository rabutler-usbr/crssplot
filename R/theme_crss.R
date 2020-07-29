#' @export
theme_crss <- function() {
  theme_gray() %+replace%
    theme(
      panel.grid.minor = element_line(color = 'white', size = .4),
      panel.grid.major = element_line(color = 'white', size = .6),
      legend.key.height = unit(2,'line'), 
      legend.key.width = grid::unit(2, 'line')
    )
}
