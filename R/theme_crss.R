#' @export
theme_crss <- function() {
  theme_gray() %+replace%
    theme(
      panel.grid.minor = element_line(color = "white", size = .4),
      panel.grid.major = element_line(color = "white", size = .6),
      legend.key.height = unit(2, "line"),
      legend.key.width = grid::unit(2, "line")
    )
}

#' @export
theme_cloud <- function() {
  
  LegendText <- 8
  LegendLabText <- 9.5
  TitleSize <- 13
  AxisLab <- 9
  AxisText <- 11
  GridMaj <- .25
  GridMin <- .25

  theme_light() %+replace%
    theme(
      legend.text = element_text(size = LegendText),
      legend.title = element_text(
        size = LegendLabText, 
        face = "bold", 
        hjust = 0
      ),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key = element_blank(),
      legend.key.size = unit(1.75, "lines"),
      plot.title = element_text(size = TitleSize),
      axis.text.y = element_text(size = AxisLab),
      axis.title = element_text(size = AxisText, face = "plain", color = "grey30"),
      panel.grid.minor = element_line(size = GridMin),
      panel.grid.major = element_line(size = GridMaj)
    )
}
