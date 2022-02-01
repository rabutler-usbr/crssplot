#' Add IG and DCP lines and annotation
#' 
#' For Lakes Powell and Mead, `add_powell_ig_tiers()` and `add_mead_ig_tiers()` 
#' add in the key elevations that distinguish the different operating tiers as 
#' specified by the IGs. Additionally, `add_dcp_start()` and `add_ig_start()` 
#' add vertical markers for the years that the IGs and DCPs started. In addition
#' to the lines, annotation text describes each line.
#' 
#' @param gg Object inheriting from `gg`.
#' 
#' @return `gg` object
#' 
#' @export
#' @rdname add_markers
add_dcp_start <- function(gg) {
  assert_that(methods::is(gg, "gg"))
  
  if (2019 %in% gg_year_range(gg)) {
    
    min_y <- floor(gg_y_range(gg)[1]/50)*50
    
    gg <- gg + 
      geom_vline(xintercept = 2019, size = 0.8, color = '#808080') +
      annotate(
        "text", 
        x = 2019.1, y = min_y, 
        label = 'Adoption of the Drought\nContingency Plan', 
        size = 2.9, hjust = 0, fontface = "bold", color = '#303030'
      )
  }
  
  gg
}

#' @export
#' @rdname add_markers
add_ig_start <- function(gg) {
  assert_that(methods::is(gg, "gg"))
  
  if (2007 %in% gg_year_range(gg)) {
    
    min_y <- floor(gg_y_range(gg)[1]/50)*50
    
    gg <- gg +
      geom_vline(
        xintercept = 2007, 
        size = 0.8, 
        color = '#808080'
      ) + 
      annotate(
        "text", x = 2007.1, y = min_y, 
        label = 'Adoption of the 2007\nInterim Guidelines', 
        size = 2.9, 
        hjust = 0,
        fontface = "bold", 
        color = '#303030'
      )
  }
  
  gg
}

#' @export
#' @rdname add_markers
add_powell_ig_tiers <- function(gg) {
  assert_that(methods::is(gg, "gg"))
  
  yrs <- gg_year_range(gg)
  EQLine <- as.data.frame(utils::read.csv(
    system.file('extdata/EQLine.csv', package = "crssplot")
  ))
  
  if (2007 %in% yrs) {
    ig_start <- 2007
    ig_label <- 2007.1
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  ann_size <- 2.9
  line_size <- 0.6
  
  max_y <- ceiling(gg_y_range(gg)[2]/50)*50
  
  gg <- gg +
    # Adding lines and annotation for Powell ops - plot only if Switch = True
    geom_line(
      data = filter(EQLine, Year %in% yrs), 
      aes(x = Year, y = EQLine), 
      size = line_size,
      color = '#808080', 
      linetype = 3
    ) + 
    annotate(
      "text", x = ig_label, y = max_y, 
      label = "Equalization Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 3575, xend = 2026, yend = 3575, 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) + 
    geom_segment(
      x = ig_start, y = 3525, xend = 2026, yend = 3525, 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 3520, 
      label = "Lower Elevation Balancing Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +    
    annotate(
      "text", x = ig_label, y = 3570, 
      label = "Mid Elevation Release Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) + 
    annotate(
      "text", x = ig_label, y = 3582, 
      label = "Upper Elevation Balancing Tier", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    )
}

#' @export
#' @rdname add_markers
add_mead_ig_tiers <- function(gg) {
  assert_that(methods::is(gg, "gg"))
  
  yrs <- gg_year_range(gg)
  
  if (2007 %in% yrs) {
    ig_start <- 2007
    ig_label <- 2007.1
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  ann_size <- 2.9
  line_size <- 0.6
  
  max_y <- ceiling(gg_y_range(gg)[2]/50)*50
  max_y <- max(max_y, 1150)
  
  gg <- gg +
    geom_segment(
      x = ig_start, y = 1075, xend = 2026, yend = 1075, 
      size = line_size, 
      color ='#808080', linetype = 3
    ) + 
    annotate(
      "text", x = ig_label, y = 1070, 
      label = "Level 1 Shortage Condition", 
      size = ann_size, hjust = 0, fontface = "italic", color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 1050, xend = 2026, yend = 1050, 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1045, 
      label = "Level 2 Shortage Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 1025, xend = 2026, yend = 1025, 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1020, 
      label = "Level 3 Shortage Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(
      x = ig_start, y = 1145, xend = 2026, yend = 1145, 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1140, 
      label = "Normal or ICS Surplus Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    annotate(
      "text", x = ig_label, y = max_y, 
      label = "Surplus Condition", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    )
  
  gg
}

gg_year_range <- function(gg) {
  tmp <- layer_scales(gg)$x$range$range
  tmp <- tmp[1]:tmp[2]
  
  tmp
}

gg_y_range <- function(gg) {
  layer_scales(gg)$y$range$range
}