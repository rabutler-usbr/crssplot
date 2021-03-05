# -----------------------------------------------------------------------------
#   							myBoxStatsMinMax
# -----------------------------------------------------------------------------
# used for custome statistics in boxplot using ggplot2
myBoxStatsMinMax <- function(x) 
{
  r <- stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# -----------------------------------------------------------------------------
# 								myBoxStats1090
# -----------------------------------------------------------------------------
# used for custome statistics in boxplot using ggplot2
myBoxStats1090 <- function(x) 
{
  r <- stats::quantile(x, probs = c(.1, 0.25, 0.5, 0.75, .9))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


determine_plot_colors <- function(plot_colors, col_vars, 
                                  msg_string = "Scenario")
{
  if (is.null(plot_colors)) {
    plot_colors <- scales::hue_pal()(length(col_vars))
    names(plot_colors) <- col_vars
  } else {
    # check that there are specified colors for each Variable
    assert_that(
      all(col_vars %in% names(plot_colors)), 
      msg = paste(msg_string, "names not found in `plot_colors`")
    )
  }
  
  plot_colors
}

