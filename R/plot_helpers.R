
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

