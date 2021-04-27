
#' @rdname stat_emp_exc_crv
#' @usage NULL
#' @format NULL
#' @export
StatEmpExcCrv <- ggproto("StatEExcCrv", Stat,
                    compute_group = function(data, scales, n = NULL, pad = TRUE) {
                      # If n is NULL, use raw values; otherwise interpolate
                      if (is.null(n)) {
                        x <- unique(data$x)
                      } else {
                        x <- seq(min(data$x), max(data$x), length.out = n)
                      }
                      
                      if (pad) {
                        x <- c(-Inf, x, Inf)
                      }
                      y <- ecdf(data$x)(x)
                      
                      data.frame(y = x, x = 1-y)
                    },
                    
                    default_aes = aes(y = ..y..),
                    
                    required_aes = c("x")
)

#' Plot the empirical exceedance curve
#' 
#' `stat_emp_exc_crv()` plots the empirical exceedance curve. It is similar to 
#' `ecdf()` and [ggplot2::stat_ecdf()] except that the exceedance curve is 
#' computed as `1 - ecdf()`. Additionally, the value of the data is shown on 
#' the y axis instead of the x axis.
#' 
#' @inheritParams ggplot2::stat_ecdf
#' 
#' @export
stat_emp_exc_crv <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEmpExcCrv,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}