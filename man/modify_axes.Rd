% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_axes.R
\name{gg_move_y_axis}
\alias{gg_move_y_axis}
\alias{pw_match_y_axis}
\title{Move axis positions and match axis scales}
\usage{
gg_move_y_axis(gg, position = "right")

pw_match_y_axis(pw)
}
\arguments{
\item{gg}{Object inheriting from \code{gg}.}

\item{position}{"right" or "left"}

\item{pw}{Object inheriting from 'patchwork'}
}
\description{
\code{gg_move_y_axis()} moves the current y axis to the specified \code{position}.
\code{pw_match_y_axis()} ensures that the y axes in a patchwork object all use
the same range.
}
\examples{
library(ggplot2)
library(dplyr)
library(patchwork)

# two plots with different y-axis ranges
p1 <- ex_pe \%>\%
  filter(Variable == "mead_dec_pe", ScenarioGroup == "April ST CT", 
        TraceNumber == 1) \%>\%
  ggplot(aes(Year, Value)) +
  geom_line()
  
p2 <- ex_pe \%>\%
  filter(Variable == "mead_dec_pe", ScenarioGroup == "April ST CT", 
        TraceNumber == 15) \%>\%
  ggplot(aes(Year, Value)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line()
  
# default patchwork has two y-axes both on the left, and with different 
# ranges
print(p1 | p2)

# move the y-axis to the right side
p2 <- gg_move_y_axis(p2)

# and then match the y axis ranges
print(pw_match_y_axis(p1 | p2))

}
