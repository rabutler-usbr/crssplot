#' @keywords internal
"_PACKAGE"

#' @importFrom assertthat assert_that
#' @import ggplot2
#' @import dplyr
#' @importFrom methods is

NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "ScenarioGroup", "bottom", "top", "middle", "pe_bin"
))