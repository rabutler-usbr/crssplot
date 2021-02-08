#' Create CRSS results package
#' 
#' `create_results_package()` creates a "package" of CRSS results. The package
#' consists of the user specified figures and comparison(s) accross one or 
#' more scenarios. All user input provided by the yaml `file`. See 
#' [https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification](https://github.com/BoulderCodeHub/Process-CRSS-Res/wiki/yaml-specification)
#' for documentation on specifying the yaml. 
#' 
#' @param file A yaml file containing user input instructions for the CRSS 
#'   results package.
#'   
#' @export

create_results_package <- function(file) {
  assert_that(file.exists(file), msg = "Specified `file` does not exist.")
  
  ui <- parse_yaml_input(file)
  
  process_everything(ui)
}