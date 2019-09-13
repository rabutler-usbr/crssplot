library(assertthat)

#' Rename Excel Files to Include Scenario Name
#' 
#' `rename_excel_file()` inserts a scenario name to the beginning of an the 
#' existing file name.
#' 
#' @param ifolder Top level folder to find the excel files.
#' 
#' @param files Vector of file names to rename. 
#' 
#' @param insert_name By default (if this is `NULL`), the top level folder name
#'   will be inserted into the current file names. If this parameter is
#'   specified, it will be used instead of the top level folder.

rename_excel_file <- function(files, ifolder, insert_name = NULL)
{
  assert_that(length(ifolder) == 1)
  assert_that(dir.exists(ifolder))
  old_files <- file.path(ifolder, files)
  assert_that(all(file.exists(old_files)))
  
  if (is.null(insert_name)) {
    insert_name <- stringr::str_split(ifolder, "/")[[1]]
    
    # don't know if there was a trailing "/" in the folder, so if there was, 
    # we need to not use the last value, otherwise we want the last value
    
    if (tail(insert_name, 1) == "") {
      insert_name <- tail(insert_name, 2)[1]
    } else {
      insert_name <- tail(insert_name, 1)
    }
  }
  
  # now loop through all files, and rename them
  
  new_files <- file.path(ifolder, paste0(insert_name, "-", files))
  
  file.rename(old_files, new_files)
  
  invisible(files)
}