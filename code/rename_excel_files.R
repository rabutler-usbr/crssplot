library(assertthat)

#' Rename Excel Files to Include Scenario Name
#' 
#' `rename_excel_file()` inserts a scenario name to the beginning of an the 
#' existing file name.
#' 
#' @param ifolder Top level folder(s) to find the excel files.
#' 
#' @param files Vector of file(s) to rename. 
#' 
#' @param insert_name By default (if this is `NULL`), the top level folder name
#'   will be inserted into the current file names. If this parameter is
#'   specified, it will be used instead of the top level folder. If it is not 
#'   `NULL`, it must be the same length as the number of files specified by 
#'   `ifolder`.

rename_excel_file <- function(files, ifolder, insert_name = NULL)
{
  #assert_that(length(ifolder) == 1)
  assert_that(all(dir.exists(ifolder)))
  file_folder_combo <- expand.grid(ifolder, files)
  all_files <- paste(file_folder_combo[,1], file_folder_combo[,2], sep = "/")
  assert_that(all(file.exists(all_files)))
  
  for (i in seq_along(ifolder)) {
    
    ff <- ifolder[i]
    old_files <- file.path(ff, files)
    
    if (is.null(insert_name)) {
      tmp_name <- stringr::str_split(ff, "/")[[1]]
      
      # don't know if there was a trailing "/" in the folder, so if there was, 
      # we need to not use the last value, otherwise we want the last value
      
      if (tail(tmp_name, 1) == "") {
        tmp_name <- tail(tmp_name, 2)[1]
      } else {
        tmp_name <- tail(tmp_name, 1)
      }
    } else {
      tmp_name <- insert_name[i]
    }
    
    # now loop through all files, and rename them
    
    new_files <- file.path(ff, paste0(tmp_name, "-", files))
    
    file.rename(old_files, new_files)
  }
  
  invisible(all_files)
}
