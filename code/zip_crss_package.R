#' Create a zip package for distribution of CRSS. 
#' 
#' By default, it includes all files in the top level folder, and the following
#' folders: control, dmi, model, results, ruleset, sct, temp. The dmi folder is 
#' large, and all files are usually not necessary, so will prompt for user input
#' to determine which folders to include in the dmi folder

#' @param ifolder Folder to zip
#' @param zip_name Name to save the file as. Should end in .zip. Will save this
#'   file in the same directory as ifolder, e.g., if ifolder is 
#'   "C:/CRSS/CRSS.Jan2010", then the zip will be saved in "C:/CRSS"
#' @param zip_folders The folders to include in the zip. 
zip_crss_package <- function(ifolder, zip_name, 
                             zip_folders = c("control", "dmi", "model", 
                                             "results", "ruleset", "sct", "temp"
                                             ))
{
  wd <- getwd()
  on.exit(setwd(wd))
  
  assert_that(tools::file_ext(zip_name) == "zip")
  
  setwd(dirname(ifolder))
  bn <- basename(ifolder)
  # get all files and folders
  
  # get the top level files
  all_files <- file.path(bn, setdiff(
    list.files(ifolder), 
    list.dirs(ifolder, recursive = FALSE, full.names = FALSE)
  ))
  
  message("Getting files...")
  for (ff in zip_folders) {
    if (ff == "dmi") {
      # handle dmi differently
      # get a list of all files and top level folders
      dmi_folders <- list.files(file.path(bn, ff))
      n1 <- length(dmi_folders) + 1
      ops <- paste(seq_along(dmi_folders), dmi_folders, sep = " - ")
      ops <- paste(c("Choose which folders to zip:",
                     "0 - None", 
                     ops, 
                     paste0(n1, " - All of the above"),
                     "\nEnter as comma seperated, e.g., 2, 4\n"), 
                   collapse = "\n")
      message(ops)
      ui <- readline(" ")
      ui_vals <- parse_ui(ui)
      all_files <- c(all_files, get_dmi_folders(ui_vals, dmi_folders, bn))
      
    } else {
      all_files <- c(
        all_files, 
        file.path(
          bn, ff, 
          list.files(
            file.path(ifolder, ff), include.dirs = TRUE, recursive = TRUE
          )
        )
      )
    }
  }

  message("Zipping...")
  zip(zip_name, all_files)
}

# gets the numbers from comma seperated list. 
parse_ui <- function(ui)
{
  as.numeric(stringr::str_split(ui, ",")[[1]])
}

get_dmi_folders <- function(ui_vals, dmi_folders, base_folder)
{
  if (length(ui_vals) == 0 || (length(ui_vals) == 1 && ui_vals == 0)) {
    rv <- c()
  } else if (length(ui_vals) == length(dmi_folders) + 1) {
    # all folders and files
    rv <- list.files(
      file.path(base_folder, "dmi"), 
      recursive = TRUE, 
      include.dirs = TRUE
    )
  } else {
    # only those specified
    rv <- c()
    for (i in ui_vals) {
      cur <- dmi_folders[i]
      if (dir.exists(cur)) {
        # it is a folder, so get all files in the folder
        cur_dir <- file.path(base_folder, "dmi", cur)
        rv <- c(
          rv, 
          file.path(
            cur_dir,
            list.files(cur_dir, recursive = TRUE, include.dirs = TRUE)
          )
        )
      } else 
        rv <- c(rv, file.path(base_folder, "dmi", cur))
    }
    
  }
  
  rv
}
