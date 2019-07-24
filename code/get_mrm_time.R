
#' `get_mrm_time_from_file()` extracts the total MRM time from a single 
#' RiverWarelog file
#' 
#' @param file_name Log file from RiverWare  
get_mrm_time_from_file <- function(file_name)
{
  zz <- readLines(file_name)
  
  # get the second to last line in the file
  zz <- tail(zz, 2)[1]
  
  # get stuff from inside parenthesis
  # from https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
  zz <- regmatches(zz, gregexpr("(?<=\\().*?(?=\\))", zz, perl = TRUE))[[1]]
  
  # now get just the number
  zz <- as.numeric(strsplit(zz, " ", fixed = TRUE)[[1]][1])
  
  zz
}

#' `find_all_log_files()` searches a directory and returns all of the RiverWare
#' log files in it. RiverWare log files are named rw__000_username.log
find_all_log_files <- function(folder)
{
  all_files <- list.files(folder)
  
  # find all files that match rw__000_name.log where 000 is incrmented and name
  # varies by user that ran RiverWare
  pattern <- "rw__\\d{1,}_.{1,}\\.log"
  
  all_files <- all_files[grep(pattern, all_files)]
  all_files <- file.path(folder, all_files)
  
  all_files
}

# Given a folder, aggregate MRM run time for all instances of RW (separate) log
# files exist for each instance of RiverWare, so must sum accross all log files
compute_total_distmrm_run_time <- function(folder)
{
  # get all the log files
  log_files <- find_all_log_files(folder)
  
  mrm_time <- lapply(as.list(log_files), function(x) {get_mrm_time_from_file(x)})
  
  sum(simplify2array(mrm_time))
}
