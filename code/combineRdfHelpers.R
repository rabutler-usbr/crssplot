# Functions to work with Jon Rocha's rdf combiner tool
# As of 12-May-2016, the tool cannot read folder names with commas, hence the change folder
# names functions

library(stringr)

#' Change folder names to have no commas
#' 
#' @param fNames A vector of foldernames that will be renamed.
#' @param fPath The path to the folders if not in the working directory. 
#' @param repWith What to replace the commas with. Defaults to '-'
#' 
#' @return An Nx2 matrix. First column contains the orginal folder names, and the second column
#' contains the new column names

fRename_noComma <- function(fNames, fPath = '', repWith = '-')
{
  newNames <- stringr::str_replace_all(fNames, ',', repWith)
  old <- file.path(fPath,fNames)
  nwNames <- file.path(fPath,newNames)
  file.rename(old, nwNames)
  return(matrix(cbind(old,nwNames),ncol = 2, dimnames = list(NULL, c('Old Names','New Names'))))
}

#' Create the batch text file used by the Combiner Executable
#' 
#' @param fNames The folder names (full path)
#' @param rdf The rdf file to combine together
#' @param oFile A full path to the new combined rdf file. Should end in .rdf
#' @param batchDir The folder to create the batch text file in
createCombinerBatchTxt <- function(fNames, rdf, oFile, batchDir)
{
  combineFiles <- paste0(fNames,'; ',rdf)
  combineFiles <- matrix(c(paste0('$',oFile), combineFiles), ncol = 1)
  write.table(combineFiles, file.path(batchDir,'RdfCombinerBatchControl.txt'), 
              quote = F, col.names = F, row.names = F)
}

#' Create the batch text file, and then call the Rdf Combiner
#' 
#' @param rdf The rdf file to combine together
#' @param fNames The folder names (full path)
#' @param batchDir The folder to create the batch text file in (also the director to create the rdf in)
#' @return TRUE if successful
callCombinerByRdf <- function(rdf, fNames, batchDir)
{
  oFile <- file.path(batchDir,rdf)
  message(paste('Creating batch text file for',rdf,'...'))
  createCombinerBatchTxt(fNames = fNames, rdf = rdf, oFile = oFile, batchDir = batchDir)
  # the executable has to be called from the directory the batch file is in
  owd <- getwd()
  setwd(batchDir)
  message(paste('Starting RDF Combiner for',rdf,'...'))
  system(file.path(batchDir,'RiverWareBatchRdfCombiner'))
  setwd(owd)
  return(TRUE)
}

