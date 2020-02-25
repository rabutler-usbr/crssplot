# Input -----------------------------------

fpath <- "M:/Shared/CRSS/2020/Scenario/Feb2020_2021,DNF,2007Dems,IG_DCP"
rdf2excel <- "C:/Program Files/CADSWES/RiverWare 7.5.2/RDFToExcel/RdfToExcelExecutable.exe"

rdfs <- c('KeySlots','Flags','SystemConditions','OWDAnn','Res','xtraRes', 
          "CRSPPowerData", "LBEnergy", "LBDCP", "UBDO")

xlsx <- rdfs

rdfs <- paste0(rdfs, ".rdf")
xlsx <- paste0(xlsx, ".xlsx")


# Run RdfToExcelExecutable -----------------------------------

stopifnot(file.exists(rdf2excel))

for (i in seq_along(rdfs)) {
  ifile <- file.path(fpath, rdfs[i])
  message("Starting: ", rdfs[i])
  
  if (!file.exists(ifile)) {
    message(ifile, "\nDoes not exist.")
  } else {
    ofile <- file.path(fpath, xlsx[i])
    
    cmd <- c("-i", ifile, "-o", ofile)
    
    system2(rdf2excel, args = cmd)
  }
}

# TODO: parse the log file. Delete it if it ends in 
# "RdfToExcel: Workbook write successfully completed", otherwise keep it.
