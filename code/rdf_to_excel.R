# Input -----------------------------------

fpath <- "M:/Shared/CRSS/2019/ModelOutput.Jun2019/"
rdf2excel <- "C:/Program Files/CADSWES/RiverWare 7.5/RDFToExcel/RdfToExcelExecutable.exe"

rdfs <- c(
  "Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP,1981_2015-MWDICS",
  "Jun2019_2020,ISM1988_2017,2007Dems,IG_DCP,1981_2015-MWDICS_mon",
  "Jun2019_2020,DNF,2007Dems,IG_DCP,1981_2015-MWDICS",
  "Jun2019_2020,DNF,2007Dems,IG_DCP,1981_2015-MWDICS_mon"
)
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
