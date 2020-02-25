# Release Process

## Create files to post

### For runs with multiple initializations

1. combine the rdf files together. See combineRdfs.R. You will have to update the user input variables and call that script. 

2. Create excel files from the rdfs. See rdf_to_excel.R. You will have to update the user input variables and call that script.

3. Rename the excel files so that the scenario name appears before the file name. See `rename_excel_file()` in rename_excel_file.R. 

### For runs with only one set of initial conditions

1. Create excel files using RiverSMART

2. Rename the excel files so that the scenario name appears before the file name. See `rename_excel_file()` in rename_excel_file.R. 

## CRSS Package

1. Create the CRSS zip package - see `zip_crss_package()` in zip_crss_package.R. Note that it is likely easiest to create the zip package with some of the defaults, and then delete the files/folders that are not needed.

2. Add in the modeling assumptions to the zip package. 
