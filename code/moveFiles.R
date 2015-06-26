# moves files to Dirty Devil for Stakeholder Modeling Workgroup
# oFolder will be created if it does not exist.
source('code/makeScenNames.R')

## USER INPUT
scens.limit <- makeAllScenNames('Jun2015_2016','DNF','2007Dems','IG',1981:2010)
iFolder <- 'M:/Shared/CRSS/CRSS.2015/Scenario/' 
oFolder <- 'M:/Shared/Public_web/DataTransfer/CRSTMWG/CRSS.Jun2015/ModelResults/'
ff <- paste0(c('Flags','KeySlots','OWDAnn','Res','xtraRes','SystemConditions'),'.xlsx')
## END USER INPUT

copyFiles <- function(fromF, toF, scens, ff)
{
  for(i in 1:length(scens)){
    print(paste('Starting:',scens[i]))
    flush.console()
    dir.create(paste(toF,scens[i],sep = ''))
    for(j in 1:length(ff)){
      file.copy(paste(fromF,scens[i],'/',ff[j],sep = ''), paste(toF,scens[i],'/',ff[j],sep = ''))
    }
  }
}

dir.create(oFolder,recursive = TRUE)
copyFiles(iFolder,oFolder,scens.limit,ff)