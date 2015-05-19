# makeScenario names

makeAllScenNames <- function(dim1,dim2, dim3, ...)
{
  scens = expand.grid(dim1, dim2, dim3, ...)

  ss <- scens[,1]
  for(i in 2:ncol(scens)){
    ss <- paste(ss,scens[,i],sep = ',')
  }
  
  return(ss)
}