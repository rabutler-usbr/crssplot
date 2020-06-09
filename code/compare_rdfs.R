library(assertthat)
library(RWDataPlyr)

compare_rdfs <- function(
  rdfs = c("KeySlots.rdf", "UBRes.rdf", "Res.rdf", "xtraRes.rdf"), base_scen, 
  new_scens
)
{
  assert_that(dir.exists(base_scen))
  
  for (ss in new_scens) {
    message("Scenario: ", ss, " to ", base_scen)
    assert_that(dir.exists(ss))
    
    for (rr in rdfs) {
      message("------ ", rr, " --------\n")
      f1 <- file.path(base_scen, rr)
      f2 <- file.path(ss, rr)
      
      r1 <- read_rdf(f1)
      r2 <- read_rdf(f2)
      
      print(all.equal(r1, r2))
    }
  }
}
