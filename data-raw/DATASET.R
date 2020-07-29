## code to prepare `DATASET` dataset goes here

pe_rwa <- RWDataPlyr::read_rwd_agg("data-raw/MPPEStats_sam.csv")
usethis::use_data(pe_rwa, overwrite = TRUE)

short_cond_rwa <- RWDataPlyr::read_rwd_agg("data-raw/crss_short_cond.csv")
usethis::use_data(short_cond_rwa, overwrite = TRUE)

ex_pe <- feather::read_feather(
  "tests/testthat/results/auto_tests/tempData/MeadPowellPE.feather"
) 
ex_pe <- dplyr::rename(ex_pe, ScenarioGroup = Agg)
usethis::use_data(ex_pe, overwrite = TRUE)
