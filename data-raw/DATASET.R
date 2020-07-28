## code to prepare `DATASET` dataset goes here

pe_rwa <- RWDataPlyr::read_rwd_agg("data-raw/MPPEStats_sam.csv")
usethis::use_data(pe_rwa, overwrite = TRUE)

short_cond_rwa <- RWDataPlyr::read_rwd_agg("data-raw/crss_short_cond.csv")
usethis::use_data(short_cond_rwa, overwrite = TRUE)

