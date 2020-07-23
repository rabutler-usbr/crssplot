
# plotting naming functions so we only have to change a name in one place

# crit stats variable names
csVarNames <- function() {
  c(
    "lbShortage" = "LB Shortage",
    "lbSurplus" = "LB Surplus",
    "mead_min_lt_1000" = "Mead < 1,000' in Any Month",
    "mead_min_lt_1020" = "Mead < 1,020' in Any Month",
    "mead_min_lt_1025" = "Mead < 1,025' in Any Month",
    "powell_wy_min_lt_3490" = "Powell < 3,490' in Any Month in the WY",
    "powell_wy_min_lt_3525" = "Powell < 3,525' in Any Month in the WY",
    "mead_dec_lt_1000" = "Mead < 1,000' in December",
    "mead_dec_lt_1020" = "Mead < 1,020' in December",
    "powell_dec_lt_3525" = "Powell < 3,525' in December",
    "mead_dec_lt_1025" = "Mead < 1,025' in December"
  )
}
