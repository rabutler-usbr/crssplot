
# plotting naming functions so we only have to change a name in one place

# crit stats variable names
csVarNames <- function() {
  c(
    "lbShortage" = "LB Shortage",
    "meadLt1000" = "Mead < 1,000' in Any Month",
    "meadLt1020" = "Mead < 1,020' in Any Month",
    "meadLt1025" = "Mead < 1,025' in Any Month",
    "powellLt3490" = "Powell < 3,490' in Any Month",
    "powellLt3525" = "Powell < 3,525' in Any Month"
  )
}
