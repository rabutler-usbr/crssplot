dnf_st_pal <- function(full = NA, st = NA) {
  pal <- c(
    "Full Hydrology" = "#138d75",
    "Stress Test Hydrology" = "#f1c40f"
  )
  
  if (!is.na(full))
    names(pal)[1] <- full
  
  if (!is.na(st))
    names(pal)[2] <- st
  
  pal
}
