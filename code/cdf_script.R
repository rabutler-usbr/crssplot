library(tidyverse)
library(feather)

source("code/Stat_emp_ExcCrv.R")

zz <- read_feather("C:/alan/CRSS/crss-janpower/results/crssJanTest/tempData/MeadPowellPE.feather")

gg <- zz %>%
  filter(
    Year <= 2026, 
    Agg %in% c("Jan Offc", "Aug Offc"), 
    Variable %in% c("Mead.Pool Elevation", "Powell.Pool Elevation")
  ) %>%
  ggplot(aes(Value, color = Agg)) +
  stat_ecdf() +
  facet_grid(.~Variable, scales = "free_x")

gg2 <- zz %>%
  filter(
    Year <= 2026, 
    Agg %in% c("Jan Offc", "Aug Offc"), 
    Variable %in% c("Mead.Pool Elevation", "Powell.Pool Elevation")
  ) %>%
  ggplot(aes(Value, color = Agg)) +
  stat_eexccrv() +
  facet_grid(Variable~., scales = "free_y")

pdf("C:/alan/CRSS/crss-janpower/results/crssJanTest/pe_cdf.pdf", width = 11, height = 8.5)
print(gg)
print(gg2)
dev.off()
  
