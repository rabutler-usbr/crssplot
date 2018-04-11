library(tidyverse)
library(feather)

source("code/Stat_emp_ExcCrv.R")

zz <- read_feather("C:/alan/CRSS/CRSS.Offc_Dev/results/Jan2018/tempData/MeadPowellPE.feather")

aggNames <- c("January 2018", "August 2017")

gg <- zz %>%
  filter(
    Year <= 2026, 
    Agg %in% aggNames, 
    Variable %in% c("Mead.Pool Elevation", "Powell.Pool Elevation")
  ) %>%
  mutate(Variable2 = factor(
    Variable, 
    levels = c("Powell.Pool Elevation", "Mead.Pool Elevation"),
    labels = c("Powell Pool Elevation", "Mead Pool Elevation")
  )) %>%
  ggplot(aes(Value, color = Agg)) +
  stat_ecdf() +
  facet_grid(.~Variable2, scales = "free_x") +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.text = element_text(size = 14),
    plot.caption = element_text(size = 9),
    strip.text = element_text(size = 14)
  ) +
  labs(x = "Elevation (feet)", y = "P(<= Elevation)") + 
  scale_y_continuous(breaks = seq(0, 1, 0.1))

gg2 <- zz %>%
  filter(
    Year <= 2026, 
    Agg %in% aggNames, 
    Variable %in% c("Mead.Pool Elevation", "Powell.Pool Elevation")
  ) %>%
  ggplot(aes(Value, color = Agg)) +
  stat_eexccrv() +
  facet_grid(Variable~., scales = "free_y")

ggsave("C:/alan/CRSS/CRSS.Offc_Dev/results/Jan2018/pe_cdf.png", plot = gg,
       device = "png", width = 9, height = 6, units = "in")
  
