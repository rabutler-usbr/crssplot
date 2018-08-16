
library(tidyverse)
library(feather)

iFolder <- "C:/alan/CRSS/CRSS.2018/results/Apr2018/tempData/include_jan/"
peYrs <- 2018:2060
csYrs <- 2019:2060
legendWrap <- 20

scens <- c(
  "Apr2018_DNF_Official" = "April 2018 Official",
  "Apr2018_DNF_MtomMost" = "April 2018 - DNF + Most Probable",
  "Apr2018_ST_MtomMost" = "April 2018 - ISM 1988-2015",
  "Apr2018_CMIP3_MtomMost" = "April 2018 - CMIP3",
  "Jan2018_DNF_Official" = "January 2018 Official"
)

pe <- read_feather(file.path(iFolder, "MeadPowellPE.feather")) %>%
  mutate(Agg = scens[Agg]) %>%
  mutate(StartMonth = Agg)

s1 <- scens[c(1, 2)]
s2 <- scens[c(2:4)]

# Powell PE ----------------

p1 <- plotEOCYElev(
  filter(pe, StartMonth %in% s1), 
  yrs = peYrs, 
  var = "Powell.Pool Elevation", 
  myTitle = "Powell End-of-December Elevation", 
  legendTitle = "Scenario", 
  legendWrap = legendWrap
)
  
p2 <- plotEOCYElev(
  filter(pe, StartMonth %in% s2), 
  yrs = peYrs, 
  var = "Powell.Pool Elevation", 
  myTitle = "Powell End-of-December Elevation", 
  legendTitle = "Scenario", 
  legendWrap = legendWrap
)

# Mead PE ---------------------

m1 <- plotEOCYElev(
  filter(pe, StartMonth %in% s1), 
  yrs = peYrs, 
  var = "Mead.Pool Elevation", 
  myTitle = "Mead End-of-December Elevation", 
  legendTitle = "Scenario", 
  legendWrap = legendWrap
)

m2 <- plotEOCYElev(
  filter(pe, StartMonth %in% s2), 
  yrs = peYrs, 
  var = "Mead.Pool Elevation", 
  myTitle = "Mead End-of-December Elevation", 
  legendTitle = "Scenario", 
  legendWrap = legendWrap
)

# CRIT STATS --------------------------

cs <- pe %>%
  filter(
    Variable %in% c('meadLt1000', 'meadLt1020', 'powellLt3490', 
                    'powellLt3525', 'meadLt1025')
  ) %>%
  mutate(AggName = Agg) %>%
  select(-StartMonth)


cs <- read_feather(file.path(iFolder, "SysCond.feather")) %>%
  mutate(Agg = scens[Agg]) %>%
  mutate(AggName = Agg) %>%
  filter(Variable %in% c('lbSurplus', 'lbShortage')) %>%
  bind_rows(cs)

ptitle <- paste(
  'Powell: Percent of Traces Less than',
  "Power Pool (elevation 3,490')",
  sep = "\n"
)

# Powell < 3490 ----------------------

p3490Fig1 <- compareCritStats(
  filter(cs, AggName %in% s1), 
  csYrs, 
  'powellLt3490', 
  '', 
  ptitle, 
  "Scenario", 
  legendWrap = legendWrap
)

p3490Fig2 <- compareCritStats(
  filter(cs, AggName %in% s2), 
  csYrs, 
  'powellLt3490', 
  '', 
  ptitle, 
  "Scenario", 
  legendWrap = legendWrap
)

# Mead Shortage ------------------------
shortTitle <- 'Lower Basin: Percent of Traces in\nShortage Conditions'
short1 <- compareCritStats(
  filter(cs, AggName %in% s1), 
  csYrs, 
  'lbShortage', 
  '', 
  shortTitle, 
  "Scenario", 
  legendWrap = legendWrap
)

short2 <- compareCritStats(
  filter(cs, AggName %in% s2), 
  csYrs, 
  'lbShortage', 
  '', 
  shortTitle, 
  "Scenario", 
  legendWrap = legendWrap
)

# SAVE --------------------------------
oFolder <- "C:/alan/CRSS/CRSS.2018/results/Apr2018/supply comp"
pdf(file.path(oFolder, "SupplyScenComparison.pdf"), width = 9, height = 6)

print(p1)
print(m1)
print(p3490Fig1)
print(short1)
print(p2)
print(m2)
print(p3490Fig2)
print(short2)

dev.off()

p_width <- 5.75
p_height <- 4.5
  
ggsave(
  file.path(oFolder, "powell1.png"), 
  plot = p1, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "mead1.png"), 
  plot = m1, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "p3490_1.png"), 
  plot = p3490Fig1, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "short1.png"), 
  plot = short1, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "powell2.png"), 
  plot = p2, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "mead2.png"), 
  plot = m2, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "p3490_2.png"), 
  plot = p3490Fig2, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)

ggsave(
  file.path(oFolder, "short2.png"), 
  plot = short2, 
  device = "png", 
  width = p_width, 
  height = p_height, 
  units = "in"
)
