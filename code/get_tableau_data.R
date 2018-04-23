library(RWDataPlyr)
library(tidyverse)
library(feather)
source("code/getICPEData.R")

scens <- rw_scen_gen_names("Apr2018_2019","DNF", "2007Dems", "IG", 1981:2015)
names(scens) <- 1981:2015

ipath <- "M:/Shared/CRSS/2018/Scenario/"

key_rwa <- rwd_agg(read.csv("data/key4vars_rwa.csv", stringsAsFactors = FALSE))

zz <- rw_scen_aggregate(
  scens, 
  agg = key_rwa, 
  scen_dir = ipath, 
  file = "c:/alan/CRSS/CRSS.2018/results/Apr2018/key4vars.feather"
)

# append IC data

resFile <- "c:/alan/CRSS/CRSS.2018/results/Apr2018/key4vars.feather"
icList <- list("April 2018" = "C:/alan/CRSS/CRSS.Offc_Dev/dmi/InitialConditions/april_2018/MtomToCrss_Monthly.xlsx")
traceMap <- read.csv('data/Trace2IcMap.csv')

getAndAppendIC(
  list("April 2018" = scens), 
  fileToAppend = resFile, 
  oFile = "c:/alan/CRSS/CRSS.2018/results/Apr2018/key4vars_withInitPe.feather", 
  icList = icList, 
  icMonth = c("April 2018" = "18-Dec"),
  addAggAttribute = FALSE, 
  aggFunction = NA, 
  traceMap = traceMap, 
  icDimNumber = 5
)

tmp <- read_feather("C:/alan/CRSS/CRSS.2018/results/Apr2018/key4vars_withInitPe.feather")
ss <- expand.grid(
  TraceNumber = 1:110, 
  Scenario = as.character(1981:2015), 
  stringsAsFactors = FALSE
)

# gets the initial reservoir elevation
zz2 <- tmp %>%
  filter(Variable %in% c("Mead.Pool Elevation", "Powell.Pool Elevation")) %>%
  mutate(
    Variable = case_when(
      Variable == "Mead.Pool Elevation" ~ "meadPe",
      Variable == "Powell.Pool Elevation" ~ "powellPe",
      TRUE ~ "uhoh"
    ),
    Scenario = stringr::str_split_fixed(Scenario, ",", 5)[,5]  
  ) %>%
  select(-TraceNumber) %>%
  inner_join(ss, by = "Scenario")

ism <- c(1906:2015, 1906:2015)
ism <- data.frame(
  "TraceNumber" = 1:110, 
  "trace_desc" = paste0(ism[1:110], "-", ism[(1:110)+7])
)

zz3 <- bind_rows(zz, zz2) %>%
  arrange(Year) %>%
  mutate(
    TraceDescription = paste0("Trace ", TraceNumber, ": ", 
      ism$trace_desc[match(TraceNumber, ism$TraceNumber)])
  ) %>%
  spread(Variable, Value) %>%
  mutate(
    meadLt1025 = as.numeric(meadPe <= 1025),
    meadLt1075 = as.numeric(meadPe <= 1075)
  )

write.csv(
  zz3, 
  "C:/alan/CRSS/CRSS.2018/results/Apr2018/apr2018_keyvars.csv", 
  row.names = FALSE
)
