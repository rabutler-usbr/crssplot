
suppressPackageStartupMessages(library(tidyverse))

ui <- parse_yaml_input("test_simple.yml")

process_everything(ui)

