library(RWcheck)

scenarios <- "Apr2020_2021,ISM1988_2018,2007Dems,IG_DCP,MTOM_most"
yaml_rule_files <- "crss_checks.yaml"
scenario_dir <- "~/crss/crss.offc/Scenario"
output_dir <- "~/crss/crss.offc/results/apr2020/rwcheck"
yaml_dir <- "data/"

check_rw_output(
  scenarios,
  yaml_rule_files,
  scenario_dir,
  output_dir,
  yaml_dir
)
