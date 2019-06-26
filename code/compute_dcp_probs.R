# compute the additional Mead tiers based on Mead Elevation

compute_mead_dcp_probs <- function(zz, aggs, yrs)
{
  zz %>% 
    filter(
      Agg %in% aggs, 
      Year %in% yrs, 
      Variable == "Mead.Pool Elevation"
    ) %>%
    mutate(
      dcp1 = as.numeric(Value <= 1090 & Value > 1075),
      dcp2 = as.numeric(Value <= 1075 & Value >= 1050),
      dcp3 = as.numeric(Value < 1050 & Value > 1045),
      dcp4 = as.numeric(Value <= 1045 & Value > 1040),
      dcp5 = as.numeric(Value <= 1040 & Value > 1035),
      dcp6 = as.numeric(Value <= 1035 & Value > 1030),
      dcp7 = as.numeric(Value <= 1030 & Value > 1025),
      dcp8 = as.numeric(Value <= 1025),
      dcp_recovery = as.numeric(Value > 1110)
    ) %>%
    select(-Variable, -Value, -Month) %>%
    gather(Variable, Value, -Year, -Scenario, -TraceNumber, -Agg) %>%
    group_by(Year, Agg, Variable) %>%
    summarise(Value = mean(Value)) %>%
    group_by(Agg, Variable) %>%
    arrange(Year) %>%
    mutate(Value = lag(Value))
}

compute_powell_dcp_probs <- function(zz, aggs, yrs) 
{
  zz %>% 
    filter(
      Agg %in% aggs, 
      Year %in% yrs, 
      Variable == "powellLt3490"
    ) %>%
    group_by(Year, Agg, Variable) %>%
    summarise(Value = mean(Value))
}

dcp_tier_names <- function()
{
  c(
    "dcp1" = "DCP Contribution - Level 1 (Mead <= 1,090 and > 1,075 ft)",
    "dcp2" = "DCP Contribution - Level 1 (Mead <= 1,075 and >= 1,050 ft)",
    "dcp3" = "DCP Contribution - Level 1 (Mead < 1,050 and > 1,045 ft)",
    "dcp4" = "DCP Contribution - Level 2 (Mead <= 1,045 and > 1,040 ft)",
    "dcp5" = "DCP Contribution - Level 3 (Mead <= 1,040 and > 1,035 ft)",
    "dcp6" = "DCP Contribution - Level 4 (Mead <= 1,035 and > 1,030 ft)",
    "dcp7" = "DCP Contribution - Level 5 (Mead <= 1,030 and >= 1,025 ft)",
    "dcp8" = "DCP Contribution - Level 5 (Mead < 1,025 ft)",
    "dcp_recovery" = "Recovery of DCP ICS (Mead > 1,110 ft)",
    "powellLt3490" = "Below Minimum Power Pool (Powell < 3,490 ft)"
  )
}

format_dcp_table <- function(zz)
{
  varname <- dcp_tier_names()
  zz %>%
    mutate(Value = Value * 100) %>%
    spread(Year, Value) %>%
    ungroup() %>%
    select(-Agg) %>%
    mutate(Variable = varname[Variable])
}
