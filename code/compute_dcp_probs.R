# compute the additional Mead tiers based on Mead Elevation

compute_mead_dcp_probs <- function(zz, aggs, yrs)
{
  zz %>% 
    filter(
      Agg %in% aggs, 
      Year %in% yrs, 
      Variable == "mead_dec_pe"
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
      normal_recovery = as.numeric(Value > 1110 & Value < 1145),
      dcp_recovery = as.numeric(Value > 1110),
      normal_no_recovery = as.numeric(Value <= 1110 & Value > 1090),
      surplus = as.numeric(Value >= 1145)
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
      Variable == "powell_wy_min_lt_3490"
    ) %>%
    group_by(Year, Agg, Variable) %>%
    summarise(Value = mean(Value))
}

dcp_tier_names <- function()
{
  c(
    "dcp1" = "DCP Contribution (Mead <= 1,090 and > 1,075 ft)",
    "dcp2" = "DCP Contribution (Mead <= 1,075 and >= 1,050 ft)",
    "dcp3" = "DCP Contribution (Mead < 1,050 and > 1,045 ft)",
    "dcp4" = "DCP Contribution (Mead <= 1,045 and > 1,040 ft)",
    "dcp5" = "DCP Contribution (Mead <= 1,040 and > 1,035 ft)",
    "dcp6" = "DCP Contribution (Mead <= 1,035 and > 1,030 ft)",
    "dcp7" = "DCP Contribution (Mead <= 1,030 and >= 1,025 ft)",
    "dcp8" = "DCP Contribution (Mead < 1,025 ft)",
    "normal_recovery" = "Normal + Recovery of DCP ICS (Mead <1,145 and > 1,110 ft)",
    "dcp_recovery" = "Recovery of DCP ICS (Mead > 1,110 ft)",
    "powell_wy_min_lt_3490" = "Below Minimum Power Pool (Powell < 3,490 ft)",
    "normal_no_recovery" = "Normal - (Mead <= 1,110 and > 1,090 ft)",
    "surplus" = "Surplus + Recovery of DCP ICS (Mead >= 1,145 ft)"
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
