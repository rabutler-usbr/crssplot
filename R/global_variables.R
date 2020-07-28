# global variables added so that there are no notes when running R CMD check

if(getRversion() >= "2.15.1"){
  # global variables necessary because of createSysCondTable
  from_many <- c('Year','Variable','Value', "Agg", "AggName", "Scenario",
                 "StartMonth", "Month", "TraceNumber")
  all_others <- c('DecElev', 'EOCYPE', 'HydrologyYear', 'LBPrct', 'Max', 
                  'Mead.Pool Elevation', 'Med', 'Min', 'Month', 'OND.Release', 
                  'Percentile', 'Powell.Pool Elevation', 'PrctTraces', 
                  'ScenName', 'Shortage', 'TheColor', 'Trace', 'VName', 
                  'WYRelease', 'dcp2', 'dcp3', 'dcp4', 'dcp5', 'dcp6', 'dcp7', 
                  'eq', 'eq_823', 'eq_823_fill', 'eq_fill', 'fc_surplus',
                  'fc_surplus_fill', 'fill_loc', 'fill_var', 'ic', 'icList', 
                  'lName', 'lbGains', 'leb823', 'lebGt823', 'lebLt823', 
                  'leb_823', 'leb_823_fill', 'leb_gt', 'leb_gt_fill', 'leb_lt', 
                  'leb_lt_fill', 'lt1075', 'lt1076', 'lt1077', 'median', 
                  'mer748', 'mer823', 'mer_748', 'mer_748_fill', 'mer_823', 
                  'mer_823_fill', 'mwdIcs', 'mwdPut', 'mwdTake', 'nfAbvMead', 
                  'nfBelowMead', 'normal_no_recovery', 'normal_recovery', 
                  'powellOut', 'short1', 'short1_fill', 'short2', 'short2_fill', 
                  'short3', 'short3_fill', 'shortOrderFull', 'shortOrderLimit', 
                  'surplus', 'surplus_fill', 'ueb823', 'uebGt823', 'uebLt823', 
                  'ueb_823', 'ueb_823_fill', 'ueb_gt', 'ueb_gt_fill', 'ueb_lt',
                  'ueb_lt_fill', 'vDescAll', 'vName', 'vNames', 'vShort', 
                  'vShortAll', 'val_lab', "month")
  
  utils::globalVariables(c(from_many, ".", all_others))
}
