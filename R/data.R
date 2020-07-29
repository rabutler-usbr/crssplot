#' RiverWare data aggregator objects used by crssplot 
#' 
#' `pe_rwa` is an [RWDataPlyr::rwd_agg] object containing the common stats that  
#' will be computed for Powell and Mead.
#' 
#' `pe_rwa` includes:
#' 
#' - Powell and Mead EOCY elevation
#' - Mead December elevation <= 1,000', 1,020', and 1,025'
#' - Mead minimum yearly elevation <=  1,000', 1,020', and 1,025'
#' - Powell December elevation <= 3,525'
#' - Powell minimum water year elevation <= 3,490' and 3,525'
#' 
#' @rdname rwd_agg_data
"pe_rwa"

#' This is ignored title2.
#' 
#' `short_cond_rwa` is an [RWDataPlyr::rwd_agg] object containing containing the 
#' annual stats that are necessary for the "conditions leading to shortage" 
#' figure.
#' 
#' `short_cond_rwa` includes:
#' 
#' - Mead EOCY elevation
#' - Powell WY release
#' - Annual total natural flow between Powell and Mead
#' - Annual total natural flow below Mead
#' - MWD annual creation and delivery (two different variables)
#' 
#' @rdname rwd_agg_data
"short_cond_rwa"

#' Example data
#' 
#' `ex_pe` is a data frame (tibble) that includes example data based on Lakes 
#' Powell and Mead pool elevation for multiple scenarios and metrics. Results
#' are from April 2020 CRSS using the stress test hydrology, and were generated
#' using [RWDataPlyr::rw_scen_aggregate()].
#' 
#' @format Data frame with 21,146 rows and 7 variables:
#' \describe{
#'   \item{Year}{year}
#'   \item{Month}{month, full name}
#'   \item{TraceNumber}{trace number, 1-31 are from CRSS, 0 are initial 
#'   conditions}
#'   \item{Scenario}{RiverSMART scenario folder, 2 are included}
#'   \item{Variable}{variable name, 11 are included}
#'   \item{Value}{variable value, in feet for `powell_dec_pe` and `mead_dec_pe`
#'   and binary occurrence for remaining variables.}
#'   \item{ScenarioGroup}{scenario group. Used to group multiple "Scenario" 
#'   folders together for processing/analysis. 2 are included}
#' }
"ex_pe"
