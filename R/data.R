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
