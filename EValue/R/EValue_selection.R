#' Compute selection bias E-value for a hazard ratio and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion if needed when outcome is common) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if outcome is not rare (>15 percent at end of follow-up)
#' @param true The true HR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @param sel_pop Whether inference is specific to selected population (TRUE) or entire population (FALSE). Defaults to FALSE.
#' @param S_eq_U Whether the unmeasured factor is assumed to be a defining characteristic of the selected population. Defaults to FALSE.
#' @param risk_inc Whether selection is assumed to be associated with increased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @param risk_dec Whether selection is assumed to be associated with decreased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @export
#' @details A selection bias E-value is a summary measure that helps assess susceptibility of a result to selection bias. 
#' Each of one or more parameters characterizing the extent of the bias must be greater than or equal to this value to be sufficient
#' to shift an estimate (\code{est}) to the null or other true value (\code{true}). The parameters, as defined in Smith and VanderWeele 2019,
#' depend on assumptions an investigator is willing to make (see arguments \code{sel_pop}, \code{S_eq_U}, \code{risk_inc}, \code{risk_dec}).
#' The \code{svalues.XX} functions print a message about which parameters the selection bias E-value refers to given the assumptions made.
#' See the cited article for details.

svalues.HR = function( est, lo = NA, hi = NA, rare = NA, true = 1,
                       sel_pop = FALSE, S_eq_U = FALSE,
                       risk_inc = FALSE, risk_dec = FALSE ) {
  
  # organize user's values
  values = c(est, lo, hi)
  
  # sanity checks
  if ( est < 0 ) stop("HR cannot be negative")
  if ( is.na(rare) ) stop("Must specify whether outcome is rare")
  
  # if needed, convert to approximate RR
  # if not rare, no conversion needed
  if (rare){ 
    #warning("Results assume a rare outcome (<15% at end of follow-up)")
    true.RR = true
  }
  
  else if ( ! rare ) {
    #warning("Results assume a common outcome (>15% at end of follow-up)")
    values = ( 1 - 0.5^sqrt( values ) ) / ( 1 - 0.5^sqrt( 1 / values ) )
    
    # convert true value to which to shift observed point estimate
    #  to RR scale
    true.RR = ( 1 - 0.5^sqrt( true ) ) / ( 1 - 0.5^sqrt( 1 / true ) )
  }
  
  return( svalues.RR( values[1], values[2], values[3], true = true.RR,
                      sel_pop = sel_pop, S_eq_U = S_eq_U,
                      risk_inc = risk_inc, risk_dec = risk_dec) )
}


#' Compute selection bias E-value for an odds ratio and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion if needed when outcome is common) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval 
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if outcome is not rare (>15 percent at end of follow-up)
#' @param true The true OR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @param sel_pop Whether inference is specific to selected population (TRUE) or entire population (FALSE). Defaults to FALSE.
#' @param S_eq_U Whether the unmeasured factor is assumed to be a defining characteristic of the selected population. Defaults to FALSE.
#' @param risk_inc Whether selection is assumed to be associated with increased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @param risk_dec Whether selection is assumed to be associated with decreased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @export
#' @details A selection bias E-value is a summary measure that helps assess susceptibility of a result to selection bias. 
#' Each of one or more parameters characterizing the extent of the bias must be greater than or equal to this value to be sufficient
#' to shift an estimate (\code{est}) to the null or other true value (\code{true}). The parameters, as defined in Smith and VanderWeele 2019,
#' depend on assumptions an investigator is willing to make (see arguments \code{sel_pop}, \code{S_eq_U}, \code{risk_inc}, \code{risk_dec}).
#' The \code{svalues.XX} functions print a message about which parameters the selection bias E-value refers to given the assumptions made.
#' See the cited article for details.

svalues.OR = function( est, lo = NA, hi = NA, rare = NA, true = 1,
                       sel_pop = FALSE, S_eq_U = FALSE,
                       risk_inc = FALSE, risk_dec = FALSE) {
  
  # organize user's values
  values = c(est, lo, hi)
  
  # sanity checks
  if ( est < 0 ) stop("OR cannot be negative")
  if ( is.na(rare) ) stop("Must specify whether outcome is rare")
  
  # if needed, convert to approximate RR
  # if not rare, no conversion needed
  if (rare){ 
    #warning("Results assume a rare outcome (<15% at end of follow-up)")
    true.RR = true
  }
  else if ( ! rare ) {
    #warning("Results assume a common outcome (>15% at end of follow-up)")
    values = sqrt(values)
    
    # convert true value to which to shift observed point estimate
    #  to RR scale
    true.RR = sqrt(true)
  }
  
  return( svalues.RR( values[1], values[2], values[3], true = true.RR, 
                      sel_pop = sel_pop, S_eq_U = S_eq_U,
                      risk_inc = risk_inc, risk_dec = risk_dec ) )
}



#' Compute selection bias E-value for a risk ratio or rate ratio and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' for the risk ratio (as provided by the user) as well as selection bias E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param true The true RR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @param sel_pop Whether inference is specific to selected population (TRUE) or entire population (FALSE). Defaults to FALSE.
#' @param S_eq_U Whether the unmeasured factor is assumed to be a defining characteristic of the selected population. Defaults to FALSE.
#' @param risk_inc Whether selection is assumed to be associated with increased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @param risk_dec Whether selection is assumed to be associated with decreased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @export
#' @details 
#' A selection bias E-value is a summary measure that helps assess susceptibility of a result to selection bias. 
#' Each of one or more parameters characterizing the extent of the bias must be greater than or equal to this value to be sufficient
#' to shift an estimate (\code{est}) to the null or other true value (\code{true}). The parameters, as defined in Smith and VanderWeele 2019,
#' depend on assumptions an investigator is willing to make (see arguments \code{sel_pop}, \code{S_eq_U}, \code{risk_inc}, \code{risk_dec}).
#' The \code{svalues.XX} functions print a message about which parameters the selection bias E-value refers to given the assumptions made.
#' See the cited article for details.
#' @examples 
#' # Examples from Smith and VanderWeele 2019
#' 
#' # Zika virus example
#' svalues.RR(est = 73.1, lo = 13.0)
#' 
#' # Endometrial cancer example
#' svalues.RR(est = 2.30, true = 11.98, S_eq_U = TRUE, risk_inc = TRUE)
#' 
#' # Obesity paradox example
#' svalues.RR(est = 1.50, lo = 1.22, sel_pop = TRUE)

svalues.RR = function( est, lo = NA, hi = NA, true = 1,
                       sel_pop = FALSE, S_eq_U = FALSE,
                       risk_inc = FALSE, risk_dec = FALSE) {

  # organize user's values
  values = c(est, lo, hi)
  
  # sanity checks
  if ( est < 0 ) stop("RR cannot be negative")
  if ( true < 0 ) stop("True value is impossible")
  if ( risk_inc & risk_dec ) stop("You have made incompatible assumptions about the 
                                  association between selection and risk.")
  
  # warn user if making unnecessary assumptions
  if ( sel_pop & (S_eq_U | risk_inc | risk_dec) ) message("Since you are interested in the selected population only, the additional assumptions aren't necessary to calculate the selection bias E-value")
  
  # warn user if using non-null true value
  if ( true != 1 ) message('You are calculating a "non-null" selection bias E-value,
                           i.e., an E-value for the minimum amount of selection bias
                           needed to move the estimate and confidence
                           interval to your specified true value rather than to
                           the null value.')

  # check if CI crosses null
  null.CI = NA
  if ( est > true & !is.na(lo) ) {
    null.CI = ( lo < true )
  }
  
  if ( est < true & !is.na(hi) ) {
    null.CI = ( hi > true )
  }
  

  # sanity checks for CI
  if ( !is.na(lo) & !is.na(hi) ) {
    # check if lo < hi
    if ( lo > hi ) stop("Lower confidence limit should be less than upper confidence limit")
  }
  
  if ( !is.na(lo) & est < lo ) stop("Point estimate should be inside confidence interval")
  if ( !is.na(hi) & est > hi ) stop("Point estimate should be inside confidence interval")
  
  thresh_res = lapply( values, FUN = function(x) threshold_selection( x, true = true, sel_pop = sel_pop, S_eq_U = S_eq_U,
                                                   risk_inc = risk_inc, risk_dec = risk_dec ) )
  m <- thresh_res[[1]][[1]] # get first message (all will be the same)
  E <- sapply(thresh_res, FUN = function(x) ifelse(is.na(x[[1]]), NA, x[[2]]))
  
  # clean up CI reporting
  # if CI crosses null, set its E-value to 1
  if ( !is.na(null.CI) & null.CI == TRUE ){
    E[ 2:3 ] = 1
    message("Confidence interval crosses the true value, so its selection bias E-value is 1.") 
  }
  
  # if user provides either CI limit...
  if ( !is.na(lo) | !is.na(hi) ) {
    # ...then only report E-value for CI limit closer to null
    if ( est > true ) E[3] = NA
    if ( est < true ) E[2] = NA
    if ( est == true ) {
      E[2] = 1
      E[3] = NA
    }
  }
  
  result = rbind(values, E)
  
  rownames(result) = c("RR", "Selection bias E-values")
  colnames(result) = c("point", "lower", "upper")
  message(m)
  result
}



#' Compute selection bias E-value for single value of risk ratio as well as a statement about what parameters it refers to
#' 
#' Computes selection bias E-value for a single value of the risk ratio. Users should typically call the 
#' relevant \code{svalues.XX()} function rather than this internal function.  
#' @param x The risk ratio
#' @param true The true RR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @param sel_pop Whether inference is specific to selected population (TRUE) or entire population (FALSE). Defaults to FALSE.
#' @param S_eq_U Whether the unmeasured factor is assumed to be a defining characteristic of the selected population. Defaults to FALSE.
#' @param risk_inc Whether selection is assumed to be associated with increased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @param risk_dec Whether selection is assumed to be associated with decreased risk of the outcome in both exposure groups. Defaults to FALSE.
#' @export

threshold_selection = function(x, true = 1, sel_pop = FALSE, S_eq_U = FALSE,
                               risk_inc = FALSE, risk_dec = FALSE) {
  
  if ( is.na(x) ) return(NA)
  
  if(x < 0){
    warning("The risk ratio must be non-negative.")
  }  
  
  if(x <= 1){
    x = 1 / x
    true = 1 / true
  }
  
  # standard case: causal effect is toward null
  if ( true <= x ) {
    rat = x / true
  }
  
  # causal effect is away from null
  else if ( true > x ) {
    # ratio that is > 1
    rat = true / x 
  }
  
  # compute E-values
  if (sel_pop) {
    m <- "This selection bias E-value refers to RR_UY|S=1 and RR_AU|S=1 (see documentation)"
    return(list(m, rat + sqrt( rat * ( rat - 1 ) )))
  }
  
  if (!S_eq_U & !risk_inc & !risk_dec) {
    m <- "This selection bias E-value refers to RR_UY|A=0, RR_UY|A=1, RR_SU|A=0, and RR_SU|A=1 (see documentation)"
    return(list(m, sqrt(rat) + sqrt( sqrt(rat) * ( sqrt(rat) - 1 ) )))
  }
  
  if (S_eq_U & !risk_inc & !risk_dec) {
    m <- "This selection bias E-value refers to RR_UY|A=0 and RR_UY|A=1 (see documentation)"
    return(list(m, sqrt(rat)))
  }
  
  if(S_eq_U & risk_inc) {
    m <- "This selection bias E-value refers to RR_UY|A = 1 (see documentation)"
    return(list(m, rat))
  }
  
  if(S_eq_U & risk_dec) {
    m <- "This selection bias E-value refers to RR_UY|A = 0 (see documentation)"
    return(list(m, rat))
  }
  
  if (risk_inc) {
    m <- "This selection bias E-value refers to RR_UY|A=1 and RR_SU|A=1 (see documentation)"
    return(list(m, rat + sqrt( rat * ( rat - 1 ) )))
  }
  
  if (risk_dec) {
    m <- "This selection bias E-value refers to RR_UY|A=0 and RR_SU|A=0 (see documentation)"
    return(list(m, rat + sqrt( rat * ( rat - 1 ) )))
  }
  
}

