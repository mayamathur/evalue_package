
#' Variance of a proportion
#'
#' An internal function that quickly calculates the variance of a proportion.
#' @noRd
var_prop = function(p, n) ( p * (1 - p) ) / n

#' Variance of bias-corrected risk difference
#'
#' An internal function that estimates the variance of a bias-corrected risk difference when the bias factor (on the ratio scale) is \code{.maxB}. Users should call \code{evalues.int.contr} instead. Assumes we are considering bias away from the null. Does not make assumptions about sign of risk difference.
#' @noRd
RDt_var = function(f, p1, p0, n1, n0, .maxB) {
  
  # if risk difference < 0, reverse strata's coding in order to
  #  apply same bound
  if ( p1 - p0 < 0 ){
    .p0 = p1
    .p1 = p0
    .n0 = n1
    .n1 = n0
  } else {
    .p0 = p0
    .p1 = p1
    .n0 = n0
    .n1 = n1
  }
  
  f.var = var_prop(p = f, n = .n1 + .n0)
  p1.var = var_prop(p = .p1, n = .n1)
  p0.var = var_prop(p = .p0, n = .n0)
  
  term1 = p1.var + p0.var * .maxB^2
  term2 = ( f + (1 - f) / .maxB )^2
  term3 = ( .p1 - .p0 * .maxB )^2 * ( 1 - 1/.maxB )^2 * f.var
  
  return( term1 * term2 + term3 )
}


#' Bounds on risk differences in each stratum and on the interaction contrast
#'
#' An internal function that calculates bounds on each stratum and on the interaction contrast given specified bias factors and bias directions in each stratum. The bounds on the risk differences are the same as those produced by \code{evalues.RD()}. This function is essentially only used for calcating E-values for the interaction contrast and is not exported to avoid confusion with \code{evalues.RD()}. Does not make assumptions on sign of the risk differences or interaction contrast because it just shifts each stratum in the specified direction, by the specified max bias factor.
#' @noRd
RDt_bound = function( p1_1,
                      p1_0,
                      n1_1,
                      n1_0,
                      f1,
                      maxB_1,  
                      biasDir_1, # direction of bias in stratum 1
                      
                      p0_1,
                      p0_0,
                      n0_1,
                      n0_0,
                      f0,
                      maxB_0 = NA,
                      biasDir_0,
                      
                      alpha = 0.05 ) {
  
  if ( is.na(maxB_0) ) {
    maxB_0 = maxB_1
    message("Assuming the bias factor is the same in each stratum because you didn't provide maxB_0")
  }
  
  ### Corrected point estimate
  # corrected RD for each stratum - pg 376
  # maxes and mins to avoid RDt > 1 or RDt < -1
  if ( biasDir_1 == "positive" ) RDt_1 = max( -1, ( p1_1 - p1_0 * maxB_1 ) * ( f1 + ( 1 - f1 ) / maxB_1 ) )
  if ( biasDir_1 == "negative" ) RDt_1 = min( 1, ( p1_1 * maxB_1 - p1_0 ) * ( f1 + ( 1 - f1 ) / maxB_1 ) )
  
  if ( biasDir_0 == "positive" ) RDt_0 = max( -1, ( p0_1 - p0_0 * maxB_0 ) * ( f0 + ( 1 - f0 ) / maxB_0 ) )
  if ( biasDir_0 == "negative" ) RDt_0 = min( 1, ( p0_1 * maxB_0 - p0_0 ) * ( f0 + ( 1 - f0 ) / maxB_0 ) )
  
  # calculate interaction contrast
  ICt = RDt_1 - RDt_0
  
  
  ### Corrected confidence interval
  # calculate var for each stratum (1 and 0) separately
  # as in Ding & VanderWeele, eAppendix 5 (delta method)
  VarRDt_1 = RDt_var(f = f1,
                     p1 = p1_1,
                     p0 = p1_0,
                     n1 = n1_1,
                     n0 = n1_0,
                     .maxB = maxB_1 )
  
  VarRDt_0 = RDt_var(f = f0,
                     p1 = p0_1,
                     p0 = p0_0,
                     n1 = n0_1,
                     n0 = n0_0,
                     .maxB = maxB_0 )
  
  VarICt = VarRDt_1 + VarRDt_0
  
  
  ### Organize and return results
  res = data.frame( stratum = c("1", "0", "effectMod"),
                    
                    RD = c( RDt_1, RDt_0, ICt),
                    
                    se = c( sqrt(VarRDt_1), sqrt(VarRDt_0), sqrt(VarICt) ) )
  
  crit = qnorm( 1 - alpha/2 )
  res$lo = res$RD - crit * res$se
  res$hi = res$RD + crit * res$se
  res$pval = 2 * ( 1 - pnorm( abs( res$RD / res$se ) ) )
  
  return(res)
}





#' Distance of bias-corrected risk difference for a stratum (or interaction contrast) versus desired true value
#'
#' An internal function used for calculating E-values for interaction contrasts. Does not make assumptions about signs of risk differences or interaction contrast, nor whether the confounded estimate is below or above the true value.
#' @noRd

# stratum: name of stratum as returned by RDt_bound ("1", "0", or "effectMod" for interaction contrast)
# varName: the statistic (of those returned by RDt_bound: "RD", "lo", "hi") whose distance from "true" should be measured
# true: value to which to compare the bias-corrected estimate
# ...: args to be passed to RDt_bound
RD_distance = function(stratum,
                       varName,  
                       true,  
                       ...) {
  
  .RDs = RDt_bound(...)
  
  return( list( dist = abs( .RDs[[varName]][ .RDs$stratum == stratum ] - true ),
                RDt = .RDs ) )
}



#' Inner helper function for calculating E-values for interaction contrasts
#'
#' An internal function called by the wrapper function \code{evalues.IC}. The present function does NOT consider monontonic bias in arbitrary direction; for that, the wrapper function will call \code{IC_evalue_inner} once for EACH candidate bias direction and then will compare the results. This function also gives E-values for each stratum separately if wanted (based on \code{varName}), which are the same as those from \code{evalues.RD}. 
#' @noRd

# see argument structure in evalues.IC, with the following differences
# monotonicBias: TRUE/FALSE 
# monotonicBiasDirection: "positive" or "negative" only
IC_evalue_inner = function( stratum,
                            varName,
                            true = 0,
                            monotonicBias = FALSE,
                            monotonicBiasDirection = NA,
                            
                            p1_1,
                            p1_0,
                            n1_1,
                            n1_0,
                            f1,
                            
                            p0_1,
                            p0_0,
                            n0_1,
                            n0_0,
                            f0,
                            
                            alpha = 0.05 ) {
  
  ##### Catch bad input #####
  
  if ( !varName %in% c("RD", "lo") ) stop("Invalid varName")
  
  
  # catch RDc < 0
  # this would break the monotonicBias == FALSE case (see comment below)
  # first calculate RDc (confounded estimate) and its CI limit
  RDc = RDt_bound(  p1_1 = p1_1,
                    p1_0 = p1_0,
                    n1_1 = n1_1,
                    n1_0 = n1_0,
                    f1 = f1,
                    
                    p0_1 = p0_1,
                    p0_0 = p0_0,
                    n0_1 = n0_1,
                    n0_0 = n0_0,
                    f0 = f0,
                    
                    # no bias
                    maxB_1 = 1,
                    maxB_0 = 1,
                    biasDir_1 = "positive",
                    biasDir_0 = "positive",
                    
                    alpha = alpha )
  
  if ( RDc$RD[ RDc$stratum == "effectMod" ] < 0 ) {
    stop( "The confounded interaction contrast (stratum 1 - stratum 0) is negative. Please recode the stratum variable so that the interaction contrast is positive." )
  }
  
  
  # catch RDc < true already
  # this would also break the monotonicBias == FALSE case (see comment below)
  if ( stratum == "effectMod" &
       varName == "RD" &
       RDc$RD[ RDc$stratum == "effectMod" ] <= true ) {
    wrapmessage( "The confounded interaction contrast (stratum 1 - stratum 0) is already less than the true value you specified, so the E-value is 1." )
    
    return( data.frame( evalue = 1,
                        biasFactor = 1,
                        bound = NA ) )
  }
  
  # catch RDc < true alrseady
  # this would also break the monotonicBias == FALSE case (see comment below)
  if ( stratum == "effectMod" &
       varName == "lo" &
       RDc$lo[ RDc$stratum == "effectMod" ] <= true ) {
    wrapmessage( "The confounded interaction contrast (stratum 1 - stratum 0) is already less than the true value you specified, so the E-value is 1." )
    
    return( data.frame( evalue = 1,
                        biasFactor = 1,
                        bound = NA ) )
  }
  
  
  # need to calculate confounded lower bd to check that one
  
  # note: we are not catching the case where stratum = "1" or "0" but RDc < true
  #  because the wrapper fn will only ever pass stratum = "effectMod" to the present fn
  
  
  ##### Prepare to pass all arguments to another fn #####
  # https://stackoverflow.com/questions/29327423/use-match-call-to-pass-all-arguments-to-other-function
  # "-1" removes the name of the fn that was called ("IC_evalue")
  #.args = as.list(match.call()[-1])
  
  # this ugly expression is like match.call, but includes args left at their defaults
  # https://stackoverflow.com/questions/14397364/match-call-with-default-arguments
  .args = mget(names(formals()), sys.frame(sys.nframe()))
  # remove other args that are not to be passed to RD_distance
  .args = .args[ !names(.args) %in% c("monotonicBias", "monotonicBiasDirection") ]
  
  
  ### Set up the bounding factor fn to be maximized to get the E-value
  # depends on biasDir assumptions
  if ( monotonicBias == FALSE ) {
    dist_from_true = function(x){
      .args$maxB_1 = x
      # **here's where we assume RD1 > RD0 (i.e., IC_c > 0):
      # **and also IC_c > true because we're applying the bias factors to DECREASE
      #  rather than increase IC_c
      .args$biasDir_1 = "positive"
      .args$maxB_0 = x
      .args$biasDir_0 = "negative"
      do.call( RD_distance, .args )
    }
  }
  
  if ( monotonicBias == TRUE & monotonicBiasDirection == "negative" ) {
    dist_from_true = function(x){
      .args$maxB_1 = 1 # no bias in this stratum
      .args$biasDir_1 = "negative"
      .args$maxB_0 = x
      .args$biasDir_0 = "negative"
      do.call( RD_distance, .args )
    }
  }
  
  if ( monotonicBias == TRUE & monotonicBiasDirection == "positive" ) {
    dist_from_true = function(x){
      .args$maxB_1 = x 
      .args$biasDir_1 = "positive"
      .args$maxB_0 = 1  # no bias in this stratum
      .args$biasDir_0 = "positive"
      do.call( RD_distance, .args )
    }
  }
  
  
  # iteratively increase upper bound of search space
  # because using a too-high value can cause it to not find the sol'n, for some reason
  searchUpper = 2 # upper bound of bias factor search
  proximity = 99  # initialize to a value that will enter the loop
  while( proximity > 0.001 ) {
    searchUpper = searchUpper * 1.5
    opt = optimize( f = function(x) dist_from_true(x)$dist,
                    interval = c(1, searchUpper),
                    maximum = FALSE )
    # closeness of thee distance to 0
    proximity = abs( opt$objective )
    
    # eventually give up
    if( searchUpper >= 200 & proximity > 0.001 ) {
      #@ 
      #browser()
      stop("Tried bias factors up to 200, but still could not move estimate close enough to desired true value. This could indicate an optimization failure.")

    }
  }
  
  return( list( evalues = data.frame( evalue = g(opt$minimum),
                                      biasFactor = opt$minimum,  # not the bias factor, but the regular bias
                                      bound = opt$objective ),  # should be equal to true
                
                # the strata's RDs and the IC evaluated at the E-value
                RDt = dist_from_true(opt$minimum)$RDt )
  )  
  
}



#' Compute an E-value for unmeasured confounding for an additive interaction contrast
#' 
#' Computes the E-value for an additive interaction contrast, representing the difference between stratum Z=1 and stratum Z=0
#' in the causal risk differences for a binary treatment X.  
#' 
#' @param stat The statistic for which to compute the E-value ("est" for the interaction
#' contrast point estimate or "CI" for its lower confidence interval limit)
#' @param true The true (unconfounded) value to which to shift the specified statistic (point estimate or confidence interval limit). Should be smaller than the confounded statistic.
#' 
#' @param monotonicBias Whether the direction of confounding bias is assumed to be the same 
#' in both strata of Z (TRUE or FALSE); see Details
#' @param monotonicBiasDirection If bias is assumed to be monotonic, its assumed direction ("positive", "negative", or "unknown"; see Details). If bias is not assumed to be monotonic, should be NA. 
#' 
#' @param p1_1 The probability of the outcome in stratum Z=1 with treatment X=1
#' @param p1_0 The probability of the outcome in stratum Z=1 with treatment X=0
#' @param n1_1 The sample size in stratum Z=1 with treatment X=1
#' @param n1_0 The sample size in stratum Z=1 with treatment X=0
#' @param f1 The probability in stratum Z=1 of having treatment X=1
#' 
#' @param p0_1 The probability of the outcome in stratum Z=0 with treatment X=1
#' @param p0_0 The probability of the outcome in stratum Z=0 with treatment X=0
#' @param n0_1 The sample size in stratum Z=0 with treatment X=1
#' @param n0_0 The sample size in stratum Z=0 with treatment X=0
#' @param f0 The probability in stratum Z=0 of treatment X=1
#' 
#' @param alpha The alpha-level to be used for p-values and confidence intervals
#' 
#' @return
#' Returns a list containing two dataframes (\code{evalues} and \code{RDt}). The E-value itself can be accessed as \code{evalues$evalue}.
#' 
#' The dataframe \code{evalues} contains the E-value, the corresponding bias factor, the bound on the interaction contrast if confounding were to attain that bias factor (this bound will be close to \code{true}, by construction), and the direction of bias when the bias factor is attained. If you specify non-monotonic, monotonic positive, or monotonic negative bias, the returned direction of bias will simply be what you requested. If you specify monotonic bias of unknown direction, the bias direction will be either positive or negative depending on which direction produces the maximum bias.
#' 
#' The dataframe \code{RDt} contains, for each stratum and for the interaction contrast, bias-corrected estimates (risk differences for the strata and the interaction contrast for \code{stratum = effectMod}), their standard errors, their confidence intervals, and their p-values. These estimates are bias-corrected for the worst-case bias that could arise for confounder(s) whose strength of association are no more severe than the requested E-value for either the estimate or the confidence interval (i.e., the bias factor indicated by \code{evalues$biasFactor}). The bias-corrected risk differences for the two strata (\code{stratum = "1"} and \code{stratum = "0"}) are corrected in the direction(s) indicated by \code{evalues$biasDir}.
#' 
#' If you specify monotonic bias of unknown direction, the E-value is calculated by taking the minimum of the E-value under positive monotonic bias and the E-value under negative monotonic bias. With this specification, a third dataframe (\code{candidates}) will be returned. This is similar to \code{evalues}, but contains the results for positive monotonic bias and negative monotonic bias (the two "candidate" E-values that were considered).
#' 
#' @details
#' ## E-values for additive effect modification
#' The interaction contrast is a measure of additive effect modification that represents the difference between stratum Z=1 versus stratum Z=0 of the causal risk differences relating a treatment X to an outcome Y. The estimated interaction contrast is given by:
#' 
#' \code{(p1_1 - p1_0) - (p0_1 - p0_0)}
#' 
#' To use this function, the strata (Z) should be coded such that the confounded interaction contrast is positive rather than negative.
#' 
#' If, in one or both strata of Z, there are unmeasured confounders of the treatment-outcome association, then the interaction contrast may be biased as well (Mathur et al., 2021). The E-value for the interaction contrast represents the minimum strength of association, on the risk ratio scale, that unmeasured confounder(s) would need to have with both the treatment (X) and the outcome (Y) in both strata of Z to fully explain away the observed interaction contrast, conditional on the measured covariates. This bound is attained when the strata have confounding bias in opposite directions ("non-monotonic bias"). Alternatively, if one assumes that the direction of confounding is the same in each stratum of Z ("monotonic bias"), then the E-value for the interaction contrast is defined as the minimum strength of association, on the risk ratio scale, that unmeasured confounder(s) would need to have with both the treatment (X) and the outcome (Y) in \emph{at least one} stratum of Z to fully explain away the observed interaction contrast, conditional on the measured covariates. This bound under monotonic confounding arises when one stratum is unbiased. See Mathur et al. (2021) for details. 
#' 
#' As for the standard E-value for main effects (Ding & VanderWeele, 2016; VanderWeele & Ding, 2017), the E-value for the interaction contrast can be computed for both the point estimate and the lower confidence interval limit, and it can be also be calculated for shifting the estimate or confidence interval to a non-null value via the argument \code{true}.
#' 
#' ## Specifying the bias direction
#' The argument \code{monotonicBias} indicates whether you are assuming monotonic bias (\code{monotonicBias = TRUE}) or not (\code{monotonicBias = FALSE}). The latter is the default because it is more conservative and requires the fewest assumptions. When setting \code{monotonicBias = FALSE}, there is no need to specify the direction of bias via \code{monotonicBiasDir}. However, when setting \code{monotonicBias = TRUE}, the direction of bias does need to be specified via \code{monotonicBiasDir}, whose options are:
#' \itemize{
#' \item \code{monotonicBiasDir = "positive"}: Assumes that the risk differences in both strata of Z are positively biased. 
#' \item \code{monotonicBiasDir = "negative"}: Assumes that the risk differences in both strata of Z are negatively biased. 
#' \item \code{monotonicBiasDir = "unknown"}: Assumes that the risk differences in both strata of Z are biased in the same direction, but that the direction could be either positive or negative.
#' }
#' 
#' ## Adjusted interaction contrasts
#' If your estimated interaction contrast has been adjusted for covariates, then you can use covariate-adjusted probabilities for \code{p1_1}, \code{p1_0}, \code{p0_1}, and \code{p0_0}. For example, these could be fitted probabilities from a covariate-adjusted regression model.
#' 
#' ## Multiplicative effect modification
#' For multiplicative measures of effect modification (e.g., the ratio of risk ratios between the two strata of Z), you can simply use the function \code{evalue}. To allow the bias to be non-monotonic, you would pass the square-root of your multiplicative effect modification estimate on the risk ratio scale to \code{evalue} rather than the estimate itself. To assume monotonic bias, regardless of direction, you would pass the multiplicative effect modification estimate itself to \code{evalue}. See Mathur et al. (2021) for details.
#' 
#' @references 
#' 1. Mathur MB, Smith LH, Yoshida K, Ding P, VanderWeele TJ (2021). E-values for effect modification and approximations for causal interaction. Under review.
#' 
#' 2. Ding P & VanderWeele TJ (2016). Sensitivity analysis without assumptions. \emph{Epidemiology.} 27(3), 368.
#' 
#' 3. VanderWeele TJ & Ding P (2017). Sensitivity analysis in observational research: Introducing the E-value. \emph{Annals of Internal Medicine.} 27(3), 368.
#' 
#' @export
#' @examples
#' ### Letenneur et al. (2000) example data
#' # this is the example given in Mathur et al. (2021)
#' # Z: sex (w = women, m = male; males are the reference category)
#' # Y: dementia (1 = developed dementia, 0 = did not develop dementia )
#' # X: low education (1 = up to 7 years, 0 = at least 12 years)
#' # n: sample size
#' 
#' # data for women
#' nw_1 = 2988
#' nw_0 = 364
#' dw = data.frame(  Y = c(1, 1, 0, 0),
#'                   X = c(1, 0, 1, 0),
#'                   n = c( 158, 6, nw_1-158, nw_0-6 ) )
#' 
#' # data for men
#' nm_1 = 1790
#' nm_0 = 605
#' dm = data.frame(  Y = c(1, 1, 0, 0),
#'                   X = c(1, 0, 1, 0),
#'                   n = c( 64, 17, nm_1-64, nm_0-17 ) )
#' 
#' # P(Y = 1 | X = 1) and P(Y = 1 | X = 0) for women and for men
#' ( pw_1 = dw$n[ dw$X == 1 & dw$Y == 1 ] / sum(dw$n[ dw$X == 1 ]) )
#' ( pw_0 = dw$n[ dw$X == 0 & dw$Y == 1 ] / sum(dw$n[ dw$X == 0 ]) )
#' ( pm_1 = dm$n[ dm$X == 1 & dm$Y == 1 ] / sum(dm$n[ dm$X == 1 ]) )
#' ( pm_0 = dm$n[ dm$X == 0 & dm$Y == 1 ] / sum(dm$n[ dm$X == 0 ]) )
#' 
#' # prevalence of low education among women and among men
#' fw = nw_1 / (nw_1 + nw_0)
#' fm = nm_1 / (nm_1 + nm_0)
#' 
#' # confounded interaction contrast estimate
#' ( pw_1 - pw_0 ) - ( pm_1 - pm_0 )
#' 
#' ### E-values without making assumptions on direction of confounding bias
#' # for interaction contrast point estimate
#' evalues.IC( stat = "est",
#'        
#'             p1_1 = pw_1,
#'             p1_0 = pw_0,
#'             n1_1 = nw_1,
#'             n1_0 = nw_0,
#'             f1 = fw,
#'             
#'             p0_1 = pm_1,
#'             p0_0 = pm_0,
#'             n0_1 = nm_1,
#'             n0_0 = nm_0,
#'             f0 = fm )
#' 
#' # and for its lower CI limit
#' evalues.IC( stat = "CI",
#'             
#'             p1_1 = pw_1,
#'             p1_0 = pw_0,
#'             n1_1 = nw_1,
#'             n1_0 = nw_0,
#'             f1 = fw,
#'             
#'             p0_1 = pm_1,
#'             p0_0 = pm_0,
#'             n0_1 = nm_1,
#'             n0_0 = nm_0,
#'             f0 = fm )
#' 
#' ### E-values assuming monotonic confounding of unknown direction
#' # for interaction contrast point estimate
#' evalues.IC( stat = "est",
#'             monotonicBias = TRUE,
#'             monotonicBiasDirection = "unknown",
#'             
#'             p1_1 = pw_1,
#'             p1_0 = pw_0,
#'             n1_1 = nw_1,
#'             n1_0 = nw_0,
#'             f1 = fw,
#'             
#'             p0_1 = pm_1,
#'             p0_0 = pm_0,
#'             n0_1 = nm_1,
#'             n0_0 = nm_0,
#'             f0 = fm )
#' 
#' # and for its lower CI limit
#' evalues.IC( stat = "CI",
#'             monotonicBias = TRUE,
#'             monotonicBiasDirection = "unknown",
#'             
#'             p1_1 = pw_1,
#'             p1_0 = pw_0,
#'             n1_1 = nw_1,
#'             n1_0 = nw_0,
#'             f1 = fw,
#'             
#'             p0_1 = pm_1,
#'             p0_0 = pm_0,
#'             n0_1 = nm_1,
#'             n0_0 = nm_0,
#'             f0 = fm )

evalues.IC = function( stat,
                       true = 0,
                       monotonicBias = FALSE,
                       monotonicBiasDirection = NA,
                       
                       p1_1,
                       p1_0,
                       n1_1,
                       n1_0,
                       f1,
                       
                       p0_1,
                       p0_0,
                       n0_1,
                       n0_0,
                       f0,
                       
                       alpha = 0.05 ) {
  
  ##### Catch Bad Input #####
  if ( !stat %in% c("est", "CI") ) stop("Argument 'stat' is invalid")
  if ( !monotonicBias %in% c(FALSE, TRUE) ) stop("Argument 'monotonicBias' is invalid")
  if ( !monotonicBiasDirection %in% c(NA, "positive", "negative", "unknown") ) stop("Argument 'monotonicBiasDirection' is invalid")
  if ( monotonicBias == TRUE & is.na(monotonicBiasDirection) ) stop("If monotonicBias is TRUE, must provide monotonicBiasDirection")
  if ( monotonicBias == FALSE & !is.na(monotonicBiasDirection) ) warning("You specified monotonicBias = FALSE, so the argument monotonicBiasDirection will be ignored.")
  
  
  ##### Prepare Args to Pass to IC_evalue_inner #####
  # collect args passed to present fn
  .args = mget(names(formals()), sys.frame(sys.nframe()))
  
  # we want the effect modification E-value (not stratum E-values)
  .args$stratum = "effectMod"
  
  # do we want E-value for estimate or CI?
  if ( stat == "est" ) .args$varName = "RD"
  # **assumes lower CI limit (i.e., again assumes strata coded such that IC_c > 0)
  if ( stat == "CI" ) .args$varName = "lo"
  
  # remove args that shouldn't be passed along
  .args = .args[ names(.args) != "stat" ]
  
  
  ### Case 0: Non-monotonic bias
  if ( monotonicBias == FALSE ) {
    
    res = do.call( IC_evalue_inner, .args )
    
    res$evalues$biasDir = "non-monotonic"
    return(res)
  }
  
  ### Cases 1-2: Monotonic bias; known direction
  # only need to call IC_evalue_inner once for these cases
  if ( monotonicBias == TRUE & monotonicBiasDirection %in% c("positive", "negative") ) {
    
    res = do.call( IC_evalue_inner, .args )
    
    res$evalues$biasDir = monotonicBiasDirection
    return(res)
  }
  
  ### Case 3: Unknown bias direction
  # now we have to consider both positive and negative bias and choose the 
  #  winning candidate E-value (i.e., the smaller one)
  if ( monotonicBias == TRUE & monotonicBiasDirection == "unknown" ) {
    
    # E-value candidate 1: Shift stratum 1 down to match stratum 0
    .args1 = .args
    .args1$monotonicBiasDirection = "positive"
    
    cand1 = do.call( IC_evalue_inner, .args1 )
    
    
    # E-value candidate 1: Shift stratum 1 down to match stratum 0
    .args2 = .args
    .args2$monotonicBiasDirection = "negative"
    
    cand2 = do.call( IC_evalue_inner, .args2 )
    
    # Choose candidate E-value that is smaller
    #browser()
    
    if ( cand1$evalues$evalue < cand2$evalues$evalue ){
      winnerCand = cand1
      winnerDir = "positive"
    } else{
      winnerCand = cand2
      winnerDir = "negative"
    }
    
    # direction of bias for winner
    winnerCand$evalues$biasDir = winnerDir 
    
    # also return both candidates
    winnerCand$candidates = data.frame( biasDir = c("positive", "negative"),
                                        evalue = c(cand1$evalues$evalue, cand2$evalues$evalue),
                                        biasFactor = c(cand1$evalues$biasFactor, cand2$evalues$biasFactor),
                                        isMin = c(cand1$evalues$evalue == winnerCand$evalues$evalue, cand2$evalues$evalue == winnerCand$evalues$evalue) )
    
    return( winnerCand )
    
  }
  
}


