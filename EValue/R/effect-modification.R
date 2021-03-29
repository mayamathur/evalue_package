
# USAGE NOTES ----------------------


# Algorithm structure:
# IC_evalue > RD_distance > RDt_bound > RDt_var

# If you eventually move these fns to package, note that:
#  - Will need to think through fns' assumptions about signs 
#    (e.g., RDc < 0 case)
#  - Have not dealt with weird cases like if IC^c is already less than IC^t 
#    IC_evalue will probably complain about sqrt(RR * (RR - 1)) being undef'nd in that case
#  - Search "#@" for notes on other assumptions in fns that would need to be gnlz'd  

# BIG STATS FUNCTIONS ----------------------

# bias-corrected variance for one stratum
# Ding & VanderWeele, eAppendix 5 (delta method)
# handles either positive or negative bias
#@think about the fact that recoding depends on confounding rather than true p1, p0

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
  
  # ### Catch bad input
  # #@test this
  # if ( (p1_1 - p1_0) - (p0_1 - p0_0) < 0 ) {
  #   stop("Confounded interaction contrast is ")
  # }
  
  ### Corrected point estimate
  # corrected RD for each stratum - pg 376
  # maxes and mins to avoid RDt > 1 or RDt < -1
  #@test those
  if ( biasDir_1 == "positive" ) RDt_1 = max( -1, ( p1_1 - p1_0 * maxB_1 ) * ( f1 + ( 1 - f1 ) / maxB_1 ) )
  if ( biasDir_1 == "negative" ) RDt_1 = min( 1, ( p1_1 * maxB_1 - p1_0 ) * ( f1 + ( 1 - f1 ) / maxB_1 ) )
  
  if ( biasDir_0 == "positive" ) RDt_0 = max( -1, ( p0_1 - p0_0 * maxB_0 ) * ( f0 + ( 1 - f0 ) / maxB_0 ) )
  if ( biasDir_0 == "negative" ) RDt_0 = min( 1, ( p0_1 * maxB_0 - p0_0 ) * ( f0 + ( 1 - f0 ) / maxB_0 ) )
  
  # # old version (agrees numerically with the above)
  # RDt_1 = ( p1_1 - p1_0 * .maxB ) * ( f1 + ( 1 - f1 ) / .maxB )
  # # corrected RD for X=0 (men) stratum (Shift downward) - pg 376
  # RDt_0 = ( p0_1 * .maxB - p0_0 ) * ( f0 + ( 1 - f0 )/.maxB )  # without recoding f
  
  # calculate interaction contrast
  ICt = RDt_1 - RDt_0
  
  
  ### Corrected confidence interval
  # calculate var for each stratum (W and M) separately
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
  .args = as.list(match.call()[-1])
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
  searchUpper = 4 # upper bound of bias factor search
  proximity = 99  # initialize to a value that will enter the loop
  while( proximity > 0.001 ) {
    #@test a situation that enter this part
    searchUpper = searchUpper * 1.5
    opt = optimize( f = function(x) dist_from_true(x)$dist,
                    interval = c(1, searchUpper),
                    maximum = FALSE )
    # closeness of thee distance to 0
    proximity = abs( opt$objective )
    
    #@give up
    if( searchUpper >= 200 & proximity > 0.001 ) stop("E-value didn't move estimate close enough to true value; look into optimize() call")
  }
  
  # #@ revisit upper bound of search space (500)
  # opt = optimize( f = dist_from_true,
  #                 interval = c(0, 500),
  #                 maximum = FALSE )
  # 
  # #@revisit this
  # if ( abs( opt$objective ) > 0.001 ) 
  
  #bm
  
  return( list( evalues = data.frame( evalue = g(opt$minimum),
                                      biasFactor = opt$minimum,  # not the bias factor, but the regular bias
                                      bound = opt$objective ),  # should be equal to true
                
                # the strata's RDs and the IC evaluated at the E-value
                RDt = dist_from_true(opt$minimum)$RDt )
  )  
  
}


# looks at monotonic bias without assuming direction by calling IC_evalue twice
# NOT in shape for package
# lots of dataset-specific things in here

# varName: "RD" or "lo"


# monotonicBias: TRUE/FALSE
# monotonicBiasDirection: NA, "positive", "negative", "unknown"
# mention that probs can be adjusted for other confounders

#' Compute an E-value for unmeasured confounding for additive effect modification
#' 
#' @param monotonicBias TRUE/FALSE
#' @param monotonicBiasDirection "positive", "negative", or "unknown", or NA
#' @param true
#' 
#' @param p1_1 The probability of the outcome in stratum Z=1 for exposure X=1
#' @param p1_0 The probability of the outcome in stratum Z=1 for exposure X=0
#' @param n1_1 The sample size in stratum Z=1 with exposure X=1
#' @param n1_0 The sample size in stratum Z=1 with exposure X=0
#' @param f1 The prevalence (or incidence) in stratum Z=1 of exposure X=1
#' 
#' @param p0_1 The probability of the outcome in stratum Z=0 for exposure X=1
#' @param p0_0 The probability of the outcome in stratum Z=0 for exposure X=0
#' @param n0_1 The sample size in stratum Z=0 with exposure X=1
#' @param n0_0 The sample size in stratum Z=0 with exposure X=0
#' @param f0 The prevalence (or incidence) in stratum Z=0 of exposure X=1
#' 
#' @param alpha The alpha-level for the p-value and confidence interval. 
#' 

#bm

# for example:
# # enter example datasets (Letenneur)
# # Y: dementia
# # X: low education
# # n: sample size
# 
# # data for women
# nw_1 = 2988
# nw_0 = 364
# dw = data.frame(  Y = c(1, 1, 0, 0),
#                   X = c(1, 0, 1, 0),
#                   n = c( 158, 6, nw_1-158, nw_0-6 ) )
# 
# # data for men
# nm_1 = 1790
# nm_0 = 605
# dm = data.frame(  Y = c(1, 1, 0, 0),
#                   X = c(1, 0, 1, 0),
#                   n = c( 64, 17, nm_1-64, nm_0-17 ) )
# 
# # P(Y = 1 | X = 1) and P(Y = 1 | X = 0) for women and for men
# ( pw_1 = dw$n[ dw$X == 1 & dw$Y == 1 ] / sum(dw$n[ dw$X == 1 ]) )
# ( pw_0 = dw$n[ dw$X == 0 & dw$Y == 1 ] / sum(dw$n[ dw$X == 0 ]) )
# ( pm_1 = dm$n[ dm$X == 1 & dm$Y == 1 ] / sum(dm$n[ dm$X == 1 ]) )
# ( pm_0 = dm$n[ dm$X == 0 & dm$Y == 1 ] / sum(dm$n[ dm$X == 0 ]) )
# 
# # prevalence of low education among women and among men
# fw = nw_1 / (nw_1 + nw_0)
# fm = nm_1 / (nm_1 + nm_0)

#bm: All existing tests are being passed; now should continue working on evalues.IC to catch bad input, generalize, etc. :) You got this! 

evalues.IC = function(
  stat,
  monotonicBias = FALSE,
  monotonicBiasDirection = NA,
  true = 0,
  
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
  
  alpha = 0.05
) {
  
  ##### Catch Bad Input #####
  if ( !stat %in% c("est", "CI") ) stop("Argument 'stat' is invalid")
  if ( !monotonicBias %in% c(FALSE, TRUE) ) stop("Argument 'monotonicBias' is invalid")
  if ( !monotonicBiasDirection %in% c(NA, "positive", "negative", "unknown") ) stop("Argument 'monotonicBiasDirection' is invalid")
  if ( monotonicBias == TRUE & is.na(monotonicBiasDirection) ) stop("If monotonicBias is TRUE, must provide monotonicBiasDirection")
  if ( monotonicBias == FALSE & !is.na(monotonicBiasDirection) ) warning("You specified monotonicBias = FALSE, so the argument monotonicBiasDirection will be ignored.")
  
  
  ##### Prepare Args to Pass to IC_evalue_inner #####
  # collect args passed to present fn
  # https://stackoverflow.com/questions/29327423/use-match-call-to-pass-all-arguments-to-other-function
  # "-1" removes the name of the present fn that was called
  .args = as.list(match.call()[-1])
  
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
    
    #@ docs: explain that this is direction for winning bias
    winnerCand$evalues$biasDir = winnerDir 
    
    # also return both candidates
    winnerCand$candidates = data.frame( biasDir = c("positive", "negative"),
                                        evalue = c(cand1$evalues$evalue, cand2$evalues$evalue),
                                        biasFactor = c(cand1$evalues$biasFactor, cand2$evalues$biasFactor),
                                        isMin = c(cand1$evalues$evalue == winnerCand$evalues$evalue, cand2$evalues$evalue == winnerCand$evalues$evalue) )
    
    return( winnerCand )
    
  }
  
}


