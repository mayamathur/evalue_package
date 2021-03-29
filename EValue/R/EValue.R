#' An example dataset
#'
#' An example dataset from Hsu and Small (Biometrics, 2013). 
#'
#' @docType data
#' @keywords datasets
"lead"


#' Compute E-value for a linear regression coefficient estimate
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion) as well as E-values for the point estimate and the
#' confidence interval limit closer to the null.
#' @param est The linear regression coefficient estimate (standardized or
#'   unstandardized)
#' @param se The standard error of the point estimate
#' @param delta The contrast of interest in the exposure
#' @param sd The standard deviation of the outcome (or residual standard
#'   deviation); see Details
#' @param true The true standardized mean difference to which to Shift the
#'   observed point estimate. Typically set to 0 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @export
#' @keywords e-value
#' @details This function is for linear regression with a continuous exposure
#' and outcome. Regarding the continuous exposure, the choice of \code{delta}
#' defines essentially a dichotomization in the exposure between hypothetical
#' groups of subjects with exposures equal to an arbitrary value \emph{c} versus
#' to another hypothetical group with exposures equal to \emph{c} +
#' \code{delta}. Regarding the continuous outcome, the function uses the
#' effect-size conversions in Chinn (2000) and VanderWeele (2017) to
#' approximately convert the mean difference between these exposure "groups" to
#' the odds ratio that would arise from dichotomizing the continuous outcome.
#'
#' For example, if resulting E-value is 2, this means that unmeasured
#' confounder(s) would need to double the probability of a subject's having
#' exposure equal to \emph{c} + \code{delta} instead of \emph{c}, and would also
#' need to double the probability of being high versus low on the outcome, in
#' which the cutoff for "high" versus "low" is arbitrary subject to some
#' distributional assumptions (Chinn, 2000).
#'
#' A true standardized mean difference for linear regression would use \code{sd}
#' = SD(Y | X, C), where Y is the outcome, X is the exposure of interest, and C
#' are any adjusted covariates. See Examples for how to extract this from
#' \code{lm}. A conservative approximation would instead use \code{sd} = SD(Y).
#' Regardless, the reported E-value for the confidence interval treats \code{sd}
#' as known, not estimated.
#' @references Chinn, S (2000). A simple method for converting an odds ratio to
#' effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22),
#' 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for
#' a common outcome. \emph{Epidemiology}, 28(6), e58.
#' @examples
#' # first standardizing conservatively by SD(Y)
#' data(lead)
#' ols = lm(age ~ income, data = lead)
#'
#' # for a 1-unit increase in income
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = sd(lead$age))
#'
#' # for a 0.5-unit increase in income
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = sd(lead$age),
#'             delta = 0.5)
#'
#' # now use residual SD to avoid conservatism
#' # here makes very little difference because income and age are
#' # not highly correlated
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = summary(ols)$sigma)

evalues.OLS = function( est, se = NA, sd, delta = 1, true = 0, ... ) {
  
  if ( !is.na( se ) ) {
    if ( se < 0 ) stop( "Standard error cannot be negative" )
  }
  
  if ( delta < 0 ) {
    delta = -delta
    wrapmessage( "Recoding delta to be positive" )
  }
  
  if ( !inherits(est, "OLS") ) est = OLS( est, sd = sd )
  if ( !inherits(se, "OLS") ) se = OLS( se, sd = attr(est, "sd") )
  if ( !inherits(true, "MD") ) true = MD( true )
  
  # rescale to reflect a contrast of size delta
  est = toMD( est, delta = delta )
  se = toMD( se, delta = delta )
  
  return( evalues.MD( est = est, se = se, true = true ) )
}


#' Compute E-value for a difference of means and its confidence interval limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion) as well as E-values for the point estimate and the
#' confidence interval limit closer to the null.
#' @param est The point estimate as a standardized difference (i.e., Cohen's d)
#' @param se The standard error of the point estimate
#' @param true The true standardized mean difference to which to Shift the
#'   observed point estimate. Typically set to 0 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @export
#' @keywords e-value
#' @details 
#' Regarding the continuous outcome, the function uses the effect-size conversions in Chinn (2000)
#' and VanderWeele (2017) to approximately convert the mean difference between the exposed versus unexposed groups
#' to the odds ratio that would arise from dichotomizing the continuous outcome.
#' 
#' For example, if resulting E-value is 2, this means that unmeasured confounder(s) would need to double
#' the probability of a subject's being exposed versus not being exposed, and would also need to
#' double the probability of being high versus low on the outcome, in which the cutoff for "high" versus
#' "low" is arbitrary subject to some distributional assumptions (Chinn, 2000). 
#' @references 
#' Chinn, S (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22), 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for a common outcome. \emph{Epidemiology}, 28(6), e58.
#' @examples
#' # compute E-value if Cohen's d = 0.5 with SE = 0.25
#' evalues.MD(.5, .25)

evalues.MD = function( est, se = NA, true = 0, ... ) {
  
  if ( !is.na( se ) ) {
    if ( se < 0 ) stop( "Standard error cannot be negative" )
  }
  
  if ( !inherits(est, "MD") ) est = MD(est)
  if ( !inherits(true, "MD") ) true = MD(true)
  
  lo = NA
  hi = NA
  if ( !is.na(se) ) {
    lo = exp( 0.91 * est - 1.78 * se )
    hi = exp( 0.91 * est + 1.78 * se )
    #lo =  exp( log( est ) - 1.96 * log( MDtoRR( se ) )) # ( est converted )
    #hi =  exp( log( est ) + 1.96 * log( MDtoRR( se ) ))
  }
  
  if ( !is.na(lo) ) lo = RR(lo)
  if ( !is.na(hi) ) hi = RR(hi)
  est = toRR(est)
  true = toRR(true)
  
  return( evalues.RR( est = est, lo = lo, hi = hi, true = true ) )
}



#' Compute E-value for a hazard ratio and its confidence interval limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion if needed when outcome is common ) as well as E-values
#' for the point estimate and the confidence interval limit closer to the null.
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if
#'   outcome is not rare (>15 percent at end of follow-up)
#' @param true The true HR to which to Shift the observed point estimate.
#'   Typically set to 1 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @export
#' @keywords e-value
#' @examples
#' # compute E-value for HR = 0.56 with CI: [0.46, 0.69]
#' # for a common outcome
#' evalues.HR(0.56, 0.46, 0.69, rare = FALSE)


evalues.HR = function( est, lo = NA, hi = NA, rare = NA, true = 1, ... ) {
  
  # sanity checks
  if ( est < 0 ) stop( "HR cannot be negative" )
  
  if ( is.na(rare) ) rare = NULL # for compatibility w/ HR constructor
  
  if ( !inherits(est, "HR") ) est = HR( est, rare = rare )
  if ( !is.na(lo) && !inherits(lo, "HR") ) lo = HR( lo, rare = attr(est, "rare") )
  if ( !is.na(hi) && !inherits(hi, "HR") ) hi = HR( hi, rare = attr(est, "rare") )
  if ( !inherits(true, "HR") ) true = HR( true, rare = attr(est, "rare") )
  
  est = toRR(est)
  if ( !is.na(lo) ) lo = toRR(lo)
  if ( !is.na(hi) ) hi = toRR(hi)
  true = toRR(true)
  
  return( evalues.RR( est = est, lo = lo, hi = hi, true = true ) )
}



#' Compute E-value for an odds ratio and its confidence interval limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (through an
#' approximate conversion if needed when outcome is common) as well as E-values
#' for the point estimate and the confidence interval limit closer to the null.
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if
#'   outcome is not rare (>15 percent at end of follow-up)
#' @param true The true OR to which to Shift the observed point estimate.
#'   Typically set to 1 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @keywords e-value
#'
#' @export
#' @examples
#' # compute E-values for OR = 0.86 with CI: [0.75, 0.99]
#' # for a common outcome
#' evalues.OR(0.86, 0.75, 0.99, rare = FALSE)
#'
#' ## Example 2
#' ## Hsu and Small (2013 Biometrics) Data
#' ## sensitivity analysis af1er log-linear or logistic regression
#'
#' head(lead)
#'
#' ## log linear model -- obtain the conditional risk ratio
#' lead.loglinear = glm(lead ~ ., family = binomial(link = "log"),
#'                          data = lead[,-1])
#' est = summary(lead.loglinear)$coef["smoking", c(1, 2)]
#'
#' RR       = exp(est[1])
#' lowerRR  = exp(est[1] - 1.96*est[2])
#' upperRR  = exp(est[1] + 1.96*est[2])
#' evalues.RR(RR, lowerRR, upperRR)
#'
#' ## logistic regression -- obtain the conditional odds ratio
#' lead.logistic = glm(lead ~ ., family = binomial(link = "logit"),
#'                         data = lead[,-1])
#' est = summary(lead.logistic)$coef["smoking", c(1, 2)]
#'
#' OR       = exp(est[1])
#' lowerOR  = exp(est[1] - 1.96*est[2])
#' upperOR  = exp(est[1] + 1.96*est[2])
#' evalues.OR(OR, lowerOR, upperOR, rare=FALSE)


evalues.OR = function( est, lo = NA, hi = NA, rare = NA, true = 1, ... ) {
  
  # sanity checks
  if ( est < 0 ) stop( "OR cannot be negative" )
  
  if ( is.na(rare) ) rare = NULL # for compatibility w/ OR constructor
  
  if ( !inherits(est, "OR") ) est = OR( est, rare = rare )
  if ( !is.na(lo) && !inherits(lo, "OR") ) lo = OR( lo, rare = attr(est, "rare") )
  if ( !is.na(hi) && !inherits(hi, "OR") ) hi = OR( hi, rare = attr(est, "rare") )
  if ( !inherits(true, "OR") ) true = OR( true, rare = attr(est, "rare"))
  
  est = toRR(est)
  if ( !is.na(lo) ) lo = toRR(lo)
  if ( !is.na(hi) ) hi = toRR(hi)
  true = toRR(true)
  
  return( evalues.RR( est = est, lo = lo, hi = hi, true = true ) )
}



#' Compute E-value for a risk ratio or rate ratio and its confidence interval
#' limits
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit for the risk ratio (as provided by the user)
#' as well as E-values for the point estimate and the confidence interval limit
#' closer to the null.
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param true The true RR to which to Shift the observed point estimate.
#'   Typically set to 1 to consider a null true effect.
#' @param ... Arguments passed to other methods.
#' @keywords e-value
#' @export
#' @examples
#' # compute E-value for leukemia example in VanderWeele and Ding (2017)
#' evalues.RR(0.80, 0.71, 0.91)
#'
#' # you can also pass just the point estimate
#' evalues.RR(0.80)
#'
#' # demonstrate symmetry of E-value
#' # this apparently causative association has same E-value as the above
#' evalues.RR(1 / 0.80)

evalues.RR = function( est, lo = NA, hi = NA, true = 1, ... ) {
  
  # organize user's values
  values = c( est, lo, hi )
  
  # sanity checks
  if ( est < 0 ) stop( "RR cannot be negative" )
  if ( true < 0 ) stop( "True value is impossible" )
  
  # warn user if using non-null true value
  if ( true != 1 ) wrapmessage(c("You are calculating a \"non-null\" E-value,",
                                 "i.e., an E-value for the minimum amount of unmeasured",
                                 "confounding needed to move the estimate and confidence",
                                 "interval to your specified true value rather than to",
                                 "the null value."))
  
  # check if CI crosses null
  null.CI = NA
  if ( est > true & !is.na( lo ) ) {
    null.CI = ( lo < true )
  }
  
  if ( est < true & !is.na( hi ) ) {
    null.CI = ( hi > true )
  }
  
  
  # sanity checks for CI
  if ( !is.na( lo ) & !is.na( hi ) ) {
    # check if lo < hi
    if ( lo > hi ) stop( "Lower confidence limit should be less than upper confidence limit" )
  }
  
  if ( !is.na( lo ) & est < lo ) stop( "Point estimate should be inside confidence interval" )
  if ( !is.na( hi ) & est > hi ) stop( "Point estimate should be inside confidence interval" )
  
  # compute E-values
  E = sapply( values, FUN = function(x) threshold( x, true = true ) )
  
  
  # clean up CI reporting
  # if CI crosses null, set its E-value to 1
  if ( !is.na(null.CI) & null.CI == TRUE ){
    E[ 2:3 ] = 1
    wrapmessage("Confidence interval crosses the true value, so its E-value is 1.") 
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
  
  rownames(result) = c("RR", "E-values")
  colnames(result) = c("point", "lower", "upper")
  class(result) = c("evalue", "matrix")
  
  result
}


#'Estimate risk ratio and compute CI limits from two-by-two table
#'
#'Given counts in a two-by-two table, computes risk ratio and confidence
#'interval limits.
#'@param n11 Number exposed (X=1) and diseased (D=1)
#'@param n10 Number exposed (X=1) and not diseased (D=0)
#'@param n01 Number unexposed (X=0) and diseased (D=1)
#'@param n00 Number unexposed (X=0) and not diseased (D=0)
#'@param alpha Alpha level associated with confidence interval
#'@export
#'@import stats
#' @examples
#' # Hammond and Holl (1958 JAMA) Data
#' # Two by Two Table
#' #          Lung Cancer    No Lung Cancer
#'# Smoker    397            78557
#'# Nonsmoker 51             108778
#'
#'twoXtwoRR(397, 78557, 51, 108778)

twoXtwoRR = function( n11, n10, n01, n00, alpha = 0.05 ){
  
  p1     = n11/( n11 + n10 )
  p0     = n01/( n01 + n00 )
  RR     = p1/p0
  logRR  = log( RR )
  
  selogRR  = sqrt( 1/n11 - 1/( n11+n10 ) + 1/n01 - 1/( n01+n00 ) )
  q.alpha  = qnorm( 1 - alpha/2 )
  
  upperRR  = exp( logRR + q.alpha*selogRR )
  lowerRR  = exp( logRR - q.alpha*selogRR )
  
  res         = c( RR, upperRR, lowerRR )
  names(res)  = c( "point", "upper", "lower" )
  
  return(res) 
}




#'Compute E-value for single value of risk ratio
#'
#'Computes E-value for a single value of the risk ratio. Users should typically
#'call the relevant \code{evalues.XX()} function rather than this internal
#'function.
#'@param x The risk ratio
#'@param true The true RR to which to Shift the observed point estimate.
#'  Typically set to 1 to consider a null true effect.
#'@export
#'@keywords internal
#'
#' @examples
#' ## Example 1
#' ## Hammond and Holl (1958 JAMA) Data
#' ## Two by Two Table
#' #          Lung Cancer    No Lung Cancer
#'# Smoker    397            78557
#'# Nonsmoker 51             108778
#'
#' # first get RR and CI bounds
#' twoXtwoRR(397, 78557, 51, 108778)
#'
#' # then compute E-values
#' evalues.RR(10.729780, 8.017457, 14.359688)


threshold = function( x, true = 1 ) {
  
  if ( is.na(x) ) return(NA)
  
  if( x < 0 ){
    warning("The risk ratio must be non-negative.")
  }  
  
  if( x <= 1 ){
    x = 1 / x
    true = 1 / true
  }
  
  # standard case: causal effect is toward null
  if ( true <= x ) return( ( x + sqrt( x * ( x - true ) ) ) / true )
  
  # causal effect is away from null
  else if ( true > x ) {
    # ratio that is > 1
    rat = true / x 
    return( rat + sqrt( rat * ( rat - 1 ) ) )
  }
  
}







#' Compute E-value for a population-standardized risk difference and its
#' confidence interval limits
#'
#' Returns E-values for the point estimate and the lower confidence interval
#' limit for a positive risk difference. If the risk difference is negative, the
#' exposure coding should be first be reversed to yield a positive risk
#' difference.
#' @param n11 Number of exposed, diseased individuals
#' @param n10 Number of exposed, non-diseased individuals
#' @param n01 Number of unexposed, diseased individuals
#' @param n00 Number of unexposed, non-diseased individuals
#' @param true True value of risk difference to which to Shift the point
#'   estimate. Usually set to 0 to consider the null.
#' @param alpha Alpha level
#' @param grid Spacing for grid search of E-value
#' @param ... Arguments passed to other methods.
#' @keywords e-value
#'
#' @export
#' @export evalues.RD
#' @import stats graphics
#' @examples
#'
#' ## example 1
#' ## Hammond and Holl (1958 JAMA) Data
#' ## Two by Two Table
#' ##          Lung Cancer    No Lung Cancer
#' ##Smoker    397            78557
#' ##Nonsmoker 51             108778
#'
#' # E-value to Shift observed risk difference to 0
#' evalues.RD(397, 78557, 51, 108778)
#'
#' # E-values to Shift observed risk difference to other null values
#' evalues.RD(397, 78557, 51, 108778, true = 0.001)


evalues.RD = function( n11, n10, n01, n00,  
                       true = 0, alpha = 0.05, grid = 0.0001, ... ) {
  
  # sanity check
  if ( any( c(n11, n10, n01, n00) < 0 ) ) stop("Negative cell counts are impossible.")
  
  # sample sizes
  N = n10 + n11 + n01 + n00
  N1 = n10 + n11  # total X=1
  N0 = n00 + n01  # total X=0
  
  # compute f = P(X = 1)
  f = N1 / N
  
  # P(D = 1 | X = 1)
  p1  = n11 / N1
  
  # P(D = 1 | X = 0)
  p0  = n01 / N0
  
  if( p1 < p0 ) stop("RD < 0; please relabel the exposure such that the risk difference > 0.")
  
  
  # standard errors
  se.p1 = sqrt( p1 * ( 1-p1 ) / N1 )
  se.p0 = sqrt( p0 * ( 1-p0 ) / N0 )
  
  # back to Peng's code
  s2.f   = f*( 1-f )/N
  s2.p1  = se.p1^2
  s2.p0  = se.p0^2
  diff   = p0*( 1-f ) - p1*f
  
  # bias factor and E-value for point estimate
  est.BF = ( sqrt( ( true + diff )^2 + 4 * p1 * p0 * f * ( 1-f )  ) - ( true + diff ) ) / ( 2 * p0 * f )
  est.Evalue    = threshold(est.BF)   
  if( p1 - p0 <= true ) stop("For risk difference, true value must be less than or equal to point estimate.")
  
  # compute lower CI limit
  Zalpha        = qnorm( 1-alpha/2 )  # critical value
  lowerCI       = p1 - p0 - Zalpha*sqrt( s2.p1 + s2.p0 )
  
  # check if CI contains null
  if ( lowerCI <= true ) {
    
    # warning( "Lower CI limit of RD is smaller than or equal to true value." )
    return( list( est.Evalue = est.Evalue, lower.Evalue = 1 ) )
    
  } else {
    # find E-value for lower CI limit
    # we know it's less than or equal to E-value for point estimate
    BF.search = seq( 1, est.BF, grid )
    
    # population-standardized risk difference
    RD.search = p1 - p0 * BF.search
    f.search  = f + ( 1-f )/BF.search
    
    # using equation for RD^true on pg 376, compute the lower CI limit for these parameters
    # RD.search * f.search is exactly the RHS of the inequality for RD^true ( population )
    Low.search = RD.search * f.search -
      Zalpha * sqrt( ( s2.p1 + s2.p0 * BF.search^2 ) * f.search^2 +
                       RD.search^2 * ( 1 - 1 / BF.search )^2 * s2.f )
    
    # get the first value for BF_u such that the CI limit hits the true value
    Low.ind    = ( Low.search <= true )
    Low.no     = which( Low.ind==1 )[1]
    lower.Evalue = threshold( BF.search[Low.no] )
    
    
    return(list(est.Evalue   = est.Evalue,
                lower.Evalue = lower.Evalue))
  }
  
}




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
#' @import
#' @noRd
var_prop = function(p, n) ( p * (1 - p) ) / n

#' Variance of bias-corrected risk difference
#'
#' An internal function that estimates the variance of a bias-corrected risk difference when the bias factor (on the ratio scale) is \code{.maxB}. Users should call \code{evalues.int.contr} instead. Assumes we are considering bias away from the null. Does not make assumptions about sign of risk difference.
#' @import
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
#' @import
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
#' @import
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
#' @import
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
#' @import
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





#' Plot bias factor as function of confounding relative risks
#'
#' Plots the bias factor required to explain away a provided relative risk.
#' @param RR The relative risk
#' @param xmax Upper limit of x-axis.
#' @export
#' @keywords e-value
#' @examples
#' # recreate the plot in VanderWeele and Ding (2017)
#' bias_plot(RR=3.9, xmax=20)

bias_plot = function( RR, xmax ) {
  
  x = seq( 0, xmax, 0.01 )
  
  # MM: reverse RR if it's preventive
  if ( RR < 1 ) RR = 1/RR
  
  plot( x, x, lty = 2, col = "white", type = "l", xaxs = "i", yaxs = "i", xaxt="n", yaxt = "n",
        xlab = expression( RR[EU] ), ylab = expression( RR[UD] ),
        xlim = c( 0,xmax ),
        main = "" )
  
  x = seq( RR, xmax, 0.01 )
  
  y    = RR*( RR-1 )/( x-RR )+RR
  
  lines( x, y, type = "l" )
  
  
  high = RR + sqrt( RR*( RR-1 ) )
  
  
  points( high, high, pch = 19 )
  
  label5 = seq( 5, 40, by = 5 )
  axis( 1, label5, label5, cex.axis = 1 )
  axis( 2, label5, label5, cex.axis = 1 )
  
  g = round( RR + sqrt( RR * ( RR - 1 ) ), 2 )
  label = paste( "( ", g, ", ", g, " )", sep="" )
  
  text( high + 3, high + 1, label )
  
  legend( "bottomlef1", expression(
    RR[EU]*RR[UD]/( RR[EU]+RR[UD]-1 )==RR
  ), 
  lty = 1:2,
  bty = "n" )
  
}


#' @export
evalue.RR = function( est, lo = NA, hi = NA, se = NA, delta = NA, true = 1, ... ){
  evalues.RR(est = est, lo = lo, hi = hi, true = true, ...)
}

#' @export
evalue.OR = function(est, lo = NA, hi = NA, se = NA, delta = NA, true = 1, ...){
  evalues.OR(est = est, lo = lo, hi = hi, true = true, ...)
}

#' @export
evalue.HR = function(est, lo = NA, hi = NA, se = NA, delta = NA,true = 1, ...){
  evalues.HR(est = est, lo = lo, hi = hi, true = true, ...)
}

#' @export
evalue.OLS = function(est, lo = NA, hi = NA, se = NA, delta = 1, true = 0, ...){
  evalues.OLS(est, se = se, delta = delta, true = true, ...)
}

#' @export
evalue.MD = function(est, lo = NA, hi = NA, se = NA, delta = NA, true = 0, ...){
  evalues.MD(est, se = se, true = true, ...)
}


#' @export
evalue.default <- function(est, ...) {
  
  if (is.null(measure) && !inherits(est, "estimate")) stop("Effect measure must be specified")
  
  measure <- class(est)[1]
  
  evalues_func = switch(measure,
                        "HR" = evalues.HR,
                        "OR" = evalues.OR,
                        "RR" = evalues.RR,
                        "RD" = evalues.RD,
                        "OLS" = evalues.OLS,
                        "MD" = evalues.MD)
  
  evalues_func(est, ...)
}

#' Compute an E-value for unmeasured confounding
#'
#' Returns a data frame containing point estimates, the lower confidence limit,
#' and the upper confidence limit on the risk ratio scale (possibly through an
#' approximate conversion) as well as E-values for the point estimate and the
#' confidence interval limit closer to the null.
#' @param est The effect estimate that was observed but which is suspected to be
#'   biased. A number of class "estimate" (constructed with [RR()], [OR()],
#'   [HR()], [OLS()], or [MD()]; for E-values for risk differences, 
#'   see [evalues.RD()]).
#' @param lo Optional. Lower bound of the confidence interval. If not an object
#'   of class "estimate", assumed to be on the same scale as `est`.
#' @param hi Optional. Upper bound of the confidence interval. If not an object
#'   of class "estimate", assumed to be on the same scale as `est`.
#' @param true A number to which to Shift the observed estimate to. Defaults to
#'   1 for ratio measures ([RR()], [OR()], [HR()]) and 0 for additive measures
#'   ([OLS()], [MD()]).
#' @param se The standard error of the point estimate, for `est` of class "OLS"
#' @param delta The contrast of interest in the exposure, for `est` of class "OLS"
#' @param ... Arguments passed to other methods.
#' @export
#' @details An E-value for unmeasured confounding is minimum strength of
#'   association, on the risk ratio scale, that an unmeasured confounder would
#'   need to have with both the treatment and the outcome to fully explain away
#'   a specific treatment–outcome association, conditional on the measured
#'   covariates.
#'
#'   The estimate is converted appropriately before the E-value is calculated.
#'   See [conversion functions][convert_measures] for more details. The point
#'   estimate and confidence limits af1er conversion are returned, as is the
#'   E-value for the point estimate and the confidence limit closest to the
#'   proposed "true" value (by default, the null value.)
#'
#'   For an [OLS()] estimate, the E-value is for linear regression with a
#'   continuous exposure and outcome. Regarding the continuous exposure, the
#'   choice of \code{delta} defines essentially a dichotomization in the
#'   exposure between hypothetical groups of subjects with exposures equal to an
#'   arbitrary value \emph{c} versus to another hypothetical group with
#'   exposures equal to \emph{c} + \code{delta}.
#'
#'   For example, if resulting E-value is 2, this means that unmeasured
#'   confounder(s) would need to double the probability of a subject's having
#'   exposure equal to \emph{c} + \code{delta} instead of \emph{c}, and would
#'   also need to double the probability of being high versus low on the
#'   outcome, in which the cutoff for "high" versus "low" is arbitrary subject
#'   to some distributional assumptions (Chinn, 2000).
#'   
#' @keywords e-value
#' @export
#' @examples
#' # compute E-value for leukemia example in VanderWeele and Ding (2017)
#' evalue(RR(0.80), 0.71, 0.91)
#'
#' # you can also pass just the point estimate
#' # and return just the E-value for the point estimate with summary()
#' summary(evalue(RR(0.80)))
#'
#' # demonstrate symmetry of E-value
#' # this apparently causative association has same E-value as the above
#' summary(evalue(RR(1 / 0.80)))
#' 
#' # E-value for a non-null true value
#' summary(evalue(RR(2), true = 1.5))
#' 
#' ## Hsu and Small (2013 Biometrics) Data
#' ## sensitivity analysis af1er log-linear or logistic regression
#' head(lead)
#'
#' ## log linear model -- obtain the conditional risk ratio
#' lead.loglinear = glm(lead ~ ., family = binomial(link = "log"),
#'                          data = lead[,-1])
#' est_se = summary(lead.loglinear)$coef["smoking", c(1, 2)]
#'
#' est      = RR(exp(est_se[1]))
#' lowerRR  = exp(est_se[1] - 1.96*est_se[2])
#' upperRR  = exp(est_se[1] + 1.96*est_se[2])
#' evalue(est, lowerRR, upperRR)
#'
#' ## logistic regression -- obtain the conditional odds ratio
#' lead.logistic = glm(lead ~ ., family = binomial(link = "logit"),
#'                         data = lead[,-1])
#' est_se = summary(lead.logistic)$coef["smoking", c(1, 2)]
#'
#' est      = OR(exp(est_se[1]), rare = FALSE)
#' lowerOR  = exp(est_se[1] - 1.96*est_se[2])
#' upperOR  = exp(est_se[1] + 1.96*est_se[2])
#' evalue(est, lowerOR, upperOR)
#' 
#' ## linear regression
#' # standardizing conservatively by SD(Y)
#' ols = lm(age ~ income, data = lead)
#' est = OLS(ols$coefficients[2], sd = sd(lead$age))
#'
#' # for a 1-unit increase in income 
#' evalue(est = est, 
#'        se = summary(ols)$coefficients['income', 'Std. Error'])
#' 
#' # for a 0.5-unit increase in income
#' evalue(est = est,
#'        se = summary(ols)$coefficients['income', 'Std. Error'],
#'        delta = 0.5)
#'
#' # E-value for Cohen's d = 0.5 with SE = 0.25
#' evalue(est = MD(.5), se = .25)
#' 
#' # compute E-value for HR = 0.56 with CI: [0.46, 0.69]
#' # for a common outcome
#' evalue(HR(0.56, rare = FALSE), lo = 0.46, hi = 0.69)
#' # for a rare outcome
#' evalue(HR(0.56, rare = TRUE), lo = 0.46, hi = 0.69)

evalue = function( est, lo = NA, hi = NA, se = NA, delta = 1, true = c(0, 1), ... ) {
  UseMethod( "evalue")
}

#' @export
summary.evalue = function( object, ... ) {
  if ( !inherits(object, "evalue")) stop('Argument must be of class "evalue"')
  object[2,1]
}

#' @export
print.evalue = function( x, ... ) {
  class(x) <- "matrix" # to suppress attr printing
  print.default(x)
}








