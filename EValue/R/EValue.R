#' An example dataset
#'
#' An example dataset from Hsu and Small (Biometrics, 2013). 
#'
#' @docType data
#' @keywords datasets
"lead"


#' Compute E-value for a linear regression coefficient estimate
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The linear regression coefficient estimate (standardized or unstandardized)
#' @param se The standard error of the point estimate
#' @param delta The contrast of interest in the exposure
#' @param sd The standard deviation of the outcome (or residual standard deviation); see Details
#' @param true The true standardized mean difference to which to shift the observed point estimate. Typically set to 0 to consider a null true effect. 
#' @export
#' @details 
#' This function is for linear regression with a continuous exposure and outcome.
#' Regarding the continuous exposure, the choice of \code{delta} defines essentially a 
#' dichotomization in the exposure between hypothetical groups of subjects with exposures equal to an arbitrary
#' value \emph{c} versus to another hypothetical group with exposures equal to \emph{c} + \code{delta}.
#' Regarding the continuous outcome, the function uses the effect-size conversions in Chinn (2000)
#' and VanderWeele (2017) to approximately convert the mean difference between these exposure "groups"
#' to the odds ratio that would arise from dichotomizing the continuous outcome.
#' 
#' For example, if resulting E-value is 2, this means that unmeasured confounder(s) would need to double
#' the probability of a subject's having exposure equal to \emph{c} + \code{delta} instead of \emph{c}, and would also need to
#' double the probability of being high versus low on the outcome, in which the cutoff for "high" versus
#' "low" is arbitrary subject to some distributional assumptions (Chinn, 2000). 
#' 
#' A true standardized mean difference for linear regression would use \code{sd} = SD(Y | X, C), where Y is
#' the outcome, X is the exposure of interest, and C are any adjusted covariates. See Examples for how to extract 
#' this from \code{lm}. A conservative approximation would instead use \code{sd} = SD(Y). Regardless, the reported E-value
#' for the confidence interval treats \code{sd} as known, not estimated.  
#' @references
#' Chinn, S (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22), 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for a common outcome. \emph{Epidemiology}, 28(6), e58.
#' @examples
#' # first standardizing conservatively by SD(Y)
#' data(lead)
#' ols = lm(age ~ income, data = lead)
#' 
#' # for a 1-unit increase in income
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = sd(lead$age) )
#'             
#' # for a 0.5-unit increase in income
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = sd(lead$age),
#'             delta = 0.5 )
#'             
#' # now use residual SD to avoid conservatism
#' # here makes very little difference because income and age are
#' # not highly correlated
#' evalues.OLS(est = ols$coefficients[2],
#'             se = summary(ols)$coefficients['income', 'Std. Error'],
#'             sd = summary(ols)$sigma )

evalues.OLS = function( est, se = NA, sd, delta = 1, true = 0 ) {
  
  if ( !is.na(se) ) {
    if ( se < 0 ) stop("Standard error cannot be negative")
  }
  
  if ( delta < 0 ) {
    delta = -delta
    message("Recoding delta to be positive")
  }
  
  # rescale to reflect a contrast of size delta
  est = est * delta
  se = se * delta
  
  return( evalues.MD( est = est / sd, se = se / sd, true = true ) )
}


#' Compute E-value for a difference of means and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate as a standardized difference (i.e., Cohen's d)
#' @param se The standard error of the point estimate
#' @param true The true standardized mean difference to which to shift the observed point estimate. Typically set to 0 to consider a null true effect. 
#' @export
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
#' evalues.MD( .5, .25 )

evalues.MD = function( est, se = NA, true = 0 ) {
  
  if ( !is.na(se) ) {
    if ( se < 0 ) stop("Standard error cannot be negative")
  }

  values = c()
  values[1] = exp( 0.91 * est )
  values[2] = exp( 0.91 * est - 1.78 * se ) 
  values[3] = exp( 0.91 * est + 1.78 * se )
  
  # convert true value to which to shift observed point estimate
  #  to RR scale
  true.RR = exp( 0.91 * true )
  
  return( evalues.RR( values[1], values[2], values[3], true = true.RR ) )
}



#' Compute E-value for a hazard ratio and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion if needed when outcome is common) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if outcome is not rare (>15 percent at end of follow-up)
#' @param true The true HR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @export
#' @examples
#' # compute E-value for HR = 0.56 with CI: [0.46, 0.69]
#' # for a common outcome
#' evalues.HR( 0.56, 0.46, 0.69, rare = FALSE )


evalues.HR = function( est, lo = NA, hi = NA, rare = NA, true = 1 ) {
  
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
  
  return( evalues.RR( values[1], values[2], values[3], true = true.RR ) )
}



#' Compute E-value for an odds ratio and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion if needed when outcome is common) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval 
#' @param rare 1 if outcome is rare (<15 percent at end of follow-up); 0 if outcome is not rare (>15 percent at end of follow-up)
#' @param true The true OR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @export
#' @examples
#' # compute E-values for OR = 0.86 with CI: [0.75, 0.99]
#' # for a common outcome
#' evalues.OR( 0.86, 0.75, 0.99, rare = FALSE )
#' 
#' ## Example 2
#' ## Hsu and Small (2013 Biometrics) Data
#' ## sensitivity analysis after log-linear or logistic regression
#' 
#' head(lead)
#' 
#' ## log linear model -- obtain the conditional risk ratio
#' lead.loglinear = glm(lead ~ ., family = binomial(link = "log"), 
#'                          data = lead)
#' est = summary( lead.loglinear )$coef[2, c(1, 2)]
#' 
#' RR       = exp(est[1])
#' lowerRR  = exp(est[1] - 1.96*est[2])
#' upperRR  = exp(est[1] + 1.96*est[2]) 
#' evalues.RR(RR, lowerRR, upperRR)
#' 
#' ## logistic regression -- obtain the conditional odds ratio
#' lead.logistic = glm(lead ~ ., family = binomial(link = "logit"), 
#'                         data = lead)
#' est = summary( lead.logistic )$coef[2, c(1, 2)]
#' 
#' OR       = exp(est[1])
#' lowerOR  = exp(est[1] - 1.96*est[2])
#' upperOR  = exp(est[1] + 1.96*est[2]) 
#' evalues.OR(OR, lowerOR, upperOR, rare=FALSE)


evalues.OR = function( est, lo = NA, hi = NA, rare = NA, true = 1 ) {
  
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
  
  return( evalues.RR( values[1], values[2], values[3], true = true.RR ) )
}



#' Compute E-value for a risk ratio or rate ratio and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' for the risk ratio (as provided by the user) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate
#' @param lo The lower limit of the confidence interval
#' @param hi The upper limit of the confidence interval
#' @param true The true RR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @export
#' @examples
#' # compute E-value for leukemia example in VanderWeele and Ding (2017)
#' evalues.RR( 0.80, 0.71, 0.91 )
#' 
#' # you can also pass just the point estimate
#' evalues.RR( 0.80 )
#' 
#' # demonstrate symmetry of E-value
#' # this apparently causative association has same E-value as the above
#' evalues.RR( 1 / 0.80 )

evalues.RR = function( est, lo = NA, hi = NA, true = 1 ) {

  # organize user's values
  values = c(est, lo, hi)
  
  # sanity checks
  if ( est < 0 ) stop("RR cannot be negative")
  if ( true < 0 ) stop("True value is impossible")
  
  # warn user if using non-null true value
  if ( true != 1 ) message('You are calculating a "non-null" E-value,
                           i.e., an E-value for the minimum amount of unmeasured
                           confounding needed to move the estimate and confidence
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
  
  # compute E-values
  E = sapply( values, FUN = function(x) threshold( x, true = true ) )
  
  
  # clean up CI reporting
  # if CI crosses null, set its E-value to 1
  if ( !is.na(null.CI) & null.CI == TRUE ){
    E[ 2:3 ] = 1
    message("Confidence interval crosses the true value, so its E-value is 1.") 
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
  
  result
}


#' Estimate risk ratio and compute CI limits from two-by-two table
#'
#' Given counts in a two-by-two table, computes risk ratio and confidence interval limits.  
#' @param n11 Number exposed (X=1) and diseased (D=1)
#' @param n10 Number exposed (X=1) and not diseased (D=0)
#' @param n01 Number unexposed (X=0) and diseased (D=1)
#' @param n00 Number unexposed (X=0) and not diseased (D=0)
#' @param alpha Alpha level associated with confidence interval
#' @export
#' @import
#' stats
#' @examples
#' # Hammond and Holl (1958 JAMA) Data
#' # Two by Two Table
#' #          Lung Cancer    No Lung Cancer
#'# Smoker    397            78557
#'# Nonsmoker 51             108778
#'
#'twoXtwoRR(397, 78557, 51, 108778)

twoXtwoRR = function(n11, n10, n01, n00, alpha = 0.05)
{
  p1     = n11/(n11 + n10)
  p0     = n01/(n01 + n00)
  RR     = p1/p0
  logRR  = log(RR)
  
  selogRR  = sqrt( 1/n11 - 1/(n11+n10) + 1/n01 - 1/(n01+n00) )
  q.alpha  = qnorm(1 - alpha/2)
  
  upperRR  = exp(logRR + q.alpha*selogRR)
  lowerRR  = exp(logRR - q.alpha*selogRR)
  
  res         = c(RR, upperRR, lowerRR)
  names(res)  = c("point", "upper", "lower")
  
  return( res  ) 
}




#' Compute E-value for single value of risk ratio
#' 
#' Computes E-value for a single value of the risk ratio. Users should typically call the 
#' relevant \code{evalues.XX()} function rather than this internal function.  
#' @param x The risk ratio
#' @param true The true RR to which to shift the observed point estimate. Typically set to 1 to consider a null true effect. 
#' @export
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


threshold = function(x, true = 1) {
  
  if ( is.na(x) ) return(NA)
  
  if(x < 0){
    warning("The risk ratio must be non-negative.")
  }  
  
  if(x <= 1){
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







#' Compute E-value for a population-standardized risk difference and its confidence interval limits
#' 
#' Returns E-values for the point estimate and the lower confidence interval limit for a positive risk difference. 
#' If the risk difference is negative, the exposure coding should be first be reversed to yield a positive risk difference. 
#' @param n11 Number of exposed, diseased individuals
#' @param n10 Number of exposed, non-diseased individuals
#' @param n01 Number of unexposed, diseased individuals
#' @param n00 Number of unexposed, non-diseased individuals
#' @param true True value of risk difference to which to shift the point estimate. Usually set to 0 to consider the null. 
#' @param alpha Alpha level
#' @param grid Spacing for grid search of E-value
#' @export
#' @import
#' stats
#' graphics
#' @examples
#' 
#' ## example 1     
#' ## Hammond and Holl (1958 JAMA) Data
#' ## Two by Two Table
#' ##          Lung Cancer    No Lung Cancer
#' ##Smoker    397            78557
#' ##Nonsmoker 51             108778
#' 
#' # E-value to shift observed risk difference to 0
#' evalues.RD( 397, 78557, 51, 108778)
#' 
#' # E-values to shift observed risk difference to other null values
#' evalues.RD( 397, 78557, 51, 108778, true = 0.001)


evalues.RD = function(n11, n10, n01, n00,  
                      true = 0, alpha = 0.05, grid = 0.0001) {
  
  # sanity check
  if ( any( c(n11, n10, n01, n00) < 0 ) ) stop("Negative cell counts are impossible.")
  
  # sample sizes
  N = n10 + n11 + n01 + n00
  N1 = n10 + n11  # total X=1
  N0 = n00 + n01  # total X=0
  
  # compute f = P( X = 1 )
  f = N1 / N
  
  # P( D = 1 | X = 1 )
  p1  = n11 / N1
  
  # P( D = 1 | X = 0 )
  p0  = n01 / N0
  
  if(p1 < p0) stop("RD < 0; please relabel the exposure such that the risk difference > 0.")
  
  
  # standard errors
  se.p1 = sqrt( p1 * (1-p1) / N1 )
  se.p0 = sqrt( p0 * (1-p0) / N0 )
  
  # back to Peng's code
  s2.f   = f*(1-f)/N
  s2.p1  = se.p1^2
  s2.p0  = se.p0^2
  diff   = p0*(1-f) - p1*f
  
  # bias factor and E-value for point estimate
  est.BF = ( sqrt( (true + diff)^2 + 4 * p1 * p0 * f * (1-f)  ) - (true + diff) ) / ( 2 * p0 * f )
  est.Evalue    = threshold(est.BF)   
  if( p1 - p0 <= true ) stop("For risk difference, true value must be less than or equal to point estimate.")
  
  # compute lower CI limit
  Zalpha        = qnorm(1-alpha/2)  # critical value
  lowerCI       = p1 - p0 - Zalpha*sqrt(s2.p1 + s2.p0)
  
  # check if CI contains null
  if ( lowerCI <= true ) {
    
    # warning("Lower CI limit of RD is smaller than or equal to true value.")
    return( list( est.Evalue = est.Evalue, lower.Evalue = 1 ) )
    
  } else {
    # find E-value for lower CI limit
    # we know it's less than or equal to E-value for point estimate
    BF.search = seq(1, est.BF, grid)
    
    # population-standardized risk difference
    RD.search = p1 - p0 * BF.search
    f.search  = f + (1-f)/BF.search
    
    # using equation for RD^true on pg 376, compute the lower CI limit for these parameters
    # RD.search * f.search is exactly the RHS of the inequality for RD^true (population)
    Low.search = RD.search * f.search -
      Zalpha * sqrt( (s2.p1 + s2.p0 * BF.search^2 ) * f.search^2 +
                       RD.search^2 * ( 1 - 1 / BF.search )^2 * s2.f )
    
    # get the first value for BF_u such that the CI limit hits the true value
    Low.ind    = (Low.search <= true)
    Low.no     = which(Low.ind==1)[1]
    lower.Evalue = threshold( BF.search[Low.no] )
    
    
    return(list(est.Evalue   = est.Evalue,
                lower.Evalue = lower.Evalue))
  }
  
}



#' Plot bias factor as function of confounding relative risks
#' 
#' Plots the bias factor required to explain away a provided relative risk. 
#' @param RR The relative risk 
#' @param xmax Upper limit of x-axis. 
#' @export
#' @examples
#' # recreate the plot in VanderWeele and Ding (2017)
#' bias_plot(RR=3.9, xmax=20)

bias_plot = function(RR, xmax) {
  
  x = seq(0, xmax, 0.01)
  
  # MM: reverse RR if it's preventive
  if (RR < 1) RR = 1/RR
  
  plot(x, x, lty = 2, col = "white", type = "l", xaxs = "i", yaxs = "i", xaxt="n", yaxt = "n",
       xlab = expression(RR[EU]), ylab = expression(RR[UD]),
       xlim = c(0,xmax),
       main = "")
  
  x = seq(RR, xmax, 0.01)
  
  y    = RR*(RR-1)/(x-RR)+RR
  
  lines(x, y, type = "l")
  
  
  high = RR + sqrt(RR*(RR-1))
  
  
  points(high, high, pch = 19)
  
  label5 = seq(5, 40, by = 5)
  axis(1, label5, label5, cex.axis = 1)
  axis(2, label5, label5, cex.axis = 1)
  
  g = round( RR + sqrt( RR * (RR - 1) ), 2 )
  label = paste("(", g, ", ", g, ")", sep="")
  
  text(high + 3, high + 1, label)
  
  legend("bottomleft", expression(
    RR[EU]*RR[UD]/(RR[EU]+RR[UD]-1)==RR
  ), 
  lty = 1:2,
  bty = "n")
  
}




############################ META-ANALYSIS FUNCTIONS ############################ 


#' Estimates and inference for sensitivity analyses
#'
#' Computes point estimates, standard errors, and confidence interval bounds
#' for (1) \code{prop}, the proportion of studies with true effect sizes above \code{q} (or below
#' \code{q} for an apparently preventive \code{yr}) as a function of the bias parameters;
#' (2) the minimum bias factor on the relative risk scale (\code{Tmin}) required to reduce to
#' less than \code{r} the proportion of studies with true effect sizes more extreme than
#' \code{q}; and (3) the counterpart to (2) in which bias is parameterized as the minimum
#' relative risk for both confounding associations (\code{Gmin}).
#' @param q True effect size that is the threshold for "scientific significance"
#' @param r For \code{Tmin} and \code{Gmin}, value to which the proportion of large effect sizes is to be reduced
#' @param muB Mean bias factor on the log scale across studies
#' @param sigB Standard deviation of log bias factor across studies
#' @param yr Pooled point estimate (on log scale) from confounded meta-analysis
#' @param vyr Estimated variance of pooled point estimate from confounded meta-analysis
#' @param t2 Estimated heterogeneity (tau^2) from confounded meta-analysis
#' @param vt2 Estimated variance of tau^2 from confounded meta-analysis
#' @param CI.level Confidence level as a proportion
#' @param tail \code{above} for the proportion of effects above \code{q}; \code{below} for
#' the proportion of effects below \code{q}. By default, is set to \code{above} for relative risks
#' above 1 and to \code{below} for relative risks below 1.
#' @export
#' @details
#' To compute all three point estimates (\code{prop, Tmin, and Gmin}) and inference, all
#' arguments must be non-\code{NA}. To compute only a point estimate for \code{prop},
#' arguments \code{r, vyr}, and \code{vt2} can be left \code{NA}. To compute only
#' point estimates for \code{Tmin} and \code{Gmin}, arguments \code{muB, vyr}, and \code{vt2}
#' can be left \code{NA}. To compute inference for all point estimates, \code{vyr} and 
#' \code{vt2} must be supplied. 
#' @import metafor
#' stats 
#' @examples
#' d = metafor::escalc(measure="RR", ai=tpos, bi=tneg,
#' ci=cpos, di=cneg, data=metafor::dat.bcg)
#' 
#' m = metafor::rma.uni(yi= d$yi, vi=d$vi, knha=FALSE,
#'                      measure="RR", method="DL" ) 
#' yr = as.numeric(m$b)  # metafor returns on log scale
#' vyr = as.numeric(m$vb)
#' t2 = m$tau2
#' vt2 = m$se.tau2^2 
#' 
#' # obtaining all three estimators and inference
#' confounded_meta( q=log(0.90), r=0.20, muB=log(1.5), sigB=0.1,
#'                  yr=yr, vyr=vyr, t2=t2, vt2=vt2,
#'                  CI.level=0.95 )
#' 
#' # passing only arguments needed for prop point estimate
#' confounded_meta( q=log(0.90), muB=log(1.5),
#'                  yr=yr, t2=t2, CI.level=0.95 )
#' 
#' # passing only arguments needed for Tmin, Gmin point estimates
#' confounded_meta( q=log(0.90), r=0.20,
#'                  yr=yr, t2=t2, CI.level=0.95 )


confounded_meta = function( q, r=NA, muB=NA, sigB=0,
                            yr, vyr=NA, t2, vt2=NA,
                            CI.level=0.95, tail=NA ) {
  
  # somewhere have option to plot the bias factor distribution, the confounded distribution, and the adjusted distribution
  
  ##### Check for Bad Input #####
  if ( t2 < 0 ) stop("Heterogeneity cannot be negative")
  if ( sigB < 0 ) stop("Bias factor variance cannot be negative")
  
  # the second condition is needed for Shiny app:
  #  if user deletes the input in box, then it's NA instead of NULL
  if ( ! is.na(vyr) ) {
    if (vyr < 0) stop("Variance of point estimate cannot be negative")
  }
    
  if ( ! is.na(vt2) ) {
    if (vt2 < 0) stop("Variance of heterogeneity cannot be negative")
  }
  
  if ( ! is.na(r) ) {
    if (r < 0 | r > 1) stop("r must be between 0 and 1")
  }
    
  if ( t2 <= sigB^2 ) stop("Must have t2 > sigB^2")


  ##### Messages When Not All Output Can Be Computed #####
  if ( is.na(vyr) | is.na(vt2) ) message("Cannot compute inference without vyr and vt2. Returning only point estimates.")
  if ( is.na(r) ) message("Cannot compute Tmin or Gmin without r. Returning only prop.")
  
  ##### Point Estimates: Causative Case #####
  
  # if tail isn't provided, assume user wants the more extreme one (away from the null)
  if ( is.na(tail) ) tail = ifelse( yr > log(1), "above", "below" )
  
  # bias-corrected mean depends on whether yr is causative, NOT on the desired tail
  if ( yr > log(1) ) yr.corr = yr - muB
  else yr.corr = yr + muB
  
  if ( tail == "above" ) {
    
    if ( !is.na(muB) ) {
      # prop above
      Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
      phat = 1 - pnorm(Z) 
    } else {
      phat = NA
    }
    
    if ( !is.na(r) ) {

      # min bias factor
      # the max is there in case no bias is needed
      # (i.e., proportion of effects > q already < r without confounding)
      Tmin = max( 1, exp( qnorm(1-r) * sqrt(t2) - q + yr ) )
      
      # min confounding strength
      # suppress warnings to avoid warnings about NaN when term inside sqrt is negative
      Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
    } else {
      Tmin = Gmin = NA
    }
  }
  
  ##### Point Estimates: Preventive Case #####
  else if ( tail == "below" ) {
    
    if ( !is.na(muB) ) {
      # prop below
      Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
      phat = pnorm(Z) 
    } else {
      phat = NA
    }
    
    if ( !is.na(r) ) {
      # min bias factor
      Tmin = exp( q - yr - qnorm(r) * sqrt(t2) )
      
      # min confounding strength
      Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
    } else {
      Tmin = Gmin = NA
    }
  }
  
  
  ##### Delta Method Inference: P-Hat #####
  # do inference only if given needed SEs
  if ( !is.na(vyr) & !is.na(vt2) & !is.na(muB) ){
    
    # term in numerator depends on whether causative or preventive RR
    num.term = ifelse( yr > log(1), q + muB - yr, q - muB - yr )
    
    term1.1 = vyr / (t2 - sigB^2 )
    term1.2 = ( vt2 * (num.term)^2 ) / ( 4 * (t2 - sigB^2 )^3 )
    term1 = sqrt( term1.1 + term1.2 )
    
    Z = num.term / sqrt( t2 - sigB^2 )
    SE = term1 * dnorm(Z)
    
    # confidence interval
    tail.prob = ( 1 - CI.level ) / 2
    lo.phat = max( 0, phat + qnorm( tail.prob )*SE )
    hi.phat = min( 1, phat - qnorm( tail.prob )*SE )
    
    # warn if bootstrapping needed
    if ( phat < 0.15 | phat > 0.85 ) warning("Phat is close to 0 or 1. We recommend using bias-corrected and accelerated bootstrapping to estimate all inference in this case.")
    
  } else {
    SE = lo.phat = hi.phat = NA
  }
  
  ##### Delta Method Inference: Tmin and Gmin #####
  # do inference only if given needed SEs and r
  if ( !is.na(vyr) & !is.na(vt2) & !is.na(r) ){
    
    ##### Tmin #####
    if (yr > log(1) ) {
      term = ( vt2 * qnorm(1-r)^2 ) / ( 4 * t2 )
      SE.T = exp( sqrt(t2) * qnorm(1-r) - q + yr ) * sqrt( vyr + term  )
    } else {
      term = ( vt2 * qnorm(r)^2 ) / ( 4 * t2 )
      SE.T = exp( q - yr - sqrt(t2) * qnorm(r) ) * sqrt( vyr + term  )
    }
    
    tail.prob = ( 1 - CI.level ) / 2
    lo.T = max( 1, Tmin + qnorm( tail.prob )*SE.T )  # bias factor can't be < 1
    hi.T = Tmin - qnorm( tail.prob )*SE.T  # but has no upper bound
    
    
    ##### Gmin #####
    SE.G = SE.T * ( 1 + ( 2*Tmin - 1 ) / ( 2 * sqrt( Tmin^2 - Tmin ) ) )
    
    lo.G = max( 1, Gmin + qnorm( tail.prob )*SE.G )  # confounding RR can't be < 1
    hi.G = Gmin - qnorm( tail.prob )*SE.G  # but has no upper bound
    
  } else {  # i.e., user didn't pass parameters needed for inference
    SE.T = SE.G = lo.T = lo.G = hi.T = hi.G = NA
  }
  
  
  # return results
  res = data.frame( Value = c("Prop", "Tmin", "Gmin"), 
                    Est = c( phat, Tmin, Gmin ),
                    SE = c(SE, SE.T, SE.G),
                    CI.lo = c(lo.phat, lo.T, lo.G), 
                    CI.hi = c(hi.phat, hi.T, hi.G) 
  )
  
  return(res)
}







#' Tables for sensitivity analyses
#'
#' Produces table showing the proportion of true effect sizes more extreme than \code{q}
#' across a grid of bias parameters \code{muB} and \code{sigB} (for \code{meas == "prop"}).
#' Alternatively, produces a table showing the minimum bias factor (for \code{meas == "Tmin"})
#' or confounding strength (for \code{meas == "Gmin"}) required to reduce to less than
#' \code{r} the proportion of true effects more extreme than \code{q}.
#' @param meas \code{prop}, \code{Tmin}, or \code{Gmin}
#' @param q True effect size that is the threshold for "scientific significance"
#' @param r For \code{Tmin} and \code{Gmin}, vector of values to which the proportion of large effect sizes is to be reduced
#' @param muB Mean bias factor on the log scale across studies
#' @param sigB Standard deviation of log bias factor across studies
#' @param yr Pooled point estimate (on log scale) from confounded meta-analysis
#' @param t2 Estimated heterogeneity (tau^2) from confounded meta-analysis
#' @keywords meta-analysis, confounding, sensitivity
#' @export
#' @details
#' For \code{meas=="Tmin"} or \code{meas=="Gmin"}, arguments \code{muB} and
#' \code{sigB} can be left \code{NA}; \code{r} can also be \code{NA} as
#' it will default to a reasonable range of proportions. Returns a \code{data.frame}
#' whose rows are values of \code{muB} (for \code{meas=="prop"}) or of \code{r} 
#' (for \code{meas=="Tmin"} or \code{meas=="Gmin"}). Its columns are values of 
#' \code{sigB} (for \code{meas=="prop"}) or of \code{q} (for \code{meas=="Tmin"}
#' or \code{meas=="Gmin"}).
#' Tables for \code{Gmin} will display \code{NaN} for cells corresponding to \code{Tmin}<1,
#' i.e., for which no bias is required to reduce the effects as specified. 
#' 
#' @import ggplot2 
#' @examples
#' sens_table( meas="prop", q=log(1.1), muB=c( log(1.1),
#' log(1.5), log(2.0) ), sigB=c(0, 0.1, 0.2), 
#' yr=log(2.5), t2=0.1 )
#' 
#' sens_table( meas="Tmin", q=c( log(1.1), log(1.5) ),
#' yr=log(1.3), t2=0.1 ) 
#' 
#' # Tmin is 1 here because we already have <80% of effects
#' #  below log(1.1) even without any confounding
#' sens_table( meas="Gmin", r=0.8, q=c( log(1.1) ),
#' yr=log(1.3), t2=0.1 )


sens_table = function( meas, q, r=seq(0.1, 0.9, 0.1), muB=NA, sigB=NA,
                       yr, t2 ) {
  

  ##### Check for Correct Inputs Given Measure ######
  if ( meas=="prop" & ( any( is.na(muB) ) | any( is.na(sigB) ) ) ) {
    stop( "To compute proportion above q, provide muB and sigB with no NA values")
  }
  
  if ( meas=="prop" & length(q) > 1 ) {
    stop( "To compute proportion above q, provide only a single value of q" )
  }
  
  ###### Generate Table #####
  
  # table skeleton
  nrow = ifelse( meas=="prop", length(muB), length(r) )
  ncol = ifelse( meas=="prop", length(sigB), length(q) )
  
  m = matrix( NA, nrow=nrow, ncol=ncol )
  
  # fill in each cell individually
  # doing this inefficient thing because confounded_meta is not vectorized
  # because it returns a dataframe
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      if ( meas == "prop" ) {
        m[i,j] = suppressMessages( confounded_meta( q=q, muB = muB[i], sigB = sigB[j],
                                  yr=yr, t2=t2 )[1,"Est"] )
      } else if ( meas == "Tmin" ) {
        m[i,j] = suppressMessages( confounded_meta( q=q[j], r=r[i],
                                  yr=yr, t2=t2 )[2,"Est"] )
      } else if ( meas == "Gmin" ) {
        m[i,j] = suppressMessages( confounded_meta( q=q[j], r=r[i],
                                  yr=yr, t2=t2 )[3,"Est"] )
      }
      
    }
  }
  
  d = data.frame(m)
  
  if ( meas=="prop" ) {
    row.names(d) = paste( "muB=", round( muB, 3 ), sep="" )
  } else if ( meas %in% c( "Tmin", "Gmin" ) ) {
    row.names(d) = round( r, 3 )
  }
  
  if( meas=="prop" ) {
    names(d) = paste( "sigB=", round( sigB, 3 ), sep="" )  
  } else if ( meas %in% c( "Tmin", "Gmin" ) ) {
    names(d) = round( q, 3 )
  }
  
  return(d)
}




#' Plots for sensitivity analyses
#'
#' Produces line plots (\code{type="line"}) showing the bias factor on the relative risk (RR) scale vs. the proportion
#' of studies with true RRs above \code{q} (or below it for an apparently preventive relative risk).
#' The plot secondarily includes a X-axis scaled based on the minimum strength of confounding
#' to produce the given bias factor. The shaded region represents a 95\% pointwise confidence band.
#' Alternatively, produces distribution plots (\code{type="dist"}) for a specific bias factor showing the observed and 
#' true distributions of RRs with a red line marking exp(\code{q}).
#' @param type \code{dist} for distribution plot; \code{line} for line plot (see Details)
#' @param q True effect size that is the threshold for "scientific significance"
#' @param muB Single mean bias factor on log scale (only needed for distribution plot)
#' @param Bmin Lower limit of lower X-axis on the log scale (only needed for line plot)
#' @param Bmax Upper limit of lower X-axis on the log scale (only needed for line plot)
#' @param sigB Standard deviation of log bias factor across studies (length 1)
#' @param yr Pooled point estimate (on log scale) from confounded meta-analysis
#' @param vyr Estimated variance of pooled point estimate from confounded meta-analysis
#' @param t2 Estimated heterogeneity (tau^2) from confounded meta-analysis
#' @param vt2 Estimated variance of tau^2 from confounded meta-analysis
#' @param breaks.x1 Breaks for lower X-axis (bias factor) on RR scale (optional for line plot; not used for distribution plot)
#' @param breaks.x2 Breaks for upper X-axis (confounding strength) on RR scale (optional for line plot; not used for distribution plot)
#' @param CI.level Pointwise confidence level as a proportion
#' @keywords meta-analysis, confounding, sensitivity
#' @details
#' Arguments \code{vyr} and \code{vt2} can be left \code{NA}, in which case no confidence
#' band will appear on the line plot. 
#' @export
#' @import ggplot2 
#' @examples
#' # with variable bias and with confidence band
#' sens_plot( type="line", q=log(1.1), Bmin=log(1), Bmax=log(4), sigB=0.1,
#'            yr=log(1.3), vyr=0.005, t2=0.4, vt2=0.03 )
#' 
#' # with fixed bias and without confidence band
#' sens_plot( type="line", q=log(1.1), Bmin=log(1), Bmax=log(4),
#'            yr=log(1.3), t2=0.4 )
#' 
#' # apparently preventive
#' sens_plot( type="line", q=log(0.90), Bmin=log(1), Bmax=log(4),
#'            yr=log(0.6), vyr=0.005, t2=0.4, vt2=0.04 )
#' 
#' # distribution plot: apparently causative
#' # commented out because takes 5-10 seconds to run
#' # sens_plot( type="dist", q=log(1.1), muB=log(2),
#' #           yr=log(1.3), t2=0.4 )
#'            
#' # distribution plot: apparently preventive
#' # commented out because takes 5-10 seconds to run
#' # sens_plot( type="dist", q=log(0.90), muB=log(1.5),
#' #           yr=log(0.7), t2=0.2 )


sens_plot = function( type, q, muB=NA, Bmin=log(1), Bmax=log(5), sigB=0,
                      yr, vyr=NA, t2, vt2=NA,
                      breaks.x1=NA, breaks.x2=NA,
                      CI.level=0.95 ) {
  
  ##### Check for Bad Input ######
  if ( type=="dist" ) {
    
    if( is.na(muB) ) stop("For type='dist', must provide muB")
    
    if ( ( length(muB) > 1 ) | ( length(sigB) > 1 ) ) {
      stop( "For type='dist', muB and sigB must be length 1")
    }
  }
  
  if ( type=="line" ) {
    
    if ( is.na(vyr) | is.na(vt2) ) {
      message( "No confidence interval because vyr or vt2 is NULL")
    }
  }
  
  ##### Distribution Plot ######
  if ( type=="dist" ) {
    
    # simulate confounded distribution
    reps = 10000
    RR.c = exp( rnorm( n=reps, mean=yr, sd=sqrt(t2) ) )
    
    # simulate unconfounded distribution
    Mt = ifelse( yr > 0, yr - muB, yr + muB )
    RR.t = exp( rnorm( n=reps, mean=Mt, sd=sqrt(t2-sigB^2) ) )
    
    # get reasonable limits for X-axis
    x.min = min( quantile(RR.c, 0.01), quantile(RR.t, 0.01) )
    x.max = max( quantile(RR.c, 0.99), quantile(RR.t, 0.99) )
    
    temp = data.frame( group = rep( c( "Observed", "True" ), each = reps ), 
                       val = c( RR.c, RR.t ) )
    
    colors=c("black", "orange")
    p = ggplot2::ggplot( data=temp, aes(x=temp$val, group=temp$group ) ) +
      geom_density( aes( fill=temp$group ), alpha=0.4 ) +
      theme_bw() + xlab("Study-specific relative risks") +
      ylab("") + guides(fill=guide_legend(title=" ")) +
      scale_fill_manual(values=colors) +
      geom_vline( xintercept = exp(q), lty=2, color="red" ) +
      scale_x_continuous( limits=c(x.min, x.max), breaks = seq( round(x.min), round(x.max), 0.5) ) +
      ggtitle("Observed and true relative risk distributions")
    
    graphics::plot(p)
  }
  
  ##### Line Plot ######
  if ( type=="line" ) {
    # get mean bias factor values for a bunch of different B's
    t = data.frame( B = seq(Bmin, Bmax, .01), phat = NA, lo = NA, hi = NA )
    t$eB = exp(t$B)
    
    for ( i in 1:dim(t)[1] ) {
      # r is irrelevant here
      cm = confounded_meta(q, r=0.10, muB=t$B[i], sigB,
                           yr, vyr, t2, vt2,
                           CI.level=CI.level)
      t$phat[i] = cm$Est[ cm$Value=="Prop" ]
      t$lo[i] = cm$CI.lo[ cm$Value=="Prop" ]
      t$hi[i] = cm$CI.hi[ cm$Value=="Prop" ]
    }
    
    # compute values of g for the dual X-axis
    if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
    if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
    
    # define transformation in a way that is monotonic over the effective range of B (>1)
    # to avoid ggplot errors
    g = Vectorize( function(x) {
      if (x < 1) return( x / 1e10 )
      x + sqrt( x^2 - x )
    } )
    
    p = ggplot2::ggplot( t, aes(x=t$eB, y=t$phat ) ) + theme_bw() +
      scale_y_continuous( limits=c(0,1),
                          breaks=seq(0, 1, .1) ) +
      scale_x_continuous(  limits = c(min(breaks.x1), 
                                      max(breaks.x1)),
                           breaks = breaks.x1,
                           sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                                name = "Minimum strength of both confounding RRs",
                                                breaks=breaks.x2 ) ) +
      geom_line(lwd=1.2) +
      xlab("Bias factor (RR scale)") +
      ylab( paste( ifelse( yr > log(1),
                           paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                           paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
    
    # can't compute a CI if the bounds aren't there
    no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) )
    
    if ( no.CI ) graphics::plot(p)
    else p + ggplot2::geom_ribbon( aes(ymin=t$lo, ymax=t$hi), alpha=0.15 )   
    
  }
}



#' Convert forest plot or summary table to meta-analytic dataset
#'
#' This function is now deprecated. You should use the improved version \code{MetaUtility::scrape_meta} instead.
#' @param type \code{RR} if point estimates are RRs or ORs (to be handled on log scale); \code{raw} if point estimates are raw differences, standardized mean differences, etc. (such that they can be handled with no transformations)
#' @param est Vector of study point estimates on RR or OR scale
#' @param hi Vector of upper bounds of 95\% CIs on RRs
#' @param sqrt Vector of booleans (TRUE/FALSE) for whether each study measured an odds ratio of a common outcome that should be approximated as a risk ratio via the square-root transformation
#' @export
#' @import stats

scrape_meta = function( type="RR", est, hi, sqrt=FALSE ){
  
  .Deprecated("MetaUtility::scrape_meta")
  
  # if ( type == "RR" ) {
  #   # take square root for certain elements
  #   RR = est
  #   RR[sqrt] = sqrt( RR[sqrt] )
  #   
  #   # same for upper CI limit
  #   hi.RR = hi
  #   hi.RR[sqrt] = sqrt( hi.RR[sqrt] )
  #   
  #   sei = ( log(hi.RR) - log(RR) ) / qnorm(.975)
  #   
  #   return( data.frame( yi = log(RR), vyi = sei^2 ) )
  #   
  # } else if ( type == "raw" ) {
  #   
  #   sei = ( hi - est ) / qnorm(.975)
  #   return( data.frame( yi = est, vyi = sei^2 ) )
  # }
}



#' Estimate proportion of population effect sizes above or below a threshold
#'
#' Estimates the proportion of true effect sizes in a meta-analysis above or below
#' a specified threshold of scientific importance. Effect sizes may be of any type (they need not
#' be relative risks). This is a wrapper for \code{confounded_meta}; it is the special case in
#' which there is no unmeasured confounding. 
#' @param q True effect size that is the threshold for "scientific importance"
#' @param yr Pooled point estimate from meta-analysis
#' @param vyr Estimated variance of pooled point estimate from meta-analysis
#' @param t2 Estimated heterogeneity (tau^2) from meta-analysis
#' @param vt2 Estimated variance of tau^2 from meta-analysis
#' @param CI.level Confidence level as a proportion
#' @param tail \code{above} for the proportion of effects above \code{q}; \code{below} for
#' the proportion of effects below \code{q}.
#' @export

stronger_than = function( q, yr, vyr=NA, t2, vt2=NA,
                          CI.level=0.95, tail ) {
  
  # suppress warnings about lack of info for doing sensitivity analysis
  # since we are not dealing with confounding 
  cm = suppressMessages( confounded_meta( q = q, muB = 0, sigB = 0,
                                          yr = yr, vyr = vyr,
                                          t2 = t2, vt2 = vt2,
                                          CI.level = CI.level,
                                          tail = tail ) )
  
  # return just the first row (proportion) since the rest are for sensitivity analyses
  return( cm[1,] ) 
}





