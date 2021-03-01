
############################ MAIN FUNCTIONS ############################ 


#' Sensitivity analysis for unmeasured confounding in meta-analyses
#' 
#' This function implements the sensitivity analyses of Mathur & VanderWeele (2020a, 2020b). It computes point estimates, standard errors, and confidence intervals
#' for (1) \code{Prop}, the proportion of studies with true causal effect sizes above or below a chosen threshold \code{q} as a function of the bias parameters;
#' (2) the minimum bias factor on the relative risk scale (\code{Tmin}) required to reduce to
#' less than \code{r} the proportion of studies with true causal effect sizes more extreme than
#' \code{q}; and (3) the counterpart to (2) in which bias is parameterized as the minimum
#' relative risk for both confounding associations (\code{Gmin}).
#' 
#' @param method \code{"calibrated"} or \code{"parametric"}. See Details. 
#' 
#' @param q True causal effect size chosen as the threshold for a meaningfully large effect.
#' 
#' @param r For \code{Tmin} and \code{Gmin}, value to which the proportion of meaningfully strong effect sizes is to be reduced.
#' 
#' @param tail \code{"above"} for the proportion of effects above \code{q}; \code{"below"} for
#' the proportion of effects below \code{q}. By default, is set to \code{"above"} if the pooled point estimate (\code{method = "parametric"}) or median of the calibrated estimates (\code{method = "calibrated"}) is above 1 on the relative risk scale and is set to \code{"below"} otherwise.
#' 
#' @param CI.level Confidence level as a proportion (e.g., 0.95).
#' 
#' @param give.CI Logical. If \code{TRUE}, confidence intervals are provided. Otherwise, only point estimates are provided.
#' 
#' @param R Number  of  bootstrap  iterates for confidence interval estimation. Only used if \code{method = "calibrated"} and \code{give.CI = TRUE}. 
#' 
#' @param muB Mean bias factor on the log scale across studies (greater than 0). When considering bias that is of homogeneous strength across studies (i.e., \code{method = "calibrated"} or \code{method = "parametric"} with \code{sigB = 0}), \code{muB} represents the log-bias factor in each study. If \code{muB} is not specified, then only \code{Tmin} and \code{Gmin} will be returned, not \code{Prop}. 
#' 
#' @param muB.toward.null Whether you want to consider bias that has on average shifted studies' point estimates away from the null (\code{FALSE}; the default) or that has on average shifted studies' point estimates toward the null (\code{TRUE}). See Details.
#' 
#' @param dat Dataframe containing studies' point estimates and variances. Only used if \code{method = "calibrated"}.
#' @param yi.name Name of variable in \code{dat} containing studies' point estimates on the log-relative risk scale. Only used if \code{method = "calibrated"}.
#' @param vi.name Name of variable in \code{dat} containing studies' variance estimates. Only used if \code{method = "calibrated"}.
#' 
#' @param sigB Standard deviation of log bias factor across studies. Only used if \code{method = "parametric"}.
#' 
#' @param yr Pooled point estimate (on log-relative risk scale) from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param vyr Estimated variance of pooled point estimate from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param t2 Estimated heterogeneity (\eqn{\tau^2}) from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param vt2 Estimated variance of \eqn{\tau^2} from confounded meta-analysis. Only used if \code{method = "parametric"}.
#' 
#' @param \dots Additional arguments passed to \code{confounded_meta}.
#' 
#' @export
#' @details
#' ## Specifying the sensitivity parameters on the bias
#' By convention, the average log-bias factor, \code{muB}, is taken to be greater than 0 (Mathur & VanderWeele, 2020a; Ding & VanderWeele, 2017). Confounding can operate on average either away from or toward the null, a choice specified via \code{muB.toward.null}. The most common choice for sensitivity analysis is to consider bias that operates on average away from the null, which is \code{confounded_meta}'s default. In such an analysis, correcting for the bias involves shifting studies' estimates back toward the null by \code{muB} (i.e., if \code{yr > 0}, the estimates will be corrected downward; if \code{yr < 0}, they will be corrected upward). Alternatively, to consider bias that operates on average away from the null, you would still specify \code{muB > 0} but would also specify \code{muB.toward.null = TRUE}. For detailed guidance on choosing the sensitivity parameters \code{muB} and \code{sigB}, see Section 5 of Mathur & VanderWeele (2020a).
#' 
#' ## Specifying the threshold \code{q}
#' For detailed guidance on choosing the threshold \code{q}, see the Supplement of Mathur & VanderWeele (2020a).   
#' 
#' ## Specifying the estimation method
#' By default, \code{confounded_meta} performs estimation using a \strong{calibrated method} (Mathur & VanderWeele, 2020b) that extends work by Wang et al. (2019). This method makes no assumptions about the distribution of population effects and performs well in meta-analyses with as few as 10 studies, and performs well even when the proportion being estimated is close to 0 or 1. However, it only accommodates bias whose strength is the same in all studies (homogeneous bias). When using this method, the following arguments need to be specified:
#' \itemize{
#'  \item \code{q}
#'  \item \code{r} (if you want to estimate \code{Tmin} and \code{Gmin})
#'  \item \code{muB}
#'  \item \code{dat}
#'  \item \code{yi.name}
#'  \item \code{vi.name}
#' }
#' 
#' The \strong{parametric method} assumes that the population effects are approximately normal and that the number of studies is large. Parametric confidence intervals should only be used when the proportion estimate is between 0.15 and 0.85 (and \code{confounded_meta} will issue a warning otherwise). Unlike the calibrated method, the parametric method can accommodate bias that is heterogeneous across studies (specifically, bias that is log-normal across studies). When using this method, the following arguments need to be specified:
#' \itemize{
#' \item \code{q}
#' \item \code{r} (if you want to estimate \code{Tmin} and \code{Gmin})
#' \item \code{muB}
#' \item \code{sigB}
#' \item \code{yr}
#' \item \code{vyr} (if you want confidence intervals)
#' \item \code{t2}
#' \item \code{vt2} (if you want confidence intervals)
#' }
#' 
#' 
#' ## Effect size measures other than log-relative risks
#' If your meta-analysis uses effect sizes other than log-relative risks, you should first approximately convert them to log-relative risks, for example via [EValue::convert_measures()] and then pass the converted point estimates or meta-analysis estimates to \code{confounded_meta}. 
#' 
#' ## Interpreting \code{Tmin} and \code{Gmin} 
#' \code{Tmin} is defined as the minimum average bias factor on the relative risk scale that would be required to reduce to less than \code{r} the proportion of studies with true causal effect sizes stronger than the threshold \code{q}, assuming that the bias factors are log-normal across studies with standard deviation \code{sigB}. \code{Gmin} is defined as the minimum confounding strength on the relative risk scale -- that is, the relative risk relating unmeasured confounder(s) to both the exposure and the outcome -- on average among the meta-analyzed studies, that would be required to reduce to less than \code{r} the proportion of studies with true causal effect sizes stronger than the threshold \code{q}, again assuming that bias factors are log-normal across studies with standard deviation \code{sigB}. \code{Gmin} is a one-to-one transformation of \code{Tmin} given by \eqn{Gmin = Tmin + \sqrt{Tmin * (Tmin - 1)} }. If the estimated proportion of meaningfully strong effect sizes is already less than \code{r} even without the introduction of any bias, \code{Tmin} and \code{Gmin} will be set to 1. (These definitions of \code{Tmin} and \code{Gmin} are generalizations of those given in Mathur & VanderWeele, 2020a, who defined these quantities in terms of bias that is homogeneous across studies. You can conduct analyses with homogeneous bias by setting \code{sigB = 0}.)
#' 
#' The direction of bias represented by \code{Tmin} and \code{Gmin} is dependent on the argument \code{tail}: when \code{tail = "above"}, these metrics consider bias that had operated to \emph{increase} studies' point estimates, and when \code{tail = "below"}, these metrics consider bias that had operated to \emph{decrease} studies' point estimates. Such bias could operate toward or away from the null depending on whether the pooled point estimate \code{yr} happens to fall above or below the null. As such, the direction of bias represented by \code{Tmin} and \code{Gmin} may or may not match that specified by the argument \code{muB.toward.null} (which is used only for estimation of \code{Prop}).
#' 
#' ## When these methods should be used
#' These methods perform well only in meta-analyses with at least 10 studies; we do not recommend reporting them in smaller meta-analyses. Additionally, it only makes sense to consider proportions of effects stronger than a threshold when the heterogeneity estimate \code{t2} is greater than 0. For meta-analyses with fewer than 10 studies or with a heterogeneity estimate of 0, you can simply report E-values for the point estimate via [EValue::evalue()] (VanderWeele & Ding, 2017; see Mathur & VanderWeele (2020a), Section 7.2 for interpretation in the meta-analysis context).
#'  
#' 
#' @keywords meta-analysis
#' @import
#' metafor
#' stats 
#' MetaUtility
#' boot
#' 
#' @references
#' Mathur MB & VanderWeele TJ (2020a). Sensitivity analysis for unmeasured confounding in meta-analyses. \emph{Journal of the American Statistical Association}.
#' 
#' Mathur MB & VanderWeele TJ (2020b). Robust metrics and sensitivity analyses for meta-analyses of heterogeneous effects. \emph{Epidemiology}.
#' 
#' Mathur MB & VanderWeele TJ (2019). New statistical metrics for meta-analyses of heterogeneous effects. \emph{Statistics in Medicine}.
#'
#' Ding P & VanderWeele TJ (2016). Sensitivity analysis without assumptions. \emph{Epidemiology}.
#' 
#' VanderWeele TJ & Ding P (2017). Introducing the E-value. \emph{Annals of Internal Medicine}.
#'
#' Wang C-C & Lee W-C (2019). A simple method to estimate prediction intervals and
#' predictive distributions: Summarizing meta-analyses
#' beyond means and confidence intervals. \emph{Research Synthesis Methods}.
#' @examples
#' 
#' ##### Using Calibrated Method #####
#' d = metafor::escalc(measure="RR", ai=tpos, bi=tneg,
#'                     ci=cpos, di=cneg, data=metafor::dat.bcg)
#' 
#' # obtaining all three estimators and inference
#' # number of bootstrap iterates
#' # should be larger in practice
#' R = 100
#' confounded_meta( method="calibrated",  # for both methods
#'                  q = log(0.90),
#'                  r = 0.20,
#'                  tail="below",
#'                  muB = log(1.5),
#'                  dat = d,
#'                  yi.name = "yi",
#'                  vi.name = "vi",
#'                  R = 100 )
#' 
#' # passing only arguments needed for prop point estimate
#' confounded_meta( method="calibrated",
#'                  q = log(0.90),
#'                  tail="below",
#'                  muB = log(1.5),
#'                  give.CI = FALSE,
#'                  dat = d,
#'                  yi.name = "yi",
#'                  vi.name = "vi" )
#' 
#' # passing only arguments needed for Tmin, Gmin point estimates
#' confounded_meta( method="calibrated",
#'                  q = log(0.90),
#'                  r = 0.10,
#'                  tail="below",
#'                  give.CI = FALSE,
#'                  dat = d,
#'                  yi.name = "yi",
#'                  vi.name = "vi" )
#' 
#' ##### Using Parametric Method #####
#' # fit random-effects meta-analysis
#' m = metafor::rma.uni(yi= d$yi,
#'                      vi=d$vi,
#'                      knha=TRUE,
#'                      measure="RR",
#'                      method="REML" )
#' 
#' yr = as.numeric(m$b)  # metafor returns on log scale
#' vyr = as.numeric(m$vb)
#' t2 = m$tau2
#' vt2 = m$se.tau2^2
#' 
#' # obtaining all three estimators and inference
#' # now the proportion considers heterogeneous bias
#' confounded_meta( method = "parametric",
#'                  q=log(0.90),
#'                  r=0.20,
#'                  tail = "below",
#'                  muB=log(1.5),
#'                  sigB=0.1,
#'                  yr=yr,
#'                  vyr=vyr,
#'                  t2=t2,
#'                  vt2=vt2,
#'                  CI.level=0.95 )
#' 
#' # passing only arguments needed for prop point estimate
#' confounded_meta( method = "parametric",
#'                  q=log(0.90),
#'                  tail = "below",
#'                  muB=log(1.5),
#'                  sigB = 0,
#'                  yr=yr,
#'                  t2=t2,
#'                  CI.level=0.95 )
#' 
#' # passing only arguments needed for Tmin, Gmin point estimates
#' confounded_meta( method = "parametric",
#'                  q = log(0.90),
#'                  sigB = 0,
#'                  r = 0.10,
#'                  tail = "below",
#'                  yr=yr,
#'                  t2=t2,
#'                  CI.level=0.95 )


confounded_meta = function( method="calibrated",  # for both methods
                            q,
                            r = NA,
                            tail = NA,
                            CI.level = 0.95,
                            give.CI = TRUE,
                            R = 1000,
                            
                            muB = NA,
                            muB.toward.null = FALSE,
                            
                            # only for calibrated
                            dat = NA,
                            yi.name = NA,
                            vi.name = NA,
                            
                            # only for parametric
                            sigB = NA,
                            yr = NA,
                            vyr = NA,
                            t2 = NA,
                            vt2 = NA,
                            
                            ...
                          
) {
  
  
  # # test only
  # method="calibrated"
  # q=median(d$calib)
  # tail = "above"
  # muB=0
  # r=0.1
  # q = 0.2
  # R = 250
  # CI.level = 0.95
  # 
  # give.CI=TRUE
  # dat = d
  # yi.name = "yi"
  # vi.name = "vyi"
  
  ##### Look for Additional Passed Arguments #####
  dots = list(...)
  
  # should warnings be simplified for use with the Shiny website?
  if ( "simplifyWarnings" %in% names(dots) ){
    simplifyWarnings =  dots$simplifyWarnings
  } else {
    simplifyWarnings = FALSE
  }
  
  
  ##### Check for Bad or Incomplete Input - Common to Parametric and Calibrated Methods #####
  if ( ! is.na(r) ) {
    if (r < 0 | r > 1) stop("r must be between 0 and 1")
  }
  
  if ( is.na(r) ){
    if ( simplifyWarnings == FALSE ) message("Cannot compute Tmin or Gmin without r. Returning only prop.")
    if ( simplifyWarnings == TRUE ) message("Cannot compute bias or confounding strength required to shift to r unless you provide r. Returning only the proportion.")
  }
  
  if ( !is.na(muB) & muB < 0 ) {
    if ( simplifyWarnings == FALSE ) stop("Must have muB > 0. Use the muB.toward.null argument instead if you want to consider bias away from the null. See Details.")
    
    if ( simplifyWarnings == TRUE ) stop("Must have muB > 0.")
    
  }
  
  
  ##### PARAMETRIC #####
  if (method=="parametric"){
    
    
    ##### Check for Bad Input #####
    if ( t2 < 0 ) stop("Heterogeneity cannot be negative")
    if ( is.na(sigB) ) stop("Must provide sigB for parametric method")
    
    
    # the second condition is needed for Shiny app:
    #  if user deletes the input in box, then it's NA instead of NULL
    if ( !is.na(vyr) ) {
      if (vyr < 0) stop("Variance of point estimate cannot be negative")
    }
    
    if ( !is.na(vt2) ) {
      if (vt2 < 0) stop("Variance of heterogeneity cannot be negative")
    }
    
    if ( !is.na(sigB) ) {
      if ( t2 <= sigB^2 ) {
        if (simplifyWarnings == FALSE) stop("Must have t2 > sigB^2")
        
        # on the website, this can only happen if they set t2=0 but proportion due to confounding > 0
        if (simplifyWarnings == TRUE) stop("Cannot have a nonzero proportion of heterogeneity due to variation in confounding bias when there is no heterogeneity to begin with")
      }
      if ( sigB < 0 ) stop("Bias factor standard deviation cannot be negative")
    }
    
    
    
    ##### Messages When Not All Output Can Be Computed #####
    if ( is.na(vyr) | is.na(vt2) ) message("Cannot compute inference without variances of point estimate and of heterogeneity estimate. Returning only point estimates.")
    
    ##### Point Estimates: Causative Case #####
    # if tail isn't provided, assume user wants the more extreme one (away from the null)
    if ( is.na(tail) ) {
      tail = ifelse( yr > 0, "above", "below" )
      warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
    }
    
    # bias-corrected mean
    # usual case: bias that went away from null, so correction shifts toward null
    if ( muB.toward.null == FALSE & yr > 0 ) yr.corr = yr - muB
    if ( muB.toward.null == FALSE & yr < 0 ) yr.corr = yr + muB
    # less-usual case: bias that went toward null, so correction shifts away from null
    if ( muB.toward.null == TRUE & yr > 0 ) yr.corr = yr + muB
    if ( muB.toward.null == TRUE & yr < 0 ) yr.corr = yr - muB
    
    
    if ( tail == "above" ) {
      
      # point estimate for Phat
      if ( !is.na(muB) & !is.na(sigB) ) {
        # point estimate for Phat
        Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
        Phat = 1 - pnorm(Z) 
      } else {
        Phat = NA
      }
      
      # point estimates for Tmin, Gmin
      if ( !is.na(r) & !is.na(sigB) ) {
        
        # first check if any shifting is actually needed
        # current Phat with no bias
        Phat.naive = 1 - pnorm( (q - yr) / sqrt(t2 - sigB^2) )
        
        if ( Phat.naive <= r ) {
          Tmin = 1
        } else {
          # min bias factor
          # the max is there in case no bias is needed
          # (i.e., the bias would be going in the other direction)
          # (i.e., proportion of effects > q already < r without confounding)
          Tmin = max( 1, exp( qnorm(1-r) * sqrt(t2 - sigB^2) - q + yr ) )
          
          # alternative way of handling this issue:
          # Tmin could be less than 1 if yr has to be shifted POSITIVELY
          #  rather than negatively to achieve r
          #  e.g., yr^c = log(0.5), q = log(1s.5), r = 0.75
          # for consistency with calibrated output, take Tmin's inverse so it's always positive
          #if ( Tmin < 1 ) Tmin = 1 / Tmin
        }
        
        # min confounding strength
        # suppress warnings to avoid warnings about NaN when term inside sqrt is negative
        Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
      }
      
      if ( is.na(r) ) {
        Tmin = Gmin = NA
      }
    } # end tail = "above"
    
    ##### Point Estimates: Preventive Case #####
    if ( tail == "below" ) {
      
      # point estimate for Phat
      if ( !is.na(muB) & !is.na(sigB) ) {
        Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
        Phat = pnorm(Z) 
      } else {
        Phat = NA
      }
     
      # point estimates for Tmin, Gmin
      if ( !is.na(r) & !is.na(sigB) ) {
        
        # first check if any shifting is actually needed
        # current Phat with no bias
        Phat.naive = pnorm( (q - yr) / sqrt(t2 - sigB^2) )
        
        if ( Phat.naive <= r ) {
          Tmin = 1
        } else {
          # the max is there in case no bias is needed
          Tmin = max( 1, exp( q - yr - qnorm(r) * sqrt(t2 - sigB^2) ) )
          
          # alternative way of handling this issue:
          # # Tmin could be less than 1 if yr has to be shifted NEGATIVELY
          # #  rather than positively to achieve r
          # #  e.g., yr^c = log(1.5), q = log(0.5), r = 0.75
          # # for consistency with calibrated output, take Tmin's inverse so it's always positive
          # if ( Tmin < 1 ) Tmin = 1 / Tmin
        }
        
        # min confounding strength
        Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
      }
      
      if ( is.na(r) ) {
        Tmin = Gmin = NA
      }
      
    } # end tail = "below"
    
    ##### Delta Method Inference: P-Hat #####
    # do inference only if given needed SEs
    if ( !is.na(vyr) & !is.na(vt2) & !is.na(muB) & !is.na(sigB) ){
      
      # term in numerator depends tail
      num.term = ifelse( tail == "above", q + muB - yr, q - muB - yr )
      
      term1.1 = vyr / (t2 - sigB^2 )
      term1.2 = ( vt2 * (num.term)^2 ) / ( 4 * (t2 - sigB^2 )^3 )
      term1 = sqrt( term1.1 + term1.2 )
      
      Z = num.term / sqrt( t2 - sigB^2 )
      SE.Phat = term1 * dnorm(Z)
      
      # confidence interval
      tail.prob = ( 1 - CI.level ) / 2
      lo.Phat = max( 0, Phat + qnorm( tail.prob )*SE.Phat )
      hi.Phat = min( 1, Phat - qnorm( tail.prob )*SE.Phat )
      
      
      # warn if bootstrapping needed
      if ( Phat < 0.15 | Phat > 0.85 ) {
        if ( simplifyWarnings == FALSE) warning('Prop is close to 0 or 1. We recommend choosing method = \"calibrated\" or alternatively using bias-corrected and accelerated bootstrapping to estimate all inference in this case.')
        
       # no warning for the simplifyWarnings == TRUE case because website has its own version 
      }
      
    } else {
      SE.Phat = lo.Phat = hi.Phat = NA
    }
    
    ##### Delta Method Inference: Tmin and Gmin #####
    # do inference only if given needed SEs and r
    # last condition: if Tmin has been set to 1, give NAs for inference
    if ( !is.na(vyr) & !is.na(vt2) & !is.na(r) & !is.na(sigB) & Tmin != 1 ){
      
      ##### Tmin #####
      if ( tail == "above" ) {
        term = ( vt2 * qnorm(1-r)^2 ) / ( 4 * (t2-sigB^2) )
        SE.T = exp( sqrt(t2 - sigB^2) * qnorm(1-r) - q + yr ) * sqrt( vyr + term  )
        
      } else {
        term = ( vt2 * qnorm(r)^2 ) / ( 4 * (t2-sigB^2) )
        SE.T = exp( q - yr - sqrt(t2 - sigB^2) * qnorm(r) ) * sqrt( vyr + term  )
      }
      
      tail.prob = ( 1 - CI.level ) / 2
      lo.T = max( 1, Tmin + qnorm( tail.prob )*SE.T )  # bias factor can't be < 1
      hi.T = Tmin - qnorm( tail.prob )*SE.T  # but has no upper bound
      
      
      ##### Gmin #####
      SE.G = SE.T * ( 1 + ( 2*Tmin - 1 ) / ( 2 * sqrt( Tmin^2 - Tmin ) ) )
      
      lo.G = max( 1, Gmin + qnorm( tail.prob )*SE.G )  # confounding RR can't be < 1
      hi.G = Gmin - qnorm( tail.prob )*SE.G  # but has no upper bound
      
    } else {  # i.e., user didn't pass parameters needed for inference, or else Tmin = 1
      SE.T = SE.G = lo.T = lo.G = hi.T = hi.G = NA
    }
    
    
  } # closes parametric method
  
  ##### CALIBRATED #####
  if( method == "calibrated" ){
    
    # no need to catch bad input for this method
    
    # if tail isn't provided, assume user wants the more extreme one (away from the null)
    if ( is.na(tail) ) {
      calib = calib_ests( yi = dat[[yi.name]], 
                          sei = sqrt( dat[[vi.name]] ) )
      
      tail = ifelse( median(calib) > 0, "above", "below" )
      warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
    }
    
    
    # initialize
    Phat = Tmin = Gmin = SE.Phat = SE.T = SE.G = lo.Phat = lo.T = lo.G = hi.Phat = hi.T = hi.G = NA
    
    
    ##### All Three Point Estimates #####
    Phat = Phat_causal( q = q, 
                        B = muB,
                        tail = tail,
                        muB.toward.null = muB.toward.null,
                        dat = dat,
                        yi.name = yi.name,
                        vi.name = vi.name )
    
    if ( !is.na(r) ) {
      Tmin = Tmin_causal(q = q,
                         r = r,
                         tail = tail,
                         dat = dat,
                         yi.name = yi.name,
                         vi.name = vi.name)
      
      
      Gmin = g(Tmin)
    }
    
    
    
    ##### All Three Confidence Intervals #####
    if ( give.CI == TRUE ) {
      
      # check for needed input
      # use length(dat) instead of is.na(dat) because latter will 
      if ( all(is.na(dat)) | is.na(yi.name) | is.na(vi.name) ) {
        stop("Must provide dat, yi.name, and vi.name to calculate confidence intervals with calibrated method")
      }
      
      Phat.CI.lims = Phat_CI_lims(.B = muB,
                                  R = R,
                                  q = q,
                                  tail = tail,
                                  dat = dat,
                                  muB.toward.null = muB.toward.null,
                                  yi.name = yi.name,
                                  vi.name = vi.name,
                                  CI.level = CI.level)
      
      lo.Phat = as.numeric( Phat.CI.lims[1] )
      hi.Phat = as.numeric( Phat.CI.lims[2] )
      SE.Phat = as.numeric( Phat.CI.lims[3] )
      
      if ( any( is.na( c(lo.Phat, hi.Phat, SE.Phat) ) ) ) {
        message("The confidence interval and/or standard error for the proportion were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold.")
      }
      
      Tmin.Gmin.CI.lims = Tmin_Gmin_CI_lims( R,
                                             q,
                                             r,
                                             tail,
                                             dat,
                                             yi.name,
                                             vi.name,
                                             CI.level )
      
      lo.T = as.numeric( Tmin.Gmin.CI.lims["lo.T"] )
      hi.T = as.numeric( Tmin.Gmin.CI.lims["hi.T"] )
      SE.T = as.numeric( Tmin.Gmin.CI.lims["SE.T"] )
      lo.G = as.numeric( Tmin.Gmin.CI.lims["lo.G"] )
      hi.G = as.numeric( Tmin.Gmin.CI.lims["hi.G"] )
      SE.G = as.numeric( Tmin.Gmin.CI.lims["SE.G"] )
      
      # last condition is because we don't actually do bootstrapping if Tmin = 1
      if ( any( is.na( c(lo.T, hi.T, SE.T, lo.G, hi.G, SE.G) ) ) & ( !is.na(Tmin) & Tmin != 1 ) ) {
        message("The confidence interval and/or standard error for Tmin and Gmin were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold.")
      }
      
    }  # closes "if ( !is.na(r) )"
    
  } # closes calibrated method
  
  ##### Messages about Results #####
  if ( exists("Tmin") ) {
    if ( !is.na(Tmin) & Tmin == 1 ) {
      if (simplifyWarnings == FALSE) message("Prop is already less than or equal to r even with no confounding, so Tmin and Gmin are simply equal to 1. No confounding at all is required to make the specified shift.")
      
      # no warning for website because it produces its own
      #if (simplifyWarnings == TRUE) message("The proportion is already less than or equal to r even with no confounding, so the amount of bias and of confounding strength required to make the specified shift are simply equal to 1. No confounding at all is required to make the specified shift.")
    }
    
    if ( !is.na(Tmin) & muB.toward.null == TRUE ) {
      message("You chose to consider bias that has on average shifted studies' estimates toward the null, rather than away from the null. This specification was applied when estimating Prop. However, because Tmin and Gmin by definition consider the amount of bias required to reduce to less than r the proportion of studies with true causal effect sizes more extreme than q, that bias may be toward or away from the null as required to make the shift.")
    }
  }  
  

  ##### Return Results #####
  return( data.frame( Value = c("Prop", "Tmin", "Gmin"), 
                      Est = c( Phat, Tmin, Gmin ),
                      SE = c(SE.Phat, SE.T, SE.G),
                      CI.lo = c(lo.Phat, lo.T, lo.G), 
                      CI.hi = c(hi.Phat, hi.T, hi.G) ) )
  
} # closes confounded_meta function








#' Plots for sensitivity analyses
#'
#' Produces line plots (\code{type="line"}) showing the average bias factor across studies on the relative risk (RR) scale vs. the estimated proportion
#' of studies with true RRs above or below a chosen threshold \code{q}.
#' The plot secondarily includes a X-axis showing the minimum strength of confounding
#' to produce the given bias factor. The shaded region represents a pointwise confidence band.
#' Alternatively, produces distribution plots (\code{type="dist"}) for a specific bias factor showing the observed and 
#' true distributions of RRs with a red line marking exp(\code{q}).
#' @param method \code{"calibrated"} or \code{"parametric"}. See Details.
#' @param type \code{dist} for distribution plot; \code{line} for line plot (see Details)
#' @param q True causal effect size chosen as the threshold for a meaningfully large effect
#' @param CI.level Pointwise confidence level as a proportion (e.g., 0.95).
#' @param tail \code{"above"} for the proportion of effects above \code{q}; \code{"below"} for
#' the proportion of effects below \code{q}. By default, is set to \code{"above"} if the pooled point estimate (\code{method = "parametric"}) or median of the calibrated estimates (\code{method = "calibrated"}) is above 1 on the relative risk scale and is set to \code{"below"} otherwise.
#' @param muB.toward.null Whether you want to consider bias that has on average shifted studies' point estimates away from the null (\code{FALSE}; the default) or that has on average shifted studies' point estimates toward the null (\code{TRUE}). See Details.
#' 
#' @param give.CI Logical. If \code{TRUE}, a pointwise confidence intervals is plotted. 
#' @param Bmin Lower limit of lower X-axis on the log scale (only needed if \code{type = "line"}). 
#' @param Bmax Upper limit of lower X-axis on the log scale (only needed if \code{type = "line"})
#' @param breaks.x1 Breaks for lower X-axis (bias factor) on RR scale. (optional for \code{type = "line"}; not used for \code{type = "dist"}). 
#' @param breaks.x2 Breaks for upper X-axis (confounding strength) on RR scale (optional for \code{type = "line"}; not used for \code{type = "dist"})
#' 
#' 
#' @param muB Single mean bias factor on log scale (only needed if \code{type = "dist"})

#' @param sigB Standard deviation of log bias factor across studies (only used if \code{method = "parametric"})
#' @param yr Pooled point estimate (on log scale) from confounded meta-analysis (only used if \code{method = "parametric"})
#' @param vyr Estimated variance of pooled point estimate from confounded meta-analysis (only used if \code{method = "parametric"})
#' @param t2 Estimated heterogeneity (\eqn{\tau^2}) from confounded meta-analysis (only used if \code{method = "parametric"})
#' @param vt2 Estimated variance of \eqn{\tau^2} from confounded meta-analysis (only used if \code{method = "parametric"})

#' @param R  Number  of  bootstrap  iterates for confidence interval estimation. Only used if \code{method = "calibrated"} and \code{give.CI = TRUE}. 
#' @param dat Dataframe containing studies' point estimates and variances. Only used if \code{method = "calibrated"}.
#' @param yi.name Name of variable in \code{dat} containing studies' point estimates. Only used if \code{method = "calibrated"}.
#' @param vi.name Name of variable in \code{dat} containing studies' variance estimates. Only used if \code{method = "calibrated"}.
#'
#' @keywords meta-analysis confounding sensitivity
#' @details
#' This function calls \code{confounded_meta} to get the point estimate and confidence interval at each value of the bias factor. See \code{?confounded_meta} for details. 
#' 
#' Note that \code{Bmin} and \code{Bmax} are specified on the log scale for consistency with the \code{muB} argument and with the function \code{confounded_meta}, whereas \code{breaks.x1} and \code{breaks.x2} are specified on the relative risk scale to facilitate adjustments to the plot appearance. 
#' @export
#' @import
#' ggplot2
#' @importFrom dplyr %>% rowwise mutate rename
#' @references
#' Mathur MB & VanderWeele TJ (2020a). Sensitivity analysis for unmeasured confounding in meta-analyses. \emph{Journal of the American Statistical Association}.
#' 
#' Mathur MB & VanderWeele TJ (2020b). Robust metrics and sensitivity analyses for meta-analyses of heterogeneous effects. \emph{Epidemiology}.
#'
#' Wang C-C & Lee W-C (2019). A simple method to estimate prediction intervals and
#' predictive distributions: Summarizing meta-analyses
#' beyond means and confidence intervals. \emph{Research Synthesis Methods}.
#' @examples
#' 
#' ##### Example 1: Calibrated Line Plots #####
#' 
#' # simulated dataset with exponentially distributed 
#' #  population effects
#' # we will use the calibrated method to avoid normality assumption
#' data(toyMeta)
#' 
#' # without confidence band
#' sens_plot( method = "calibrated",
#'            type="line",
#'            q=log(.9),
#'            tail = "below",
#'            dat = toyMeta,
#'            yi.name = "est",
#'            vi.name = "var",
#'            give.CI = FALSE )
#' 
#' 
#' # # with confidence band and a different threshold, q
#' # # commented out because takes a while too run
#' # sens_plot( method = "calibrated",
#' #            type="line",
#' #            q=0,
#' #            tail = "below",
#' #            dat = toyMeta,
#' #            yi.name = "est",
#' #            vi.name = "var",
#' #            give.CI = TRUE,
#' #            R = 300 ) # should be higher in practice
#' 
#' 
#' ##### Example 2: Calibrated and Parametric Line Plots #####
#' 
#' # example dataset
#' d = metafor::escalc(measure="RR",
#'                     ai=tpos,
#'                     bi=tneg,
#'                     ci=cpos,
#'                     di=cneg,
#'                     data=metafor::dat.bcg)
#' 
#' # without confidence band
#' sens_plot( method = "calibrated",
#'            type="line",
#'            tail = "below",
#'            q=log(1.1),
#'            dat = d,
#'            yi.name = "yi",
#'            vi.name = "vi",
#'            give.CI = FALSE )
#' 
#' # # with confidence band
#' # # commented out because it takes a while
#' # # this example gives bootstrap warnings because of its small sample size
#' # sens_plot( method = "calibrated",
#' #            type="line",
#' #            q=log(1.1),
#' #            R = 500,  # should be higher in practice (e.g., 1000)
#' #            dat = d,
#' #            yi.name = "yi",
#' #            vi.name = "vi",
#' #            give.CI = TRUE )
#' 
#' 
#' # now with heterogeneous bias across studies (via sigB) and with confidence band
#' sens_plot( method = "parametric",
#'            type="line",
#'            q=log(1.1),
#'            yr=log(1.3),
#'            vyr = .05,
#'            vt2 = .001,
#'            t2=0.4,
#'            sigB = 0.1,
#'            Bmin=0,
#'            Bmax=log(4) )
#' 
#' ##### Distribution Line Plot #####
#' 
#' # distribution plot: apparently causative
#' sens_plot( type="dist",
#'            q=log(1.1),
#'            muB=log(2),
#'            sigB = 0.1,
#'            yr=log(1.3),
#'            t2=0.4 )
#' 
#' # distribution plot: apparently preventive
#' sens_plot( type="dist",
#'            q=log(0.90),
#'            muB=log(1.5),
#'            sigB = 0.1,
#'            yr=log(0.7),
#'            t2=0.2 )

sens_plot = function(method="calibrated",
                     type,
                     q,
                     CI.level=0.95,
                     tail=NA,
                     muB.toward.null = FALSE,
                     give.CI=TRUE,
                     Bmin = 0,
                     Bmax = log(4),
                     breaks.x1=NA,
                     breaks.x2=NA,
                     
                     # for plot type "dist"
                     muB,
                     
                     # for type "line" and method "parametric"
                     sigB,
                     yr,
                     vyr=NA,
                     t2,
                     vt2=NA,
                     
                     
                     # for type "line" and method "calibrated"
                     R=1000,
                     dat = NA,
                     yi.name = NA,
                     vi.name = NA) {
  
  # # test only
  # method="calibrated"
  # type = "line"
  # q=median(d$calib)
  # tail = "above"
  # muB=0
  # r=0.1
  # q = 0.2
  # R = 250
  # CI.level = 0.95
  # 
  # give.CI=TRUE
  # dat = d
  # yi.name = "yi"
  # vi.name = "vi"
  # Bmin = 0
  # Bmax = log(5)
  # CI.level = 0.95
  # tail = "above"
  # breaks.x1 = NA
  # breaks.x2 = NA
  
  # method = "parametric"
  # type = "line"
  # q = log(1.1)
  # muB = log(2)
  # sigB = 0.1
  # yr = log(1.4)
  # vyr = 0.5
  # t2 = 0.3
  # vt2 = 0.02
  # r = 0.1
  # Bmin = 0
  # Bmax = log(5)
  # CI.level = 0.95
  # tail = "above"
  # breaks.x1 = NA
  # breaks.x2 = NA
  # give.CI = TRUE
  # muB.toward.null = TRUE
  
  val = group = eB = phat = lo = hi = B = B.x = Phat = NULL
  
  ##### Distribution Plot ######
  if ( type=="dist" ) {
    
    # check for bad input
    if( is.na(muB) ) stop("For type='dist', must provide muB")
    
    if ( ( length(muB) > 1 ) | ( length(sigB) > 1 ) ) {
      stop( "For type='dist', muB and sigB must be length 1")
    }
    
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
    
    # cut the dataframe to match axis limits
    # avoids warnings from stat_density about non-finite values being removed
    temp = temp[ temp$val >= x.min & temp$val <= x.max, ]
    
    colors=c("black", "orange")
    
    p = ggplot2::ggplot( data = temp, aes(x=val, group=group ) ) +
      
      geom_density( aes( x=val, fill=group ), alpha=0.4 ) +
      theme_bw() +
      xlab("Study-specific relative risks") +
      ylab("") +
      guides(fill=guide_legend(title=" ")) +
      scale_fill_manual(values=colors) +
      geom_vline( xintercept = exp(q), lty=2, color="red" ) +
      scale_x_continuous( limits=c(x.min, x.max), breaks = seq( round(x.min), round(x.max), 0.5) ) +
      ggtitle("Observed and true relative risk distributions") 
    
    graphics::plot(p)
  }
  
  ##### Line Plot ######
  if ( type=="line" ) {
    
    
    # compute axis tick points for both X-axes
    if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
    if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
    
    
    if ( method=="parametric" ) {
      
      
      
      if ( is.na(tail) ) {
        tail = ifelse( yr > 0, "above", "below" )
        warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
      }
      
      if ( is.na(vyr) | is.na(vt2) ) {
        message( "No confidence interval because vyr or vt2 is NULL")
      }
      
      # get mean bias factor values for a vector of B's from Bmin to Bmax
      t = data.frame( B = seq(Bmin, Bmax, .01), phat = NA, lo = NA, hi = NA )
      t$eB = exp(t$B)
      
      for ( i in 1:dim(t)[1] ) {
        # r is irrelevant here
        # suppress warnings about Phat being close to 0 or 1
        cm = suppressWarnings( suppressMessages( confounded_meta( method = method,
                                                                  q = q,
                                                                  r = NA,
                                                                  muB=t$B[i],
                                                                  sigB=sigB,
                                                                  yr=yr,
                                                                  vyr=vyr,
                                                                  t2=t2,
                                                                  vt2=vt2,
                                                                  CI.level=CI.level,
                                                                  tail=tail,
                                                                  muB.toward.null = muB.toward.null) ) )
        
        t$phat[i] = cm$Est[ cm$Value=="Prop" ]
        t$lo[i] = cm$CI.lo[ cm$Value=="Prop" ]
        t$hi[i] = cm$CI.hi[ cm$Value=="Prop" ]
      }
      
      
      p = ggplot2::ggplot( t, aes(x=eB,
                                  y=phat ) ) +
        theme_bw() +
        
        scale_y_continuous( limits=c(0,1),
                            breaks=seq(0, 1, .1)) +
        
        scale_x_continuous(  breaks = breaks.x1,
                             sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                                  name = "Minimum strength of both confounding RRs",
                                                  breaks = breaks.x2) ) +
        
        geom_line(lwd=1.2) +
        xlab("Hypothetical average bias factor across studies (RR scale)") +
        ylab( paste( ifelse( tail=="above",
                             paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                             paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
      
      # can't compute a CI if the bounds aren't there
      no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) ) | (give.CI == FALSE)
      
      if ( no.CI ){
        graphics::plot(p)
        return(p)
      } else {
        
        warning("Calculating parametric confidence intervals in the plot. For values of the proportion that are less than 0.15 or greater than 0.85, these confidence intervals may not perform well.")
        
        p = p + ggplot2::geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15 )
        graphics::plot( p )
        return(p)
      }
      
      
    } ## closes method=="parametric"
    
    
    if ( method == "calibrated" ) {
      
      # if tail isn't provided, assume user wants the more extreme one (away from the null)
      if ( is.na(tail) ) {
        calib = calib_ests( yi = dat[[yi.name]], 
                            sei = sqrt( dat[[vi.name]] ) )
        
        tail = ifelse( median(calib) > 0, "above", "below" )
        warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
      }
      
      res = data.frame( B = seq(Bmin, Bmax, .01) )
      

      # evaluate Phat causal at each value of B
      res = res %>% rowwise() %>%
        mutate( Phat = Phat_causal( q = q, 
                                    B = B,
                                    tail = tail,
                                    muB.toward.null = muB.toward.null,
                                    dat = dat,
                                    yi.name = yi.name,
                                    vi.name = vi.name ) ) 
      
      if ( give.CI == TRUE ) {
        # look at just the values of B at which Phat jumpss
        #  this will not exceed the number of point estimates in the meta-analysis
        # first entry should definitely be bootstrapped, so artificially set its diff to nonzero value
        diffs = c( 1, diff(res$Phat) )  
        res.short = res[ diffs != 0, ]
        
        
        # bootstrap a CI for each entry in res.short
        res.short = res.short %>% rowwise() %>%
          mutate( Phat_CI_lims(.B = B,
                               R = R,
                               q = q,
                               tail = tail,
                               muB.toward.null = muB.toward.null,
                               dat = dat,
                               yi.name = yi.name,
                               vi.name = vi.name,
                               CI.level = CI.level)[1:2] )
        
        # merge this with the full-length res dataframe, merging by Phat itself
        res = merge( res, res.short, by = "Phat", all.x = TRUE )
        
        res = res %>% rename( B = B.x )
        
        
        ##### Warnings About Missing CIs Due to Boot Failures #####
        # if ALL CI limits are missing
        if ( all( is.na(res$lo) ) ) {
          message( "None of the pointwise confidence intervals was estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot is omitted. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold." )
          # avoid even trying to plot the CI if it's always NA to avoid geom_ribbon errors later
          give.CI = FALSE
        }
        
        
        # outer "if" handles case in which AT LEAST ONE CI limit is NA because of boot failures
        if ( any( !is.na(res$lo) ) & any( !is.na(res$hi) ) ) {
          
          message( "Some of the pointwise confidence intervals were not estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot may not be shown for some values of the bias factor. This usually happens at values with a proportion estimate close to 0 or 1. You can try increasing the number of bootstrap iterates or choosing a less extreme threshold." )
          
          if ( any( res$lo[ !is.na(res$lo) ] > res$Phat[ !is.na(res$lo) ] ) | any( res$hi[ !is.na(res$lo) ] < res$Phat[ !is.na(res$lo) ] ) ) {
            
            message( "Some of the pointwise confidence intervals do not contain the proportion estimate itself. This reflects instability in the bootstrapping process. See the other warnings for details." )
            
          }
        }
        
        
      }
      
      p = ggplot2::ggplot( data = res,
                           aes( x = exp(B),
                                y = Phat ) ) +
        theme_bw() +
        
        
        scale_y_continuous( limits=c(0,1),
                            breaks=seq(0, 1, .1)) +
        scale_x_continuous(  #limits = c( min(breaks.x1), max(breaks.x1) ),  # this line causes an error with geom_line having "missing values"
          breaks = breaks.x1,
          sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                               name = "Minimum strength of both confounding RRs",
                               breaks = breaks.x2)
        ) +
        geom_line(lwd=1.2) +
        
        xlab("Hypothetical bias factor in all studies (RR scale)") +
        ylab( paste( ifelse( tail=="above",
                             paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                             paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
      
      
      
      if ( give.CI == TRUE ) {
        p = p + geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15, fill = "black" )
      }
      
      graphics::plot(p)
    }  # closes method == "calibrated"
    
  } ## closes type=="line"
} ## closes sens_plot function

############################ INTERNAL FUNCTIONS ############################ 

#' Proportion of studies with causal effects above or below q
#'
#' An internal function that estimates the proportion of studies with true effect sizes above or below \code{q} given the bias factor \code{B}. Users should call \code{confounded_meta} instead.
#' @import
#' boot 
#' @noRd
Phat_causal = function( q,
                        B,
                        tail,
                        muB.toward.null,
                        dat,
                        yi.name,
                        vi.name) {
  
  if ( ! yi.name %in% names(dat) ) stop("dat does not contain a column named yi.name")
  if ( ! vi.name %in% names(dat) ) stop("dat does not contain a column named vi.name")
  
  calib = MetaUtility::calib_ests( yi = dat[[yi.name]],
                                   sei = sqrt(dat[[vi.name]] ) )
  # confounding-adjusted calibrated estimates
  # bias that went away from null, so correction goes toward null
  if ( median(calib) > 0 & muB.toward.null == FALSE ) calib.t = calib - B
  if ( median(calib) < 0 & muB.toward.null == FALSE ) calib.t = calib + B
  # bias that went toward null, so correction goes away from null
  if ( median(calib) > 0 & muB.toward.null == TRUE ) calib.t = calib + B
  if ( median(calib) < 0 & muB.toward.null == TRUE ) calib.t = calib - B
  
  # confounding-adjusted Phat
  if ( tail == "above" ) Phat.t = mean( calib.t > q )
  if ( tail == "below" ) Phat.t = mean( calib.t < q )
  
  return(Phat.t)
}



#' Transformation from bias factor to confounding strength scale
#'
#' An internal function. 
#' @noRd
g = Vectorize( function(x) {
  # define transformation in a way that is monotonic over the effective range of B (>1)
  # to avoid ggplot errors in sens_plot
  # helper function for confounded_meta
  if ( is.na(x) ) return(NA)
  if (x < 1) return( x / 1e10 )
  x + sqrt( x^2 - x )
} )



#' Minimum common bias factor to reduce proportion of studies with causal effects above or below q t less than r
#'
#' An internal function that estimates; users should call \code{confounded_meta} instead.
#' @import
#' MetaUtility 
#' @noRd
Tmin_causal = function( q,
                        r,
                        tail,
                        
                        dat,
                        yi.name,
                        vi.name ) {
  
  # # test only
  # dat = d
  # calib.temp = MetaUtility::calib_ests(yi = d$yi,
  #                                      sei = sqrt(d$vyi))
  # q = quantile(calib.temp, 0.8)
  # r = 0.3
  # yi.name = "yi"
  # vi.name = "vyi"
  # tail = "above"
  
  
  # here, check if any shifting is actually needed
  # current Phat with no bias
  Phatc = Phat_causal(q = q,
                      B = 0,
                      tail = tail,
                      # this doesn't matter because there's no bias yet
                      muB.toward.null = FALSE,
                      dat = dat,
                      yi.name = yi.name,
                      vi.name = vi.name)
  if ( Phatc <= r ){
    return(1)
  }
  
  # evaluate the ECDF of the unshifted calib at those calib themselves
  #  to get the possible values that Phat can take
  #  this approach handles ties
  calib = sort( calib_ests( yi = dat[[yi.name]], sei = sqrt(dat[[vi.name]]) ) )
  Phat.options = unique( ecdf(calib)(calib) )
  # always possible to choose 0
  Phat.options = c(Phat.options, 0)
  
  # of Phats that are <= r, find the largest one (i.e., closest to r)
  Phat.target = max( Phat.options[ Phat.options <= r ] ) 
  
  
  # find calib.star, the calibrated estimate that needs to move to q
  # example for tail == "above":
  # calib.star is the largest calibrated estimate that needs to move to just
  #  BELOW q after shifting
  # k * Phat.target is the number of calibrated estimates that should remain
  #  ABOVE q after shifting
  k = length(calib)
  if ( tail == "above" ) calib.star = calib[ k - (k * Phat.target) ]
  if ( tail == "below" ) calib.star = calib[ (k * Phat.target) + 1 ]
  
  # pick the bias factor that shifts calib.star to q
  #  and then add a tiny bit (0.001) to shift calib.star to just
  # below or above q
  # if multiple calibrated estimates are exactly equal to calib.star, 
  #  all of these will be shifted just below q (if tail == "above")
  #
  # because we're taking Tmin to be the (exp) ABOLSUTE difference between 
  #  the calib estimate that needs to move to q and q itself, Tmin
  #  will automatically be the bias in whatever direction is needed to
  #  make the shift
  ( Tmin = exp( abs(calib.star - q) + 0.001 ) )
  
  return(as.numeric(Tmin))
}


#' CI for proportion of studies with causal effects above or below q
#'
#' An internal function that estimates a CI for the proportion of studies with true effect sizes above or below \code{q} given the bias factor \code{B}. Users should call \code{confounded_meta} instead.
#' @import
#' boot 
#' @noRd
Phat_CI_lims = function(.B,
                        R,
                        q,
                        tail,
                        muB.toward.null,
                        dat,
                        yi.name,
                        vi.name,
                        CI.level) {
  
  tryCatch({
    boot.res = suppressWarnings( boot( data = dat,
                                       parallel = "multicore",
                                       R = R, 
                                       statistic = function(original, indices) {
                                         
                                         # draw bootstrap sample
                                         b = original[indices,]
                                         
                                         Phatb = suppressWarnings( Phat_causal( q = q, 
                                                                                B = .B,
                                                                                tail = tail,
                                                                                muB.toward.null = muB.toward.null,
                                                                                dat = b,
                                                                                yi.name = yi.name,
                                                                                vi.name = vi.name) )
                                         return(Phatb)
                                       } ) )
    
    bootCIs = boot.ci(boot.res,
                      type="bca",
                      conf = CI.level )
    
    lo = bootCIs$bca[4]
    hi = bootCIs$bca[5]
    se = sd(boot.res$t)
    
    # avoid issues with creating df below
    if ( is.null(lo) ) lo = NA
    if ( is.null(hi) ) hi = NA
    
  }, error = function(err) {
    lo <<- NA
    hi <<- NA
    se <<- NA
  })
  
  # return as data frame to play well with rowwise() and mutate()
  return( data.frame( lo, hi, se ) )
}



#' CI for Tmin and Gmin
#'
#' An internal function that estimates a CI for Tmin and Gmin. Users should call \code{confounded_meta} instead.
#' @import
#' boot 
#' @noRd
Tmin_Gmin_CI_lims = function(
  R,
  q,
  r,
  tail,
  dat,
  yi.name,
  vi.name,
  CI.level) {
  
  tryCatch({
    boot.res = suppressWarnings( boot( data = dat,
                                       parallel = "multicore",
                                       R = R, 
                                       statistic = function(original, indices) {
                                         
                                         # draw bootstrap sample
                                         b = original[indices,]
                                         
                                         Tminb = Tmin_causal(q = q,
                                                             r = r,
                                                             tail = tail,
                                                             dat = b,
                                                             yi.name = yi.name,
                                                             vi.name = vi.name)
                                         return(Tminb)
                                       } ) )
    
    
    bootCIs.Tmin = boot.ci(boot.res,
                           type="bca",
                           conf = CI.level )
    
    lo.T = max(1, bootCIs.Tmin$bca[4])  # bias factor can't be < 1
    hi.T = bootCIs.Tmin$bca[5]  # but has no upper bound
    SE.T = sd(boot.res$t)
    
    # avoid issues with creating df below and with g() transformation
    if ( is.null(lo.T) ) lo.T = NA
    if ( is.null(hi.T) ) hi.T = NA
    
    
    ##### Gmin #####
    lo.G = max( 1, g(lo.T) )  # confounding RR can't be < 1
    hi.G = g(hi.T)  # but has no upper bound
    SE.G = sd( g(boot.res$t) )
    
    
    # avoid issues with creating df below
    if ( is.null(lo.G) ) lo.G = NA
    if ( is.null(hi.G) ) hi.G = NA
    
  }, error = function(err) {
    lo.T <<- NA
    hi.T <<- NA
    
    lo.G <<- NA
    hi.G <<- NA
    
    SE.T <<- NA
    SE.G <<- NA
  })
  
  # return as data frame to play well with rowwise() and mutate()
  return( data.frame( lo.T, hi.T, SE.T, lo.G, hi.G, SE.G ) )
}



############################ EXAMPLE DATASETS ############################ 

#' An example meta-analysis
#'
#' A simple simulated meta-analysis of 50 studies with exponentially distributed population effects.
#'
#' @docType data
#' @keywords datasets
#' @details
#' The variables are as follows:
#' \itemize{
#'   \item \code{est} Point estimate on the log-relative risk scale.
#'   \item \code{var} Variance of the log-relative risk.
#' }
"toyMeta"



#' A meta-analysis on soy intake and breast cancer risk (Trock et al., 2006)
#'
#' A meta-analysis of observational studies (12 case-control and six cohort or nested case-control) on the association of soy-food intake with breast cancer risk. Data are from Trock et al.'s (2006) Table 1. This dataset was used as the applied example in Mathur & VanderWeele (2020a). 
#'
#' @docType data
#' @keywords datasets
#' @references 
#' Trock BJ, Hilakivi-Clarke L, Clark R (2006). Meta-analysis of soy intake and breast cancer risk. \emph{Journal of the National Cancer Institute}.
#' 
#' Mathur MB & VanderWeele TJ (2020a). Sensitivity analysis for unmeasured confounding in meta-analyses. \emph{Journal of the American Statistical Association}.
#' @details
#' The variables are as follows:
#' \itemize{
#'\item \code{author} Last name of the study's first author.
#'   \item \code{est} Point estimate on the log-relative risk or log-odds ratio scale.
#'   \item \code{var} Variance of the log-relative risk or log-odds ratio.
#' }
"soyMeta"







