############################ META-ANALYSIS FUNCTIONS ############################ 

#' ###### Phat after shifting by bias factor B and using calibrated estimates #####
#' .dat needs to have a column called "calib"
#' helper function for confounded_meta
#' 
Phat_causal = function( .q,
                        .B,
                        .calib, # assumed on log scale
                        .tail,
                        
                        .give.CI = TRUE,
                        .R = 2000,
                        .dat = NA,
                        .calib.name = NA ) {
  
  if(.B > .q) calib.t = .calib + log(.B)
  if(.B < .q) calib.t = .calib - log(.B)
  
  # confounding-adjusted Phat
  if ( .tail == "above" ) Phat.t = mean( calib.t > .q )
  if ( .tail == "below" ) Phat.t = mean( calib.t < .q )
  
  
  if ( .give.CI == FALSE ) {
    
    return(Phat.t)
    
  } else {
    boot.res = suppressWarnings( boot( data = .dat,
                                       parallel = "multicore",
                                       R = .R, 
                                       statistic = Phat_causal_bt,
                                       # below arguments are being passed to get_stat
                                       .calib.name = .calib.name,
                                       .q = .q,
                                       .B = .B,
                                       .tail = .tail ) )
    
    bootCIs = boot.ci(boot.res,
                      type="bca",
                      conf = 0.95 )
    
    lo = bootCIs$bca[4]
    hi = bootCIs$bca[5]
    SE = sd(boot.res$t)
    
    return( data.frame( Est = Phat.t,
                        SE = SE,
                        lo = lo, 
                        hi = hi ) )
  }
}



###### Simplified version of above for boot to call #####
Phat_causal_bt = function( original,
                           indices,
                           .calib.name,
                           .q,
                           .B,
                           .tail ) {
  
  b = original[indices,]
  
  phatb = Phat_causal( .q = .q, 
                       .B = .B,
                       .calib = b[[.calib.name]], 
                       .tail = .tail,
                       .give.CI = FALSE)
  return(phatb)
}

#' define transformation in a way that is monotonic over the effective range of B (>1)
#' to avoid ggplot errors
#' helper function for confounded_meta
#' 
g = Vectorize( function(x) {
  if (x < 1) return( x / 1e10 )
  x + sqrt( x^2 - x )
} )

logHR_to_logRR = function(logRR){
  log( ( 1 - 0.5^sqrt( exp(logRR) ) ) / ( 1 - 0.5^sqrt( 1 / exp(logRR) ) ) )
}

#' ##### That and Ghat from grid search of Phat values #####
#' # for each of a vector of bias factors, calculates Phat causal and then finds the one
#' # that's closest to threshold proportion, .r
#' helper function for confounded_meta
#'
##### Simplified version of the above for boot to call #####
That_causal_bt = function( original,
                           indices, 
                           .calib.name,
                           .q,
                           .r,
                           .B.vec,
                           .tail ) {
  b = original[indices,]
  
  Bl = as.list(.B.vec)
  
  # calculate Phat for a vector of B
  Phat.t.vec = unlist( lapply( Bl,
                               FUN = function(B) Phat_causal( .q = .q, 
                                                              .B = B,
                                                              .calib = b[[.calib.name]],
                                                              .tail = .tail,
                                                              .give.CI = FALSE ) ) )
  
  
  That = .B.vec[ which.min( abs( Phat.t.vec - .r ) ) ]
  return(That)
  
}


#' Estimates and inference for sensitivity analyses
#'
#' Computes point estimates, standard errors, and confidence interval bounds
#' for (1) \code{prop}, the proportion of studies with true effect sizes above \code{q} (or below
#' \code{q} for an apparently preventive \code{yr}) as a function of the bias parameters;
#' (2) the minimum bias factor on the relative risk scale (\code{Tmin}) required to reduce to
#' less than \code{r} the proportion of studies with true effect sizes more extreme than
#' \code{q}; and (3) the counterpart to (2) in which bias is parameterized as the minimum
#' relative risk for both confounding associations (\code{Gmin}).
#' @param method "calibrated" or "parametric"
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
#' @param Bmin Lower limit of bias factor (used for "calibrated" method only)
#' @param Bmax Upper limit of bias factor (used for "calibrated" method only)
#' @param .calib Calibrated estimates on logRR scale
#' @param .give.CI Logical. If TRUE, bootstrap confidence intervals provided
#' @param .R Number  of  bootstrap  or  simulation  iterates  (depending  on  the  methods  cho-sen).   Not required if using ci.method = "parametric"and bootstrapping is not needed.
#' @param .calb.name column name in dataframe containing calibrated estimates
#' @export
#' @details
#' To compute all three point estimates (\code{prop, Tmin, and Gmin}) and inference, all
#' arguments must be non-\code{NA}. To compute only a point estimate for \code{prop},
#' arguments \code{r, vyr}, and \code{vt2} can be left \code{NA}. To compute only
#' point estimates for \code{Tmin} and \code{Gmin}, arguments \code{muB, vyr}, and \code{vt2}
#' can be left \code{NA}. To compute inference for all point estimates, \code{vyr} and 
#' \code{vt2} must be supplied. 
#' @keywords meta-analysis
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


confounded_meta = function( method="calibrated", q, r=NA, muB, sigB,
                            yr, vyr=NA, t2, vt2=NA,
                            CI.level=0.95, tail=NA, Bmin, Bmax,
                            .calib, .give.CI=TRUE, .R=2000, .dat, .calib.name ) {
  
  # somewhere have option to plot the bias factor distribution, the confounded distribution, and the adjusted distribution
  ### for parametric
  if (method=="parametric"){
    
    
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
    if ( yr > log(1) ) {
      yr.corr = yr - muB
    }else{ yr.corr = yr + muB}
    
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
    if ( tail == "below" ) {
      
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
  } ## closes parametric method
  
  ## for calibrated
  if(method=="calibrated"){
    
    require(boot)
    .B.vec = seq(Bmin, Bmax, .01)
    
    
    # confounding-adjusted Phat
    if ( tail == "above" ) Phat.t = mean( .calib > q )
    if ( tail == "below" ) Phat.t = mean( .calib < q )
    
    
    if ( .give.CI == FALSE ) {
      
      return(Phat.t)
      
    } else {
      boot.res = suppressWarnings( boot( data = .dat,
                                         parallel = "multicore",
                                         R = .R, 
                                         statistic = Phat_causal_bt,
                                         # below arguments are being passed to get_stat
                                         .calib.name = .calib.name,
                                         .q = q,
                                         .B = .B.vec,
                                         .tail = tail ) )
      
      bootCIs = boot.ci(boot.res,
                        type="bca",
                        conf = 0.95 )
      
      lo_Phat = bootCIs$bca[4]
      hi_Phat = bootCIs$bca[5]
      SE_Phat = sd(boot.res$t)
      Bl = as.list(.B.vec)
      
      Phat.t.vec = lapply( Bl,
                           FUN = function(B) Phat_causal( .q = q, 
                                                          .B = B,
                                                          .calib = .calib,
                                                          .tail = tail,
                                                          .give.CI = FALSE ) )
      
      res = data.frame( B = .B.vec,
                        Phat.t = unlist(Phat.t.vec) )
      
      That = res$B[ which.min( abs( res$Phat.t - r ) ) ]
      Ghat = g(That)
      
      if ( .give.CI == FALSE ) {
        
        return( data.frame( That, Ghat ) )
        
      } else {
        boot.res = suppressWarnings( boot( data = .dat,
                                           parallel = "multicore",
                                           R = .R, 
                                           statistic = That_causal_bt,
                                           # below arguments are being passed to get_stat
                                           .calib.name = .calib.name,
                                           .q = q,
                                           .r = r,
                                           .B.vec = .B.vec,
                                           .tail = tail ) )
        
        bootCIs = boot.ci(boot.res,
                          type="bca",
                          conf = 0.95 )
        
        lo = bootCIs$bca[4]
        hi = bootCIs$bca[5]
        SE = sd(boot.res$t)
        
        # return results
        res = data.frame( Value = c("Phat.t", "That", "Ghat"), 
                          Est = c(Phat.t, That, Ghat),
                          SE = c(SE_Phat, SE, NA),  # ~~ for latter, could replace with delta method
                          CI.lo = c(lo_Phat, lo, g(lo)), 
                          CI.hi = c(hi_Phat, hi, g(hi)),
                          Meaning = c(NA, "Bias factor required", "Confounding strength required")
        )
        
        return(res)
      }
    }
    
    
    
  } #closes calibrated method
  
} #closes confounded_meta function










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
#' @keywords meta-analysis
#' confounding
#' sensitivity
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
#' @keywords meta-analysis
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
#' to produce the given bias factor. The shaded region represents a 95% pointwise confidence band.
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
#' @keywords meta-analysis confounding sensitivity
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
                      CI.level=0.95, tail=c("above", "below") ) {
  
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
                           CI.level=CI.level, tail=tail)
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
      ylab( paste( ifelse( tail=="above",
                           paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                           paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
    
    # can't compute a CI if the bounds aren't there
    no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) )
    
    if ( no.CI ) graphics::plot(p)
    else p + ggplot2::geom_ribbon( aes(ymin=t$lo, ymax=t$hi), alpha=0.15 )   
    
  }
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
#' @keywords meta-analysis
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
