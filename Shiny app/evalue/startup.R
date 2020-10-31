# install.packages("shinyWidgets")

library(shiny)
# library(EValue) #include confounded_meta and sens_plot below to test, will eventually be loaded into EValue package and can remove the functions below
library(plotly)
library(shinythemes)
library(shinyBS)
library(shinyalert)
library(bsplus)
library(shinydashboard)
library(shinyWidgets)
library(MetaUtility)


# try to fix deployment problem
library(purrr)
library(plogr)

# keeps original error messages
options(shiny.sanitize.errors = FALSE)

# ## test w/ updated confounded_meta and sens_plot_addtail
### packages and functions from Maya's old code (will eventually be updated in EValue package and can just load package):
library(boot)

###helper functions (Phat_causal, That_causal, That_causal_bt, g, logHR_to_logRR) from Maya code:
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
  
  if(.tail == "above") calib.t = .calib - log(.B)
  if(.tail == "below") calib.t = .calib + log(.B)
  
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

confounded_meta = function( method="calibrated", q, r=NA, muB, sigB,
                            yr, vyr=NA, t2, vt2=NA,
                            CI.level=0.95, tail=NA, Bmin, Bmax,
                            .calib, .give.CI=TRUE, .R=2000, .dat, .calib.name ) {
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

    .dat[["calib"]] = MetaUtility::calib_ests(yi=.dat[[yr]],
                                      sei=sqrt(.dat[[vyr]]))
    .calib=.dat$calib
    .calib.name="calib"
    
    .B=muB
    
    if(tail == "above") calib.t = .calib - log(.B)
    if(tail == "below") calib.t = .calib + log(.B)

    # confounding-adjusted Phat
    if ( tail == "above" ) Phat.t = mean( calib.t > q )
    if ( tail == "below" ) Phat.t = mean( calib.t < q )
    
    
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


sens_plot = function(method="calibrated", type, q, r=NA, muB, Bmin, Bmax, sigB,
                             yr, vyr=NA, t2, vt2=NA,
                             breaks.x1=NA, breaks.x2=NA,
                             CI.level=0.95, tail=NA,
                             # .calib, 
                             .give.CI=TRUE, .R=2000, .dat
                             # , .calib.name
                             ) {
  
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
    if(method=="parametric"){
      # get mean bias factor values for a bunch of different B's
      t = data.frame( B = seq(Bmin, Bmax, .01), phat = NA, lo = NA, hi = NA )
      t$eB = exp(t$B)
      
      for ( i in 1:dim(t)[1] ) {
        # r is irrelevant here
        cm = confounded_meta(method=method,q=q, r=r, muB=t$B[i], sigB=sigB,
                             yr=yr, vyr=vyr, t2=t2, vt2=vt2,
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
    } ## closes method=="parametric"
    
    else {if(method=="calibrated"){
      
      require(boot)
      .B.vec = seq(Bmin, Bmax, .01)
      
      .dat[["calib"]] = MetaUtility::calib_ests(yi=.dat[[yr]],
                                                sei=sqrt(.dat[[vyr]]))
      .calib=.dat$calib
      .calib.name="calib"
      
      .B=muB
      
      if(tail == "above") calib.t = .calib - log(.B)
      if(tail == "below") calib.t = .calib + log(.B)
      
      # confounding-adjusted Phat
      if ( tail == "above" ) Phat.t = mean( calib.t > q )
      if ( tail == "below" ) Phat.t = mean( calib.t < q )
      
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
        
        # look at just the values of B at which Phat jumps
        #  this will not exceed the number of point estimates in the meta-analysis
        res.short = res[ diff(res$Phat.t) != 0, ]
        
        library(dplyr)
        
        # bootstrap a CI for each entry in res.short
        temp = res.short %>% rowwise() %>%
          do( Phat_causal( .q = q, 
                           .B = .$B,
                           .calib = .calib,
                           .tail = tail,
                           .give.CI = TRUE,
                           .dat = .dat,
                           .R = .R,
                           .calib.name = .calib.name ) )
        
        # merge this with the full-length res dataframe, merging by Phat itself
        res = merge( res, temp, by.x = "Phat.t", by.y = "Est")
        
        ##### Make Plot #####
        library(ggplot2)
        
        ggplot( data = res,
                aes( x = B,
                     y = Phat.t ) ) +
          theme_bw() +
          
          # proprtion "r" line
          geom_hline( yintercept = r, 
                      lty = 2,
                      color = "red" ) +
          
          # That line
          geom_vline( xintercept = That, 
                      lty = 2,
                      color = "black" ) +
          
          scale_y_continuous( limits=c(0,1), breaks=seq(0, 1, .1)) +
          scale_x_continuous(  breaks = seq(1, 20, .1),
                               sec.axis = sec_axis( ~ g(.),  # confounding strength axis
                                                    name = "Minimum strength of both confounding RRs",
                                                    breaks = seq(1, 8, .5 )) ) +
          geom_line(lwd=1.2) +
          
          # # parametric estimate for comparison
          # geom_line( data = res,
          #            aes( x = B,
          #                 y = Est.param ),
          #            lwd = 1.2,
          #            color = "blue") +
          
          xlab("Hypothetical bias factor in all studies (RR scale)") +
          ylab( paste( ifelse( tail=="above",
                               paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
                               paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) ) +
          
          
          geom_ribbon( aes(ymin=res$lo, ymax=res$hi), alpha=0.15, fill = "black" ) 
        
      }
    } ## closes method="calibrated
    }} ## closes type=="line"
} ## closes sens_plot function