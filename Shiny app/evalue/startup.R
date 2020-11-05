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

source("~/Box Sync/jlee/Maya/evalue/EValue/R/meta-analysis.R")



# d = metafor::escalc(measure="RR",
#                     ai=tpos,
#                     bi=tneg,
#                     ci=cpos,
#                     di=cneg,
#                     data=metafor::dat.bcg)
# 
# write.csv(d, "~/Box Sync/jlee/Maya/unmeasured_confounding/metashiny/d_sens_plot.csv", row.names = FALSE)


#' # without confidence band
#' sens_plot( method = "calibrated",
#'            type="line",
#'            q=log(1.1),
#'            Bmin=log(1),
#'            Bmax=log(4),
#'            dat = d,
#'            yi.name = "yi",
#'            vi.name = "vi",
#'            give.CI = FALSE )
#' 
#' 
#' sens_plot = function(method="calibrated",
#'                      type,
#'                      q,
#'                      #r=NA,
#'                      CI.level=0.95,
#'                      tail=NA,
#'                      give.CI=TRUE,
#'                      Bmin,
#'                      Bmax,
#'                      breaks.x1=NA,
#'                      breaks.x2=NA,
#'                      
#'                      # for plot type "dist"
#'                      muB,
#'                      
#'                      # for type "line" and method "parametric"
#'                      sigB,
#'                      yr,
#'                      vyr=NA,
#'                      t2,
#'                      vt2=NA,
#'                      
#'                      
#'                      # for type "line" and method "calibrated"
#'                      R=2000,
#'                      dat = NA,
#'                      yi.name = NA,
#'                      vi.name = NA) {
#'   
#'   # # test only
#'   # method="calibrated"
#'   # type = "line"
#'   # q=median(d$calib)
#'   # tail = "above"
#'   # muB=0
#'   # r=0.1
#'   # q = 0.2
#'   # R = 250
#'   # CI.level = 0.95
#'   # 
#'   # give.CI=TRUE
#'   # dat = d
#'   # yi.name = "yi"
#'   # vi.name = "vi"
#'   # Bmin = log(1)
#'   # Bmax = log(5)
#'   # CI.level = 0.95
#'   # tail = "above"
#'   # breaks.x1 = NA
#'   # breaks.x2 = NA
#'   
#'   # method = "parametric"
#'   # type = "line"
#'   # q = log(1.1)
#'   # muB = log(2)
#'   # sigB = 0.1
#'   # yr = log(1.4)
#'   # vyr = 0.5
#'   # t2 = 0.3
#'   # vt2 = 0.02
#'   # r = 0.1
#'   # Bmin = log(1)
#'   # Bmax = log(5)
#'   # CI.level = 0.95
#'   # tail = "above"
#'   # breaks.x1 = NA
#'   # breaks.x2 = NA
#'   
#'   ##### Distribution Plot ######
#'   if ( type=="dist" ) {
#'     
#'     # check for bad input
#'     if( is.na(muB) ) stop("For type='dist', must provide muB")
#'     
#'     if ( ( length(muB) > 1 ) | ( length(sigB) > 1 ) ) {
#'       stop( "For type='dist', muB and sigB must be length 1")
#'     }
#'     
#'     # simulate confounded distribution
#'     reps = 10000
#'     RR.c = exp( rnorm( n=reps, mean=yr, sd=sqrt(t2) ) )
#'     
#'     # simulate unconfounded distribution
#'     Mt = ifelse( yr > 0, yr - muB, yr + muB )
#'     RR.t = exp( rnorm( n=reps, mean=Mt, sd=sqrt(t2-sigB^2) ) )
#'     
#'     # get reasonable limits for X-axis
#'     x.min = min( quantile(RR.c, 0.01), quantile(RR.t, 0.01) )
#'     x.max = max( quantile(RR.c, 0.99), quantile(RR.t, 0.99) )
#'     
#'     temp = data.frame( group = rep( c( "Observed", "True" ), each = reps ), 
#'                        val = c( RR.c, RR.t ) )
#'     
#'     colors=c("black", "orange")
#'     p = ggplot2::ggplot( data=temp, aes(x=temp$val, group=temp$group ) ) +
#'       geom_density( aes( fill=temp$group ), alpha=0.4 ) +
#'       theme_bw() + xlab("Study-specific relative risks") +
#'       ylab("") + guides(fill=guide_legend(title=" ")) +
#'       scale_fill_manual(values=colors) +
#'       geom_vline( xintercept = exp(q), lty=2, color="red" ) +
#'       scale_x_continuous( limits=c(x.min, x.max), breaks = seq( round(x.min), round(x.max), 0.5) ) +
#'       ggtitle("Observed and true relative risk distributions")
#'     
#'     graphics::plot(p)
#'   }
#'   
#'   ##### Line Plot ######
#'   if ( type=="line" ) {
#'     
#'     
#'     # compute axis tick points for both X-axes
#'     if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
#'     if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
#'     
#'     
#'     if ( method=="parametric" ) {
#'       
#'       
#'       
#'       if ( is.na(tail) ) {
#'         tail = ifelse( yr > log(1), "above", "below" )
#'         warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
#'       }
#'       
#'       if ( is.na(vyr) | is.na(vt2) ) {
#'         message( "No confidence interval because vyr or vt2 is NULL")
#'       }
#'       
#'       # get mean bias factor values for a vector of B's from Bmin to Bmax
#'       t = data.frame( B = seq(Bmin, Bmax, .01), phat = NA, lo = NA, hi = NA )
#'       t$eB = exp(t$B)
#'       
#'       for ( i in 1:dim(t)[1] ) {
#'         # r is irrelevant here
#'         # suppress warnings about Phat being close to 0 or 1
#'         browser()
#'         cm = suppressWarnings( confounded_meta( method=method,
#'                                                 q=q,
#'                                                 r=r,
#'                                                 muB=t$B[i],
#'                                                 sigB=sigB,
#'                                                 yr=yr,
#'                                                 vyr=vyr,
#'                                                 t2=t2,
#'                                                 vt2=vt2,
#'                                                 CI.level=CI.level,
#'                                                 tail=tail ) )
#'         
#'         t$phat[i] = cm$Est[ cm$Value=="Prop" ]
#'         t$lo[i] = cm$CI.lo[ cm$Value=="Prop" ]
#'         t$hi[i] = cm$CI.hi[ cm$Value=="Prop" ]
#'       }
#'       
#'       # # compute axis tick points for both X-axes
#'       # if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
#'       # if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
#'       
#'       p = ggplot2::ggplot( t, aes(x=eB,
#'                                   y=phat ) ) +
#'         theme_bw() +
#'         
#'         scale_y_continuous( limits=c(0,1),
#'                             breaks=seq(0, 1, .1)) +
#'         
#'         scale_x_continuous(  breaks = breaks.x1,
#'                              sec.axis = sec_axis( ~ g(.),  # confounding strength axis
#'                                                   name = "Minimum strength of both confounding RRs",
#'                                                   breaks = breaks.x2) ) +
#'         
#'         geom_line(lwd=1.2) +
#'         xlab("Hypothetical average bias factor across studies (RR scale)") +
#'         ylab( paste( ifelse( tail=="above",
#'                              paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
#'                              paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
#'       
#'       # can't compute a CI if the bounds aren't there
#'       no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) ) | (give.CI == FALSE)
#'       
#'       if ( no.CI ){
#'         graphics::plot(p)
#'       } else {
#'         graphics::plot( p + ggplot2::geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15 ) )
#'         
#'         warning("Calculating parametric confidence intervals in the plot. For values of Phat that are less than 0.15 or greater than 0.85, these confidence intervals may not perform well.")
#'       }
#'       
#'       
#'     } ## closes method=="parametric"
#'     
#'     
#'     if ( method == "calibrated" ) {
#'       
#'       # if tail isn't provided, assume user wants the more extreme one (away from the null)
#'       if ( is.na(tail) ) {
#'         calib = calib_ests( yi = dat[[yi.name]], 
#'                             sei = sqrt( dat[[vi.name]] ) )
#'         
#'         tail = ifelse( median(calib) > log(1), "above", "below" )
#'         warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
#'       }
#'       
#'       res = data.frame( B = seq(Bmin, Bmax, .01) )
#'       
#'       # evaluate Phat causal at each value of B
#'       res = res %>% rowwise() %>%
#'         mutate( Phat = Phat_causal( q = q, 
#'                                     B = B,
#'                                     tail = tail,
#'                                     dat = dat,
#'                                     yi.name = yi.name,
#'                                     vi.name = vi.name ) ) 
#'       
#'       if ( give.CI == TRUE ) {
#'         require(boot)
#'         # look at just the values of B at which Phat jumps
#'         #  this will not exceed the number of point estimates in the meta-analysis
#'         # first entry should definitely be bootstrapped, so artificially set its diff to nonzero value
#'         diffs = c( 1, diff(res$Phat) )  
#'         res.short = res[ diffs != 0, ]
#'         
#'         require(dplyr)
#'         
#'         
#'         # bootstrap a CI for each entry in res.short
#'         res.short = res.short %>% rowwise() %>%
#'           mutate( Phat_CI_lims(.B = B,
#'                                q = q,
#'                                tail = tail,
#'                                dat = dat,
#'                                yi.name = yi.name,
#'                                vi.name = vi.name ) )
#'         
#'         # merge this with the full-length res dataframe, merging by Phat itself
#'         res = merge( res, res.short, by = "Phat", all.x = TRUE )
#'         
#'         res = res %>% rename( B = B.x )
#'         
#'         # @@need to test
#'         if ( any( res$lo > res$Phat ) | any( res$hi < res$Phat ) ) {
#'           warning( paste( "Some of the pointwise confidence intervals do not contain the proportion estimate itself. This reflects instability in the bootstrapping process. See the other warnings for details." ) )
#'         }
#'       }
#'       
#'       #browser()
#'       
#'       #bm
#'       p = ggplot2::ggplot( data = res,
#'                            aes( x = B,
#'                                 y = Phat ) ) +
#'         theme_bw() +
#'         
#'         
#'         scale_y_continuous( limits=c(0,1),
#'                             breaks=seq(0, 1, .1)) +
#'         scale_x_continuous(  breaks = breaks.x1,
#'                              sec.axis = sec_axis( ~ g(.),  # confounding strength axis
#'                                                   name = "Minimum strength of both confounding RRs",
#'                                                   breaks = breaks.x2) ) +
#'         geom_line(lwd=1.2) +
#'         
#'         xlab("Hypothetical bias factor in all studies (RR scale)") +
#'         ylab( paste( ifelse( tail=="above",
#'                              paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
#'                              paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
#'       
#'       
#'       
#'       if ( give.CI == TRUE ) {
#'         p = p + geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15, fill = "black" )
#'       }
#'       
#'       graphics::plot(p)
#'     }  # closes method == "calibrated"
#'     
#'   } ## closes type=="line"
#' } ## closes sens_plot function
#' 
#' # fn of B; everything else is taken as a global var
#' # @put as separate internal fn
#' Phat_CI_lims = function(.B,
#'                         q,
#'                         tail,
#'                         dat,
#'                         yi.name,
#'                         vi.name) {
#'   
#'   tryCatch({
#'     boot.res = suppressWarnings( boot( data = dat,
#'                                        parallel = "multicore",
#'                                        R = R, 
#'                                        statistic = function(original, indices) {
#'                                          
#'                                          # draw bootstrap sample
#'                                          b = original[indices,]
#'                                          
#'                                          Phatb = suppressWarnings( Phat_causal( q = q, 
#'                                                                                 B = .B,
#'                                                                                 tail = tail,
#'                                                                                 dat = b,
#'                                                                                 yi.name = yi.name,
#'                                                                                 vi.name = vi.name) )
#'                                          return(Phatb)
#'                                        } ) )
#'     
#'     bootCIs = boot.ci(boot.res,
#'                       type="bca",
#'                       conf = CI.level )
#'     
#'     lo = bootCIs$bca[4]
#'     hi = bootCIs$bca[5]
#'     
#'   }, error = function(err) {
#'     lo <<- NA
#'     hi <<- NA
#'   })
#'   
#'   # return as data frame to play well with rowwise() and mutate()
#'   return( data.frame( lo, hi ) )
#' }
#' 
#' 
#' 
#' Phat_causal = function( q,
#'                         B,
#'                         tail,
#'                         
#'                         dat,
#'                         yi.name,
#'                         vi.name) {
#'   
#'   if ( ! yi.name %in% names(dat) ) stop("dat does not contain a column named yi.name")
#'   if ( ! vi.name %in% names(dat) ) stop("dat does not contain a column named vi.name")
#'   
#'   calib = MetaUtility::calib_ests( yi = dat[[yi.name]],
#'                                    sei = sqrt(dat[[vi.name]] ) )
#'   
#'   # confounding-adjusted calibrated estimates
#'   # always shift the estimates in the direction that will DECREASE the proportion
#'   if ( tail == "above" ) calib.t = calib - B
#'   if ( tail == "below" ) calib.t = calib +B
#'   
#'   # confounding-adjusted Phat
#'   if ( tail == "above" ) Phat.t = mean( calib.t > q )
#'   if ( tail == "below" ) Phat.t = mean( calib.t < q )
#'   
#'   return(Phat.t)
#' }
#' 
#' #' define transformation in a way that is monotonic over the effective range of B (>1)
#' #' to avoid ggplot errors in sens_plot
#' #' helper function for confounded_meta
#' #' 
#' g = Vectorize( function(x) {
#'   if (x < 1) return( x / 1e10 )
#'   x + sqrt( x^2 - x )
#' } )










#### DEBUG ###

#' ###helper functions (Phat_causal, That_causal, That_causal_bt, g, logHR_to_logRR) from Maya code:
#' #' helper function for confounded_meta
#' #' 
#' Phat_causal = function( q,
#'                         B,
#'                         tail,
#'                         
#'                         dat,
#'                         yi.name,
#'                         vi.name) {
#'   
#'   if ( ! yi.name %in% names(dat) ) stop("dat does not contain a column named yi.name")
#'   if ( ! vi.name %in% names(dat) ) stop("dat does not contain a column named vi.name")
#'   
#'   calib = MetaUtility::calib_ests( yi = dat[[yi.name]],
#'                                    sei = sqrt(dat[[vi.name]] ) )
#'   
#'   # confounding-adjusted calibrated estimates
#'   # always shift the estimates in the direction that will DECREASE the proportion
#'   if ( tail == "above" ) calib.t = calib - B
#'   if ( tail == "below" ) calib.t = calib +B
#'   
#'   # confounding-adjusted Phat
#'   if ( tail == "above" ) Phat.t = mean( calib.t > q )
#'   if ( tail == "below" ) Phat.t = mean( calib.t < q )
#'   
#'   return(Phat.t)
#' }
#' 
#' 
#' #' define transformation in a way that is monotonic over the effective range of B (>1)
#' #' to avoid ggplot errors
#' #' helper function for confounded_meta
#' #' 
#' g = Vectorize( function(x) {
#'   if (x < 1) return( x / 1e10 )
#'   x + sqrt( x^2 - x )
#' } )
#' 
#' # @@ needed?
#' logHR_to_logRR = function(logRR){
#'   log( ( 1 - 0.5^sqrt( exp(logRR) ) ) / ( 1 - 0.5^sqrt( 1 / exp(logRR) ) ) )
#' }
#' 
#' Tmin_causal = function( q,
#'                         r,
#'                         tail,
#'                         
#'                         dat,
#'                         yi.name,
#'                         vi.name ) {
#'   
#'   # # test only
#'   # dat = d
#'   # calib.temp = MetaUtility::calib_ests(yi = d$yi,
#'   #                                      sei = sqrt(d$vyi))
#'   # q = quantile(calib.temp, 0.8)
#'   # r = 0.3
#'   # yi.name = "yi"
#'   # vi.name = "vyi"
#'   # tail = "above"
#'   
#'   
#'   # here, check if any shifting is actually needed
#'   # current Phat
#'   Phatc = Phat_causal(q = q,
#'                       B = 0,
#'                       tail = tail,
#'                       dat = dat,
#'                       yi.name = yi.name,
#'                       vi.name = vi.name)
#'   if ( Phatc <= r ){
#'     # this warning is now in confounded_meta
#'     #warning("Phat is already less than or equal to r even with no confounding, so Tmin is not applicable. No confounding at all is required to make the specified shift.")
#'     return(1)
#'   }
#'   
#'   # evaluate the ECDF of the unshifted calib at those calib themselves
#'   #  to get the possible values that Phat can take
#'   #  this approach handles ties
#'   calib = sort( calib_ests( yi = dat[[yi.name]], sei = sqrt(dat[[vi.name]]) ) )
#'   Phat.options = unique( ecdf(calib)(calib) )
#'   # always possible to choose 0
#'   Phat.options = c(Phat.options, 0)
#'   
#'   # of Phats that are <= r, find the largest one (i.e., closest to r)
#'   Phat.target = max( Phat.options[ Phat.options <= r ] ) 
#'   
#'   
#'   # find calib.star, the calibrated estimate that needs to move to q
#'   # example for tail == "above":
#'   # calib.star is the largest calibrated estimate that needs to move to just
#'   #  BELOW q after shifting
#'   # k * Phat.target is the number of calibrated estimates that should remain
#'   #  ABOVE q after shifting
#'   k = length(calib)
#'   if ( tail == "above" ) calib.star = calib[ k - (k * Phat.target) ]
#'   if ( tail == "below" ) calib.star = calib[ (k * Phat.target) + 1 ]
#'   
#'   # pick the bias factor that shifts calib.star to q
#'   #  and then add a tiny bit (0.001) to shift calib.star to just
#'   # below or above q
#'   # if multiple calibrated estimates are exactly equal to calib.star, 
#'   #  all of these will be shifted just below q (if tail == "above")
#'   ( Tmin = exp( abs(calib.star - q) + 0.001 ) )
#'   
#'   return(as.numeric(Tmin))
#' }
#' 
#' 
#' # fn of B; everything else is taken as a global var
#' # @put as separate internal fn
#' Phat_CI_lims = function(.B,
#'                         q,
#'                         tail,
#'                         dat,
#'                         yi.name,
#'                         vi.name) {
#'   
#'   tryCatch({
#'     boot.res = suppressWarnings( boot( data = dat,
#'                                        parallel = "multicore",
#'                                        R = R, 
#'                                        statistic = function(original, indices) {
#'                                          
#'                                          # draw bootstrap sample
#'                                          b = original[indices,]
#'                                          
#'                                          Phatb = suppressWarnings( Phat_causal( q = q, 
#'                                                                                 B = .B,
#'                                                                                 tail = tail,
#'                                                                                 dat = b,
#'                                                                                 yi.name = yi.name,
#'                                                                                 vi.name = vi.name) )
#'                                          return(Phatb)
#'                                        } ) )
#'     
#'     bootCIs = boot.ci(boot.res,
#'                       type="bca",
#'                       conf = CI.level )
#'     
#'     lo = bootCIs$bca[4]
#'     hi = bootCIs$bca[5]
#'     
#'   }, error = function(err) {
#'     lo <<- NA
#'     hi <<- NA
#'   })
#'   
#'   # return as data frame to play well with rowwise() and mutate()
#'   return( data.frame( lo, hi ) )
#' }
#' 
#' 
#' confounded_meta = function( method="calibrated",  # for both methods
#'                             q,
#'                             r = NA,
#'                             CI.level = 0.95,
#'                             tail = NA,
#'                             muB = NA,
#'                             R = 2000,
#'                             
#'                             # only for parametric
#'                             sigB = NA,
#'                             yr = NA,
#'                             vyr = NA,
#'                             t2 = NA,
#'                             vt2 = NA,
#'                             
#'                             # only for calibrated
#'                             give.CI = TRUE,
#'                             dat = NA,
#'                             yi.name = NA,
#'                             vi.name = NA) {
#'   
#'   
#'   # # test only
#'   # method="calibrated"
#'   # q=median(d$calib)
#'   # tail = "above"
#'   # muB=0
#'   # r=0.1
#'   # q = 0.2
#'   # R = 250
#'   # CI.level = 0.95
#'   # 
#'   # give.CI=TRUE
#'   # dat = d
#'   # yi.name = "yi"
#'   # vi.name = "vyi"
#'   
#'   
#'   
#'   
#'   ##### Check for Bad Input - Common to Parametric and Calibrated Methods #####
#'   if ( ! is.na(r) ) {
#'     if (r < 0 | r > 1) stop("r must be between 0 and 1")
#'   }
#'   
#'   if ( is.na(r) ) message("Cannot compute Tmin or Gmin without r. Returning only prop.")
#'   
#'   
#'   ##### PARAMETRIC #####
#'   if (method=="parametric"){
#'     
#'     
#'     ##### Check for Bad Input #####
#'     if ( t2 < 0 ) stop("Heterogeneity cannot be negative")
#'     if ( !is.na(sigB) & sigB < 0 ) stop("Bias factor variance cannot be negative")
#'     
#'     # the second condition is needed for Shiny app:
#'     #  if user deletes the input in box, then it's NA instead of NULL
#'     if ( ! is.na(vyr) ) {
#'       if (vyr < 0) stop("Variance of point estimate cannot be negative")
#'     }
#'     
#'     if ( ! is.na(vt2) ) {
#'       if (vt2 < 0) stop("Variance of heterogeneity cannot be negative")
#'     }
#'     
#'     if ( t2 <= sigB^2 ) stop("Must have t2 > sigB^2")
#'     
#'     ##### Messages When Not All Output Can Be Computed #####
#'     if ( is.na(vyr) | is.na(vt2) ) message("Cannot compute inference without vyr and vt2. Returning only point estimates.")
#'     
#'     ##### Point Estimates: Causative Case #####
#'     # if tail isn't provided, assume user wants the more extreme one (away from the null)
#'     if ( is.na(tail) ) {
#'       tail = ifelse( yr > log(1), "above", "below" )
#'       warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
#'     }
#'     
#'     # bias-corrected mean depends on whether yr is causative, NOT on the desired tail
#'     # @@make sure Phat_causal is consistent with this
#'     if ( yr > log(1) ) {
#'       yr.corr = yr - muB
#'     } else {
#'       yr.corr = yr + muB
#'     }
#'     
#'     if ( tail == "above" ) {
#'       
#'       if ( !is.na(muB) ) {
#'         # prop above
#'         Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
#'         Phat = 1 - pnorm(Z) 
#'       } else {
#'         Phat = NA
#'       }
#'       
#'       if ( !is.na(r) ) {
#'         
#'         # min bias factor
#'         # the max is there in case no bias is needed
#'         # (i.e., proportion of effects > q already < r without confounding)
#'         Tmin = max( 1, exp( qnorm(1-r) * sqrt(t2) - q + yr ) )
#'         
#'         # min confounding strength
#'         # suppress warnings to avoid warnings about NaN when term inside sqrt is negative
#'         Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
#'       } else {
#'         Tmin = Gmin = NA
#'       }
#'     }
#'     
#'     ##### Point Estimates: Preventive Case #####
#'     if ( tail == "below" ) {
#'       
#'       if ( !is.na(muB) ) {
#'         # prop below
#'         Z = ( q - yr.corr ) / sqrt( t2 - sigB^2 )
#'         Phat = pnorm(Z) 
#'       } else {
#'         Phat = NA
#'       }
#'       
#'       if ( !is.na(r) ) {
#'         # min bias factor
#'         Tmin = exp( q - yr - qnorm(r) * sqrt(t2) )
#'         
#'         # min confounding strength
#'         Gmin = suppressWarnings( Tmin + sqrt( Tmin^2 - Tmin ) )
#'       } else {
#'         Tmin = Gmin = NA
#'       }
#'     }
#'     
#'     ##### Delta Method Inference: P-Hat #####
#'     # do inference only if given needed SEs
#'     if ( !is.na(vyr) & !is.na(vt2) & !is.na(muB) ){
#'       
#'       # term in numerator depends on whether causative or preventive RR
#'       num.term = ifelse( yr > log(1), q + muB - yr, q - muB - yr )
#'       
#'       term1.1 = vyr / (t2 - sigB^2 )
#'       term1.2 = ( vt2 * (num.term)^2 ) / ( 4 * (t2 - sigB^2 )^3 )
#'       term1 = sqrt( term1.1 + term1.2 )
#'       
#'       Z = num.term / sqrt( t2 - sigB^2 )
#'       SE.Phat = term1 * dnorm(Z)
#'       
#'       # confidence interval
#'       tail.prob = ( 1 - CI.level ) / 2
#'       lo.Phat = max( 0, Phat + qnorm( tail.prob )*SE.Phat )
#'       hi.Phat = min( 1, Phat - qnorm( tail.prob )*SE.Phat )
#'       
#'       # warn if bootstrapping needed
#'       # @ change to recommending calibrated?
#'       if ( Phat < 0.15 | Phat > 0.85 ) warning("Phat is close to 0 or 1. We recommend using bias-corrected and accelerated bootstrapping to estimate all inference in this case.")
#'       
#'     } else {
#'       SE.Phat = lo.Phat = hi.Phat = NA
#'     }
#'     
#'     ##### Delta Method Inference: Tmin and Gmin #####
#'     # do inference only if given needed SEs and r
#'     if ( !is.na(vyr) & !is.na(vt2) & !is.na(r) ){
#'       
#'       ##### Tmin #####
#'       if (yr > log(1) ) {
#'         term = ( vt2 * qnorm(1-r)^2 ) / ( 4 * t2 )
#'         SE.T = exp( sqrt(t2) * qnorm(1-r) - q + yr ) * sqrt( vyr + term  )
#'       } else {
#'         term = ( vt2 * qnorm(r)^2 ) / ( 4 * t2 )
#'         SE.T = exp( q - yr - sqrt(t2) * qnorm(r) ) * sqrt( vyr + term  )
#'       }
#'       
#'       tail.prob = ( 1 - CI.level ) / 2
#'       lo.T = max( 1, Tmin + qnorm( tail.prob )*SE.T )  # bias factor can't be < 1
#'       hi.T = Tmin - qnorm( tail.prob )*SE.T  # but has no upper bound
#'       
#'       
#'       ##### Gmin #####
#'       SE.G = SE.T * ( 1 + ( 2*Tmin - 1 ) / ( 2 * sqrt( Tmin^2 - Tmin ) ) )
#'       
#'       lo.G = max( 1, Gmin + qnorm( tail.prob )*SE.G )  # confounding RR can't be < 1
#'       hi.G = Gmin - qnorm( tail.prob )*SE.G  # but has no upper bound
#'       
#'     } else {  # i.e., user didn't pass parameters needed for inference
#'       SE.T = SE.G = lo.T = lo.G = hi.T = hi.G = NA
#'     }
#'     
#'   } # closes parametric method
#'   
#'   ##### CALIBRATED #####
#'   if( method == "calibrated" ){
#'     
#'     # bm1
#'     
#'     ##### Check for Bad Input #####
#'     # @@do me
#'     
#'     # if tail isn't provided, assume user wants the more extreme one (away from the null)
#'     if ( is.na(tail) ) {
#'       calib = calib_ests( yi = dat[[yi.name]], 
#'                           sei = sqrt( dat[[vi.name]] ) )
#'       
#'       tail = ifelse( median(calib) > log(1), "above", "below" )
#'       warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
#'     }
#'     
#'     
#'     # initialize
#'     Phat = Tmin = Gmin = SE.Phat = SE.T = SE.G = lo.Phat = lo.T = lo.G = hi.Phat = hi.T = hi.G = NA
#'     
#'     
#'     ##### All Three Point Estimates #####
#'     Phat = Phat_causal( q = q, 
#'                         B = muB,
#'                         tail = tail,
#'                         dat = dat,
#'                         yi.name = yi.name,
#'                         vi.name = vi.name )
#'     
#'     if ( !is.na(r) ) {
#'       Tmin = Tmin_causal(q = q,
#'                          r = r,
#'                          tail = tail,
#'                          dat = dat,
#'                          yi.name = yi.name,
#'                          vi.name = vi.name)
#'       
#'       
#'       Gmin = g(Tmin)
#'     }
#'     
#'     
#'     
#'     ##### All Three Confidence Intervals #####
#'     if ( give.CI == TRUE ) {
#'       
#'       require(boot)
#'       
#'       # CI for Phat
#'       boot.res.Phat = suppressWarnings( boot( data = dat,
#'                                               parallel = "multicore",
#'                                               R = R, 
#'                                               statistic = function(original, indices) {
#'                                                 
#'                                                 # draw bootstrap sample
#'                                                 b = original[indices,]
#'                                                 
#'                                                 Phatb = Phat_causal( q = q, 
#'                                                                      B = muB,
#'                                                                      tail = tail,
#'                                                                      dat = b,
#'                                                                      yi.name = yi.name,
#'                                                                      vi.name = vi.name)
#'                                                 return(Phatb)
#'                                               } ) )
#'       
#'       bootCIs.Phat = boot.ci(boot.res.Phat,
#'                              type="bca",
#'                              conf = CI.level )
#'       
#'       lo.Phat = bootCIs.Phat$bca[4]
#'       hi.Phat = bootCIs.Phat$bca[5]
#'       SE.Phat = sd(boot.res.Phat$t)
#'       
#'       
#'       # Tmin and Gmin
#'       if ( !is.na(r) ) {
#'         boot.res.Tmin = suppressWarnings( boot( data = dat,
#'                                                 parallel = "multicore",
#'                                                 R = R, 
#'                                                 statistic = function(original, indices) {
#'                                                   
#'                                                   # draw bootstrap sample
#'                                                   b = original[indices,]
#'                                                   
#'                                                   Tminb = Tmin_causal(q = q,
#'                                                                       r = r,
#'                                                                       tail = tail,
#'                                                                       dat = b,
#'                                                                       yi.name = yi.name,
#'                                                                       vi.name = vi.name)
#'                                                   return(Tminb)
#'                                                 } ) )
#'         
#'         bootCIs.Tmin = boot.ci(boot.res.Tmin,
#'                                type="bca",
#'                                conf = CI.level )
#'         
#'         lo.T = max(1, bootCIs.Tmin$bca[4])  # bias factor can't be < 1
#'         hi.T = bootCIs.Tmin$bca[5]  # but has no upper bound
#'         SE.T = sd(boot.res.Tmin$t)
#'         
#'         
#'         ##### Gmin #####
#'         lo.G = max( 1, g(lo.T) )  # confounding RR can't be < 1
#'         hi.G = g(hi.T)  # but has no upper bound
#'         SE.G = sd( g(boot.res.Tmin$t) )
#'       }
#'     }  # closes "if ( !is.na(r) )"
#'     
#'   } # closes calibrated method
#'   
#'   ##### Messages about Results #####
#'   if ( exists("Tmin") ) {
#'     if ( !is.na(Tmin) & Tmin == 1 ) {
#'       warning("Phat is already less than or equal to r even with no confounding, so Tmin and Gmin are simply equal to 1. No confounding at all is required to make the specified shift.")
#'     }
#'   }  
#'   
#'   
#'   ##### Return Results #####
#'   return( data.frame( Value = c("Prop", "Tmin", "Gmin"), 
#'                       Est = c( Phat, Tmin, Gmin ),
#'                       SE = c(SE.Phat, SE.T, SE.G),
#'                       CI.lo = c(lo.Phat, lo.T, lo.G), 
#'                       CI.hi = c(hi.Phat, hi.T, hi.G) ) )
#'   
#' } # closes confounded_meta function
#' 
#' 
#' sens_plot = function(method="calibrated",
#'                      type,
#'                      q,
#'                      #r=NA,
#'                      CI.level=0.95,
#'                      tail=NA,
#'                      give.CI=TRUE,
#'                      Bmin,
#'                      Bmax,
#'                      breaks.x1=NA,
#'                      breaks.x2=NA,
#'                      
#'                      # for plot type "dist"
#'                      muB,
#'                      
#'                      # for type "line" and method "parametric"
#'                      sigB,
#'                      yr,
#'                      vyr=NA,
#'                      t2,
#'                      vt2=NA,
#'                      
#'                      
#'                      # for type "line" and method "calibrated"
#'                      R=2000,
#'                      dat = NA,
#'                      yi.name = NA,
#'                      vi.name = NA) {
#'   
#'   # # test only
#'   # method="calibrated"
#'   # type = "line"
#'   # q=median(d$calib)
#'   # tail = "above"
#'   # muB=0
#'   # r=0.1
#'   # q = 0.2
#'   # R = 250
#'   # CI.level = 0.95
#'   # 
#'   # give.CI=TRUE
#'   # dat = d
#'   # yi.name = "yi"
#'   # vi.name = "vi"
#'   # Bmin = log(1)
#'   # Bmax = log(5)
#'   # CI.level = 0.95
#'   # tail = "above"
#'   # breaks.x1 = NA
#'   # breaks.x2 = NA
#'   
#'   # method = "parametric"
#'   # type = "line"
#'   # q = log(1.1)
#'   # muB = log(2)
#'   # sigB = 0.1
#'   # yr = log(1.4)
#'   # vyr = 0.5
#'   # t2 = 0.3
#'   # vt2 = 0.02
#'   # r = 0.1
#'   # Bmin = log(1)
#'   # Bmax = log(5)
#'   # CI.level = 0.95
#'   # tail = "above"
#'   # breaks.x1 = NA
#'   # breaks.x2 = NA
#'   
#'   ##### Distribution Plot ######
#'   if ( type=="dist" ) {
#'     
#'     # check for bad input
#'     if( is.na(muB) ) stop("For type='dist', must provide muB")
#'     
#'     if ( ( length(muB) > 1 ) | ( length(sigB) > 1 ) ) {
#'       stop( "For type='dist', muB and sigB must be length 1")
#'     }
#'     
#'     # simulate confounded distribution
#'     reps = 10000
#'     RR.c = exp( rnorm( n=reps, mean=yr, sd=sqrt(t2) ) )
#'     
#'     # simulate unconfounded distribution
#'     Mt = ifelse( yr > 0, yr - muB, yr + muB )
#'     RR.t = exp( rnorm( n=reps, mean=Mt, sd=sqrt(t2-sigB^2) ) )
#'     
#'     # get reasonable limits for X-axis
#'     x.min = min( quantile(RR.c, 0.01), quantile(RR.t, 0.01) )
#'     x.max = max( quantile(RR.c, 0.99), quantile(RR.t, 0.99) )
#'     
#'     temp = data.frame( group = rep( c( "Observed", "True" ), each = reps ), 
#'                        val = c( RR.c, RR.t ) )
#'     
#'     colors=c("black", "orange")
#'     p = ggplot2::ggplot( data=temp, aes(x=temp$val, group=temp$group ) ) +
#'       geom_density( aes( fill=temp$group ), alpha=0.4 ) +
#'       theme_bw() + xlab("Study-specific relative risks") +
#'       ylab("") + guides(fill=guide_legend(title=" ")) +
#'       scale_fill_manual(values=colors) +
#'       geom_vline( xintercept = exp(q), lty=2, color="red" ) +
#'       scale_x_continuous( limits=c(x.min, x.max), breaks = seq( round(x.min), round(x.max), 0.5) ) +
#'       ggtitle("Observed and true relative risk distributions")
#'     
#'     graphics::plot(p)
#'   }
#'   
#'   ##### Line Plot ######
#'   if ( type=="line" ) {
#'     
#'     
#'     # compute axis tick points for both X-axes
#'     if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
#'     if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
#'     
#'     
#'     if ( method=="parametric" ) {
#'       
#'       # yr = log(1.2)
#'       # t2 = 0.1
#'       # q = 1.1
#'       # vyr = 0.01
#'       # vt2 = 0.1
#'       # muB = 1.5
#'       # sigB = 0
#'       # r = 0.2
#'       # tail = NA
#'       # method = "parametric"
#'       # Bmin=log(1)
#'       # Bmax=log(2)
#'       # CI.level=0.95
#'       # breaks.x1=NA
#'       # breaks.x2=NA
#'       # give.CI=TRUE
#'       
#'       
#'       if ( is.na(tail) ) {
#'         tail = ifelse( yr > log(1), "above", "below" )
#'         warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
#'       }
#'       
#'       if ( is.na(vyr) | is.na(vt2) ) {
#'         message( "No confidence interval because vyr or vt2 is NULL")
#'       }
#'       
#'       # get mean bias factor values for a vector of B's from Bmin to Bmax
#'       t = data.frame( B = seq(Bmin, Bmax, .01), phat = NA, lo = NA, hi = NA )
#'       t$eB = exp(t$B)
#'       
#'       for ( i in 1:dim(t)[1] ) {
#'         # r is irrelevant here
#'         # suppress warnings about Phat being close to 0 or 1
#'         # browser()
#'         cm = suppressWarnings( confounded_meta( method=method,
#'                                                 q=q,
#'                                                 r=r,
#'                                                 muB=t$B[i],
#'                                                 sigB=sigB,
#'                                                 yr=yr,
#'                                                 vyr=vyr,
#'                                                 t2=t2,
#'                                                 vt2=vt2,
#'                                                 CI.level=CI.level,
#'                                                 tail=tail ) )
#'         
#'         t$phat[i] = cm$Est[ cm$Value=="Prop" ]
#'         t$lo[i] = cm$CI.lo[ cm$Value=="Prop" ]
#'         t$hi[i] = cm$CI.hi[ cm$Value=="Prop" ]
#'       }
#'       
#'       # # compute axis tick points for both X-axes
#'       # if ( any( is.na(breaks.x1) ) ) breaks.x1 = seq( exp(Bmin), exp(Bmax), .5 )
#'       # if ( any( is.na(breaks.x2) ) ) breaks.x2 = round( breaks.x1 + sqrt( breaks.x1^2 - breaks.x1 ), 2)
#'       
#'       p = ggplot2::ggplot( t, aes(x=eB,
#'                                   y=phat ) ) +
#'         theme_bw() +
#'         
#'         scale_y_continuous( limits=c(0,1),
#'                             breaks=seq(0, 1, .1)) +
#'         
#'         scale_x_continuous(  breaks = breaks.x1,
#'                              sec.axis = sec_axis( ~ g(.),  # confounding strength axis
#'                                                   name = "Minimum strength of both confounding RRs",
#'                                                   breaks = breaks.x2) ) +
#'         
#'         geom_line(lwd=1.2) +
#'         xlab("Hypothetical average bias factor across studies (RR scale)") +
#'         ylab( paste( ifelse( tail=="above",
#'                              paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
#'                              paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
#'       
#'       # can't compute a CI if the bounds aren't there
#'       no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) ) | (give.CI == FALSE)
#'       
#'       if ( no.CI ){
#'         graphics::plot(p)
#'       } else {
#'         graphics::plot( p + ggplot2::geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15 ) )
#'         
#'         warning("Calculating parametric confidence intervals in the plot. For values of Phat that are less than 0.15 or greater than 0.85, these confidence intervals may not perform well.")
#'       }
#'       
#'       
#'     } ## closes method=="parametric"
#'     
#'     
#'     if ( method == "calibrated" ) {
#'       
#'       ### testing
#'           # method = "calibrated"
#'           #       type="line"
#'           #       tail="above"
#'           #       muB=0
#'           #       r=0.1
#'           #       q=0.2
#'           #       R=250
#'           #       CI.level = 0.95
#'           #       give.CI = TRUE
#'           #       dat=metafor::escalc(measure="RR",
#'           #                           ai=tpos,
#'           #                           bi=tneg,
#'           #                           ci=cpos,
#'           #                           di=cneg,
#'           #                           data=metafor::dat.bcg)
#'           #       yi.name = "yi"
#'           #       vi.name="vi"
#'           #       Bmin=log(1)
#'           #       Bmax=log(5)
#'           #       breaks.x1=NA
#'           #       breaks.x2=NA
#'       
#'       # if tail isn't provided, assume user wants the more extreme one (away from the null)
#'       if ( is.na(tail) ) {
#'         calib = calib_ests( yi = dat[[yi.name]], 
#'                             sei = sqrt( dat[[vi.name]] ) )
#'         
#'         tail = ifelse( median(calib) > log(1), "above", "below" )
#'         warning( paste( "Assuming you want tail =", tail, "because it wasn't specified") )
#'       }
#'       
#'       res = data.frame( B = seq(Bmin, Bmax, .01) )
#'       
#'       # evaluate Phat causal at each value of B
#'       res = res %>% rowwise() %>%
#'         mutate( Phat = Phat_causal( q = q, 
#'                                     B = B,
#'                                     tail = tail,
#'                                     dat = dat,
#'                                     yi.name = yi.name,
#'                                     vi.name = vi.name ) ) 
#'       
#'       if ( give.CI == TRUE ) {
#'         require(boot)
#'         # look at just the values of B at which Phat jumps
#'         #  this will not exceed the number of point estimates in the meta-analysis
#'         # first entry should definitely be bootstrapped, so artificially set its diff to nonzero value
#'         diffs = c( 1, diff(res$Phat) )  
#'         res.short = res[ diffs != 0, ]
#'         
#'         require(dplyr)
#'         
#'         
#'         # bootstrap a CI for each entry in res.short
#'         res.short = res.short %>% rowwise() %>%
#'           mutate( CI= Phat_CI_lims(.B = B,
#'                                q = q,
#'                                tail = tail,
#'                                dat = dat,
#'                                yi.name = yi.name,
#'                                vi.name = vi.name ) )
#'         
#'         # merge this with the full-length res dataframe, merging by Phat itself
#'         res = merge( res, res.short, by = "Phat", all.x = TRUE )
#'         
#'         res = res %>% rename( B = B.x )
#'         
#'         # @@need to test
#'         if ( any( res$lo > res$Phat ) | any( res$hi < res$Phat ) ) {
#'           warning( paste( "Some of the pointwise confidence intervals do not contain the proportion estimate itself. This reflects instability in the bootstrapping process. See the other warnings for details." ) )
#'         }
#'       }
#'       
#'       #browser()
#'       
#'       #bm
#'       p = ggplot2::ggplot( data = res,
#'                            aes( x = B,
#'                                 y = Phat ) ) +
#'         theme_bw() +
#'         
#'         
#'         scale_y_continuous( limits=c(0,1),
#'                             breaks=seq(0, 1, .1)) +
#'         scale_x_continuous(  breaks = breaks.x1,
#'                              sec.axis = sec_axis( ~ g(.),  # confounding strength axis
#'                                                   name = "Minimum strength of both confounding RRs",
#'                                                   breaks = breaks.x2) ) +
#'         geom_line(lwd=1.2) +
#'         
#'         xlab("Hypothetical bias factor in all studies (RR scale)") +
#'         ylab( paste( ifelse( tail=="above",
#'                              paste( "Estimated proportion of studies with true RR >", round( exp(q), 3 ) ),
#'                              paste( "Estimated proportion of studies with true RR <", round( exp(q), 3 ) ) ) ) )
#'       
#'       
#'       
#'       if ( give.CI == TRUE ) {
#'         p = p + geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15, fill = "black" )
#'       }
#'       
#'       graphics::plot(p)
#'     }  # closes method == "calibrated"
#'     
#'   } ## closes type=="line"
#' } ## closes sens_plot function
#' 
#' 
#' 
#' 
#' 
#' ### look at output of Maya's updated confounded_meta:
#' # sim_data2 = function( k, # total number of studies
#' #                       m = k, # number of clusters (m=k implies no clustering)
#' #                       b0, # intercept
#' #                       bc, # effect of continuous moderator
#' #                       bb, # effect of binary moderator
#' #                       V,  # TOTAL residual heterogeneity after conditioning on moderators (including within- and between-cluster variance)
#' #                       Vzeta = 0, # between-cluster variance (must be less than V)
#' #                       muN,
#' #                       minN,
#' #                       sd.w,
#' #                       true.effect.dist) {
#' # 
#' #   # # @test for m=k case
#' #   # # TEST ONLY
#' #   # k = 3
#' #   # m = 3
#' #   # b0 = 0 # intercept
#' #   # bc = 0 # effect of continuous moderator
#' #   # bb = 0 # effect of binary moderator
#' #   # V = .5
#' #   # Vzeta = 0.25
#' #   # muN = 100
#' #   # minN = 50
#' #   # sd.w = 1
#' #   # true.effect.dist = "expo"
#' # 
#' # 
#' #   if ( Vzeta > V ) stop( "Vzeta must be less than or equal to V" )
#' # 
#' # 
#' #   # assign studies to clusters
#' #   # each in own cluster:
#' #   if (m == k) cluster = 1:k  # each in its own cluster
#' #   # k not divisble by m:
#' #   # fine if k isn't divisible by m (number of clusters); clusters will just be unbalanced and actual number of cluster might be less than m
#' #   #if ( m < k ) cluster = sample( 1:m, size = k, replace = TRUE )
#' #   # k not divisible by m: assign each observation to a cluster chosen at random (unbalanced clusters)
#' #   if ( m < k & (k %% m != 0) ) cluster = sample( 1:m, size = k, replace = TRUE )
#' #   # k divisible by m: assign observations to clusters in a balanced way
#' #   if ( m < k & (k %% m == 0) ) cluster = rep(1:m, each = k/m)
#' # 
#' #   if (m > k) stop("m must be <= k")
#' # 
#' #   cluster = sort(cluster)
#' # 
#' #   # generate cluster random intercepts (zeta)
#' #   # these are normal even when true effect dist is exponential
#' #   zeta1 = rnorm( n = m, mean = 0, sd = sqrt( Vzeta ) )  # one entry per cluster
#' #   zeta1i = zeta1[cluster]  # one entry per study
#' # 
#' # 
#' #   # simulate k studies
#' #   for (i in 1:k) {
#' #     study = sim_one_study2( b0, # intercept
#' #                             bc, # effect of continuous moderator
#' #                             bb, # effect of binary moderator
#' #                             V = V,
#' #                             Vzeta = Vzeta,
#' #                             zeta1 = zeta1i[i], # cluster random intercept for this study's cluster
#' #                             muN = muN,
#' #                             minN = minN,
#' #                             sd.w = sd.w,
#' #                             true.effect.dist = true.effect.dist)
#' # 
#' #     if ( i == 1 ) d = study else d = rbind( d, study )
#' #   }
#' # 
#' #   # add cluster indicator
#' #   d = d %>% mutate( cluster, .before = 1)
#' # 
#' #   # ICC of study population effects within clusters (will be static for dataset)
#' #   d$icc = ICCbareF( x = as.factor(d$cluster),
#' #                     y = d$Mi )
#' # 
#' #   return(d)
#' # }
#' # 
#' # # potentially with clustering
#' # sim_one_study2 = function(b0, # intercept
#' #                           bc, # effect of continuous moderator
#' #                           bb, # effect of binary moderator
#' #                           V,
#' #                           Vzeta, # used to calcuate within-cluster variance
#' #                           zeta1,  # scalar cluster random intercept for this study's cluster
#' #                           muN,
#' #                           minN,
#' #                           sd.w,
#' #                           true.effect.dist = "normal"
#' # ) {
#' # 
#' #   # # @test for m=1 case
#' #   # # TEST ONLY
#' #   # b0 = 0.5 # intercept
#' #   # bc = 0.5 # effect of continuous moderator
#' #   # bb = 1 # effect of binary moderator
#' #   # V = .5
#' #   # Vzeta = 0.25
#' #   # zeta1 = -0.2
#' #   # muN = 100
#' #   # minN = 50
#' #   # sd.w = 1
#' #   # true.effect.dist = "normal"
#' # 
#' #   if( !true.effect.dist %in% c("normal", "expo") ) stop("True effect dist not recognized")
#' # 
#' #   ##### Simulate Sample Size and Fixed Design Matrix for This Study #####
#' #   # simulate total N for this study
#' #   N = round( runif( n = 1, min = minN, max = minN + 2*( muN - minN ) ) ) # draw from uniform centered on muN
#' # 
#' #   # simulate study-level moderators (each a scalar)
#' #   Zc = rnorm( n = 1, mean = 0, sd = 1)
#' #   Zb = rbinom( n = 1, size = 1, prob = 0.5)
#' # 
#' #   # mean (i.e., linear predictor) conditional on the moderators and cluster membership
#' #   mu = b0 + zeta1 + bc*Zc + bb*Zb
#' #   # all that follows is that same as in NPPhat, except incorporating clustering as in SAPB
#' # 
#' #   ##### Draw a Single Population True Effect for This Study #####
#' #   if ( true.effect.dist == "normal" ) {
#' #     Mi = rnorm( n=1, mean=mu, sd=sqrt(V - Vzeta) )
#' #   }
#' #   if ( true.effect.dist == "expo" ) {
#' #     # within-cluster variance = total - between
#' #     Vwithin = V - Vzeta
#' #     # set the rate so the heterogeneity is correct
#' #     Mi = rexp( n = 1, rate = sqrt(1/Vwithin) )
#' #     # now the mean is sqrt(V) rather than mu
#' #     # shift to have the correct mean (in expectation)
#' #     Mi = Mi + (mu - sqrt(Vwithin))
#' #   }
#' # 
#' #   ###### Simulate Data For Individual Subjects ######
#' #   # group assignments
#' #   X = c( rep( 0, N/2 ), rep( 1, N/2 ) )
#' # 
#' #   # simulate continuous outcomes
#' #   # 2-group study of raw mean difference with means 0 and Mi in each group
#' #   # and same SD
#' #   Y = c( rnorm( n = N/2, mean = 0, sd = sd.w ),
#' #          rnorm( n = N/2, mean = Mi, sd = sd.w ) )
#' # 
#' #   # calculate ES for this study using metafor (see Viechtbauer "Conducting...", pg 10)
#' #   require(metafor)
#' #   ES = escalc( measure="SMD",
#' #                n1i = N/2,
#' #                n2i = N/2,
#' #                m1i = mean( Y[X==1] ),
#' #                m2i = mean( Y[X==0] ),
#' #                sd1i = sd( Y[X==1] ),
#' #                sd2i = sd( Y[X==0] ) )
#' #   yi = ES$yi
#' #   vyi = ES$vi
#' # 
#' #   return( data.frame( Mi, # study's true effect size; if within-cluster heterogeneity is zero, will be equal to mu
#' #                       mu, # study's linear predictor conditional on the moderators and cluster membership
#' #                       zeta1,
#' #                       Zc,
#' #                       Zb,
#' #                       yi,
#' #                       vyi ) )
#' # }
#' # 
#' # require(ICC)
#' # d = sim_data2( k = 20,
#' #                m = 20,
#' #                b0 = log(.9), # intercept
#' #                bc = 0, # effect of continuous moderator
#' #                bb = 0, # effect of binary moderator
#' #                V = 1,
#' #                Vzeta = 0, # used to calcuate within-cluster variance
#' # 
#' #                muN = 100,
#' #                minN = 100,
#' #                sd.w = 1,
#' #                true.effect.dist = "normal" )
#' # 
#' # ind = sample( 1: nrow(d), size = 40, replace = TRUE )
#' # 
#' # d = d[ ind, ]
#' # 
#' # r = .2
#' # muB = log(2)
#' # 
#' # # choose q to be the 10th percentile of naive calibrated estimates
#' # # so that no confounding should be needed
#' # calib = MetaUtility::calib_ests(yi = d$yi,
#' #                                 sei = sqrt(d$vyi) )
#' # 
#' # q = quantile(calib, .9)
#' # 
#' # x = confounded_meta(method="calibrated",
#' #                     q=q,
#' #                     r=r,
#' #                     tail = "above",
#' #                     muB=0,
#' #                     
#' #                     give.CI=FALSE,
#' #                     dat = d,
#' #                     yi.name = "yi",
#' #                     vi.name = "vyi")
#' # 
#' # # calculate the correct Phat
#' # # might not be exactly .1 due to ties
#' # Phat = mean(calib > q)
#' # 
#' # expect_equal( x$Est[x$Value == "Prop"], Phat )
#' # expect_equal( x$Est[x$Value == "Tmin"], 1 )
#' # expect_equal( x$Est[x$Value == "Gmin"], 1 )