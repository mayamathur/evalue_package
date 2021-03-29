
# no need to source helper_testthat.R explicitly to pass CRAN checks
#  because helper files that start with "helper" automatically 
#  get sourced first: https://testthat.r-lib.org/reference/test_dir.html

# for local testing:
library(testthat)
library(devtools)
library(dplyr)
library(ICC)
library(msm)
library(MetaUtility)
library(here())
setwd(here())
setwd("~/Dropbox/Personal computer/Independent studies/R packages/EValue package (git)/evalue_package/EValue")
setwd("tests")
source("helper_testthat.R")



###################### EVALUES FOR EFFECT MODIFICATION (INTERACTION CONTRAST) ######################

# ~ RDt_var ----------------------
# sanity check for symmetry

test_that("RDt_var, test set #1", {
  
  # test for symmetry
  v1 = RDt_var( f = .25,
                p1 = 0.3,
                p0 = 0.1,
                n1 = 50,
                n0 = 100,
                .maxB = 2 )
  
  # here I've changed argument order to reverse sign of RD
  v2 = RDt_var( f = .25,
                p0 = 0.3,
                p1 = 0.1,
                n0 = 50,
                n1 = 100,
                .maxB = 2 )
  
  expect_equal(v1, v2)
})

# ~ RDt_bound ----------------------
# sanity check: symmetry when shif1ing strata in opposite directions
# RDw and RDm should match here by symmetry

test_that( "RDt_bound, test set #1", {
  x1 = RDt_bound( p1_1 = 0.6,
                  p1_0 = 0.4,
                  n1_1 = 100,
                  n1_0 = 10,
                  f1 = 0.25,
                  maxB_1 = 2,
                  biasDir_1 = "positive",
                  
                  p0_1 = 0.4,
                  p0_0 = 0.6,
                  n0_1 = 10,
                  n0_0 = 100,
                  f0 = .25,
                  biasDir_0 = "negative",
                  
                  alpha = 0.05 )
  
  expect_equal( x1$RD[1], -x1$RD[2] )
  expect_equal( x1$se[1], x1$se[2] )
  expect_equal( x1$lo[1], -x1$hi[2] )
  expect_equal( x1$lo[2], -x1$hi[1] )
  expect_equal( x1$pval[1], x1$pval[2] )
  
  # sanity check: equality when both strata are the same and are positively biased
  x1 = RDt_bound( p1_1 = 0.6,
                  p1_0 = 0.4,
                  n1_1 = 100,
                  n1_0 = 10,
                  f1 = 0.25,
                  maxB_1 = 1.6,
                  biasDir_1 = "positive",
                  
                  p0_1 = 0.6,
                  p0_0 = 0.4,
                  n0_1 = 100,
                  n0_0 = 10,
                  f0 = .25,
                  maxB_0 = 1.6,
                  biasDir_0 = "positive",
                  
                  alpha = 0.05 )
  
  expect_equal( x1$RD[1], x1$RD[2] )
  expect_equal( x1$se[1], x1$se[2] )
  expect_equal( x1$lo[1], x1$lo[2] )
  expect_equal( x1$hi[1], x1$hi[2] )
  expect_equal( x1$pval[1], x1$pval[2] )
  
  
  # sanity check: equality when both strata are the same and are NEGATIVELY biased
  x1 = RDt_bound( p1_1 = 0.6,
                  p1_0 = 0.4,
                  n1_1 = 100,
                  n1_0 = 10,
                  f1 = 0.25,
                  maxB_1 = 1.6,
                  biasDir_1 = "negative",
                  
                  p0_1 = 0.6,
                  p0_0 = 0.4,
                  n0_1 = 100,
                  n0_0 = 10,
                  f0 = .25,
                  maxB_0 = 1.6,
                  biasDir_0 = "negative",
                  
                  alpha = 0.05 )
  
  expect_equal( x1$RD[1], x1$RD[2] )
  expect_equal( x1$se[1], x1$se[2] )
  expect_equal( x1$lo[1], x1$lo[2] )
  expect_equal( x1$hi[1], x1$hi[2] )
  expect_equal( x1$pval[1], x1$pval[2] )
  
})




# ~ Point estimates from RDt_bound with B=1 should match naive RDs ----------------------

# enter example datasets (Letenneur)
# Y: dementia
# X: low education
# n: sample size

# data for women
nw_1 = 2988
nw_0 = 364
dw = data.frame(  Y = c(1, 1, 0, 0),
                  X = c(1, 0, 1, 0),
                  n = c( 158, 6, nw_1-158, nw_0-6 ) )

# data for men
nm_1 = 1790
nm_0 = 605
dm = data.frame(  Y = c(1, 1, 0, 0),
                  X = c(1, 0, 1, 0),
                  n = c( 64, 17, nm_1-64, nm_0-17 ) )

# P(Y = 1 | X = 1) and P(Y = 1 | X = 0) for women and for men
( pw_1 = dw$n[ dw$X == 1 & dw$Y == 1 ] / sum(dw$n[ dw$X == 1 ]) )
( pw_0 = dw$n[ dw$X == 0 & dw$Y == 1 ] / sum(dw$n[ dw$X == 0 ]) )
( pm_1 = dm$n[ dm$X == 1 & dm$Y == 1 ] / sum(dm$n[ dm$X == 1 ]) )
( pm_0 = dm$n[ dm$X == 0 & dm$Y == 1 ] / sum(dm$n[ dm$X == 0 ]) )

# prevalence of low education among women and among men
fw = nw_1 / (nw_1 + nw_0)
fm = nm_1 / (nm_1 + nm_0)

# confounded estimates
RDw = pw_1 - pw_0
RDm = pm_1 - pm_0


test_that( "Bound from RDt_bound should agree with closed form in paper", {
  B = 2.3
  term1 = (pw_1 - pw_0 * B) * ( fw + (1 - fw) / B )
  term2 = (pm_1 * B - pm_0) * ( fm + (1 - fm) / B )
  
  mine = term1 - term2
  
  x = RDt_bound( p1_1 = pw_1,
                 p1_0 = pw_0,
                 n1_1 = nw_1,
                 n1_0 = nw_0,
                 f1 = fw,
                 biasDir_1 = "positive",
                 maxB_1 = B,
                 
                 p0_1 = pm_1,
                 p0_0 = pm_0,
                 n0_1 = nm_1,
                 n0_0 = nm_0,
                 f0 = fm,
                 biasDir_0 = "negative",
                 maxB_0 = B )
  expect_equal( x$RD[ x$stratum == "effectMod" ], mine, tol = 0.0001 )
  
  
  
  # E-value for 1 stratum from IC_evalue should match R package
  ( evalueEst = IC_evalue_inner( stratum = "1",
                                 varName = "RD",
                                 true = 0,
                                 monotonicBias = FALSE,
                                 
                                 p1_1 = pw_1,
                                 p1_0 = pw_0,
                                 n1_1 = nw_1,
                                 n1_0 = nw_0,
                                 f1 = fw,
                                 
                                 p0_1 = pm_1,
                                 p0_0 = pm_0,
                                 n0_1 = nm_1,
                                 n0_0 = nm_0,
                                 f0 = fm,
                                 
                                 alpha = 0.05 )$evalues$evalue )
  
  
  ( evalueCI = IC_evalue_inner( stratum = "1",
                                varName = "lo",
                                true = 0,
                                monotonicBias = FALSE,
                                
                                p1_1 = pw_1,
                                p1_0 = pw_0,
                                n1_1 = nw_1,
                                n1_0 = nw_0,
                                f1 = fw,
                                
                                p0_1 = pm_1,
                                p0_0 = pm_0,
                                n0_1 = nm_1,
                                n0_0 = nm_0,
                                f0 = fm,
                                
                                alpha = 0.05 )$evalues$evalue )
  
  
  # now try against package:
  evalueOld = EValue::evalues.RD( n11 = dw$n[ dw$X == 1 & dw$Y == 1 ],
                                  n10 = dw$n[ dw$X == 1 & dw$Y == 0 ],
                                  n01 = dw$n[ dw$X == 0 & dw$Y == 1 ],
                                  n00 = dw$n[ dw$X == 0 & dw$Y == 0 ],
                                  true = 0,
                                  alpha = 0.05)
  
  
  # they agree! :D
  # woohoo!!!!!
  expect_equal( evalueOld$est.Evalue, evalueEst$evalue, tol = 0.001 )
  expect_equal( evalueOld$lower.Evalue, evalueCI$evalue, tol = 0.001 )
  
})


# ~~~ Bound from RDt_bound should be symmetric after flipping signs of both RDs ----------------------

test_that( "RDt_bound, test set #3", {
  
  # shift each stratum by different amount
  B1 = 1.5 
  B0 = 1.2
  
  # both RDs > 0
  ( x = RDt_bound( p1_1 = .8,
                   p1_0 = .6,
                   n1_1 = 20,
                   n1_0 = 200,
                   f1 = 0.5,
                   biasDir_1 = "positive",
                   maxB_1 = B1,
                   
                   p0_1 = .8,
                   p0_0 = .75,
                   n0_1 = 30,
                   n0_0 = 40,
                   f0 = 0.5,
                   biasDir_0 = "positive",
                   maxB_0 = B0 ) )
  
  # both RDs < 0
  # strata labels reversed so that the interaction contrast itself remains positive
  ( x2 = RDt_bound( p0_1 = .6,
                    p0_0 = .8,
                    n0_1 = 200,
                    n0_0 = 20,
                    f0 = 0.5,
                    biasDir_0 = "negative",
                    maxB_0 = B1,
                    
                    p1_1 = .75,
                    p1_0 = .8,
                    n1_1 = 40,
                    n1_0 = 30,
                    f1 = 0.5,
                    biasDir_1 = "negative",
                    maxB_1 = B0 ) )
  
  # interaction contrast should be exactly the same 
  expect_equal( x[3, 3:6], x2[3, 3:6] )
  
  # strata are reversed and signs are reversed
  # inference stays same
  # but CIs flip 
  expect_equal( as.numeric( x[1, c("se","pval")] ), as.numeric( x2[2, c("se", "pval")] ) )
  expect_equal( as.numeric( x[2, c("se","pval")] ), as.numeric( x2[1, c("se", "pval")] ) )
  expect_equal( x[1, "RD"], -x2[2, "RD"] )
  expect_equal( x[2, "RD"], -x2[1, "RD"] )
})


# E-value should be the same when flipping strata signs
test_that( "evalues.IC test", {
  x = evalues.IC(  stat = "est",
                   true = 0.1,
                   monotonicBias = FALSE,
                   
                   p1_1 = .5,
                   p1_0 = .3,
                   n1_1 = 100,
                   n1_0 = 20,
                   f1 = .6,
                   
                   p0_1 = .4,
                   p0_0 = .35,
                   n0_1 = 40,
                   n0_0 = 60,
                   f0 = .2,
                   
                   alpha = 0.05 )$evalues
  
  x2 = evalues.IC(  stat = "est",
                    true = 0.1,
                    monotonicBias = FALSE,
                    
                    p0_1 = .3,
                    p0_0 = .5,
                    n0_1 = 20,
                    n0_0 = 100,
                    f0 = .6,
                    
                    p1_1 = .35,
                    p1_0 = .4,
                    n1_1 = 60,
                    n1_0 = 40,
                    f1 = .2,
                    
                    alpha = 0.05 )$evalues
  
  expect_equal( x$evalue, x2$evalue, tol = 0.001 )
  expect_equal( x$biasFactor, x2$biasFactor, tol = 0.001 )
  
} )

#bm: stopped here. focusing on symmetry tests. 


test_that("evalues.IC should warn if E-value is 1", {
  
  # E-value for estimate is 1
  expect_message( evalues.IC(  stat = "est",
                               true = .5,
                               monotonicBias = FALSE,
                               
                               p1_1 = .5,
                               p1_0 = .4,
                               n1_1 = 100,
                               n1_0 = 100,
                               f1 = .5,
                               
                               p0_1 = .5,
                               p0_0 = .42,
                               n0_1 = 100,
                               n0_0 = 100,
                               f0 = .5,
                               
                               alpha = 0.05 ) )
  
  # E-value for CI is 1
  # first check the CI limit
  RDt_bound(    p1_1 = .5,
                p1_0 = .4,
                n1_1 = 100,
                n1_0 = 100,
                f1 = .5,
                
                p0_1 = .5,
                p0_0 = .42,
                n0_1 = 100,
                n0_0 = 100,
                f0 = .5,
                
                # no bias
                maxB_1 = 1,
                maxB_0 = 1,
                biasDir_1 = "positive",
                biasDir_0 = "positive" )
  
  expect_message( evalues.IC(  stat = "CI",
                               true = .5,
                               monotonicBias = FALSE,
                               
                               p1_1 = .5,
                               p1_0 = .4,
                               n1_1 = 100,
                               n1_0 = 100,
                               f1 = .5,
                               
                               p0_1 = .5,
                               p0_0 = .42,
                               n0_1 = 100,
                               n0_0 = 100,
                               f0 = .5,
                               
                               alpha = 0.05 ) )
  
  
})



test_that("evalues.IC should reject bad input"), {
  
  # if monotonicBias is TRUE, must provide monotonicBiasDirection
  expect_error( evalues.IC(  stat = "est",
                             true = 0,
                             monotonicBias = TRUE,
                             
                             p1_1 = .4,
                             p1_0 = .4,
                             n1_1 = 100,
                             n1_0 = 100,
                             f1 = .5,
                             
                             p0_1 = .5,
                             p0_0 = .42,
                             n0_1 = 100,
                             n0_0 = 100,
                             f0 = .5,
                             
                             alpha = 0.05 ) )
  
  # specified monotonicBias = FALSE, so the argument monotonicBiasDirection will be ignored
  expect_warning( evalues.IC(  stat = "est",
                               true = 0,
                               monotonicBias = FALSE,
                               monotonicBiasDirection = "positive",
                               
                               p1_1 = .5,
                               p1_0 = .4,
                               n1_1 = 100,
                               n1_0 = 100,
                               f1 = .5,
                               
                               p0_1 = .5,
                               p0_0 = .42,
                               n0_1 = 100,
                               n0_0 = 100,
                               f0 = .5,
                               
                               alpha = 0.05 ) )
  
  
  #bm
  evalues.IC(  stat = "est",
               true = 0,
               monotonicBias = FALSE,
               monotonicBiasDirection = "negative",
               
               p1_1 = .5,
               p1_0 = .4,
               n1_1 = 100,
               n1_0 = 100,
               f1 = .5,
               
               p0_1 = .5,
               p0_0 = .42,
               n0_1 = 100,
               n0_0 = 100,
               f0 = .5,
               
               alpha = 0.05 )
  
} )

test_that( "E-value from evalues.IC should be the solution to RDt_bound and should agree with theory in paper", {
  
  Eadd.est = evalues.IC(  stat = "est",
                          true = 0,
                          monotonicBias = FALSE,
                          
                          p1_1 = pw_1,
                          p1_0 = pw_0,
                          n1_1 = nw_1,
                          n1_0 = nw_0,
                          f1 = fw,
                          
                          p0_1 = pm_1,
                          p0_0 = pm_0,
                          n0_1 = nm_1,
                          n0_0 = nm_0,
                          f0 = fm,
                          
                          alpha = 0.05 )$evalues
  
  # pass the resulting bias factor (i.e., equivalent to alleged E-value) to each stratum
  x = RDt_bound( p1_1 = pw_1,
                 p1_0 = pw_0,
                 n1_1 = nw_1,
                 n1_0 = nw_0,
                 f1 = fw,
                 biasDir_1 = "positive",
                 # from above test
                 maxB_1 = Eadd.est$biasFactor,
                 
                 p0_1 = pm_1,
                 p0_0 = pm_0,
                 n0_1 = nm_1,
                 n0_0 = nm_0,
                 f0 = fm,
                 biasDir_0 = "negative",
                 maxB_0 = Eadd.est$biasFactor )
  expect_equal( x$RD[ x$stratum == "effectMod" ], 0, tol = 0.0001 )
  
  # should agree with closed form
  gamma = pw_0 * (1 - fw) - pw_1 * fw + pm_1 * (1 - fm) - pm_0 * fm
  term1 = 1 / ( 2 * (fw * pw_0 + fm * pm_1) )
  term2 = 4 * (pw_0 * fw + pm_1 * fm) * ( pw_1 * (1 - fw) + pm_0 * (1-fm) )
  
  # bias factor
  ( my.Badd.est = term1 * ( sqrt( gamma^2 + term2 ) - gamma ) )
  expect_equal( Eadd.est$biasFactor, my.Badd.est, tol = 0.001 )
  
  # E-value
  expect_equal( Eadd.est$evalue, g(my.Badd.est), tol = 0.001 )
  
  
})

# evalues.IC solution for one stratum should agree with existing fn, evalues.RD
test_that("RDt_bound and evalues.IC, test set #2", {
  
  ( resNonMono = evalues.IC( stat = "est",
                             true = 0,
                             monotonicBias = FALSE,
                             
                             p1_1 = .6,
                             p1_0 = .4,
                             n1_1 = 100,
                             n1_0 = 100,
                             f1 = .5,
                             
                             p0_1 = 0.3,
                             p0_0 = 0.20,
                             n0_1 = 100,
                             n0_0 = 100,
                             f0 = .5,
                             
                             alpha = 0.05 )$evalues )
  
  # RDt_bound 
  RDs2 = RDt_bound(   p1_1 = .6,
                      p1_0 = .4,
                      n1_1 = 100,
                      n1_0 = 100,
                      f1 = .5,
                      biasDir_1 = "positive",
                      maxB_1 = resNonMono$biasFactor,
                      
                      p0_1 = 0.3,
                      p0_0 = 0.20,
                      n0_1 = 100,
                      n0_0 = 100,
                      f0 = .5,
                      biasDir_0 = "negative",
                      maxB_0 = resNonMono$biasFactor )
  
  
  # stratum 1 only
  evalueOld2 = EValue::evalues.RD( n11 = 100 * (0.6),
                                   n10 = 100 * (1-0.6),
                                   n01 = 100 * 0.4,
                                   n00 = 100 * (1-0.4),
                                   true = RDs2$RD[1],
                                   alpha = 0.05)
  
  expect_equal( evalueOld2$est.Evalue, resNonMono$evalue, tol = 0.001 )
  
  
  # # stratum M only
  # evalueOld2 = EValue::evalues.RD( n11 = 100 * (0.3),
  #                                  n10 = 100 * (1-0.3),
  #                                  n01 = 100 * 0.2,
  #                                  n00 = 100 * (1-0.2),
  #                                  true = RDs2$RD[2],
  #                                  alpha = 0.05)
  # 
  # 
  # # they agree! :D
  # # woohoo!!!!!
  # expect_equal( evalueOld2$est.Evalue, resNonMono$evalue, tol = 0.001 )
  
})




# ~~ Monotonic confounding ----------------------

# ~~~ Evalue candidate #1 (positive bias) should successfully move RDw down to RDm


test_that("evalues.IC, monotonic, test set #1", {
  ( Eadd.est.mono = evalues.IC( stat = "est",
                                true = 0,
                                monotonicBias = TRUE,
                                monotonicBiasDirection = "unknown",
                                
                                p1_1 = pw_1,
                                p1_0 = pw_0,
                                n1_1 = nw_1,
                                n1_0 = nw_0,
                                f1 = fw,
                                
                                p0_1 = pm_1,
                                p0_0 = pm_0,
                                n0_1 = nm_1,
                                n0_0 = nm_0,
                                f0 = fm,
                                
                                alpha = 0.05 )$evalues )
  
  x = RDt_bound( p1_1 = pw_1,
                 p1_0 = pw_0,
                 n1_1 = nw_1,
                 n1_0 = nw_0,
                 f1 = fw,
                 biasDir_1 = "positive",
                 maxB_1 = Eadd.est.mono$candidates$biasFactor[ Eadd.est.mono$candidates$biasDir == "positive" ],
                 
                 p0_1 = pm_1,
                 p0_0 = pm_0,
                 n0_1 = nm_1,
                 n0_0 = nm_0,
                 f0 = fm,
                 biasDir_0 = "positive",
                 maxB_0 = 1 )
  expect_equal( x$RD[ x$stratum == "1" ], RDm, tol = 0.0001 )
  
  # and likewise for CI limit
  ( Eadd.CI.mono = evalues.IC( stat = "CI",
                               true = 0,
                               monotonicBias = TRUE,
                               monotonicBiasDirection = "unknown",
                               
                               p1_1 = pw_1,
                               p1_0 = pw_0,
                               n1_1 = nw_1,
                               n1_0 = nw_0,
                               f1 = fw,
                               
                               p0_1 = pm_1,
                               p0_0 = pm_0,
                               n0_1 = nm_1,
                               n0_0 = nm_0,
                               f0 = fm,
                               
                               alpha = 0.05 )$evalues )
  
  ( x = RDt_bound( p1_1 = pw_1,
                   p1_0 = pw_0,
                   n1_1 = nw_1,
                   n1_0 = nw_0,
                   f1 = fw,
                   biasDir_1 = "positive",
                   maxB_1 = Eadd.CI.mono$candidates$biasFactor[ Eadd.CI.mono$candidates$biasDir == "positive" ],
                   
                   p0_1 = pm_1,
                   p0_0 = pm_0,
                   n0_1 = nm_1,
                   n0_0 = nm_0,
                   f0 = fm,
                   biasDir_0 = "positive",
                   maxB_0 = 1 ) )
  expect_equal( x$lo[ x$stratum == "effectMod" ], 0, tol = 0.0001 )
  
  # ~~~ Evalue candidate #2 (negative bias) should successfully move RDm up to RDw ----------------------
  ( x = RDt_bound( p1_1 = pw_1,
                   p1_0 = pw_0,
                   n1_1 = nw_1,
                   n1_0 = nw_0,
                   f1 = fw,
                   biasDir_1 = "negative",
                   maxB_1 = 1,
                   
                   p0_1 = pm_1,
                   p0_0 = pm_0,
                   n0_1 = nm_1,
                   n0_0 = nm_0,
                   f0 = fm,
                   biasDir_0 = "negative",
                   maxB_0 = Eadd.est.mono$candidates$biasFactor[ Eadd.est.mono$candidates$biasDir == "negative" ] ) )
  expect_equal( x$RD[ x$stratum == "0" ], RDw, tol = 0.0001 )
  
  # and likewise for CI limit
  ( x = RDt_bound( p1_1 = pw_1,
                   p1_0 = pw_0,
                   n1_1 = nw_1,
                   n1_0 = nw_0,
                   f1 = fw,
                   biasDir_1 = "negative",
                   maxB_1 = 1,
                   
                   p0_1 = pm_1,
                   p0_0 = pm_0,
                   n0_1 = nm_1,
                   n0_0 = nm_0,
                   f0 = fm,
                   biasDir_0 = "negative",
                   maxB_0 = Eadd.CI.mono$candidates$biasFactor[ Eadd.CI.mono$candidates$biasDir == "negative" ] ) )
  expect_equal( x$lo[ x$stratum == "effectMod" ], 0, tol = 0.0001 )
  
  
  
  # ~~~ E-values from IC_evalue (grid search) should match closed form in paper ----------------------
  
  ### check ??
  B = 4
  true = ( pm_1 * B - pm_0 ) * ( fm + (1-fm) / B )
  
  # suggestively name terms as in quadratic formula
  termA = fm * pm_1
  termB = pm_1 * ( 1 - fm ) - fm * pm_0 - true
  termC = -pm_0 * (1 - fm)
  
  # this is the polynomial that is to be solved for E-value
  ( mine = termA * B^2 + termB * B + termC )
  expect_equal( mine, 0, tol = 0.0001 )
  
  # check E-value for negatively biased stratum in paper
  #  this is the one that arises from reversing roles and signs in the existing E-value on 
  #  Ding Appendix, pg 18
  
  
  ### check E-value for positive bias (shift only stratum W)
  # check against Ding Appendix, pg 18 (Prop A.11)
  targetB = Eadd.est.mono$candidates$biasFactor[ Eadd.est.mono$candidates$biasDir == "positive" ]
  
  lambda = pw_0 * (1 - fw) - pw_1 * fw
  term1 = 1 / ( 2 * (fw * pw_0) )
  term2 = 4 * pw_1 * pw_0 * fw * (1 - fw)
  true = RDm
  term3 = true + lambda
  
  # bias factor
  ( myB = term1 * ( sqrt( term3^2 + term2 ) - term3 ) )
  expect_equal( targetB, myB, tol = 0.001 )
  
  ### check E-value for negative bias (shif1 only stratum M)
  targetB = Eadd.est.mono$candidates$biasFactor[ Eadd.est.mono$candidates$biasDir == "negative" ]
  
  lambda = pm_1 * (1 - fm) - pm_0 * fm 
  term1 = 1 / ( 2 * (fm * pm_1) )
  term2 = 4 * pm_1 * pm_0 * fm * (1 - fm)
  true = -RDw
  term3 = true + lambda
  
  # bias factor
  ( myB = term1 * ( sqrt( term3^2 + term2 ) - term3 ) )
  expect_equal( targetB, myB, tol = 0.001 )
  
})



