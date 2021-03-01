
# no need to source helper_testthat.R explicitly to pass CRAN checks
#  because helper files that start with "helper" automatically 
#  get sourced first: https://testthat.r-lib.org/reference/test_dir.html

# # for local testing:
library(testthat)
library(devtools)
library(dplyr)
library(ICC)
library(msm)
library(MetaUtility)
library(here())
setwd(here())
#setwd("~/Dropbox/Personal computer/Independent studies/R packages/EValue package (git)/evalue_package/EValue")
setwd("tests")
source("helper_testthat.R")




###################### EVALUE: ANNALS PAPER EXAMPLES ######################

test_that("Annals case #1", {
  expect_equal( 3.9 + sqrt( 3.9 * (3.9 - 1) ), evalues.RR( 3.9, 1.8, 8.7 )[2, "point"] )
  expect_equal( 1.8 + sqrt( 1.8 * (1.8 - 1) ), evalues.RR( 3.9, 1.8, 8.7 )[2, "lower"] )
  expect_identical( is.na(NA), is.na(evalues.RR( 3.9, 1.8, 8.7 )[2, "upper"]) )
})


test_that("Annals case #1, preventive version", {
  expect_equal( 3.9 + sqrt( 3.9 * (3.9 - 1) ), evalues.RR( 1/3.9, 1/8.7, 1/1.8 )[2, "point"] )
  expect_identical( is.na(NA), is.na( evalues.RR( 1/3.9, 1/8.7, 1/1.8 )[2, "lower"]) )
  expect_equal( 1.8 + sqrt( 1.8 * (1.8 - 1) ), evalues.RR( 1/3.9, 1/8.7, 1/1.8 )[2, "upper"] )
})

test_that("Annals, Further Examples #1", {
  expect_equal( 1/0.8 + sqrt( 1/0.8 * (1/0.8 - 1) ), evalues.RR( 0.80, 0.71, 0.91 )[2, "point"] )
  expect_identical( is.na(NA), is.na( evalues.RR( 1/3.9, 1/8.7, 1/1.8 )[2, "lower"]) )
  expect_equal( 1/0.91 + sqrt( 1/0.91 * (1/0.91 - 1) ), evalues.RR( 0.80, 0.71, 0.91 )[2, "upper"] )
})


###################### EVALUE: OTHER OUTCOME TYPES ######################

test_that("SMD, causative", {
  
  d = 0.5
  se = 0.2
  true = 0.1
  
  RR = exp(0.91 * d)
  RR.true = exp(0.91 * true)
  E = ( RR + sqrt( RR * (RR - RR.true) ) ) / RR.true
  
  RR.lo = exp( 0.91 * d - 1.78 * se )
  E.lo = ( RR.lo + sqrt( RR.lo * (RR.lo - RR.true) ) ) / RR.true
  
  expect_equal( E, evalues.MD( d, se, true = true )[2,"point"] )
  expect_equal( E.lo, evalues.MD( d, se, true = true )[2,"lower"] )
  expect_identical( TRUE, is.na( evalues.MD( d, se, true = true )[2,"upper"] ) )
})


test_that("SMD, preventive", {
  
  d = -0.5
  se = 0.4
  true = -0.1
  
  RR = exp(0.91 * d)
  RR.true = exp(0.91 * true)
  E = ( 1/RR + sqrt( 1/RR * (1/RR - 1/RR.true) ) ) / (1/RR.true)
  
  # CI will cross true
  expect_equal( E, evalues.MD( d, se, true = true )[2,"point"] )
  expect_equal( 1, evalues.MD( d, se, true = true )[2,"upper"] )
})



test_that("SMD #3", {
  
  # true is already more extreme than observed
  d = -0.5
  se = 0.4
  true = -0.6
  
  RR = exp(0.91 * d)
  RR.true = exp(0.91 * true)
  
  # ratio because true is more extreme than observed
  rat = RR / RR.true
  E = rat + sqrt( rat * (rat - 1) )
  
  # CI will cross true
  expect_equal( E, evalues.MD( d, se, true = true )[2,"point"] )
  expect_equal( 1, evalues.MD( d, se, true = true )[2,"lower"] )
})



test_that("Regression coefficient, causative", {
  
  est = 0.5
  sd = 0.8
  se.b = 0.2
  true = 0.1
  
  # calculate SMD
  d = est/sd
  se = se.b/sd
  
  RR = exp(0.91 * d)
  RR.true = exp(0.91 * true)
  E = ( RR + sqrt( RR * (RR - RR.true) ) ) / RR.true
  
  RR.lo = exp( 0.91 * d - 1.78 * se )
  E.lo = ( RR.lo + sqrt( RR.lo * (RR.lo - RR.true) ) ) / RR.true
  
  package = evalues.OLS( est = est,
                         se = se.b,
                         sd = sd,
                         true = true )
  
  expect_equal( E, package[2,"point"] )
  expect_equal( E.lo, package[2,"lower"] )
  expect_identical( TRUE, is.na( package[2,"upper"] ) )
})


test_that("Regression coefficient, preventive", {
  
  est = -10
  sd = 12
  se.b = 1.5
  true = 0.8
  
  # calculate SMD
  d = est/sd
  se = se.b/sd
  
  RR = 1/exp(0.91 * d)
  RR.true = 1/exp(0.91 * true)
  E = ( RR + sqrt( RR * (RR - RR.true) ) ) / RR.true
  
  RR.hi = 1/exp( 0.91 * d + 1.78 * se )
  E.hi = ( RR.hi + sqrt( RR.hi * (RR.hi - RR.true) ) ) / RR.true
  
  package = evalues.OLS( est = est,
                         se = se.b,
                         sd = sd,
                         true = true )
  
  expect_equal( E, package[2,"point"] )
  expect_equal( E.hi, package[2,"upper"] )
  expect_identical( TRUE, is.na( package[2,"lower"] ) )
})

# ???
# changed to -delta under calculate SMD
test_that("Regression coefficient, preventive, different delta", {
  
  est = -10
  sd = 12
  se.b = 1.5
  true = 0.8
  delta = -2
  
  # calculate SMD
  d = (-delta*est)/sd
  se = (-delta*se.b)/sd
  
  RR = 1/exp(0.91 * d)
  RR.true = 1/exp(0.91 * true)
  E = ( RR + sqrt( RR * (RR - RR.true) ) ) / RR.true
  
  RR.hi = 1/exp( 0.91 * d + 1.78 * se )
  E.hi = ( RR.hi + sqrt( RR.hi * (RR.hi - RR.true) ) ) / RR.true
  
  package = evalues.OLS( est = est,
                         se = se.b,
                         sd = sd,
                         delta = delta,
                         true = true )
  
  expect_equal( E, package[2,"point"] )
  expect_equal( E.hi, package[2,"upper"] )
  expect_identical( TRUE, is.na( package[2,"lower"] ) )
})





test_that("Peng's risk difference example", {
  f = (397+78557) / (51+108778+397+78557)
  p1 = 397 / (397+78557)
  p0 = 51 / (51+108778)
  RD.true = 0
  
  diff = p0 * (1 - f) - p1 * f
  
  B = ( sqrt( ( RD.true + diff )^2 + 4 * p1 * p0 * f * (1-f) ) - ( RD.true + diff ) ) / (2 * p0 * f)
  
  E = threshold(B)
  
  expect_equal( E, evalues.RD( 397, 78557, 51, 108778 )$est.Evalue, tolerance = 0.001 )
  expect_equal( 15.95703, evalues.RD( 397, 78557, 51, 108778 )$lower.Evalue, tolerance = 0.001 )
})


test_that("RD that crosses null", {
  f = (55+108778) / (51+108778+55+108778)
  p1 = 55 / (55+108778)
  p0 = 51 / (51+108778)
  RD.true = 0
  
  diff = p0 * (1 - f) - p1 * f
  
  B = ( sqrt( ( RD.true + diff )^2 + 4 * p1 * p0 * f * (1-f) ) - ( RD.true + diff ) ) / (2 * p0 * f)
  
  E = threshold(B)
  
  expect_equal( E, evalues.RD( 55, 108778, 51, 108778 )$est.Evalue, tolerance = 0.001 )
  expect_equal( 1, evalues.RD( 55, 108778, 51, 108778 )$lower.Evalue, tolerance = 0.001 )
})


test_that("True RD = 0 should have E = observed RR", {
  
  # If true causal RD = 0, then B should equal observed RR
  # See last paragraph of Ding (2016) pg 376
  B = (100/300)/(50/250)
  E = threshold(B)
  
  expect_equal( E, evalues.RD( 100, 200, 50, 200, true = 0)$est.Evalue, tolerance = 0.001 )
})


test_that("Do not allow true > observed for RD", {
  
  expect_error( evalues.RD( 100, 200, 50, 200, true = 0.8) )
})








test_that("Rare OR", {
  expect_equal( 5 + sqrt( 5 * (5 - 1) ), evalues.OR( 5, rare = TRUE )[2, "point"] )
})

test_that("Rare HR", {
  expect_equal( ( 1/0.3 + sqrt( 1/0.3 * (1/0.3 - 1/0.4) ) ) / (1/0.4),
                evalues.HR( 0.3, true = 0.4, rare = TRUE )[2, "point"] )
})

test_that("Common OR", {
  expect_equal( ( sqrt(5) + sqrt( sqrt(5) * (sqrt(5) - sqrt(2)) ) ) / sqrt(2),
                evalues.OR( 5, true = 2, rare = FALSE )[2, "point"] )
  
  expect_equal( ( sqrt(3) + sqrt( sqrt(3) * (sqrt(3) - sqrt(2)) ) ) / sqrt(2),
                evalues.OR( 5, 3, true = 2, rare = FALSE )[2, "lower"] )
})

test_that("Common HR", {
  
  HR = 1.3
  RR = (1 - 0.5^sqrt(HR)) / ( 1 - 0.5^sqrt(1/HR) )
  
  HR.lo = 1.2
  RR.lo = (1 - 0.5^sqrt(HR.lo)) / ( 1 - 0.5^sqrt(1/HR.lo) )
  
  HR.hi = 1.4
  
  HR.true = 1.1
  RR.true = (1 - 0.5^sqrt(HR.true)) / ( 1 - 0.5^sqrt(1/HR.true) )
  
  expect_equal( ( RR + sqrt( RR * (RR - RR.true) ) ) / RR.true,
                evalues.HR( HR, HR.lo, HR.hi, true = HR.true, rare = FALSE )[2, "point"] )
  
  expect_equal( ( RR.lo + sqrt( RR.lo * (RR.lo - RR.true) ) ) / RR.true,
                evalues.HR( HR, HR.lo, HR.hi, true = HR.true, rare = FALSE )[2, "lower"] )
  
  expect_identical( TRUE,
                    is.na( evalues.HR( HR, HR.lo, HR.hi, true = HR.true, rare = FALSE )[2, "upper"] ) )
})





###################### EVALUE: POINT ESTIMATE NOT INSIDE CI ######################

test_that("Point estimate not inside CI #1", {
  expect_error( evalues.RR( 1.2, 1.3, 1.4 ), "Point estimate should be inside confidence interval" )
})

test_that("Point estimate not inside CI #2", {
  expect_error( evalues.RR( 1.5, 1.3, 1.4 ), "Point estimate should be inside confidence interval" )
})

test_that("Point estimate not inside CI #3", {
  expect_error( evalues.RR( 0.6, 0.7, 1.1 ), "Point estimate should be inside confidence interval" )
})

test_that("Point estimate not inside CI #4", {
  expect_error( evalues.RR( 0.8, 0.3, 0.7 ), "Point estimate should be inside confidence interval" )
})




###################### EVALUE: VARIOUS TRUE VALUES ######################
# four cases for location of true value wrt confidence interval and point estimate

#### Causative

test_that("True value < lower CI limit, causative", {
  expect_equal( ( 1.3 + sqrt( 1.3 * (1.3 - 1.05) ) ) / 1.05,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.05 )[2, "point"] )
  
  expect_equal( ( 1.1 + sqrt( 1.1 * (1.1 - 1.05) ) ) / 1.05,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.05 )[2, "lower"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 1.3, 1.1, 1.4, true = 1.05 )[2, "upper"]) )
})


test_that("True value = lower CI limit, causative", {
  expect_equal( ( 1.3 + sqrt( 1.3 * (1.3 - 1.1) ) ) / 1.1,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.1 )[2, "point"] )
  
  expect_equal( 1,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.1 )[2, "lower"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 1.3, 1.1, 1.4, true = 1.05 )[2, "upper"]) )
})


test_that("Lower CI limit < True value < Point estimate, causative", {
  expect_equal( ( 1.3 + sqrt( 1.3 * (1.3 - 1.2) ) ) / 1.2,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.2 )[2, "point"] )
  
  expect_equal( 1,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.2 )[2, "lower"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 1.3, 1.1, 1.4, true = 1.2 )[2, "upper"]) )
})




test_that("Point estimate < True value < Upper CI limit, causative", {
  
  # true is more extreme than observed
  rat = 1.35/1.3
  E = rat + sqrt( rat * (rat-1) )
  
  expect_equal( E,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.35 )[2, "point"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 1.3, 1.1, 1.4, true = 1.35 )[2, "lower"]) )
  
  expect_equal( 1,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.35 )[2, "upper"] )
})



test_that("Upper CI limit = True value, causative", {
  
  # true is more extreme than observed
  rat = 1.4/1.3
  E = rat + sqrt( rat * (rat-1) )
  
  expect_equal( E,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.4 )[2, "point"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 1.3, 1.1, 1.4, true = 1.4 )[2, "lower"]) )
  
  expect_equal( 1,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.4 )[2, "upper"] )
})


test_that("Upper CI limit < True value, causative", {
  
  # true is more extreme than observed
  rat = 1.5/1.3
  E = rat + sqrt( rat * (rat-1) )
  
  rat = 1.5/1.4
  E.CI = rat + sqrt( rat * (rat-1) )
  
  expect_equal( E,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.5 )[2, "point"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 1.3, 1.1, 1.4, true = 1.5 )[2, "lower"]) )
  
  expect_equal( E.CI,
                evalues.RR( 1.3, 1.1, 1.4, true = 1.5 )[2, "upper"] )
})




#### Preventive

test_that("True value < lower CI limit, preventive", {
  
  # true is more extreme than observed
  rat = 0.6/0.4
  E = rat + sqrt( rat * (rat-1) )
  
  rat = 0.5/0.4
  E.CI = rat + sqrt( rat * (rat-1) )
  
  expect_equal( E,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.4 )[2, "point"] )
  
  expect_equal( E.CI,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.4 )[2, "lower"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 0.6, 0.5, 0.7, true = 0.4 )[2, "upper"]) )
  
})


test_that("True value = lower CI limit, preventive", {
  
  # true is more extreme than observed
  rat = 0.6/0.5
  E = rat + sqrt( rat * (rat-1) )
  
  expect_equal( E,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.5 )[2, "point"] )
  
  expect_equal( 1,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.5 )[2, "lower"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 0.6, 0.5, 0.7, true = 0.5 )[2, "upper"]) )
  
})


test_that("Lower CI limit < True value < Point estimate, preventive", {
  
  # true is more extreme than observed
  rat = 0.6/0.55
  E = rat + sqrt( rat * (rat-1) )
  
  expect_equal( E,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.55 )[2, "point"] )
  
  expect_equal( 1,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.55 )[2, "lower"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 0.6, 0.5, 0.7, true = 0.55 )[2, "upper"]) )
})


test_that("Point estimate < True value < Upper CI limit, preventive", {
  
  expect_equal( ( 1/0.6 + sqrt( (1/0.6) * (1/0.6 - 1/0.65) ) ) / (1/0.65),
                evalues.RR( 0.6, 0.5, 0.7, true = 0.65 )[2, "point"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 0.6, 0.5, 0.7, true = 0.65 )[2, "lower"]) )
  
  expect_equal( 1,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.65 )[2, "upper"] )
})


test_that("Upper CI limit = True value, preventive", {
  expect_equal( ( 1/0.6 + sqrt( (1/0.6) * (1/0.6 - 1/0.7) ) ) / (1/0.7),
                evalues.RR( 0.6, 0.5, 0.7, true = 0.7 )[2, "point"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 0.6, 0.5, 0.7, true = 0.7 )[2, "lower"]) )
  
  expect_equal( 1,
                evalues.RR( 0.6, 0.5, 0.7, true = 0.7 )[2, "upper"] )
})


test_that("Upper CI limit < True value, preventive", {
  expect_equal( ( 1/0.6 + sqrt( (1/0.6) * (1/0.6 - 1/0.75) ) ) / (1/0.75),
                evalues.RR( 0.6, 0.5, 0.7, true = 0.75 )[2, "point"] )
  
  expect_identical( is.na(NA), is.na(evalues.RR( 0.6, 0.5, 0.7, true = 0.75 )[2, "lower"]) )
  
  expect_equal( ( 1/0.7 + sqrt( (1/0.7) * (1/0.7 - 1/0.75) ) ) / (1/0.75),
                evalues.RR( 0.6, 0.5, 0.7, true = 0.75 )[2, "upper"] )
})



######## True = estimate, no CI provided ######

test_that("True = est, no CI, preventive", {
  expect_equal( 1,
                evalues.RR( 0.9, true = 0.9 )[2, "point"] )
})

test_that("True = est, no CI, causative", {
  expect_equal( 1,
                evalues.RR( 1.1, true = 1.1 )[2, "point"] )
})



###################### EVALUE: IMPOSSIBLE INPUTS ######################

test_that("Reject negative outcomes", {
  expect_error( evalues.RR( -0.1 ) )
  expect_error( evalues.HR( -0.1, rare=FALSE ) )
  expect_error( evalues.OR( -0.1, rare=FALSE ) )
})







###################### CONFOUNDED_META AND SENS_PLOT ######################

#bm

##### Parametric Method #####
test_that("Parametric, test set #1 (setting q equal to observed mean without bias should yield 50%)", {
  expect_equal( 0.5, confounded_meta(method="parametric",
                                     q=log(1.4),
                                     muB=0,
                                     sigB=0,
                                     yr=log(1.4),
                                     t2=0.1 )[1,2] )
  
  expect_equal( 0.5, confounded_meta(method="parametric",
                                     q=log(0.5),
                                     muB=0,
                                     sigB=0,
                                     yr=log(0.5),
                                     t2=0.1 )[1,2] )
  
})



test_that("Parametric, test set #2 (causative)", {
  
  q = log(1.1)
  muB = log(2)
  sigB = 0.1
  yr = log(1.4)
  vyr = 0.5
  t2 = 0.3
  vt2 = 0.02
  
  r = 0.1
  
  cm = confounded_meta(method="parametric", q=q, r=r, muB=muB, sigB=sigB,
                       yr=yr, vyr=vyr,
                       t2=t2, vt2=vt2 )
  
  # make q more extreme to trigger bootstrap warning
  expect_warning( confounded_meta(method="parametric", q=log(1.5), r=r, muB=muB, sigB=sigB,
                                  yr=yr, vyr=vyr,
                                  t2=t2, vt2=vt2 ) )
  
  
  ##### Prop Above ######
  # point estimate
  expect_equal( 1 - pnorm( ( q + muB - yr ) / ( sqrt(t2 - sigB^2) ) ),
                cm[1,2] )
  
  # check standard error against deltamethod function
  SE = deltamethod( ~ 1 - pnorm( ( log(1.1) + log(2) - x1 ) / sqrt( x2 - 0.1^2 ) ),
                    mean = c( yr, t2 ), cov = diag( c(vyr, vt2) ) )
  expect_equal( SE, cm[1,3] )
  #
  #   # CI limits
  expect_equal( max(0, cm[1,2] - SE*qnorm(0.975)), cm[1,4] )
  expect_equal( min(1, cm[1,2] + SE*qnorm(0.975)), cm[1,5] )
  
  
  ##### TMin ######
  # point estimate
  expect_equal( exp( qnorm(1-r) * sqrt(t2 - sigB^2) - q + yr ),
                cm[2,2] )
  
  # check standard error against deltamethod function
  #qnorm( 1 - 0.1 ) # because deltamethod can't take its derivatve
  SE = deltamethod( ~ exp( 1.281552 * sqrt(x2) - log(1.1) + x1 ), mean = c( log(1.4), 0.3 - .1^2 ), cov = diag( c(0.5, 0.02) ) )
  
  expect_equal( SE, cm[2,3], tol = 0.001 )
  
  # CI limits
  expect_equal( max(1, cm[2,2] - SE*qnorm(0.975)), cm[2,4], tol = 0.001 )
  expect_equal( cm[2,2] + SE*qnorm(0.975), cm[2,5], tol = 0.001 )
  
  
  ##### GMin ######
  # point estimate
  # compute from Tmin
  Tmin = cm[2,2]
  SE.T = cm[2,3]
  expect_equal( Tmin + sqrt( Tmin^2 - Tmin ),
                cm[3,2] )
  
  # check standard error against deltamethod function
  SE = deltamethod( ~ x1 + sqrt( x1^2 - x1 ), mean = c( Tmin ), cov = SE.T^2 )
  expect_equal( SE, cm[3,3] )
  
  # CI limits
  expect_equal( max(1, cm[3,2] - SE*qnorm(0.975)), cm[3,4] )
  expect_equal( cm[3,2] + SE*qnorm(0.975), cm[3,5] )

})




test_that("Parametric, test set #3 (preventive)", {
  
  q = log(0.9)
  muB = log(1.2)
  sigB = 0.1
  yr = log(0.6)
  vyr = 0.2
  t2 = 0.2
  vt2 = 0.01
  
  r = 0.1
  
  cm = confounded_meta(method="parametric", q=q, r=r, muB=muB, sigB=sigB,
                       yr=yr, vyr=vyr,
                       t2=t2, vt2=vt2 )
  
  
  ##### Prop Above ######
  # point estimate
  expect_equal( pnorm( ( q - muB - yr ) / ( sqrt(t2 - sigB^2) ) ),
                cm[1,2] )
  
  # check standard error against deltamethod function
  SE = deltamethod( ~ pnorm( ( log(0.9) - log(1.2) - x1 ) / sqrt( x2 - 0.1^2 ) ),
                    mean = c( yr, t2 ), cov = diag( c(vyr, vt2) ) )
  expect_equal( SE, cm[1,3] )
  
  # CI limits
  expect_equal( max(0, cm[1,2] - SE*qnorm(0.975)), cm[1,4] )
  expect_equal( min(1, cm[1,2] + SE*qnorm(0.975)), cm[1,5] )
  
  
  ##### TMin ######
  # point estimate
  expect_equal( exp( q - yr - qnorm(r) * sqrt(t2 - sigB^2) ),
                cm[2,2] )
  
  # check standard error against deltamethod function
  #qnorm( 0.1 ) # because deltamethod can't take its derivative
  SE = deltamethod( ~ exp( log(0.9) - x1 - (-1.281552) * sqrt(x2) ), mean = c( log(0.6), 0.2 - .1^2 ), cov = diag( c(0.2, 0.01) ) )
  
  expect_equal( SE, cm[2,3], tol = 0.001 )
  
  # CI limits
  expect_equal( max(1, cm[2,2] - SE*qnorm(0.975)), cm[2,4], tol = 0.001 )
  expect_equal( cm[2,2] + SE*qnorm(0.975), cm[2,5], tol = 0.001 )
  
  
  ##### GMin ######
  # point estimate
  # compute from Tmin
  Tmin = cm[2,2]
  SE.T = cm[2,3]
  expect_equal( Tmin + sqrt( Tmin^2 - Tmin ),
                cm[3,2] )
  
  # check standard error against deltamethod function
  SE = deltamethod( ~ x1 + sqrt( x1^2 - x1 ), mean = c( Tmin ), cov = SE.T^2 )
  expect_equal( SE, cm[3,3] )
  
  # CI limits
  expect_equal( max(1, cm[3,2] - SE*qnorm(0.975)), cm[3,4] )
  expect_equal( cm[3,2] + SE*qnorm(0.975), cm[3,5] )
})




test_that("Parametric, test set #4, (no bias needed to reduce this Phat to less than r)", {
  cm = suppressMessages( confounded_meta(method="parametric", q=log(1.5), r = 0.1, muB=0, sigB=0,
                                         yr=log(1.3), t2=0.01) )
  
  # Tmin and Gmin should both be 1 because already fewer than 10% of true effects
  #  are above q
  expect_equal( 1, cm[2,2] )
  expect_equal( 1, cm[3,2] )
})



test_that("Parametric, test set #5 (exactly 200 estimates; manipulate muB.toward.null)", {
  
  est = -99
  # example will fail if we accidentally generate data with point estimate < 0
  while( est < 0 ) {
    # make data with exactly 200 calibrated estimates so that Tmin and Gmin should exactly hit r
    d = sim_data2( k = 200,
                   m = 200,
                   b0 = log(1.1), # intercept
                   bc = 0, # effect of continuous moderator
                   bb = 0, # effect of binary moderator
                   V = 1,
                   Vzeta = 0, # used to calcuate within-cluster variance
                   
                   muN = 100,
                   minN = 100,
                   sd.w = 1,
                   true.effect.dist = "normal" )
    
    # shift AWAY from null
    q = log(1.1)  # set q to the (true) mean
    r = .2
    muB = log(1.5)
    
    # first no bias
    meta = rma.uni( yi = d$yi,
                    sei = sqrt(d$vyi),
                    method = "REML")
    est = meta$b
  }
  
 
  
  x0 = confounded_meta(method="parametric",
                       q = q,
                       r = r,
                       tail = "above",
                       muB = 0,
                       sigB = 0,
                       
                       yr = meta$b,
                       t2 = meta$tau2,
                       vyr = meta$se^2,
                       vt2 = meta$se.tau2^2,
                       
                       give.CI=FALSE)
  
  # bias TOWARD null
  x1 = confounded_meta(method="parametric",
                       q = q,
                       r = r,
                       tail = "above",
                       muB.toward.null = TRUE,
                       muB = muB,
                       sigB = 0,
                       
                       yr = meta$b,
                       t2 = meta$tau2,
                       vyr = meta$se^2,
                       vt2 = meta$se.tau2^2,
                       
                       give.CI=FALSE)
  
  # since bias correction INCREASES the mean in this case, 
  #  we should have MORE meaningfully strong effects
  expect_equal( x1$Est[ x1$Value == "Prop" ] > x0$Est[ x0$Value == "Prop" ], TRUE ) 
  
  # bias AWAY from null (default)
  x2 = confounded_meta(method="parametric",
                       q = q,
                       r = r,
                       tail = "above",
                       muB = muB,
                       sigB = 0,
                       
                       yr = meta$b,
                       t2 = meta$tau2,
                       vyr = meta$se^2,
                       vt2 = meta$se.tau2^2,
                       
                       give.CI=FALSE)
  
  expect_equal( x2$Est[ x2$Value == "Prop" ] < x0$Est[ x0$Value == "Prop" ], TRUE ) 
  
  
  # Tmin and Gmin should be the same in all cases
  expect_equal( x0$Est[ x0$Value == "Tmin" ],
                x1$Est[ x1$Value == "Tmin" ],
                x2$Est[ x2$Value == "Tmin" ] )
  
  expect_equal( x0$Est[ x0$Value == "Gmin" ],
                x1$Est[ x1$Value == "Gmin" ],
                x2$Est[ x2$Value == "Gmin" ] )
  
  # this is a case where Tmin represents bias TOWARD the null
  # check that their numerical value is correct

  
  yr.corr = meta$beta - log(x0$Est[ x0$Value == "Tmin" ])
  expect_equal( pnorm( q = q, 
                       mean = yr.corr,
                       sd = sqrt(meta$tau2),
                       lower.tail = FALSE ), r )
} )




test_that("Parametric, test set #6 (Tmin gets set to 1)", {

  
  ##### tail = "below" case ######
  # here, yr^c is positive, but tail = "below"
  # for Tmin and Gmin, bias has to shift downward to get q
  q = log(0.5)
  muB = log(1.5)
  sigB = sqrt(0.5*0.25)
  yr = log(1.5)
  vyr = 0.5
  t2 = 0.25
  vt2 = 0.5
  r = 0.75
  tail = "below"

  cm = confounded_meta(method="parametric", q=q, r=r, muB=muB, sigB=sigB,
                       yr=yr, vyr=vyr,
                       t2=t2, vt2=vt2, tail = tail )


  # Tmin
  expect_equal( cm[2,2], 1 )
  # Gmin
  expect_equal( cm[3,2], 1 )
  # their CIs should be NA
  expect_equal( as.numeric( c(NA, NA, NA) ), as.numeric( cm[2, 3:5] ) )
  

  ##### tail = "above" case ######
  # symmetric to above
  q = log(1.5)
  yr = log(0.5)
  tail = "above"

  cm = confounded_meta(method="parametric", q=q, r=r, muB=muB, sigB=sigB,
                       yr=yr, vyr=vyr,
                       t2=t2, vt2=vt2, tail = tail )


  # Tmin
  expect_equal( cm[2,2], 1 )
  # Gmin
  expect_equal( cm[3,2], 1 )
  # their CIs should be NA
  expect_equal( as.numeric( c(NA, NA, NA) ), as.numeric( cm[2, 3:5] ) )
} )



##### Calibrated Method #####


# test the helper fns
test_that("Calibrated, Tmin_causal and Phat_causal, test set #1", {
  
  
  
  ##### 1 #####
  # make dataset with lots of ties
  set.seed(10)
  yi = rnorm(10, mean = 0, sd = 3)
  vi = 1
  yi = sort( sample( yi, size = 30, replace = TRUE) )
  calib = MetaUtility::calib_ests(yi = yi, sei = sqrt(vi) )
  # pick a q that's between repeated values
  q = -2.3
  r = 0.20
  tail = "above"
  d = data.frame( yi, vi )
  
  # naive Phat
  Phat0 = Phat_causal(q = q,
                      B = 0,
                      tail = tail,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vi")
  
  expect_equal( Phat0,
                mean(calib > q) )
  
  # Tmin
  Tmin = Tmin_causal(q = q,
                     r = r,
                     tail = tail,
                     dat = d,
                     yi.name = "yi",
                     vi.name = "vi")
  
  # evaluate Phat(B) at Tmin
  Phat.at.Tmin = Phat_causal(q = q,
                             B = log(Tmin),
                             tail = tail,
                             dat = d,
                             yi.name = "yi",
                             vi.name = "vi")
  
  # manually find the value of r we should have been targeting
  calib.t = sort( calib - log(Tmin) )
  ecdf(calib.t)(calib.t)
  expect_equal( 0.1666667, Phat.at.Tmin, tol = 0.001)
  
  
  ##### 2 #####
  # same dataset, but now tail == "below"
  # pick a q that's between repeated values
  q = 0.6
  r = 0.30
  tail = "below"
  d = data.frame( yi, vi )
  
  # naive Phat
  Phat0 = Phat_causal(q = q,
                      B = 0,
                      tail = tail,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vi")
  
  expect_equal( Phat0,
                mean(calib < q) )
  
  # Tmin
  Tmin = Tmin_causal(q = q,
                     r = r,
                     tail = tail,
                     dat = d,
                     yi.name = "yi",
                     vi.name = "vi")
  
  # evaluate Phat(B) at Tmin
  Phat.at.Tmin = Phat_causal(q = q,
                             B = log(Tmin),
                             tail = tail,
                             dat = d,
                             yi.name = "yi",
                             vi.name = "vi")
  
  # manually find the value of r we should have been targeting
  calib.t = sort( calib - log(Tmin) )
  ecdf(calib.t)(calib.t)
  expect_equal( 0.30, Phat.at.Tmin, tol = 0.001)
  
  
  ##### 3 - no confounding needed because Phat0 is already < r without confounding #####
  # Phat0 is 0.9
  r = 0.95
  
  Tmin = Tmin_causal(q = q,
                     r = r,
                     tail = tail,
                     dat = d,
                     yi.name = "yi",
                     vi.name = "vi")
  
  expect_equal( 1, Tmin )
  
  ##### 4 - no confounding needed because Phat0 is already < r without confounding #####
  # now tail == "above"
  d = sim_data2( k = 100,
                 m = 100,
                 b0 = log(1.4), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 0.1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "expo" )
  
  .calib = MetaUtility::calib_ests(yi = d$yi, sei = sqrt(d$vyi) )
  q = quantile(.calib, 0.8)
  r = 0.1
  tail = "above"
  
  # choose q such that Phat0 = 0.20
  # naive Phat
  Phat0 = Phat_causal(q = q,
                      B = 0,
                      tail = tail,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  expect_equal( Phat0,
                0.20 )
  
  Tmin = Tmin_causal(q = q,
                     r = r,
                     tail = tail,
                     dat = d,
                     yi.name = "yi",
                     vi.name = "vyi")
  
  # evaluate Phat(B) at Tmin
  Phat.at.Tmin = Phat_causal(q = q,
                             B = log(Tmin),
                             tail = tail,
                             dat = d,
                             yi.name = "yi",
                             vi.name = "vyi")
  
  expect_equal( r, Phat.at.Tmin, tol = 0.001)
  
} )


test_that("Calibrated, test set #1 (setting q equal to observed mean without bias should yield 50%)", {
  d = sim_data2( k = 100,
                 m = 100,
                 b0 = log(1.4), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 0.1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "expo" )
  
  
  # hand-calculate the calibrated estimates and their median
  d$calib = MetaUtility::calib_ests(yi = d$yi,
                                    sei = sqrt(d$vyi) )
  
  q = median(d$calib)
  
  x = confounded_meta(method="calibrated",
                      q=q,
                      tail = "above",
                      muB=0,
                      
                      give.CI=FALSE,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  expect_equal( 0.5, x$Est[x$Value == "Prop"] )
  
})


test_that("Calibrated, test set #2 (causative)", {
  
  
  d = sim_data2( k = 20,
                 m = 20,
                 b0 = log(2), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 0.1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "expo" )
  
  q = log(1.8)
  r = .2
  muB = log(1.1)
  
  # hand-calculate the calibrated estimates and their median
  calib = MetaUtility::calib_ests(yi = d$yi,
                                  sei = sqrt(d$vyi) )
  mean(calib > q)
  
  
  # expect warning about setting tail because not specified
  expect_warning( confounded_meta(method="calibrated",
                                  q = q,
                                  r = r,
                                  #tail = "above",
                                  muB = muB,
                                  
                                  give.CI=FALSE,
                                  dat = d,
                                  yi.name = "yi",
                                  vi.name = "vyi") )
  
  # repeat to capture the object
  x = confounded_meta(method="calibrated",
                      q = q,
                      r = r,
                      #tail = "above",
                      muB = muB,
                      
                      give.CI=FALSE,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  # check Phat
  Phat.t = mean( calib - muB > q )
  expect_equal( Phat.t, x$Est[ x$Value == "Prop" ])
  
  # check Tmin and Gmin
  # Tmin_causal received its own tests (above)
  Tmin = Tmin_causal(q = q,
                     r = r,
                     tail = "above",
                     dat = d,
                     yi.name = "yi",
                     vi.name = "vyi")
  expect_equal( Tmin, x$Est[ x$Value == "Tmin" ])
  expect_equal( g(Tmin), x$Est[ x$Value == "Gmin" ])
  
  # fail to provide r
  # expect warning about setting tail because not specified
  expect_message( confounded_meta(method="calibrated",
                                  q = q,
                                  #r = r,
                                  tail = "above",
                                  muB = muB,
                                  
                                  give.CI=FALSE,
                                  dat = d,
                                  yi.name = "yi",
                                  vi.name = "vyi") )
  
  
  # # look at CIs, though we can't really check them
  # x = confounded_meta(method="calibrated",
  #                     q = q,
  #                     r = r,
  #                     #tail = "above",
  #                     muB = muB,
  #
  #                     give.CI=TRUE,
  #                     dat = d,
  #                     yi.name = "yi",
  #                     vi.name = "vyi")
  
})


test_that("Calibrated, test set #3 (exactly 200 estimates; manipulate muB.toward.null)", {
  
  # make data with exactly 200 calibrated estimates so that Tmin and Gmin should exactly hit r
  d = sim_data2( k = 200,
                 m = 200,
                 b0 = log(1.1), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "normal" )

  
  # shift AWAY from null
  q = log(1.1)  # set q to the (true) mean
  r = .2
  muB = log(1.5)
  
  # first no bias
  x0 = confounded_meta(method="calibrated",
                      q = q,
                      r = r,
                      tail = "above",
                      muB = 0,
                      
                      give.CI=FALSE,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  # bias TOWARD null
  x1 = confounded_meta(method="calibrated",
                      q = q,
                      r = r,
                      tail = "above",
                      muB = muB,
                      muB.toward.null = TRUE,
                      
                      give.CI=FALSE,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  # since bias correction INCREASES the mean in this case, 
  #  we should have MORE meaningfully strong effects
  expect_equal( x1$Est[ x1$Value == "Prop" ] > x0$Est[ x0$Value == "Prop" ], TRUE ) 
  
  # bias AWAY from null (default)
  x2 = confounded_meta(method="calibrated",
                       q = q,
                       r = r,
                       tail = "above",
                       muB = muB,
                       
                       give.CI=FALSE,
                       dat = d,
                       yi.name = "yi",
                       vi.name = "vyi")
  
  expect_equal( x2$Est[ x2$Value == "Prop" ] < x0$Est[ x0$Value == "Prop" ], TRUE ) 
  

  # Tmin and Gmin should be the same in all cases
  expect_equal( x0$Est[ x0$Value == "Tmin" ],
                x1$Est[ x1$Value == "Tmin" ],
                x2$Est[ x2$Value == "Tmin" ] )
  
  expect_equal( x0$Est[ x0$Value == "Gmin" ],
                x1$Est[ x1$Value == "Gmin" ],
                x2$Est[ x2$Value == "Gmin" ] )
  
  # this is a case where Tmin represents bias TOWARD the null
  # check that their numerical value is correct
  calib = MetaUtility::calib_ests(yi = d$yi,
                                  sei = sqrt(d$vyi) )
  expect_equal( mean( calib - log(x0$Est[ x0$Value == "Tmin" ]) > q ), 
        r ) 


})


test_that("Calibrated and parametric, test set #4 (exactly 200 estimates); Tmin shifts away from the null", {
  
  # in these examples, tail = above but yr < 0, so Tmin is shifting AWAY from null 
  
  # make data with exactly 200 calibrated estimates so that Tmin and Gmin should exactly hit r
  d = sim_data2( k = 200,
                 m = 200,
                 b0 = log(.75), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "normal" )
  
  q = log(0.9)  # q is ABOVE the true mean
  r = .05
  muB = log(1.5)
 
  
  ##### Calibrated #####
  x0 = confounded_meta(method="calibrated",
                       q = q,
                       r = r,
                       tail = "above",
                       muB = 0,
                       
                       give.CI=FALSE,
                       dat = d,
                       yi.name = "yi",
                       vi.name = "vyi")
  
  # since q is above the true mean and we're considering effects above q, 
  #  reducing that proportion to r means Tmin represents bias
  #  AWAY from null (i.e., even more protective)
  calib = MetaUtility::calib_ests(yi = d$yi,
                                  sei = sqrt(d$vyi) )
  expect_equal( mean( calib - log(x0$Est[ x0$Value == "Tmin" ]) > q ), 
                r )
  
  ##### Parametric #####
  
  meta = rma.uni( yi = d$yi,
                  sei = sqrt(d$vyi),
                  method = "REML")
  
  x1 = confounded_meta(method="parametric",
                       q = q,
                       r = r,
                       tail = "above",
                       muB = 0,
                       sigB = 0,
                       
                       yr = meta$b,
                       t2 = meta$tau2,
                       vyr = meta$se^2,
                       vt2 = meta$se.tau2^2,
                       
                       give.CI=FALSE)
  
  # confirm that Tmin is right
  yr.corr = meta$b - log( x1$Est[ x1$Value == "Tmin" ] )
  
  # should be exactly equal to r
  expect_equal( pnorm( q = q,
         mean = yr.corr,
         sd = sqrt(meta$tau2),
         lower.tail = FALSE ), r )
  
})









test_that("Parametric, test set #3 (preventive)", {
  
  # data with ties
  d = sim_data2( k = 20,
                 m = 20,
                 b0 = log(.9), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "expo" )
  
  ind = sample( 1: nrow(d), size = 40, replace = TRUE )
  
  d = d[ ind, ]
  
  q = log(0.9)
  r = .2
  muB = log(2)
  
  # hand-calculate the calibrated estimates and their median
  calib = MetaUtility::calib_ests(yi = d$yi,
                                  sei = sqrt(d$vyi) )
  mean(calib < q)
  
  
  # expect warning about setting tail because not specified
  expect_warning( confounded_meta(method="calibrated",
                                  q = q,
                                  r = r,
                                  #tail = "above",
                                  muB = muB,
                                  
                                  give.CI=FALSE,
                                  dat = d,
                                  yi.name = "yi",
                                  vi.name = "vyi") )
  
  # repeat to capture the object
  x = confounded_meta(method="calibrated",
                      q = q,
                      r = r,
                      #tail = "above",
                      muB = muB,
                      
                      give.CI=FALSE,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  # check Phat
  Phat.t = mean( calib + muB < q )
  expect_equal( Phat.t, x$Est[ x$Value == "Prop" ])
  
  # check Tmin and Gmin
  # Tmin_causal received its own tests (above)
  Tmin = Tmin_causal(q = q,
                     r = r,
                     tail = "below",
                     dat = d,
                     yi.name = "yi",
                     vi.name = "vyi")
  expect_equal( Tmin, x$Est[ x$Value == "Tmin" ])
  expect_equal( g(Tmin), x$Est[ x$Value == "Gmin" ])
  
  # fail to provide r
  expect_message( confounded_meta(method="calibrated",
                                  q = q,
                                  #r = r,
                                  tail = "below",
                                  muB = muB,
                                  
                                  give.CI=FALSE,
                                  dat = d,
                                  yi.name = "yi",
                                  vi.name = "vyi") )
  
  
  # # look at CIs, though we can't really check them
  # x = confounded_meta(method="calibrated",
  #                     q = q,
  #                     r = r,
  #                     #tail = "above",
  #                     muB = muB,
  #
  #                     give.CI=TRUE,
  #                     dat = d,
  #                     yi.name = "yi",
  #                     vi.name = "vyi")
  
})




test_that("Calibrated, test set #4, (no bias needed to reduce this Phat to less than r)", {
  
  # data with ties
  d = sim_data2( k = 20,
                 m = 20,
                 b0 = log(.9), # intercept
                 bc = 0, # effect of continuous moderator
                 bb = 0, # effect of binary moderator
                 V = 1,
                 Vzeta = 0, # used to calcuate within-cluster variance
                 
                 muN = 100,
                 minN = 100,
                 sd.w = 1,
                 true.effect.dist = "normal" )
  
  ind = sample( 1: nrow(d), size = 40, replace = TRUE )
  
  d = d[ ind, ]
  
  r = .2
  muB = log(2)
  
  # choose q to be the 10th percentile of naive calibrated estimates
  # so that no confounding should be needed
  calib = MetaUtility::calib_ests(yi = d$yi,
                                  sei = sqrt(d$vyi) )
  
  q = quantile(calib, .9)
  
  x = confounded_meta(method="calibrated",
                      q=q,
                      r=r,
                      tail = "above",
                      muB=0,
                      
                      give.CI=FALSE,
                      dat = d,
                      yi.name = "yi",
                      vi.name = "vyi")
  
  # calculate the correct Phat
  # might not be exactly .1 due to ties
  Phat = mean(calib > q)
  
  expect_equal( x$Est[x$Value == "Prop"], Phat )
  expect_equal( x$Est[x$Value == "Tmin"], 1 )
  expect_equal( x$Est[x$Value == "Gmin"], 1 )
  
})





