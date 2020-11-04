library(testthat)
#library(EValue)
library(devtools)
library(dplyr)
library(ICC)

# for simulating meta-analysis data
library(here())
setwd(here())
setwd("tests")
source("testthat_helper.R")

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
  library(msm)
  SE = deltamethod( ~ 1 - pnorm( ( log(1.1) + log(2) - x1 ) / sqrt( x2 - 0.1^2 ) ),
                    mean = c( yr, t2 ), cov = diag( c(vyr, vt2) ) )
  expect_equal( SE, cm[1,3] )
  #   
  #   # CI limits
  expect_equal( max(0, cm[1,2] - SE*qnorm(0.975)), cm[1,4] )
  expect_equal( min(1, cm[1,2] + SE*qnorm(0.975)), cm[1,5] )
  
  
  ##### TMin ######
  # point estimate
  expect_equal( exp( qnorm(1-r) * sqrt(t2) - q + yr ),
                cm[2,2] )
  
  # check standard error against deltamethod function
  #qnorm( 1 - 0.1 ) # because deltamethod can't take its derivatve
  SE = deltamethod( ~ exp( 1.281552 * sqrt(x2) - log(1.1) + x1 ), mean = c( log(1.4), 0.3 ), cov = diag( c(0.5, 0.02) ) )
  
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
  
  ##### sens_plot
  
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
  library(msm)
  SE = deltamethod( ~ pnorm( ( log(0.9) - log(1.2) - x1 ) / sqrt( x2 - 0.1^2 ) ),
                    mean = c( yr, t2 ), cov = diag( c(vyr, vt2) ) )
  expect_equal( SE, cm[1,3] )
  
  # CI limits
  expect_equal( max(0, cm[1,2] - SE*qnorm(0.975)), cm[1,4] )
  expect_equal( min(1, cm[1,2] + SE*qnorm(0.975)), cm[1,5] )
  
  
  ##### TMin ######
  # point estimate
  expect_equal( exp( q - yr - qnorm(r) * sqrt(t2) ),
                cm[2,2] )
  
  # check standard error against deltamethod function
  #qnorm( 0.1 ) # because deltamethod can't take its derivative
  SE = deltamethod( ~ exp( log(0.9) - x1 - (-1.281552) * sqrt(x2) ), mean = c( log(0.6), 0.2 ), cov = diag( c(0.2, 0.01) ) )
  
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
  library(dplyr)
  library(ICC)
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
  
  # fns for generating data
  library(here())
  setwd(here())
  setwd("tests")
  source("testthat_helper.R")
  
  library(dplyr)
  library(ICC)
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
  expect_warning( confounded_meta(method="calibrated",
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









# ### jl testing calibrated confounded_meta 1 ###
# test_that("calibrated confounded_meta compared to That_causal results in analysis.R",{
#   ### packages and functions from Maya's old code:
#   library(boot)
#   
#   ###helper functions (Phat_causal, That_causal, That_causal_bt, g, logHR_to_logRR) from Maya code:
#   ###### Phat after shifting by bias factor B and using calibrated estimates #####
#   # .dat needs to have a column called "calib"
#   Phat_causal = function( .q,
#                           .B,
#                           .calib, # assumed on log scale
#                           .tail,
#                           
#                           .give.CI = TRUE,
#                           .R = 2000,
#                           .dat = NA,
#                           .calib.name = NA ) {
#     
#     
#     # confounding-adjusted calibrated estimates
#     # ~~~ SPECIFIC TO THIS STUDY: assumes apparently protective case -- jl edited to allow for tail above/below
#     # calib.t = .calib + log(.B)
#     if(.tail == "above") calib.t = .calib - log(.B)
#     if(.tail == "below") calib.t = .calib + log(.B)
# 
#     # confounding-adjusted Phat
#     if ( .tail == "above" ) Phat.t = mean( calib.t > .q )
#     if ( .tail == "below" ) Phat.t = mean( calib.t < .q )
#     
#     
#     if ( .give.CI == FALSE ) {
#       
#       return(Phat.t)
#       
#     } else {
#       boot.res = suppressWarnings( boot( data = .dat,
#                                          parallel = "multicore",
#                                          R = .R, 
#                                          statistic = Phat_causal_bt,
#                                          # below arguments are being passed to get_stat
#                                          .calib.name = .calib.name,
#                                          .q = .q,
#                                          .B = .B,
#                                          .tail = .tail ) )
#       
#       bootCIs = boot.ci(boot.res,
#                         type="bca",
#                         conf = 0.95 )
#       
#       lo = bootCIs$bca[4]
#       hi = bootCIs$bca[5]
#       SE = sd(boot.res$t)
#       
#       return( data.frame( Est = Phat.t,
#                           SE = SE,
#                           lo = lo, 
#                           hi = hi ) )
#     }
#   }
#   
#   That_causal = function( .q,  # on log-RR scale (transformed if needed)
#                           .r,
#                           .B.vec,  # RR scale
#                           .calib, # on log-RR scale (transformed if needed)
#                           .tail,
#                           
#                           .give.CI = TRUE,
#                           .R = 2000,
#                           .dat,
#                           .calib.name ) {
#     
#     
#     Bl = as.list(.B.vec)
#     
#     Phat.t.vec = lapply( Bl,
#                          FUN = function(B) Phat_causal( .q = .q, 
#                                                         .B = B,
#                                                         .calib = .calib,
#                                                         .tail = .tail,
#                                                         .give.CI = FALSE ) )
#     
#     res = data.frame( B = .B.vec,
#                       Phat.t = unlist(Phat.t.vec) )
#     
#     That = res$B[ which.min( abs( res$Phat.t - .r ) ) ]
#     Ghat = g(That)
#     
#     
#     if ( .give.CI == FALSE ) {
#       
#       return( data.frame( That, Ghat ) )
#       
#     } else {
#       boot.res = suppressWarnings( boot( data = .dat,
#                                          parallel = "multicore",
#                                          R = .R, 
#                                          statistic = That_causal_bt,
#                                          # below arguments are being passed to get_stat
#                                          .calib.name = .calib.name,
#                                          .q = .q,
#                                          .r = .r,
#                                          .B.vec = .B.vec,
#                                          .tail = .tail ) )
#       
#       bootCIs = boot.ci(boot.res,
#                         type="bca",
#                         conf = 0.95 )
#       
#       lo = bootCIs$bca[4]
#       hi = bootCIs$bca[5]
#       SE = sd(boot.res$t)
#       
#       return( data.frame( Stat = c("That", "Ghat"),
#                           Est = c(That, Ghat),
#                           SE = c(SE, NA),  # ~~ for latter, could replace with delta method
#                           lo = c(lo, g(lo)), 
#                           hi = c(hi, g(hi)),
#                           Meaning = c("Bias factor required", "Confounding strength required") ) )
#     }
#   }
#   
#   
#   ##### Simplified version of the above for boot to call #####
#   That_causal_bt = function( original,
#                              indices, 
#                              .calib.name,
#                              .q,
#                              .r,
#                              .B.vec,
#                              .tail ) {
#     b = original[indices,]
#     
#     Bl = as.list(.B.vec)
#     
#     # calculate Phat for a vector of B
#     Phat.t.vec = unlist( lapply( Bl,
#                                  FUN = function(B) Phat_causal( .q = .q, 
#                                                                 .B = B,
#                                                                 .calib = b[[.calib.name]],
#                                                                 .tail = .tail,
#                                                                 .give.CI = FALSE ) ) )
#     
#     
#     That = .B.vec[ which.min( abs( Phat.t.vec - .r ) ) ]
#     return(That)
#     
#   }
#   
#   # define transformation in a way that is monotonic over the effective range of B (>1)
#   # to avoid ggplot errors
#   g = Vectorize( function(x) {
#     if (x < 1) return( x / 1e10 )
#     x + sqrt( x^2 - x )
#   } )
#   logHR_to_logRR = function(logRR){
#     log( ( 1 - 0.5^sqrt( exp(logRR) ) ) / ( 1 - 0.5^sqrt( 1 / exp(logRR) ) ) )
#   }
#   
#   ### end helper functions from Maya code
#   
#   ### test parameters for both Maya old code and updated calibrated confounded_meta function
#   .q=logHR_to_logRR(log(.8))
#   .r=.10
#   .Bmin=1
#   .Bmax=4
#   .B.vec=seq( 1, 4, 0.01 )
#   .tail="below"
#   .R = 2000
#   
#   ### dataframe from Maya code:
#   d=as.data.frame(list(author = c("Teo 2010", "Da silva-gane 2012", "Hussain 2013", 
#                              "Shih 2013", "Shum 2014", "Brown 2015", "Kwok 2016", "Verberne 2016", 
#                              "Chandna 2016", "Reindl-schwaighofer 2017", "Raman 2018", "Tam-tham 2018"),
#                        year = c(2010, 2012, 2013, 2013, 2014, 2015, 2016, 2016, 2016,2017, 2018, 2018),
#                        hr = c(0.44, 0.44, 0.46, 1.16, 0.46, 0.31,0.22, 0.62, 0.53, 0.23, 0.61, 0.67),
#                        lb = c(0.22, 0.22, 0.32,1.07, 0.31, 0.21, 0.17, 0.42, 0.39, 0.18, 0.41, 0.53),
#                        ub = c(0.86, 0.92, 0.68, 1.25, 0.68, 0.47, 0.3, 0.92, 0.73, 0.29, 0.91, 0.83),
#                        n = c(57, 154, 306, 8341, 199, 286, 558, 311, 250, 8796, 204,838),
#                        nx = c(41, 124, 164, 6292, 157, 164, 126, 204, 92, 8622,123, 500), 
#                        n0 = c(16, 30, 142, 2049, 42, 122, 432, 107, 158,174, 81, 338),
#                        yi = c(-0.82098055206983, -0.82098055206983, -0.776528789498996,0.148420005118273, -0.776528789498996, -1.17118298150295, -1.51412773262978,
#                               -0.478035800943, -0.63487827243597, -1.46967597005894, -0.49429632181478, -0.400477566597125),
#                        vyi = c(0.116911650846615, 0.141626456589046,0.0397704305571613, 0.00145351248489691, 0.0397704305571613,0.0450842985184214, 0.0250415490680121,
#                                0.0405449956738431, 0.0266844577833026,0.0139873914540288, 0.0416478534714748, 0.0119380066476652),
#                        calib = c(-0.815500241994327, -0.814528779625426, -0.776052752266121,0.147175232542529, -0.776052752266121, -1.15489254358239,-1.49702475156308,
#                                  -0.488331228832504, -0.637983041992715, -1.46055146962155, -0.504254067888611, -0.404485663510471),
#                        calib.logRR = c(-0.560977462897841, -0.560319288814832, 
#                      -0.534223240540279, 0.101988678750287, -0.534223240540279, 
#                      -0.788465088962296, -1.01180042488262, -0.337559759120245, 
#                      -0.440157063435869, -0.988320988323321, -0.3485033298636, 
#                      -0.279841496330782)))
#   
#   cm_old = That_causal( .q = .q,
#                         .r = .r,
#                         .B.vec = .B.vec,
#                         .calib = d$calib.logRR, 
#                         .tail = .tail,
#                         
#                         .give.CI = TRUE,
#                         .R = .R,
#                         .dat = d, 
#                         .calib.name = "calib.logRR")
#   
#   
#   ### remove all packages and functions used above before using updated calibrated confounded_meta:
#   detach("package:boot", unload = TRUE)
#   rm(g, logHR_to_logRR, Phat_causal, That_causal, That_causal_bt)
# 
#   ### jl updated confounded_meta function, will eventually be able to read from EValue package but for now 
#   ### load confounded_meta, g, logHR_to_logRR, Phat_causal, Phat_causal_bt, That_causal_bet from meta-analysis.R:
#   source("~/Box Sync/jlee/Maya/evalue/EValue/R/meta-analysis.R")
#   cm = confounded_meta( method = "calibrated",q = .q,
#                         r = .r,
#                         Bmin = .Bmin,
#                         Bmax = .Bmax,
#                         .calib = d$calib.logRR, 
#                         tail = .tail,
#                         
#                         .give.CI = TRUE,
#                         .R = .R,
#                         .dat = d, 
#                         .calib.name = "calib.logRR")
#   
#   ### compare below That and Ghat values
#   ## That value from That_causal results in analysis.R
#   expect_equal(cm_old[1,2], cm[2,2])
#   ## Ghat value from That_causal results in analysis.R
#   expect_equal(cm_old[2,2], cm[3,2])
# })
# 
# ### jl testing calibrated confounded_meta 2 ###
# test_that("calibrated confounded_meta compared to That_causal results in analysis.R using different parameters",{
#   ### packages and functions from Maya's old code:
#   library(boot)
#   
#   ###helper functions (Phat_causal, That_causal, That_causal_bt, g, logHR_to_logRR) from Maya code:
#   ###### Phat after shifting by bias factor B and using calibrated estimates #####
#   # .dat needs to have a column called "calib"
#   Phat_causal = function( .q,
#                           .B,
#                           .calib, # assumed on log scale
#                           .tail,
#                           
#                           .give.CI = TRUE,
#                           .R = 2000,
#                           .dat = NA,
#                           .calib.name = NA ) {
#     
#     
#     # confounding-adjusted calibrated estimates
#     # ~~~ SPECIFIC TO THIS STUDY: assumes apparently protective case -- jl edited to allow for tail above/below
#     # calib.t = .calib + log(.B)
#     if(.tail == "above") calib.t = .calib - log(.B)
#     if(.tail == "below") calib.t = .calib + log(.B)
#     
#     # confounding-adjusted Phat
#     if ( .tail == "above" ) Phat.t = mean( calib.t > .q )
#     if ( .tail == "below" ) Phat.t = mean( calib.t < .q )
#     
#     
#     if ( .give.CI == FALSE ) {
#       
#       return(Phat.t)
#       
#     } else {
#       boot.res = suppressWarnings( boot( data = .dat,
#                                          parallel = "multicore",
#                                          R = .R, 
#                                          statistic = Phat_causal_bt,
#                                          # below arguments are being passed to get_stat
#                                          .calib.name = .calib.name,
#                                          .q = .q,
#                                          .B = .B,
#                                          .tail = .tail ) )
#       
#       bootCIs = boot.ci(boot.res,
#                         type="bca",
#                         conf = 0.95 )
#       
#       lo = bootCIs$bca[4]
#       hi = bootCIs$bca[5]
#       SE = sd(boot.res$t)
#       
#       return( data.frame( Est = Phat.t,
#                           SE = SE,
#                           lo = lo, 
#                           hi = hi ) )
#     }
#   }
#   
#   That_causal = function( .q,  # on log-RR scale (transformed if needed)
#                           .r,
#                           .B.vec,  # RR scale
#                           .calib, # on log-RR scale (transformed if needed)
#                           .tail,
#                           
#                           .give.CI = TRUE,
#                           .R = 2000,
#                           .dat,
#                           .calib.name ) {
#     
#     
#     Bl = as.list(.B.vec)
#     
#     Phat.t.vec = lapply( Bl,
#                          FUN = function(B) Phat_causal( .q = .q, 
#                                                         .B = B,
#                                                         .calib = .calib,
#                                                         .tail = .tail,
#                                                         .give.CI = FALSE ) )
#     
#     res = data.frame( B = .B.vec,
#                       Phat.t = unlist(Phat.t.vec) )
#     
#     That = res$B[ which.min( abs( res$Phat.t - .r ) ) ]
#     Ghat = g(That)
#     
#     
#     if ( .give.CI == FALSE ) {
#       
#       return( data.frame( That, Ghat ) )
#       
#     } else {
#       boot.res = suppressWarnings( boot( data = .dat,
#                                          parallel = "multicore",
#                                          R = .R, 
#                                          statistic = That_causal_bt,
#                                          # below arguments are being passed to get_stat
#                                          .calib.name = .calib.name,
#                                          .q = .q,
#                                          .r = .r,
#                                          .B.vec = .B.vec,
#                                          .tail = .tail ) )
#       
#       bootCIs = boot.ci(boot.res,
#                         type="bca",
#                         conf = 0.95 )
#       
#       lo = bootCIs$bca[4]
#       hi = bootCIs$bca[5]
#       SE = sd(boot.res$t)
#       
#       return( data.frame( Stat = c("That", "Ghat"),
#                           Est = c(That, Ghat),
#                           SE = c(SE, NA),  # ~~ for latter, could replace with delta method
#                           lo = c(lo, g(lo)), 
#                           hi = c(hi, g(hi)),
#                           Meaning = c("Bias factor required", "Confounding strength required") ) )
#     }
#   }
#   
#   
#   ##### Simplified version of the above for boot to call #####
#   That_causal_bt = function( original,
#                              indices, 
#                              .calib.name,
#                              .q,
#                              .r,
#                              .B.vec,
#                              .tail ) {
#     b = original[indices,]
#     
#     Bl = as.list(.B.vec)
#     
#     # calculate Phat for a vector of B
#     Phat.t.vec = unlist( lapply( Bl,
#                                  FUN = function(B) Phat_causal( .q = .q, 
#                                                                 .B = B,
#                                                                 .calib = b[[.calib.name]],
#                                                                 .tail = .tail,
#                                                                 .give.CI = FALSE ) ) )
#     
#     
#     That = .B.vec[ which.min( abs( Phat.t.vec - .r ) ) ]
#     return(That)
#     
#   }
#   
#   # define transformation in a way that is monotonic over the effective range of B (>1)
#   # to avoid ggplot errors
#   g = Vectorize( function(x) {
#     if (x < 1) return( x / 1e10 )
#     x + sqrt( x^2 - x )
#   } )
#   logHR_to_logRR = function(logRR){
#     log( ( 1 - 0.5^sqrt( exp(logRR) ) ) / ( 1 - 0.5^sqrt( 1 / exp(logRR) ) ) )
#   }
#   
#   ### end helper functions from Maya code
#   
#   ### test parameters for both Maya old code and updated calibrated confounded_meta function
#   .q=logHR_to_logRR(log(.25))
#   .r=.25
#   .Bmin=0
#   .Bmax=6
#   .B.vec=seq( 0, 6, 0.01 )
#   .tail="below"
#   .R = 2000
#   
#   ### dataframe from Maya code:
#   d=as.data.frame(list(author = c("Teo 2010", "Da silva-gane 2012", "Hussain 2013", 
#                                   "Shih 2013", "Shum 2014", "Brown 2015", "Kwok 2016", "Verberne 2016", 
#                                   "Chandna 2016", "Reindl-schwaighofer 2017", "Raman 2018", "Tam-tham 2018"),
#                        year = c(2010, 2012, 2013, 2013, 2014, 2015, 2016, 2016, 2016,2017, 2018, 2018),
#                        hr = c(0.44, 0.44, 0.46, 1.16, 0.46, 0.31,0.22, 0.62, 0.53, 0.23, 0.61, 0.67),
#                        lb = c(0.22, 0.22, 0.32,1.07, 0.31, 0.21, 0.17, 0.42, 0.39, 0.18, 0.41, 0.53),
#                        ub = c(0.86, 0.92, 0.68, 1.25, 0.68, 0.47, 0.3, 0.92, 0.73, 0.29, 0.91, 0.83),
#                        n = c(57, 154, 306, 8341, 199, 286, 558, 311, 250, 8796, 204,838),
#                        nx = c(41, 124, 164, 6292, 157, 164, 126, 204, 92, 8622,123, 500), 
#                        n0 = c(16, 30, 142, 2049, 42, 122, 432, 107, 158,174, 81, 338),
#                        yi = c(-0.82098055206983, -0.82098055206983, -0.776528789498996,0.148420005118273, -0.776528789498996, -1.17118298150295, -1.51412773262978,
#                               -0.478035800943, -0.63487827243597, -1.46967597005894, -0.49429632181478, -0.400477566597125),
#                        vyi = c(0.116911650846615, 0.141626456589046,0.0397704305571613, 0.00145351248489691, 0.0397704305571613,0.0450842985184214, 0.0250415490680121,
#                                0.0405449956738431, 0.0266844577833026,0.0139873914540288, 0.0416478534714748, 0.0119380066476652),
#                        calib = c(-0.815500241994327, -0.814528779625426, -0.776052752266121,0.147175232542529, -0.776052752266121, -1.15489254358239,-1.49702475156308,
#                                  -0.488331228832504, -0.637983041992715, -1.46055146962155, -0.504254067888611, -0.404485663510471),
#                        calib.logRR = c(-0.560977462897841, -0.560319288814832, 
#                                        -0.534223240540279, 0.101988678750287, -0.534223240540279, 
#                                        -0.788465088962296, -1.01180042488262, -0.337559759120245, 
#                                        -0.440157063435869, -0.988320988323321, -0.3485033298636, 
#                                        -0.279841496330782)))
#   
#   cm_old = That_causal( .q = .q,
#                         .r = .r,
#                         .B.vec = .B.vec,
#                         .calib = d$calib.logRR, 
#                         .tail = .tail,
#                         
#                         .give.CI = TRUE,
#                         .R = .R,
#                         .dat = d, 
#                         .calib.name = "calib.logRR")
#   
#   
#   ### remove all packages and functions used above before using updated calibrated confounded_meta:
#   detach("package:boot", unload = TRUE)
#   rm(g, logHR_to_logRR, Phat_causal, That_causal, That_causal_bt)
#   
#   ### jl updated confounded_meta function, will eventually be able to read from EValue package but for now 
#   ### load confounded_meta, g, logHR_to_logRR, Phat_causal, Phat_causal_bt, That_causal_bet from meta-analysis.R:
#   source("~/Box Sync/jlee/Maya/evalue/EValue/R/meta-analysis.R")
#   cm = confounded_meta( method = "calibrated",q = .q,
#                         r = .r,
#                         Bmin = .Bmin,
#                         Bmax = .Bmax,
#                         .calib = d$calib.logRR, 
#                         tail = .tail,
#                         
#                         .give.CI = TRUE,
#                         .R = .R,
#                         .dat = d, 
#                         .calib.name = "calib.logRR")
#   
#   ### compare below That and Ghat values
#   ## That value from That_causal results in analysis.R
#   expect_equal(cm_old[1,2], cm[2,2])
#   ## Ghat value from That_causal results in analysis.R
#   expect_equal(cm_old[2,2], cm[3,2])
# })
# 
# ### jl testing calibrated confounded_meta 3 ###
# test_that("calibrated confounded_meta compared to That_causal results in analysis.R using different parameters and change tail",{
#   ### packages and functions from Maya's old code:
#   library(boot)
#   
#   ###helper functions (Phat_causal, That_causal, That_causal_bt, g, logHR_to_logRR) from Maya code:
#   ###### Phat after shifting by bias factor B and using calibrated estimates #####
#   # .dat needs to have a column called "calib"
#   Phat_causal = function( .q,
#                           .B,
#                           .calib, # assumed on log scale
#                           .tail,
#                           
#                           .give.CI = TRUE,
#                           .R = 2000,
#                           .dat = NA,
#                           .calib.name = NA ) {
#     
#     
#     # confounding-adjusted calibrated estimates
#     # ~~~ SPECIFIC TO THIS STUDY: assumes apparently protective case -- jl edited to allow for tail above/below
#     # calib.t = .calib + log(.B)
#     if(.tail == "above") calib.t = .calib - log(.B)
#     if(.tail == "below") calib.t = .calib + log(.B)
#     
#     # confounding-adjusted Phat
#     if ( .tail == "above" ) Phat.t = mean( calib.t > .q )
#     if ( .tail == "below" ) Phat.t = mean( calib.t < .q )
#     
#     
#     if ( .give.CI == FALSE ) {
#       
#       return(Phat.t)
#       
#     } else {
#       boot.res = suppressWarnings( boot( data = .dat,
#                                          parallel = "multicore",
#                                          R = .R, 
#                                          statistic = Phat_causal_bt,
#                                          # below arguments are being passed to get_stat
#                                          .calib.name = .calib.name,
#                                          .q = .q,
#                                          .B = .B,
#                                          .tail = .tail ) )
#       
#       bootCIs = boot.ci(boot.res,
#                         type="bca",
#                         conf = 0.95 )
#       
#       lo = bootCIs$bca[4]
#       hi = bootCIs$bca[5]
#       SE = sd(boot.res$t)
#       
#       return( data.frame( Est = Phat.t,
#                           SE = SE,
#                           lo = lo, 
#                           hi = hi ) )
#     }
#   }
#   
#   That_causal = function( .q,  # on log-RR scale (transformed if needed)
#                           .r,
#                           .B.vec,  # RR scale
#                           .calib, # on log-RR scale (transformed if needed)
#                           .tail,
#                           
#                           .give.CI = TRUE,
#                           .R = 2000,
#                           .dat,
#                           .calib.name ) {
#     
#     
#     Bl = as.list(.B.vec)
#     
#     Phat.t.vec = lapply( Bl,
#                          FUN = function(B) Phat_causal( .q = .q, 
#                                                         .B = B,
#                                                         .calib = .calib,
#                                                         .tail = .tail,
#                                                         .give.CI = FALSE ) )
#     
#     res = data.frame( B = .B.vec,
#                       Phat.t = unlist(Phat.t.vec) )
#     
#     That = res$B[ which.min( abs( res$Phat.t - .r ) ) ]
#     Ghat = g(That)
#     
#     
#     if ( .give.CI == FALSE ) {
#       
#       return( data.frame( That, Ghat ) )
#       
#     } else {
#       boot.res = suppressWarnings( boot( data = .dat,
#                                          parallel = "multicore",
#                                          R = .R, 
#                                          statistic = That_causal_bt,
#                                          # below arguments are being passed to get_stat
#                                          .calib.name = .calib.name,
#                                          .q = .q,
#                                          .r = .r,
#                                          .B.vec = .B.vec,
#                                          .tail = .tail ) )
#       
#       bootCIs = boot.ci(boot.res,
#                         type="bca",
#                         conf = 0.95 )
#       
#       lo = bootCIs$bca[4]
#       hi = bootCIs$bca[5]
#       SE = sd(boot.res$t)
#       
#       return( data.frame( Stat = c("That", "Ghat"),
#                           Est = c(That, Ghat),
#                           SE = c(SE, NA),  # ~~ for latter, could replace with delta method
#                           lo = c(lo, g(lo)), 
#                           hi = c(hi, g(hi)),
#                           Meaning = c("Bias factor required", "Confounding strength required") ) )
#     }
#   }
#   
#   
#   ##### Simplified version of the above for boot to call #####
#   That_causal_bt = function( original,
#                              indices, 
#                              .calib.name,
#                              .q,
#                              .r,
#                              .B.vec,
#                              .tail ) {
#     b = original[indices,]
#     
#     Bl = as.list(.B.vec)
#     
#     # calculate Phat for a vector of B
#     Phat.t.vec = unlist( lapply( Bl,
#                                  FUN = function(B) Phat_causal( .q = .q, 
#                                                                 .B = B,
#                                                                 .calib = b[[.calib.name]],
#                                                                 .tail = .tail,
#                                                                 .give.CI = FALSE ) ) )
#     
#     
#     That = .B.vec[ which.min( abs( Phat.t.vec - .r ) ) ]
#     return(That)
#     
#   }
#   
#   # define transformation in a way that is monotonic over the effective range of B (>1)
#   # to avoid ggplot errors
#   g = Vectorize( function(x) {
#     if (x < 1) return( x / 1e10 )
#     x + sqrt( x^2 - x )
#   } )
#   logHR_to_logRR = function(logRR){
#     log( ( 1 - 0.5^sqrt( exp(logRR) ) ) / ( 1 - 0.5^sqrt( 1 / exp(logRR) ) ) )
#   }
#   
#   ### end helper functions from Maya code
#   
#   ### test parameters for both Maya old code and updated calibrated confounded_meta function
#   .q=logHR_to_logRR(log(.5))
#   .r=.1
#   .Bmin=1
#   .Bmax=5
#   .B.vec=seq( 1, 5, 0.01 )
#   .tail="above"
#   .R = 2000
#   
#   ### dataframe from Maya code:
#   d=as.data.frame(list(author = c("Teo 2010", "Da silva-gane 2012", "Hussain 2013", 
#                                   "Shih 2013", "Shum 2014", "Brown 2015", "Kwok 2016", "Verberne 2016", 
#                                   "Chandna 2016", "Reindl-schwaighofer 2017", "Raman 2018", "Tam-tham 2018"),
#                        year = c(2010, 2012, 2013, 2013, 2014, 2015, 2016, 2016, 2016,2017, 2018, 2018),
#                        hr = c(0.44, 0.44, 0.46, 1.16, 0.46, 0.31,0.22, 0.62, 0.53, 0.23, 0.61, 0.67),
#                        lb = c(0.22, 0.22, 0.32,1.07, 0.31, 0.21, 0.17, 0.42, 0.39, 0.18, 0.41, 0.53),
#                        ub = c(0.86, 0.92, 0.68, 1.25, 0.68, 0.47, 0.3, 0.92, 0.73, 0.29, 0.91, 0.83),
#                        n = c(57, 154, 306, 8341, 199, 286, 558, 311, 250, 8796, 204,838),
#                        nx = c(41, 124, 164, 6292, 157, 164, 126, 204, 92, 8622,123, 500), 
#                        n0 = c(16, 30, 142, 2049, 42, 122, 432, 107, 158,174, 81, 338),
#                        yi = c(-0.82098055206983, -0.82098055206983, -0.776528789498996,0.148420005118273, -0.776528789498996, -1.17118298150295, -1.51412773262978,
#                               -0.478035800943, -0.63487827243597, -1.46967597005894, -0.49429632181478, -0.400477566597125),
#                        vyi = c(0.116911650846615, 0.141626456589046,0.0397704305571613, 0.00145351248489691, 0.0397704305571613,0.0450842985184214, 0.0250415490680121,
#                                0.0405449956738431, 0.0266844577833026,0.0139873914540288, 0.0416478534714748, 0.0119380066476652),
#                        calib = c(-0.815500241994327, -0.814528779625426, -0.776052752266121,0.147175232542529, -0.776052752266121, -1.15489254358239,-1.49702475156308,
#                                  -0.488331228832504, -0.637983041992715, -1.46055146962155, -0.504254067888611, -0.404485663510471),
#                        calib.logRR = c(-0.560977462897841, -0.560319288814832, 
#                                        -0.534223240540279, 0.101988678750287, -0.534223240540279, 
#                                        -0.788465088962296, -1.01180042488262, -0.337559759120245, 
#                                        -0.440157063435869, -0.988320988323321, -0.3485033298636, 
#                                        -0.279841496330782)))
#   
#   cm_old = That_causal( .q = .q,
#                         .r = .r,
#                         .B.vec = .B.vec,
#                         .calib = d$calib.logRR, 
#                         .tail = .tail,
#                         
#                         .give.CI = TRUE,
#                         .R = .R,
#                         .dat = d, 
#                         .calib.name = "calib.logRR")
#   
#   
#   ### remove all packages and functions used above before using updated calibrated confounded_meta:
#   detach("package:boot", unload = TRUE)
#   rm(g, logHR_to_logRR, Phat_causal, That_causal, That_causal_bt)
#   
#   ### jl updated confounded_meta function, will eventually be able to read from EValue package but for now 
#   ### load confounded_meta, g, logHR_to_logRR, Phat_causal, Phat_causal_bt, That_causal_bet from meta-analysis.R:
#   source("~/Box Sync/jlee/Maya/evalue/EValue/R/meta-analysis.R")
#   cm = confounded_meta( method = "calibrated",q = .q,
#                         r = .r,
#                         Bmin = .Bmin,
#                         Bmax = .Bmax,
#                         .calib = d$calib.logRR, 
#                         tail = .tail,
#                         
#                         .give.CI = TRUE,
#                         .R = .R,
#                         .dat = d, 
#                         .calib.name = "calib.logRR")
#   
#   ### compare below That and Ghat values
#   ## That value from That_causal results in analysis.R
#   expect_equal(cm_old[1,2], cm[2,2])
#   ## Ghat value from That_causal results in analysis.R
#   expect_equal(cm_old[2,2], cm[3,2])
# })






##### sens_plot



#### sens_table function #####

test_that("True = est, no CI, preventive", {
  expect_equal( 1,
                evalues.RR( 0.9, true = 0.9 )[2, "point"] )
  
  tab = sens_table( meas="prop", q=log(1.1), muB=c( log(1.1),
                    log(1.5) ), sigB=c(0, 0.1), 
                    yr=log(2.5), t2=0.1 )
  
  # get table values directly from other function
  mine = suppressMessages( confounded_meta(q=log(1.1), muB=log(1.1), sigB=0, 
                                           yr=log(2.5), t2=0.1)[1,"Est"] )
  expect_equal( mine, tab[1,1] )
  
  
  mine = suppressMessages( confounded_meta(q=log(1.1), muB=log(1.5), sigB=0, 
                                           yr=log(2.5), t2=0.1)[1,"Est"] )
  expect_equal( mine, tab[2,1] )
  
  
  mine = suppressMessages( confounded_meta(q=log(1.1), muB=log(1.1), sigB=0.1, 
                                           yr=log(2.5), t2=0.1)[1,"Est"] )
  expect_equal( mine, tab[1,2] )
  
  
  mine = suppressMessages( confounded_meta(q=log(1.1), muB=log(1.5), sigB=0.1, 
                                           yr=log(2.5), t2=0.1)[1,"Est"] )
  expect_equal( mine, tab[2,2] )
})


# ##### stronger_than function #####
# 
# test_that("stronger_than #1", {
#   
#   # proportion above and below should sum to 1
#   expect_equal( 1,
#                 stronger_than( q = .5, yr = .6, vyr = .07, t2 = 0.2, vt2=0.02,
#                                CI.level=0.95, tail = "above" )$Est +
#                   stronger_than( q = .5, yr = .6, vyr = .07, t2 = 0.2, vt2=0.02,
#                                  CI.level=0.95, tail = "below" )$Est                 
#                   )
# 
#   q = 0.5
#   yr = 0.6
#   vyr = 0.07
#   t2 = 0.2
#   vt2 = 0.02
#   CI.level = 0.9
#   
#   st = stronger_than( q = q, yr = yr, vyr = vyr, t2 = t2, vt2=vt2,
#                       CI.level=CI.level, tail = "above" )
#   
#   expect_equal( 1 - pnorm( ( q - yr ) / sqrt( t2 ) ), st$Est )
#   
#   expect_equal( sqrt( (vyr / t2) + ( ( vt2 * (q - yr)^2 ) / ( 4 * t2^3 ) ) ) * dnorm( ( q - yr ) / sqrt( t2 ) ),
#                 st$SE )
#   
#   alpha = 1 - CI.level
#   crit = qnorm( 1 - alpha/2 )
#   
#   expect_equal( min( 1, st$Est + st$SE * crit ), st$CI.hi )
#   expect_equal( max( 0, st$Est - st$SE * crit ), st$CI.lo )
#     
# })
# 
# 
# test_that("stronger_than #2", {
#   
#  # proportion below a preventive effect
#   q = 0.6
#   yr = -0.2
#   vyr = 0.002
#   t2 = 0.4
#   vt2 = 0.02
#   CI.level = 0.75
#   
#   st = stronger_than( q = q, yr = yr, vyr = vyr, t2 = t2, vt2=vt2,
#                       CI.level=CI.level, tail = "below" )
#   
#   expect_equal( pnorm( ( q - yr ) / sqrt( t2 ) ), st$Est )
#   
#   expect_equal( sqrt( (vyr / t2) + ( ( vt2 * (q - yr)^2 ) / ( 4 * t2^3 ) ) ) * dnorm( ( q - yr ) / sqrt( t2 ) ),
#                 st$SE )
#   
#   alpha = 1 - CI.level
#   crit = qnorm( 1 - alpha/2 )
#   
#   expect_equal( min( 1, st$Est + st$SE * crit ), st$CI.hi )
#   expect_equal( max( 0, st$Est - st$SE * crit ), st$CI.lo )
#   
# })
# 
# 
