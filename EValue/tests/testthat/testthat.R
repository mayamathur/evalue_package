library(testthat)
library(EValue)
library(devtools)


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







###################### CONFOUNDEDMETA ###################### 


test_that("Setting q equal to observed mean without bias should yield 50%", {
  expect_equal( 0.5, confounded_meta(method="parametric", q=log(1.4), muB=0, sigB=0,
                                     yr=log(1.4), t2=0.1 )[1,2] )
  
  expect_equal( 0.5, confounded_meta(method="parametric", q=log(0.5), muB=0, sigB=0,
                                     yr=log(0.5), t2=0.1 )[1,2] )
})



test_that("Case #1, causative", {
  
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
})




test_that("Case #2, preventive", {
  
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





test_that("No bias needed to reduce strong effects to less than r", {
  cm = suppressMessages( confounded_meta(method="parametric", q=log(1.5), r = 0.1, muB=0, sigB=0, 
                                         yr=log(1.3), t2=0.01) )
  
  # Tmin and Gmin should both be 1 because already fewer than 10% of true effects
  #  are above q
  expect_equal( 1, cm[2,2] )
  expect_equal( 1, cm[3,2] )
})


### jl testing calibrated confounded_meta ###
test_that("calibrated confounded_meta compared to That_causal results in analysis.R",{
  cm = confounded_meta( method = "calibrated",q = logHR_to_logRR(log(.8)),
                        r = .1,
                        Bmin=1,
                        Bmax=4,
                        .calib = d$calib.logRR, 
                        tail = "below",
                        
                        .give.CI = TRUE,
                        .R = 2000,
                        .dat = d, 
                        .calib.name = "calib.logRR")
  ## That value from That_causal results in analysis.R
  expect_equal(2.3100000, cm[2,2])
  ## Ghat value from That_causal results in analysis.R
  expect_equal(4.0495689, cm[3,2])
})



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


##### stronger_than function #####

test_that("stronger_than #1", {
  
  # proportion above and below should sum to 1
  expect_equal( 1,
                stronger_than( q = .5, yr = .6, vyr = .07, t2 = 0.2, vt2=0.02,
                               CI.level=0.95, tail = "above" )$Est +
                  stronger_than( q = .5, yr = .6, vyr = .07, t2 = 0.2, vt2=0.02,
                                 CI.level=0.95, tail = "below" )$Est                 
                  )

  q = 0.5
  yr = 0.6
  vyr = 0.07
  t2 = 0.2
  vt2 = 0.02
  CI.level = 0.9
  
  st = stronger_than( q = q, yr = yr, vyr = vyr, t2 = t2, vt2=vt2,
                      CI.level=CI.level, tail = "above" )
  
  expect_equal( 1 - pnorm( ( q - yr ) / sqrt( t2 ) ), st$Est )
  
  expect_equal( sqrt( (vyr / t2) + ( ( vt2 * (q - yr)^2 ) / ( 4 * t2^3 ) ) ) * dnorm( ( q - yr ) / sqrt( t2 ) ),
                st$SE )
  
  alpha = 1 - CI.level
  crit = qnorm( 1 - alpha/2 )
  
  expect_equal( min( 1, st$Est + st$SE * crit ), st$CI.hi )
  expect_equal( max( 0, st$Est - st$SE * crit ), st$CI.lo )
    
})


test_that("stronger_than #2", {
  
 # proportion below a preventive effect
  q = 0.6
  yr = -0.2
  vyr = 0.002
  t2 = 0.4
  vt2 = 0.02
  CI.level = 0.75
  
  st = stronger_than( q = q, yr = yr, vyr = vyr, t2 = t2, vt2=vt2,
                      CI.level=CI.level, tail = "below" )
  
  expect_equal( pnorm( ( q - yr ) / sqrt( t2 ) ), st$Est )
  
  expect_equal( sqrt( (vyr / t2) + ( ( vt2 * (q - yr)^2 ) / ( 4 * t2^3 ) ) ) * dnorm( ( q - yr ) / sqrt( t2 ) ),
                st$SE )
  
  alpha = 1 - CI.level
  crit = qnorm( 1 - alpha/2 )
  
  expect_equal( min( 1, st$Est + st$SE * crit ), st$CI.hi )
  expect_equal( max( 0, st$Est - st$SE * crit ), st$CI.lo )
  
})


