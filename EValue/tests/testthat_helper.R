
# Helper fns for testthat

########################### SIMULATE META-ANALYSIS DATA (FROM MRM) ###########################

# potentially with clustering
# notes:
# - number of actually generated clusters could be less than m
#  because we randomly draw from the m clusters with replacement (to allow for k not divisible by m)
# - if bc = bb = 0 (i.e., moderators have no effect), then mu will be static within clusters and equal to zeta1 + b0
# see helper_MRM_sanity_checks.R for extensive sanity checks of this function

sim_data2 = function( k, # total number of studies
                      m = k, # number of clusters (m=k implies no clustering)
                      b0, # intercept
                      bc, # effect of continuous moderator
                      bb, # effect of binary moderator 
                      V,  # TOTAL residual heterogeneity after conditioning on moderators (including within- and between-cluster variance)
                      Vzeta = 0, # between-cluster variance (must be less than V)
                      muN, 
                      minN,
                      sd.w, 
                      true.effect.dist) {
  
  # # @test for m=k case
  # # TEST ONLY
  # k = 3
  # m = 3
  # b0 = 0 # intercept
  # bc = 0 # effect of continuous moderator
  # bb = 0 # effect of binary moderator
  # V = .5
  # Vzeta = 0.25
  # muN = 100
  # minN = 50
  # sd.w = 1
  # true.effect.dist = "expo"
  
  
  if ( Vzeta > V ) stop( "Vzeta must be less than or equal to V" )
  
  
  # assign studies to clusters
  # each in own cluster:
  if (m == k) cluster = 1:k  # each in its own cluster
  # k not divisble by m:
  # fine if k isn't divisible by m (number of clusters); clusters will just be unbalanced and actual number of cluster might be less than m 
  #if ( m < k ) cluster = sample( 1:m, size = k, replace = TRUE )
  # k not divisible by m: assign each observation to a cluster chosen at random (unbalanced clusters)
  if ( m < k & (k %% m != 0) ) cluster = sample( 1:m, size = k, replace = TRUE )
  # k divisible by m: assign observations to clusters in a balanced way
  if ( m < k & (k %% m == 0) ) cluster = rep(1:m, each = k/m)
  
  if (m > k) stop("m must be <= k")
  
  cluster = sort(cluster)
  
  # generate cluster random intercepts (zeta)
  # these are normal even when true effect dist is exponential
  zeta1 = rnorm( n = m, mean = 0, sd = sqrt( Vzeta ) )  # one entry per cluster
  zeta1i = zeta1[cluster]  # one entry per study
  
  
  # simulate k studies
  for (i in 1:k) {
    study = sim_one_study2( b0, # intercept
                            bc, # effect of continuous moderator
                            bb, # effect of binary moderator
                            V = V, 
                            Vzeta = Vzeta, 
                            zeta1 = zeta1i[i], # cluster random intercept for this study's cluster
                            muN = muN,
                            minN = minN,
                            sd.w = sd.w,
                            true.effect.dist = true.effect.dist)
    
    if ( i == 1 ) d = study else d = rbind( d, study )
  }
  
  # add cluster indicator
  d = d %>% mutate( cluster, .before = 1) 
  
  # ICC of study population effects within clusters (will be static for dataset)
  d$icc = ICCbareF( x = as.factor(d$cluster), 
                    y = d$Mi )  
  
  return(d)
}



# potentially with clustering
sim_one_study2 = function(b0, # intercept
                          bc, # effect of continuous moderator
                          bb, # effect of binary moderator
                          V, 
                          Vzeta, # used to calcuate within-cluster variance
                          zeta1,  # scalar cluster random intercept for this study's cluster
                          muN,
                          minN,
                          sd.w,
                          true.effect.dist = "normal"
) {
  
  # # @test for m=1 case
  # # TEST ONLY
  # b0 = 0.5 # intercept
  # bc = 0.5 # effect of continuous moderator
  # bb = 1 # effect of binary moderator
  # V = .5
  # Vzeta = 0.25
  # zeta1 = -0.2
  # muN = 100
  # minN = 50
  # sd.w = 1
  # true.effect.dist = "normal"
  
  if( !true.effect.dist %in% c("normal", "expo") ) stop("True effect dist not recognized")
  
  ##### Simulate Sample Size and Fixed Design Matrix for This Study #####
  # simulate total N for this study
  N = round( runif( n = 1, min = minN, max = minN + 2*( muN - minN ) ) ) # draw from uniform centered on muN
  
  # simulate study-level moderators (each a scalar)
  Zc = rnorm( n = 1, mean = 0, sd = 1)
  Zb = rbinom( n = 1, size = 1, prob = 0.5)
  
  # mean (i.e., linear predictor) conditional on the moderators and cluster membership
  mu = b0 + zeta1 + bc*Zc + bb*Zb
  # all that follows is that same as in NPPhat, except incorporating clustering as in SAPB
  
  ##### Draw a Single Population True Effect for This Study #####
  if ( true.effect.dist == "normal" ) {
    Mi = rnorm( n=1, mean=mu, sd=sqrt(V - Vzeta) )
  }
  if ( true.effect.dist == "expo" ) {
    # within-cluster variance = total - between
    Vwithin = V - Vzeta
    # set the rate so the heterogeneity is correct
    Mi = rexp( n = 1, rate = sqrt(1/Vwithin) )
    # now the mean is sqrt(V) rather than mu
    # shift to have the correct mean (in expectation)
    Mi = Mi + (mu - sqrt(Vwithin))
  }
  
  ###### Simulate Data For Individual Subjects ######
  # group assignments
  X = c( rep( 0, N/2 ), rep( 1, N/2 ) )
  
  # simulate continuous outcomes
  # 2-group study of raw mean difference with means 0 and Mi in each group
  # and same SD
  Y = c( rnorm( n = N/2, mean = 0, sd = sd.w ),
         rnorm( n = N/2, mean = Mi, sd = sd.w ) )
  
  # calculate ES for this study using metafor (see Viechtbauer "Conducting...", pg 10)
  require(metafor)
  ES = escalc( measure="SMD",   
               n1i = N/2, 
               n2i = N/2,
               m1i = mean( Y[X==1] ),
               m2i = mean( Y[X==0] ),
               sd1i = sd( Y[X==1] ),
               sd2i = sd( Y[X==0] ) ) 
  yi = ES$yi
  vyi = ES$vi
  
  return( data.frame( Mi, # study's true effect size; if within-cluster heterogeneity is zero, will be equal to mu
                      mu, # study's linear predictor conditional on the moderators and cluster membership
                      zeta1,
                      Zc,
                      Zb,
                      yi,
                      vyi ) )
}



# return the threshold q that is the TheoryP^th quantile 
#  when moderators are set to zc.star and zb.star
calculate_theory_p = function(true.effect.dist, 
                              q,
                              b0,
                              bc,
                              bb,
                              zc,   
                              zb,
                              V){
  
  # get the mean for this combination of moderators
  mu = b0 + bc*zc + bb*zb
  
  if ( true.effect.dist == "normal" ) {
    return( 1 - pnorm( q = q,
                       mean = mu,
                       sd = sqrt(V) ) )
  }
  
  if ( true.effect.dist == "expo" ) {
    # we generate from a exponential, then shift to achieve the correct mean, 
    #  so q is the threshold BEFORE shifting
    # here is the data generation code from sim_data:
    # Mi = rexp( n = 1, rate = sqrt(1/V) )
    # Mi = Mi + (mu - sqrt(V))
    # is this first line right?
    #stop("Not tested/written yet")
    q0 = q - ( mu - sqrt(V) )
    return( pexp( q = q0,
                  rate = sqrt(1/V),
                  lower.tail = FALSE) )
  }
  
  if ( true.effect.dist == "unif2" ) {
    stop("unif2 method not tested/written yet")
    # return( qunif2( p = 1 - TheoryP, 
    #                 mu = mu, 
    #                 V = V) )
  }
  
  if (true.effect.dist == "t.scaled") {
    stop("t.scaled method not tested/written yet")
    # from metRology package
    # return( qt.scaled(p = 1 - TheoryP,
    #                   df = 3,
    #                   mean = mu,
    #                   sd = sqrt(V) ) )
  }
  
  else stop("true.effect.dist not recognized.")
}